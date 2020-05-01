--  MIT License
--
--  Copyright (c) 2020 Max Reznik
--
--  Permission is hereby granted, free of charge, to any person obtaining a
--  copy of this software and associated documentation files (the "Software"),
--  to deal in the Software without restriction, including without limitation
--  the rights to use, copy, modify, merge, publish, distribute, sublicense,
--  and/or sell copies of the Software, and to permit persons to whom the
--  Software is furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
--  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
--  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
--  DEALINGS IN THE SOFTWARE.

with Ada.Characters.Wide_Wide_Latin_1;

with Ada_Pretty;

with League.String_Vectors;

with Compiler.Descriptors;
with Compiler.Enum_Descriptors;

package body Compiler.File_Descriptors is

   F : Ada_Pretty.Factory renames Compiler.Context.Factory;

   use type Ada_Pretty.Node_Access;

   function Package_Name
     (Self : Google.Protobuf.File_Descriptor_Proto)
      return League.Strings.Universal_String;

   function Dependency
     (Self    : Google.Protobuf.File_Descriptor_Proto;
      Request : Google.Protobuf.Compiler.Code_Generator_Request)
      return Ada_Pretty.Node_Access;
   --  return 'with Unit' for each dependency

   ----------------
   -- Dependency --
   ----------------

   function Dependency
     (Self    : Google.Protobuf.File_Descriptor_Proto;
      Request : Google.Protobuf.Compiler.Code_Generator_Request)
      return Ada_Pretty.Node_Access
   is
      use type League.Strings.Universal_String;

      Set    : Compiler.Context.String_Sets.Set;
      Result : Ada_Pretty.Node_Access;
      Unit_Name   : Ada_Pretty.Node_Access;
      With_Clause : Ada_Pretty.Node_Access;
      My_Name     : constant League.Strings.Universal_String :=
        Package_Name (Self);
   begin
      for J in 1 .. Self.Dependency.Length loop
         declare
            Item : constant Google.Protobuf.File_Descriptor_Proto :=
              Compiler.Context.Get_File
                (Request, Self.Dependency.Element (J));
         begin
            Set.Include (Package_Name (Item));
         end;
      end loop;

      for J in 1 .. Self.Message_Type.Length loop
         declare
            Item : constant Google.Protobuf.Descriptor_Proto :=
              Self.Message_Type.Get (J);
         begin
            Compiler.Descriptors.Dependency (Item, Set);
         end;
      end loop;

      for J of Set loop
         if J /= My_Name then
            Unit_Name := F.New_Selected_Name (J);
            With_Clause := F.New_With (Unit_Name);
            Result := F.New_List (Result, With_Clause);
         end if;
      end loop;

      return Result;
   end Dependency;

   ---------------
   -- File_Name --
   ---------------

   function File_Name
     (Self : Google.Protobuf.File_Descriptor_Proto)
      return League.Strings.Universal_String
   is
      Value : constant League.Strings.Universal_String :=
        Package_Name (Self);
      List  : constant League.String_Vectors.Universal_String_Vector :=
        Value.To_Lowercase.Split ('.');
      Result : League.Strings.Universal_String;
   begin
      if Value.Is_Empty then
         Result.Append ("output");
      else
         Result := List.Join ('-');
      end if;

      return Result;
   end File_Name;

   ------------------
   -- Package_Name --
   ------------------

   function Package_Name
     (Self : Google.Protobuf.File_Descriptor_Proto)
      return League.Strings.Universal_String
   is
      Result : League.Strings.Universal_String := Self.PB_Package;
   begin
      if Result.Is_Empty then
         Result.Append ("Output");
      else
         Result := Compiler.Context.To_Selected_Ada_Name (Result);
      end if;

      return Result;
   end Package_Name;

   --------------------------
   -- Populate_Named_Types --
   --------------------------

   procedure Populate_Named_Types
     (Self : Google.Protobuf.File_Descriptor_Proto;
      Map  : in out Compiler.Context.Named_Type_Maps.Map)
   is
      use type League.Strings.Universal_String;

      PB_Package : constant League.Strings.Universal_String :=
        "." & Self.PB_Package;

      Ada_Package : constant League.Strings.Universal_String :=
        Package_Name (Self);
   begin
      for J in 1 .. Self.Message_Type.Length loop
         Compiler.Descriptors.Populate_Named_Types
           (Self   => Self.Message_Type.Get (J),
            PB_Prefix => PB_Package,
            Ada_Package => Ada_Package,
            Map    => Map);
      end loop;

      for J in 1 .. Self.Enum_Type.Length loop
         Compiler.Enum_Descriptors.Populate_Named_Types
           (Self        => Self.Enum_Type.Get (J),
            PB_Prefix   => PB_Package,
            Ada_Package => Ada_Package,
            Map         => Map);
      end loop;
   end Populate_Named_Types;

   ------------------------
   -- Specification_Text --
   ------------------------

   function Specification_Text
     (Self    : Google.Protobuf.File_Descriptor_Proto;
      Request : Google.Protobuf.Compiler.Code_Generator_Request)
      return League.Strings.Universal_String
   is
      function Get_Public return Ada_Pretty.Node_Access;

      ----------------
      -- Get_Public --
      ----------------

      function Get_Public return Ada_Pretty.Node_Access is
         Result : Ada_Pretty.Node_Access;
         Next   : Ada_Pretty.Node_Access;
      begin

         for J in 1 .. Self.Enum_Type.Length loop
            Next := Compiler.Enum_Descriptors.Public_Spec
              (Self.Enum_Type.Get (J));

            if Next /= null then
               Result := F.New_List (Result, Next);
            end if;
         end loop;

         for J in 1 .. Self.Message_Type.Length loop
            declare
               Item : constant Google.Protobuf.Descriptor_Proto :=
                 Self.Message_Type.Get (J);
            begin
               Next := Compiler.Descriptors.Enum_Types (Item);

               if Next /= null then
                  Result := F.New_List (Result, Next);
               end if;
            end;
         end loop;

         return Result;
      end Get_Public;

      Result : League.Strings.Universal_String;

      Name   : constant Ada_Pretty.Node_Access :=
        F.New_Selected_Name (Package_Name (Self));

      Clauses : constant Ada_Pretty.Node_Access :=
        Dependency (Self, Request);

      Public : constant Ada_Pretty.Node_Access := Get_Public;
      Root   : constant Ada_Pretty.Node_Access :=
        F.New_Package (Name, Public, null);  --  Get_Private);
      Unit   : constant Ada_Pretty.Node_Access :=
        F.New_Compilation_Unit (Root, Clauses);
   begin
      Result := F.To_Text (Unit).Join (Ada.Characters.Wide_Wide_Latin_1.LF);
      return Result;
   end Specification_Text;

end Compiler.File_Descriptors;