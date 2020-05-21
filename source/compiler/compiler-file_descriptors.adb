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
with Ada.Directories;

with Ada_Pretty;

with League.String_Vectors;

with Compiler.Descriptors;
with Compiler.Enum_Descriptors;

package body Compiler.File_Descriptors is

   F : Ada_Pretty.Factory renames Compiler.Context.Factory;

   use type Ada_Pretty.Node_Access;

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   function Package_Name
     (Self : Google.Protobuf.Descriptor.File_Descriptor_Proto)
      return League.Strings.Universal_String;

   function Dependency
     (Self    : Google.Protobuf.Descriptor.File_Descriptor_Proto;
      Request : Google.Protobuf.Compiler.Plugin.Code_Generator_Request)
      return Ada_Pretty.Node_Access;
   --  return 'with Unit' for each dependency

   procedure Get_Used_Types
     (Self   : Google.Protobuf.Descriptor.File_Descriptor_Proto;
      Result : out Compiler.Context.String_Sets.Set);

   ---------------
   -- Body_Text --
   ---------------

   function Body_Text
     (Self : Google.Protobuf.Descriptor.File_Descriptor_Proto)
      return League.Strings.Universal_String
   is

      function Get_Subprograms return Ada_Pretty.Node_Access;
      function Get_Instances return Ada_Pretty.Node_Access;

      Pkg : constant League.Strings.Universal_String := Package_Name (Self);

      -------------------
      -- Get_Instances --
      -------------------

      function Get_Instances return Ada_Pretty.Node_Access is
         use type League.Strings.Universal_String;
         use type Compiler.Context.Ada_Type_Name;
         Info   : Compiler.Context.Named_Type;
         Types  : Compiler.Context.String_Sets.Set;
         Result : Ada_Pretty.Node_Access;
         Full   : League.Strings.Universal_String;
         Append : League.Strings.Universal_String;
         Min    : Integer;
         Max    : Integer;
      begin
         Get_Used_Types (Self, Types);
         for J of Types loop
            if Compiler.Context.Named_Types.Contains (J) then
               Info := Compiler.Context.Named_Types (J);

               if Info.Is_Enumeration then
                  Min := Info.Enum.Min;
                  Max := Info.Enum.Max;

                  Full := Compiler.Context.Relative_Name (+Info.Ada_Type, Pkg);

                  Result := F.New_List
                    (Result,
                     F.New_Type
                       (Name          => F.New_Name
                          ("Integer_" & Info.Ada_Type.Type_Name),
                        Definition    => F.New_Infix
                          (+"range",
                           F.New_List
                            (Compiler.Enum_Descriptors.Get_Literal (Min),
                             F.New_Infix
                              (+"..",
                               Compiler.Enum_Descriptors.Get_Literal (Max)))),
                        Aspects       => F.New_Argument_Association
                          (Choice => F.New_Name (+"Size"),
                           Value  => F.New_Selected_Name
                             (Full & "'Size"))));

                  Result := F.New_List
                    (Result,
                     F.New_Package_Instantiation
                       (Name        => F.New_Name
                          (Info.Ada_Type.Type_Name & "_IO"),
                        Template    => F.New_Selected_Name
                          (+"PB_Support.IO.Enum_IO"),
                        Actual_Part => F.New_List
                          ((F.New_Argument_Association
                              (F.New_Selected_Name (Full)),
                            F.New_Name
                              ("Integer_" & Info.Ada_Type.Type_Name),
                            F.New_Argument_Association
                              (F.New_Selected_Name
                                (Full & "_Vectors"))))));
               else
                  Full := Compiler.Context.Relative_Name (+Info.Ada_Type, Pkg);
                  Append := Compiler.Context.Relative_Name
                    (Info.Ada_Type.Package_Name & ".Append", Pkg);

                  Result := F.New_List
                    (Result,
                     F.New_Package_Instantiation
                       (Name        => F.New_Name
                          (Info.Ada_Type.Type_Name & "_IO"),
                        Template    => F.New_Selected_Name
                          (+"PB_Support.IO.Message_IO"),
                        Actual_Part => F.New_List
                          ((F.New_Argument_Association
                               (F.New_Selected_Name (Full)),
                            F.New_Argument_Association
                             (F.New_Selected_Name (Full & "_Vector")),
                            F.New_Argument_Association
                             (F.New_Selected_Name (Append))))));
               end if;
            end if;
         end loop;

         return Result;
      end Get_Instances;

      --------------------
      -- Get_Subprograms --
      --------------------

      function Get_Subprograms return Ada_Pretty.Node_Access is
         Next   : Ada_Pretty.Node_Access;
         Result : Ada_Pretty.Node_Access;
      begin
         for J in 1 .. Self.Message_Type.Length loop
            Next := Compiler.Descriptors.Subprograms
              (Self.Message_Type (J), Pkg);
            Result := F.New_List (Result, Next);
         end loop;

         return Result;
      end Get_Subprograms;

      Result : League.Strings.Universal_String;

      Name   : constant Ada_Pretty.Node_Access := F.New_Selected_Name (Pkg);

      Root   : constant Ada_Pretty.Node_Access :=
        F.New_Package_Body
          (Name,
           F.New_List
             (Get_Instances, Get_Subprograms));

      With_Clauses : constant Ada_Pretty.Node_Access :=
        F.New_List
          ((F.New_With (F.New_Selected_Name (+"Ada.Unchecked_Deallocation")),
            F.New_With (F.New_Selected_Name (+"PB_Support.IO")),
            F.New_With (F.New_Selected_Name (+"PB_Support.Internal"))));

      Unit   : constant Ada_Pretty.Node_Access :=
        F.New_Compilation_Unit
          (Root,
           Clauses => With_Clauses);
   begin
      Result := F.To_Text (Unit).Join (Ada.Characters.Wide_Wide_Latin_1.LF);
      return Result;
   end Body_Text;

   ----------------
   -- Dependency --
   ----------------

   function Dependency
     (Self    : Google.Protobuf.Descriptor.File_Descriptor_Proto;
      Request : Google.Protobuf.Compiler.Plugin.Code_Generator_Request)
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
      Compiler.Context.Is_Proto_2 := not Self.Syntax.Is_Set or else
        Self.Syntax.Value /= +"proto3";

      for J in 1 .. Self.Dependency.Length loop
         declare
            Item : constant Google.Protobuf.Descriptor.File_Descriptor_Proto :=
              Compiler.Context.Get_File
                (Request, Self.Dependency.Element (J));
         begin
            Set.Include (Package_Name (Item));
         end;
      end loop;

      for J in 1 .. Self.Message_Type.Length loop
         Compiler.Descriptors.Dependency (Self.Message_Type (J), Set);
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
     (Self : Google.Protobuf.Descriptor.File_Descriptor_Proto)
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

   --------------------
   -- Get_Used_Types --
   --------------------

   procedure Get_Used_Types
     (Self   : Google.Protobuf.Descriptor.File_Descriptor_Proto;
      Result : out Compiler.Context.String_Sets.Set) is
   begin
      for J in 1 .. Self.Message_Type.Length loop
         Compiler.Descriptors.Get_Used_Types
           (Self.Message_Type (J), Result);
      end loop;
   end Get_Used_Types;

   ------------------
   -- Package_Name --
   ------------------

   function Package_Name
     (Self : Google.Protobuf.Descriptor.File_Descriptor_Proto)
      return League.Strings.Universal_String
   is
      File_Name : constant String := Self.Name.Value.To_UTF_8_String;
      Base_Name : constant String := Ada.Directories.Base_Name (File_Name);
      PB_Pkg    : League.Strings.Universal_String;
      Result    : League.Strings.Universal_String :=
        Compiler.Context.To_Ada_Name
          (League.Strings.From_UTF_8_String (Base_Name));
   begin
      if Self.PB_Package.Is_Set then
         PB_Pkg := Self.PB_Package.Value;
         PB_Pkg := Compiler.Context.To_Selected_Ada_Name (PB_Pkg);
         PB_Pkg.Append (".");
         Result.Prepend (PB_Pkg);
      end if;

      return Result;
   end Package_Name;

   --------------------------
   -- Populate_Named_Types --
   --------------------------

   procedure Populate_Named_Types
     (Self : Google.Protobuf.Descriptor.File_Descriptor_Proto;
      Map  : in out Compiler.Context.Named_Type_Maps.Map)
   is
      use type League.Strings.Universal_String;

      PB_Package : League.Strings.Universal_String := +".";

      Ada_Package : constant League.Strings.Universal_String :=
        Package_Name (Self);
   begin
      if Self.PB_Package.Is_Set then
         PB_Package.Append (Self.PB_Package.Value);
      end if;

      for J in 1 .. Self.Message_Type.Length loop
         Compiler.Descriptors.Populate_Named_Types
           (Self   => Self.Message_Type (J),
            PB_Prefix => PB_Package,
            Ada_Package => Ada_Package,
            Map    => Map);
      end loop;

      for J in 1 .. Self.Enum_Type.Length loop
         Compiler.Enum_Descriptors.Populate_Named_Types
           (Self        => Self.Enum_Type (J),
            PB_Prefix   => PB_Package,
            Ada_Package => Ada_Package,
            Map         => Map);
      end loop;
   end Populate_Named_Types;

   ------------------------
   -- Specification_Text --
   ------------------------

   function Specification_Text
     (Self    : Google.Protobuf.Descriptor.File_Descriptor_Proto;
      Request : Google.Protobuf.Compiler.Plugin.Code_Generator_Request)
      return League.Strings.Universal_String
   is
      function Get_Public return Ada_Pretty.Node_Access;
      --  Generate public part declaration list
      function Get_Private return Ada_Pretty.Node_Access;
      --  Generate private part declaration list

      function Get_Private return Ada_Pretty.Node_Access is
         Result : Ada_Pretty.Node_Access;
         Next   : Ada_Pretty.Node_Access;
      begin
         for J in 1 .. Self.Message_Type.Length loop
            Next := Compiler.Descriptors.Private_Spec
              (Self.Message_Type (J));
            Result := F.New_List (Result, Next);
         end loop;

         return Result;
      end Get_Private;

      ----------------
      -- Get_Public --
      ----------------

      function Get_Public return Ada_Pretty.Node_Access is
         Pkg : constant League.Strings.Universal_String := Package_Name (Self);

         Result : Ada_Pretty.Node_Access;
         Next   : Ada_Pretty.Node_Access;
         Vector : Ada_Pretty.Node_Access;
         Done   : Compiler.Context.String_Sets.Set;
         Again  : Boolean := True;
         Force  : Natural := 0;
         Going  : Boolean;
      begin
         for J in 1 .. Self.Enum_Type.Length loop
            Next := Compiler.Enum_Descriptors.Public_Spec (Self.Enum_Type (J));

            if Next /= null then
               Result := F.New_List (Result, Next);
            end if;
         end loop;

         for J in 1 .. Self.Message_Type.Length loop
            Next := Compiler.Descriptors.Enum_Types (Self.Message_Type (J));

            if Next /= null then
               Result := F.New_List (Result, Next);
            end if;

            Next := Compiler.Descriptors.Vector_Declarations
              (Self.Message_Type (J));

            Vector := F.New_List (Vector, Next);
         end loop;

         if Vector /= null then
            Result := F.New_List (Result, Vector);
         end if;

         while Again loop
            Again := False;
            Going := False;

            for J in 1 .. Self.Message_Type.Length loop
               Compiler.Descriptors.Public_Spec
                 (Self.Message_Type (J), Pkg, Next, Again, Done, Force);

               if Next /= null then
                  Result := F.New_List (Result, Next);
                  Force := 0;
                  Going := True;
               end if;
            end loop;

            if not Going then
               Force := Force + 1;
            end if;
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
        F.New_Package (Name, Public, Get_Private);
      Unit   : constant Ada_Pretty.Node_Access :=
        F.New_Compilation_Unit (Root, Clauses);
   begin
      Result := F.To_Text (Unit).Join (Ada.Characters.Wide_Wide_Latin_1.LF);

      return Result;
   end Specification_Text;

end Compiler.File_Descriptors;
