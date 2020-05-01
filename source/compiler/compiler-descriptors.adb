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

with Compiler.Enum_Descriptors;
with Compiler.Field_Descriptors;

package body Compiler.Descriptors is

   F : Ada_Pretty.Factory renames Compiler.Context.Factory;

   use type Ada_Pretty.Node_Access;
   use type League.Strings.Universal_String;

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   function Type_Name
     (Self : Google.Protobuf.Descriptor_Proto)
      return League.Strings.Universal_String;
   --  Return Ada type (simple) name

   function Check_Dependency
     (Self   : Google.Protobuf.Descriptor_Proto;
      Pkg    : League.Strings.Universal_String;
      Done   : Compiler.Context.String_Sets.Set) return Boolean;

   function Public_Spec
     (Self : Google.Protobuf.Descriptor_Proto)
      return Ada_Pretty.Node_Access;

   ----------------
   -- Enum_Types --
   ----------------

   function Enum_Types
     (Self : Google.Protobuf.Descriptor_Proto)
      return Ada_Pretty.Node_Access
   is
      Result : Ada_Pretty.Node_Access;
      Item   : Ada_Pretty.Node_Access;
   begin
      for J in 1 .. Self.Enum_Type.Length loop
         Item := Compiler.Enum_Descriptors.Public_Spec
           (Self.Enum_Type.Get (J));

         Result := F.New_List (Result, Item);
      end loop;

      for J in 1 .. Self.Nested_Type.Length loop
         Item := Enum_Types (Self.Nested_Type.Get (J));

         if Item /= null then
            Result := F.New_List (Result, Item);
         end if;
      end loop;

      return Result;
   end Enum_Types;

   ----------------------
   -- Check_Dependency --
   ----------------------

   function Check_Dependency
     (Self   : Google.Protobuf.Descriptor_Proto;
      Pkg    : League.Strings.Universal_String;
      Done   : Compiler.Context.String_Sets.Set) return Boolean
   is
      Name : constant League.Strings.Universal_String := Type_Name (Self);
   begin
      for J in 1 .. Self.Field.Length loop
         declare
            use all type Google.Protobuf.Label;
            Field : constant Google.Protobuf.Field_Descriptor_Proto :=
              Self.Field.Get (J);
            Type_Name : constant League.Strings.Universal_String :=
              Field.Type_Name;
         begin
            if not (Type_Name.Is_Empty
              or else Done.Contains (Type_Name)
              or else Compiler.Context.Named_Types (Type_Name).Is_Enumeration
              or else Compiler.Context.Named_Types (Type_Name).
                           Ada_Type.Package_Name /= Pkg
              or else (Compiler.Context.Named_Types (Type_Name).
                           Ada_Type.Type_Name = Name
                      and Field.Label = LABEL_REPEATED))
            then
               return False;
            end if;
         end;
      end loop;

      return True;
   end Check_Dependency;

   ----------------
   -- Dependency --
   ----------------

   procedure Dependency
     (Self   : Google.Protobuf.Descriptor_Proto;
      Result : in out Compiler.Context.String_Sets.Set)
   is
   begin
      Result.Include (+"Ada.Finalization");
      Result.Include (+"Ada.Streams");

      if Self.Enum_Type.Length > 0 then
         Result.Include (+"PB_Support.Vectors");
      end if;

      for J in 1 .. Self.Field.Length loop
         Compiler.Field_Descriptors.Dependency (Self.Field.Get (J), Result);
      end loop;

      for J in 1 .. Self.Nested_Type.Length loop
         Dependency (Self.Nested_Type.Get (J), Result);
      end loop;
   end Dependency;

   --------------------------
   -- Populate_Named_Types --
   --------------------------

   procedure Populate_Named_Types
     (Self        : Google.Protobuf.Descriptor_Proto;
      PB_Prefix   : League.Strings.Universal_String;
      Ada_Package : League.Strings.Universal_String;
      Map         : in out Compiler.Context.Named_Type_Maps.Map)
   is
      Name  : constant League.Strings.Universal_String := Type_Name (Self);
      Key   : League.Strings.Universal_String := PB_Prefix;
      Value : constant Compiler.Context.Named_Type :=
        (Is_Enumeration => False,
         Ada_Type       =>
           (Package_Name => Ada_Package,
            Type_Name    => Name));
   begin
      Key.Append (".");
      Key.Append (Self.Name);
      Map.Insert (Key, Value);

      for J in 1 .. Self.Nested_Type.Length loop
         Populate_Named_Types
           (Self        => Self.Nested_Type.Get (J),
            PB_Prefix   => Key,
            Ada_Package => Ada_Package,
            Map         => Map);
      end loop;

      for J in 1 .. Self.Enum_Type.Length loop
         Compiler.Enum_Descriptors.Populate_Named_Types
           (Self        => Self.Enum_Type.Get (J),
            PB_Prefix   => Key,
            Ada_Package => Ada_Package,
            Map         => Map);
      end loop;
   end Populate_Named_Types;

   -----------------
   -- Public_Spec --
   -----------------

   function Public_Spec
     (Self : Google.Protobuf.Descriptor_Proto)
      return Ada_Pretty.Node_Access
   is
      My_Name : constant League.Strings.Universal_String := Type_Name (Self);
      Me     : constant Ada_Pretty.Node_Access := F.New_Name (My_Name);
      V_Name : Ada_Pretty.Node_Access;
      P_Self : Ada_Pretty.Node_Access;
      Is_Set : Ada_Pretty.Node_Access;
      Vector : Ada_Pretty.Node_Access;
      Getter : Ada_Pretty.Node_Access;
      Count  : Ada_Pretty.Node_Access;
      Clear  : Ada_Pretty.Node_Access;
      Append : Ada_Pretty.Node_Access;
      Option : Ada_Pretty.Node_Access;
      Result : Ada_Pretty.Node_Access;
      Item   : Ada_Pretty.Node_Access;
   begin
      for J in 1 .. Self.Field.Length loop
         Item := Compiler.Field_Descriptors.Component (Self.Field.Get (J));

         Result := F.New_List (Result, Item);
      end loop;

      Result := F.New_Type
        (F.New_Name (My_Name),
         Definition => F.New_Record (Components => Result));

      Is_Set := F.New_Name (+"Is_Set");

      Option := F.New_Type
        (Name          => F.New_Name ("Optional_" & My_Name),
         Discriminants => F.New_Parameter
           (Name            => Is_Set,
            Type_Definition => F.New_Name (+"Boolean"),
            Initialization  => F.New_Name (+"False")),
         Definition    => F.New_Record
           (Components => F.New_Case
              (Expression => Is_Set,
               List       => F.New_List
                 (F.New_Case_Path
                      (Choice => F.New_Name (+"True"),
                       List   => F.New_Variable
                         (Name            => F.New_Name (+"Value"),
                          Type_Definition => Me)),
                  F.New_Case_Path
                    (Choice => F.New_Name (+"False"),
                     List   => F.New_Statement)))));

      V_Name := F.New_Name (My_Name & "_Vector");

      Vector := F.New_Type
        (Name        => V_Name,
         Definition  => F.New_Private_Record
           (Is_Tagged   => True));

      P_Self := F.New_Name (+"Self");

      Count := F.New_Subprogram_Declaration
        (F.New_Subprogram_Specification
           (Name       => F.New_Name (+"Length"),
            Parameters => F.New_Parameter
              (Name            => P_Self,
               Type_Definition => V_Name),
            Result     => F.New_Name (+"Natural")));

      Getter := F.New_Subprogram_Declaration
        (F.New_Subprogram_Specification
           (Name       => F.New_Name (+"Get"),
            Parameters => F.New_List
              (F.New_Parameter
                 (Name            => P_Self,
                  Type_Definition => V_Name),
               F.New_Parameter
                 (Name            => F.New_Name (+"Index"),
                  Type_Definition => F.New_Name (+"Positive"))),
            Result     => Me));

      Clear := F.New_Subprogram_Declaration
        (F.New_Subprogram_Specification
           (Name       => F.New_Name (+"Clear"),
            Parameters => F.New_Parameter
              (Name            => P_Self,
               Type_Definition => V_Name,
               Is_In           => True,
               Is_Out          => True)));

      Append := F.New_Subprogram_Declaration
        (F.New_Subprogram_Specification
           (Name       => F.New_Name (+"Append"),
            Parameters => F.New_List
              (F.New_Parameter
                 (Name            => P_Self,
                  Type_Definition => V_Name,
                  Is_In           => True,
                  Is_Out          => True),
               F.New_Parameter
                 (F.New_Name (+"Value"), Me))));

      Result := F.New_List
        ((Vector, Result, Option, Count, Getter, Clear, Append));

      return Result;
   end Public_Spec;

   -----------------
   -- Public_Spec --
   -----------------

   procedure Public_Spec
     (Self   : Google.Protobuf.Descriptor_Proto;
      Pkg    : League.Strings.Universal_String;
      Result : out Ada_Pretty.Node_Access;
      Again  : in out Boolean;
      Done   : in out Compiler.Context.String_Sets.Set)
   is
      Name : constant League.Strings.Universal_String := Type_Name (Self);
      Item : Ada_Pretty.Node_Access;
   begin
      for J in 1 .. Self.Nested_Type.Length loop
         Public_Spec (Self.Nested_Type.Get (J), Pkg, Item, Again, Done);

         if Item /= null then
            Result := F.New_List (Result, Item);
         end if;
      end loop;

      if not Done.Contains (Name) then
         if Check_Dependency (Self, Pkg, Done) then
            Result := F.New_List (Result, Public_Spec (Self));
            Done.Insert (Name);
         end if;
      end if;
   end Public_Spec;

   ---------------
   -- Type_Name --
   ---------------

   function Type_Name
     (Self : Google.Protobuf.Descriptor_Proto)
      return League.Strings.Universal_String
   is
      Result : constant League.Strings.Universal_String :=
        Compiler.Context.To_Ada_Name (Self.Name);
   begin
      if Result.Is_Empty then
         return +"Message";
      else
         return Result;
      end if;
   end Type_Name;

end Compiler.Descriptors;
