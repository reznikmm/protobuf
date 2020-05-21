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

with PB_Support.Integer_32_Vectors;

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
     (Self : Google.Protobuf.Descriptor.Descriptor_Proto)
      return League.Strings.Universal_String;
   --  Return Ada type (simple) name

   function Check_Dependency
     (Self  : Google.Protobuf.Descriptor.Descriptor_Proto;
      Pkg   : League.Strings.Universal_String;
      Done  : Compiler.Context.String_Sets.Set;
      Force : Natural;
      Fake  : in out Compiler.Context.String_Sets.Set) return Boolean;

   function Public_Spec
     (Self : Google.Protobuf.Descriptor.Descriptor_Proto;
      Pkg  : League.Strings.Universal_String;
      Fake : Compiler.Context.String_Sets.Set)
      return Ada_Pretty.Node_Access;

   function Read_Subprogram
     (Self : Google.Protobuf.Descriptor.Descriptor_Proto;
      Pkg  : League.Strings.Universal_String;
      Fake : Compiler.Context.String_Sets.Set)
      return Ada_Pretty.Node_Access;

   function Write_Subprogram
     (Self : Google.Protobuf.Descriptor.Descriptor_Proto;
      Pkg  : League.Strings.Universal_String;
      Fake : Compiler.Context.String_Sets.Set)
      return Ada_Pretty.Node_Access;

   procedure One_Of_Declaration
     (Self      : Google.Protobuf.Descriptor.Descriptor_Proto;
      Index     : Positive;
      Pkg       : League.Strings.Universal_String;
      Fake      : Compiler.Context.String_Sets.Set;
      Types     : in out Ada_Pretty.Node_Access;
      Component : in out Ada_Pretty.Node_Access);

   function Is_One_Of
     (Value : PB_Support.Integer_32_Vectors.Option;
      Index : Positive) return Boolean;

   function Indexing_Spec
     (Self     : Google.Protobuf.Descriptor.Descriptor_Proto;
      Prefix   : League.Strings.Universal_String;
      Variable : Boolean) return Ada_Pretty.Node_Access;

   function Indexing_Body
     (Self     : Google.Protobuf.Descriptor.Descriptor_Proto;
      Prefix   : League.Strings.Universal_String;
      Variable : Boolean) return Ada_Pretty.Node_Access;

   ----------------
   -- Enum_Types --
   ----------------

   function Enum_Types
     (Self : Google.Protobuf.Descriptor.Descriptor_Proto)
      return Ada_Pretty.Node_Access
   is
      Result : Ada_Pretty.Node_Access;
      Item   : Ada_Pretty.Node_Access;
   begin
      for J in 1 .. Self.Enum_Type.Length loop
         Item := Compiler.Enum_Descriptors.Public_Spec (Self.Enum_Type (J));

         Result := F.New_List (Result, Item);
      end loop;

      for J in 1 .. Self.Nested_Type.Length loop
         Item := Enum_Types (Self.Nested_Type (J));

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
     (Self  : Google.Protobuf.Descriptor.Descriptor_Proto;
      Pkg   : League.Strings.Universal_String;
      Done  : Compiler.Context.String_Sets.Set;
      Force : Natural;
      Fake  : in out Compiler.Context.String_Sets.Set) return Boolean
   is
      Required : Compiler.Context.String_Sets.Set;
      Tipe     : constant League.Strings.Universal_String := Type_Name (Self);
   begin
      for J in 1 .. Self.Field.Length loop
         declare
            use all type Google.Protobuf.Descriptor.Label;
            Field : constant Google.Protobuf.Descriptor.Field_Descriptor_Proto
              := Self.Field (J);
            Type_Name  : League.Strings.Universal_String;
            Named_Type : Compiler.Context.Named_Type;
         begin
            if Field.Type_Name.Is_Set then
               Type_Name := Field.Type_Name.Value;
            end if;

            if not Compiler.Context.Named_Types.Contains (Type_Name) then
               null;
            else
               Named_Type := Compiler.Context.Named_Types (Type_Name);

               if not (Done.Contains (Named_Type.Ada_Type.Type_Name)
                       or else Named_Type.Is_Enumeration
                       or else Named_Type.Ada_Type.Package_Name /= Pkg
                       or else
                         (Field.Label.Is_Set
                            and then Field.Label.Value = LABEL_REPEATED))
               then
                  Required.Insert
                    (Compiler.Field_Descriptors.Unique_Id
                       (Field, Pkg, Tipe));
               end if;
            end if;
         end;
      end loop;

      if Natural (Required.Length) <= Force then
         Fake.Union (Required);
         return True;
      else
         return False;
      end if;
   end Check_Dependency;

   ----------------
   -- Dependency --
   ----------------

   procedure Dependency
     (Self   : Google.Protobuf.Descriptor.Descriptor_Proto;
      Result : in out Compiler.Context.String_Sets.Set)
   is
   begin
      Result.Include (+"Ada.Finalization");
      Result.Include (+"Ada.Streams");

      if Self.Enum_Type.Length > 0 then
         Result.Include (+"PB_Support.Vectors");
      end if;

      for J in 1 .. Self.Field.Length loop
         Compiler.Field_Descriptors.Dependency (Self.Field (J), Result);
      end loop;

      for J in 1 .. Self.Nested_Type.Length loop
         Dependency (Self.Nested_Type (J), Result);
      end loop;
   end Dependency;

   --------------------
   -- Get_Used_Types --
   --------------------

   procedure Get_Used_Types
     (Self   : Google.Protobuf.Descriptor.Descriptor_Proto;
      Result : in out Compiler.Context.String_Sets.Set) is
   begin
      for J in 1 .. Self.Field.Length loop
         Compiler.Field_Descriptors.Get_Used_Types (Self.Field (J), Result);
      end loop;

      for J in 1 .. Self.Nested_Type.Length loop
         Get_Used_Types (Self.Nested_Type (J), Result);
      end loop;
   end Get_Used_Types;

   -------------------
   -- Indexing_Body --
   -------------------

   function Indexing_Body
     (Self     : Google.Protobuf.Descriptor.Descriptor_Proto;
      Prefix   : League.Strings.Universal_String;
      Variable : Boolean) return Ada_Pretty.Node_Access
   is
      My_Name  : constant League.Strings.Universal_String := Type_Name (Self);
      Ref_Name : constant League.Strings.Universal_String :=
        My_Name & "_" & Prefix & "_Reference";
      Result   : Ada_Pretty.Node_Access;
   begin
      Result := F.New_Subprogram_Body
        (Specification =>
          F.New_Subprogram_Specification
           (Name          => F.New_Name ("Get_" & Ref_Name),
            Is_Overriding => Ada_Pretty.False,
            Parameters    => F.New_List
             (F.New_Parameter
               (Name            => F.New_Name (+"Self"),
                Type_Definition => F.New_Name (My_Name & "_Vector"),
                Is_In           => Variable,
                Is_Out          => Variable,
                Is_Aliased      => True),
              F.New_Parameter
               (Name            => F.New_Name (+"Index"),
                Type_Definition => F.New_Name (+"Positive"))),
            Result        => F.New_Name (Ref_Name)),
         Statements    => F.New_Return
           (F.New_Parentheses
                (F.New_Argument_Association
                     (Value  => F.New_Name (+"Self.Data (Index)'Access"),
                      Choice => F.New_Name (+"Element")))));

      return Result;
   end Indexing_Body;

   -------------------
   -- Indexing_Spec --
   -------------------

   function Indexing_Spec
     (Self     : Google.Protobuf.Descriptor.Descriptor_Proto;
      Prefix   : League.Strings.Universal_String;
      Variable : Boolean) return Ada_Pretty.Node_Access
   is
      My_Name  : constant League.Strings.Universal_String := Type_Name (Self);
      Ref_Name : constant League.Strings.Universal_String :=
        My_Name & "_" & Prefix & "_Reference";
      Map      : constant array (Boolean) of Ada_Pretty.Access_Modifier :=
        (True => Ada_Pretty.Unspecified,
         False => Ada_Pretty.Access_Constant);
      Result   : Ada_Pretty.Node_Access;
   begin
      Result := F.New_Type
        (Name          => F.New_Name (Ref_Name),
         Discriminants => F.New_Parameter
           (Name            => F.New_Name (+"Element"),
            Type_Definition => F.New_Null_Exclusion
              (F.New_Access
                (Target   => F.New_Name (My_Name),
                 Modifier => Map (Variable)))),
         Definition    => F.New_Record,
         Aspects       => F.New_Argument_Association
           (Choice => F.New_Name (+"Implicit_Dereference"),
            Value  => F.New_Name (+"Element")));

      Result := F.New_List
        (Result,
         F.New_Subprogram_Declaration
          (Specification =>
           F.New_Subprogram_Specification
            (Name          => F.New_Name ("Get_" & Ref_Name),
             Is_Overriding => Ada_Pretty.False,
             Parameters    => F.New_List
              (F.New_Parameter
                (Name            => F.New_Name (+"Self"),
                 Type_Definition => F.New_Name (My_Name & "_Vector"),
                 Is_In           => Variable,
                 Is_Out          => Variable,
                 Is_Aliased      => True),
               F.New_Parameter
                (Name            => F.New_Name (+"Index"),
                 Type_Definition => F.New_Name (+"Positive"))),
            Result        => F.New_Name (Ref_Name)),
            Aspects       => F.New_Name (+"Inline")));

      return Result;
   end Indexing_Spec;

   ---------------
   -- Is_One_Of --
   ---------------

   function Is_One_Of
     (Value : PB_Support.Integer_32_Vectors.Option;
      Index : Positive) return Boolean
   is
   begin
      return Value.Is_Set and then Natural (Value.Value) + 1 = Index;
   end Is_One_Of;

   ------------------------
   -- One_Of_Declaration --
   ------------------------

   procedure One_Of_Declaration
     (Self      : Google.Protobuf.Descriptor.Descriptor_Proto;
      Index     : Positive;
      Pkg       : League.Strings.Universal_String;
      Fake      : Compiler.Context.String_Sets.Set;
      Types     : in out Ada_Pretty.Node_Access;
      Component : in out Ada_Pretty.Node_Access)
   is
      My_Name : constant League.Strings.Universal_String := Type_Name (Self);
      Item_Name : constant League.Strings.Universal_String :=
        Compiler.Context.To_Ada_Name (Self.Oneof_Decl (Index).Name.Value);
      Next : Ada_Pretty.Node_Access;
      Name : constant League.Strings.Universal_String :=
        My_Name & "_Variant";
      Choices : Ada_Pretty.Node_Access;
   begin
      Next := F.New_Name (Item_Name & "_Not_Set");

      Choices :=
        F.New_Case_Path
          (Next,
           F.New_Statement);

      for J in 1 .. Self.Field.Length loop
         declare
            Field : constant Google.Protobuf.Descriptor.Field_Descriptor_Proto
              := Self.Field (J);
            Literal : League.Strings.Universal_String;

         begin
            if Is_One_Of (Field.Oneof_Index, Index) then
               Literal := Compiler.Context.To_Ada_Name (Field.Name.Value) &
                 "_Kind";
               Next := F.New_List
                 (Next,
                  F.New_Argument_Association (F.New_Name (Literal)));

               Choices := F.New_List
                 (Choices,
                  F.New_Case_Path
                    (F.New_Name (Literal),
                     Compiler.Field_Descriptors.Component
                       (Field, Pkg, My_Name, Fake)));
            end if;
         end;
      end loop;

      Types := F.New_List
        (Types,
         F.New_Type
           (Name          => F.New_Name (Name & "_Kind"),
            Definition    => F.New_Parentheses (Next)));

      Types := F.New_List
        (Types,
         F.New_Type
           (Name          => F.New_Name (Name),
            Discriminants =>
              F.New_Parameter
                (Name            => F.New_Name (Item_Name),
                 Type_Definition => F.New_Name (Name & "_Kind"),
                 Initialization  => F.New_Name (Item_Name & "_Not_Set")),
            Definition    => F.New_Record
              (Components =>
                F.New_Case
                 (Expression => F.New_Name (Item_Name),
                  List       => Choices))));

      Component := F.New_List
        (Component,
         F.New_Variable
           (Name            => F.New_Name (+"Variant"),
            Type_Definition => F.New_Name (Name)));
   end One_Of_Declaration;

   --------------------------
   -- Populate_Named_Types --
   --------------------------

   procedure Populate_Named_Types
     (Self        : Google.Protobuf.Descriptor.Descriptor_Proto;
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

      if Self.Name.Is_Set then
         Key.Append (Self.Name.Value);
      end if;

      Map.Insert (Key, Value);

      for J in 1 .. Self.Nested_Type.Length loop
         Populate_Named_Types
           (Self        => Self.Nested_Type (J),
            PB_Prefix   => Key,
            Ada_Package => Ada_Package,
            Map         => Map);
      end loop;

      for J in 1 .. Self.Enum_Type.Length loop
         Compiler.Enum_Descriptors.Populate_Named_Types
           (Self        => Self.Enum_Type (J),
            PB_Prefix   => Key,
            Ada_Package => Ada_Package,
            Map         => Map);
      end loop;
   end Populate_Named_Types;

   ------------------
   -- Private_Spec --
   ------------------

   function Private_Spec
     (Self : Google.Protobuf.Descriptor.Descriptor_Proto)
      return Ada_Pretty.Node_Access
   is
      My_Name : constant League.Strings.Universal_String := Type_Name (Self);
      T_Array : Ada_Pretty.Node_Access;
      Array_Access : Ada_Pretty.Node_Access;
      Result : Ada_Pretty.Node_Access;
      Item   : Ada_Pretty.Node_Access;
      Read   : Ada_Pretty.Node_Access;
      Write  : Ada_Pretty.Node_Access;
      Use_R  : Ada_Pretty.Node_Access;
      Use_W  : Ada_Pretty.Node_Access;
      Adjust : Ada_Pretty.Node_Access;
      Final  : Ada_Pretty.Node_Access;
   begin
      for J in 1 .. Self.Nested_Type.Length loop
         Item := Private_Spec (Self.Nested_Type (J));
         Result := F.New_List (Result, Item);
      end loop;

      Read := F.New_Subprogram_Declaration
        (F.New_Subprogram_Specification
           (Name          => F.New_Name ("Read_" & My_Name),
            Parameters    => F.New_List
              (F.New_Parameter
                   (Name            => F.New_Name (+"Stream"),
                    Type_Definition => F.New_Selected_Name
                      (+"access Ada.Streams.Root_Stream_Type'Class")),
               F.New_Parameter
                   (Name            => F.New_Name (+"V"),
                    Type_Definition => F.New_Name (My_Name),
                    Is_Out          => True))));

      Write := F.New_Subprogram_Declaration
        (F.New_Subprogram_Specification
           (Name          => F.New_Name ("Write_" & My_Name),
            Parameters    => F.New_List
              (F.New_Parameter
                   (Name            => F.New_Name (+"Stream"),
                    Type_Definition => F.New_Selected_Name
                      (+"access Ada.Streams.Root_Stream_Type'Class")),
               F.New_Parameter
                   (Name            => F.New_Name (+"V"),
                    Type_Definition => F.New_Name (My_Name)))));

      Use_R := F.New_Statement
        (F.New_Name ("for " & My_Name & "'Read use Read_" & My_Name));

      Use_W := F.New_Statement
        (F.New_Name ("for " & My_Name & "'Write use Write_" & My_Name));

      T_Array := F.New_Type
        (Name          => F.New_Name (My_Name & "_Array"),
         Definition    => F.New_Array
           (Indexes   => F.New_Name (+"Positive range <>"),
            Component => F.New_Name ("aliased " & My_Name)));

      Array_Access := F.New_Type
        (Name          => F.New_Name (My_Name & "_Array_Access"),
         Definition    => F.New_Access
           (Target   => F.New_Name (My_Name & "_Array")));

      Item := F.New_Type
        (F.New_Name (Type_Name (Self) & "_Vector"),
         Definition => F.New_Record
           (Parent      => F.New_Selected_Name
                (+"Ada.Finalization.Controlled"),
            Components  => F.New_List
              (F.New_Variable
                  (Name            => F.New_Name (+"Data"),
                   Type_Definition => F.New_Name (My_Name & "_Array_Access")),
               F.New_Variable
                  (Name            => F.New_Name (+"Length"),
                   Type_Definition => F.New_Name (+"Natural"),
                   Initialization  => F.New_Literal (0)))));

      Adjust := F.New_Subprogram_Declaration
        (F.New_Subprogram_Specification
           (Is_Overriding => Ada_Pretty.True,
            Name          => F.New_Name (+"Adjust"),
            Parameters    => F.New_Parameter
              (Name            => F.New_Name (+"Self"),
               Type_Definition => F.New_Name (Type_Name (Self) & "_Vector"),
               Is_In           => True,
               Is_Out          => True)));

      Final := F.New_Subprogram_Declaration
        (F.New_Subprogram_Specification
           (Is_Overriding => Ada_Pretty.True,
            Name          => F.New_Name (+"Finalize"),
            Parameters    => F.New_Parameter
              (Name            => F.New_Name (+"Self"),
               Type_Definition => F.New_Name (Type_Name (Self) & "_Vector"),
               Is_In           => True,
               Is_Out          => True)));

      Result := F.New_List
        (Result,
         F.New_List
           ((Read, Write, Use_R, Use_W,
            T_Array, Array_Access, Item, Adjust, Final)));

      return Result;
   end Private_Spec;

   -----------------
   -- Public_Spec --
   -----------------

   function Public_Spec
     (Self : Google.Protobuf.Descriptor.Descriptor_Proto;
      Pkg  : League.Strings.Universal_String;
      Fake : Compiler.Context.String_Sets.Set)
      return Ada_Pretty.Node_Access
   is
      My_Name : constant League.Strings.Universal_String := Type_Name (Self);
      Me     : constant Ada_Pretty.Node_Access := F.New_Name (My_Name);
      V_Name : Ada_Pretty.Node_Access;
      P_Self : Ada_Pretty.Node_Access;
      Is_Set : Ada_Pretty.Node_Access;
      Count  : Ada_Pretty.Node_Access;
      Clear  : Ada_Pretty.Node_Access;
      Append : Ada_Pretty.Node_Access;
      Option : Ada_Pretty.Node_Access;
      Result : Ada_Pretty.Node_Access;
      Item   : Ada_Pretty.Node_Access;
      One_Of : Ada_Pretty.Node_Access;

      Indexing : Ada_Pretty.Node_Access;
   begin
      for J in 1 .. Self.Field.Length loop
         if not Self.Field (J).Oneof_Index.Is_Set then
            Item := Compiler.Field_Descriptors.Component
              (Self.Field (J), Pkg, My_Name, Fake);

            Result := F.New_List (Result, Item);
         end if;
      end loop;

      for J in 1 .. Self.Oneof_Decl.Length loop
         One_Of_Declaration (Self, J, Pkg, Fake, One_Of, Result);
      end loop;

      Result := F.New_List
        (One_Of,
         F.New_Type
           (F.New_Name (My_Name),
            Definition => F.New_Record (Components => Result)));

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
                          Type_Definition =>
                            F.New_Selected_Name
                             (Compiler.Context.Relative_Name
                               (Pkg & "." & My_Name, Pkg)))),
                  F.New_Case_Path
                    (Choice => F.New_Name (+"False"),
                     List   => F.New_Statement)))));

      V_Name := F.New_Name (My_Name & "_Vector");

      P_Self := F.New_Name (+"Self");

      Count := F.New_Subprogram_Declaration
        (F.New_Subprogram_Specification
           (Name       => F.New_Name (+"Length"),
            Parameters => F.New_Parameter
              (Name            => P_Self,
               Type_Definition => V_Name),
            Result     => F.New_Name (+"Natural")));

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
                 (F.New_Name (+"V"), Me))));

      Indexing := F.New_List
        (Indexing_Spec (Self, +"Variable", True),
         Indexing_Spec (Self, +"Constant", False));

      Result := F.New_List ((Result, Option, Count, Clear, Append, Indexing));

      return Result;
   end Public_Spec;

   -----------------
   -- Public_Spec --
   -----------------

   procedure Public_Spec
     (Self   : Google.Protobuf.Descriptor.Descriptor_Proto;
      Pkg    : League.Strings.Universal_String;
      Result : out Ada_Pretty.Node_Access;
      Again  : in out Boolean;
      Done   : in out Compiler.Context.String_Sets.Set;
      Force  : in out Natural)
   is
      Name : constant League.Strings.Universal_String := Type_Name (Self);
      Item : Ada_Pretty.Node_Access;
      Fake : Compiler.Context.String_Sets.Set renames
        Compiler.Context.Fake;
   begin
      Result := null;

      for J in 1 .. Self.Nested_Type.Length loop
         Public_Spec (Self.Nested_Type (J), Pkg, Item, Again, Done, Force);

         if Item /= null then
            Result := F.New_List (Result, Item);
            Force := 0;
         end if;
      end loop;

      if not Done.Contains (Name) then
         if Check_Dependency (Self, Pkg, Done, Force, Fake) then
            Result := F.New_List (Result, Public_Spec (Self, Pkg, Fake));
            Done.Insert (Name);
         else
            Again := True;
         end if;
      end if;
   end Public_Spec;

   ---------------------
   -- Read_Subprogram --
   ---------------------

   function Read_Subprogram
     (Self : Google.Protobuf.Descriptor.Descriptor_Proto;
      Pkg  : League.Strings.Universal_String;
      Fake : Compiler.Context.String_Sets.Set)
      return Ada_Pretty.Node_Access
   is
      function Oneof_Name
        (Field : Google.Protobuf.Descriptor.Field_Descriptor_Proto)
         return League.Strings.Universal_String;

      function Oneof_Name
        (Field : Google.Protobuf.Descriptor.Field_Descriptor_Proto)
         return League.Strings.Universal_String is
      begin
         if Field.Oneof_Index.Is_Set then
            return Compiler.Context.To_Ada_Name
              (Self.Oneof_Decl
                (Natural (Field.Oneof_Index.Value) + 1).Name.Value);
         else
            return League.Strings.Empty_Universal_String;
         end if;
      end Oneof_Name;

      My_Name : constant League.Strings.Universal_String := Type_Name (Self);
      Key : Ada_Pretty.Node_Access;
      Result : Ada_Pretty.Node_Access;
      Field : Ada_Pretty.Node_Access;
   begin
      Key := F.New_Variable
        (Name            => F.New_Name (+"Key"),
         Type_Definition => F.New_Selected_Name (+"PB_Support.IO.Key"),
         Is_Aliased      => True);

      for J in 1 .. Self.Field.Length loop
         Field := Compiler.Field_Descriptors.Read_Case
           (Self.Field (J),
            Pkg,
            My_Name,
            Fake,
            Oneof_Name (Self.Field (J)));
         Result := F.New_List (Result, Field);
      end loop;

      Result := F.New_List
        (Result,
         F.New_Case_Path
           (Choice => F.New_Name (+"others"),
            List   => F.New_Statement
              (F.New_Apply
                 (Prefix    => F.New_Selected_Name
                    (+"PB_Support.IO.Unknown_Field"),
                  Arguments => F.New_List
                    (F.New_Argument_Association (F.New_Name (+"Stream")),
                     F.New_Argument_Association
                       (F.New_Selected_Name (+"Key.Encoding")))))));

      Result := F.New_Subprogram_Body
        (F.New_Subprogram_Specification
           (Name          => F.New_Name ("Read_" & My_Name),
            Parameters    => F.New_List
              (F.New_Parameter
                   (Name            => F.New_Name (+"Stream"),
                    Type_Definition => F.New_Selected_Name
                      (+"access Ada.Streams.Root_Stream_Type'Class")),
               F.New_Parameter
                 (Name            => F.New_Name (+"V"),
                  Type_Definition => F.New_Name (My_Name),
                  Is_Out          => True))),
         Declarations => Key,
         Statements   => F.New_Loop
           (Condition  => F.New_Apply
                (Prefix    => F.New_Selected_Name
                     (+"PB_Support.IO.Read_Key"),
                 Arguments => F.New_List
                   (F.New_Argument_Association
                      (F.New_Name (+"Stream")),
                    F.New_Argument_Association
                      (F.New_Name (+"Key'Access")))),
            Statements => F.New_Case
              (Expression => F.New_Selected_Name (+"Key.Field"),
               List       => Result)));

      return Result;
   end Read_Subprogram;

   -----------------
   -- Subprograms --
   -----------------

   function Subprograms
     (Self : Google.Protobuf.Descriptor.Descriptor_Proto;
      Pkg  : League.Strings.Universal_String)
      return Ada_Pretty.Node_Access
   is
      My_Name : constant League.Strings.Universal_String := Type_Name (Self);
      Me      : constant Ada_Pretty.Node_Access := F.New_Name (My_Name);

      V_Name  : Ada_Pretty.Node_Access;
      P_Self : Ada_Pretty.Node_Access;
      Free   : Ada_Pretty.Node_Access;
      Count  : Ada_Pretty.Node_Access;
      Clear  : Ada_Pretty.Node_Access;
      Append : Ada_Pretty.Node_Access;
      Adjust : Ada_Pretty.Node_Access;
      Final  : Ada_Pretty.Node_Access;
      Read   : Ada_Pretty.Node_Access;
      Write  : Ada_Pretty.Node_Access;
      Ref    : Ada_Pretty.Node_Access;
      Result : Ada_Pretty.Node_Access;
   begin
      V_Name := F.New_Name (My_Name & "_Vector");
      P_Self := F.New_Name (+"Self");

      Count := F.New_Subprogram_Body
        (F.New_Subprogram_Specification
           (Name       => F.New_Name (+"Length"),
            Parameters => F.New_Parameter
              (Name            => P_Self,
               Type_Definition => V_Name),
            Result     => F.New_Name (+"Natural")),
         Statements => F.New_Return
           (F.New_Selected_Name (+"Self.Length")));

      Clear := F.New_Subprogram_Body
        (F.New_Subprogram_Specification
           (Name       => F.New_Name (+"Clear"),
            Parameters => F.New_Parameter
              (Name            => P_Self,
               Type_Definition => V_Name,
               Is_In           => True,
               Is_Out          => True)),
         Statements => F.New_Assignment
           (Left  => F.New_Selected_Name (+"Self.Length"),
            Right => F.New_Literal (0)));

      Free := F.New_Statement
        (F.New_Apply
           (F.New_Name (+"procedure Free is new Ada.Unchecked_Deallocation"),
            F.New_List
              (F.New_Argument_Association
                 (F.New_Name (My_Name & "_Array")),
               F.New_Argument_Association
                 (F.New_Name (My_Name & "_Array_Access")))));

      Append := F.New_Subprogram_Body
        (F.New_Subprogram_Specification
           (Name       => F.New_Name (+"Append"),
            Parameters => F.New_List
              (F.New_Parameter
                 (Name            => P_Self,
                  Type_Definition => V_Name,
                  Is_In           => True,
                  Is_Out          => True),
               F.New_Parameter
                 (F.New_Name (+"V"), Me))),
         Declarations => F.New_Variable
           (Name            => F.New_Name (+"Init_Length"),
            Type_Definition => F.New_Name (+"Positive"),
            Is_Constant     => True,
            Initialization  => F.New_Apply
              (Prefix    => F.New_Selected_Name (+"Positive'Max"),
               Arguments => F.New_List
                 (F.New_Argument_Association (F.New_Literal (1)),
                  F.New_Argument_Association
                    (F.New_List
                       (F.New_Literal (256),
                        F.New_Infix
                          (+"/",
                           F.New_Selected_Name (My_Name & "'Size"))))))),
         Statements => F.New_List
           ((F.New_If
             (Condition  => F.New_Selected_Name (+"Self.Length = 0"),
              Then_Path  => F.New_Assignment
                (F.New_Selected_Name (+"Self.Data"),
                 F.New_Infix
                   (Operator => +"new",
                    Left     => F.New_Apply
                      (F.New_Selected_Name (My_Name & "_Array"),
                       F.New_Selected_Name (+"1 .. Init_Length")))),
              Elsif_List => F.New_Elsif
                (Condition => F.New_List
                   (F.New_Selected_Name (+"Self.Length"),
                    F.New_Infix
                      (+"=",
                       F.New_Selected_Name (+"Self.Data'Last"))),
                 List      => F.New_Assignment
                   (F.New_Selected_Name (+"Self.Data"),
                    F.New_Qualified_Expession
                      (F.New_Selected_Name ("new " & My_Name & "_Array"),
                       F.New_List
                         (F.New_Selected_Name (+"Self.Data.all"),
                          F.New_Infix
                            (+"&",
                             F.New_Qualified_Expession
                               (F.New_Selected_Name (My_Name & "_Array"),
                                F.New_Selected_Name
                                  (+"1 .. Self.Length => <>"))
                            )))))),
            F.New_Assignment
              (F.New_Selected_Name (+"Self.Length"),
               F.New_List
                 (F.New_Selected_Name (+"Self.Length"),
                  F.New_Infix (+"+", F.New_Literal (1)))),
            F.New_Assignment
              (F.New_Apply
                 (F.New_Selected_Name (+"Self.Data"),
                    F.New_Selected_Name (+"Self.Length")),
               F.New_Name (+"V")))));

      Adjust := F.New_Subprogram_Body
        (F.New_Subprogram_Specification
           (Is_Overriding => Ada_Pretty.True,
            Name          => F.New_Name (+"Adjust"),
            Parameters    => F.New_Parameter
              (Name            => F.New_Name (+"Self"),
               Type_Definition => F.New_Name (Type_Name (Self) & "_Vector"),
               Is_In           => True,
               Is_Out          => True)),
         Statements => F.New_If
           (Condition  => F.New_Name (+"Self.Length > 0"),
            Then_Path  => F.New_Assignment
              (F.New_Selected_Name (+"Self.Data"),
               F.New_Qualified_Expession
                 (F.New_Name ("new " & My_Name & "_Array"),
                  F.New_Apply
                    (F.New_Selected_Name (+"Self.Data"),
                     F.New_List
                       (F.New_Literal (1),
                        F.New_Infix
                          (+"..",
                           F.New_Selected_Name (+"Self.Length"))))))));

      Final := F.New_Subprogram_Body
        (F.New_Subprogram_Specification
           (Is_Overriding => Ada_Pretty.True,
            Name          => F.New_Name (+"Finalize"),
            Parameters    => F.New_Parameter
              (Name            => F.New_Name (+"Self"),
               Type_Definition => F.New_Name (Type_Name (Self) & "_Vector"),
               Is_In           => True,
               Is_Out          => True)),
         Statements => F.New_If
           (Condition  => F.New_Name (+"Self.Data /= null"),
            Then_Path  => F.New_Statement
              (F.New_Apply
                 (F.New_Name (+"Free"),
                  F.New_Selected_Name (+"Self.Data")))));

      Read := Read_Subprogram (Self, Pkg, Compiler.Context.Fake);

      Write := Write_Subprogram (Self, Pkg, Compiler.Context.Fake);

      Ref := F.New_List
        (Indexing_Body (Self, +"Variable", True),
         Indexing_Body (Self, +"Constant", False));

      Result := F.New_List
        ((Count, Clear, Free, Append, Adjust, Final, Ref, Read, Write));

      for J in 1 .. Self.Nested_Type.Length loop
         Result := F.New_List
           (Result, Subprograms (Self.Nested_Type (J), Pkg));
      end loop;

      return Result;
   end Subprograms;

   ---------------
   -- Type_Name --
   ---------------

   function Type_Name
     (Self : Google.Protobuf.Descriptor.Descriptor_Proto)
      return League.Strings.Universal_String is
   begin
      if Self.Name.Is_Set then
         return Compiler.Context.To_Ada_Name (Self.Name.Value);
      else
         return +"Message";
      end if;
   end Type_Name;

   -------------------------
   -- Vector_Declarations --
   -------------------------

   function Vector_Declarations
     (Self : Google.Protobuf.Descriptor.Descriptor_Proto)
      return Ada_Pretty.Node_Access
   is
      My_Name  : constant League.Strings.Universal_String := Type_Name (Self);

      Result   : Ada_Pretty.Node_Access;
      Item     : Ada_Pretty.Node_Access;
   begin
      Result := F.New_List
        (Result,
         F.New_Type
          (Name        => F.New_Name (My_Name & "_Vector"),
           Definition  => F.New_Private_Record
             (Is_Tagged   => True),
           Aspects     => F.New_List
             (F.New_Argument_Association
               (F.New_Name ("Get_" & My_Name & "_Variable_Reference"),
                F.New_Name (+"Variable_Indexing")),
             F.New_Argument_Association
               (F.New_Name ("Get_" & My_Name & "_Constant_Reference"),
                F.New_Name (+"Constant_Indexing")))));

      for J in 1 .. Self.Nested_Type.Length loop
         Item := Vector_Declarations (Self.Nested_Type (J));

         Result := F.New_List (Result, Item);
      end loop;

      return Result;
   end Vector_Declarations;

   ----------------------
   -- Write_Subprogram --
   ----------------------

   function Write_Subprogram
     (Self : Google.Protobuf.Descriptor.Descriptor_Proto;
      Pkg  : League.Strings.Universal_String;
      Fake : Compiler.Context.String_Sets.Set)
      return Ada_Pretty.Node_Access
   is
      My_Name : constant League.Strings.Universal_String := Type_Name (Self);
      Result  : Ada_Pretty.Node_Access;
      If_Stmt : Ada_Pretty.Node_Access;
      Decl    : Ada_Pretty.Node_Access;
      Stream  : constant Ada_Pretty.Node_Access :=
        F.New_Selected_Name (+"PB_Support.Internal.Stream");
      Stmts   : Ada_Pretty.Node_Access;
   begin
      If_Stmt := F.New_If
        (F.New_List
           (F.New_Selected_Name (+"Stream.all"),
            F.New_Infix
              (+"not in",
               F.New_Selected_Name (+"PB_Support.Internal.Stream"))),
         F.New_Block
           (Declarations => F.New_Variable
                (Name            => F.New_Name (+"WS"),
                 Type_Definition => F.New_Apply
                   (Stream,
                    F.New_Name (+"Stream")),
                 Is_Aliased      => True),
            Statements   => F.New_List
              (F.New_Statement
                   (F.New_Apply
                        (F.New_Name ("Write_" & My_Name),
                         F.New_List
                           (F.New_Argument_Association
                              (F.New_Name (+"WS'Access")),
                            F.New_Argument_Association
                              (F.New_Name (+"V"))))),
               F.New_Return)));

      Stmts := F.New_Statement (F.New_Selected_Name (+"WS.Start_Message"));

      for J in 1 .. Self.Field.Length loop
         if not Self.Field (J).Oneof_Index.Is_Set then
            Stmts := F.New_List
              (Stmts,
               Compiler.Field_Descriptors.Write_Call
                 (Self.Field (J), Pkg, My_Name, Fake));
         end if;
      end loop;

      for K in 1 .. Self.Oneof_Decl.Length loop
         declare
            Name   : constant League.Strings.Universal_String :=
              Compiler.Context.To_Ada_Name (Self.Oneof_Decl (K).Name.Value);
            Cases : Ada_Pretty.Node_Access;
         begin
            for J in 1 .. Self.Field.Length loop
               if Is_One_Of (Self.Field (J).Oneof_Index, K) then
                  Cases := F.New_List
                    (Cases,
                     Compiler.Field_Descriptors.Case_Path
                       (Self.Field (J), Pkg, My_Name, Fake));
               end if;
            end loop;

            Cases := F.New_List
              (Cases,
               F.New_Case_Path
                 (F.New_Name (Name & "_Not_Set"),
                  F.New_Statement));

            Stmts := F.New_List
              (Stmts,
               F.New_Case
                 (F.New_Selected_Name ("V.Variant." & Name),
                  Cases));
         end;
      end loop;

      Stmts := F.New_List
        (Stmts,
         F.New_If
           (F.New_Selected_Name (+"WS.End_Message"),
            F.New_Statement
              (F.New_Apply
                (F.New_Name ("Write_" & My_Name),
                 F.New_List
                   (F.New_Argument_Association
                      (F.New_Name (+"WS'Access")),
                    F.New_Argument_Association
                      (F.New_Name (+"V")))))));

      Decl := F.New_Block
        (Declarations =>
           F.New_Variable
             (Name            => F.New_Name (+"WS"),
              Type_Definition => Stream,
              Rename          =>
                F.New_Apply
                  (Stream,
                   F.New_Argument_Association
                     (F.New_Selected_Name (+"Stream.all")))),
         Statements   => Stmts);

      Result := F.New_Subprogram_Body
        (F.New_Subprogram_Specification
           (Name          => F.New_Name ("Write_" & My_Name),
            Parameters    => F.New_List
              (F.New_Parameter
                   (Name            => F.New_Name (+"Stream"),
                    Type_Definition => F.New_Selected_Name
                      (+"access Ada.Streams.Root_Stream_Type'Class")),
               F.New_Parameter
                   (Name            => F.New_Name (+"V"),
                    Type_Definition => F.New_Name (My_Name)))),
         Statements => F.New_List (If_Stmt, Decl));

      return Result;
   end Write_Subprogram;
end Compiler.Descriptors;
