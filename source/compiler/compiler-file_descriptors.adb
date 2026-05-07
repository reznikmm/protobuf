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

with Interfaces;
with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Directories;

with Ada_Pretty;

with League.String_Vectors;

with Compiler.Descriptors;
with Compiler.Enum_Descriptors;
with Compiler.Field_Descriptors;

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

   function Get_Prefix
     (Self : Google.Protobuf.Descriptor.File_Descriptor_Proto)
       return League.Strings.Universal_String;
   --  Return protobuf prefix for given file: the dot appended with package.
   --  Like ".google.protobuf"

   function Header_Comment
     (Self    : Google.Protobuf.Descriptor.File_Descriptor_Proto;
      Request : Google.Protobuf.Compiler.Plugin.Code_Generator_Request)
      return League.Strings.Universal_String;

   ---------------
   -- Body_Text --
   ---------------

   function Body_Text
     (Self    : Google.Protobuf.Descriptor.File_Descriptor_Proto;
      Request : Google.Protobuf.Compiler.Plugin.Code_Generator_Request)
      return League.Strings.Universal_String
   is

      function Get_Subprograms return Ada_Pretty.Node_Access;
      function Get_Instances return Ada_Pretty.Node_Access;

      Prefix : constant League.Strings.Universal_String := Get_Prefix (Self);

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
                          ("Integer_" & Compiler.Context.Compound_Name
                             (Info.Ada_Type, Pkg)),
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
                          (Compiler.Context.Compound_Name
                             (Info.Ada_Type, Pkg) & "_IO"),
                        Template    => F.New_Selected_Name
                          (+"PB_Support.IO.Enum_IO"),
                        Actual_Part => F.New_List
                          ((F.New_Argument_Association
                              (F.New_Selected_Name (Full)),
                            F.New_Name
                              ("Integer_" &  Compiler.Context.Compound_Name
                                 (Info.Ada_Type, Pkg)),
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
                          (Compiler.Context.Compound_Name
                             (Info.Ada_Type, Pkg) & "_IO"),
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
              (Self.Message_Type (J), Pkg, Prefix);
            Result := F.New_List (Result, Next);
         end loop;

         return Result;
      end Get_Subprograms;

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
           Clauses => With_Clauses,
           License => Header_Comment (Self, Request));
   begin
      return League.Strings."&"
        (F.To_Text (Unit).Join (Ada.Characters.Wide_Wide_Latin_1.LF),
         Ada.Characters.Wide_Wide_Latin_1.LF);
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

      if Self.Enum_Type.Length > 0 then
         Set.Include (+"PB_Support.Vectors");
      end if;

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

   ----------------
   -- Get_Prefix --
   ----------------

   function Get_Prefix
     (Self : Google.Protobuf.Descriptor.File_Descriptor_Proto)
       return League.Strings.Universal_String
   is
      Result : League.Strings.Universal_String;
   begin
      if Self.PB_Package.Is_Set then
         Result.Append (".");
         Result.Append (Self.PB_Package.Value);
      end if;

      return Result;
   end Get_Prefix;

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
      Base_Name : constant League.Strings.Universal_String :=
        League.Strings.From_UTF_8_String
          (Ada.Directories.Base_Name (File_Name));
      --  Replace every dash ('-') with underscore ('_') to make Base_Name
      --  look like an identifier.
      Fixed : constant League.Strings.Universal_String :=
        Base_Name.Split ('-', League.Strings.Skip_Empty).Join ("_");
      PB_Pkg    : League.Strings.Universal_String;
      Result    : League.Strings.Universal_String :=
        Compiler.Context.To_Ada_Name (Fixed);
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

      Prefix : constant League.Strings.Universal_String := Get_Prefix (Self);

      Local : Compiler.Context.Named_Type_Maps.Map;
      --  Map for type local to current file

      Used : Compiler.Context.String_Sets.Set;
      --  Name clash protection

      Ada_Package : constant League.Strings.Universal_String :=
        Package_Name (Self);
   begin
      for J in 1 .. Self.Enum_Type.Length loop
         Compiler.Enum_Descriptors.Populate_Named_Types
           (Self        => Self.Enum_Type (J),
            Prefix      => Prefix,
            Ada_Package => Ada_Package,
            Map         => Local,
            Used        => Used);
      end loop;

      for J in 1 .. Self.Message_Type.Length loop
         Compiler.Descriptors.Populate_Named_Types
           (Self        => Self.Message_Type (J),
            Prefix      => Prefix,
            Ada_Package => Ada_Package,
            Map         => Local,
            Used        => Used);
      end loop;

      for J in 1 .. Self.Message_Type.Length loop
         Compiler.Descriptors.Populate_Nested_Types
           (Self        => Self.Message_Type (J),
            Prefix      => Prefix,
            Ada_Package => Ada_Package,
            Map         => Local,
            Used        => Used);
      end loop;

      --  Find name clashes for Optional_ type and fallback these names
      --  to Facultative_
      for J of Local loop
         if Used.Contains (J.Optional_Type) then
            J.Optional_Type := "Facultative_" & J.Ada_Type.Type_Name;
         end if;
      end loop;

      --  Merge Local into Map
      for Cursor in Local.Iterate loop
         Map.Insert
           (Compiler.Context.Named_Type_Maps.Key (Cursor),
            Compiler.Context.Named_Type_Maps.Element (Cursor));
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

      Prefix : constant League.Strings.Universal_String := Get_Prefix (Self);

      -----------------
      -- Get_Private --
      -----------------

      function Get_Private return Ada_Pretty.Node_Access is
         Result : Ada_Pretty.Node_Access;
         Next   : Ada_Pretty.Node_Access;
      begin
         for J in 1 .. Self.Message_Type.Length loop
            Next := Compiler.Descriptors.Private_Spec
              (Self.Message_Type (J), Prefix);
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
            Next := Compiler.Enum_Descriptors.Public_Spec
              (Self.Enum_Type (J), Prefix);

            if Next /= null then
               Result := F.New_List (Result, Next);
            end if;
         end loop;

         for J in 1 .. Self.Message_Type.Length loop
            Next := Compiler.Descriptors.Enum_Types
              (Self.Message_Type (J), Prefix);

            if Next /= null then
               Result := F.New_List (Result, Next);
            end if;

            Next := Compiler.Descriptors.Vector_Declarations
              (Self.Message_Type (J), Prefix);

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
                 (Self.Message_Type (J),
                  Pkg, Prefix, Next, Again, Done, Force);

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

      Name   : constant Ada_Pretty.Node_Access :=
        F.New_Selected_Name (Package_Name (Self));

      Clauses : constant Ada_Pretty.Node_Access :=
        Dependency (Self, Request);

      Public : constant Ada_Pretty.Node_Access := Get_Public;
      Root   : constant Ada_Pretty.Node_Access :=
        F.New_Package (Name, Public, Get_Private);
      Unit   : constant Ada_Pretty.Node_Access :=
        F.New_Compilation_Unit (Root, Clauses, Header_Comment (Self, Request));
   begin
      return League.Strings."&"
        (F.To_Text (Unit).Join (Ada.Characters.Wide_Wide_Latin_1.LF),
         Ada.Characters.Wide_Wide_Latin_1.LF);
   end Specification_Text;

   -------------------
   -- Header_Comment --
   -------------------

   function Header_Comment
     (Self    : Google.Protobuf.Descriptor.File_Descriptor_Proto;
      Request : Google.Protobuf.Compiler.Plugin.Code_Generator_Request)
      return League.Strings.Universal_String
   is
      use type League.Strings.Universal_String;

      function Image
        (V : Interfaces.Integer_32) return League.Strings.Universal_String;

      function Image
        (V : Interfaces.Integer_32) return League.Strings.Universal_String
      is
         Img : constant Wide_Wide_String :=
           Interfaces.Integer_32'Wide_Wide_Image (V);
      begin
         return
           +(if Img (Img'First) = ' '
             then Img (Img'First + 1 .. Img'Last)
             else Img);
      end Image;

      Result : League.Strings.Universal_String;
      LF     : Wide_Wide_Character renames
        Ada.Characters.Wide_Wide_Latin_1.LF;
   begin
      Result.Append
        (+"--  Generated by the protocol buffer compiler.  DO NOT EDIT!");
      Result.Append (LF);
      Result.Append (+"--  Source: ");
      if Self.Name.Is_Set then
         Result.Append (Self.Name.Value);
      end if;
      Result.Append (LF);

      Result.Append (+"--  Protobuf Compiler Version: ");
      if Request.Compiler_Version.Is_Set then
         declare
            V : constant Google.Protobuf.Compiler.Plugin.Version :=
              Request.Compiler_Version.Value;
         begin
            if V.Major.Is_Set then
               Result.Append (Image (V.Major.Value));
            end if;
            if V.Minor.Is_Set then
               Result.Append (+".");
               Result.Append (Image (V.Minor.Value));
            end if;
            if V.Patch.Is_Set then
               Result.Append (+".");
               Result.Append (Image (V.Patch.Value));
            end if;
            if V.Suffix.Is_Set and then V.Suffix.Value.Length > 0 then
               Result.Append (+"-");
               Result.Append (V.Suffix.Value);
            end if;
         end;
      else
         Result.Append (+"unknown");
      end if;
      Result.Append (LF);

      return Result;
   end Header_Comment;

   -----------------------------
   -- JSON_Specification_Text --
   -----------------------------

   function JSON_Specification_Text
     (Self    : Google.Protobuf.Descriptor.File_Descriptor_Proto;
      Request : Google.Protobuf.Compiler.Plugin.Code_Generator_Request)
      return League.Strings.Universal_String
   is
      use type League.Strings.Universal_String;
      LF : constant League.Strings.Universal_String :=
        +("" & Ada.Characters.Wide_Wide_Latin_1.LF);
      Pkg : constant League.Strings.Universal_String := Package_Name (Self);
      Prefix : constant League.Strings.Universal_String := Get_Prefix (Self);

      function Generate_Spec
        (Msg    : Google.Protobuf.Descriptor.Descriptor_Proto;
         Prefix : League.Strings.Universal_String)
         return Ada_Pretty.Node_Access;

      function Generate_Spec
        (Msg    : Google.Protobuf.Descriptor.Descriptor_Proto;
         Prefix : League.Strings.Universal_String)
         return Ada_Pretty.Node_Access
      is
         Key : constant League.Strings.Universal_String :=
           Compiler.Context.Join (Prefix, Msg.Name);
         Name : constant League.Strings.Universal_String :=
           Compiler.Context.Named_Types (Key).Ada_Type.Type_Name;
         Result : Ada_Pretty.Node_Access := null;
         Nested : Ada_Pretty.Node_Access := null;
      begin
         if not Compiler.Context.Named_Types (Key).Is_Enumeration then
            Result := F.New_Subprogram_Declaration
              (F.New_Subprogram_Specification
                 (Name       => F.New_Name (+"Write"),
                  Parameters => F.New_List
                    (F.New_Parameter
                       (Name            => F.New_Name (+"Stream"),
                        Type_Definition => F.New_Selected_Name
                          (+"PB_Support.JSON.JSON_Writer"),
                        Is_In           => True,
                        Is_Out          => True),
                     F.New_Parameter
                       (Name            => F.New_Name (+"Value"),
                        Type_Definition => F.New_Selected_Name
                          (+"Standard." & Pkg & "." & Name)))));
         end if;

         for J in 1 .. Msg.Nested_Type.Length loop
            Nested := Generate_Spec (Msg.Nested_Type (J), Key);
            if Nested /= null then
               Result := (if Result = null then Nested
                          else F.New_List (Result, Nested));
            end if;
         end loop;
         return Result;
      end Generate_Spec;

      Specs : Ada_Pretty.Node_Access := null;
      Nested : Ada_Pretty.Node_Access := null;
   begin
      for J in 1 .. Self.Message_Type.Length loop
         Nested := Generate_Spec (Self.Message_Type (J), Prefix);
         if Nested /= null then
            Specs := (if Specs = null then Nested
                      else F.New_List (Specs, Nested));
         end if;
      end loop;

      declare
         Unit : constant Ada_Pretty.Node_Access :=
           F.New_Compilation_Unit
             (Root    => F.New_Package
                (Name    => F.New_Name (Pkg & ".JSON"),
                 Public_Part => Specs),
              Clauses => F.New_With (F.New_Name (+"PB_Support.JSON")),
              License => Header_Comment (Self, Request));
      begin
         return League.Strings."&" (F.To_Text (Unit).Join (LF), LF);
      end;
   end JSON_Specification_Text;

   function JSON_Body_Text
     (Self    : Google.Protobuf.Descriptor.File_Descriptor_Proto;
      Request : Google.Protobuf.Compiler.Plugin.Code_Generator_Request)
      return League.Strings.Universal_String
   is
      use type League.Strings.Universal_String;
      LF : constant League.Strings.Universal_String :=
        +("" & Ada.Characters.Wide_Wide_Latin_1.LF);
      Pkg : constant League.Strings.Universal_String := Package_Name (Self);
      Prefix : constant League.Strings.Universal_String := Get_Prefix (Self);

      function Generate_Body
        (Msg    : Google.Protobuf.Descriptor.Descriptor_Proto;
         Prefix : League.Strings.Universal_String)
         return Ada_Pretty.Node_Access;

      function Generate_Field
        (Field : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
         Acc   : League.Strings.Universal_String)
         return Ada_Pretty.Node_Access;

      function Generate_Field
        (Field : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
         Acc   : League.Strings.Universal_String) return Ada_Pretty.Node_Access
      is
         use all type Google.Protobuf.Descriptor.PB_Type;
         Is_Enum    : constant Boolean :=
           Compiler.Field_Descriptors.Is_Enum (Field);
         Is_Message : constant Boolean :=
           Compiler.Field_Descriptors.Is_Message (Field);
      begin
         if Is_Message then
            declare
               Target : constant Compiler.Context.Named_Type :=
                 Compiler.Context.Named_Types (Field.Type_Name.Value);
            begin
               if Target.Ada_Type.Package_Name = Pkg then
                  return
                    F.New_Statement
                      (F.New_Apply
                         (F.New_Name (+"Write"),
                          F.New_List
                            (F.New_Name (+"Stream"),
                             F.New_Selected_Name (Acc))));
               else
                  return
                    F.New_Statement
                      (F.New_Apply
                         (F.New_Selected_Name
                            (Target.Ada_Type.Package_Name & ".JSON.Write"),
                          F.New_List
                            (F.New_Name (+"Stream"),
                             F.New_Selected_Name (Acc))));
               end if;
            end;
         elsif Is_Enum then
            if Field.Type_Name.Is_Set
              and then Field.Type_Name.Value = +".google.protobuf.NullValue"
            then
               return
                 F.New_Statement (F.New_Selected_Name (+"Stream.Write_Null"));
            else
               return
                 F.New_Statement
                   (F.New_Apply
                      (F.New_Selected_Name (+"Stream.Write_String"),
                       F.New_Selected_Name (Acc & "'Image")));
            end if;
         elsif Field.PB_Type.Is_Set then
            case Field.PB_Type.Value is
               when TYPE_BOOL                =>
                  return
                    F.New_Statement
                      (F.New_Apply
                         (F.New_Selected_Name (+"Stream.Write_Boolean"),
                          F.New_Selected_Name (Acc)));

               when TYPE_STRING              =>
                  return
                    F.New_Statement
                      (F.New_Apply
                         (F.New_Selected_Name (+"Stream.Write_String"),
                          (F.New_Name (+"+" & Acc))));

               when TYPE_FLOAT | TYPE_DOUBLE =>
                  declare
                     Stmt : Ada_Pretty.Node_Access;
                  begin
                     if Field.PB_Type.Value = TYPE_FLOAT then
                        Stmt :=
                          F.New_Statement
                            (F.New_Apply
                               (F.New_Selected_Name (+"Stream.Write_Float"),
                                F.New_Apply
                                  (F.New_Name (+"Interfaces.IEEE_Float_64"),
                                   F.New_Selected_Name (Acc))));
                     else
                        Stmt :=
                          F.New_Statement
                            (F.New_Apply
                               (F.New_Selected_Name (+"Stream.Write_Float"),
                                F.New_Selected_Name (Acc)));
                     end if;
                     return
                       F.New_Block
                         (Statements   => Stmt,
                          Declarations =>
                            F.New_Pragma
                              (F.New_Name (+"Suppress"),
                               F.New_Name (+"Range_Check")));
                  end;

               when TYPE_INT32
                  | TYPE_UINT32
                  | TYPE_SINT32
                  | TYPE_FIXED32
                  | TYPE_SFIXED32            =>
                  return
                    F.New_Statement
                      (F.New_Apply
                         (F.New_Selected_Name (+"Stream.Write_Integer"),
                          F.New_Apply
                            (F.New_Name (+"Long_Long_Integer"),
                             F.New_Selected_Name (Acc))));

               when TYPE_FIXED64
                  | TYPE_SFIXED64
                  | TYPE_SINT64
                  | TYPE_INT64
                  | TYPE_UINT64              =>
                  return
                    F.New_Statement
                      (F.New_Apply
                         (F.New_Selected_Name (+"Stream.Write_Integer"),
                          F.New_Selected_Name (Acc)));

               when TYPE_BYTES               =>
                  return
                    F.New_Statement
                      (F.New_Apply
                         (F.New_Selected_Name (+"Stream.Write_Bytes"),
                          F.New_Selected_Name (Acc)));

               when others                   =>
                  return
                    F.New_Statement
                      (F.New_Selected_Name (+"Stream.Write_Null"));
            end case;
         end if;
         return null;
      end Generate_Field;

      function Generate_Body
        (Msg    : Google.Protobuf.Descriptor.Descriptor_Proto;
         Prefix : League.Strings.Universal_String)
         return Ada_Pretty.Node_Access
      is
         use type Google.Protobuf.Descriptor.Label;
         Key    : constant League.Strings.Universal_String :=
           Compiler.Context.Join (Prefix, Msg.Name);
         Name   : constant League.Strings.Universal_String :=
           Compiler.Context.Named_Types (Key).Ada_Type.Type_Name;
         Result : Ada_Pretty.Node_Access := null;
         Stmts  : Ada_Pretty.Node_Access := null;
      begin
         if not Compiler.Context.Named_Types (Key).Is_Enumeration then
            if Pkg & "." & Name = +"Google.Protobuf.Timestamp.Timestamp" then
               Stmts :=
                 F.New_Statement
                   (F.New_Apply
                      (F.New_Selected_Name
                         (+"PB_Support.JSON.Write_Timestamp"),
                       F.New_List
                         (F.New_List
                            (F.New_Name (+"Stream"),
                             F.New_Selected_Name (+"Value.Seconds")),
                          F.New_Selected_Name (+"Value.Nanos"))));
            elsif Pkg & "." & Name = +"Google.Protobuf.Duration.Duration" then
               Stmts :=
                 F.New_Statement
                   (F.New_Apply
                      (F.New_Selected_Name (+"PB_Support.JSON.Write_Duration"),
                       F.New_List
                         (F.New_List
                            (F.New_Name (+"Stream"),
                             F.New_Selected_Name (+"Value.Seconds")),
                          F.New_Selected_Name (+"Value.Nanos"))));
            else
               Stmts :=
                 F.New_Statement
                   (F.New_Selected_Name (+"Stream.Start_Object"));

               for K in 1 .. Msg.Field.Length loop
                  declare
                     use all type Google.Protobuf.Descriptor.PB_Type;
                     use all type Compiler.Field_Descriptors.Option_Kind;
                     Field         :
                       constant Google
                                  .Protobuf
                                  .Descriptor
                                  .Field_Descriptor_Proto := Msg.Field (K);
                     F_Name        :
                       constant League.Strings.Universal_String :=
                         Field.Name.Value;
                     Ada_Name      :
                       constant League.Strings.Universal_String :=
                         Compiler.Context.To_Ada_Name (Field.Name.Value);
                     Json_Key      :
                       constant League.Strings.Universal_String :=
                         (if Field.Json_Name.Is_Set
                          then Field.Json_Name.Value
                          else F_Name);
                     Is_Vector     : constant Boolean :=
                       Compiler.Field_Descriptors.Is_Repeated
                         (Field, Pkg, Name, Compiler.Context.Fake);
                     Is_JSON_Array : constant Boolean :=
                       Field.Label.Is_Set
                       and then
                         Field.Label.Value
                         = Google.Protobuf.Descriptor.LABEL_REPEATED;
                     Is_Option     :
                       constant Compiler.Field_Descriptors.Option_Kind :=
                         Compiler.Field_Descriptors.Is_Optional (Field);
                     Is_Oneof      : constant Boolean :=
                       Compiler.Field_Descriptors.Is_One_Of (Field);
                     Is_Message    : constant Boolean :=
                       Compiler.Field_Descriptors.Is_Message (Field);
                     Field_Stmts   : Ada_Pretty.Node_Access := null;
                  begin
                     if Is_Vector then
                        Field_Stmts :=
                          F.New_Statement
                            (F.New_Apply
                               (F.New_Selected_Name (+"Stream.Write_Key"),
                                F.New_String_Literal (Json_Key)));
                        if Is_JSON_Array then
                           Field_Stmts :=
                             F.New_List
                               (Field_Stmts,
                                F.New_Statement
                                  (F.New_Selected_Name
                                     (+"Stream.Start_Array")));
                        end if;

                        declare
                           Acc       :
                             constant League.Strings.Universal_String :=
                               (if Field.PB_Type.Is_Set
                                  and then not Is_Message
                                  and then
                                    not (Compiler.Context.Runtime_Dep
                                         = Compiler.Runtime_League
                                         and then
                                           Field.PB_Type.Value = TYPE_STRING)
                                then +"Value." & Ada_Name & ".Get (J)"
                                else +"Value." & Ada_Name & " (J)");
                           Loop_Body : constant Ada_Pretty.Node_Access :=
                             Generate_Field (Field, Acc);
                        begin
                           declare
                              Iter : constant Ada_Pretty.Node_Access :=
                                F.New_List
                                  (F.New_Literal (1),
                                   F.New_Infix
                                     (+"..",
                                      F.New_Selected_Name
                                        (+"Value." & Ada_Name & ".Length")));
                           begin
                              Field_Stmts :=
                                F.New_List
                                  (Field_Stmts,
                                   F.New_For
                                     (F.New_Name (+"J"), Iter, Loop_Body));
                           end;
                        end;

                        if Is_JSON_Array then
                           Field_Stmts :=
                             F.New_List
                               (Field_Stmts,
                                F.New_Statement
                                  (F.New_Selected_Name (+"Stream.End_Array")));
                        end if;

                        Stmts :=
                          F.New_List
                            (Stmts,
                             F.New_If
                               (F.New_List
                                  (F.New_Selected_Name
                                     (+"Value." & Ada_Name & ".Length"),
                                   F.New_Infix (+">", F.New_Literal (0))),
                                Field_Stmts));
                     else
                        declare
                           Acc : League.Strings.Universal_String :=
                             (if Is_Oneof
                              then +"Value.Variant." & Ada_Name
                              else +"Value." & Ada_Name);
                        begin
                           Field_Stmts :=
                             F.New_Statement
                               (F.New_Apply
                                  (F.New_Selected_Name (+"Stream.Write_Key"),
                                   F.New_String_Literal (Json_Key)));

                           if Is_Oneof then
                              declare
                                 Selector :
                                   constant League.Strings.Universal_String :=
                                     Compiler.Context.To_Ada_Name
                                       (Msg.Oneof_Decl
                                          (Natural (Field.Oneof_Index.Value)
                                           + 1)
                                          .Name
                                          .Value);
                              begin
                                 Field_Stmts :=
                                   F.New_List
                                     (Field_Stmts,
                                      Generate_Field (Field, Acc));
                                 Stmts :=
                                   F.New_List
                                     (Stmts,
                                      F.New_If
                                        (F.New_List
                                           (F.New_Selected_Name
                                              (+"Value.Variant." & Selector),
                                            F.New_Infix
                                              (+"=",
                                               F.New_Name
                                                 (Ada_Name & "_Kind"))),
                                         Field_Stmts));
                              end;
                           elsif Is_Option = Optional then
                              declare
                                 Cond : constant Ada_Pretty.Node_Access :=
                                   F.New_Selected_Name (Acc & ".Is_Set");
                              begin
                                 Acc.Append (+".Value");
                                 Field_Stmts :=
                                   F.New_List
                                     (Field_Stmts,
                                      Generate_Field (Field, Acc));
                                 Stmts :=
                                   F.New_List
                                     (Stmts, F.New_If (Cond, Field_Stmts));
                              end;
                           else
                              Field_Stmts :=
                                F.New_List
                                  (Field_Stmts, Generate_Field (Field, Acc));
                              Stmts := F.New_List (Stmts, Field_Stmts);
                           end if;
                        end;
                     end if;
                  end;
               end loop;

               Stmts :=
                 F.New_List
                   (Stmts,
                    F.New_Statement
                      (F.New_Selected_Name (+"Stream.End_Object")));
            end if;

            Result :=
              F.New_Subprogram_Body
                (Specification =>
                   F.New_Subprogram_Specification
                     (Name       => F.New_Name (+"Write"),
                      Parameters =>
                        F.New_List
                          (F.New_Parameter
                             (Name            => F.New_Name (+"Stream"),
                              Type_Definition =>
                                F.New_Selected_Name
                                  (+"PB_Support.JSON.JSON_Writer"),
                              Is_In           => True,
                              Is_Out          => True),
                           F.New_Parameter
                             (Name            => F.New_Name (+"Value"),
                              Type_Definition =>
                                F.New_Selected_Name
                                  (+"Standard." & Pkg & "." & Name)))),
                 Statements    => Stmts);
         end if;

         for J in 1 .. Msg.Nested_Type.Length loop
            declare
               Nested : constant Ada_Pretty.Node_Access :=
                 Generate_Body (Msg.Nested_Type (J), Key);
            begin
               if Nested /= null then
                  Result :=
                    (if Result = null
                     then Nested
                     else F.New_List (Result, Nested));
               end if;
            end;
         end loop;
         return Result;
      end Generate_Body;

      Body_Stmts : Ada_Pretty.Node_Access := null;
      Unit       : Ada_Pretty.Node_Access;
   begin
      for J in 1 .. Self.Message_Type.Length loop
         declare
            Nested : constant Ada_Pretty.Node_Access :=
              Generate_Body (Self.Message_Type (J), Prefix);
         begin
            if Nested /= null then
               Body_Stmts :=
                 (if Body_Stmts = null
                  then Nested
                  else F.New_List (Body_Stmts, Nested));
            end if;
         end;
      end loop;

      declare
         Clauses : Ada_Pretty.Node_Access := null;
      begin
         for J in 1 .. Self.Dependency.Length loop
            declare
               Item   :
                 constant Google.Protobuf.Descriptor.File_Descriptor_Proto :=
                   Compiler.Context.Get_File
                     (Request, Self.Dependency.Element (J));
               Name   : constant League.Strings.Universal_String :=
                 Package_Name (Item) & ".JSON";
               Clause : constant Ada_Pretty.Node_Access :=
                 F.New_With (F.New_Name (Name));
            begin
               if Clauses = null then
                  Clauses := Clause;
               else
                  Clauses := F.New_List (Clauses, Clause);
               end if;
            end;
         end loop;

         declare
            New_Body : Ada_Pretty.Node_Access := Body_Stmts;
         begin
            if Compiler.Context.Runtime_Dep /= Compiler.Runtime_League then
               New_Body :=
                 F.New_List
                   (F.New_Subprogram_Declaration
                      (Specification =>
                         F.New_Subprogram_Specification
                           (Name       => F.New_Name (+"""+"""),
                            Parameters =>
                              F.New_Parameter
                                (Name            => F.New_Name (+"Text"),
                                 Type_Definition =>
                                   F.New_Name
                                     (+"Ada.Strings.Unbounded."
                                      & "Unbounded_String")),
                            Result     => F.New_Name (+"String")),
                       Renamed       =>
                         (F.New_Name (+"Ada.Strings.Unbounded.To_String"))),
                    Body_Stmts);

               declare
                  U_Clause : constant Ada_Pretty.Node_Access :=
                    F.New_With (F.New_Name (+"Ada.Strings.Unbounded"));
               begin
                  if Clauses = null then
                     Clauses := U_Clause;
                  else
                     Clauses := F.New_List (Clauses, U_Clause);
                  end if;
               end;
            else
               New_Body :=
                 F.New_List
                   (F.New_Subprogram_Declaration
                      (Specification =>
                         F.New_Subprogram_Specification
                           (Name       => F.New_Name (+"""+"""),
                            Parameters =>
                              F.New_Parameter
                                (Name            => F.New_Name (+"Text"),
                                 Type_Definition =>
                                   F.New_Name
                                     (+"League.Strings.Universal_String" &
                                      "'Class")),
                            Result     => F.New_Name (+"String")),
                       Renamed       =>
                         F.New_Selected_Name
                           (+"League.Strings.To_UTF_8_String")),
                    Body_Stmts);
               declare
                  U_Clause : constant Ada_Pretty.Node_Access :=
                    F.New_With (F.New_Name (+"League.Strings"));
               begin
                  if Clauses = null then
                     Clauses := U_Clause;
                  else
                     Clauses := F.New_List (Clauses, U_Clause);
                  end if;
               end;
            end if;

            Unit :=
              F.New_Compilation_Unit
                (Root    =>
                   F.New_Package_Body
                     (Name => F.New_Name (Pkg & ".JSON"), List => New_Body),
                 Clauses => Clauses,
                 License => Header_Comment (Self, Request));
         end;
      end;
      return League.Strings."&" (F.To_Text (Unit).Join (LF), LF);
   end JSON_Body_Text;


end Compiler.File_Descriptors;
