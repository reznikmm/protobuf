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

package body Compiler.Field_Descriptors is

   use all type Google.Protobuf.Descriptor.Label;

   F : Ada_Pretty.Factory renames Compiler.Context.Factory;

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   function Type_Name
     (Self        : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
      Is_Option   : Boolean;
      Is_Repeated : Boolean) return Compiler.Context.Ada_Type_Name;

   function Default (X : Google.Protobuf.Descriptor.PB_Type)
     return League.Strings.Universal_String;
   --  Default value for a predefined type

   function Default
     (Self      : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
      Is_Option : Boolean;
      Pkg       : League.Strings.Universal_String;
      Tipe      : League.Strings.Universal_String;
      Fake      : Compiler.Context.String_Sets.Set)
      return Ada_Pretty.Node_Access;
   --  Default value for a field

   function Map (X : Google.Protobuf.Descriptor.PB_Type)
     return Compiler.Context.Ada_Type_Name;

   function Is_Enum
     (Self : Google.Protobuf.Descriptor.Field_Descriptor_Proto)
       return Boolean;

   function Is_Repeated
     (Self : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
      Pkg  : League.Strings.Universal_String;
      Tipe : League.Strings.Universal_String;
      Fake : Compiler.Context.String_Sets.Set)
      return Boolean;

   function Is_Packed
     (Self : Google.Protobuf.Descriptor.Field_Descriptor_Proto)
       return Boolean;

   function Is_Optional
     (Self : Google.Protobuf.Descriptor.Field_Descriptor_Proto)
       return Boolean;

   function Is_Message
     (Self : Google.Protobuf.Descriptor.Field_Descriptor_Proto)
       return Boolean;

   function Read_Name
     (Self : Google.Protobuf.Descriptor.Field_Descriptor_Proto)
      return League.Strings.Universal_String;

   function Write_Name
     (Self      : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
      Is_Option : Boolean)
      return League.Strings.Universal_String;

   ---------------
   -- Case_Path --
   ---------------

   function Case_Path
     (Self : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
      Pkg  : League.Strings.Universal_String;
      Tipe : League.Strings.Universal_String;
      Fake : Compiler.Context.String_Sets.Set)
      return Ada_Pretty.Node_Access
   is
      use type League.Strings.Universal_String;

      My_Name : constant League.Strings.Universal_String :=
        Compiler.Context.To_Ada_Name (Self.Name.Value);
      Result : Ada_Pretty.Node_Access;
   begin
      Result := F.New_Case_Path
        (F.New_Name (My_Name & "_Kind"),
         Write_Call (Self, Pkg, Tipe, Fake));

      return Result;
   end Case_Path;

   ---------------
   -- Component --
   ---------------

   function Component
     (Self : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
      Pkg  : League.Strings.Universal_String;
      Tipe : League.Strings.Universal_String;
      Fake : Compiler.Context.String_Sets.Set)
      return Ada_Pretty.Node_Access
   is
      use type Compiler.Context.Ada_Type_Name;
      Result : Ada_Pretty.Node_Access;
      Name : constant League.Strings.Universal_String :=
        Compiler.Context.To_Ada_Name (Self.Name.Value);
      Is_Vector : constant Boolean := Is_Repeated (Self, Pkg, Tipe, Fake);
      Is_Option : constant Boolean :=
        Is_Optional (Self) and not Is_Vector and not Self.Oneof_Index.Is_Set;
      My_Type : constant League.Strings.Universal_String :=
        Compiler.Context.Relative_Name
          (+Type_Name (Self, Is_Option,  Is_Vector), Pkg);
   begin
      Result := F.New_Variable
        (Name            => F.New_Name (Name),
         Type_Definition => F.New_Selected_Name (My_Type),
         Initialization  => Default (Self, Is_Option, Pkg, Tipe, Fake));

      return Result;
   end Component;

   -------------
   -- Default --
   -------------

   function Default (X : Google.Protobuf.Descriptor.PB_Type)
     return League.Strings.Universal_String
   is
      use all type Google.Protobuf.Descriptor.PB_Type;
   begin
      case X is
         when TYPE_DOUBLE   => return +"0.0";
         when TYPE_FLOAT    => return +"0.0";
         when TYPE_INT64    => return +"0";
         when TYPE_UINT64   => return +"0";
         when TYPE_INT32    => return +"0";
         when TYPE_FIXED64  => return +"0";
         when TYPE_FIXED32  => return +"0";
         when TYPE_BOOL     => return +"False";
         when TYPE_STRING   => return League.Strings.Empty_Universal_String;
         when TYPE_GROUP    => return League.Strings.Empty_Universal_String;
         when TYPE_MESSAGE  => return League.Strings.Empty_Universal_String;
         when TYPE_BYTES    => return League.Strings.Empty_Universal_String;
         when TYPE_UINT32   => return +"0";
         when TYPE_ENUM     => return League.Strings.Empty_Universal_String;
         when TYPE_SFIXED32 => return +"0";
         when TYPE_SFIXED64 => return +"0";
         when TYPE_SINT32   => return +"0";
         when TYPE_SINT64   => return +"0";
      end case;
   end Default;

   -------------
   -- Default --
   -------------

   function Default
     (Self      : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
      Is_Option : Boolean;
      Pkg       : League.Strings.Universal_String;
      Tipe      : League.Strings.Universal_String;
      Fake      : Compiler.Context.String_Sets.Set)
      return Ada_Pretty.Node_Access
   is
      Result : Ada_Pretty.Node_Access;
   begin
      if Is_Repeated (Self, Pkg, Tipe, Fake) then
         null;
      elsif Is_Option and Compiler.Context.Is_Proto_2 then
         null;
      elsif Self.Type_Name.Is_Set then
         declare
            Value : constant League.Strings.Universal_String :=
              Self.Type_Name.Value;
         begin
            if Compiler.Context.Named_Types.Contains (Value) then
               declare
                  Full    : League.Strings.Universal_String;
                  Element : constant Compiler.Context.Named_Type :=
                    Compiler.Context.Named_Types (Value);
               begin
                  if Element.Is_Enumeration then
                     Full := Element.Ada_Type.Package_Name;
                     Full.Append (".");
                     Full.Append (Element.Enum.Default);
                     Full := Compiler.Context.Relative_Name (Full, Pkg);
                     Result := F.New_Selected_Name (Full);
                  end if;
               end;
            else
               raise Constraint_Error with
                  "Type not found: " & Value.To_UTF_8_String;
            end if;
         end;
      elsif Self.PB_Type.Is_Set then
         declare
            Value : constant League.Strings.Universal_String :=
              Default (Self.PB_Type.Value);
         begin
            if not Value.Is_Empty then
               Result := F.New_Name (Value);
            end if;
         end;
      end if;

      return Result;
   end Default;

   ----------------
   -- Dependency --
   ----------------

   procedure Dependency
     (Self   : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
      Result : in out Compiler.Context.String_Sets.Set)
   is
      Is_Vector : constant Boolean := Is_Repeated
        (Self, +"", +"", Compiler.Context.String_Sets.Empty_Set);
      Is_Option : constant Boolean :=
        Is_Optional (Self) and not Is_Vector and not Self.Oneof_Index.Is_Set;
      My_Pkg    : constant League.Strings.Universal_String :=
        Type_Name (Self, Is_Option, Is_Vector).Package_Name;
   begin
      if not My_Pkg.Is_Empty then
         Result.Include (My_Pkg);
      end if;

      if Is_Enum (Self) then
         Result.Include (+"PB_Support.Vectors");
      end if;
   end Dependency;

   --------------------
   -- Get_Used_Types --
   --------------------

   procedure Get_Used_Types
     (Self   : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
      Result : in out Compiler.Context.String_Sets.Set) is
   begin
      if Self.Type_Name.Is_Set then
         Result.Include (Self.Type_Name.Value);
      end if;
   end Get_Used_Types;

   -------------
   -- Is_Enum --
   -------------

   function Is_Enum
     (Self : Google.Protobuf.Descriptor.Field_Descriptor_Proto)
       return Boolean is
   begin
      if Self.Type_Name.Is_Set
        and then Compiler.Context.Named_Types.Contains (Self.Type_Name.Value)
      then
         return
           Compiler.Context.Named_Types (Self.Type_Name.Value).Is_Enumeration;
      else
         return False;
      end if;
   end Is_Enum;

   ----------------
   -- Is_Message --
   ----------------

   function Is_Message
     (Self : Google.Protobuf.Descriptor.Field_Descriptor_Proto)
       return Boolean is
   begin
      if Self.Type_Name.Is_Set
        and then Compiler.Context.Named_Types.Contains (Self.Type_Name.Value)
      then
         return not
           Compiler.Context.Named_Types (Self.Type_Name.Value).Is_Enumeration;
      else
         return False;
      end if;
   end Is_Message;

   -----------------
   -- Is_Optional --
   -----------------

   function Is_Optional
     (Self : Google.Protobuf.Descriptor.Field_Descriptor_Proto)
       return Boolean is
   begin
      return not Self.Label.Is_Set or else Self.Label.Value = LABEL_OPTIONAL;
   end Is_Optional;

   ---------------
   -- Is_Packed --
   ---------------

   function Is_Packed
     (Self : Google.Protobuf.Descriptor.Field_Descriptor_Proto)
      return Boolean
   is
      use all type Google.Protobuf.Descriptor.PB_Type;

      Is_Primitive_Numeric_Vector : constant Boolean :=
        Self.Label.Is_Set
        and then Self.Label.Value = LABEL_REPEATED
        and then Self.PB_Type.Is_Set
        and then Self.PB_Type.Value not in TYPE_BYTES | TYPE_STRING;

      Packed : constant Boolean :=
        (Self.Options.Is_Set
         and then Self.Options.Value.Packed.Is_Set
         and then Self.Options.Value.Packed.Value)  --  set explicitly
        or else                                     --  default in proto3
          (Is_Primitive_Numeric_Vector
           and not Self.Options.Is_Set
           and not Compiler.Context.Is_Proto_2)
        or else                                     --  default in proto3
          (Is_Primitive_Numeric_Vector
           and then Self.Options.Is_Set
           and then not Self.Options.Value.Packed.Is_Set
           and then not Compiler.Context.Is_Proto_2);
   begin
      return Packed;
   end Is_Packed;

   -----------------
   -- Is_Repeated --
   -----------------

   function Is_Repeated
     (Self : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
      Pkg  : League.Strings.Universal_String;
      Tipe : League.Strings.Universal_String;
      Fake : Compiler.Context.String_Sets.Set)
      return Boolean is
   begin
      return (Self.Label.Is_Set and then Self.Label.Value = LABEL_REPEATED)
       or else Fake.Contains (Unique_Id (Self, Pkg, Tipe));
   end Is_Repeated;

   ---------
   -- Map --
   ---------

   function Map (X : Google.Protobuf.Descriptor.PB_Type)
     return Compiler.Context.Ada_Type_Name
   is
      use all type Google.Protobuf.Descriptor.PB_Type;
   begin
      case X is
         when TYPE_DOUBLE   => return (+"Interfaces", +"IEEE_Float_64");
         when TYPE_FLOAT    => return (+"Interfaces", +"IEEE_Float_32");
         when TYPE_INT64    => return (+"Interfaces", +"Integer_64");
         when TYPE_UINT64   => return (+"Interfaces", +"Unsigned_64");
         when TYPE_INT32    => return (+"Interfaces", +"Integer_32");
         when TYPE_FIXED64  => return (+"Interfaces", +"Unsigned_64");
         when TYPE_FIXED32  => return (+"Interfaces", +"Unsigned_32");
         when TYPE_BOOL     => return (+"", +"Boolean");
         when TYPE_STRING   => return (+"League.Strings", +"Universal_String");
         when TYPE_BYTES    => return
              (+"League.Stream_Element_Vectors",
               +"Stream_Element_Vector");
         when TYPE_UINT32   => return (+"Interfaces", +"Unsigned_32");
         when TYPE_SFIXED32 => return (+"Interfaces", +"Integer_32");
         when TYPE_SFIXED64 => return (+"Interfaces", +"Integer_64");
         when TYPE_SINT32   => return (+"Interfaces", +"Integer_32");
         when TYPE_SINT64   => return (+"Interfaces", +"Integer_64");
         when TYPE_GROUP | TYPE_MESSAGE | TYPE_ENUM =>
            raise Program_Error;
      end case;
   end Map;

   ---------------
   -- Read_Case --
   ---------------

   function Read_Case
     (Self  : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
      Pkg   : League.Strings.Universal_String;
      Tipe  : League.Strings.Universal_String;
      Fake  : Compiler.Context.String_Sets.Set;
      Oneof : League.Strings.Universal_String)
      return Ada_Pretty.Node_Access
   is
      use type League.Strings.Universal_String;
      Is_Vector : constant Boolean := Is_Repeated (Self, Pkg, Tipe, Fake);
      My_Name : League.Strings.Universal_String :=
        Compiler.Context.To_Ada_Name (Self.Name.Value);
      Result  : Ada_Pretty.Node_Access;
      Field   : Integer;
   begin
      Field := Integer (Self.Number.Value);

      if Self.Oneof_Index.Is_Set then
         Result := F.New_If
          (Condition  => F.New_List
            (F.New_Selected_Name ("V.Variant." & Oneof),
             F.New_Infix (+"/=", F.New_Name (My_Name & "_Kind"))),
           Then_Path  => F.New_Assignment
            (F.New_Selected_Name (+"V.Variant"),
             F.New_Parentheses
              (F.New_List
                (F.New_Component_Association (F.New_Name (My_Name & "_Kind")),
                 F.New_Component_Association
                  (Choices => F.New_Name (+"others"),
                   Value   => F.New_Name (+"<>"))))));

         My_Name.Prepend ("V.Variant.");
      elsif not Is_Vector and Is_Optional (Self)
        and (Is_Message (Self) or Compiler.Context.Is_Proto_2)
      then
         My_Name.Prepend ("V.");
         Result := F.New_If
           (Condition  => F.New_Infix
              (Operator => +"not",
               Left     => F.New_Selected_Name
                (My_Name & ".Is_Set")),
            Then_Path  => F.New_Assignment
              (Left  => F.New_Selected_Name (My_Name),
               Right => F.New_Parentheses
                 (F.New_List
                      (F.New_Component_Association
                           (F.New_Name (+"True")),
                       F.New_Component_Association
                      (Choices => F.New_Name (+"others"),
                       Value   => F.New_Name (+"<>"))))));

         My_Name.Append (".Value");
      else
         My_Name.Prepend ("V.");
      end if;

      if Fake.Contains (Unique_Id (Self, Pkg, Tipe)) then
         Result := F.New_List
          (Result,
           F.New_If
            (Condition  => F.New_List
              (F.New_Selected_Name (My_Name & ".Length"),
               F.New_Infix (+"=", F.New_Literal (0))),
             Then_Path  => F.New_Statement
              (F.New_Apply
                (F.New_Selected_Name (My_Name & ".Append"),
                 F.New_Parentheses (F.New_Name (+"others => <>"))))));

         My_Name.Append (" (1)");
      end if;

      Result := F.New_List
        (Result,
         F.New_Statement
           (F.New_Apply
             (Prefix    => F.New_Selected_Name (Read_Name (Self)),
              Arguments => F.New_List
                ((F.New_Argument_Association (F.New_Name (+"Stream")),
                  F.New_Argument_Association
                    (F.New_Selected_Name (+"Key.Encoding")),
                  F.New_Argument_Association
                    (F.New_Selected_Name (My_Name)))))));

      Result := F.New_Case_Path
        (Choice => F.New_Literal (Field),
         List   => Result);
      return Result;
   end Read_Case;

   ---------------
   -- Read_Name --
   ---------------

   function Read_Name
     (Self : Google.Protobuf.Descriptor.Field_Descriptor_Proto)
      return League.Strings.Universal_String
   is
      use all type Google.Protobuf.Descriptor.PB_Type;
      use type League.Strings.Universal_String;
      Result  : League.Strings.Universal_String := +"PB_Support.IO.Read";
      Is_Vector : constant Boolean :=
        Self.Label.Is_Set and then Self.Label.Value = LABEL_REPEATED;
   begin
      if Self.Type_Name.Is_Set
        and then Compiler.Context.Named_Types.Contains (Self.Type_Name.Value)
      then
         Result := Compiler.Context.Named_Types
           (Self.Type_Name.Value).Ada_Type.Type_Name;
         Result.Append ("_IO.Read");
      elsif Self.PB_Type.Is_Set and then Self.PB_Type.Value in
        TYPE_INT64 | TYPE_UINT64 | TYPE_INT32 | TYPE_UINT32
      then
         Result.Append ("_Varint");
      elsif Self.PB_Type.Is_Set and then Self.PB_Type.Value in
        TYPE_FIXED64 | TYPE_FIXED32 | TYPE_SFIXED32 | TYPE_SFIXED64
      then
         Result.Append ("_Fixed");
      elsif Self.PB_Type.Is_Set and then Self.PB_Type.Value in
        TYPE_SINT32 | TYPE_SINT64
      then
         Result.Append ("_Zigzag");
      end if;

      if Is_Vector then
         Result.Append ("_Vector");
      end if;

      return Result;
   end Read_Name;

   ---------------
   -- Type_Name --
   ---------------

   function Type_Name
     (Self        : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
      Is_Option   : Boolean;
      Is_Repeated : Boolean)
      return Compiler.Context.Ada_Type_Name
   is
      use type League.Strings.Universal_String;
      use all type Google.Protobuf.Descriptor.PB_Type;
      Result : Compiler.Context.Ada_Type_Name;
   begin
      if Self.Type_Name.Is_Set then  --  Message or enum
         declare
            Value : constant League.Strings.Universal_String :=
              Self.Type_Name.Value;
         begin
            if Compiler.Context.Named_Types.Contains (Value) then
               declare
                  Element : constant Compiler.Context.Named_Type :=
                    Compiler.Context.Named_Types (Value);
               begin
                  Result := Element.Ada_Type;

                  if Element.Is_Enumeration then
                     if Is_Repeated then
                        Result.Type_Name.Append ("_Vectors.Vector");
                     elsif Is_Option and Compiler.Context.Is_Proto_2 then
                        Result.Type_Name.Append ("_Vectors.Option");
                     end if;
                  elsif Is_Repeated then
                     Result.Type_Name.Append ("_Vector");
                  elsif Is_Option and not Self.Oneof_Index.Is_Set then
                     Result.Type_Name.Prepend ("Optional_");
                  end if;
               end;
            else
               raise Constraint_Error with
                 "Type not found: " & Value.To_UTF_8_String;
            end if;
         end;
      elsif Is_Option and Compiler.Context.Is_Proto_2 then
         Result := Map (Self.PB_Type.Value);
         Result.Package_Name :=
           "PB_Support." & Result.Type_Name & "_Vectors";
         Result.Type_Name := +"Option";
      elsif not Is_Repeated then
         Result := Map (Self.PB_Type.Value);
      elsif Self.PB_Type.Value = TYPE_STRING then
         Result := (+"League.String_Vectors", +"Universal_String_Vector");
      else
         Result := Map (Self.PB_Type.Value);
         Result.Package_Name :=
           "PB_Support." & Result.Type_Name & "_Vectors";
         Result.Type_Name := +"Vector";
      end if;

      return Result;
   end Type_Name;

   ---------------
   -- Unique_Id --
   ---------------

   function Unique_Id
     (Self : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
      Pkg  : League.Strings.Universal_String;
      Tipe : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is
      use type League.Strings.Universal_String;
   begin
      return Pkg & "." & Tipe & "." & Self.Name.Value;
   end Unique_Id;

   ----------------
   -- Write_Call --
   ----------------

   function Write_Call
     (Self : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
      Pkg  : League.Strings.Universal_String;
      Tipe : League.Strings.Universal_String;
      Fake : Compiler.Context.String_Sets.Set)
      return Ada_Pretty.Node_Access
   is
      use type Compiler.Context.Ada_Type_Name;
      use type League.Strings.Universal_String;

      Is_Enum   : constant Boolean := Field_Descriptors.Is_Enum (Self);
      Is_Vector : constant Boolean := Is_Repeated (Self, Pkg, Tipe, Fake);
      Is_Option : constant Boolean := Is_Optional (Self)
        and not Is_Vector
        and not Self.Oneof_Index.Is_Set;

      My_Name : constant League.Strings.Universal_String :=
        Compiler.Context.To_Ada_Name (Self.Name.Value);
      Result  : Ada_Pretty.Node_Access;
      Get     : League.Strings.Universal_String;
      Full    : League.Strings.Universal_String;
      Initial : League.Strings.Universal_String;
      Value   : League.Strings.Universal_String := "V." & My_Name;
   begin
      if Self.Oneof_Index.Is_Set then
         Value := "V.Variant." & My_Name;
      elsif Is_Message (Self) then
         if Is_Vector then
            Value.Append (+" (J)");
         elsif Is_Option then
            Value.Append (+".Value");
         end if;
      elsif Is_Option and Compiler.Context.Is_Proto_2 then
         Value.Append (+".Value");
      end if;

      if Is_Message (Self) then

         Full := Compiler.Context.Relative_Name
           (+Type_Name (Self, False, False), Pkg);

         Result := F.New_List
           (F.New_Statement
             (F.New_Apply
               (F.New_Selected_Name (+"WS.Write_Key"),
                F.New_Argument_Association
                 (F.New_Parentheses
                   (F.New_List
                     (F.New_Argument_Association
                       (F.New_Literal (Integer (Self.Number.Value))),
                      F.New_Argument_Association
                       (F.New_Selected_Name
                         (+"PB_Support.Length_Delimited"))))))),
            F.New_Statement
             (F.New_Apply
               (F.New_Selected_Name (Full & "'Write"),
                F.New_List
                 (F.New_Name (+"Stream"),
                  F.New_Selected_Name (Value)))));

         if Is_Vector then
            Result := F.New_For
              (F.New_Name (+"J"),
               F.New_Name (+"1 .. V." & My_Name & ".Length"),
               Result);
         end if;
      elsif Is_Enum then

         Get := Compiler.Context.Named_Types
           (Self.Type_Name.Value).Ada_Type.Type_Name;

         Result := F.New_List
           ((F.New_Argument_Association (F.New_Name (+"WS")),
            F.New_Argument_Association
              (F.New_Literal (Integer (Self.Number.Value))),
            F.New_Argument_Association
              (F.New_Selected_Name (Value))));

         if Is_Option and not Compiler.Context.Is_Proto_2 then
            Result := F.New_Apply
             (F.New_Selected_Name (Get & "_IO.Write_Option"),
              F.New_List
                (Result,
                 Default (Self, False, Pkg, Tipe, Fake)));
         elsif Is_Packed (Self) then
            Result := F.New_Apply
             (F.New_Selected_Name (Get & "_IO.Write_Packed"),
              Result);
         else
            Result := F.New_Apply
             (F.New_Selected_Name (Get & "_IO.Write"),
              Result);
         end if;

         Result := F.New_Statement (Result);

      else
         Result := F.New_List
           (F.New_Argument_Association
             (F.New_Literal (Integer (Self.Number.Value))),
            F.New_Argument_Association
             (F.New_Selected_Name (Value)));

         if Is_Option and not Compiler.Context.Is_Proto_2 then
            Initial := Default (Self.PB_Type.Value);

            if Initial.Is_Empty then
               Result := F.New_Apply
                 (F.New_Selected_Name ("WS." & Write_Name (Self, Is_Option)),
                  Result);
            else
               Result := F.New_Apply
                 (F.New_Selected_Name ("WS." & Write_Name (Self, Is_Option)),
                  F.New_List (Result, F.New_Name (Initial)));
            end if;
         else
            Result := F.New_Apply
              (F.New_Selected_Name ("WS." & Write_Name (Self, Is_Option)),
               Result);
         end if;


         Result := F.New_Statement (Result);
      end if;

      if Is_Option and
        (Is_Message (Self) or Compiler.Context.Is_Proto_2) and
        not Self.Oneof_Index.Is_Set
      then
         Result := F.New_If
           (F.New_Selected_Name ("V." & My_Name & ".Is_Set"),
            Result);
      end if;

      return Result;
   end Write_Call;

   ----------------
   -- Write_Name --
   ----------------

   function Write_Name
     (Self      : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
      Is_Option : Boolean)
      return League.Strings.Universal_String
   is
      use all type Google.Protobuf.Descriptor.PB_Type;

      Result : League.Strings.Universal_String := +"Write";
      Packed : constant Boolean := Is_Packed (Self);
   begin
      if Self.PB_Type.Is_Set and then Self.PB_Type.Value in
        TYPE_INT64 | TYPE_UINT64 | TYPE_INT32 | TYPE_UINT32
      then
         Result.Append ("_Varint");
      elsif Self.PB_Type.Is_Set and then Self.PB_Type.Value in
        TYPE_FIXED64 | TYPE_FIXED32 | TYPE_SFIXED32 | TYPE_SFIXED64
      then
         Result.Append ("_Fixed");
      elsif Self.PB_Type.Is_Set and then Self.PB_Type.Value in
        TYPE_SINT32 | TYPE_SINT64
      then
         Result.Append ("_Zigzag");
      end if;

      if Is_Option and not Compiler.Context.Is_Proto_2 then
         Result.Append ("_Option");
      elsif Packed then
         Result.Append ("_Packed");
      end if;

      return Result;
   end Write_Name;

end Compiler.Field_Descriptors;
