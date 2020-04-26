with Compiler.Tools;

with Google_Protobuf.DescriptorProto;

with Ada.Wide_Wide_Text_IO;

package body Compiler.FieldDescriptorProto is

   F : Ada_Pretty.Factory renames Compiler.Contexts.F;

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   function Map (X : Google_Protobuf.FieldDescriptorProto.TypeX)
     return League.Strings.Universal_String;

   function Default (X : Google_Protobuf.FieldDescriptorProto.TypeX)
     return League.Strings.Universal_String;

   function Read_Name
     (Self : Google_Protobuf.FieldDescriptorProto.Instance)
      return League.Strings.Universal_String;

   function Default
     (Self : Google_Protobuf.FieldDescriptorProto.Instance)
      return Ada_Pretty.Node_Access;

   function Is_Message
     (Self : Google_Protobuf.FieldDescriptorProto.Instance)
      return Boolean;

   ---------------
   -- Component --
   ---------------

   function Component
     (Self : Google_Protobuf.FieldDescriptorProto.Instance)
      return Ada_Pretty.Node_Access
   is
      use type Compiler.Contexts.Ada_Type;
      Result : Ada_Pretty.Node_Access;
      Name : constant League.Strings.Universal_String := Field_Name (Self);
      Is_Vector : constant Boolean := Is_Repeated (Self);
   begin
      Result := F.New_Variable
        (Name            => F.New_Name (Name),
         Type_Definition => F.New_Selected_Name (+Type_Name (Self, Is_Vector)),
         Initialization  => Default (Self));

      return Result;
   end Component;

   -------------
   -- Default --
   -------------

   function Default
     (Self : Google_Protobuf.FieldDescriptorProto.Instance)
      return Ada_Pretty.Node_Access
   is
      Result : Ada_Pretty.Node_Access;
   begin
      if Is_Repeated (Self) then
         null;
      elsif Self.Has_Type_Name then
         declare
            Value : constant League.Strings.Universal_String :=
              League.Strings.From_UTF_8_String (Self.Get_TypeX_Name);
         begin
            if Compiler.Contexts.Type_Map.Contains (Value) then
               declare
                  Full    : League.Strings.Universal_String;
                  Element : constant Compiler.Contexts.Ada_Type_Info :=
                    Compiler.Contexts.Type_Map (Value);
               begin
                  if not Element.Default.Is_Empty then
                     Full := Element.T.Package_Name;
                     Full.Append (".");
                     Full.Append (Element.Default);
                     Result := F.New_Selected_Name (Full);
                  end if;
               end;
            else
               Ada.Wide_Wide_Text_IO.Put_Line
                 (Ada.Wide_Wide_Text_IO.Standard_Error,
                  "Type not found: " & Value.To_Wide_Wide_String);
            end if;
         end;
      elsif Self.Has_Type_Pb then
         declare
            Value : constant League.Strings.Universal_String :=
              Default (Self.Get_Type_Pb);
         begin
            if not Value.Is_Empty then
               Result := F.New_Name (Value);
            end if;
         end;
      end if;

      return Result;

   end Default;

   ------------------
   -- PB_Type_Name --
   ------------------

   function PB_Type_Name
     (Self : Google_Protobuf.FieldDescriptorProto.Instance)
      return League.Strings.Universal_String
   is
   begin
      if Self.Has_Type_Name then
         return League.Strings.From_UTF_8_String (Self.Get_TypeX_Name);
      else
         return League.Strings.Empty_Universal_String;
      end if;
   end PB_Type_Name;

   ----------------
   -- Dependency --
   ----------------

   function Dependency
     (Self : Google_Protobuf.FieldDescriptorProto.Instance)
      return Compiler.Contexts.String_Sets.Set
   is
      Result : Compiler.Contexts.String_Sets.Set;
      Is_Vector : constant Boolean := Is_Repeated (Self);
      My_Pkg : constant League.Strings.Universal_String :=
        Type_Name (Self, Is_Vector).Package_Name;
   begin
      if not My_Pkg.Is_Empty then
         Result.Insert (My_Pkg);
      end if;

      return Result;
   end Dependency;

   ----------------
   -- Field_Name --
   ----------------

   function Field_Name
     (Self : Google_Protobuf.FieldDescriptorProto.Instance)
      return League.Strings.Universal_String
   is
      Result : League.Strings.Universal_String;
   begin
      if Self.Has_Name then
         declare
            Value : constant League.Strings.Universal_String :=
              League.Strings.From_UTF_8_String (Self.Get_Name);
         begin
            Result := Compiler.Tools.To_Ada_Name (Value);
         end;
      else
         Result.Append ("Message");
      end if;

      return Result;
   end Field_Name;

   function Default (X : Google_Protobuf.FieldDescriptorProto.TypeX)
     return League.Strings.Universal_String
   is
      use Google_Protobuf.Enumeration.FieldDescriptorProto;
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

   --------------------
   -- Get_Used_Types --
   --------------------

   procedure Get_Used_Types
     (Self   : Google_Protobuf.FieldDescriptorProto.Instance;
      Result : in out Compiler.Contexts.String_Sets.Set)
   is
      Value : constant League.Strings.Universal_String := PB_Type_Name (Self);
   begin
      Result.Include (Value);
   end Get_Used_Types;

   ---------
   -- Map --
   ---------

   function Map (X : Google_Protobuf.FieldDescriptorProto.TypeX)
     return League.Strings.Universal_String
   is
      use Google_Protobuf.Enumeration.FieldDescriptorProto;
   begin
      case X is
         when TYPE_DOUBLE   => return +"Interfaces.IEEE_Float_64";
         when TYPE_FLOAT    => return +"Interfaces.IEEE_Float_32";
         when TYPE_INT64    => return +"Interfaces.Integer_64";
         when TYPE_UINT64   => return +"Interfaces.Unsigned_64";
         when TYPE_INT32    => return +"Interfaces.Unsigned_32";
         when TYPE_FIXED64  => return +"Interfaces.Unsigned_64";
         when TYPE_FIXED32  => return +"Interfaces.Unsigned_32";
         when TYPE_BOOL     => return +"Boolean";
         when TYPE_STRING   => return +"League.Strings.Universal_String";
         when TYPE_GROUP    => return +"group";
         when TYPE_MESSAGE  => return +"message";
         when TYPE_BYTES    => return +("League.Stream_Element_Vectors" &
              ".Stream_Element_Vector");
         when TYPE_UINT32   => return +"Interfaces.Unsigned_32";
         when TYPE_ENUM     => return +"enum";
         when TYPE_SFIXED32 => return +"Interfaces.Integer_32";
         when TYPE_SFIXED64 => return +"Interfaces.Integer_64";
         when TYPE_SINT32   => return +"Interfaces.Integer_32";
         when TYPE_SINT64   => return +"Interfaces.Integer_64";
      end case;
   end Map;

   ----------------
   -- Is_Message --
   ----------------

   function Is_Message
     (Self : Google_Protobuf.FieldDescriptorProto.Instance)
      return Boolean
   is
      use type Google_Protobuf.DescriptorProto.DescriptorProto_Access;
      PB_Type : constant League.Strings.Universal_String :=
        PB_Type_Name (Self);
   begin
      return Compiler.Contexts.Type_Map.Contains (PB_Type)
        and then Compiler.Contexts.Type_Map (PB_Type).Message /= null;
   end Is_Message;

   -----------------
   -- Is_Optional --
   -----------------

   function Is_Optional
     (Self : Google_Protobuf.FieldDescriptorProto.Instance)
      return Boolean
   is
      use type Google_Protobuf.FieldDescriptorProto.Label;
   begin
      if Self.Has_Label then
         return Self.Get_Label =
           Google_Protobuf.Enumeration.FieldDescriptorProto.LABEL_OPTIONAL;
      end if;

      return False;
   end Is_Optional;

   -----------------
   -- Is_Repeated --
   -----------------

   function Is_Repeated
     (Self : Google_Protobuf.FieldDescriptorProto.Instance)
      return Boolean
   is
      use type Google_Protobuf.FieldDescriptorProto.Label;
   begin
      if Self.Has_Label then
         return Self.Get_Label =
           Google_Protobuf.Enumeration.FieldDescriptorProto.LABEL_REPEATED;
      end if;

      return False;
   end Is_Repeated;

   ---------------
   -- Read_Case --
   ---------------

   function Read_Case
     (Self : Google_Protobuf.FieldDescriptorProto.Instance)
      return Ada_Pretty.Node_Access
   is
      use type League.Strings.Universal_String;
      My_Name : League.Strings.Universal_String := Field_Name (Self);
      Result  : Ada_Pretty.Node_Access;
      Field   : Integer;
   begin
      Field := Integer (Self.Get_Number);
      My_Name.Prepend ("Value.");

      if Is_Optional (Self) and Is_Message (Self) then
         Result := F.New_If
           (Condition  => F.New_Infix
              (Operator => +"not",
               Left     => F.New_Selected_Name
                (My_Name & ".Is_Set")),
            Then_Path  => F.New_Assignment
              (Left  => F.New_Selected_Name (My_Name),
               Right => F.New_Parentheses
                 (F.New_List
                      (F.New_Argument_Association
                           (F.New_Name (+"True")),
                       F.New_Argument_Association
                         (F.New_Name (+"others => <>"))))));

         My_Name.Append (".Value");
      end if;

      Result := F.New_List
        (Result,
         F.New_Statement
           (F.New_Apply
             (Prefix    => F.New_Selected_Name
                  (Read_Name (Self)),
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
     (Self : Google_Protobuf.FieldDescriptorProto.Instance)
      return League.Strings.Universal_String
   is
      use type League.Strings.Universal_String;
      Result : League.Strings.Universal_String := +"PB_Support.IO.Read_";
      Tp  : constant Compiler.Contexts.Ada_Type := Type_Name (Self, False);
      PB_Type : constant League.Strings.Universal_String :=
        PB_Type_Name (Self);
   begin
      if Compiler.Contexts.Type_Map.Contains (PB_Type) then
         Result := Compiler.Contexts.Type_Map (PB_Type).T.Type_Name;
         Result.Append ("_IO.Read");
      else
         Result.Append (Tp.Type_Name);
      end if;

      if Is_Repeated (Self) then
         Result.Append ("_Vector");
      end if;

      return Result;
   end Read_Name;

   ---------------
   -- Type_Name --
   ---------------

   function Type_Name
     (Self        : Google_Protobuf.FieldDescriptorProto.Instance;
      Is_Repeated : Boolean)
      return Compiler.Contexts.Ada_Type
   is
      use type League.Strings.Universal_String;
      use all type Google_Protobuf.FieldDescriptorProto.TypeX;
      Result : Compiler.Contexts.Ada_Type;
      TypeX : constant Google_Protobuf.FieldDescriptorProto.TypeX :=
        Self.Get_Type_Pb;
   begin
      if Self.Has_Type_Name then
         declare
            Value : constant League.Strings.Universal_String :=
              League.Strings.From_UTF_8_String (Self.Get_TypeX_Name);
         begin
            if Compiler.Contexts.Type_Map.Contains (Value) then
               declare
                  Element : constant Compiler.Contexts.Ada_Type_Info :=
                    Compiler.Contexts.Type_Map (Value);
               begin
                  Result := Element.T;

                  if Is_Repeated then
                     Result.Type_Name.Append ("_Vector");
                  elsif Is_Optional (Self)
                    and (not Self.Has_Type_Pb or TypeX /= TYPE_ENUM)
                  then
                     Result.Type_Name.Prepend ("Optional_");
                  end if;
               end;
            else
               Ada.Wide_Wide_Text_IO.Put_Line
                 (Ada.Wide_Wide_Text_IO.Standard_Error,
                  "Type not found: " & Value.To_Wide_Wide_String);
            end if;
         end;
      elsif Self.Has_Type_Pb then
         declare
            Text  : constant League.Strings.Universal_String := Map (TypeX);
         begin
            if Text.Index ('.') > 0 then
               Result.Package_Name := Text.Head_To (Text.Last_Index ('.') - 1);
               Result.Type_Name := Text.Tail_From (Text.Last_Index ('.') + 1);
            else
               Result.Type_Name := Text;
            end if;

            if Is_Repeated then
               if TypeX = TYPE_STRING then
                  Result.Package_Name := +"League.String_Vectors";
                  Result.Type_Name := +"Universal_String_Vector";
               else
                  Result.Package_Name :=
                    "PB_Support." & Result.Type_Name & "_Vectors";
                  Result.Type_Name := +"Vector";
               end if;
            end if;
         end;
      else
         Result.Type_Name := +"Field_Type";
      end if;

      return Result;
   end Type_Name;

end Compiler.FieldDescriptorProto;
