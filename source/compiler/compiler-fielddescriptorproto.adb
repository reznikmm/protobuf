with Compiler.Contexts;
with Compiler.DescriptorProto;
with Compiler.Tools;

with Ada.Wide_Wide_Text_IO;

package body Compiler.FieldDescriptorProto is

   F : Ada_Pretty.Factory renames Compiler.Contexts.F;

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   function Map (X : Google_Protobuf.FieldDescriptorProto.TypeX)
     return League.Strings.Universal_String;

   function Parameters
     (Field   : Google_Protobuf.FieldDescriptorProto.Instance;
      Message : Google_Protobuf.DescriptorProto.Instance)
      return Ada_Pretty.Node_Access;

   function Get_Self_Parameter
     (Message : Google_Protobuf.DescriptorProto.Instance;
      Change  : Boolean := False)
      return Ada_Pretty.Node_Access;

   ------------------------
   -- Get_Self_Parameter --
   ------------------------

   function Get_Self_Parameter
     (Message : Google_Protobuf.DescriptorProto.Instance;
      Change  : Boolean := False) return Ada_Pretty.Node_Access
   is
      Msg  : constant League.Strings.Universal_String :=
        Compiler.DescriptorProto.Type_Name (Message);

      Self : constant Ada_Pretty.Node_Access := F.New_Parameter
        (Name            => F.New_Name (+"Self"),
         Type_Definition => F.New_Name (Msg),
         Is_In           => Change,
         Is_Out          => Change);

   begin
      return Self;
   end Get_Self_Parameter;

   ----------------
   -- Parameters --
   ----------------

   function Parameters
     (Field   : Google_Protobuf.FieldDescriptorProto.Instance;
      Message : Google_Protobuf.DescriptorProto.Instance)
      return Ada_Pretty.Node_Access
   is
      Self : constant Ada_Pretty.Node_Access := Get_Self_Parameter (Message);

      Index : Ada_Pretty.Node_Access;
   begin
      if Is_Repeated (Field) then
         Index := F.New_Parameter
           (Name            => F.New_Name (+"Index"),
            Type_Definition => F.New_Name (+"Positive"));

         return F.New_List (Self, Index);
      else
         return Self;
      end if;
   end Parameters;

   -----------------
   -- Public_Spec --
   -----------------

   function Public_Spec
     (Self    : Google_Protobuf.FieldDescriptorProto.Instance;
      Message : Google_Protobuf.DescriptorProto.Instance)
      return Ada_Pretty.Node_Access
   is
      use type League.Strings.Universal_String;
      Result : Ada_Pretty.Node_Access;
      Item   : Ada_Pretty.Node_Access;
      Field_Type : constant Ada_Pretty.Node_Access :=
        F.New_Selected_Name (Type_Name (Self));
      Name : constant League.Strings.Universal_String := Field_Name (Self);
   begin
      Result := F.New_Subprogram_Declaration
        (F.New_Subprogram_Specification
           (Name       => F.New_Name ("Get_" & Name),
            Parameters => Parameters (Self, Message),
            Result     => Field_Type));

      if Is_Repeated (Self) then
         Item := F.New_Subprogram_Declaration
           (F.New_Subprogram_Specification
              (Name       => F.New_Name (Name & "_Count"),
               Parameters => Get_Self_Parameter (Message),
               Result     => F.New_Name (+"Natural")));

         Result := F.New_List (Result, Item);

         Item := F.New_Subprogram_Declaration
           (F.New_Subprogram_Specification
              (Name       => F.New_Name ("Clear_" & Name),
               Parameters => Get_Self_Parameter (Message, Change => True)));

         Result := F.New_List (Result, Item);

         Item := F.New_Subprogram_Declaration
           (F.New_Subprogram_Specification
              (Name       => F.New_Name ("Append_" & Name),
               Parameters => F.New_List
                 (Get_Self_Parameter (Message, Change => True),
                  F.New_Parameter
                    (F.New_Name (+"Value"), Field_Type))));

         Result := F.New_List (Result, Item);
      end if;

      return Result;
   end Public_Spec;

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

   --------------
   -- Repeated --
   --------------

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
   -- Type_Name --
   ---------------

   function Type_Name
     (Self : Google_Protobuf.FieldDescriptorProto.Instance)
      return League.Strings.Universal_String
   is
      Result : League.Strings.Universal_String;
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
                  Result := Element.Package_Name;
                  Result.Append (".");
                  Result.Append (Element.Type_Name);
               end;
            else
               Ada.Wide_Wide_Text_IO.Put_Line
                 (Ada.Wide_Wide_Text_IO.Standard_Error,
                  "Type not found: " & Value.To_Wide_Wide_String);
            end if;
         end;
      elsif Self.Has_Type_Pb then
         declare
            Value : constant Google_Protobuf.FieldDescriptorProto.TypeX :=
              Self.Get_Type_Pb;
         begin
            Result := Map (Value);
         end;
      else
         Result.Append ("Field_Type");
      end if;

      return Result;
   end Type_Name;

end Compiler.FieldDescriptorProto;
