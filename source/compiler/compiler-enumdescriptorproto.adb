with Compiler.Tools;
with Compiler.Contexts;

with Google_Protobuf.EnumValueDescriptorProto;

package body Compiler.EnumDescriptorProto is

   F : Ada_Pretty.Factory renames Compiler.Contexts.F;

   -----------------
   -- Public_Spec --
   -----------------

   function Public_Spec
     (Self : Google_Protobuf.EnumDescriptorProto.Instance)
      return Ada_Pretty.Node_Access
   is
      Result : Ada_Pretty.Node_Access;
      Item   : Ada_Pretty.Node_Access;
   begin
      for J in 0 .. Self.Value_Size - 1 loop
         declare
            Name : League.Strings.Universal_String;
            Next : Google_Protobuf.EnumValueDescriptorProto.Instance renames
              Self.Get_Value (J).all;
         begin
            Name := League.Strings.From_UTF_8_String (Next.Get_Name);
            Item := F.New_Argument_Association (F.New_Name (Name));
            Result := F.New_List (Result, Item);
         end;
      end loop;

      Result := F.New_Type
        (Name       => F.New_Name (Type_Name (Self)),
         Definition => F.New_Parentheses (Result));

      return Result;
   end Public_Spec;

   ---------------------
   -- Proto_Type_Name --
   ---------------------

   function Proto_Type_Name
     (Self : Google_Protobuf.EnumDescriptorProto.Instance)
      return League.Strings.Universal_String
   is
      Result : League.Strings.Universal_String;
   begin
      if Self.Has_Name then
         Result :=
           League.Strings.From_UTF_8_String (Self.Get_Name);
      end if;

      return Result;
   end Proto_Type_Name;

   ---------------
   -- Type_Name --
   ---------------

   function Type_Name
     (Self : Google_Protobuf.EnumDescriptorProto.Instance)
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
         Result.Append ("Enum");
      end if;

      return Result;
   end Type_Name;

end Compiler.EnumDescriptorProto;
