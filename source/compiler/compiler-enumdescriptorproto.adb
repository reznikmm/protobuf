with Compiler.Tools;

package body Compiler.EnumDescriptorProto is

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
