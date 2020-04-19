with Compiler.Contexts;
with Compiler.FieldDescriptorProto;
with Compiler.Tools;

package body Compiler.DescriptorProto is

   F : Ada_Pretty.Factory renames Compiler.Contexts.F;

   -----------------
   -- Public_Spec --
   -----------------

   function Public_Spec
     (Self : Google_Protobuf.DescriptorProto.Instance)
      return Ada_Pretty.Node_Access
   is
      Result : Ada_Pretty.Node_Access;
      Item   : Ada_Pretty.Node_Access;
   begin
      for J in 0 .. Self.Nested_Type_Size - 1 loop
         Item := Public_Spec (Self.Get_Nested_Type (J).all);
         Result := F.New_List (Result, Item);
      end loop;

      Item := F.New_Type
        (F.New_Name (Type_Name (Self)),
         Definition => F.New_Private_Record);
      Result := F.New_List (Result, Item);

      for J in 0 .. Self.Field_Size - 1 loop
         Item := Compiler.FieldDescriptorProto.Public_Spec
           (Self.Get_Field (J).all, Self);

         Result := F.New_List (Result, Item);
      end loop;

      return Result;
   end Public_Spec;

   ------------------
   -- Private_Spec --
   ------------------

   function Private_Spec
     (Self : Google_Protobuf.DescriptorProto.Instance)
      return Ada_Pretty.Node_Access
   is
      Result : Ada_Pretty.Node_Access;
      Item   : Ada_Pretty.Node_Access;
   begin
      for J in 0 .. Self.Nested_Type_Size - 1 loop
         Item := Private_Spec (Self.Get_Nested_Type (J).all);
         Result := F.New_List (Result, Item);
      end loop;

      Item := F.New_Type
        (F.New_Name (Type_Name (Self)),
         Definition => F.New_Record);
      Result := F.New_List (Result, Item);

--        for J in 0 .. Self.Field_Size - 1 loop
--           Item := Compiler.FieldDescriptorProto.Public_Spec
--             (Self.Get_Field (J).all, Self);
--
--           Result := F.New_List (Result, Item);
--        end loop;

      return Result;
   end Private_Spec;

   ---------------
   -- Type_Name --
   ---------------

   function Type_Name
     (Self : Google_Protobuf.DescriptorProto.Instance)
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
   end Type_Name;

end Compiler.DescriptorProto;
