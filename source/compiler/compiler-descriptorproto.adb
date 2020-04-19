with Compiler.Contexts;
with Compiler.EnumDescriptorProto;
with Compiler.FieldDescriptorProto;
with Compiler.Tools;

with Google_Protobuf.EnumDescriptorProto;

package body Compiler.DescriptorProto is

   use type Ada_Pretty.Node_Access;

   F : Ada_Pretty.Factory renames Compiler.Contexts.F;

   ----------------
   -- Enum_Types --
   ----------------

   function Enum_Types
     (Self : Google_Protobuf.DescriptorProto.Instance)
     return Ada_Pretty.Node_Access
   is
      Result : Ada_Pretty.Node_Access;
      Item   : Ada_Pretty.Node_Access;
      Enum   : Google_Protobuf.EnumDescriptorProto
        .EnumDescriptorProto_Access;
   begin
      for J in 0 .. Self.Enum_Type_Size - 1 loop
         Enum := Self.Get_Enum_Type (J);
         Item := Compiler.EnumDescriptorProto.Public_Spec (Enum.all);

         if Item /= null then
            Result := F.New_List (Result, Item);
         end if;
      end loop;

      for J in 0 .. Self.Nested_Type_Size - 1 loop
         Item := Enum_Types (Self.Get_Nested_Type (J).all);

         if Item /= null then
            Result := F.New_List (Result, Item);
         end if;
      end loop;

      return Result;
   end Enum_Types;

   -----------------------
   -- Populate_Type_Map --
   -----------------------

   procedure Populate_Type_Map
     (Self        : Google_Protobuf.DescriptorProto.Instance;
      PB_Package  : League.Strings.Universal_String;
      Ada_Package : League.Strings.Universal_String)
   is
      use type League.Strings.Universal_String;

      Key  : League.Strings.Universal_String := PB_Package;
      Name : constant League.Strings.Universal_String := Type_Name (Self);
   begin
      Key.Append (".");

      if Self.Has_Name then
         Key.Append (League.Strings.From_UTF_8_String (Self.Get_Name));
      end if;

      Compiler.Contexts.Type_Map.Insert
        (Key, (Package_Name => Ada_Package, Type_Name => Name));

      for J in 0 .. Self.Nested_Type_Size - 1 loop
         Populate_Type_Map
           (Self.Get_Nested_Type (J).all,
            PB_Package => Key,
            Ada_Package => Ada_Package);
      end loop;

      for J in 0 .. Self.Enum_Type_Size - 1 loop
         Compiler.Contexts.Type_Map.Insert
           (Key & "." &
              Compiler.EnumDescriptorProto.Proto_Type_Name
                (Self.Get_Enum_Type (J).all),
            (Package_Name => Ada_Package,
             Type_Name    => Compiler.EnumDescriptorProto.Type_Name
               (Self.Get_Enum_Type (J).all)));
      end loop;
   end Populate_Type_Map;

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
