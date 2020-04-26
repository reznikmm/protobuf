with Compiler.Tools;
with Compiler.Contexts;

with Google_Protobuf.EnumValueDescriptorProto;

package body Compiler.EnumDescriptorProto is

   F : Ada_Pretty.Factory renames Compiler.Contexts.F;

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   function Default
     (Self : Google_Protobuf.EnumDescriptorProto.Instance)
      return League.Strings.Universal_String
   is
      First : Google_Protobuf.EnumValueDescriptorProto.Instance renames
        Self.Get_Value (0).all;
   begin
      return League.Strings.From_UTF_8_String (First.Get_Name);
   end Default;

   ---------------
   -- Max_Value --
   ---------------

   function Max_Value
     (Self : Google_Protobuf.EnumDescriptorProto.Instance) return Integer
   is
      Result : Integer := Integer (Self.Get_Value (0).Get_Number);
      Next   : Integer;
   begin
      for J in 1 .. Self.Value_Size - 1 loop
         Next := Integer (Self.Get_Value (J).Get_Number);
         Result := Integer'Max (Result, Next);
      end loop;

      return Result;
   end Max_Value;

   ---------------
   -- Min_Value --
   ---------------

   function Min_Value
     (Self : Google_Protobuf.EnumDescriptorProto.Instance)
      return Integer
   is
      Result : Integer := Integer (Self.Get_Value (0).Get_Number);
      Next   : Integer;
   begin
      for J in 1 .. Self.Value_Size - 1 loop
         Next := Integer (Self.Get_Value (J).Get_Number);
         Result := Integer'Min (Result, Next);
      end loop;

      return Result;
   end Min_Value;

   -----------------
   -- Public_Spec --
   -----------------

   function Public_Spec
     (Self : Google_Protobuf.EnumDescriptorProto.Instance)
      return not null Ada_Pretty.Node_Access
   is
      use type League.Strings.Universal_String;

      Name   : constant League.Strings.Universal_String := Type_Name (Self);
      Result : Ada_Pretty.Node_Access;
      Clause : Ada_Pretty.Node_Access;
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

            Clause := F.New_List
              (Clause,
               F.New_Argument_Association
                 (Choice => F.New_Name (Name),
                  Value  => F.New_Literal
                    (Natural (Self.Get_Value (J).Get_Number))));

         end;
      end loop;

      Clause := F.New_Statement
        (F.New_Apply
          (F.New_Name ("for " & Name & " use"),
           Clause));

      Result := F.New_Type
        (Name       => F.New_Name (Name),
         Definition => F.New_Parentheses (Result));

      Item := F.New_Package_Instantiation
        (Name        => F.New_Name (Name & "_Vectors"),
         Template    => F.New_Selected_Name (+"PB_Support.Vectors"),
           Actual_Part => F.New_Name (Name));

      Result := F.New_List ((Result, Clause, Item));

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
