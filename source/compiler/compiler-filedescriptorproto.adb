with Ada.Characters.Wide_Wide_Latin_1;

with League.String_Vectors;

with Ada_Pretty;

with Google_Protobuf.DescriptorProto;

with Compiler.Contexts;
with Compiler.DescriptorProto;
with Compiler.Tools;

package body Compiler.FileDescriptorProto is

   F : Ada_Pretty.Factory renames Compiler.Contexts.F;

   function Dependency
     (Self : Google_Protobuf.FileDescriptorProto.Instance)
      return Ada_Pretty.Node_Access;
   --  return 'with Unit; use Unit;' for each dependency

   ----------------
   -- Dependency --
   ----------------

   function Dependency
     (Self : Google_Protobuf.FileDescriptorProto.Instance)
      return Ada_Pretty.Node_Access
   is
      Item   : Google_Protobuf.FileDescriptorProto.FileDescriptorProto_Access;
      Result : Ada_Pretty.Node_Access;
      Unit_Name   : Ada_Pretty.Node_Access;
      With_Clause : Ada_Pretty.Node_Access;
--        Use_Clause  : Ada_Pretty.Node_Access;
   begin
      for J in 0 .. Self.Dependency_Size - 1 loop
         Item := Compiler.Contexts.Find_FileDescriptorProto
           (Self.Get_Dependency (J));
         Unit_Name := F.New_Selected_Name (Package_Name (Item.all));
         With_Clause := F.New_With (Unit_Name);
         Result := F.New_List (Result, With_Clause);
--           Use_Clause := F.New_Use (Unit_Name);
--           Result := F.New_List (Result, Use_Clause);
      end loop;

      return Result;
   end Dependency;

   ---------------
   -- File_Name --
   ---------------

   function File_Name
     (Self : Google_Protobuf.FileDescriptorProto.Instance)
      return League.Strings.Universal_String
   is
      Result : League.Strings.Universal_String;
   begin
      if Self.Has_Package_Pb then
         declare
            Value : constant League.Strings.Universal_String :=
              League.Strings.From_UTF_8_String (Self.Get_Package_Pb);
            List  : constant League.String_Vectors.Universal_String_Vector :=
              Value.Split ('.');
         begin
            Result := List.Join ('-');
            Result.Append (".ads");
         end;
      else
         Result.Append ("output.ads");
      end if;

      return Result;
   end File_Name;

   ---------------
   -- File_Text --
   ---------------

   function File_Text
     (Self : Google_Protobuf.FileDescriptorProto.Instance)
      return League.Strings.Universal_String
   is

      function Get_Public return Ada_Pretty.Node_Access;
      function Get_Private return Ada_Pretty.Node_Access;

      -----------------
      -- Get_Private --
      -----------------

      function Get_Private return Ada_Pretty.Node_Access is
         Result : Ada_Pretty.Node_Access;
         Next   : Ada_Pretty.Node_Access;
         Item   : Google_Protobuf.DescriptorProto.DescriptorProto_Access;
      begin
         for J in 0 .. Self.Message_Type_Size - 1 loop
            Item := Self.Get_Message_Type (J);
            Next := Compiler.DescriptorProto.Private_Spec (Item.all);
            Result := F.New_List (Result, Next);
         end loop;

         return Result;
      end Get_Private;

      ----------------
      -- Get_Public --
      ----------------

      function Get_Public return Ada_Pretty.Node_Access is
         Result : Ada_Pretty.Node_Access;
         Next   : Ada_Pretty.Node_Access;
         Item   : Google_Protobuf.DescriptorProto.DescriptorProto_Access;
      begin
         for J in 0 .. Self.Message_Type_Size - 1 loop
            Item := Self.Get_Message_Type (J);
            Next := Compiler.DescriptorProto.Public_Spec (Item.all);
            Result := F.New_List (Result, Next);
         end loop;

         return Result;
      end Get_Public;

      Result : League.Strings.Universal_String;

      Name   : constant Ada_Pretty.Node_Access :=
        F.New_Selected_Name (Package_Name (Self));

      Clauses : constant Ada_Pretty.Node_Access :=
        Dependency (Self);

      Public : constant Ada_Pretty.Node_Access := Get_Public;
      Root   : constant Ada_Pretty.Node_Access :=
        F.New_Package (Name, Public, Get_Private);
      Unit   : constant Ada_Pretty.Node_Access :=
        F.New_Compilation_Unit (Root, Clauses);
   begin
      Result := F.To_Text (Unit).Join (Ada.Characters.Wide_Wide_Latin_1.LF);
      return Result;
   end File_Text;

   ------------------
   -- Package_Name --
   ------------------

   function Package_Name
     (Self : Google_Protobuf.FileDescriptorProto.Instance)
      return League.Strings.Universal_String
   is
      Result : League.Strings.Universal_String;
   begin
      if Self.Has_Package_Pb then
         declare
            Value : constant League.Strings.Universal_String :=
              League.Strings.From_UTF_8_String (Self.Get_Package_Pb);
         begin
            Result := Compiler.Tools.To_Selected_Ada_Name (Value);
         end;
      else
         Result.Append ("Output");
      end if;

      return Result;
   end Package_Name;

   -----------------------
   -- Populate_Type_Map --
   -----------------------

   procedure Populate_Type_Map
     (Self : Google_Protobuf.FileDescriptorProto.Instance)
   is
      Item     : Google_Protobuf.DescriptorProto.DescriptorProto_Access;
      PB_Name  : League.Strings.Universal_String;
      Ada_Name : constant League.Strings.Universal_String :=
        Package_Name (Self);
   begin
      PB_Name.Append (".");

      if Self.Has_Package_Pb then
         PB_Name.Append
           (League.Strings.From_UTF_8_String (Self.Get_Package_Pb));
      end if;

      for J in 0 .. Self.Message_Type_Size - 1 loop
         Item := Self.Get_Message_Type (J);
         Compiler.DescriptorProto.Populate_Type_Map
           (Item.all, PB_Package => PB_Name, Ada_Package => Ada_Name);
      end loop;
   end Populate_Type_Map;

end Compiler.FileDescriptorProto;
