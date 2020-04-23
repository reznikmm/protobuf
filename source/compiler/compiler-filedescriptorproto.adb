with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Doubly_Linked_Lists;

with System.Address_To_Access_Conversions;
with System.Storage_Elements;

with League.String_Vectors;

with Ada_Pretty;

with Google_Protobuf.DescriptorProto;
with Google_Protobuf.EnumDescriptorProto;
with Google_Protobuf.FieldDescriptorProto;

with Compiler.Contexts;
with Compiler.DescriptorProto;
with Compiler.EnumDescriptorProto;
with Compiler.FieldDescriptorProto;
with Compiler.Tools;

package body Compiler.FileDescriptorProto is

   use type Ada_Pretty.Node_Access;

   F : Ada_Pretty.Factory renames Compiler.Contexts.F;

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   function Less
     (Left, Right : Google_Protobuf.DescriptorProto.DescriptorProto_Access)
      return Boolean;

   package Message_Sets is new Ada.Containers.Ordered_Sets
     (Google_Protobuf.DescriptorProto.DescriptorProto_Access,
      "<" => Less,
      "=" => Google_Protobuf.DescriptorProto."=");

   package Message_List is new Ada.Containers.Doubly_Linked_Lists
     (Google_Protobuf.DescriptorProto.DescriptorProto_Access,
      "=" => Google_Protobuf.DescriptorProto."=");

   type Message_Order is record
      List        : Message_List.List;
      Done        : Message_Sets.Set;
      In_Progress : Message_Sets.Set;
   end record;

   Messages : Message_Order;

   function Dependency
     (Self : Google_Protobuf.FileDescriptorProto.Instance)
      return Ada_Pretty.Node_Access;
   --  return 'with Unit; use Unit;' for each dependency

   procedure Compute_Message_Order
     (Self : Google_Protobuf.FileDescriptorProto.Instance);

   procedure Add_Message_To_Order
     (Self         : Google_Protobuf.DescriptorProto.DescriptorProto_Access;
      Package_Name : League.Strings.Universal_String);

   --------------------------
   -- Add_Message_To_Order --
   --------------------------

   procedure Add_Message_To_Order
     (Self : Google_Protobuf.DescriptorProto.DescriptorProto_Access;
      Package_Name : League.Strings.Universal_String)
   is
      use type League.Strings.Universal_String;
      use type Google_Protobuf.DescriptorProto.DescriptorProto_Access;

      Msg  : League.Strings.Universal_String;
      Next : Compiler.Contexts.Ada_Type_Info;
   begin
      if Messages.Done.Contains (Self) then
         return;
      elsif Messages.In_Progress.Contains (Self) then
         raise Constraint_Error with "Circular depencency";
      end if;

      Messages.In_Progress.Insert (Self);

      for J in 0 .. Self.Field_Size - 1 loop
         declare
            Item : constant Google_Protobuf.FieldDescriptorProto
              .FieldDescriptorProto_Access := Self.Get_Field (J);
         begin
            Msg := Compiler.FieldDescriptorProto.PB_Type_Name (Item.all);

            if Compiler.Contexts.Type_Map.Contains (Msg) then
               Next := Compiler.Contexts.Type_Map (Msg);

               if Next.Package_Name = Package_Name
                 and then Next.Message /= Self
                 and then Next.Message /= null
               then
                  Add_Message_To_Order (Next.Message, Package_Name);
               end if;
            end if;
         end;
      end loop;

      Messages.In_Progress.Delete (Self);
      Messages.Done.Insert (Self);
      Messages.List.Append (Self);

      for J in 0 .. Self.Nested_Type_Size - 1 loop
         declare
            Item : constant Google_Protobuf.DescriptorProto
              .DescriptorProto_Access := Self.Get_Nested_Type (J);
         begin
            Add_Message_To_Order (Item, Package_Name);
         end;
      end loop;
   end Add_Message_To_Order;

   ---------------------------
   -- Compute_Message_Order --
   ---------------------------

   procedure Compute_Message_Order
     (Self : Google_Protobuf.FileDescriptorProto.Instance)
   is
      Item : Google_Protobuf.DescriptorProto.DescriptorProto_Access;
      Name : constant League.Strings.Universal_String := Package_Name (Self);
   begin
      for J in 0 .. Self.Message_Type_Size - 1 loop
         Item := Self.Get_Message_Type (J);
         Add_Message_To_Order (Item, Name);
      end loop;
   end Compute_Message_Order;

   ----------------
   -- Dependency --
   ----------------

   function Dependency
     (Self : Google_Protobuf.FileDescriptorProto.Instance)
      return Ada_Pretty.Node_Access
   is
      use type League.Strings.Universal_String;

      Set    : Compiler.Contexts.String_Sets.Set;
      File   : Google_Protobuf.FileDescriptorProto.FileDescriptorProto_Access;
      Item   : Google_Protobuf.DescriptorProto.DescriptorProto_Access;
      Result : Ada_Pretty.Node_Access;
      Unit_Name   : Ada_Pretty.Node_Access;
      With_Clause : Ada_Pretty.Node_Access;
      My_Name     : constant League.Strings.Universal_String :=
        Package_Name (Self);
   begin
      Set.Include (+"PB_Support.Vectors");

      for J in 0 .. Self.Dependency_Size - 1 loop
         File := Compiler.Contexts.Find_FileDescriptorProto
           (Self.Get_Dependency (J));

         Set.Include (Package_Name (File.all));
      end loop;

      for J in 0 .. Self.Message_Type_Size - 1 loop
         Item := Self.Get_Message_Type (J);
         Set.Union (Compiler.DescriptorProto.Dependency (Item.all));
      end loop;

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
         Enum   : Google_Protobuf.EnumDescriptorProto
           .EnumDescriptorProto_Access;
      begin
         Compute_Message_Order (Self);

         for J in 0 .. Self.Enum_Type_Size - 1 loop
            Enum := Self.Get_Enum_Type (J);
            Next := Compiler.EnumDescriptorProto.Public_Spec (Enum.all);

            if Next /= null then
               Result := F.New_List (Result, Next);
            end if;
         end loop;

         for J in 0 .. Self.Message_Type_Size - 1 loop
            Item := Self.Get_Message_Type (J);
            Next := Compiler.DescriptorProto.Enum_Types (Item.all);

            if Next /= null then
               Result := F.New_List (Result, Next);
            end if;
         end loop;

         for Item of Messages.List loop
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

   ----------
   -- Less --
   ----------

   function Less
     (Left, Right : Google_Protobuf.DescriptorProto.DescriptorProto_Access)
      return Boolean
   is
      use type System.Storage_Elements.Integer_Address;

      package Conv is new System.Address_To_Access_Conversions
        (Object => Google_Protobuf.DescriptorProto.Instance);

      L_Addr : constant System.Address :=
        Conv.To_Address (Conv.Object_Pointer (Left));

      R_Addr : constant System.Address :=
        Conv.To_Address (Conv.Object_Pointer (Right));

      L_Int : constant System.Storage_Elements.Integer_Address :=
        System.Storage_Elements.To_Integer (L_Addr);

      R_Int : constant System.Storage_Elements.Integer_Address :=
        System.Storage_Elements.To_Integer (R_Addr);

   begin
      return L_Int < R_Int;
   end Less;
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
           (Item, PB_Package => PB_Name, Ada_Package => Ada_Name);
      end loop;
   end Populate_Type_Map;

end Compiler.FileDescriptorProto;
