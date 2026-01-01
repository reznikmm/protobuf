with Ada.Unchecked_Deallocation;
with Proto_Support.IO;
with Proto_Support.Internal;

package body Google.Protobuf.Compiler.Plugin is

   package Google_Protobuf_Descriptor_File_Descriptor_Proto_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.File_Descriptor_Proto,
        Google.Protobuf.Descriptor.File_Descriptor_Proto_Vector,
        Google.Protobuf.Descriptor.Append);

   package Google_Protobuf_Descriptor_Generated_Code_Info_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Generated_Code_Info,
        Google.Protobuf.Descriptor.Generated_Code_Info_Vector,
        Google.Protobuf.Descriptor.Append);

   package Google_Protobuf_Compiler_Plugin_File_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Compiler.Plugin.File,
        Google.Protobuf.Compiler.Plugin.File_Vector,
        Google.Protobuf.Compiler.Plugin.Append);

   package Google_Protobuf_Compiler_Plugin_Version_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Compiler.Plugin.Version,
        Google.Protobuf.Compiler.Plugin.Version_Vector,
        Google.Protobuf.Compiler.Plugin.Append);

   function Length (Self : Version_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Version_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Version_Array, Version_Array_Access);

   procedure Append (Self : in out Version_Vector; V    : Version) is
      Init_Length : constant Positive := Positive'Max (1, 256 / Version'Size);
      Aux_Data    : Version_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Version_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Version_Array'
             (Self.Data.all & Version_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Version_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Version_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Version_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Version_Variable_Reference
    (Self  : aliased in out Version_Vector;
     Index : Positive)
      return Version_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Version_Variable_Reference;

   not overriding function Get_Version_Constant_Reference
    (Self  : aliased Version_Vector;
     Index : Positive)
      return Version_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Version_Constant_Reference;

   procedure Read_Version
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Version) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Major.Is_Set then
                  V.Major := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Major.Value);
            when 2 =>
               if  not V.Minor.Is_Set then
                  V.Minor := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Minor.Value);
            when 3 =>
               if  not V.Patch.Is_Set then
                  V.Patch := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Patch.Value);
            when 4 =>
               if  not V.Suffix.Is_Set then
                  V.Suffix := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Suffix.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Version;

   procedure Write_Version
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Version) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Version (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Major.Is_Set then
            WS.Write_Varint (1, V.Major.Value);
         end if;
         if V.Minor.Is_Set then
            WS.Write_Varint (2, V.Minor.Value);
         end if;
         if V.Patch.Is_Set then
            WS.Write_Varint (3, V.Patch.Value);
         end if;
         if V.Suffix.Is_Set then
            WS.Write (4, V.Suffix.Value);
         end if;
         if WS.End_Message then
            Write_Version (WS'Access, V);
         end if;
      end;
   end Write_Version;

   function Length (Self : Code_Generator_Request_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Code_Generator_Request_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Code_Generator_Request_Array, Code_Generator_Request_Array_Access);

   procedure Append
    (Self : in out Code_Generator_Request_Vector;
     V    : Code_Generator_Request) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Code_Generator_Request'Size);
      Aux_Data    : Code_Generator_Request_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Code_Generator_Request_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Code_Generator_Request_Array'
             (Self.Data.all
                & Code_Generator_Request_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Code_Generator_Request_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Code_Generator_Request_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Code_Generator_Request_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Code_Generator_Request_Variable_Reference
    (Self  : aliased in out Code_Generator_Request_Vector;
     Index : Positive)
      return Code_Generator_Request_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Code_Generator_Request_Variable_Reference;

   not overriding function Get_Code_Generator_Request_Constant_Reference
    (Self  : aliased Code_Generator_Request_Vector;
     Index : Positive)
      return Code_Generator_Request_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Code_Generator_Request_Constant_Reference;

   procedure Read_Code_Generator_Request
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Code_Generator_Request) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               Proto_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.File_To_Generate);
            when 2 =>
               if  not V.Parameter.Is_Set then
                  V.Parameter := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Parameter.Value);
            when 15 =>
               Google_Protobuf_Descriptor_File_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, V.Proto_File);
            when 17 =>
               Google_Protobuf_Descriptor_File_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, V.Source_File_Descriptors);
            when 3 =>
               if  not V.Compiler_Version.Is_Set then
                  V.Compiler_Version := (True, others => <>);
               end if;
               Google_Protobuf_Compiler_Plugin_Version_IO.Read
                 (Stream, Key.Encoding, V.Compiler_Version.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Code_Generator_Request;

   procedure Write_Code_Generator_Request
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Code_Generator_Request) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Code_Generator_Request (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write (1, V.File_To_Generate);
         if V.Parameter.Is_Set then
            WS.Write (2, V.Parameter.Value);
         end if;
         for J in 1 .. V.Proto_File.Length loop
            WS.Write_Key ((15, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.File_Descriptor_Proto'Write
              (Stream, V.Proto_File (J));
         end loop;
         for J in 1 .. V.Source_File_Descriptors.Length loop
            WS.Write_Key ((17, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.File_Descriptor_Proto'Write
              (Stream, V.Source_File_Descriptors (J));
         end loop;
         if V.Compiler_Version.Is_Set then
            WS.Write_Key ((3, Proto_Support.Length_Delimited));
            Google.Protobuf.Compiler.Plugin.Version'Write
              (Stream, V.Compiler_Version.Value);
         end if;
         if WS.End_Message then
            Write_Code_Generator_Request (WS'Access, V);
         end if;
      end;
   end Write_Code_Generator_Request;

   function Length (Self : Code_Generator_Response_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Code_Generator_Response_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Code_Generator_Response_Array, Code_Generator_Response_Array_Access);

   procedure Append
    (Self : in out Code_Generator_Response_Vector;
     V    : Code_Generator_Response) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Code_Generator_Response'Size);
      Aux_Data    : Code_Generator_Response_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Code_Generator_Response_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Code_Generator_Response_Array'
             (Self.Data.all
                & Code_Generator_Response_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Code_Generator_Response_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Code_Generator_Response_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Code_Generator_Response_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Code_Generator_Response_Variable_Reference
    (Self  : aliased in out Code_Generator_Response_Vector;
     Index : Positive)
      return Code_Generator_Response_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Code_Generator_Response_Variable_Reference;

   not overriding function Get_Code_Generator_Response_Constant_Reference
    (Self  : aliased Code_Generator_Response_Vector;
     Index : Positive)
      return Code_Generator_Response_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Code_Generator_Response_Constant_Reference;

   procedure Read_Code_Generator_Response
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Code_Generator_Response) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Error.Is_Set then
                  V.Error := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Error.Value);
            when 2 =>
               if  not V.Supported_Features.Is_Set then
                  V.Supported_Features := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Supported_Features.Value);
            when 3 =>
               if  not V.Minimum_Edition.Is_Set then
                  V.Minimum_Edition := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Minimum_Edition.Value);
            when 4 =>
               if  not V.Maximum_Edition.Is_Set then
                  V.Maximum_Edition := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Maximum_Edition.Value);
            when 15 =>
               Google_Protobuf_Compiler_Plugin_File_IO.Read_Vector
                 (Stream, Key.Encoding, V.File);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Code_Generator_Response;

   procedure Write_Code_Generator_Response
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Code_Generator_Response) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Code_Generator_Response (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Error.Is_Set then
            WS.Write (1, V.Error.Value);
         end if;
         if V.Supported_Features.Is_Set then
            WS.Write_Varint (2, V.Supported_Features.Value);
         end if;
         if V.Minimum_Edition.Is_Set then
            WS.Write_Varint (3, V.Minimum_Edition.Value);
         end if;
         if V.Maximum_Edition.Is_Set then
            WS.Write_Varint (4, V.Maximum_Edition.Value);
         end if;
         for J in 1 .. V.File.Length loop
            WS.Write_Key ((15, Proto_Support.Length_Delimited));
            Google.Protobuf.Compiler.Plugin.File'Write (Stream, V.File (J));
         end loop;
         if WS.End_Message then
            Write_Code_Generator_Response (WS'Access, V);
         end if;
      end;
   end Write_Code_Generator_Response;

   function Length (Self : File_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out File_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (File_Array, File_Array_Access);

   procedure Append (Self : in out File_Vector; V    : File) is
      Init_Length : constant Positive := Positive'Max (1, 256 / File'Size);
      Aux_Data    : File_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new File_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new File_Array'
             (Self.Data.all & File_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out File_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new File_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out File_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_File_Variable_Reference
    (Self  : aliased in out File_Vector;
     Index : Positive)
      return File_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_File_Variable_Reference;

   not overriding function Get_File_Constant_Reference
    (Self  : aliased File_Vector;
     Index : Positive)
      return File_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_File_Constant_Reference;

   procedure Read_File
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out File) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Name.Is_Set then
                  V.Name := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Name.Value);
            when 2 =>
               if  not V.Insertion_Point.Is_Set then
                  V.Insertion_Point := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Insertion_Point.Value);
            when 15 =>
               if  not V.Content.Is_Set then
                  V.Content := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Content.Value);
            when 16 =>
               if  not V.Generated_Code_Info.Is_Set then
                  V.Generated_Code_Info := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Generated_Code_Info_IO.Read
                 (Stream, Key.Encoding, V.Generated_Code_Info.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_File;

   procedure Write_File
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : File) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_File (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Name.Is_Set then
            WS.Write (1, V.Name.Value);
         end if;
         if V.Insertion_Point.Is_Set then
            WS.Write (2, V.Insertion_Point.Value);
         end if;
         if V.Content.Is_Set then
            WS.Write (15, V.Content.Value);
         end if;
         if V.Generated_Code_Info.Is_Set then
            WS.Write_Key ((16, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Generated_Code_Info'Write
              (Stream, V.Generated_Code_Info.Value);
         end if;
         if WS.End_Message then
            Write_File (WS'Access, V);
         end if;
      end;
   end Write_File;

end Google.Protobuf.Compiler.Plugin;