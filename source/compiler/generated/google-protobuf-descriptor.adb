with Ada.Unchecked_Deallocation;
with PB_Support.IO;
with PB_Support.Internal;

package body Google.Protobuf.Descriptor is

   package Descriptor_Proto_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Descriptor_Proto,
        Google.Protobuf.Descriptor.Descriptor_Proto_Vector,
        Google.Protobuf.Descriptor.Append);

   package Extension_Range_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Extension_Range,
        Google.Protobuf.Descriptor.Extension_Range_Vector,
        Google.Protobuf.Descriptor.Append);

   package Reserved_Range_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Reserved_Range,
        Google.Protobuf.Descriptor.Reserved_Range_Vector,
        Google.Protobuf.Descriptor.Append);

   package Enum_Descriptor_Proto_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Enum_Descriptor_Proto,
        Google.Protobuf.Descriptor.Enum_Descriptor_Proto_Vector,
        Google.Protobuf.Descriptor.Append);

   package Enum_Options_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Enum_Options,
        Google.Protobuf.Descriptor.Enum_Options_Vector,
        Google.Protobuf.Descriptor.Append);

   package Enum_Value_Descriptor_Proto_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Enum_Value_Descriptor_Proto,
        Google.Protobuf.Descriptor.Enum_Value_Descriptor_Proto_Vector,
        Google.Protobuf.Descriptor.Append);

   package Enum_Value_Options_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Enum_Value_Options,
        Google.Protobuf.Descriptor.Enum_Value_Options_Vector,
        Google.Protobuf.Descriptor.Append);

   package Field_Descriptor_Proto_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Field_Descriptor_Proto,
        Google.Protobuf.Descriptor.Field_Descriptor_Proto_Vector,
        Google.Protobuf.Descriptor.Append);

   type Integer_Label is  range 1 .. 3
     with Size => Google.Protobuf.Descriptor.Label'Size;

   package Label_IO is
     new PB_Support.IO.Enum_IO
       (Google.Protobuf.Descriptor.Label, Integer_Label,
        Google.Protobuf.Descriptor.Label_Vectors);

   type Integer_PB_Type is  range 1 .. 18
     with Size => Google.Protobuf.Descriptor.PB_Type'Size;

   package PB_Type_IO is
     new PB_Support.IO.Enum_IO
       (Google.Protobuf.Descriptor.PB_Type, Integer_PB_Type,
        Google.Protobuf.Descriptor.PB_Type_Vectors);

   package Field_Options_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Field_Options,
        Google.Protobuf.Descriptor.Field_Options_Vector,
        Google.Protobuf.Descriptor.Append);

   type Integer_CType is  range 0 .. 2
     with Size => Google.Protobuf.Descriptor.CType'Size;

   package CType_IO is
     new PB_Support.IO.Enum_IO
       (Google.Protobuf.Descriptor.CType, Integer_CType,
        Google.Protobuf.Descriptor.CType_Vectors);

   type Integer_JSType is  range 0 .. 2
     with Size => Google.Protobuf.Descriptor.JSType'Size;

   package JSType_IO is
     new PB_Support.IO.Enum_IO
       (Google.Protobuf.Descriptor.JSType, Integer_JSType,
        Google.Protobuf.Descriptor.JSType_Vectors);

   package File_Descriptor_Proto_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.File_Descriptor_Proto,
        Google.Protobuf.Descriptor.File_Descriptor_Proto_Vector,
        Google.Protobuf.Descriptor.Append);

   package File_Options_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.File_Options,
        Google.Protobuf.Descriptor.File_Options_Vector,
        Google.Protobuf.Descriptor.Append);

   type Integer_Optimize_Mode is  range 1 .. 3
     with Size => Google.Protobuf.Descriptor.Optimize_Mode'Size;

   package Optimize_Mode_IO is
     new PB_Support.IO.Enum_IO
       (Google.Protobuf.Descriptor.Optimize_Mode, Integer_Optimize_Mode,
        Google.Protobuf.Descriptor.Optimize_Mode_Vectors);

   package Annotation_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Annotation,
        Google.Protobuf.Descriptor.Annotation_Vector,
        Google.Protobuf.Descriptor.Append);

   package Message_Options_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Message_Options,
        Google.Protobuf.Descriptor.Message_Options_Vector,
        Google.Protobuf.Descriptor.Append);

   package Method_Descriptor_Proto_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Method_Descriptor_Proto,
        Google.Protobuf.Descriptor.Method_Descriptor_Proto_Vector,
        Google.Protobuf.Descriptor.Append);

   package Method_Options_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Method_Options,
        Google.Protobuf.Descriptor.Method_Options_Vector,
        Google.Protobuf.Descriptor.Append);

   package Oneof_Descriptor_Proto_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Oneof_Descriptor_Proto,
        Google.Protobuf.Descriptor.Oneof_Descriptor_Proto_Vector,
        Google.Protobuf.Descriptor.Append);

   package Oneof_Options_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Oneof_Options,
        Google.Protobuf.Descriptor.Oneof_Options_Vector,
        Google.Protobuf.Descriptor.Append);

   package Service_Descriptor_Proto_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Service_Descriptor_Proto,
        Google.Protobuf.Descriptor.Service_Descriptor_Proto_Vector,
        Google.Protobuf.Descriptor.Append);

   package Service_Options_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Service_Options,
        Google.Protobuf.Descriptor.Service_Options_Vector,
        Google.Protobuf.Descriptor.Append);

   package Source_Code_Info_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Source_Code_Info,
        Google.Protobuf.Descriptor.Source_Code_Info_Vector,
        Google.Protobuf.Descriptor.Append);

   package Location_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Location,
        Google.Protobuf.Descriptor.Location_Vector,
        Google.Protobuf.Descriptor.Append);

   package Uninterpreted_Option_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Uninterpreted_Option,
        Google.Protobuf.Descriptor.Uninterpreted_Option_Vector,
        Google.Protobuf.Descriptor.Append);

   package Name_Part_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Name_Part,
        Google.Protobuf.Descriptor.Name_Part_Vector,
        Google.Protobuf.Descriptor.Append);

   function Length (Self : File_Descriptor_Set_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   function Get
    (Self  : File_Descriptor_Set_Vector;
     Index : Positive)
      return File_Descriptor_Set is
   begin
      return Self.Data (Index);
   end Get;

   procedure Clear (Self : in out File_Descriptor_Set_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (File_Descriptor_Set_Array, File_Descriptor_Set_Array_Access);

   procedure Append
    (Self : in out File_Descriptor_Set_Vector;
     V    : File_Descriptor_Set) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / File_Descriptor_Set'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new File_Descriptor_Set_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new File_Descriptor_Set_Array'
             (Self.Data.all
                & File_Descriptor_Set_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out File_Descriptor_Set_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new File_Descriptor_Set_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out File_Descriptor_Set_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   procedure Read_File_Descriptor_Set
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out File_Descriptor_Set) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               File_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, V.File);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_File_Descriptor_Set;

   procedure Write_File_Descriptor_Set
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : File_Descriptor_Set) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_File_Descriptor_Set (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         for J in 1 .. V.File.Length loop
            WS.Write_Key ((1, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.File_Descriptor_Proto'Write
              (Stream, V.File.Get (J));
         end loop;
         if WS.End_Message then
            Write_File_Descriptor_Set (WS'Access, V);
         end if;
      end;
   end Write_File_Descriptor_Set;

   function Length (Self : File_Descriptor_Proto_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   function Get
    (Self  : File_Descriptor_Proto_Vector;
     Index : Positive)
      return File_Descriptor_Proto is
   begin
      return Self.Data (Index);
   end Get;

   procedure Clear (Self : in out File_Descriptor_Proto_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (File_Descriptor_Proto_Array, File_Descriptor_Proto_Array_Access);

   procedure Append
    (Self : in out File_Descriptor_Proto_Vector;
     V    : File_Descriptor_Proto) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / File_Descriptor_Proto'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new File_Descriptor_Proto_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new File_Descriptor_Proto_Array'
             (Self.Data.all
                & File_Descriptor_Proto_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out File_Descriptor_Proto_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new File_Descriptor_Proto_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out File_Descriptor_Proto_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   procedure Read_File_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out File_Descriptor_Proto) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, V.Name);
            when 2 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, V.PB_Package);
            when 3 =>
               PB_Support.IO.Read_Universal_String_Vector
                 (Stream, Key.Encoding, V.Dependency);
            when 10 =>
               PB_Support.IO.Read_Unsigned_32_Vector
                 (Stream, Key.Encoding, V.Public_Dependency);
            when 11 =>
               PB_Support.IO.Read_Unsigned_32_Vector
                 (Stream, Key.Encoding, V.Weak_Dependency);
            when 4 =>
               Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, V.Message_Type);
            when 5 =>
               Enum_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, V.Enum_Type);
            when 6 =>
               Service_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, V.Service);
            when 7 =>
               Field_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, V.Extension);
            when 8 =>
               if  not V.Options.Is_Set then
                  V.Options := (True, others => <>);
               end if;
               File_Options_IO.Read (Stream, Key.Encoding, V.Options.Value);
            when 9 =>
               if  not V.Source_Code_Info.Is_Set then
                  V.Source_Code_Info := (True, others => <>);
               end if;
               Source_Code_Info_IO.Read
                 (Stream, Key.Encoding, V.Source_Code_Info.Value);
            when 12 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, V.Syntax);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_File_Descriptor_Proto;

   procedure Write_File_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : File_Descriptor_Proto) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_File_Descriptor_Proto (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write (1, V.Name);
         WS.Write (2, V.PB_Package);
         WS.Write (3, V.Dependency);
         WS.Write_Varint (10, V.Public_Dependency);
         WS.Write_Varint (11, V.Weak_Dependency);
         for J in 1 .. V.Message_Type.Length loop
            WS.Write_Key ((4, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Descriptor_Proto'Write
              (Stream, V.Message_Type.Get (J));
         end loop;
         for J in 1 .. V.Enum_Type.Length loop
            WS.Write_Key ((5, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Enum_Descriptor_Proto'Write
              (Stream, V.Enum_Type.Get (J));
         end loop;
         for J in 1 .. V.Service.Length loop
            WS.Write_Key ((6, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Service_Descriptor_Proto'Write
              (Stream, V.Service.Get (J));
         end loop;
         for J in 1 .. V.Extension.Length loop
            WS.Write_Key ((7, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Field_Descriptor_Proto'Write
              (Stream, V.Extension.Get (J));
         end loop;
         if V.Options.Is_Set then
            WS.Write_Key ((8, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.File_Options'Write
              (Stream, V.Options.Value);
         end if;
         if V.Source_Code_Info.Is_Set then
            WS.Write_Key ((9, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Source_Code_Info'Write
              (Stream, V.Source_Code_Info.Value);
         end if;
         WS.Write (12, V.Syntax);
         if WS.End_Message then
            Write_File_Descriptor_Proto (WS'Access, V);
         end if;
      end;
   end Write_File_Descriptor_Proto;

   function Length (Self : Descriptor_Proto_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   function Get
    (Self  : Descriptor_Proto_Vector;
     Index : Positive)
      return Descriptor_Proto is
   begin
      return Self.Data (Index);
   end Get;

   procedure Clear (Self : in out Descriptor_Proto_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Descriptor_Proto_Array, Descriptor_Proto_Array_Access);

   procedure Append
    (Self : in out Descriptor_Proto_Vector;
     V    : Descriptor_Proto) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Descriptor_Proto'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Descriptor_Proto_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Descriptor_Proto_Array'
             (Self.Data.all & Descriptor_Proto_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Descriptor_Proto_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Descriptor_Proto_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Descriptor_Proto_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   procedure Read_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Descriptor_Proto) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, V.Name);
            when 2 =>
               Field_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, V.Field);
            when 6 =>
               Field_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, V.Extension);
            when 3 =>
               Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, V.Nested_Type);
            when 4 =>
               Enum_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, V.Enum_Type);
            when 5 =>
               Extension_Range_IO.Read_Vector
                 (Stream, Key.Encoding, V.Extension_Range);
            when 8 =>
               Oneof_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, V.Oneof_Decl);
            when 7 =>
               if  not V.Options.Is_Set then
                  V.Options := (True, others => <>);
               end if;
               Message_Options_IO.Read (Stream, Key.Encoding, V.Options.Value);
            when 9 =>
               Reserved_Range_IO.Read_Vector
                 (Stream, Key.Encoding, V.Reserved_Range);
            when 10 =>
               PB_Support.IO.Read_Universal_String_Vector
                 (Stream, Key.Encoding, V.Reserved_Name);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Descriptor_Proto;

   procedure Write_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Descriptor_Proto) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Descriptor_Proto (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write (1, V.Name);
         for J in 1 .. V.Field.Length loop
            WS.Write_Key ((2, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Field_Descriptor_Proto'Write
              (Stream, V.Field.Get (J));
         end loop;
         for J in 1 .. V.Extension.Length loop
            WS.Write_Key ((6, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Field_Descriptor_Proto'Write
              (Stream, V.Extension.Get (J));
         end loop;
         for J in 1 .. V.Nested_Type.Length loop
            WS.Write_Key ((3, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Descriptor_Proto'Write
              (Stream, V.Nested_Type.Get (J));
         end loop;
         for J in 1 .. V.Enum_Type.Length loop
            WS.Write_Key ((4, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Enum_Descriptor_Proto'Write
              (Stream, V.Enum_Type.Get (J));
         end loop;
         for J in 1 .. V.Extension_Range.Length loop
            WS.Write_Key ((5, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Extension_Range'Write
              (Stream, V.Extension_Range.Get (J));
         end loop;
         for J in 1 .. V.Oneof_Decl.Length loop
            WS.Write_Key ((8, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Oneof_Descriptor_Proto'Write
              (Stream, V.Oneof_Decl.Get (J));
         end loop;
         if V.Options.Is_Set then
            WS.Write_Key ((7, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Message_Options'Write
              (Stream, V.Options.Value);
         end if;
         for J in 1 .. V.Reserved_Range.Length loop
            WS.Write_Key ((9, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Reserved_Range'Write
              (Stream, V.Reserved_Range.Get (J));
         end loop;
         WS.Write (10, V.Reserved_Name);
         if WS.End_Message then
            Write_Descriptor_Proto (WS'Access, V);
         end if;
      end;
   end Write_Descriptor_Proto;

   function Length (Self : Extension_Range_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   function Get
    (Self  : Extension_Range_Vector;
     Index : Positive)
      return Extension_Range is
   begin
      return Self.Data (Index);
   end Get;

   procedure Clear (Self : in out Extension_Range_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Extension_Range_Array, Extension_Range_Array_Access);

   procedure Append
    (Self : in out Extension_Range_Vector;
     V    : Extension_Range) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Extension_Range'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Extension_Range_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Extension_Range_Array'
             (Self.Data.all & Extension_Range_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Extension_Range_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Extension_Range_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Extension_Range_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   procedure Read_Extension_Range
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Extension_Range) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Unsigned_32 (Stream, Key.Encoding, V.Start);
            when 2 =>
               PB_Support.IO.Read_Unsigned_32 (Stream, Key.Encoding, V.PB_End);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Extension_Range;

   procedure Write_Extension_Range
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Extension_Range) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Extension_Range (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Varint (1, V.Start);
         WS.Write_Varint (2, V.PB_End);
         if WS.End_Message then
            Write_Extension_Range (WS'Access, V);
         end if;
      end;
   end Write_Extension_Range;

   function Length (Self : Reserved_Range_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   function Get
    (Self  : Reserved_Range_Vector;
     Index : Positive)
      return Reserved_Range is
   begin
      return Self.Data (Index);
   end Get;

   procedure Clear (Self : in out Reserved_Range_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Reserved_Range_Array, Reserved_Range_Array_Access);

   procedure Append
    (Self : in out Reserved_Range_Vector;
     V    : Reserved_Range) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Reserved_Range'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Reserved_Range_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Reserved_Range_Array'
             (Self.Data.all & Reserved_Range_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Reserved_Range_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Reserved_Range_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Reserved_Range_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   procedure Read_Reserved_Range
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Reserved_Range) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Unsigned_32 (Stream, Key.Encoding, V.Start);
            when 2 =>
               PB_Support.IO.Read_Unsigned_32 (Stream, Key.Encoding, V.PB_End);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Reserved_Range;

   procedure Write_Reserved_Range
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Reserved_Range) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Reserved_Range (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Varint (1, V.Start);
         WS.Write_Varint (2, V.PB_End);
         if WS.End_Message then
            Write_Reserved_Range (WS'Access, V);
         end if;
      end;
   end Write_Reserved_Range;

   function Length (Self : Field_Descriptor_Proto_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   function Get
    (Self  : Field_Descriptor_Proto_Vector;
     Index : Positive)
      return Field_Descriptor_Proto is
   begin
      return Self.Data (Index);
   end Get;

   procedure Clear (Self : in out Field_Descriptor_Proto_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Field_Descriptor_Proto_Array, Field_Descriptor_Proto_Array_Access);

   procedure Append
    (Self : in out Field_Descriptor_Proto_Vector;
     V    : Field_Descriptor_Proto) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Field_Descriptor_Proto'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Field_Descriptor_Proto_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Field_Descriptor_Proto_Array'
             (Self.Data.all
                & Field_Descriptor_Proto_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Field_Descriptor_Proto_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Field_Descriptor_Proto_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Field_Descriptor_Proto_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   procedure Read_Field_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Field_Descriptor_Proto) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, V.Name);
            when 3 =>
               PB_Support.IO.Read_Unsigned_32 (Stream, Key.Encoding, V.Number);
            when 4 =>
               Label_IO.Read (Stream, Key.Encoding, V.Label);
            when 5 =>
               PB_Type_IO.Read (Stream, Key.Encoding, V.PB_Type);
            when 6 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, V.Type_Name);
            when 2 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, V.Extendee);
            when 7 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, V.Default_Value);
            when 9 =>
               PB_Support.IO.Read_Unsigned_32
                 (Stream, Key.Encoding, V.Oneof_Index);
            when 10 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, V.Json_Name);
            when 8 =>
               if  not V.Options.Is_Set then
                  V.Options := (True, others => <>);
               end if;
               Field_Options_IO.Read (Stream, Key.Encoding, V.Options.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Field_Descriptor_Proto;

   procedure Write_Field_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Field_Descriptor_Proto) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Field_Descriptor_Proto (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write (1, V.Name);
         WS.Write_Varint (3, V.Number);
         Label_IO.Write (WS, 4, V.Label);
         PB_Type_IO.Write (WS, 5, V.PB_Type);
         WS.Write (6, V.Type_Name);
         WS.Write (2, V.Extendee);
         WS.Write (7, V.Default_Value);
         WS.Write_Varint (9, V.Oneof_Index);
         WS.Write (10, V.Json_Name);
         if V.Options.Is_Set then
            WS.Write_Key ((8, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Field_Options'Write
              (Stream, V.Options.Value);
         end if;
         if WS.End_Message then
            Write_Field_Descriptor_Proto (WS'Access, V);
         end if;
      end;
   end Write_Field_Descriptor_Proto;

   function Length (Self : Oneof_Descriptor_Proto_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   function Get
    (Self  : Oneof_Descriptor_Proto_Vector;
     Index : Positive)
      return Oneof_Descriptor_Proto is
   begin
      return Self.Data (Index);
   end Get;

   procedure Clear (Self : in out Oneof_Descriptor_Proto_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Oneof_Descriptor_Proto_Array, Oneof_Descriptor_Proto_Array_Access);

   procedure Append
    (Self : in out Oneof_Descriptor_Proto_Vector;
     V    : Oneof_Descriptor_Proto) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Oneof_Descriptor_Proto'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Oneof_Descriptor_Proto_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Oneof_Descriptor_Proto_Array'
             (Self.Data.all
                & Oneof_Descriptor_Proto_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Oneof_Descriptor_Proto_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Oneof_Descriptor_Proto_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Oneof_Descriptor_Proto_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   procedure Read_Oneof_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Oneof_Descriptor_Proto) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, V.Name);
            when 2 =>
               if  not V.Options.Is_Set then
                  V.Options := (True, others => <>);
               end if;
               Oneof_Options_IO.Read (Stream, Key.Encoding, V.Options.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Oneof_Descriptor_Proto;

   procedure Write_Oneof_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Oneof_Descriptor_Proto) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Oneof_Descriptor_Proto (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write (1, V.Name);
         if V.Options.Is_Set then
            WS.Write_Key ((2, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Oneof_Options'Write
              (Stream, V.Options.Value);
         end if;
         if WS.End_Message then
            Write_Oneof_Descriptor_Proto (WS'Access, V);
         end if;
      end;
   end Write_Oneof_Descriptor_Proto;

   function Length (Self : Enum_Descriptor_Proto_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   function Get
    (Self  : Enum_Descriptor_Proto_Vector;
     Index : Positive)
      return Enum_Descriptor_Proto is
   begin
      return Self.Data (Index);
   end Get;

   procedure Clear (Self : in out Enum_Descriptor_Proto_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Enum_Descriptor_Proto_Array, Enum_Descriptor_Proto_Array_Access);

   procedure Append
    (Self : in out Enum_Descriptor_Proto_Vector;
     V    : Enum_Descriptor_Proto) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Enum_Descriptor_Proto'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Enum_Descriptor_Proto_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Enum_Descriptor_Proto_Array'
             (Self.Data.all
                & Enum_Descriptor_Proto_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Enum_Descriptor_Proto_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Enum_Descriptor_Proto_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Enum_Descriptor_Proto_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   procedure Read_Enum_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Enum_Descriptor_Proto) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, V.Name);
            when 2 =>
               Enum_Value_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, V.Value);
            when 3 =>
               if  not V.Options.Is_Set then
                  V.Options := (True, others => <>);
               end if;
               Enum_Options_IO.Read (Stream, Key.Encoding, V.Options.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Enum_Descriptor_Proto;

   procedure Write_Enum_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Enum_Descriptor_Proto) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Enum_Descriptor_Proto (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write (1, V.Name);
         for J in 1 .. V.Value.Length loop
            WS.Write_Key ((2, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Enum_Value_Descriptor_Proto'Write
              (Stream, V.Value.Get (J));
         end loop;
         if V.Options.Is_Set then
            WS.Write_Key ((3, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Enum_Options'Write
              (Stream, V.Options.Value);
         end if;
         if WS.End_Message then
            Write_Enum_Descriptor_Proto (WS'Access, V);
         end if;
      end;
   end Write_Enum_Descriptor_Proto;

   function Length
    (Self : Enum_Value_Descriptor_Proto_Vector)
      return Natural is
   begin
      return Self.Length;
   end Length;

   function Get
    (Self  : Enum_Value_Descriptor_Proto_Vector;
     Index : Positive)
      return Enum_Value_Descriptor_Proto is
   begin
      return Self.Data (Index);
   end Get;

   procedure Clear (Self : in out Enum_Value_Descriptor_Proto_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Enum_Value_Descriptor_Proto_Array,
      Enum_Value_Descriptor_Proto_Array_Access);

   procedure Append
    (Self : in out Enum_Value_Descriptor_Proto_Vector;
     V    : Enum_Value_Descriptor_Proto) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Enum_Value_Descriptor_Proto'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=
            new Enum_Value_Descriptor_Proto_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Enum_Value_Descriptor_Proto_Array'
             (Self.Data.all
                & Enum_Value_Descriptor_Proto_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Enum_Value_Descriptor_Proto_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Enum_Value_Descriptor_Proto_Array'
             (Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Enum_Value_Descriptor_Proto_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   procedure Read_Enum_Value_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Enum_Value_Descriptor_Proto) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, V.Name);
            when 2 =>
               PB_Support.IO.Read_Unsigned_32 (Stream, Key.Encoding, V.Number);
            when 3 =>
               if  not V.Options.Is_Set then
                  V.Options := (True, others => <>);
               end if;
               Enum_Value_Options_IO.Read
                 (Stream, Key.Encoding, V.Options.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Enum_Value_Descriptor_Proto;

   procedure Write_Enum_Value_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Enum_Value_Descriptor_Proto) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Enum_Value_Descriptor_Proto (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write (1, V.Name);
         WS.Write_Varint (2, V.Number);
         if V.Options.Is_Set then
            WS.Write_Key ((3, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Enum_Value_Options'Write
              (Stream, V.Options.Value);
         end if;
         if WS.End_Message then
            Write_Enum_Value_Descriptor_Proto (WS'Access, V);
         end if;
      end;
   end Write_Enum_Value_Descriptor_Proto;

   function Length (Self : Service_Descriptor_Proto_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   function Get
    (Self  : Service_Descriptor_Proto_Vector;
     Index : Positive)
      return Service_Descriptor_Proto is
   begin
      return Self.Data (Index);
   end Get;

   procedure Clear (Self : in out Service_Descriptor_Proto_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Service_Descriptor_Proto_Array, Service_Descriptor_Proto_Array_Access);

   procedure Append
    (Self : in out Service_Descriptor_Proto_Vector;
     V    : Service_Descriptor_Proto) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Service_Descriptor_Proto'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Service_Descriptor_Proto_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Service_Descriptor_Proto_Array'
             (Self.Data.all
                & Service_Descriptor_Proto_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Service_Descriptor_Proto_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Service_Descriptor_Proto_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Service_Descriptor_Proto_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   procedure Read_Service_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Service_Descriptor_Proto) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, V.Name);
            when 2 =>
               Method_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, V.Method);
            when 3 =>
               if  not V.Options.Is_Set then
                  V.Options := (True, others => <>);
               end if;
               Service_Options_IO.Read (Stream, Key.Encoding, V.Options.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Service_Descriptor_Proto;

   procedure Write_Service_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Service_Descriptor_Proto) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Service_Descriptor_Proto (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write (1, V.Name);
         for J in 1 .. V.Method.Length loop
            WS.Write_Key ((2, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Method_Descriptor_Proto'Write
              (Stream, V.Method.Get (J));
         end loop;
         if V.Options.Is_Set then
            WS.Write_Key ((3, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Service_Options'Write
              (Stream, V.Options.Value);
         end if;
         if WS.End_Message then
            Write_Service_Descriptor_Proto (WS'Access, V);
         end if;
      end;
   end Write_Service_Descriptor_Proto;

   function Length (Self : Method_Descriptor_Proto_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   function Get
    (Self  : Method_Descriptor_Proto_Vector;
     Index : Positive)
      return Method_Descriptor_Proto is
   begin
      return Self.Data (Index);
   end Get;

   procedure Clear (Self : in out Method_Descriptor_Proto_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Method_Descriptor_Proto_Array, Method_Descriptor_Proto_Array_Access);

   procedure Append
    (Self : in out Method_Descriptor_Proto_Vector;
     V    : Method_Descriptor_Proto) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Method_Descriptor_Proto'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Method_Descriptor_Proto_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Method_Descriptor_Proto_Array'
             (Self.Data.all
                & Method_Descriptor_Proto_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Method_Descriptor_Proto_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Method_Descriptor_Proto_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Method_Descriptor_Proto_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   procedure Read_Method_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Method_Descriptor_Proto) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, V.Name);
            when 2 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, V.Input_Type);
            when 3 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, V.Output_Type);
            when 4 =>
               if  not V.Options.Is_Set then
                  V.Options := (True, others => <>);
               end if;
               Method_Options_IO.Read (Stream, Key.Encoding, V.Options.Value);
            when 5 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, V.Client_Streaming);
            when 6 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, V.Server_Streaming);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Method_Descriptor_Proto;

   procedure Write_Method_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Method_Descriptor_Proto) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Method_Descriptor_Proto (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write (1, V.Name);
         WS.Write (2, V.Input_Type);
         WS.Write (3, V.Output_Type);
         if V.Options.Is_Set then
            WS.Write_Key ((4, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Method_Options'Write
              (Stream, V.Options.Value);
         end if;
         WS.Write (5, V.Client_Streaming);
         WS.Write (6, V.Server_Streaming);
         if WS.End_Message then
            Write_Method_Descriptor_Proto (WS'Access, V);
         end if;
      end;
   end Write_Method_Descriptor_Proto;

   function Length (Self : File_Options_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   function Get
    (Self  : File_Options_Vector;
     Index : Positive)
      return File_Options is
   begin
      return Self.Data (Index);
   end Get;

   procedure Clear (Self : in out File_Options_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (File_Options_Array, File_Options_Array_Access);

   procedure Append (Self : in out File_Options_Vector; V    : File_Options) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / File_Options'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new File_Options_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new File_Options_Array'
             (Self.Data.all & File_Options_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out File_Options_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new File_Options_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out File_Options_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   procedure Read_File_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out File_Options) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, V.Java_Package);
            when 8 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, V.Java_Outer_Classname);
            when 10 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, V.Java_Multiple_Files);
            when 20 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, V.Java_Generate_Equals_And_Hash);
            when 27 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, V.Java_String_Check_Utf_8);
            when 9 =>
               Optimize_Mode_IO.Read (Stream, Key.Encoding, V.Optimize_For);
            when 11 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, V.Go_Package);
            when 16 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, V.Cc_Generic_Services);
            when 17 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, V.Java_Generic_Services);
            when 18 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, V.Py_Generic_Services);
            when 23 =>
               PB_Support.IO.Read_Boolean (Stream, Key.Encoding, V.Deprecated);
            when 31 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, V.Cc_Enable_Arenas);
            when 36 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, V.Objc_Class_Prefix);
            when 37 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, V.Csharp_Namespace);
            when 999 =>
               Uninterpreted_Option_IO.Read_Vector
                 (Stream, Key.Encoding, V.Uninterpreted_Option);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_File_Options;

   procedure Write_File_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : File_Options) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_File_Options (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write (1, V.Java_Package);
         WS.Write (8, V.Java_Outer_Classname);
         WS.Write (10, V.Java_Multiple_Files);
         WS.Write (20, V.Java_Generate_Equals_And_Hash);
         WS.Write (27, V.Java_String_Check_Utf_8);
         Optimize_Mode_IO.Write (WS, 9, V.Optimize_For);
         WS.Write (11, V.Go_Package);
         WS.Write (16, V.Cc_Generic_Services);
         WS.Write (17, V.Java_Generic_Services);
         WS.Write (18, V.Py_Generic_Services);
         WS.Write (23, V.Deprecated);
         WS.Write (31, V.Cc_Enable_Arenas);
         WS.Write (36, V.Objc_Class_Prefix);
         WS.Write (37, V.Csharp_Namespace);
         for J in 1 .. V.Uninterpreted_Option.Length loop
            WS.Write_Key ((999, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Uninterpreted_Option'Write
              (Stream, V.Uninterpreted_Option.Get (J));
         end loop;
         if WS.End_Message then
            Write_File_Options (WS'Access, V);
         end if;
      end;
   end Write_File_Options;

   function Length (Self : Message_Options_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   function Get
    (Self  : Message_Options_Vector;
     Index : Positive)
      return Message_Options is
   begin
      return Self.Data (Index);
   end Get;

   procedure Clear (Self : in out Message_Options_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Message_Options_Array, Message_Options_Array_Access);

   procedure Append
    (Self : in out Message_Options_Vector;
     V    : Message_Options) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Message_Options'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Message_Options_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Message_Options_Array'
             (Self.Data.all & Message_Options_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Message_Options_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Message_Options_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Message_Options_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   procedure Read_Message_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Message_Options) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, V.Message_Set_Wire_Format);
            when 2 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, V.No_Standard_Descriptor_Accessor);
            when 3 =>
               PB_Support.IO.Read_Boolean (Stream, Key.Encoding, V.Deprecated);
            when 7 =>
               PB_Support.IO.Read_Boolean (Stream, Key.Encoding, V.Map_Entry);
            when 999 =>
               Uninterpreted_Option_IO.Read_Vector
                 (Stream, Key.Encoding, V.Uninterpreted_Option);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Message_Options;

   procedure Write_Message_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Message_Options) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Message_Options (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write (1, V.Message_Set_Wire_Format);
         WS.Write (2, V.No_Standard_Descriptor_Accessor);
         WS.Write (3, V.Deprecated);
         WS.Write (7, V.Map_Entry);
         for J in 1 .. V.Uninterpreted_Option.Length loop
            WS.Write_Key ((999, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Uninterpreted_Option'Write
              (Stream, V.Uninterpreted_Option.Get (J));
         end loop;
         if WS.End_Message then
            Write_Message_Options (WS'Access, V);
         end if;
      end;
   end Write_Message_Options;

   function Length (Self : Field_Options_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   function Get
    (Self  : Field_Options_Vector;
     Index : Positive)
      return Field_Options is
   begin
      return Self.Data (Index);
   end Get;

   procedure Clear (Self : in out Field_Options_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Field_Options_Array, Field_Options_Array_Access);

   procedure Append
    (Self : in out Field_Options_Vector;
     V    : Field_Options) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Field_Options'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Field_Options_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Field_Options_Array'
             (Self.Data.all & Field_Options_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Field_Options_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Field_Options_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Field_Options_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   procedure Read_Field_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Field_Options) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               CType_IO.Read (Stream, Key.Encoding, V.Ctype);
            when 2 =>
               PB_Support.IO.Read_Boolean (Stream, Key.Encoding, V.Packed);
            when 6 =>
               JSType_IO.Read (Stream, Key.Encoding, V.Jstype);
            when 5 =>
               PB_Support.IO.Read_Boolean (Stream, Key.Encoding, V.Lazy);
            when 3 =>
               PB_Support.IO.Read_Boolean (Stream, Key.Encoding, V.Deprecated);
            when 10 =>
               PB_Support.IO.Read_Boolean (Stream, Key.Encoding, V.Weak);
            when 999 =>
               Uninterpreted_Option_IO.Read_Vector
                 (Stream, Key.Encoding, V.Uninterpreted_Option);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Field_Options;

   procedure Write_Field_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Field_Options) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Field_Options (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         CType_IO.Write (WS, 1, V.Ctype);
         WS.Write (2, V.Packed);
         JSType_IO.Write (WS, 6, V.Jstype);
         WS.Write (5, V.Lazy);
         WS.Write (3, V.Deprecated);
         WS.Write (10, V.Weak);
         for J in 1 .. V.Uninterpreted_Option.Length loop
            WS.Write_Key ((999, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Uninterpreted_Option'Write
              (Stream, V.Uninterpreted_Option.Get (J));
         end loop;
         if WS.End_Message then
            Write_Field_Options (WS'Access, V);
         end if;
      end;
   end Write_Field_Options;

   function Length (Self : Oneof_Options_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   function Get
    (Self  : Oneof_Options_Vector;
     Index : Positive)
      return Oneof_Options is
   begin
      return Self.Data (Index);
   end Get;

   procedure Clear (Self : in out Oneof_Options_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Oneof_Options_Array, Oneof_Options_Array_Access);

   procedure Append
    (Self : in out Oneof_Options_Vector;
     V    : Oneof_Options) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Oneof_Options'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Oneof_Options_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Oneof_Options_Array'
             (Self.Data.all & Oneof_Options_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Oneof_Options_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Oneof_Options_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Oneof_Options_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   procedure Read_Oneof_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Oneof_Options) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 999 =>
               Uninterpreted_Option_IO.Read_Vector
                 (Stream, Key.Encoding, V.Uninterpreted_Option);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Oneof_Options;

   procedure Write_Oneof_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Oneof_Options) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Oneof_Options (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         for J in 1 .. V.Uninterpreted_Option.Length loop
            WS.Write_Key ((999, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Uninterpreted_Option'Write
              (Stream, V.Uninterpreted_Option.Get (J));
         end loop;
         if WS.End_Message then
            Write_Oneof_Options (WS'Access, V);
         end if;
      end;
   end Write_Oneof_Options;

   function Length (Self : Enum_Options_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   function Get
    (Self  : Enum_Options_Vector;
     Index : Positive)
      return Enum_Options is
   begin
      return Self.Data (Index);
   end Get;

   procedure Clear (Self : in out Enum_Options_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Enum_Options_Array, Enum_Options_Array_Access);

   procedure Append (Self : in out Enum_Options_Vector; V    : Enum_Options) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Enum_Options'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Enum_Options_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Enum_Options_Array'
             (Self.Data.all & Enum_Options_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Enum_Options_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Enum_Options_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Enum_Options_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   procedure Read_Enum_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Enum_Options) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 2 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, V.Allow_Alias);
            when 3 =>
               PB_Support.IO.Read_Boolean (Stream, Key.Encoding, V.Deprecated);
            when 999 =>
               Uninterpreted_Option_IO.Read_Vector
                 (Stream, Key.Encoding, V.Uninterpreted_Option);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Enum_Options;

   procedure Write_Enum_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Enum_Options) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Enum_Options (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write (2, V.Allow_Alias);
         WS.Write (3, V.Deprecated);
         for J in 1 .. V.Uninterpreted_Option.Length loop
            WS.Write_Key ((999, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Uninterpreted_Option'Write
              (Stream, V.Uninterpreted_Option.Get (J));
         end loop;
         if WS.End_Message then
            Write_Enum_Options (WS'Access, V);
         end if;
      end;
   end Write_Enum_Options;

   function Length (Self : Enum_Value_Options_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   function Get
    (Self  : Enum_Value_Options_Vector;
     Index : Positive)
      return Enum_Value_Options is
   begin
      return Self.Data (Index);
   end Get;

   procedure Clear (Self : in out Enum_Value_Options_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Enum_Value_Options_Array, Enum_Value_Options_Array_Access);

   procedure Append
    (Self : in out Enum_Value_Options_Vector;
     V    : Enum_Value_Options) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Enum_Value_Options'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Enum_Value_Options_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Enum_Value_Options_Array'
             (Self.Data.all
                & Enum_Value_Options_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Enum_Value_Options_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Enum_Value_Options_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Enum_Value_Options_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   procedure Read_Enum_Value_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Enum_Value_Options) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Boolean (Stream, Key.Encoding, V.Deprecated);
            when 999 =>
               Uninterpreted_Option_IO.Read_Vector
                 (Stream, Key.Encoding, V.Uninterpreted_Option);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Enum_Value_Options;

   procedure Write_Enum_Value_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Enum_Value_Options) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Enum_Value_Options (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write (1, V.Deprecated);
         for J in 1 .. V.Uninterpreted_Option.Length loop
            WS.Write_Key ((999, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Uninterpreted_Option'Write
              (Stream, V.Uninterpreted_Option.Get (J));
         end loop;
         if WS.End_Message then
            Write_Enum_Value_Options (WS'Access, V);
         end if;
      end;
   end Write_Enum_Value_Options;

   function Length (Self : Service_Options_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   function Get
    (Self  : Service_Options_Vector;
     Index : Positive)
      return Service_Options is
   begin
      return Self.Data (Index);
   end Get;

   procedure Clear (Self : in out Service_Options_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Service_Options_Array, Service_Options_Array_Access);

   procedure Append
    (Self : in out Service_Options_Vector;
     V    : Service_Options) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Service_Options'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Service_Options_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Service_Options_Array'
             (Self.Data.all & Service_Options_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Service_Options_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Service_Options_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Service_Options_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   procedure Read_Service_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Service_Options) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 33 =>
               PB_Support.IO.Read_Boolean (Stream, Key.Encoding, V.Deprecated);
            when 999 =>
               Uninterpreted_Option_IO.Read_Vector
                 (Stream, Key.Encoding, V.Uninterpreted_Option);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Service_Options;

   procedure Write_Service_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Service_Options) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Service_Options (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write (33, V.Deprecated);
         for J in 1 .. V.Uninterpreted_Option.Length loop
            WS.Write_Key ((999, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Uninterpreted_Option'Write
              (Stream, V.Uninterpreted_Option.Get (J));
         end loop;
         if WS.End_Message then
            Write_Service_Options (WS'Access, V);
         end if;
      end;
   end Write_Service_Options;

   function Length (Self : Method_Options_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   function Get
    (Self  : Method_Options_Vector;
     Index : Positive)
      return Method_Options is
   begin
      return Self.Data (Index);
   end Get;

   procedure Clear (Self : in out Method_Options_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Method_Options_Array, Method_Options_Array_Access);

   procedure Append
    (Self : in out Method_Options_Vector;
     V    : Method_Options) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Method_Options'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Method_Options_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Method_Options_Array'
             (Self.Data.all & Method_Options_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Method_Options_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Method_Options_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Method_Options_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   procedure Read_Method_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Method_Options) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 33 =>
               PB_Support.IO.Read_Boolean (Stream, Key.Encoding, V.Deprecated);
            when 999 =>
               Uninterpreted_Option_IO.Read_Vector
                 (Stream, Key.Encoding, V.Uninterpreted_Option);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Method_Options;

   procedure Write_Method_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Method_Options) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Method_Options (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write (33, V.Deprecated);
         for J in 1 .. V.Uninterpreted_Option.Length loop
            WS.Write_Key ((999, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Uninterpreted_Option'Write
              (Stream, V.Uninterpreted_Option.Get (J));
         end loop;
         if WS.End_Message then
            Write_Method_Options (WS'Access, V);
         end if;
      end;
   end Write_Method_Options;

   function Length (Self : Uninterpreted_Option_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   function Get
    (Self  : Uninterpreted_Option_Vector;
     Index : Positive)
      return Uninterpreted_Option is
   begin
      return Self.Data (Index);
   end Get;

   procedure Clear (Self : in out Uninterpreted_Option_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Uninterpreted_Option_Array, Uninterpreted_Option_Array_Access);

   procedure Append
    (Self : in out Uninterpreted_Option_Vector;
     V    : Uninterpreted_Option) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Uninterpreted_Option'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Uninterpreted_Option_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Uninterpreted_Option_Array'
             (Self.Data.all
                & Uninterpreted_Option_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Uninterpreted_Option_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Uninterpreted_Option_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Uninterpreted_Option_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   procedure Read_Uninterpreted_Option
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Uninterpreted_Option) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 2 =>
               Name_Part_IO.Read_Vector (Stream, Key.Encoding, V.Name);
            when 3 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, V.Identifier_Value);
            when 4 =>
               PB_Support.IO.Read_Unsigned_64
                 (Stream, Key.Encoding, V.Positive_Int_Value);
            when 5 =>
               PB_Support.IO.Read_Integer_64
                 (Stream, Key.Encoding, V.Negative_Int_Value);
            when 6 =>
               PB_Support.IO.Read_IEEE_Float_64
                 (Stream, Key.Encoding, V.Double_Value);
            when 7 =>
               PB_Support.IO.Read_Stream_Element_Vector
                 (Stream, Key.Encoding, V.String_Value);
            when 8 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, V.Aggregate_Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Uninterpreted_Option;

   procedure Write_Uninterpreted_Option
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Uninterpreted_Option) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Uninterpreted_Option (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         for J in 1 .. V.Name.Length loop
            WS.Write_Key ((2, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Name_Part'Write
              (Stream, V.Name.Get (J));
         end loop;
         WS.Write (3, V.Identifier_Value);
         WS.Write_Varint (4, V.Positive_Int_Value);
         WS.Write_Varint (5, V.Negative_Int_Value);
         WS.Write (6, V.Double_Value);
         WS.Write (7, V.String_Value);
         WS.Write (8, V.Aggregate_Value);
         if WS.End_Message then
            Write_Uninterpreted_Option (WS'Access, V);
         end if;
      end;
   end Write_Uninterpreted_Option;

   function Length (Self : Name_Part_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   function Get
    (Self  : Name_Part_Vector;
     Index : Positive)
      return Name_Part is
   begin
      return Self.Data (Index);
   end Get;

   procedure Clear (Self : in out Name_Part_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Name_Part_Array, Name_Part_Array_Access);

   procedure Append (Self : in out Name_Part_Vector; V    : Name_Part) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Name_Part'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Name_Part_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Name_Part_Array'
             (Self.Data.all & Name_Part_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Name_Part_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Name_Part_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Name_Part_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   procedure Read_Name_Part
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Name_Part) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, V.Name_Part);
            when 2 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, V.Is_Extension);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Name_Part;

   procedure Write_Name_Part
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Name_Part) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Name_Part (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write (1, V.Name_Part);
         WS.Write (2, V.Is_Extension);
         if WS.End_Message then
            Write_Name_Part (WS'Access, V);
         end if;
      end;
   end Write_Name_Part;

   function Length (Self : Source_Code_Info_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   function Get
    (Self  : Source_Code_Info_Vector;
     Index : Positive)
      return Source_Code_Info is
   begin
      return Self.Data (Index);
   end Get;

   procedure Clear (Self : in out Source_Code_Info_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Source_Code_Info_Array, Source_Code_Info_Array_Access);

   procedure Append
    (Self : in out Source_Code_Info_Vector;
     V    : Source_Code_Info) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Source_Code_Info'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Source_Code_Info_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Source_Code_Info_Array'
             (Self.Data.all & Source_Code_Info_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Source_Code_Info_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Source_Code_Info_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Source_Code_Info_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   procedure Read_Source_Code_Info
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Source_Code_Info) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               Location_IO.Read_Vector (Stream, Key.Encoding, V.Location);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Source_Code_Info;

   procedure Write_Source_Code_Info
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Source_Code_Info) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Source_Code_Info (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         for J in 1 .. V.Location.Length loop
            WS.Write_Key ((1, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Location'Write
              (Stream, V.Location.Get (J));
         end loop;
         if WS.End_Message then
            Write_Source_Code_Info (WS'Access, V);
         end if;
      end;
   end Write_Source_Code_Info;

   function Length (Self : Location_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   function Get (Self  : Location_Vector; Index : Positive) return Location is
   begin
      return Self.Data (Index);
   end Get;

   procedure Clear (Self : in out Location_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Location_Array, Location_Array_Access);

   procedure Append (Self : in out Location_Vector; V    : Location) is
      Init_Length : constant Positive := Positive'Max (1, 256 / Location'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Location_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Location_Array'
             (Self.Data.all & Location_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Location_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Location_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Location_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   procedure Read_Location
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Location) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Unsigned_32_Vector
                 (Stream, Key.Encoding, V.Path);
            when 2 =>
               PB_Support.IO.Read_Unsigned_32_Vector
                 (Stream, Key.Encoding, V.Span);
            when 3 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, V.Leading_Comments);
            when 4 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, V.Trailing_Comments);
            when 6 =>
               PB_Support.IO.Read_Universal_String_Vector
                 (Stream, Key.Encoding, V.Leading_Detached_Comments);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Location;

   procedure Write_Location
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Location) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Location (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Varint (1, V.Path);
         WS.Write_Varint (2, V.Span);
         WS.Write (3, V.Leading_Comments);
         WS.Write (4, V.Trailing_Comments);
         WS.Write (6, V.Leading_Detached_Comments);
         if WS.End_Message then
            Write_Location (WS'Access, V);
         end if;
      end;
   end Write_Location;

   function Length (Self : Generated_Code_Info_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   function Get
    (Self  : Generated_Code_Info_Vector;
     Index : Positive)
      return Generated_Code_Info is
   begin
      return Self.Data (Index);
   end Get;

   procedure Clear (Self : in out Generated_Code_Info_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Generated_Code_Info_Array, Generated_Code_Info_Array_Access);

   procedure Append
    (Self : in out Generated_Code_Info_Vector;
     V    : Generated_Code_Info) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Generated_Code_Info'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Generated_Code_Info_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Generated_Code_Info_Array'
             (Self.Data.all
                & Generated_Code_Info_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Generated_Code_Info_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Generated_Code_Info_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Generated_Code_Info_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   procedure Read_Generated_Code_Info
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Generated_Code_Info) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               Annotation_IO.Read_Vector (Stream, Key.Encoding, V.Annotation);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Generated_Code_Info;

   procedure Write_Generated_Code_Info
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Generated_Code_Info) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Generated_Code_Info (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         for J in 1 .. V.Annotation.Length loop
            WS.Write_Key ((1, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Annotation'Write
              (Stream, V.Annotation.Get (J));
         end loop;
         if WS.End_Message then
            Write_Generated_Code_Info (WS'Access, V);
         end if;
      end;
   end Write_Generated_Code_Info;

   function Length (Self : Annotation_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   function Get
    (Self  : Annotation_Vector;
     Index : Positive)
      return Annotation is
   begin
      return Self.Data (Index);
   end Get;

   procedure Clear (Self : in out Annotation_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Annotation_Array, Annotation_Array_Access);

   procedure Append (Self : in out Annotation_Vector; V    : Annotation) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Annotation'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Annotation_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Annotation_Array'
             (Self.Data.all & Annotation_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Annotation_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Annotation_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Annotation_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   procedure Read_Annotation
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Annotation) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Unsigned_32_Vector
                 (Stream, Key.Encoding, V.Path);
            when 2 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, V.Source_File);
            when 3 =>
               PB_Support.IO.Read_Unsigned_32
                 (Stream, Key.Encoding, V.PB_Begin);
            when 4 =>
               PB_Support.IO.Read_Unsigned_32 (Stream, Key.Encoding, V.PB_End);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Annotation;

   procedure Write_Annotation
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Annotation) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Annotation (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Varint (1, V.Path);
         WS.Write (2, V.Source_File);
         WS.Write_Varint (3, V.PB_Begin);
         WS.Write_Varint (4, V.PB_End);
         if WS.End_Message then
            Write_Annotation (WS'Access, V);
         end if;
      end;
   end Write_Annotation;

end Google.Protobuf.Descriptor;