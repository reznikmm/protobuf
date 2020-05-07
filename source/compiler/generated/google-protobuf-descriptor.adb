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
    (Self  : in out File_Descriptor_Set_Vector;
     Value : File_Descriptor_Set) is
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
      Self.Data (Self.Length) := Value;
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
     Value  : out File_Descriptor_Set) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               File_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, Value.File);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_File_Descriptor_Set;

   procedure Write_File_Descriptor_Set
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : File_Descriptor_Set) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_File_Descriptor_Set (WS'Access, Value);
            return;
         end;
      end if;
      declare
         WS     : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
         Offset : constant Ada.Streams.Stream_Element_Count := WS.Written;
      begin
         WS.Start_Message (Value'Address);
         for J in 1 .. Value.File.Length loop
            WS.Write_Key ((1, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.File_Descriptor_Proto'Write
              (Stream, Value.File.Get (J));
         end loop;
         if WS.End_Message (Value'Address, Offset) then
            Write_File_Descriptor_Set (WS'Access, Value);
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
    (Self  : in out File_Descriptor_Proto_Vector;
     Value : File_Descriptor_Proto) is
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
      Self.Data (Self.Length) := Value;
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
     Value  : out File_Descriptor_Proto) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, Value.Name);
            when 2 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, Value.PB_Package);
            when 3 =>
               PB_Support.IO.Read_Universal_String_Vector
                 (Stream, Key.Encoding, Value.Dependency);
            when 10 =>
               PB_Support.IO.Read_Unsigned_32_Vector
                 (Stream, Key.Encoding, Value.Public_Dependency);
            when 11 =>
               PB_Support.IO.Read_Unsigned_32_Vector
                 (Stream, Key.Encoding, Value.Weak_Dependency);
            when 4 =>
               Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, Value.Message_Type);
            when 5 =>
               Enum_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, Value.Enum_Type);
            when 6 =>
               Service_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, Value.Service);
            when 7 =>
               Field_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, Value.Extension);
            when 8 =>
               if  not Value.Options.Is_Set then
                  Value.Options := (True, others => <>);
               end if;
               File_Options_IO.Read
                 (Stream, Key.Encoding, Value.Options.Value);
            when 9 =>
               if  not Value.Source_Code_Info.Is_Set then
                  Value.Source_Code_Info := (True, others => <>);
               end if;
               Source_Code_Info_IO.Read
                 (Stream, Key.Encoding, Value.Source_Code_Info.Value);
            when 12 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, Value.Syntax);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_File_Descriptor_Proto;

   procedure Write_File_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : File_Descriptor_Proto) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_File_Descriptor_Proto (WS'Access, Value);
            return;
         end;
      end if;
      declare
         WS     : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
         Offset : constant Ada.Streams.Stream_Element_Count := WS.Written;
      begin
         WS.Start_Message (Value'Address);
         WS.Write (1, Value.Name);
         WS.Write (2, Value.PB_Package);
         WS.Write (3, Value.Dependency);
         WS.Write_Varint (10, Value.Public_Dependency);
         WS.Write_Varint (11, Value.Weak_Dependency);
         for J in 1 .. Value.Message_Type.Length loop
            WS.Write_Key ((4, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Descriptor_Proto'Write
              (Stream, Value.Message_Type.Get (J));
         end loop;
         for J in 1 .. Value.Enum_Type.Length loop
            WS.Write_Key ((5, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Enum_Descriptor_Proto'Write
              (Stream, Value.Enum_Type.Get (J));
         end loop;
         for J in 1 .. Value.Service.Length loop
            WS.Write_Key ((6, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Service_Descriptor_Proto'Write
              (Stream, Value.Service.Get (J));
         end loop;
         for J in 1 .. Value.Extension.Length loop
            WS.Write_Key ((7, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Field_Descriptor_Proto'Write
              (Stream, Value.Extension.Get (J));
         end loop;
         if Value.Options.Is_Set then
            WS.Write_Key ((8, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.File_Options'Write
              (Stream, Value.Options.Value);
         end if;
         if Value.Source_Code_Info.Is_Set then
            WS.Write_Key ((9, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Source_Code_Info'Write
              (Stream, Value.Source_Code_Info.Value);
         end if;
         WS.Write (12, Value.Syntax);
         if WS.End_Message (Value'Address, Offset) then
            Write_File_Descriptor_Proto (WS'Access, Value);
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
    (Self  : in out Descriptor_Proto_Vector;
     Value : Descriptor_Proto) is
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
      Self.Data (Self.Length) := Value;
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
     Value  : out Descriptor_Proto) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, Value.Name);
            when 2 =>
               Field_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, Value.Field);
            when 6 =>
               Field_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, Value.Extension);
            when 3 =>
               Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, Value.Nested_Type);
            when 4 =>
               Enum_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, Value.Enum_Type);
            when 5 =>
               Extension_Range_IO.Read_Vector
                 (Stream, Key.Encoding, Value.Extension_Range);
            when 8 =>
               Oneof_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, Value.Oneof_Decl);
            when 7 =>
               if  not Value.Options.Is_Set then
                  Value.Options := (True, others => <>);
               end if;
               Message_Options_IO.Read
                 (Stream, Key.Encoding, Value.Options.Value);
            when 9 =>
               Reserved_Range_IO.Read_Vector
                 (Stream, Key.Encoding, Value.Reserved_Range);
            when 10 =>
               PB_Support.IO.Read_Universal_String_Vector
                 (Stream, Key.Encoding, Value.Reserved_Name);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Descriptor_Proto;

   procedure Write_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Descriptor_Proto) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Descriptor_Proto (WS'Access, Value);
            return;
         end;
      end if;
      declare
         WS     : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
         Offset : constant Ada.Streams.Stream_Element_Count := WS.Written;
      begin
         WS.Start_Message (Value'Address);
         WS.Write (1, Value.Name);
         for J in 1 .. Value.Field.Length loop
            WS.Write_Key ((2, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Field_Descriptor_Proto'Write
              (Stream, Value.Field.Get (J));
         end loop;
         for J in 1 .. Value.Extension.Length loop
            WS.Write_Key ((6, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Field_Descriptor_Proto'Write
              (Stream, Value.Extension.Get (J));
         end loop;
         for J in 1 .. Value.Nested_Type.Length loop
            WS.Write_Key ((3, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Descriptor_Proto'Write
              (Stream, Value.Nested_Type.Get (J));
         end loop;
         for J in 1 .. Value.Enum_Type.Length loop
            WS.Write_Key ((4, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Enum_Descriptor_Proto'Write
              (Stream, Value.Enum_Type.Get (J));
         end loop;
         for J in 1 .. Value.Extension_Range.Length loop
            WS.Write_Key ((5, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Extension_Range'Write
              (Stream, Value.Extension_Range.Get (J));
         end loop;
         for J in 1 .. Value.Oneof_Decl.Length loop
            WS.Write_Key ((8, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Oneof_Descriptor_Proto'Write
              (Stream, Value.Oneof_Decl.Get (J));
         end loop;
         if Value.Options.Is_Set then
            WS.Write_Key ((7, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Message_Options'Write
              (Stream, Value.Options.Value);
         end if;
         for J in 1 .. Value.Reserved_Range.Length loop
            WS.Write_Key ((9, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Reserved_Range'Write
              (Stream, Value.Reserved_Range.Get (J));
         end loop;
         WS.Write (10, Value.Reserved_Name);
         if WS.End_Message (Value'Address, Offset) then
            Write_Descriptor_Proto (WS'Access, Value);
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
    (Self  : in out Extension_Range_Vector;
     Value : Extension_Range) is
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
      Self.Data (Self.Length) := Value;
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
     Value  : out Extension_Range) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Unsigned_32
                 (Stream, Key.Encoding, Value.Start);
            when 2 =>
               PB_Support.IO.Read_Unsigned_32
                 (Stream, Key.Encoding, Value.PB_End);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Extension_Range;

   procedure Write_Extension_Range
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Extension_Range) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Extension_Range (WS'Access, Value);
            return;
         end;
      end if;
      declare
         WS     : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
         Offset : constant Ada.Streams.Stream_Element_Count := WS.Written;
      begin
         WS.Start_Message (Value'Address);
         WS.Write_Varint (1, Value.Start);
         WS.Write_Varint (2, Value.PB_End);
         if WS.End_Message (Value'Address, Offset) then
            Write_Extension_Range (WS'Access, Value);
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
    (Self  : in out Reserved_Range_Vector;
     Value : Reserved_Range) is
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
      Self.Data (Self.Length) := Value;
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
     Value  : out Reserved_Range) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Unsigned_32
                 (Stream, Key.Encoding, Value.Start);
            when 2 =>
               PB_Support.IO.Read_Unsigned_32
                 (Stream, Key.Encoding, Value.PB_End);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Reserved_Range;

   procedure Write_Reserved_Range
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Reserved_Range) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Reserved_Range (WS'Access, Value);
            return;
         end;
      end if;
      declare
         WS     : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
         Offset : constant Ada.Streams.Stream_Element_Count := WS.Written;
      begin
         WS.Start_Message (Value'Address);
         WS.Write_Varint (1, Value.Start);
         WS.Write_Varint (2, Value.PB_End);
         if WS.End_Message (Value'Address, Offset) then
            Write_Reserved_Range (WS'Access, Value);
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
    (Self  : in out Field_Descriptor_Proto_Vector;
     Value : Field_Descriptor_Proto) is
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
      Self.Data (Self.Length) := Value;
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
     Value  : out Field_Descriptor_Proto) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, Value.Name);
            when 3 =>
               PB_Support.IO.Read_Unsigned_32
                 (Stream, Key.Encoding, Value.Number);
            when 4 =>
               Label_IO.Read (Stream, Key.Encoding, Value.Label);
            when 5 =>
               PB_Type_IO.Read (Stream, Key.Encoding, Value.PB_Type);
            when 6 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, Value.Type_Name);
            when 2 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, Value.Extendee);
            when 7 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, Value.Default_Value);
            when 9 =>
               PB_Support.IO.Read_Unsigned_32
                 (Stream, Key.Encoding, Value.Oneof_Index);
            when 10 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, Value.Json_Name);
            when 8 =>
               if  not Value.Options.Is_Set then
                  Value.Options := (True, others => <>);
               end if;
               Field_Options_IO.Read
                 (Stream, Key.Encoding, Value.Options.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Field_Descriptor_Proto;

   procedure Write_Field_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Field_Descriptor_Proto) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Field_Descriptor_Proto (WS'Access, Value);
            return;
         end;
      end if;
      declare
         WS     : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
         Offset : constant Ada.Streams.Stream_Element_Count := WS.Written;
      begin
         WS.Start_Message (Value'Address);
         WS.Write (1, Value.Name);
         WS.Write_Varint (3, Value.Number);
         Label_IO.Write (WS, 4, Value.Label);
         PB_Type_IO.Write (WS, 5, Value.PB_Type);
         WS.Write (6, Value.Type_Name);
         WS.Write (2, Value.Extendee);
         WS.Write (7, Value.Default_Value);
         WS.Write_Varint (9, Value.Oneof_Index);
         WS.Write (10, Value.Json_Name);
         if Value.Options.Is_Set then
            WS.Write_Key ((8, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Field_Options'Write
              (Stream, Value.Options.Value);
         end if;
         if WS.End_Message (Value'Address, Offset) then
            Write_Field_Descriptor_Proto (WS'Access, Value);
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
    (Self  : in out Oneof_Descriptor_Proto_Vector;
     Value : Oneof_Descriptor_Proto) is
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
      Self.Data (Self.Length) := Value;
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
     Value  : out Oneof_Descriptor_Proto) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, Value.Name);
            when 2 =>
               if  not Value.Options.Is_Set then
                  Value.Options := (True, others => <>);
               end if;
               Oneof_Options_IO.Read
                 (Stream, Key.Encoding, Value.Options.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Oneof_Descriptor_Proto;

   procedure Write_Oneof_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Oneof_Descriptor_Proto) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Oneof_Descriptor_Proto (WS'Access, Value);
            return;
         end;
      end if;
      declare
         WS     : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
         Offset : constant Ada.Streams.Stream_Element_Count := WS.Written;
      begin
         WS.Start_Message (Value'Address);
         WS.Write (1, Value.Name);
         if Value.Options.Is_Set then
            WS.Write_Key ((2, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Oneof_Options'Write
              (Stream, Value.Options.Value);
         end if;
         if WS.End_Message (Value'Address, Offset) then
            Write_Oneof_Descriptor_Proto (WS'Access, Value);
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
    (Self  : in out Enum_Descriptor_Proto_Vector;
     Value : Enum_Descriptor_Proto) is
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
      Self.Data (Self.Length) := Value;
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
     Value  : out Enum_Descriptor_Proto) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, Value.Name);
            when 2 =>
               Enum_Value_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, Value.Value);
            when 3 =>
               if  not Value.Options.Is_Set then
                  Value.Options := (True, others => <>);
               end if;
               Enum_Options_IO.Read
                 (Stream, Key.Encoding, Value.Options.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Enum_Descriptor_Proto;

   procedure Write_Enum_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Enum_Descriptor_Proto) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Enum_Descriptor_Proto (WS'Access, Value);
            return;
         end;
      end if;
      declare
         WS     : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
         Offset : constant Ada.Streams.Stream_Element_Count := WS.Written;
      begin
         WS.Start_Message (Value'Address);
         WS.Write (1, Value.Name);
         for J in 1 .. Value.Value.Length loop
            WS.Write_Key ((2, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Enum_Value_Descriptor_Proto'Write
              (Stream, Value.Value.Get (J));
         end loop;
         if Value.Options.Is_Set then
            WS.Write_Key ((3, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Enum_Options'Write
              (Stream, Value.Options.Value);
         end if;
         if WS.End_Message (Value'Address, Offset) then
            Write_Enum_Descriptor_Proto (WS'Access, Value);
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
    (Self  : in out Enum_Value_Descriptor_Proto_Vector;
     Value : Enum_Value_Descriptor_Proto) is
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
      Self.Data (Self.Length) := Value;
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
     Value  : out Enum_Value_Descriptor_Proto) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, Value.Name);
            when 2 =>
               PB_Support.IO.Read_Unsigned_32
                 (Stream, Key.Encoding, Value.Number);
            when 3 =>
               if  not Value.Options.Is_Set then
                  Value.Options := (True, others => <>);
               end if;
               Enum_Value_Options_IO.Read
                 (Stream, Key.Encoding, Value.Options.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Enum_Value_Descriptor_Proto;

   procedure Write_Enum_Value_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Enum_Value_Descriptor_Proto) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Enum_Value_Descriptor_Proto (WS'Access, Value);
            return;
         end;
      end if;
      declare
         WS     : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
         Offset : constant Ada.Streams.Stream_Element_Count := WS.Written;
      begin
         WS.Start_Message (Value'Address);
         WS.Write (1, Value.Name);
         WS.Write_Varint (2, Value.Number);
         if Value.Options.Is_Set then
            WS.Write_Key ((3, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Enum_Value_Options'Write
              (Stream, Value.Options.Value);
         end if;
         if WS.End_Message (Value'Address, Offset) then
            Write_Enum_Value_Descriptor_Proto (WS'Access, Value);
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
    (Self  : in out Service_Descriptor_Proto_Vector;
     Value : Service_Descriptor_Proto) is
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
      Self.Data (Self.Length) := Value;
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
     Value  : out Service_Descriptor_Proto) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, Value.Name);
            when 2 =>
               Method_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, Value.Method);
            when 3 =>
               if  not Value.Options.Is_Set then
                  Value.Options := (True, others => <>);
               end if;
               Service_Options_IO.Read
                 (Stream, Key.Encoding, Value.Options.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Service_Descriptor_Proto;

   procedure Write_Service_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Service_Descriptor_Proto) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Service_Descriptor_Proto (WS'Access, Value);
            return;
         end;
      end if;
      declare
         WS     : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
         Offset : constant Ada.Streams.Stream_Element_Count := WS.Written;
      begin
         WS.Start_Message (Value'Address);
         WS.Write (1, Value.Name);
         for J in 1 .. Value.Method.Length loop
            WS.Write_Key ((2, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Method_Descriptor_Proto'Write
              (Stream, Value.Method.Get (J));
         end loop;
         if Value.Options.Is_Set then
            WS.Write_Key ((3, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Service_Options'Write
              (Stream, Value.Options.Value);
         end if;
         if WS.End_Message (Value'Address, Offset) then
            Write_Service_Descriptor_Proto (WS'Access, Value);
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
    (Self  : in out Method_Descriptor_Proto_Vector;
     Value : Method_Descriptor_Proto) is
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
      Self.Data (Self.Length) := Value;
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
     Value  : out Method_Descriptor_Proto) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, Value.Name);
            when 2 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, Value.Input_Type);
            when 3 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, Value.Output_Type);
            when 4 =>
               if  not Value.Options.Is_Set then
                  Value.Options := (True, others => <>);
               end if;
               Method_Options_IO.Read
                 (Stream, Key.Encoding, Value.Options.Value);
            when 5 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, Value.Client_Streaming);
            when 6 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, Value.Server_Streaming);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Method_Descriptor_Proto;

   procedure Write_Method_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Method_Descriptor_Proto) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Method_Descriptor_Proto (WS'Access, Value);
            return;
         end;
      end if;
      declare
         WS     : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
         Offset : constant Ada.Streams.Stream_Element_Count := WS.Written;
      begin
         WS.Start_Message (Value'Address);
         WS.Write (1, Value.Name);
         WS.Write (2, Value.Input_Type);
         WS.Write (3, Value.Output_Type);
         if Value.Options.Is_Set then
            WS.Write_Key ((4, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Method_Options'Write
              (Stream, Value.Options.Value);
         end if;
         WS.Write (5, Value.Client_Streaming);
         WS.Write (6, Value.Server_Streaming);
         if WS.End_Message (Value'Address, Offset) then
            Write_Method_Descriptor_Proto (WS'Access, Value);
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

   procedure Append
    (Self  : in out File_Options_Vector;
     Value : File_Options) is
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
      Self.Data (Self.Length) := Value;
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
     Value  : out File_Options) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, Value.Java_Package);
            when 8 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, Value.Java_Outer_Classname);
            when 10 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, Value.Java_Multiple_Files);
            when 20 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, Value.Java_Generate_Equals_And_Hash);
            when 27 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, Value.Java_String_Check_Utf_8);
            when 9 =>
               Optimize_Mode_IO.Read
                 (Stream, Key.Encoding, Value.Optimize_For);
            when 11 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, Value.Go_Package);
            when 16 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, Value.Cc_Generic_Services);
            when 17 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, Value.Java_Generic_Services);
            when 18 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, Value.Py_Generic_Services);
            when 23 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, Value.Deprecated);
            when 31 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, Value.Cc_Enable_Arenas);
            when 36 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, Value.Objc_Class_Prefix);
            when 37 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, Value.Csharp_Namespace);
            when 999 =>
               Uninterpreted_Option_IO.Read_Vector
                 (Stream, Key.Encoding, Value.Uninterpreted_Option);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_File_Options;

   procedure Write_File_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : File_Options) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_File_Options (WS'Access, Value);
            return;
         end;
      end if;
      declare
         WS     : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
         Offset : constant Ada.Streams.Stream_Element_Count := WS.Written;
      begin
         WS.Start_Message (Value'Address);
         WS.Write (1, Value.Java_Package);
         WS.Write (8, Value.Java_Outer_Classname);
         WS.Write (10, Value.Java_Multiple_Files);
         WS.Write (20, Value.Java_Generate_Equals_And_Hash);
         WS.Write (27, Value.Java_String_Check_Utf_8);
         Optimize_Mode_IO.Write (WS, 9, Value.Optimize_For);
         WS.Write (11, Value.Go_Package);
         WS.Write (16, Value.Cc_Generic_Services);
         WS.Write (17, Value.Java_Generic_Services);
         WS.Write (18, Value.Py_Generic_Services);
         WS.Write (23, Value.Deprecated);
         WS.Write (31, Value.Cc_Enable_Arenas);
         WS.Write (36, Value.Objc_Class_Prefix);
         WS.Write (37, Value.Csharp_Namespace);
         for J in 1 .. Value.Uninterpreted_Option.Length loop
            WS.Write_Key ((999, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Uninterpreted_Option'Write
              (Stream, Value.Uninterpreted_Option.Get (J));
         end loop;
         if WS.End_Message (Value'Address, Offset) then
            Write_File_Options (WS'Access, Value);
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
    (Self  : in out Message_Options_Vector;
     Value : Message_Options) is
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
      Self.Data (Self.Length) := Value;
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
     Value  : out Message_Options) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, Value.Message_Set_Wire_Format);
            when 2 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, Value.No_Standard_Descriptor_Accessor);
            when 3 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, Value.Deprecated);
            when 7 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, Value.Map_Entry);
            when 999 =>
               Uninterpreted_Option_IO.Read_Vector
                 (Stream, Key.Encoding, Value.Uninterpreted_Option);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Message_Options;

   procedure Write_Message_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Message_Options) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Message_Options (WS'Access, Value);
            return;
         end;
      end if;
      declare
         WS     : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
         Offset : constant Ada.Streams.Stream_Element_Count := WS.Written;
      begin
         WS.Start_Message (Value'Address);
         WS.Write (1, Value.Message_Set_Wire_Format);
         WS.Write (2, Value.No_Standard_Descriptor_Accessor);
         WS.Write (3, Value.Deprecated);
         WS.Write (7, Value.Map_Entry);
         for J in 1 .. Value.Uninterpreted_Option.Length loop
            WS.Write_Key ((999, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Uninterpreted_Option'Write
              (Stream, Value.Uninterpreted_Option.Get (J));
         end loop;
         if WS.End_Message (Value'Address, Offset) then
            Write_Message_Options (WS'Access, Value);
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
    (Self  : in out Field_Options_Vector;
     Value : Field_Options) is
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
      Self.Data (Self.Length) := Value;
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
     Value  : out Field_Options) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               CType_IO.Read (Stream, Key.Encoding, Value.Ctype);
            when 2 =>
               PB_Support.IO.Read_Boolean (Stream, Key.Encoding, Value.Packed);
            when 6 =>
               JSType_IO.Read (Stream, Key.Encoding, Value.Jstype);
            when 5 =>
               PB_Support.IO.Read_Boolean (Stream, Key.Encoding, Value.Lazy);
            when 3 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, Value.Deprecated);
            when 10 =>
               PB_Support.IO.Read_Boolean (Stream, Key.Encoding, Value.Weak);
            when 999 =>
               Uninterpreted_Option_IO.Read_Vector
                 (Stream, Key.Encoding, Value.Uninterpreted_Option);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Field_Options;

   procedure Write_Field_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Field_Options) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Field_Options (WS'Access, Value);
            return;
         end;
      end if;
      declare
         WS     : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
         Offset : constant Ada.Streams.Stream_Element_Count := WS.Written;
      begin
         WS.Start_Message (Value'Address);
         CType_IO.Write (WS, 1, Value.Ctype);
         WS.Write (2, Value.Packed);
         JSType_IO.Write (WS, 6, Value.Jstype);
         WS.Write (5, Value.Lazy);
         WS.Write (3, Value.Deprecated);
         WS.Write (10, Value.Weak);
         for J in 1 .. Value.Uninterpreted_Option.Length loop
            WS.Write_Key ((999, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Uninterpreted_Option'Write
              (Stream, Value.Uninterpreted_Option.Get (J));
         end loop;
         if WS.End_Message (Value'Address, Offset) then
            Write_Field_Options (WS'Access, Value);
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
    (Self  : in out Oneof_Options_Vector;
     Value : Oneof_Options) is
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
      Self.Data (Self.Length) := Value;
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
     Value  : out Oneof_Options) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 999 =>
               Uninterpreted_Option_IO.Read_Vector
                 (Stream, Key.Encoding, Value.Uninterpreted_Option);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Oneof_Options;

   procedure Write_Oneof_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Oneof_Options) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Oneof_Options (WS'Access, Value);
            return;
         end;
      end if;
      declare
         WS     : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
         Offset : constant Ada.Streams.Stream_Element_Count := WS.Written;
      begin
         WS.Start_Message (Value'Address);
         for J in 1 .. Value.Uninterpreted_Option.Length loop
            WS.Write_Key ((999, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Uninterpreted_Option'Write
              (Stream, Value.Uninterpreted_Option.Get (J));
         end loop;
         if WS.End_Message (Value'Address, Offset) then
            Write_Oneof_Options (WS'Access, Value);
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

   procedure Append
    (Self  : in out Enum_Options_Vector;
     Value : Enum_Options) is
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
      Self.Data (Self.Length) := Value;
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
     Value  : out Enum_Options) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 2 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, Value.Allow_Alias);
            when 3 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, Value.Deprecated);
            when 999 =>
               Uninterpreted_Option_IO.Read_Vector
                 (Stream, Key.Encoding, Value.Uninterpreted_Option);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Enum_Options;

   procedure Write_Enum_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Enum_Options) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Enum_Options (WS'Access, Value);
            return;
         end;
      end if;
      declare
         WS     : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
         Offset : constant Ada.Streams.Stream_Element_Count := WS.Written;
      begin
         WS.Start_Message (Value'Address);
         WS.Write (2, Value.Allow_Alias);
         WS.Write (3, Value.Deprecated);
         for J in 1 .. Value.Uninterpreted_Option.Length loop
            WS.Write_Key ((999, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Uninterpreted_Option'Write
              (Stream, Value.Uninterpreted_Option.Get (J));
         end loop;
         if WS.End_Message (Value'Address, Offset) then
            Write_Enum_Options (WS'Access, Value);
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
    (Self  : in out Enum_Value_Options_Vector;
     Value : Enum_Value_Options) is
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
      Self.Data (Self.Length) := Value;
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
     Value  : out Enum_Value_Options) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, Value.Deprecated);
            when 999 =>
               Uninterpreted_Option_IO.Read_Vector
                 (Stream, Key.Encoding, Value.Uninterpreted_Option);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Enum_Value_Options;

   procedure Write_Enum_Value_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Enum_Value_Options) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Enum_Value_Options (WS'Access, Value);
            return;
         end;
      end if;
      declare
         WS     : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
         Offset : constant Ada.Streams.Stream_Element_Count := WS.Written;
      begin
         WS.Start_Message (Value'Address);
         WS.Write (1, Value.Deprecated);
         for J in 1 .. Value.Uninterpreted_Option.Length loop
            WS.Write_Key ((999, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Uninterpreted_Option'Write
              (Stream, Value.Uninterpreted_Option.Get (J));
         end loop;
         if WS.End_Message (Value'Address, Offset) then
            Write_Enum_Value_Options (WS'Access, Value);
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
    (Self  : in out Service_Options_Vector;
     Value : Service_Options) is
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
      Self.Data (Self.Length) := Value;
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
     Value  : out Service_Options) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 33 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, Value.Deprecated);
            when 999 =>
               Uninterpreted_Option_IO.Read_Vector
                 (Stream, Key.Encoding, Value.Uninterpreted_Option);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Service_Options;

   procedure Write_Service_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Service_Options) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Service_Options (WS'Access, Value);
            return;
         end;
      end if;
      declare
         WS     : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
         Offset : constant Ada.Streams.Stream_Element_Count := WS.Written;
      begin
         WS.Start_Message (Value'Address);
         WS.Write (33, Value.Deprecated);
         for J in 1 .. Value.Uninterpreted_Option.Length loop
            WS.Write_Key ((999, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Uninterpreted_Option'Write
              (Stream, Value.Uninterpreted_Option.Get (J));
         end loop;
         if WS.End_Message (Value'Address, Offset) then
            Write_Service_Options (WS'Access, Value);
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
    (Self  : in out Method_Options_Vector;
     Value : Method_Options) is
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
      Self.Data (Self.Length) := Value;
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
     Value  : out Method_Options) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 33 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, Value.Deprecated);
            when 999 =>
               Uninterpreted_Option_IO.Read_Vector
                 (Stream, Key.Encoding, Value.Uninterpreted_Option);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Method_Options;

   procedure Write_Method_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Method_Options) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Method_Options (WS'Access, Value);
            return;
         end;
      end if;
      declare
         WS     : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
         Offset : constant Ada.Streams.Stream_Element_Count := WS.Written;
      begin
         WS.Start_Message (Value'Address);
         WS.Write (33, Value.Deprecated);
         for J in 1 .. Value.Uninterpreted_Option.Length loop
            WS.Write_Key ((999, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Uninterpreted_Option'Write
              (Stream, Value.Uninterpreted_Option.Get (J));
         end loop;
         if WS.End_Message (Value'Address, Offset) then
            Write_Method_Options (WS'Access, Value);
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
    (Self  : in out Uninterpreted_Option_Vector;
     Value : Uninterpreted_Option) is
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
      Self.Data (Self.Length) := Value;
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
     Value  : out Uninterpreted_Option) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 2 =>
               Name_Part_IO.Read_Vector (Stream, Key.Encoding, Value.Name);
            when 3 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, Value.Identifier_Value);
            when 4 =>
               PB_Support.IO.Read_Unsigned_64
                 (Stream, Key.Encoding, Value.Positive_Int_Value);
            when 5 =>
               PB_Support.IO.Read_Integer_64
                 (Stream, Key.Encoding, Value.Negative_Int_Value);
            when 6 =>
               PB_Support.IO.Read_IEEE_Float_64
                 (Stream, Key.Encoding, Value.Double_Value);
            when 7 =>
               PB_Support.IO.Read_Stream_Element_Vector
                 (Stream, Key.Encoding, Value.String_Value);
            when 8 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, Value.Aggregate_Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Uninterpreted_Option;

   procedure Write_Uninterpreted_Option
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Uninterpreted_Option) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Uninterpreted_Option (WS'Access, Value);
            return;
         end;
      end if;
      declare
         WS     : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
         Offset : constant Ada.Streams.Stream_Element_Count := WS.Written;
      begin
         WS.Start_Message (Value'Address);
         for J in 1 .. Value.Name.Length loop
            WS.Write_Key ((2, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Name_Part'Write
              (Stream, Value.Name.Get (J));
         end loop;
         WS.Write (3, Value.Identifier_Value);
         WS.Write_Varint (4, Value.Positive_Int_Value);
         WS.Write_Varint (5, Value.Negative_Int_Value);
         WS.Write (6, Value.Double_Value);
         WS.Write (7, Value.String_Value);
         WS.Write (8, Value.Aggregate_Value);
         if WS.End_Message (Value'Address, Offset) then
            Write_Uninterpreted_Option (WS'Access, Value);
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

   procedure Append (Self  : in out Name_Part_Vector; Value : Name_Part) is
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
      Self.Data (Self.Length) := Value;
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
     Value  : out Name_Part) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, Value.Name_Part);
            when 2 =>
               PB_Support.IO.Read_Boolean
                 (Stream, Key.Encoding, Value.Is_Extension);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Name_Part;

   procedure Write_Name_Part
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Name_Part) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Name_Part (WS'Access, Value);
            return;
         end;
      end if;
      declare
         WS     : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
         Offset : constant Ada.Streams.Stream_Element_Count := WS.Written;
      begin
         WS.Start_Message (Value'Address);
         WS.Write (1, Value.Name_Part);
         WS.Write (2, Value.Is_Extension);
         if WS.End_Message (Value'Address, Offset) then
            Write_Name_Part (WS'Access, Value);
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
    (Self  : in out Source_Code_Info_Vector;
     Value : Source_Code_Info) is
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
      Self.Data (Self.Length) := Value;
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
     Value  : out Source_Code_Info) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               Location_IO.Read_Vector (Stream, Key.Encoding, Value.Location);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Source_Code_Info;

   procedure Write_Source_Code_Info
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Source_Code_Info) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Source_Code_Info (WS'Access, Value);
            return;
         end;
      end if;
      declare
         WS     : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
         Offset : constant Ada.Streams.Stream_Element_Count := WS.Written;
      begin
         WS.Start_Message (Value'Address);
         for J in 1 .. Value.Location.Length loop
            WS.Write_Key ((1, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Location'Write
              (Stream, Value.Location.Get (J));
         end loop;
         if WS.End_Message (Value'Address, Offset) then
            Write_Source_Code_Info (WS'Access, Value);
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

   procedure Append (Self  : in out Location_Vector; Value : Location) is
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
      Self.Data (Self.Length) := Value;
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
     Value  : out Location) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Unsigned_32_Vector
                 (Stream, Key.Encoding, Value.Path);
            when 2 =>
               PB_Support.IO.Read_Unsigned_32_Vector
                 (Stream, Key.Encoding, Value.Span);
            when 3 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, Value.Leading_Comments);
            when 4 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, Value.Trailing_Comments);
            when 6 =>
               PB_Support.IO.Read_Universal_String_Vector
                 (Stream, Key.Encoding, Value.Leading_Detached_Comments);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Location;

   procedure Write_Location
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Location) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Location (WS'Access, Value);
            return;
         end;
      end if;
      declare
         WS     : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
         Offset : constant Ada.Streams.Stream_Element_Count := WS.Written;
      begin
         WS.Start_Message (Value'Address);
         WS.Write_Varint (1, Value.Path);
         WS.Write_Varint (2, Value.Span);
         WS.Write (3, Value.Leading_Comments);
         WS.Write (4, Value.Trailing_Comments);
         WS.Write (6, Value.Leading_Detached_Comments);
         if WS.End_Message (Value'Address, Offset) then
            Write_Location (WS'Access, Value);
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
    (Self  : in out Generated_Code_Info_Vector;
     Value : Generated_Code_Info) is
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
      Self.Data (Self.Length) := Value;
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
     Value  : out Generated_Code_Info) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               Annotation_IO.Read_Vector
                 (Stream, Key.Encoding, Value.Annotation);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Generated_Code_Info;

   procedure Write_Generated_Code_Info
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Generated_Code_Info) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Generated_Code_Info (WS'Access, Value);
            return;
         end;
      end if;
      declare
         WS     : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
         Offset : constant Ada.Streams.Stream_Element_Count := WS.Written;
      begin
         WS.Start_Message (Value'Address);
         for J in 1 .. Value.Annotation.Length loop
            WS.Write_Key ((1, PB_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Annotation'Write
              (Stream, Value.Annotation.Get (J));
         end loop;
         if WS.End_Message (Value'Address, Offset) then
            Write_Generated_Code_Info (WS'Access, Value);
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

   procedure Append (Self  : in out Annotation_Vector; Value : Annotation) is
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
      Self.Data (Self.Length) := Value;
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
     Value  : out Annotation) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Unsigned_32_Vector
                 (Stream, Key.Encoding, Value.Path);
            when 2 =>
               PB_Support.IO.Read_Universal_String
                 (Stream, Key.Encoding, Value.Source_File);
            when 3 =>
               PB_Support.IO.Read_Unsigned_32
                 (Stream, Key.Encoding, Value.PB_Begin);
            when 4 =>
               PB_Support.IO.Read_Unsigned_32
                 (Stream, Key.Encoding, Value.PB_End);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Annotation;

   procedure Write_Annotation
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Annotation) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Annotation (WS'Access, Value);
            return;
         end;
      end if;
      declare
         WS     : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
         Offset : constant Ada.Streams.Stream_Element_Count := WS.Written;
      begin
         WS.Start_Message (Value'Address);
         WS.Write_Varint (1, Value.Path);
         WS.Write (2, Value.Source_File);
         WS.Write_Varint (3, Value.PB_Begin);
         WS.Write_Varint (4, Value.PB_End);
         if WS.End_Message (Value'Address, Offset) then
            Write_Annotation (WS'Access, Value);
         end if;
      end;
   end Write_Annotation;

end Google.Protobuf.Descriptor;