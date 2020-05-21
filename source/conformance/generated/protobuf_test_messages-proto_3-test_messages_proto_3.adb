with Ada.Unchecked_Deallocation;
with PB_Support.IO;
with PB_Support.Internal;

package body Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3 is

   package Any_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Any.Any, Google.Protobuf.Any.Any_Vector,
        Google.Protobuf.Any.Append);

   package Bool_Value_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Wrappers.Bool_Value,
        Google.Protobuf.Wrappers.Bool_Value_Vector,
        Google.Protobuf.Wrappers.Append);

   package Bytes_Value_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Wrappers.Bytes_Value,
        Google.Protobuf.Wrappers.Bytes_Value_Vector,
        Google.Protobuf.Wrappers.Append);

   package Double_Value_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Wrappers.Double_Value,
        Google.Protobuf.Wrappers.Double_Value_Vector,
        Google.Protobuf.Wrappers.Append);

   package Duration_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Duration.Duration,
        Google.Protobuf.Duration.Duration_Vector,
        Google.Protobuf.Duration.Append);

   package Field_Mask_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Field_Mask.Field_Mask,
        Google.Protobuf.Field_Mask.Field_Mask_Vector,
        Google.Protobuf.Field_Mask.Append);

   package Float_Value_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Wrappers.Float_Value,
        Google.Protobuf.Wrappers.Float_Value_Vector,
        Google.Protobuf.Wrappers.Append);

   package Int_32Value_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Wrappers.Int_32Value,
        Google.Protobuf.Wrappers.Int_32Value_Vector,
        Google.Protobuf.Wrappers.Append);

   package Int_64Value_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Wrappers.Int_64Value,
        Google.Protobuf.Wrappers.Int_64Value_Vector,
        Google.Protobuf.Wrappers.Append);

   package List_Value_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Struct.List_Value,
        Google.Protobuf.Struct.List_Value_Vector,
        Google.Protobuf.Struct.Append);

   package String_Value_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Wrappers.String_Value,
        Google.Protobuf.Wrappers.String_Value_Vector,
        Google.Protobuf.Wrappers.Append);

   package Struct_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Struct.Struct, Google.Protobuf.Struct.Struct_Vector,
        Google.Protobuf.Struct.Append);

   package Timestamp_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Timestamp.Timestamp,
        Google.Protobuf.Timestamp.Timestamp_Vector,
        Google.Protobuf.Timestamp.Append);

   package UInt_32Value_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Wrappers.UInt_32Value,
        Google.Protobuf.Wrappers.UInt_32Value_Vector,
        Google.Protobuf.Wrappers.Append);

   package UInt_64Value_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Wrappers.UInt_64Value,
        Google.Protobuf.Wrappers.UInt_64Value_Vector,
        Google.Protobuf.Wrappers.Append);

   package Value_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Struct.Value, Google.Protobuf.Struct.Value_Vector,
        Google.Protobuf.Struct.Append);

   type Integer_Foreign_Enum is  range 0 .. 2
     with Size =>
       Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Foreign_Enum'Size;

   package Foreign_Enum_IO is
     new PB_Support.IO.Enum_IO
       (Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Foreign_Enum,
        Integer_Foreign_Enum,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Foreign_Enum_Vectors);

   package Foreign_Message_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Foreign_Message,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Foreign_Message_Vector,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Append);

   package Test_All_Types_Proto_3_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Test_All_Types_Proto_3,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Test_All_Types_Proto_3_Vector,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Append);

   type Integer_Aliased_Enum is  range 0 .. 2
     with Size =>
       Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Aliased_Enum'Size;

   package Aliased_Enum_IO is
     new PB_Support.IO.Enum_IO
       (Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Aliased_Enum,
        Integer_Aliased_Enum,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Aliased_Enum_Vectors);

   package Map_Bool_Bool_Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_Bool_Bool_Entry,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_Bool_Bool_Entry_Vector,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Append);

   package Map_Fixed_32Fixed_32Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_Fixed_32Fixed_32Entry,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_Fixed_32Fixed_32Entry_Vector,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Append);

   package Map_Fixed_64Fixed_64Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_Fixed_64Fixed_64Entry,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_Fixed_64Fixed_64Entry_Vector,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Append);

   package Map_Int_32Double_Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_Int_32Double_Entry,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_Int_32Double_Entry_Vector,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Append);

   package Map_Int_32Float_Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_Int_32Float_Entry,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_Int_32Float_Entry_Vector,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Append);

   package Map_Int_32Int_32Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_Int_32Int_32Entry,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_Int_32Int_32Entry_Vector,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Append);

   package Map_Int_64Int_64Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_Int_64Int_64Entry,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_Int_64Int_64Entry_Vector,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Append);

   package Map_Sfixed_32Sfixed_32Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_Sfixed_32Sfixed_32Entry,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_Sfixed_32Sfixed_32Entry_Vector,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Append);

   package Map_Sfixed_64Sfixed_64Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_Sfixed_64Sfixed_64Entry,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_Sfixed_64Sfixed_64Entry_Vector,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Append);

   package Map_Sint_32Sint_32Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_Sint_32Sint_32Entry,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_Sint_32Sint_32Entry_Vector,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Append);

   package Map_Sint_64Sint_64Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_Sint_64Sint_64Entry,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_Sint_64Sint_64Entry_Vector,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Append);

   package Map_String_Bytes_Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_String_Bytes_Entry,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_String_Bytes_Entry_Vector,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Append);

   package Map_String_Foreign_Enum_Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_String_Foreign_Enum_Entry,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_String_Foreign_Enum_Entry_Vector,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Append);

   package Map_String_Foreign_Message_Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_String_Foreign_Message_Entry,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_String_Foreign_Message_Entry_Vector,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Append);

   package Map_String_Nested_Enum_Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_String_Nested_Enum_Entry,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_String_Nested_Enum_Entry_Vector,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Append);

   package Map_String_Nested_Message_Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_String_Nested_Message_Entry,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_String_Nested_Message_Entry_Vector,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Append);

   package Map_String_String_Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_String_String_Entry,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_String_String_Entry_Vector,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Append);

   package Map_Uint_32Uint_32Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_Uint_32Uint_32Entry,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_Uint_32Uint_32Entry_Vector,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Append);

   package Map_Uint_64Uint_64Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_Uint_64Uint_64Entry,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Map_Uint_64Uint_64Entry_Vector,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Append);

   type Integer_Nested_Enum is  range  - 1 .. 2
     with Size =>
       Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Nested_Enum'Size;

   package Nested_Enum_IO is
     new PB_Support.IO.Enum_IO
       (Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Nested_Enum,
        Integer_Nested_Enum,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Nested_Enum_Vectors);

   package Nested_Message_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Nested_Message,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
          .Nested_Message_Vector,
        Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.Append);

   function Length (Self : Test_All_Types_Proto_3_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Test_All_Types_Proto_3_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Test_All_Types_Proto_3_Array, Test_All_Types_Proto_3_Array_Access);

   procedure Append
    (Self : in out Test_All_Types_Proto_3_Vector;
     V    : Test_All_Types_Proto_3) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Test_All_Types_Proto_3'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Test_All_Types_Proto_3_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Test_All_Types_Proto_3_Array'
             (Self.Data.all
                & Test_All_Types_Proto_3_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Test_All_Types_Proto_3_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Test_All_Types_Proto_3_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Test_All_Types_Proto_3_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Test_All_Types_Proto_3_Variable_Reference
    (Self  : aliased in out Test_All_Types_Proto_3_Vector;
     Index : Positive)
      return Test_All_Types_Proto_3_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Test_All_Types_Proto_3_Variable_Reference;

   not overriding function Get_Test_All_Types_Proto_3_Constant_Reference
    (Self  : aliased Test_All_Types_Proto_3_Vector;
     Index : Positive)
      return Test_All_Types_Proto_3_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Test_All_Types_Proto_3_Constant_Reference;

   procedure Read_Test_All_Types_Proto_3
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Test_All_Types_Proto_3) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Optional_Int_32);
            when 2 =>
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Optional_Int_64);
            when 3 =>
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Optional_Uint_32);
            when 4 =>
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Optional_Uint_64);
            when 5 =>
               PB_Support.IO.Read_Zigzag
                 (Stream, Key.Encoding, V.Optional_Sint_32);
            when 6 =>
               PB_Support.IO.Read_Zigzag
                 (Stream, Key.Encoding, V.Optional_Sint_64);
            when 7 =>
               PB_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Optional_Fixed_32);
            when 8 =>
               PB_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Optional_Fixed_64);
            when 9 =>
               PB_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Optional_Sfixed_32);
            when 10 =>
               PB_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Optional_Sfixed_64);
            when 11 =>
               PB_Support.IO.Read (Stream, Key.Encoding, V.Optional_Float);
            when 12 =>
               PB_Support.IO.Read (Stream, Key.Encoding, V.Optional_Double);
            when 13 =>
               PB_Support.IO.Read (Stream, Key.Encoding, V.Optional_Bool);
            when 14 =>
               PB_Support.IO.Read (Stream, Key.Encoding, V.Optional_String);
            when 15 =>
               PB_Support.IO.Read (Stream, Key.Encoding, V.Optional_Bytes);
            when 18 =>
               if  not V.Optional_Nested_Message.Is_Set then
                  V.Optional_Nested_Message := (True, others => <>);
               end if;
               Nested_Message_IO.Read
                 (Stream, Key.Encoding, V.Optional_Nested_Message.Value);
            when 19 =>
               if  not V.Optional_Foreign_Message.Is_Set then
                  V.Optional_Foreign_Message := (True, others => <>);
               end if;
               Foreign_Message_IO.Read
                 (Stream, Key.Encoding, V.Optional_Foreign_Message.Value);
            when 21 =>
               Nested_Enum_IO.Read
                 (Stream, Key.Encoding, V.Optional_Nested_Enum);
            when 22 =>
               Foreign_Enum_IO.Read
                 (Stream, Key.Encoding, V.Optional_Foreign_Enum);
            when 23 =>
               Aliased_Enum_IO.Read
                 (Stream, Key.Encoding, V.Optional_Aliased_Enum);
            when 24 =>
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Optional_String_Piece);
            when 25 =>
               PB_Support.IO.Read (Stream, Key.Encoding, V.Optional_Cord);
            when 27 =>
               if V.Recursive_Message.Length = 0 then
                  V.Recursive_Message.Append ((others => <>));
               end if;
               Test_All_Types_Proto_3_IO.Read
                 (Stream, Key.Encoding, V.Recursive_Message (1));
            when 31 =>
               PB_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Repeated_Int_32);
            when 32 =>
               PB_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Repeated_Int_64);
            when 33 =>
               PB_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Repeated_Uint_32);
            when 34 =>
               PB_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Repeated_Uint_64);
            when 35 =>
               PB_Support.IO.Read_Zigzag_Vector
                 (Stream, Key.Encoding, V.Repeated_Sint_32);
            when 36 =>
               PB_Support.IO.Read_Zigzag_Vector
                 (Stream, Key.Encoding, V.Repeated_Sint_64);
            when 37 =>
               PB_Support.IO.Read_Fixed_Vector
                 (Stream, Key.Encoding, V.Repeated_Fixed_32);
            when 38 =>
               PB_Support.IO.Read_Fixed_Vector
                 (Stream, Key.Encoding, V.Repeated_Fixed_64);
            when 39 =>
               PB_Support.IO.Read_Fixed_Vector
                 (Stream, Key.Encoding, V.Repeated_Sfixed_32);
            when 40 =>
               PB_Support.IO.Read_Fixed_Vector
                 (Stream, Key.Encoding, V.Repeated_Sfixed_64);
            when 41 =>
               PB_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Float);
            when 42 =>
               PB_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Double);
            when 43 =>
               PB_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Bool);
            when 44 =>
               PB_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_String);
            when 45 =>
               PB_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Bytes);
            when 48 =>
               Nested_Message_IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Nested_Message);
            when 49 =>
               Foreign_Message_IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Foreign_Message);
            when 51 =>
               Nested_Enum_IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Nested_Enum);
            when 52 =>
               Foreign_Enum_IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Foreign_Enum);
            when 54 =>
               PB_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_String_Piece);
            when 55 =>
               PB_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Cord);
            when 75 =>
               PB_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Packed_Int_32);
            when 76 =>
               PB_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Packed_Int_64);
            when 77 =>
               PB_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Packed_Uint_32);
            when 78 =>
               PB_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Packed_Uint_64);
            when 79 =>
               PB_Support.IO.Read_Zigzag_Vector
                 (Stream, Key.Encoding, V.Packed_Sint_32);
            when 80 =>
               PB_Support.IO.Read_Zigzag_Vector
                 (Stream, Key.Encoding, V.Packed_Sint_64);
            when 81 =>
               PB_Support.IO.Read_Fixed_Vector
                 (Stream, Key.Encoding, V.Packed_Fixed_32);
            when 82 =>
               PB_Support.IO.Read_Fixed_Vector
                 (Stream, Key.Encoding, V.Packed_Fixed_64);
            when 83 =>
               PB_Support.IO.Read_Fixed_Vector
                 (Stream, Key.Encoding, V.Packed_Sfixed_32);
            when 84 =>
               PB_Support.IO.Read_Fixed_Vector
                 (Stream, Key.Encoding, V.Packed_Sfixed_64);
            when 85 =>
               PB_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Packed_Float);
            when 86 =>
               PB_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Packed_Double);
            when 87 =>
               PB_Support.IO.Read_Vector (Stream, Key.Encoding, V.Packed_Bool);
            when 88 =>
               Nested_Enum_IO.Read_Vector
                 (Stream, Key.Encoding, V.Packed_Nested_Enum);
            when 89 =>
               PB_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Unpacked_Int_32);
            when 90 =>
               PB_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Unpacked_Int_64);
            when 91 =>
               PB_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Unpacked_Uint_32);
            when 92 =>
               PB_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Unpacked_Uint_64);
            when 93 =>
               PB_Support.IO.Read_Zigzag_Vector
                 (Stream, Key.Encoding, V.Unpacked_Sint_32);
            when 94 =>
               PB_Support.IO.Read_Zigzag_Vector
                 (Stream, Key.Encoding, V.Unpacked_Sint_64);
            when 95 =>
               PB_Support.IO.Read_Fixed_Vector
                 (Stream, Key.Encoding, V.Unpacked_Fixed_32);
            when 96 =>
               PB_Support.IO.Read_Fixed_Vector
                 (Stream, Key.Encoding, V.Unpacked_Fixed_64);
            when 97 =>
               PB_Support.IO.Read_Fixed_Vector
                 (Stream, Key.Encoding, V.Unpacked_Sfixed_32);
            when 98 =>
               PB_Support.IO.Read_Fixed_Vector
                 (Stream, Key.Encoding, V.Unpacked_Sfixed_64);
            when 99 =>
               PB_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Unpacked_Float);
            when 100 =>
               PB_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Unpacked_Double);
            when 101 =>
               PB_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Unpacked_Bool);
            when 102 =>
               Nested_Enum_IO.Read_Vector
                 (Stream, Key.Encoding, V.Unpacked_Nested_Enum);
            when 56 =>
               Map_Int_32Int_32Entry_IO.Read_Vector
                 (Stream, Key.Encoding, V.Map_Int_32_Int_32);
            when 57 =>
               Map_Int_64Int_64Entry_IO.Read_Vector
                 (Stream, Key.Encoding, V.Map_Int_64_Int_64);
            when 58 =>
               Map_Uint_32Uint_32Entry_IO.Read_Vector
                 (Stream, Key.Encoding, V.Map_Uint_32_Uint_32);
            when 59 =>
               Map_Uint_64Uint_64Entry_IO.Read_Vector
                 (Stream, Key.Encoding, V.Map_Uint_64_Uint_64);
            when 60 =>
               Map_Sint_32Sint_32Entry_IO.Read_Vector
                 (Stream, Key.Encoding, V.Map_Sint_32_Sint_32);
            when 61 =>
               Map_Sint_64Sint_64Entry_IO.Read_Vector
                 (Stream, Key.Encoding, V.Map_Sint_64_Sint_64);
            when 62 =>
               Map_Fixed_32Fixed_32Entry_IO.Read_Vector
                 (Stream, Key.Encoding, V.Map_Fixed_32_Fixed_32);
            when 63 =>
               Map_Fixed_64Fixed_64Entry_IO.Read_Vector
                 (Stream, Key.Encoding, V.Map_Fixed_64_Fixed_64);
            when 64 =>
               Map_Sfixed_32Sfixed_32Entry_IO.Read_Vector
                 (Stream, Key.Encoding, V.Map_Sfixed_32_Sfixed_32);
            when 65 =>
               Map_Sfixed_64Sfixed_64Entry_IO.Read_Vector
                 (Stream, Key.Encoding, V.Map_Sfixed_64_Sfixed_64);
            when 66 =>
               Map_Int_32Float_Entry_IO.Read_Vector
                 (Stream, Key.Encoding, V.Map_Int_32_Float);
            when 67 =>
               Map_Int_32Double_Entry_IO.Read_Vector
                 (Stream, Key.Encoding, V.Map_Int_32_Double);
            when 68 =>
               Map_Bool_Bool_Entry_IO.Read_Vector
                 (Stream, Key.Encoding, V.Map_Bool_Bool);
            when 69 =>
               Map_String_String_Entry_IO.Read_Vector
                 (Stream, Key.Encoding, V.Map_String_String);
            when 70 =>
               Map_String_Bytes_Entry_IO.Read_Vector
                 (Stream, Key.Encoding, V.Map_String_Bytes);
            when 71 =>
               Map_String_Nested_Message_Entry_IO.Read_Vector
                 (Stream, Key.Encoding, V.Map_String_Nested_Message);
            when 72 =>
               Map_String_Foreign_Message_Entry_IO.Read_Vector
                 (Stream, Key.Encoding, V.Map_String_Foreign_Message);
            when 73 =>
               Map_String_Nested_Enum_Entry_IO.Read_Vector
                 (Stream, Key.Encoding, V.Map_String_Nested_Enum);
            when 74 =>
               Map_String_Foreign_Enum_Entry_IO.Read_Vector
                 (Stream, Key.Encoding, V.Map_String_Foreign_Enum);
            when 111 =>
               if V.Variant.Oneof_Field /= Oneof_Uint_32_Kind then
                  V.Variant := (Oneof_Uint_32_Kind, others => <>);
               end if;
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Variant.Oneof_Uint_32);
            when 112 =>
               if V.Variant.Oneof_Field /= Oneof_Nested_Message_Kind then
                  V.Variant := (Oneof_Nested_Message_Kind, others => <>);
               end if;
               Nested_Message_IO.Read
                 (Stream, Key.Encoding, V.Variant.Oneof_Nested_Message);
            when 113 =>
               if V.Variant.Oneof_Field /= Oneof_String_Kind then
                  V.Variant := (Oneof_String_Kind, others => <>);
               end if;
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Variant.Oneof_String);
            when 114 =>
               if V.Variant.Oneof_Field /= Oneof_Bytes_Kind then
                  V.Variant := (Oneof_Bytes_Kind, others => <>);
               end if;
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Variant.Oneof_Bytes);
            when 115 =>
               if V.Variant.Oneof_Field /= Oneof_Bool_Kind then
                  V.Variant := (Oneof_Bool_Kind, others => <>);
               end if;
               PB_Support.IO.Read (Stream, Key.Encoding, V.Variant.Oneof_Bool);
            when 116 =>
               if V.Variant.Oneof_Field /= Oneof_Uint_64_Kind then
                  V.Variant := (Oneof_Uint_64_Kind, others => <>);
               end if;
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Variant.Oneof_Uint_64);
            when 117 =>
               if V.Variant.Oneof_Field /= Oneof_Float_Kind then
                  V.Variant := (Oneof_Float_Kind, others => <>);
               end if;
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Variant.Oneof_Float);
            when 118 =>
               if V.Variant.Oneof_Field /= Oneof_Double_Kind then
                  V.Variant := (Oneof_Double_Kind, others => <>);
               end if;
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Variant.Oneof_Double);
            when 119 =>
               if V.Variant.Oneof_Field /= Oneof_Enum_Kind then
                  V.Variant := (Oneof_Enum_Kind, others => <>);
               end if;
               Nested_Enum_IO.Read
                 (Stream, Key.Encoding, V.Variant.Oneof_Enum);
            when 201 =>
               if  not V.Optional_Bool_Wrapper.Is_Set then
                  V.Optional_Bool_Wrapper := (True, others => <>);
               end if;
               Bool_Value_IO.Read
                 (Stream, Key.Encoding, V.Optional_Bool_Wrapper.Value);
            when 202 =>
               if  not V.Optional_Int_32_Wrapper.Is_Set then
                  V.Optional_Int_32_Wrapper := (True, others => <>);
               end if;
               Int_32Value_IO.Read
                 (Stream, Key.Encoding, V.Optional_Int_32_Wrapper.Value);
            when 203 =>
               if  not V.Optional_Int_64_Wrapper.Is_Set then
                  V.Optional_Int_64_Wrapper := (True, others => <>);
               end if;
               Int_64Value_IO.Read
                 (Stream, Key.Encoding, V.Optional_Int_64_Wrapper.Value);
            when 204 =>
               if  not V.Optional_Uint_32_Wrapper.Is_Set then
                  V.Optional_Uint_32_Wrapper := (True, others => <>);
               end if;
               UInt_32Value_IO.Read
                 (Stream, Key.Encoding, V.Optional_Uint_32_Wrapper.Value);
            when 205 =>
               if  not V.Optional_Uint_64_Wrapper.Is_Set then
                  V.Optional_Uint_64_Wrapper := (True, others => <>);
               end if;
               UInt_64Value_IO.Read
                 (Stream, Key.Encoding, V.Optional_Uint_64_Wrapper.Value);
            when 206 =>
               if  not V.Optional_Float_Wrapper.Is_Set then
                  V.Optional_Float_Wrapper := (True, others => <>);
               end if;
               Float_Value_IO.Read
                 (Stream, Key.Encoding, V.Optional_Float_Wrapper.Value);
            when 207 =>
               if  not V.Optional_Double_Wrapper.Is_Set then
                  V.Optional_Double_Wrapper := (True, others => <>);
               end if;
               Double_Value_IO.Read
                 (Stream, Key.Encoding, V.Optional_Double_Wrapper.Value);
            when 208 =>
               if  not V.Optional_String_Wrapper.Is_Set then
                  V.Optional_String_Wrapper := (True, others => <>);
               end if;
               String_Value_IO.Read
                 (Stream, Key.Encoding, V.Optional_String_Wrapper.Value);
            when 209 =>
               if  not V.Optional_Bytes_Wrapper.Is_Set then
                  V.Optional_Bytes_Wrapper := (True, others => <>);
               end if;
               Bytes_Value_IO.Read
                 (Stream, Key.Encoding, V.Optional_Bytes_Wrapper.Value);
            when 211 =>
               Bool_Value_IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Bool_Wrapper);
            when 212 =>
               Int_32Value_IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Int_32_Wrapper);
            when 213 =>
               Int_64Value_IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Int_64_Wrapper);
            when 214 =>
               UInt_32Value_IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Uint_32_Wrapper);
            when 215 =>
               UInt_64Value_IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Uint_64_Wrapper);
            when 216 =>
               Float_Value_IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Float_Wrapper);
            when 217 =>
               Double_Value_IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Double_Wrapper);
            when 218 =>
               String_Value_IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_String_Wrapper);
            when 219 =>
               Bytes_Value_IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Bytes_Wrapper);
            when 301 =>
               if  not V.Optional_Duration.Is_Set then
                  V.Optional_Duration := (True, others => <>);
               end if;
               Duration_IO.Read
                 (Stream, Key.Encoding, V.Optional_Duration.Value);
            when 302 =>
               if  not V.Optional_Timestamp.Is_Set then
                  V.Optional_Timestamp := (True, others => <>);
               end if;
               Timestamp_IO.Read
                 (Stream, Key.Encoding, V.Optional_Timestamp.Value);
            when 303 =>
               if  not V.Optional_Field_Mask.Is_Set then
                  V.Optional_Field_Mask := (True, others => <>);
               end if;
               Field_Mask_IO.Read
                 (Stream, Key.Encoding, V.Optional_Field_Mask.Value);
            when 304 =>
               if  not V.Optional_Struct.Is_Set then
                  V.Optional_Struct := (True, others => <>);
               end if;
               Struct_IO.Read (Stream, Key.Encoding, V.Optional_Struct.Value);
            when 305 =>
               if  not V.Optional_Any.Is_Set then
                  V.Optional_Any := (True, others => <>);
               end if;
               Any_IO.Read (Stream, Key.Encoding, V.Optional_Any.Value);
            when 306 =>
               if  not V.Optional_Value.Is_Set then
                  V.Optional_Value := (True, others => <>);
               end if;
               Value_IO.Read (Stream, Key.Encoding, V.Optional_Value.Value);
            when 311 =>
               Duration_IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Duration);
            when 312 =>
               Timestamp_IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Timestamp);
            when 313 =>
               Field_Mask_IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Fieldmask);
            when 324 =>
               Struct_IO.Read_Vector (Stream, Key.Encoding, V.Repeated_Struct);
            when 315 =>
               Any_IO.Read_Vector (Stream, Key.Encoding, V.Repeated_Any);
            when 316 =>
               Value_IO.Read_Vector (Stream, Key.Encoding, V.Repeated_Value);
            when 317 =>
               List_Value_IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_List_Value);
            when 401 =>
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.Fieldname_1);
            when 402 =>
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_2);
            when 403 =>
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_3);
            when 404 =>
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_4);
            when 405 =>
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_0name_5);
            when 406 =>
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_0_Name_6);
            when 407 =>
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_7);
            when 408 =>
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_8);
            when 409 =>
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_9);
            when 410 =>
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_10);
            when 411 =>
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.FIELD_NAME11);
            when 412 =>
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.FIELD_Name_12);
            when 413 =>
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_13);
            when 414 =>
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_14);
            when 415 =>
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_15);
            when 416 =>
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_16);
            when 417 =>
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_17);
            when 418 =>
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_18);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Test_All_Types_Proto_3;

   procedure Write_Test_All_Types_Proto_3
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Test_All_Types_Proto_3) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Test_All_Types_Proto_3 (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Varint_Option (1, V.Optional_Int_32, 0);
         WS.Write_Varint_Option (2, V.Optional_Int_64, 0);
         WS.Write_Varint_Option (3, V.Optional_Uint_32, 0);
         WS.Write_Varint_Option (4, V.Optional_Uint_64, 0);
         WS.Write_Zigzag_Option (5, V.Optional_Sint_32, 0);
         WS.Write_Zigzag_Option (6, V.Optional_Sint_64, 0);
         WS.Write_Fixed_Option (7, V.Optional_Fixed_32, 0);
         WS.Write_Fixed_Option (8, V.Optional_Fixed_64, 0);
         WS.Write_Fixed_Option (9, V.Optional_Sfixed_32, 0);
         WS.Write_Fixed_Option (10, V.Optional_Sfixed_64, 0);
         WS.Write_Option (11, V.Optional_Float, 0.0);
         WS.Write_Option (12, V.Optional_Double, 0.0);
         WS.Write_Option (13, V.Optional_Bool, False);
         WS.Write_Option (14, V.Optional_String);
         WS.Write_Option (15, V.Optional_Bytes);
         if V.Optional_Nested_Message.Is_Set then
            WS.Write_Key ((18, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
              .Nested_Message'Write
              (Stream, V.Optional_Nested_Message.Value);
         end if;
         if V.Optional_Foreign_Message.Is_Set then
            WS.Write_Key ((19, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
              .Foreign_Message'Write
              (Stream, V.Optional_Foreign_Message.Value);
         end if;
         Nested_Enum_IO.Write_Option
           (WS, 21, V.Optional_Nested_Enum,
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.FOO);
         Foreign_Enum_IO.Write_Option
           (WS, 22, V.Optional_Foreign_Enum,
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.FOREIGN_FOO);
         Aliased_Enum_IO.Write_Option
           (WS, 23, V.Optional_Aliased_Enum,
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.ALIAS_FOO);
         WS.Write_Option (24, V.Optional_String_Piece);
         WS.Write_Option (25, V.Optional_Cord);
         for J in 1 .. V.Recursive_Message.Length loop
            WS.Write_Key ((27, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
              .Test_All_Types_Proto_3'Write
              (Stream, V.Recursive_Message (J));
         end loop;
         WS.Write_Varint_Packed (31, V.Repeated_Int_32);
         WS.Write_Varint_Packed (32, V.Repeated_Int_64);
         WS.Write_Varint_Packed (33, V.Repeated_Uint_32);
         WS.Write_Varint_Packed (34, V.Repeated_Uint_64);
         WS.Write_Zigzag_Packed (35, V.Repeated_Sint_32);
         WS.Write_Zigzag_Packed (36, V.Repeated_Sint_64);
         WS.Write_Fixed_Packed (37, V.Repeated_Fixed_32);
         WS.Write_Fixed_Packed (38, V.Repeated_Fixed_64);
         WS.Write_Fixed_Packed (39, V.Repeated_Sfixed_32);
         WS.Write_Fixed_Packed (40, V.Repeated_Sfixed_64);
         WS.Write_Packed (41, V.Repeated_Float);
         WS.Write_Packed (42, V.Repeated_Double);
         WS.Write_Packed (43, V.Repeated_Bool);
         WS.Write (44, V.Repeated_String);
         WS.Write (45, V.Repeated_Bytes);
         for J in 1 .. V.Repeated_Nested_Message.Length loop
            WS.Write_Key ((48, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
              .Nested_Message'Write
              (Stream, V.Repeated_Nested_Message (J));
         end loop;
         for J in 1 .. V.Repeated_Foreign_Message.Length loop
            WS.Write_Key ((49, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
              .Foreign_Message'Write
              (Stream, V.Repeated_Foreign_Message (J));
         end loop;
         Nested_Enum_IO.Write_Packed (WS, 51, V.Repeated_Nested_Enum);
         Foreign_Enum_IO.Write_Packed (WS, 52, V.Repeated_Foreign_Enum);
         WS.Write (54, V.Repeated_String_Piece);
         WS.Write (55, V.Repeated_Cord);
         WS.Write_Varint_Packed (75, V.Packed_Int_32);
         WS.Write_Varint_Packed (76, V.Packed_Int_64);
         WS.Write_Varint_Packed (77, V.Packed_Uint_32);
         WS.Write_Varint_Packed (78, V.Packed_Uint_64);
         WS.Write_Zigzag_Packed (79, V.Packed_Sint_32);
         WS.Write_Zigzag_Packed (80, V.Packed_Sint_64);
         WS.Write_Fixed_Packed (81, V.Packed_Fixed_32);
         WS.Write_Fixed_Packed (82, V.Packed_Fixed_64);
         WS.Write_Fixed_Packed (83, V.Packed_Sfixed_32);
         WS.Write_Fixed_Packed (84, V.Packed_Sfixed_64);
         WS.Write_Packed (85, V.Packed_Float);
         WS.Write_Packed (86, V.Packed_Double);
         WS.Write_Packed (87, V.Packed_Bool);
         Nested_Enum_IO.Write_Packed (WS, 88, V.Packed_Nested_Enum);
         WS.Write_Varint (89, V.Unpacked_Int_32);
         WS.Write_Varint (90, V.Unpacked_Int_64);
         WS.Write_Varint (91, V.Unpacked_Uint_32);
         WS.Write_Varint (92, V.Unpacked_Uint_64);
         WS.Write_Zigzag (93, V.Unpacked_Sint_32);
         WS.Write_Zigzag (94, V.Unpacked_Sint_64);
         WS.Write_Fixed (95, V.Unpacked_Fixed_32);
         WS.Write_Fixed (96, V.Unpacked_Fixed_64);
         WS.Write_Fixed (97, V.Unpacked_Sfixed_32);
         WS.Write_Fixed (98, V.Unpacked_Sfixed_64);
         WS.Write (99, V.Unpacked_Float);
         WS.Write (100, V.Unpacked_Double);
         WS.Write (101, V.Unpacked_Bool);
         Nested_Enum_IO.Write (WS, 102, V.Unpacked_Nested_Enum);
         for J in 1 .. V.Map_Int_32_Int_32.Length loop
            WS.Write_Key ((56, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
              .Map_Int_32Int_32Entry'Write
              (Stream, V.Map_Int_32_Int_32 (J));
         end loop;
         for J in 1 .. V.Map_Int_64_Int_64.Length loop
            WS.Write_Key ((57, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
              .Map_Int_64Int_64Entry'Write
              (Stream, V.Map_Int_64_Int_64 (J));
         end loop;
         for J in 1 .. V.Map_Uint_32_Uint_32.Length loop
            WS.Write_Key ((58, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
              .Map_Uint_32Uint_32Entry'Write
              (Stream, V.Map_Uint_32_Uint_32 (J));
         end loop;
         for J in 1 .. V.Map_Uint_64_Uint_64.Length loop
            WS.Write_Key ((59, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
              .Map_Uint_64Uint_64Entry'Write
              (Stream, V.Map_Uint_64_Uint_64 (J));
         end loop;
         for J in 1 .. V.Map_Sint_32_Sint_32.Length loop
            WS.Write_Key ((60, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
              .Map_Sint_32Sint_32Entry'Write
              (Stream, V.Map_Sint_32_Sint_32 (J));
         end loop;
         for J in 1 .. V.Map_Sint_64_Sint_64.Length loop
            WS.Write_Key ((61, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
              .Map_Sint_64Sint_64Entry'Write
              (Stream, V.Map_Sint_64_Sint_64 (J));
         end loop;
         for J in 1 .. V.Map_Fixed_32_Fixed_32.Length loop
            WS.Write_Key ((62, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
              .Map_Fixed_32Fixed_32Entry'Write
              (Stream, V.Map_Fixed_32_Fixed_32 (J));
         end loop;
         for J in 1 .. V.Map_Fixed_64_Fixed_64.Length loop
            WS.Write_Key ((63, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
              .Map_Fixed_64Fixed_64Entry'Write
              (Stream, V.Map_Fixed_64_Fixed_64 (J));
         end loop;
         for J in 1 .. V.Map_Sfixed_32_Sfixed_32.Length loop
            WS.Write_Key ((64, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
              .Map_Sfixed_32Sfixed_32Entry'Write
              (Stream, V.Map_Sfixed_32_Sfixed_32 (J));
         end loop;
         for J in 1 .. V.Map_Sfixed_64_Sfixed_64.Length loop
            WS.Write_Key ((65, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
              .Map_Sfixed_64Sfixed_64Entry'Write
              (Stream, V.Map_Sfixed_64_Sfixed_64 (J));
         end loop;
         for J in 1 .. V.Map_Int_32_Float.Length loop
            WS.Write_Key ((66, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
              .Map_Int_32Float_Entry'Write
              (Stream, V.Map_Int_32_Float (J));
         end loop;
         for J in 1 .. V.Map_Int_32_Double.Length loop
            WS.Write_Key ((67, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
              .Map_Int_32Double_Entry'Write
              (Stream, V.Map_Int_32_Double (J));
         end loop;
         for J in 1 .. V.Map_Bool_Bool.Length loop
            WS.Write_Key ((68, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
              .Map_Bool_Bool_Entry'Write
              (Stream, V.Map_Bool_Bool (J));
         end loop;
         for J in 1 .. V.Map_String_String.Length loop
            WS.Write_Key ((69, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
              .Map_String_String_Entry'Write
              (Stream, V.Map_String_String (J));
         end loop;
         for J in 1 .. V.Map_String_Bytes.Length loop
            WS.Write_Key ((70, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
              .Map_String_Bytes_Entry'Write
              (Stream, V.Map_String_Bytes (J));
         end loop;
         for J in 1 .. V.Map_String_Nested_Message.Length loop
            WS.Write_Key ((71, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
              .Map_String_Nested_Message_Entry'Write
              (Stream, V.Map_String_Nested_Message (J));
         end loop;
         for J in 1 .. V.Map_String_Foreign_Message.Length loop
            WS.Write_Key ((72, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
              .Map_String_Foreign_Message_Entry'Write
              (Stream, V.Map_String_Foreign_Message (J));
         end loop;
         for J in 1 .. V.Map_String_Nested_Enum.Length loop
            WS.Write_Key ((73, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
              .Map_String_Nested_Enum_Entry'Write
              (Stream, V.Map_String_Nested_Enum (J));
         end loop;
         for J in 1 .. V.Map_String_Foreign_Enum.Length loop
            WS.Write_Key ((74, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
              .Map_String_Foreign_Enum_Entry'Write
              (Stream, V.Map_String_Foreign_Enum (J));
         end loop;
         if V.Optional_Bool_Wrapper.Is_Set then
            WS.Write_Key ((201, PB_Support.Length_Delimited));
            Google.Protobuf.Wrappers.Bool_Value'Write
              (Stream, V.Optional_Bool_Wrapper.Value);
         end if;
         if V.Optional_Int_32_Wrapper.Is_Set then
            WS.Write_Key ((202, PB_Support.Length_Delimited));
            Google.Protobuf.Wrappers.Int_32Value'Write
              (Stream, V.Optional_Int_32_Wrapper.Value);
         end if;
         if V.Optional_Int_64_Wrapper.Is_Set then
            WS.Write_Key ((203, PB_Support.Length_Delimited));
            Google.Protobuf.Wrappers.Int_64Value'Write
              (Stream, V.Optional_Int_64_Wrapper.Value);
         end if;
         if V.Optional_Uint_32_Wrapper.Is_Set then
            WS.Write_Key ((204, PB_Support.Length_Delimited));
            Google.Protobuf.Wrappers.UInt_32Value'Write
              (Stream, V.Optional_Uint_32_Wrapper.Value);
         end if;
         if V.Optional_Uint_64_Wrapper.Is_Set then
            WS.Write_Key ((205, PB_Support.Length_Delimited));
            Google.Protobuf.Wrappers.UInt_64Value'Write
              (Stream, V.Optional_Uint_64_Wrapper.Value);
         end if;
         if V.Optional_Float_Wrapper.Is_Set then
            WS.Write_Key ((206, PB_Support.Length_Delimited));
            Google.Protobuf.Wrappers.Float_Value'Write
              (Stream, V.Optional_Float_Wrapper.Value);
         end if;
         if V.Optional_Double_Wrapper.Is_Set then
            WS.Write_Key ((207, PB_Support.Length_Delimited));
            Google.Protobuf.Wrappers.Double_Value'Write
              (Stream, V.Optional_Double_Wrapper.Value);
         end if;
         if V.Optional_String_Wrapper.Is_Set then
            WS.Write_Key ((208, PB_Support.Length_Delimited));
            Google.Protobuf.Wrappers.String_Value'Write
              (Stream, V.Optional_String_Wrapper.Value);
         end if;
         if V.Optional_Bytes_Wrapper.Is_Set then
            WS.Write_Key ((209, PB_Support.Length_Delimited));
            Google.Protobuf.Wrappers.Bytes_Value'Write
              (Stream, V.Optional_Bytes_Wrapper.Value);
         end if;
         for J in 1 .. V.Repeated_Bool_Wrapper.Length loop
            WS.Write_Key ((211, PB_Support.Length_Delimited));
            Google.Protobuf.Wrappers.Bool_Value'Write
              (Stream, V.Repeated_Bool_Wrapper (J));
         end loop;
         for J in 1 .. V.Repeated_Int_32_Wrapper.Length loop
            WS.Write_Key ((212, PB_Support.Length_Delimited));
            Google.Protobuf.Wrappers.Int_32Value'Write
              (Stream, V.Repeated_Int_32_Wrapper (J));
         end loop;
         for J in 1 .. V.Repeated_Int_64_Wrapper.Length loop
            WS.Write_Key ((213, PB_Support.Length_Delimited));
            Google.Protobuf.Wrappers.Int_64Value'Write
              (Stream, V.Repeated_Int_64_Wrapper (J));
         end loop;
         for J in 1 .. V.Repeated_Uint_32_Wrapper.Length loop
            WS.Write_Key ((214, PB_Support.Length_Delimited));
            Google.Protobuf.Wrappers.UInt_32Value'Write
              (Stream, V.Repeated_Uint_32_Wrapper (J));
         end loop;
         for J in 1 .. V.Repeated_Uint_64_Wrapper.Length loop
            WS.Write_Key ((215, PB_Support.Length_Delimited));
            Google.Protobuf.Wrappers.UInt_64Value'Write
              (Stream, V.Repeated_Uint_64_Wrapper (J));
         end loop;
         for J in 1 .. V.Repeated_Float_Wrapper.Length loop
            WS.Write_Key ((216, PB_Support.Length_Delimited));
            Google.Protobuf.Wrappers.Float_Value'Write
              (Stream, V.Repeated_Float_Wrapper (J));
         end loop;
         for J in 1 .. V.Repeated_Double_Wrapper.Length loop
            WS.Write_Key ((217, PB_Support.Length_Delimited));
            Google.Protobuf.Wrappers.Double_Value'Write
              (Stream, V.Repeated_Double_Wrapper (J));
         end loop;
         for J in 1 .. V.Repeated_String_Wrapper.Length loop
            WS.Write_Key ((218, PB_Support.Length_Delimited));
            Google.Protobuf.Wrappers.String_Value'Write
              (Stream, V.Repeated_String_Wrapper (J));
         end loop;
         for J in 1 .. V.Repeated_Bytes_Wrapper.Length loop
            WS.Write_Key ((219, PB_Support.Length_Delimited));
            Google.Protobuf.Wrappers.Bytes_Value'Write
              (Stream, V.Repeated_Bytes_Wrapper (J));
         end loop;
         if V.Optional_Duration.Is_Set then
            WS.Write_Key ((301, PB_Support.Length_Delimited));
            Google.Protobuf.Duration.Duration'Write
              (Stream, V.Optional_Duration.Value);
         end if;
         if V.Optional_Timestamp.Is_Set then
            WS.Write_Key ((302, PB_Support.Length_Delimited));
            Google.Protobuf.Timestamp.Timestamp'Write
              (Stream, V.Optional_Timestamp.Value);
         end if;
         if V.Optional_Field_Mask.Is_Set then
            WS.Write_Key ((303, PB_Support.Length_Delimited));
            Google.Protobuf.Field_Mask.Field_Mask'Write
              (Stream, V.Optional_Field_Mask.Value);
         end if;
         if V.Optional_Struct.Is_Set then
            WS.Write_Key ((304, PB_Support.Length_Delimited));
            Google.Protobuf.Struct.Struct'Write
              (Stream, V.Optional_Struct.Value);
         end if;
         if V.Optional_Any.Is_Set then
            WS.Write_Key ((305, PB_Support.Length_Delimited));
            Google.Protobuf.Any.Any'Write (Stream, V.Optional_Any.Value);
         end if;
         if V.Optional_Value.Is_Set then
            WS.Write_Key ((306, PB_Support.Length_Delimited));
            Google.Protobuf.Struct.Value'Write
              (Stream, V.Optional_Value.Value);
         end if;
         for J in 1 .. V.Repeated_Duration.Length loop
            WS.Write_Key ((311, PB_Support.Length_Delimited));
            Google.Protobuf.Duration.Duration'Write
              (Stream, V.Repeated_Duration (J));
         end loop;
         for J in 1 .. V.Repeated_Timestamp.Length loop
            WS.Write_Key ((312, PB_Support.Length_Delimited));
            Google.Protobuf.Timestamp.Timestamp'Write
              (Stream, V.Repeated_Timestamp (J));
         end loop;
         for J in 1 .. V.Repeated_Fieldmask.Length loop
            WS.Write_Key ((313, PB_Support.Length_Delimited));
            Google.Protobuf.Field_Mask.Field_Mask'Write
              (Stream, V.Repeated_Fieldmask (J));
         end loop;
         for J in 1 .. V.Repeated_Struct.Length loop
            WS.Write_Key ((324, PB_Support.Length_Delimited));
            Google.Protobuf.Struct.Struct'Write
              (Stream, V.Repeated_Struct (J));
         end loop;
         for J in 1 .. V.Repeated_Any.Length loop
            WS.Write_Key ((315, PB_Support.Length_Delimited));
            Google.Protobuf.Any.Any'Write (Stream, V.Repeated_Any (J));
         end loop;
         for J in 1 .. V.Repeated_Value.Length loop
            WS.Write_Key ((316, PB_Support.Length_Delimited));
            Google.Protobuf.Struct.Value'Write (Stream, V.Repeated_Value (J));
         end loop;
         for J in 1 .. V.Repeated_List_Value.Length loop
            WS.Write_Key ((317, PB_Support.Length_Delimited));
            Google.Protobuf.Struct.List_Value'Write
              (Stream, V.Repeated_List_Value (J));
         end loop;
         WS.Write_Varint_Option (401, V.Fieldname_1, 0);
         WS.Write_Varint_Option (402, V.Field_Name_2, 0);
         WS.Write_Varint_Option (403, V.Field_Name_3, 0);
         WS.Write_Varint_Option (404, V.Field_Name_4, 0);
         WS.Write_Varint_Option (405, V.Field_0name_5, 0);
         WS.Write_Varint_Option (406, V.Field_0_Name_6, 0);
         WS.Write_Varint_Option (407, V.Field_Name_7, 0);
         WS.Write_Varint_Option (408, V.Field_Name_8, 0);
         WS.Write_Varint_Option (409, V.Field_Name_9, 0);
         WS.Write_Varint_Option (410, V.Field_Name_10, 0);
         WS.Write_Varint_Option (411, V.FIELD_NAME11, 0);
         WS.Write_Varint_Option (412, V.FIELD_Name_12, 0);
         WS.Write_Varint_Option (413, V.Field_Name_13, 0);
         WS.Write_Varint_Option (414, V.Field_Name_14, 0);
         WS.Write_Varint_Option (415, V.Field_Name_15, 0);
         WS.Write_Varint_Option (416, V.Field_Name_16, 0);
         WS.Write_Varint_Option (417, V.Field_Name_17, 0);
         WS.Write_Varint_Option (418, V.Field_Name_18, 0);
         case V.Variant.Oneof_Field is
            when Oneof_Uint_32_Kind =>
               WS.Write_Varint (111, V.Variant.Oneof_Uint_32);
            when Oneof_Nested_Message_Kind =>
               WS.Write_Key ((112, PB_Support.Length_Delimited));
               Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
                 .Nested_Message'Write
                 (Stream, V.Variant.Oneof_Nested_Message);
            when Oneof_String_Kind =>
               WS.Write (113, V.Variant.Oneof_String);
            when Oneof_Bytes_Kind =>
               WS.Write (114, V.Variant.Oneof_Bytes);
            when Oneof_Bool_Kind =>
               WS.Write (115, V.Variant.Oneof_Bool);
            when Oneof_Uint_64_Kind =>
               WS.Write_Varint (116, V.Variant.Oneof_Uint_64);
            when Oneof_Float_Kind =>
               WS.Write (117, V.Variant.Oneof_Float);
            when Oneof_Double_Kind =>
               WS.Write (118, V.Variant.Oneof_Double);
            when Oneof_Enum_Kind =>
               Nested_Enum_IO.Write (WS, 119, V.Variant.Oneof_Enum);
            when Oneof_Field_Not_Set =>
               null;
         end case;
         if WS.End_Message then
            Write_Test_All_Types_Proto_3 (WS'Access, V);
         end if;
      end;
   end Write_Test_All_Types_Proto_3;

   function Length (Self : Nested_Message_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Nested_Message_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Nested_Message_Array, Nested_Message_Array_Access);

   procedure Append
    (Self : in out Nested_Message_Vector;
     V    : Nested_Message) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Nested_Message'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Nested_Message_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Nested_Message_Array'
             (Self.Data.all & Nested_Message_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Nested_Message_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Nested_Message_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Nested_Message_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Nested_Message_Variable_Reference
    (Self  : aliased in out Nested_Message_Vector;
     Index : Positive)
      return Nested_Message_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Nested_Message_Variable_Reference;

   not overriding function Get_Nested_Message_Constant_Reference
    (Self  : aliased Nested_Message_Vector;
     Index : Positive)
      return Nested_Message_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Nested_Message_Constant_Reference;

   procedure Read_Nested_Message
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Nested_Message) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.A);
            when 2 =>
               if V.Corecursive.Length = 0 then
                  V.Corecursive.Append ((others => <>));
               end if;
               Test_All_Types_Proto_3_IO.Read
                 (Stream, Key.Encoding, V.Corecursive (1));
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Nested_Message;

   procedure Write_Nested_Message
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Nested_Message) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Nested_Message (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Varint_Option (1, V.A, 0);
         for J in 1 .. V.Corecursive.Length loop
            WS.Write_Key ((2, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
              .Test_All_Types_Proto_3'Write
              (Stream, V.Corecursive (J));
         end loop;
         if WS.End_Message then
            Write_Nested_Message (WS'Access, V);
         end if;
      end;
   end Write_Nested_Message;

   function Length (Self : Map_Int_32Int_32Entry_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Map_Int_32Int_32Entry_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Map_Int_32Int_32Entry_Array, Map_Int_32Int_32Entry_Array_Access);

   procedure Append
    (Self : in out Map_Int_32Int_32Entry_Vector;
     V    : Map_Int_32Int_32Entry) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Map_Int_32Int_32Entry'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Map_Int_32Int_32Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Map_Int_32Int_32Entry_Array'
             (Self.Data.all
                & Map_Int_32Int_32Entry_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Map_Int_32Int_32Entry_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Map_Int_32Int_32Entry_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Map_Int_32Int_32Entry_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Map_Int_32Int_32Entry_Variable_Reference
    (Self  : aliased in out Map_Int_32Int_32Entry_Vector;
     Index : Positive)
      return Map_Int_32Int_32Entry_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Int_32Int_32Entry_Variable_Reference;

   not overriding function Get_Map_Int_32Int_32Entry_Constant_Reference
    (Self  : aliased Map_Int_32Int_32Entry_Vector;
     Index : Positive)
      return Map_Int_32Int_32Entry_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Int_32Int_32Entry_Constant_Reference;

   procedure Read_Map_Int_32Int_32Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Int_32Int_32Entry) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.Key);
            when 2 =>
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Int_32Int_32Entry;

   procedure Write_Map_Int_32Int_32Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Int_32Int_32Entry) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Map_Int_32Int_32Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Varint_Option (1, V.Key, 0);
         WS.Write_Varint_Option (2, V.Value, 0);
         if WS.End_Message then
            Write_Map_Int_32Int_32Entry (WS'Access, V);
         end if;
      end;
   end Write_Map_Int_32Int_32Entry;

   function Length (Self : Map_Int_64Int_64Entry_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Map_Int_64Int_64Entry_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Map_Int_64Int_64Entry_Array, Map_Int_64Int_64Entry_Array_Access);

   procedure Append
    (Self : in out Map_Int_64Int_64Entry_Vector;
     V    : Map_Int_64Int_64Entry) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Map_Int_64Int_64Entry'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Map_Int_64Int_64Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Map_Int_64Int_64Entry_Array'
             (Self.Data.all
                & Map_Int_64Int_64Entry_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Map_Int_64Int_64Entry_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Map_Int_64Int_64Entry_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Map_Int_64Int_64Entry_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Map_Int_64Int_64Entry_Variable_Reference
    (Self  : aliased in out Map_Int_64Int_64Entry_Vector;
     Index : Positive)
      return Map_Int_64Int_64Entry_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Int_64Int_64Entry_Variable_Reference;

   not overriding function Get_Map_Int_64Int_64Entry_Constant_Reference
    (Self  : aliased Map_Int_64Int_64Entry_Vector;
     Index : Positive)
      return Map_Int_64Int_64Entry_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Int_64Int_64Entry_Constant_Reference;

   procedure Read_Map_Int_64Int_64Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Int_64Int_64Entry) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.Key);
            when 2 =>
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Int_64Int_64Entry;

   procedure Write_Map_Int_64Int_64Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Int_64Int_64Entry) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Map_Int_64Int_64Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Varint_Option (1, V.Key, 0);
         WS.Write_Varint_Option (2, V.Value, 0);
         if WS.End_Message then
            Write_Map_Int_64Int_64Entry (WS'Access, V);
         end if;
      end;
   end Write_Map_Int_64Int_64Entry;

   function Length (Self : Map_Uint_32Uint_32Entry_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Map_Uint_32Uint_32Entry_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Map_Uint_32Uint_32Entry_Array, Map_Uint_32Uint_32Entry_Array_Access);

   procedure Append
    (Self : in out Map_Uint_32Uint_32Entry_Vector;
     V    : Map_Uint_32Uint_32Entry) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Map_Uint_32Uint_32Entry'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Map_Uint_32Uint_32Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Map_Uint_32Uint_32Entry_Array'
             (Self.Data.all
                & Map_Uint_32Uint_32Entry_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Map_Uint_32Uint_32Entry_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Map_Uint_32Uint_32Entry_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Map_Uint_32Uint_32Entry_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Map_Uint_32Uint_32Entry_Variable_Reference
    (Self  : aliased in out Map_Uint_32Uint_32Entry_Vector;
     Index : Positive)
      return Map_Uint_32Uint_32Entry_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Uint_32Uint_32Entry_Variable_Reference;

   not overriding function Get_Map_Uint_32Uint_32Entry_Constant_Reference
    (Self  : aliased Map_Uint_32Uint_32Entry_Vector;
     Index : Positive)
      return Map_Uint_32Uint_32Entry_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Uint_32Uint_32Entry_Constant_Reference;

   procedure Read_Map_Uint_32Uint_32Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Uint_32Uint_32Entry) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.Key);
            when 2 =>
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Uint_32Uint_32Entry;

   procedure Write_Map_Uint_32Uint_32Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Uint_32Uint_32Entry) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Map_Uint_32Uint_32Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Varint_Option (1, V.Key, 0);
         WS.Write_Varint_Option (2, V.Value, 0);
         if WS.End_Message then
            Write_Map_Uint_32Uint_32Entry (WS'Access, V);
         end if;
      end;
   end Write_Map_Uint_32Uint_32Entry;

   function Length (Self : Map_Uint_64Uint_64Entry_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Map_Uint_64Uint_64Entry_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Map_Uint_64Uint_64Entry_Array, Map_Uint_64Uint_64Entry_Array_Access);

   procedure Append
    (Self : in out Map_Uint_64Uint_64Entry_Vector;
     V    : Map_Uint_64Uint_64Entry) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Map_Uint_64Uint_64Entry'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Map_Uint_64Uint_64Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Map_Uint_64Uint_64Entry_Array'
             (Self.Data.all
                & Map_Uint_64Uint_64Entry_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Map_Uint_64Uint_64Entry_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Map_Uint_64Uint_64Entry_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Map_Uint_64Uint_64Entry_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Map_Uint_64Uint_64Entry_Variable_Reference
    (Self  : aliased in out Map_Uint_64Uint_64Entry_Vector;
     Index : Positive)
      return Map_Uint_64Uint_64Entry_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Uint_64Uint_64Entry_Variable_Reference;

   not overriding function Get_Map_Uint_64Uint_64Entry_Constant_Reference
    (Self  : aliased Map_Uint_64Uint_64Entry_Vector;
     Index : Positive)
      return Map_Uint_64Uint_64Entry_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Uint_64Uint_64Entry_Constant_Reference;

   procedure Read_Map_Uint_64Uint_64Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Uint_64Uint_64Entry) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.Key);
            when 2 =>
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Uint_64Uint_64Entry;

   procedure Write_Map_Uint_64Uint_64Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Uint_64Uint_64Entry) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Map_Uint_64Uint_64Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Varint_Option (1, V.Key, 0);
         WS.Write_Varint_Option (2, V.Value, 0);
         if WS.End_Message then
            Write_Map_Uint_64Uint_64Entry (WS'Access, V);
         end if;
      end;
   end Write_Map_Uint_64Uint_64Entry;

   function Length (Self : Map_Sint_32Sint_32Entry_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Map_Sint_32Sint_32Entry_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Map_Sint_32Sint_32Entry_Array, Map_Sint_32Sint_32Entry_Array_Access);

   procedure Append
    (Self : in out Map_Sint_32Sint_32Entry_Vector;
     V    : Map_Sint_32Sint_32Entry) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Map_Sint_32Sint_32Entry'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Map_Sint_32Sint_32Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Map_Sint_32Sint_32Entry_Array'
             (Self.Data.all
                & Map_Sint_32Sint_32Entry_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Map_Sint_32Sint_32Entry_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Map_Sint_32Sint_32Entry_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Map_Sint_32Sint_32Entry_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Map_Sint_32Sint_32Entry_Variable_Reference
    (Self  : aliased in out Map_Sint_32Sint_32Entry_Vector;
     Index : Positive)
      return Map_Sint_32Sint_32Entry_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Sint_32Sint_32Entry_Variable_Reference;

   not overriding function Get_Map_Sint_32Sint_32Entry_Constant_Reference
    (Self  : aliased Map_Sint_32Sint_32Entry_Vector;
     Index : Positive)
      return Map_Sint_32Sint_32Entry_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Sint_32Sint_32Entry_Constant_Reference;

   procedure Read_Map_Sint_32Sint_32Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Sint_32Sint_32Entry) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Zigzag (Stream, Key.Encoding, V.Key);
            when 2 =>
               PB_Support.IO.Read_Zigzag (Stream, Key.Encoding, V.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Sint_32Sint_32Entry;

   procedure Write_Map_Sint_32Sint_32Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Sint_32Sint_32Entry) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Map_Sint_32Sint_32Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Zigzag_Option (1, V.Key, 0);
         WS.Write_Zigzag_Option (2, V.Value, 0);
         if WS.End_Message then
            Write_Map_Sint_32Sint_32Entry (WS'Access, V);
         end if;
      end;
   end Write_Map_Sint_32Sint_32Entry;

   function Length (Self : Map_Sint_64Sint_64Entry_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Map_Sint_64Sint_64Entry_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Map_Sint_64Sint_64Entry_Array, Map_Sint_64Sint_64Entry_Array_Access);

   procedure Append
    (Self : in out Map_Sint_64Sint_64Entry_Vector;
     V    : Map_Sint_64Sint_64Entry) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Map_Sint_64Sint_64Entry'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Map_Sint_64Sint_64Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Map_Sint_64Sint_64Entry_Array'
             (Self.Data.all
                & Map_Sint_64Sint_64Entry_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Map_Sint_64Sint_64Entry_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Map_Sint_64Sint_64Entry_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Map_Sint_64Sint_64Entry_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Map_Sint_64Sint_64Entry_Variable_Reference
    (Self  : aliased in out Map_Sint_64Sint_64Entry_Vector;
     Index : Positive)
      return Map_Sint_64Sint_64Entry_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Sint_64Sint_64Entry_Variable_Reference;

   not overriding function Get_Map_Sint_64Sint_64Entry_Constant_Reference
    (Self  : aliased Map_Sint_64Sint_64Entry_Vector;
     Index : Positive)
      return Map_Sint_64Sint_64Entry_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Sint_64Sint_64Entry_Constant_Reference;

   procedure Read_Map_Sint_64Sint_64Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Sint_64Sint_64Entry) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Zigzag (Stream, Key.Encoding, V.Key);
            when 2 =>
               PB_Support.IO.Read_Zigzag (Stream, Key.Encoding, V.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Sint_64Sint_64Entry;

   procedure Write_Map_Sint_64Sint_64Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Sint_64Sint_64Entry) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Map_Sint_64Sint_64Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Zigzag_Option (1, V.Key, 0);
         WS.Write_Zigzag_Option (2, V.Value, 0);
         if WS.End_Message then
            Write_Map_Sint_64Sint_64Entry (WS'Access, V);
         end if;
      end;
   end Write_Map_Sint_64Sint_64Entry;

   function Length (Self : Map_Fixed_32Fixed_32Entry_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Map_Fixed_32Fixed_32Entry_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Map_Fixed_32Fixed_32Entry_Array, Map_Fixed_32Fixed_32Entry_Array_Access);

   procedure Append
    (Self : in out Map_Fixed_32Fixed_32Entry_Vector;
     V    : Map_Fixed_32Fixed_32Entry) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Map_Fixed_32Fixed_32Entry'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Map_Fixed_32Fixed_32Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Map_Fixed_32Fixed_32Entry_Array'
             (Self.Data.all
                & Map_Fixed_32Fixed_32Entry_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Map_Fixed_32Fixed_32Entry_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Map_Fixed_32Fixed_32Entry_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Map_Fixed_32Fixed_32Entry_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Map_Fixed_32Fixed_32Entry_Variable_Reference
    (Self  : aliased in out Map_Fixed_32Fixed_32Entry_Vector;
     Index : Positive)
      return Map_Fixed_32Fixed_32Entry_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Fixed_32Fixed_32Entry_Variable_Reference;

   not overriding function Get_Map_Fixed_32Fixed_32Entry_Constant_Reference
    (Self  : aliased Map_Fixed_32Fixed_32Entry_Vector;
     Index : Positive)
      return Map_Fixed_32Fixed_32Entry_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Fixed_32Fixed_32Entry_Constant_Reference;

   procedure Read_Map_Fixed_32Fixed_32Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Fixed_32Fixed_32Entry) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Fixed (Stream, Key.Encoding, V.Key);
            when 2 =>
               PB_Support.IO.Read_Fixed (Stream, Key.Encoding, V.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Fixed_32Fixed_32Entry;

   procedure Write_Map_Fixed_32Fixed_32Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Fixed_32Fixed_32Entry) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Map_Fixed_32Fixed_32Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Fixed_Option (1, V.Key, 0);
         WS.Write_Fixed_Option (2, V.Value, 0);
         if WS.End_Message then
            Write_Map_Fixed_32Fixed_32Entry (WS'Access, V);
         end if;
      end;
   end Write_Map_Fixed_32Fixed_32Entry;

   function Length (Self : Map_Fixed_64Fixed_64Entry_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Map_Fixed_64Fixed_64Entry_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Map_Fixed_64Fixed_64Entry_Array, Map_Fixed_64Fixed_64Entry_Array_Access);

   procedure Append
    (Self : in out Map_Fixed_64Fixed_64Entry_Vector;
     V    : Map_Fixed_64Fixed_64Entry) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Map_Fixed_64Fixed_64Entry'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Map_Fixed_64Fixed_64Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Map_Fixed_64Fixed_64Entry_Array'
             (Self.Data.all
                & Map_Fixed_64Fixed_64Entry_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Map_Fixed_64Fixed_64Entry_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Map_Fixed_64Fixed_64Entry_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Map_Fixed_64Fixed_64Entry_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Map_Fixed_64Fixed_64Entry_Variable_Reference
    (Self  : aliased in out Map_Fixed_64Fixed_64Entry_Vector;
     Index : Positive)
      return Map_Fixed_64Fixed_64Entry_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Fixed_64Fixed_64Entry_Variable_Reference;

   not overriding function Get_Map_Fixed_64Fixed_64Entry_Constant_Reference
    (Self  : aliased Map_Fixed_64Fixed_64Entry_Vector;
     Index : Positive)
      return Map_Fixed_64Fixed_64Entry_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Fixed_64Fixed_64Entry_Constant_Reference;

   procedure Read_Map_Fixed_64Fixed_64Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Fixed_64Fixed_64Entry) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Fixed (Stream, Key.Encoding, V.Key);
            when 2 =>
               PB_Support.IO.Read_Fixed (Stream, Key.Encoding, V.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Fixed_64Fixed_64Entry;

   procedure Write_Map_Fixed_64Fixed_64Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Fixed_64Fixed_64Entry) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Map_Fixed_64Fixed_64Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Fixed_Option (1, V.Key, 0);
         WS.Write_Fixed_Option (2, V.Value, 0);
         if WS.End_Message then
            Write_Map_Fixed_64Fixed_64Entry (WS'Access, V);
         end if;
      end;
   end Write_Map_Fixed_64Fixed_64Entry;

   function Length
    (Self : Map_Sfixed_32Sfixed_32Entry_Vector)
      return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Map_Sfixed_32Sfixed_32Entry_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Map_Sfixed_32Sfixed_32Entry_Array,
      Map_Sfixed_32Sfixed_32Entry_Array_Access);

   procedure Append
    (Self : in out Map_Sfixed_32Sfixed_32Entry_Vector;
     V    : Map_Sfixed_32Sfixed_32Entry) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Map_Sfixed_32Sfixed_32Entry'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=
            new Map_Sfixed_32Sfixed_32Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Map_Sfixed_32Sfixed_32Entry_Array'
             (Self.Data.all
                & Map_Sfixed_32Sfixed_32Entry_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Map_Sfixed_32Sfixed_32Entry_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Map_Sfixed_32Sfixed_32Entry_Array'
             (Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Map_Sfixed_32Sfixed_32Entry_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Map_Sfixed_32Sfixed_32Entry_Variable_Reference
    (Self  : aliased in out Map_Sfixed_32Sfixed_32Entry_Vector;
     Index : Positive)
      return Map_Sfixed_32Sfixed_32Entry_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Sfixed_32Sfixed_32Entry_Variable_Reference;

   not overriding function Get_Map_Sfixed_32Sfixed_32Entry_Constant_Reference
    (Self  : aliased Map_Sfixed_32Sfixed_32Entry_Vector;
     Index : Positive)
      return Map_Sfixed_32Sfixed_32Entry_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Sfixed_32Sfixed_32Entry_Constant_Reference;

   procedure Read_Map_Sfixed_32Sfixed_32Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Sfixed_32Sfixed_32Entry) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Fixed (Stream, Key.Encoding, V.Key);
            when 2 =>
               PB_Support.IO.Read_Fixed (Stream, Key.Encoding, V.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Sfixed_32Sfixed_32Entry;

   procedure Write_Map_Sfixed_32Sfixed_32Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Sfixed_32Sfixed_32Entry) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Map_Sfixed_32Sfixed_32Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Fixed_Option (1, V.Key, 0);
         WS.Write_Fixed_Option (2, V.Value, 0);
         if WS.End_Message then
            Write_Map_Sfixed_32Sfixed_32Entry (WS'Access, V);
         end if;
      end;
   end Write_Map_Sfixed_32Sfixed_32Entry;

   function Length
    (Self : Map_Sfixed_64Sfixed_64Entry_Vector)
      return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Map_Sfixed_64Sfixed_64Entry_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Map_Sfixed_64Sfixed_64Entry_Array,
      Map_Sfixed_64Sfixed_64Entry_Array_Access);

   procedure Append
    (Self : in out Map_Sfixed_64Sfixed_64Entry_Vector;
     V    : Map_Sfixed_64Sfixed_64Entry) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Map_Sfixed_64Sfixed_64Entry'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=
            new Map_Sfixed_64Sfixed_64Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Map_Sfixed_64Sfixed_64Entry_Array'
             (Self.Data.all
                & Map_Sfixed_64Sfixed_64Entry_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Map_Sfixed_64Sfixed_64Entry_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Map_Sfixed_64Sfixed_64Entry_Array'
             (Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Map_Sfixed_64Sfixed_64Entry_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Map_Sfixed_64Sfixed_64Entry_Variable_Reference
    (Self  : aliased in out Map_Sfixed_64Sfixed_64Entry_Vector;
     Index : Positive)
      return Map_Sfixed_64Sfixed_64Entry_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Sfixed_64Sfixed_64Entry_Variable_Reference;

   not overriding function Get_Map_Sfixed_64Sfixed_64Entry_Constant_Reference
    (Self  : aliased Map_Sfixed_64Sfixed_64Entry_Vector;
     Index : Positive)
      return Map_Sfixed_64Sfixed_64Entry_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Sfixed_64Sfixed_64Entry_Constant_Reference;

   procedure Read_Map_Sfixed_64Sfixed_64Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Sfixed_64Sfixed_64Entry) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Fixed (Stream, Key.Encoding, V.Key);
            when 2 =>
               PB_Support.IO.Read_Fixed (Stream, Key.Encoding, V.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Sfixed_64Sfixed_64Entry;

   procedure Write_Map_Sfixed_64Sfixed_64Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Sfixed_64Sfixed_64Entry) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Map_Sfixed_64Sfixed_64Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Fixed_Option (1, V.Key, 0);
         WS.Write_Fixed_Option (2, V.Value, 0);
         if WS.End_Message then
            Write_Map_Sfixed_64Sfixed_64Entry (WS'Access, V);
         end if;
      end;
   end Write_Map_Sfixed_64Sfixed_64Entry;

   function Length (Self : Map_Int_32Float_Entry_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Map_Int_32Float_Entry_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Map_Int_32Float_Entry_Array, Map_Int_32Float_Entry_Array_Access);

   procedure Append
    (Self : in out Map_Int_32Float_Entry_Vector;
     V    : Map_Int_32Float_Entry) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Map_Int_32Float_Entry'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Map_Int_32Float_Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Map_Int_32Float_Entry_Array'
             (Self.Data.all
                & Map_Int_32Float_Entry_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Map_Int_32Float_Entry_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Map_Int_32Float_Entry_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Map_Int_32Float_Entry_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Map_Int_32Float_Entry_Variable_Reference
    (Self  : aliased in out Map_Int_32Float_Entry_Vector;
     Index : Positive)
      return Map_Int_32Float_Entry_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Int_32Float_Entry_Variable_Reference;

   not overriding function Get_Map_Int_32Float_Entry_Constant_Reference
    (Self  : aliased Map_Int_32Float_Entry_Vector;
     Index : Positive)
      return Map_Int_32Float_Entry_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Int_32Float_Entry_Constant_Reference;

   procedure Read_Map_Int_32Float_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Int_32Float_Entry) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.Key);
            when 2 =>
               PB_Support.IO.Read (Stream, Key.Encoding, V.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Int_32Float_Entry;

   procedure Write_Map_Int_32Float_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Int_32Float_Entry) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Map_Int_32Float_Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Varint_Option (1, V.Key, 0);
         WS.Write_Option (2, V.Value, 0.0);
         if WS.End_Message then
            Write_Map_Int_32Float_Entry (WS'Access, V);
         end if;
      end;
   end Write_Map_Int_32Float_Entry;

   function Length (Self : Map_Int_32Double_Entry_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Map_Int_32Double_Entry_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Map_Int_32Double_Entry_Array, Map_Int_32Double_Entry_Array_Access);

   procedure Append
    (Self : in out Map_Int_32Double_Entry_Vector;
     V    : Map_Int_32Double_Entry) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Map_Int_32Double_Entry'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Map_Int_32Double_Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Map_Int_32Double_Entry_Array'
             (Self.Data.all
                & Map_Int_32Double_Entry_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Map_Int_32Double_Entry_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Map_Int_32Double_Entry_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Map_Int_32Double_Entry_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Map_Int_32Double_Entry_Variable_Reference
    (Self  : aliased in out Map_Int_32Double_Entry_Vector;
     Index : Positive)
      return Map_Int_32Double_Entry_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Int_32Double_Entry_Variable_Reference;

   not overriding function Get_Map_Int_32Double_Entry_Constant_Reference
    (Self  : aliased Map_Int_32Double_Entry_Vector;
     Index : Positive)
      return Map_Int_32Double_Entry_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Int_32Double_Entry_Constant_Reference;

   procedure Read_Map_Int_32Double_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Int_32Double_Entry) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.Key);
            when 2 =>
               PB_Support.IO.Read (Stream, Key.Encoding, V.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Int_32Double_Entry;

   procedure Write_Map_Int_32Double_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Int_32Double_Entry) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Map_Int_32Double_Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Varint_Option (1, V.Key, 0);
         WS.Write_Option (2, V.Value, 0.0);
         if WS.End_Message then
            Write_Map_Int_32Double_Entry (WS'Access, V);
         end if;
      end;
   end Write_Map_Int_32Double_Entry;

   function Length (Self : Map_Bool_Bool_Entry_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Map_Bool_Bool_Entry_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Map_Bool_Bool_Entry_Array, Map_Bool_Bool_Entry_Array_Access);

   procedure Append
    (Self : in out Map_Bool_Bool_Entry_Vector;
     V    : Map_Bool_Bool_Entry) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Map_Bool_Bool_Entry'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Map_Bool_Bool_Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Map_Bool_Bool_Entry_Array'
             (Self.Data.all
                & Map_Bool_Bool_Entry_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Map_Bool_Bool_Entry_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Map_Bool_Bool_Entry_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Map_Bool_Bool_Entry_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Map_Bool_Bool_Entry_Variable_Reference
    (Self  : aliased in out Map_Bool_Bool_Entry_Vector;
     Index : Positive)
      return Map_Bool_Bool_Entry_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Bool_Bool_Entry_Variable_Reference;

   not overriding function Get_Map_Bool_Bool_Entry_Constant_Reference
    (Self  : aliased Map_Bool_Bool_Entry_Vector;
     Index : Positive)
      return Map_Bool_Bool_Entry_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Bool_Bool_Entry_Constant_Reference;

   procedure Read_Map_Bool_Bool_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Bool_Bool_Entry) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read (Stream, Key.Encoding, V.Key);
            when 2 =>
               PB_Support.IO.Read (Stream, Key.Encoding, V.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Bool_Bool_Entry;

   procedure Write_Map_Bool_Bool_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Bool_Bool_Entry) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Map_Bool_Bool_Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Option (1, V.Key, False);
         WS.Write_Option (2, V.Value, False);
         if WS.End_Message then
            Write_Map_Bool_Bool_Entry (WS'Access, V);
         end if;
      end;
   end Write_Map_Bool_Bool_Entry;

   function Length (Self : Map_String_String_Entry_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Map_String_String_Entry_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Map_String_String_Entry_Array, Map_String_String_Entry_Array_Access);

   procedure Append
    (Self : in out Map_String_String_Entry_Vector;
     V    : Map_String_String_Entry) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Map_String_String_Entry'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Map_String_String_Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Map_String_String_Entry_Array'
             (Self.Data.all
                & Map_String_String_Entry_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Map_String_String_Entry_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Map_String_String_Entry_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Map_String_String_Entry_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Map_String_String_Entry_Variable_Reference
    (Self  : aliased in out Map_String_String_Entry_Vector;
     Index : Positive)
      return Map_String_String_Entry_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_String_String_Entry_Variable_Reference;

   not overriding function Get_Map_String_String_Entry_Constant_Reference
    (Self  : aliased Map_String_String_Entry_Vector;
     Index : Positive)
      return Map_String_String_Entry_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_String_String_Entry_Constant_Reference;

   procedure Read_Map_String_String_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_String_String_Entry) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read (Stream, Key.Encoding, V.Key);
            when 2 =>
               PB_Support.IO.Read (Stream, Key.Encoding, V.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_String_String_Entry;

   procedure Write_Map_String_String_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_String_String_Entry) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Map_String_String_Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Option (1, V.Key);
         WS.Write_Option (2, V.Value);
         if WS.End_Message then
            Write_Map_String_String_Entry (WS'Access, V);
         end if;
      end;
   end Write_Map_String_String_Entry;

   function Length (Self : Map_String_Bytes_Entry_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Map_String_Bytes_Entry_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Map_String_Bytes_Entry_Array, Map_String_Bytes_Entry_Array_Access);

   procedure Append
    (Self : in out Map_String_Bytes_Entry_Vector;
     V    : Map_String_Bytes_Entry) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Map_String_Bytes_Entry'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Map_String_Bytes_Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Map_String_Bytes_Entry_Array'
             (Self.Data.all
                & Map_String_Bytes_Entry_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Map_String_Bytes_Entry_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Map_String_Bytes_Entry_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Map_String_Bytes_Entry_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Map_String_Bytes_Entry_Variable_Reference
    (Self  : aliased in out Map_String_Bytes_Entry_Vector;
     Index : Positive)
      return Map_String_Bytes_Entry_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_String_Bytes_Entry_Variable_Reference;

   not overriding function Get_Map_String_Bytes_Entry_Constant_Reference
    (Self  : aliased Map_String_Bytes_Entry_Vector;
     Index : Positive)
      return Map_String_Bytes_Entry_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_String_Bytes_Entry_Constant_Reference;

   procedure Read_Map_String_Bytes_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_String_Bytes_Entry) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read (Stream, Key.Encoding, V.Key);
            when 2 =>
               PB_Support.IO.Read (Stream, Key.Encoding, V.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_String_Bytes_Entry;

   procedure Write_Map_String_Bytes_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_String_Bytes_Entry) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Map_String_Bytes_Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Option (1, V.Key);
         WS.Write_Option (2, V.Value);
         if WS.End_Message then
            Write_Map_String_Bytes_Entry (WS'Access, V);
         end if;
      end;
   end Write_Map_String_Bytes_Entry;

   function Length
    (Self : Map_String_Nested_Message_Entry_Vector)
      return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Map_String_Nested_Message_Entry_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Map_String_Nested_Message_Entry_Array,
      Map_String_Nested_Message_Entry_Array_Access);

   procedure Append
    (Self : in out Map_String_Nested_Message_Entry_Vector;
     V    : Map_String_Nested_Message_Entry) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Map_String_Nested_Message_Entry'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=
            new Map_String_Nested_Message_Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Map_String_Nested_Message_Entry_Array'
             (Self.Data.all
                & Map_String_Nested_Message_Entry_Array'
                  (1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Map_String_Nested_Message_Entry_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Map_String_Nested_Message_Entry_Array'
             (Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Map_String_Nested_Message_Entry_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Map_String_Nested_Message_Entry_Variable_Reference
    (Self  : aliased in out Map_String_Nested_Message_Entry_Vector;
     Index : Positive)
      return Map_String_Nested_Message_Entry_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_String_Nested_Message_Entry_Variable_Reference;

   not overriding function Get_Map_String_Nested_Message_Entry_Constant_Reference
    (Self  : aliased Map_String_Nested_Message_Entry_Vector;
     Index : Positive)
      return Map_String_Nested_Message_Entry_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_String_Nested_Message_Entry_Constant_Reference;

   procedure Read_Map_String_Nested_Message_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_String_Nested_Message_Entry) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read (Stream, Key.Encoding, V.Key);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Nested_Message_IO.Read (Stream, Key.Encoding, V.Value.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_String_Nested_Message_Entry;

   procedure Write_Map_String_Nested_Message_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_String_Nested_Message_Entry) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Map_String_Nested_Message_Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Option (1, V.Key);
         if V.Value.Is_Set then
            WS.Write_Key ((2, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
              .Nested_Message'Write
              (Stream, V.Value.Value);
         end if;
         if WS.End_Message then
            Write_Map_String_Nested_Message_Entry (WS'Access, V);
         end if;
      end;
   end Write_Map_String_Nested_Message_Entry;

   function Length
    (Self : Map_String_Foreign_Message_Entry_Vector)
      return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Map_String_Foreign_Message_Entry_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Map_String_Foreign_Message_Entry_Array,
      Map_String_Foreign_Message_Entry_Array_Access);

   procedure Append
    (Self : in out Map_String_Foreign_Message_Entry_Vector;
     V    : Map_String_Foreign_Message_Entry) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Map_String_Foreign_Message_Entry'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=
            new Map_String_Foreign_Message_Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Map_String_Foreign_Message_Entry_Array'
             (Self.Data.all
                & Map_String_Foreign_Message_Entry_Array'
                  (1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Map_String_Foreign_Message_Entry_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Map_String_Foreign_Message_Entry_Array'
             (Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Map_String_Foreign_Message_Entry_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Map_String_Foreign_Message_Entry_Variable_Reference
    (Self  : aliased in out Map_String_Foreign_Message_Entry_Vector;
     Index : Positive)
      return Map_String_Foreign_Message_Entry_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_String_Foreign_Message_Entry_Variable_Reference;

   not overriding function Get_Map_String_Foreign_Message_Entry_Constant_Reference
    (Self  : aliased Map_String_Foreign_Message_Entry_Vector;
     Index : Positive)
      return Map_String_Foreign_Message_Entry_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_String_Foreign_Message_Entry_Constant_Reference;

   procedure Read_Map_String_Foreign_Message_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_String_Foreign_Message_Entry) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read (Stream, Key.Encoding, V.Key);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Foreign_Message_IO.Read (Stream, Key.Encoding, V.Value.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_String_Foreign_Message_Entry;

   procedure Write_Map_String_Foreign_Message_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_String_Foreign_Message_Entry) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Map_String_Foreign_Message_Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Option (1, V.Key);
         if V.Value.Is_Set then
            WS.Write_Key ((2, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3
              .Foreign_Message'Write
              (Stream, V.Value.Value);
         end if;
         if WS.End_Message then
            Write_Map_String_Foreign_Message_Entry (WS'Access, V);
         end if;
      end;
   end Write_Map_String_Foreign_Message_Entry;

   function Length
    (Self : Map_String_Nested_Enum_Entry_Vector)
      return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Map_String_Nested_Enum_Entry_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Map_String_Nested_Enum_Entry_Array,
      Map_String_Nested_Enum_Entry_Array_Access);

   procedure Append
    (Self : in out Map_String_Nested_Enum_Entry_Vector;
     V    : Map_String_Nested_Enum_Entry) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Map_String_Nested_Enum_Entry'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=
            new Map_String_Nested_Enum_Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Map_String_Nested_Enum_Entry_Array'
             (Self.Data.all
                & Map_String_Nested_Enum_Entry_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Map_String_Nested_Enum_Entry_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Map_String_Nested_Enum_Entry_Array'
             (Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Map_String_Nested_Enum_Entry_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Map_String_Nested_Enum_Entry_Variable_Reference
    (Self  : aliased in out Map_String_Nested_Enum_Entry_Vector;
     Index : Positive)
      return Map_String_Nested_Enum_Entry_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_String_Nested_Enum_Entry_Variable_Reference;

   not overriding function Get_Map_String_Nested_Enum_Entry_Constant_Reference
    (Self  : aliased Map_String_Nested_Enum_Entry_Vector;
     Index : Positive)
      return Map_String_Nested_Enum_Entry_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_String_Nested_Enum_Entry_Constant_Reference;

   procedure Read_Map_String_Nested_Enum_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_String_Nested_Enum_Entry) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read (Stream, Key.Encoding, V.Key);
            when 2 =>
               Nested_Enum_IO.Read (Stream, Key.Encoding, V.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_String_Nested_Enum_Entry;

   procedure Write_Map_String_Nested_Enum_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_String_Nested_Enum_Entry) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Map_String_Nested_Enum_Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Option (1, V.Key);
         Nested_Enum_IO.Write_Option
           (WS, 2, V.Value,
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.FOO);
         if WS.End_Message then
            Write_Map_String_Nested_Enum_Entry (WS'Access, V);
         end if;
      end;
   end Write_Map_String_Nested_Enum_Entry;

   function Length
    (Self : Map_String_Foreign_Enum_Entry_Vector)
      return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Map_String_Foreign_Enum_Entry_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Map_String_Foreign_Enum_Entry_Array,
      Map_String_Foreign_Enum_Entry_Array_Access);

   procedure Append
    (Self : in out Map_String_Foreign_Enum_Entry_Vector;
     V    : Map_String_Foreign_Enum_Entry) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Map_String_Foreign_Enum_Entry'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=
            new Map_String_Foreign_Enum_Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Map_String_Foreign_Enum_Entry_Array'
             (Self.Data.all
                & Map_String_Foreign_Enum_Entry_Array'
                  (1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Map_String_Foreign_Enum_Entry_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Map_String_Foreign_Enum_Entry_Array'
             (Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Map_String_Foreign_Enum_Entry_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Map_String_Foreign_Enum_Entry_Variable_Reference
    (Self  : aliased in out Map_String_Foreign_Enum_Entry_Vector;
     Index : Positive)
      return Map_String_Foreign_Enum_Entry_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_String_Foreign_Enum_Entry_Variable_Reference;

   not overriding function Get_Map_String_Foreign_Enum_Entry_Constant_Reference
    (Self  : aliased Map_String_Foreign_Enum_Entry_Vector;
     Index : Positive)
      return Map_String_Foreign_Enum_Entry_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_String_Foreign_Enum_Entry_Constant_Reference;

   procedure Read_Map_String_Foreign_Enum_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_String_Foreign_Enum_Entry) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read (Stream, Key.Encoding, V.Key);
            when 2 =>
               Foreign_Enum_IO.Read (Stream, Key.Encoding, V.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_String_Foreign_Enum_Entry;

   procedure Write_Map_String_Foreign_Enum_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_String_Foreign_Enum_Entry) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Map_String_Foreign_Enum_Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Option (1, V.Key);
         Foreign_Enum_IO.Write_Option
           (WS, 2, V.Value,
            Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3.FOREIGN_FOO);
         if WS.End_Message then
            Write_Map_String_Foreign_Enum_Entry (WS'Access, V);
         end if;
      end;
   end Write_Map_String_Foreign_Enum_Entry;

   function Length (Self : Foreign_Message_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Foreign_Message_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Foreign_Message_Array, Foreign_Message_Array_Access);

   procedure Append
    (Self : in out Foreign_Message_Vector;
     V    : Foreign_Message) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Foreign_Message'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Foreign_Message_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Foreign_Message_Array'
             (Self.Data.all & Foreign_Message_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Foreign_Message_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Foreign_Message_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Foreign_Message_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Foreign_Message_Variable_Reference
    (Self  : aliased in out Foreign_Message_Vector;
     Index : Positive)
      return Foreign_Message_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Foreign_Message_Variable_Reference;

   not overriding function Get_Foreign_Message_Constant_Reference
    (Self  : aliased Foreign_Message_Vector;
     Index : Positive)
      return Foreign_Message_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Foreign_Message_Constant_Reference;

   procedure Read_Foreign_Message
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Foreign_Message) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.C);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Foreign_Message;

   procedure Write_Foreign_Message
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Foreign_Message) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Foreign_Message (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Varint_Option (1, V.C, 0);
         if WS.End_Message then
            Write_Foreign_Message (WS'Access, V);
         end if;
      end;
   end Write_Foreign_Message;

end Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3;