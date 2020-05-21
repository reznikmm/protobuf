with Ada.Unchecked_Deallocation;
with PB_Support.IO;
with PB_Support.Internal;

package body Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2 is

   type Integer_Foreign_Enum_Proto_2 is  range 0 .. 2
     with Size =>
       Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
         .Foreign_Enum_Proto_2'Size;

   package Foreign_Enum_Proto_2_IO is
     new PB_Support.IO.Enum_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Foreign_Enum_Proto_2, Integer_Foreign_Enum_Proto_2,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Foreign_Enum_Proto_2_Vectors);

   package Foreign_Message_Proto_2_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Foreign_Message_Proto_2,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Foreign_Message_Proto_2_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Test_All_Types_Proto_2_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Test_All_Types_Proto_2,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Test_All_Types_Proto_2_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Data_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Data,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Data_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Map_Bool_Bool_Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Bool_Bool_Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Bool_Bool_Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Map_Fixed_32Fixed_32Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Fixed_32Fixed_32Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Fixed_32Fixed_32Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Map_Fixed_64Fixed_64Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Fixed_64Fixed_64Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Fixed_64Fixed_64Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Map_Int_32Double_Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Int_32Double_Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Int_32Double_Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Map_Int_32Float_Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Int_32Float_Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Int_32Float_Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Map_Int_32Int_32Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Int_32Int_32Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Int_32Int_32Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Map_Int_64Int_64Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Int_64Int_64Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Int_64Int_64Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Map_Sfixed_32Sfixed_32Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Sfixed_32Sfixed_32Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Sfixed_32Sfixed_32Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Map_Sfixed_64Sfixed_64Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Sfixed_64Sfixed_64Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Sfixed_64Sfixed_64Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Map_Sint_32Sint_32Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Sint_32Sint_32Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Sint_32Sint_32Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Map_Sint_64Sint_64Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Sint_64Sint_64Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Sint_64Sint_64Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Map_String_Bytes_Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_String_Bytes_Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_String_Bytes_Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Map_String_Foreign_Enum_Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_String_Foreign_Enum_Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_String_Foreign_Enum_Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Map_String_Foreign_Message_Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_String_Foreign_Message_Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_String_Foreign_Message_Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Map_String_Nested_Enum_Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_String_Nested_Enum_Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_String_Nested_Enum_Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Map_String_Nested_Message_Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_String_Nested_Message_Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_String_Nested_Message_Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Map_String_String_Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_String_String_Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_String_String_Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Map_Uint_32Uint_32Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Uint_32Uint_32Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Uint_32Uint_32Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Map_Uint_64Uint_64Entry_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Uint_64Uint_64Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Uint_64Uint_64Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   type Integer_Nested_Enum is  range  - 1 .. 2
     with Size =>
       Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Nested_Enum'Size;

   package Nested_Enum_IO is
     new PB_Support.IO.Enum_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Nested_Enum,
        Integer_Nested_Enum,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Nested_Enum_Vectors);

   package Nested_Message_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Nested_Message,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Nested_Message_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Optional_Group_IO is
     new PB_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Optional_Group,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Optional_Group_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   function Length (Self : Test_All_Types_Proto_2_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Test_All_Types_Proto_2_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Test_All_Types_Proto_2_Array, Test_All_Types_Proto_2_Array_Access);

   procedure Append
    (Self : in out Test_All_Types_Proto_2_Vector;
     V    : Test_All_Types_Proto_2) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Test_All_Types_Proto_2'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Test_All_Types_Proto_2_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Test_All_Types_Proto_2_Array'
             (Self.Data.all
                & Test_All_Types_Proto_2_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Test_All_Types_Proto_2_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Test_All_Types_Proto_2_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Test_All_Types_Proto_2_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Test_All_Types_Proto_2_Variable_Reference
    (Self  : aliased in out Test_All_Types_Proto_2_Vector;
     Index : Positive)
      return Test_All_Types_Proto_2_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Test_All_Types_Proto_2_Variable_Reference;

   not overriding function Get_Test_All_Types_Proto_2_Constant_Reference
    (Self  : aliased Test_All_Types_Proto_2_Vector;
     Index : Positive)
      return Test_All_Types_Proto_2_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Test_All_Types_Proto_2_Constant_Reference;

   procedure Read_Test_All_Types_Proto_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Test_All_Types_Proto_2) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Optional_Int_32.Is_Set then
                  V.Optional_Int_32 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Optional_Int_32.Value);
            when 2 =>
               if  not V.Optional_Int_64.Is_Set then
                  V.Optional_Int_64 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Optional_Int_64.Value);
            when 3 =>
               if  not V.Optional_Uint_32.Is_Set then
                  V.Optional_Uint_32 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Optional_Uint_32.Value);
            when 4 =>
               if  not V.Optional_Uint_64.Is_Set then
                  V.Optional_Uint_64 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Optional_Uint_64.Value);
            when 5 =>
               if  not V.Optional_Sint_32.Is_Set then
                  V.Optional_Sint_32 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Zigzag
                 (Stream, Key.Encoding, V.Optional_Sint_32.Value);
            when 6 =>
               if  not V.Optional_Sint_64.Is_Set then
                  V.Optional_Sint_64 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Zigzag
                 (Stream, Key.Encoding, V.Optional_Sint_64.Value);
            when 7 =>
               if  not V.Optional_Fixed_32.Is_Set then
                  V.Optional_Fixed_32 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Optional_Fixed_32.Value);
            when 8 =>
               if  not V.Optional_Fixed_64.Is_Set then
                  V.Optional_Fixed_64 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Optional_Fixed_64.Value);
            when 9 =>
               if  not V.Optional_Sfixed_32.Is_Set then
                  V.Optional_Sfixed_32 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Optional_Sfixed_32.Value);
            when 10 =>
               if  not V.Optional_Sfixed_64.Is_Set then
                  V.Optional_Sfixed_64 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Optional_Sfixed_64.Value);
            when 11 =>
               if  not V.Optional_Float.Is_Set then
                  V.Optional_Float := (True, others => <>);
               end if;
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Optional_Float.Value);
            when 12 =>
               if  not V.Optional_Double.Is_Set then
                  V.Optional_Double := (True, others => <>);
               end if;
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Optional_Double.Value);
            when 13 =>
               if  not V.Optional_Bool.Is_Set then
                  V.Optional_Bool := (True, others => <>);
               end if;
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Optional_Bool.Value);
            when 14 =>
               if  not V.Optional_String.Is_Set then
                  V.Optional_String := (True, others => <>);
               end if;
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Optional_String.Value);
            when 15 =>
               if  not V.Optional_Bytes.Is_Set then
                  V.Optional_Bytes := (True, others => <>);
               end if;
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Optional_Bytes.Value);
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
               Foreign_Message_Proto_2_IO.Read
                 (Stream, Key.Encoding, V.Optional_Foreign_Message.Value);
            when 21 =>
               if  not V.Optional_Nested_Enum.Is_Set then
                  V.Optional_Nested_Enum := (True, others => <>);
               end if;
               Nested_Enum_IO.Read
                 (Stream, Key.Encoding, V.Optional_Nested_Enum.Value);
            when 22 =>
               if  not V.Optional_Foreign_Enum.Is_Set then
                  V.Optional_Foreign_Enum := (True, others => <>);
               end if;
               Foreign_Enum_Proto_2_IO.Read
                 (Stream, Key.Encoding, V.Optional_Foreign_Enum.Value);
            when 24 =>
               if  not V.Optional_String_Piece.Is_Set then
                  V.Optional_String_Piece := (True, others => <>);
               end if;
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Optional_String_Piece.Value);
            when 25 =>
               if  not V.Optional_Cord.Is_Set then
                  V.Optional_Cord := (True, others => <>);
               end if;
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Optional_Cord.Value);
            when 27 =>
               if V.Recursive_Message.Length = 0 then
                  V.Recursive_Message.Append ((others => <>));
               end if;
               Test_All_Types_Proto_2_IO.Read
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
               Foreign_Message_Proto_2_IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Foreign_Message);
            when 51 =>
               Nested_Enum_IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Nested_Enum);
            when 52 =>
               Foreign_Enum_Proto_2_IO.Read_Vector
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
               if  not V.Data.Is_Set then
                  V.Data := (True, others => <>);
               end if;
               Data_IO.Read (Stream, Key.Encoding, V.Data.Value);
            when 401 =>
               if  not V.Fieldname_1.Is_Set then
                  V.Fieldname_1 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Fieldname_1.Value);
            when 402 =>
               if  not V.Field_Name_2.Is_Set then
                  V.Field_Name_2 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_2.Value);
            when 403 =>
               if  not V.Field_Name_3.Is_Set then
                  V.Field_Name_3 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_3.Value);
            when 404 =>
               if  not V.Field_Name_4.Is_Set then
                  V.Field_Name_4 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_4.Value);
            when 405 =>
               if  not V.Field_0name_5.Is_Set then
                  V.Field_0name_5 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_0name_5.Value);
            when 406 =>
               if  not V.Field_0_Name_6.Is_Set then
                  V.Field_0_Name_6 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_0_Name_6.Value);
            when 407 =>
               if  not V.Field_Name_7.Is_Set then
                  V.Field_Name_7 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_7.Value);
            when 408 =>
               if  not V.Field_Name_8.Is_Set then
                  V.Field_Name_8 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_8.Value);
            when 409 =>
               if  not V.Field_Name_9.Is_Set then
                  V.Field_Name_9 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_9.Value);
            when 410 =>
               if  not V.Field_Name_10.Is_Set then
                  V.Field_Name_10 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_10.Value);
            when 411 =>
               if  not V.FIELD_NAME11.Is_Set then
                  V.FIELD_NAME11 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.FIELD_NAME11.Value);
            when 412 =>
               if  not V.FIELD_Name_12.Is_Set then
                  V.FIELD_Name_12 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.FIELD_Name_12.Value);
            when 413 =>
               if  not V.Field_Name_13.Is_Set then
                  V.Field_Name_13 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_13.Value);
            when 414 =>
               if  not V.Field_Name_14.Is_Set then
                  V.Field_Name_14 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_14.Value);
            when 415 =>
               if  not V.Field_Name_15.Is_Set then
                  V.Field_Name_15 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_15.Value);
            when 416 =>
               if  not V.Field_Name_16.Is_Set then
                  V.Field_Name_16 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_16.Value);
            when 417 =>
               if  not V.Field_Name_17.Is_Set then
                  V.Field_Name_17 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_17.Value);
            when 418 =>
               if  not V.Field_Name_18.Is_Set then
                  V.Field_Name_18 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_18.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Test_All_Types_Proto_2;

   procedure Write_Test_All_Types_Proto_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Test_All_Types_Proto_2) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Test_All_Types_Proto_2 (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Optional_Int_32.Is_Set then
            WS.Write_Varint (1, V.Optional_Int_32.Value);
         end if;
         if V.Optional_Int_64.Is_Set then
            WS.Write_Varint (2, V.Optional_Int_64.Value);
         end if;
         if V.Optional_Uint_32.Is_Set then
            WS.Write_Varint (3, V.Optional_Uint_32.Value);
         end if;
         if V.Optional_Uint_64.Is_Set then
            WS.Write_Varint (4, V.Optional_Uint_64.Value);
         end if;
         if V.Optional_Sint_32.Is_Set then
            WS.Write_Zigzag (5, V.Optional_Sint_32.Value);
         end if;
         if V.Optional_Sint_64.Is_Set then
            WS.Write_Zigzag (6, V.Optional_Sint_64.Value);
         end if;
         if V.Optional_Fixed_32.Is_Set then
            WS.Write_Fixed (7, V.Optional_Fixed_32.Value);
         end if;
         if V.Optional_Fixed_64.Is_Set then
            WS.Write_Fixed (8, V.Optional_Fixed_64.Value);
         end if;
         if V.Optional_Sfixed_32.Is_Set then
            WS.Write_Fixed (9, V.Optional_Sfixed_32.Value);
         end if;
         if V.Optional_Sfixed_64.Is_Set then
            WS.Write_Fixed (10, V.Optional_Sfixed_64.Value);
         end if;
         if V.Optional_Float.Is_Set then
            WS.Write (11, V.Optional_Float.Value);
         end if;
         if V.Optional_Double.Is_Set then
            WS.Write (12, V.Optional_Double.Value);
         end if;
         if V.Optional_Bool.Is_Set then
            WS.Write (13, V.Optional_Bool.Value);
         end if;
         if V.Optional_String.Is_Set then
            WS.Write (14, V.Optional_String.Value);
         end if;
         if V.Optional_Bytes.Is_Set then
            WS.Write (15, V.Optional_Bytes.Value);
         end if;
         if V.Optional_Nested_Message.Is_Set then
            WS.Write_Key ((18, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Nested_Message'Write
              (Stream, V.Optional_Nested_Message.Value);
         end if;
         if V.Optional_Foreign_Message.Is_Set then
            WS.Write_Key ((19, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Foreign_Message_Proto_2'Write
              (Stream, V.Optional_Foreign_Message.Value);
         end if;
         if V.Optional_Nested_Enum.Is_Set then
            Nested_Enum_IO.Write (WS, 21, V.Optional_Nested_Enum.Value);
         end if;
         if V.Optional_Foreign_Enum.Is_Set then
            Foreign_Enum_Proto_2_IO.Write
              (WS, 22, V.Optional_Foreign_Enum.Value);
         end if;
         if V.Optional_String_Piece.Is_Set then
            WS.Write (24, V.Optional_String_Piece.Value);
         end if;
         if V.Optional_Cord.Is_Set then
            WS.Write (25, V.Optional_Cord.Value);
         end if;
         for J in 1 .. V.Recursive_Message.Length loop
            WS.Write_Key ((27, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Test_All_Types_Proto_2'Write
              (Stream, V.Recursive_Message (J));
         end loop;
         WS.Write_Varint (31, V.Repeated_Int_32);
         WS.Write_Varint (32, V.Repeated_Int_64);
         WS.Write_Varint (33, V.Repeated_Uint_32);
         WS.Write_Varint (34, V.Repeated_Uint_64);
         WS.Write_Zigzag (35, V.Repeated_Sint_32);
         WS.Write_Zigzag (36, V.Repeated_Sint_64);
         WS.Write_Fixed (37, V.Repeated_Fixed_32);
         WS.Write_Fixed (38, V.Repeated_Fixed_64);
         WS.Write_Fixed (39, V.Repeated_Sfixed_32);
         WS.Write_Fixed (40, V.Repeated_Sfixed_64);
         WS.Write (41, V.Repeated_Float);
         WS.Write (42, V.Repeated_Double);
         WS.Write (43, V.Repeated_Bool);
         WS.Write (44, V.Repeated_String);
         WS.Write (45, V.Repeated_Bytes);
         for J in 1 .. V.Repeated_Nested_Message.Length loop
            WS.Write_Key ((48, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Nested_Message'Write
              (Stream, V.Repeated_Nested_Message (J));
         end loop;
         for J in 1 .. V.Repeated_Foreign_Message.Length loop
            WS.Write_Key ((49, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Foreign_Message_Proto_2'Write
              (Stream, V.Repeated_Foreign_Message (J));
         end loop;
         Nested_Enum_IO.Write (WS, 51, V.Repeated_Nested_Enum);
         Foreign_Enum_Proto_2_IO.Write (WS, 52, V.Repeated_Foreign_Enum);
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
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Int_32Int_32Entry'Write
              (Stream, V.Map_Int_32_Int_32 (J));
         end loop;
         for J in 1 .. V.Map_Int_64_Int_64.Length loop
            WS.Write_Key ((57, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Int_64Int_64Entry'Write
              (Stream, V.Map_Int_64_Int_64 (J));
         end loop;
         for J in 1 .. V.Map_Uint_32_Uint_32.Length loop
            WS.Write_Key ((58, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Uint_32Uint_32Entry'Write
              (Stream, V.Map_Uint_32_Uint_32 (J));
         end loop;
         for J in 1 .. V.Map_Uint_64_Uint_64.Length loop
            WS.Write_Key ((59, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Uint_64Uint_64Entry'Write
              (Stream, V.Map_Uint_64_Uint_64 (J));
         end loop;
         for J in 1 .. V.Map_Sint_32_Sint_32.Length loop
            WS.Write_Key ((60, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Sint_32Sint_32Entry'Write
              (Stream, V.Map_Sint_32_Sint_32 (J));
         end loop;
         for J in 1 .. V.Map_Sint_64_Sint_64.Length loop
            WS.Write_Key ((61, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Sint_64Sint_64Entry'Write
              (Stream, V.Map_Sint_64_Sint_64 (J));
         end loop;
         for J in 1 .. V.Map_Fixed_32_Fixed_32.Length loop
            WS.Write_Key ((62, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Fixed_32Fixed_32Entry'Write
              (Stream, V.Map_Fixed_32_Fixed_32 (J));
         end loop;
         for J in 1 .. V.Map_Fixed_64_Fixed_64.Length loop
            WS.Write_Key ((63, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Fixed_64Fixed_64Entry'Write
              (Stream, V.Map_Fixed_64_Fixed_64 (J));
         end loop;
         for J in 1 .. V.Map_Sfixed_32_Sfixed_32.Length loop
            WS.Write_Key ((64, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Sfixed_32Sfixed_32Entry'Write
              (Stream, V.Map_Sfixed_32_Sfixed_32 (J));
         end loop;
         for J in 1 .. V.Map_Sfixed_64_Sfixed_64.Length loop
            WS.Write_Key ((65, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Sfixed_64Sfixed_64Entry'Write
              (Stream, V.Map_Sfixed_64_Sfixed_64 (J));
         end loop;
         for J in 1 .. V.Map_Int_32_Float.Length loop
            WS.Write_Key ((66, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Int_32Float_Entry'Write
              (Stream, V.Map_Int_32_Float (J));
         end loop;
         for J in 1 .. V.Map_Int_32_Double.Length loop
            WS.Write_Key ((67, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Int_32Double_Entry'Write
              (Stream, V.Map_Int_32_Double (J));
         end loop;
         for J in 1 .. V.Map_Bool_Bool.Length loop
            WS.Write_Key ((68, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Bool_Bool_Entry'Write
              (Stream, V.Map_Bool_Bool (J));
         end loop;
         for J in 1 .. V.Map_String_String.Length loop
            WS.Write_Key ((69, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_String_String_Entry'Write
              (Stream, V.Map_String_String (J));
         end loop;
         for J in 1 .. V.Map_String_Bytes.Length loop
            WS.Write_Key ((70, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_String_Bytes_Entry'Write
              (Stream, V.Map_String_Bytes (J));
         end loop;
         for J in 1 .. V.Map_String_Nested_Message.Length loop
            WS.Write_Key ((71, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_String_Nested_Message_Entry'Write
              (Stream, V.Map_String_Nested_Message (J));
         end loop;
         for J in 1 .. V.Map_String_Foreign_Message.Length loop
            WS.Write_Key ((72, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_String_Foreign_Message_Entry'Write
              (Stream, V.Map_String_Foreign_Message (J));
         end loop;
         for J in 1 .. V.Map_String_Nested_Enum.Length loop
            WS.Write_Key ((73, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_String_Nested_Enum_Entry'Write
              (Stream, V.Map_String_Nested_Enum (J));
         end loop;
         for J in 1 .. V.Map_String_Foreign_Enum.Length loop
            WS.Write_Key ((74, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_String_Foreign_Enum_Entry'Write
              (Stream, V.Map_String_Foreign_Enum (J));
         end loop;
         if V.Data.Is_Set then
            WS.Write_Key ((201, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Data'Write
              (Stream, V.Data.Value);
         end if;
         if V.Fieldname_1.Is_Set then
            WS.Write_Varint (401, V.Fieldname_1.Value);
         end if;
         if V.Field_Name_2.Is_Set then
            WS.Write_Varint (402, V.Field_Name_2.Value);
         end if;
         if V.Field_Name_3.Is_Set then
            WS.Write_Varint (403, V.Field_Name_3.Value);
         end if;
         if V.Field_Name_4.Is_Set then
            WS.Write_Varint (404, V.Field_Name_4.Value);
         end if;
         if V.Field_0name_5.Is_Set then
            WS.Write_Varint (405, V.Field_0name_5.Value);
         end if;
         if V.Field_0_Name_6.Is_Set then
            WS.Write_Varint (406, V.Field_0_Name_6.Value);
         end if;
         if V.Field_Name_7.Is_Set then
            WS.Write_Varint (407, V.Field_Name_7.Value);
         end if;
         if V.Field_Name_8.Is_Set then
            WS.Write_Varint (408, V.Field_Name_8.Value);
         end if;
         if V.Field_Name_9.Is_Set then
            WS.Write_Varint (409, V.Field_Name_9.Value);
         end if;
         if V.Field_Name_10.Is_Set then
            WS.Write_Varint (410, V.Field_Name_10.Value);
         end if;
         if V.FIELD_NAME11.Is_Set then
            WS.Write_Varint (411, V.FIELD_NAME11.Value);
         end if;
         if V.FIELD_Name_12.Is_Set then
            WS.Write_Varint (412, V.FIELD_Name_12.Value);
         end if;
         if V.Field_Name_13.Is_Set then
            WS.Write_Varint (413, V.Field_Name_13.Value);
         end if;
         if V.Field_Name_14.Is_Set then
            WS.Write_Varint (414, V.Field_Name_14.Value);
         end if;
         if V.Field_Name_15.Is_Set then
            WS.Write_Varint (415, V.Field_Name_15.Value);
         end if;
         if V.Field_Name_16.Is_Set then
            WS.Write_Varint (416, V.Field_Name_16.Value);
         end if;
         if V.Field_Name_17.Is_Set then
            WS.Write_Varint (417, V.Field_Name_17.Value);
         end if;
         if V.Field_Name_18.Is_Set then
            WS.Write_Varint (418, V.Field_Name_18.Value);
         end if;
         case V.Variant.Oneof_Field is
            when Oneof_Uint_32_Kind =>
               WS.Write_Varint (111, V.Variant.Oneof_Uint_32);
            when Oneof_Nested_Message_Kind =>
               WS.Write_Key ((112, PB_Support.Length_Delimited));
               Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
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
            Write_Test_All_Types_Proto_2 (WS'Access, V);
         end if;
      end;
   end Write_Test_All_Types_Proto_2;

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
               if  not V.A.Is_Set then
                  V.A := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.A.Value);
            when 2 =>
               if V.Corecursive.Length = 0 then
                  V.Corecursive.Append ((others => <>));
               end if;
               Test_All_Types_Proto_2_IO.Read
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
         if V.A.Is_Set then
            WS.Write_Varint (1, V.A.Value);
         end if;
         for J in 1 .. V.Corecursive.Length loop
            WS.Write_Key ((2, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Test_All_Types_Proto_2'Write
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
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.Value.Value);
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
         if V.Key.Is_Set then
            WS.Write_Varint (1, V.Key.Value);
         end if;
         if V.Value.Is_Set then
            WS.Write_Varint (2, V.Value.Value);
         end if;
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
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.Value.Value);
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
         if V.Key.Is_Set then
            WS.Write_Varint (1, V.Key.Value);
         end if;
         if V.Value.Is_Set then
            WS.Write_Varint (2, V.Value.Value);
         end if;
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
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.Value.Value);
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
         if V.Key.Is_Set then
            WS.Write_Varint (1, V.Key.Value);
         end if;
         if V.Value.Is_Set then
            WS.Write_Varint (2, V.Value.Value);
         end if;
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
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.Value.Value);
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
         if V.Key.Is_Set then
            WS.Write_Varint (1, V.Key.Value);
         end if;
         if V.Value.Is_Set then
            WS.Write_Varint (2, V.Value.Value);
         end if;
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
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               PB_Support.IO.Read_Zigzag (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               PB_Support.IO.Read_Zigzag (Stream, Key.Encoding, V.Value.Value);
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
         if V.Key.Is_Set then
            WS.Write_Zigzag (1, V.Key.Value);
         end if;
         if V.Value.Is_Set then
            WS.Write_Zigzag (2, V.Value.Value);
         end if;
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
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               PB_Support.IO.Read_Zigzag (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               PB_Support.IO.Read_Zigzag (Stream, Key.Encoding, V.Value.Value);
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
         if V.Key.Is_Set then
            WS.Write_Zigzag (1, V.Key.Value);
         end if;
         if V.Value.Is_Set then
            WS.Write_Zigzag (2, V.Value.Value);
         end if;
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
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               PB_Support.IO.Read_Fixed (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               PB_Support.IO.Read_Fixed (Stream, Key.Encoding, V.Value.Value);
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
         if V.Key.Is_Set then
            WS.Write_Fixed (1, V.Key.Value);
         end if;
         if V.Value.Is_Set then
            WS.Write_Fixed (2, V.Value.Value);
         end if;
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
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               PB_Support.IO.Read_Fixed (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               PB_Support.IO.Read_Fixed (Stream, Key.Encoding, V.Value.Value);
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
         if V.Key.Is_Set then
            WS.Write_Fixed (1, V.Key.Value);
         end if;
         if V.Value.Is_Set then
            WS.Write_Fixed (2, V.Value.Value);
         end if;
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
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               PB_Support.IO.Read_Fixed (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               PB_Support.IO.Read_Fixed (Stream, Key.Encoding, V.Value.Value);
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
         if V.Key.Is_Set then
            WS.Write_Fixed (1, V.Key.Value);
         end if;
         if V.Value.Is_Set then
            WS.Write_Fixed (2, V.Value.Value);
         end if;
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
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               PB_Support.IO.Read_Fixed (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               PB_Support.IO.Read_Fixed (Stream, Key.Encoding, V.Value.Value);
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
         if V.Key.Is_Set then
            WS.Write_Fixed (1, V.Key.Value);
         end if;
         if V.Value.Is_Set then
            WS.Write_Fixed (2, V.Value.Value);
         end if;
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
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               PB_Support.IO.Read (Stream, Key.Encoding, V.Value.Value);
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
         if V.Key.Is_Set then
            WS.Write_Varint (1, V.Key.Value);
         end if;
         if V.Value.Is_Set then
            WS.Write (2, V.Value.Value);
         end if;
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
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               PB_Support.IO.Read (Stream, Key.Encoding, V.Value.Value);
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
         if V.Key.Is_Set then
            WS.Write_Varint (1, V.Key.Value);
         end if;
         if V.Value.Is_Set then
            WS.Write (2, V.Value.Value);
         end if;
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
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               PB_Support.IO.Read (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               PB_Support.IO.Read (Stream, Key.Encoding, V.Value.Value);
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
         if V.Key.Is_Set then
            WS.Write (1, V.Key.Value);
         end if;
         if V.Value.Is_Set then
            WS.Write (2, V.Value.Value);
         end if;
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
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               PB_Support.IO.Read (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               PB_Support.IO.Read (Stream, Key.Encoding, V.Value.Value);
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
         if V.Key.Is_Set then
            WS.Write (1, V.Key.Value);
         end if;
         if V.Value.Is_Set then
            WS.Write (2, V.Value.Value);
         end if;
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
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               PB_Support.IO.Read (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               PB_Support.IO.Read (Stream, Key.Encoding, V.Value.Value);
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
         if V.Key.Is_Set then
            WS.Write (1, V.Key.Value);
         end if;
         if V.Value.Is_Set then
            WS.Write (2, V.Value.Value);
         end if;
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
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               PB_Support.IO.Read (Stream, Key.Encoding, V.Key.Value);
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
         if V.Key.Is_Set then
            WS.Write (1, V.Key.Value);
         end if;
         if V.Value.Is_Set then
            WS.Write_Key ((2, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
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
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               PB_Support.IO.Read (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Foreign_Message_Proto_2_IO.Read
                 (Stream, Key.Encoding, V.Value.Value);
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
         if V.Key.Is_Set then
            WS.Write (1, V.Key.Value);
         end if;
         if V.Value.Is_Set then
            WS.Write_Key ((2, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Foreign_Message_Proto_2'Write
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
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               PB_Support.IO.Read (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Nested_Enum_IO.Read (Stream, Key.Encoding, V.Value.Value);
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
         if V.Key.Is_Set then
            WS.Write (1, V.Key.Value);
         end if;
         if V.Value.Is_Set then
            Nested_Enum_IO.Write (WS, 2, V.Value.Value);
         end if;
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
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               PB_Support.IO.Read (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Foreign_Enum_Proto_2_IO.Read
                 (Stream, Key.Encoding, V.Value.Value);
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
         if V.Key.Is_Set then
            WS.Write (1, V.Key.Value);
         end if;
         if V.Value.Is_Set then
            Foreign_Enum_Proto_2_IO.Write (WS, 2, V.Value.Value);
         end if;
         if WS.End_Message then
            Write_Map_String_Foreign_Enum_Entry (WS'Access, V);
         end if;
      end;
   end Write_Map_String_Foreign_Enum_Entry;

   function Length (Self : Data_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Data_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Data_Array, Data_Array_Access);

   procedure Append (Self : in out Data_Vector; V    : Data) is
      Init_Length : constant Positive := Positive'Max (1, 256 / Data'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Data_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Data_Array'
             (Self.Data.all & Data_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Data_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Data_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Data_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Data_Variable_Reference
    (Self  : aliased in out Data_Vector;
     Index : Positive)
      return Data_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Data_Variable_Reference;

   not overriding function Get_Data_Constant_Reference
    (Self  : aliased Data_Vector;
     Index : Positive)
      return Data_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Data_Constant_Reference;

   procedure Read_Data
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Data) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 202 =>
               if  not V.Group_Int_32.Is_Set then
                  V.Group_Int_32 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Group_Int_32.Value);
            when 203 =>
               if  not V.Group_Uint_32.Is_Set then
                  V.Group_Uint_32 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Group_Uint_32.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Data;

   procedure Write_Data
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Data) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Data (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Group_Int_32.Is_Set then
            WS.Write_Varint (202, V.Group_Int_32.Value);
         end if;
         if V.Group_Uint_32.Is_Set then
            WS.Write_Varint (203, V.Group_Uint_32.Value);
         end if;
         if WS.End_Message then
            Write_Data (WS'Access, V);
         end if;
      end;
   end Write_Data;

   function Length (Self : Message_Set_Correct_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Message_Set_Correct_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Message_Set_Correct_Array, Message_Set_Correct_Array_Access);

   procedure Append
    (Self : in out Message_Set_Correct_Vector;
     V    : Message_Set_Correct) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Message_Set_Correct'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Message_Set_Correct_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Message_Set_Correct_Array'
             (Self.Data.all
                & Message_Set_Correct_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Message_Set_Correct_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Message_Set_Correct_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Message_Set_Correct_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Message_Set_Correct_Variable_Reference
    (Self  : aliased in out Message_Set_Correct_Vector;
     Index : Positive)
      return Message_Set_Correct_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Message_Set_Correct_Variable_Reference;

   not overriding function Get_Message_Set_Correct_Constant_Reference
    (Self  : aliased Message_Set_Correct_Vector;
     Index : Positive)
      return Message_Set_Correct_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Message_Set_Correct_Constant_Reference;

   procedure Read_Message_Set_Correct
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Message_Set_Correct) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Message_Set_Correct;

   procedure Write_Message_Set_Correct
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Message_Set_Correct) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Message_Set_Correct (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if WS.End_Message then
            Write_Message_Set_Correct (WS'Access, V);
         end if;
      end;
   end Write_Message_Set_Correct;

   function Length
    (Self : Message_Set_Correct_Extension_1_Vector)
      return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Message_Set_Correct_Extension_1_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Message_Set_Correct_Extension_1_Array,
      Message_Set_Correct_Extension_1_Array_Access);

   procedure Append
    (Self : in out Message_Set_Correct_Extension_1_Vector;
     V    : Message_Set_Correct_Extension_1) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Message_Set_Correct_Extension_1'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=
            new Message_Set_Correct_Extension_1_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Message_Set_Correct_Extension_1_Array'
             (Self.Data.all
                & Message_Set_Correct_Extension_1_Array'
                  (1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Message_Set_Correct_Extension_1_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Message_Set_Correct_Extension_1_Array'
             (Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Message_Set_Correct_Extension_1_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Message_Set_Correct_Extension_1_Variable_Reference
    (Self  : aliased in out Message_Set_Correct_Extension_1_Vector;
     Index : Positive)
      return Message_Set_Correct_Extension_1_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Message_Set_Correct_Extension_1_Variable_Reference;

   not overriding function Get_Message_Set_Correct_Extension_1_Constant_Reference
    (Self  : aliased Message_Set_Correct_Extension_1_Vector;
     Index : Positive)
      return Message_Set_Correct_Extension_1_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Message_Set_Correct_Extension_1_Constant_Reference;

   procedure Read_Message_Set_Correct_Extension_1
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Message_Set_Correct_Extension_1) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 25 =>
               if  not V.Str.Is_Set then
                  V.Str := (True, others => <>);
               end if;
               PB_Support.IO.Read (Stream, Key.Encoding, V.Str.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Message_Set_Correct_Extension_1;

   procedure Write_Message_Set_Correct_Extension_1
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Message_Set_Correct_Extension_1) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Message_Set_Correct_Extension_1 (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Str.Is_Set then
            WS.Write (25, V.Str.Value);
         end if;
         if WS.End_Message then
            Write_Message_Set_Correct_Extension_1 (WS'Access, V);
         end if;
      end;
   end Write_Message_Set_Correct_Extension_1;

   function Length
    (Self : Message_Set_Correct_Extension_2_Vector)
      return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Message_Set_Correct_Extension_2_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Message_Set_Correct_Extension_2_Array,
      Message_Set_Correct_Extension_2_Array_Access);

   procedure Append
    (Self : in out Message_Set_Correct_Extension_2_Vector;
     V    : Message_Set_Correct_Extension_2) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Message_Set_Correct_Extension_2'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=
            new Message_Set_Correct_Extension_2_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Message_Set_Correct_Extension_2_Array'
             (Self.Data.all
                & Message_Set_Correct_Extension_2_Array'
                  (1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Message_Set_Correct_Extension_2_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Message_Set_Correct_Extension_2_Array'
             (Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Message_Set_Correct_Extension_2_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Message_Set_Correct_Extension_2_Variable_Reference
    (Self  : aliased in out Message_Set_Correct_Extension_2_Vector;
     Index : Positive)
      return Message_Set_Correct_Extension_2_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Message_Set_Correct_Extension_2_Variable_Reference;

   not overriding function Get_Message_Set_Correct_Extension_2_Constant_Reference
    (Self  : aliased Message_Set_Correct_Extension_2_Vector;
     Index : Positive)
      return Message_Set_Correct_Extension_2_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Message_Set_Correct_Extension_2_Constant_Reference;

   procedure Read_Message_Set_Correct_Extension_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Message_Set_Correct_Extension_2) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 9 =>
               if  not V.I.Is_Set then
                  V.I := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.I.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Message_Set_Correct_Extension_2;

   procedure Write_Message_Set_Correct_Extension_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Message_Set_Correct_Extension_2) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Message_Set_Correct_Extension_2 (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.I.Is_Set then
            WS.Write_Varint (9, V.I.Value);
         end if;
         if WS.End_Message then
            Write_Message_Set_Correct_Extension_2 (WS'Access, V);
         end if;
      end;
   end Write_Message_Set_Correct_Extension_2;

   function Length (Self : Foreign_Message_Proto_2_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Foreign_Message_Proto_2_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Foreign_Message_Proto_2_Array, Foreign_Message_Proto_2_Array_Access);

   procedure Append
    (Self : in out Foreign_Message_Proto_2_Vector;
     V    : Foreign_Message_Proto_2) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Foreign_Message_Proto_2'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Foreign_Message_Proto_2_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Foreign_Message_Proto_2_Array'
             (Self.Data.all
                & Foreign_Message_Proto_2_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Foreign_Message_Proto_2_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Foreign_Message_Proto_2_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Foreign_Message_Proto_2_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Foreign_Message_Proto_2_Variable_Reference
    (Self  : aliased in out Foreign_Message_Proto_2_Vector;
     Index : Positive)
      return Foreign_Message_Proto_2_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Foreign_Message_Proto_2_Variable_Reference;

   not overriding function Get_Foreign_Message_Proto_2_Constant_Reference
    (Self  : aliased Foreign_Message_Proto_2_Vector;
     Index : Positive)
      return Foreign_Message_Proto_2_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Foreign_Message_Proto_2_Constant_Reference;

   procedure Read_Foreign_Message_Proto_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Foreign_Message_Proto_2) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.C.Is_Set then
                  V.C := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.C.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Foreign_Message_Proto_2;

   procedure Write_Foreign_Message_Proto_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Foreign_Message_Proto_2) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Foreign_Message_Proto_2 (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.C.Is_Set then
            WS.Write_Varint (1, V.C.Value);
         end if;
         if WS.End_Message then
            Write_Foreign_Message_Proto_2 (WS'Access, V);
         end if;
      end;
   end Write_Foreign_Message_Proto_2;

   function Length (Self : Unknown_To_Test_All_Types_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Unknown_To_Test_All_Types_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Unknown_To_Test_All_Types_Array, Unknown_To_Test_All_Types_Array_Access);

   procedure Append
    (Self : in out Unknown_To_Test_All_Types_Vector;
     V    : Unknown_To_Test_All_Types) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Unknown_To_Test_All_Types'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Unknown_To_Test_All_Types_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Unknown_To_Test_All_Types_Array'
             (Self.Data.all
                & Unknown_To_Test_All_Types_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Unknown_To_Test_All_Types_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Unknown_To_Test_All_Types_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Unknown_To_Test_All_Types_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Unknown_To_Test_All_Types_Variable_Reference
    (Self  : aliased in out Unknown_To_Test_All_Types_Vector;
     Index : Positive)
      return Unknown_To_Test_All_Types_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Unknown_To_Test_All_Types_Variable_Reference;

   not overriding function Get_Unknown_To_Test_All_Types_Constant_Reference
    (Self  : aliased Unknown_To_Test_All_Types_Vector;
     Index : Positive)
      return Unknown_To_Test_All_Types_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Unknown_To_Test_All_Types_Constant_Reference;

   procedure Read_Unknown_To_Test_All_Types
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Unknown_To_Test_All_Types) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1001 =>
               if  not V.Optional_Int_32.Is_Set then
                  V.Optional_Int_32 := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Optional_Int_32.Value);
            when 1002 =>
               if  not V.Optional_String.Is_Set then
                  V.Optional_String := (True, others => <>);
               end if;
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Optional_String.Value);
            when 1003 =>
               if  not V.Nested_Message.Is_Set then
                  V.Nested_Message := (True, others => <>);
               end if;
               Foreign_Message_Proto_2_IO.Read
                 (Stream, Key.Encoding, V.Nested_Message.Value);
            when 1004 =>
               if  not V.Optionalgroup.Is_Set then
                  V.Optionalgroup := (True, others => <>);
               end if;
               Optional_Group_IO.Read
                 (Stream, Key.Encoding, V.Optionalgroup.Value);
            when 1006 =>
               if  not V.Optional_Bool.Is_Set then
                  V.Optional_Bool := (True, others => <>);
               end if;
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Optional_Bool.Value);
            when 1011 =>
               PB_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Repeated_Int_32);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Unknown_To_Test_All_Types;

   procedure Write_Unknown_To_Test_All_Types
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Unknown_To_Test_All_Types) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Unknown_To_Test_All_Types (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Optional_Int_32.Is_Set then
            WS.Write_Varint (1001, V.Optional_Int_32.Value);
         end if;
         if V.Optional_String.Is_Set then
            WS.Write (1002, V.Optional_String.Value);
         end if;
         if V.Nested_Message.Is_Set then
            WS.Write_Key ((1003, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Foreign_Message_Proto_2'Write
              (Stream, V.Nested_Message.Value);
         end if;
         if V.Optionalgroup.Is_Set then
            WS.Write_Key ((1004, PB_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Optional_Group'Write
              (Stream, V.Optionalgroup.Value);
         end if;
         if V.Optional_Bool.Is_Set then
            WS.Write (1006, V.Optional_Bool.Value);
         end if;
         WS.Write_Varint (1011, V.Repeated_Int_32);
         if WS.End_Message then
            Write_Unknown_To_Test_All_Types (WS'Access, V);
         end if;
      end;
   end Write_Unknown_To_Test_All_Types;

   function Length (Self : Optional_Group_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Optional_Group_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Optional_Group_Array, Optional_Group_Array_Access);

   procedure Append
    (Self : in out Optional_Group_Vector;
     V    : Optional_Group) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Optional_Group'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Optional_Group_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Optional_Group_Array'
             (Self.Data.all & Optional_Group_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Optional_Group_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Optional_Group_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Optional_Group_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Optional_Group_Variable_Reference
    (Self  : aliased in out Optional_Group_Vector;
     Index : Positive)
      return Optional_Group_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Optional_Group_Variable_Reference;

   not overriding function Get_Optional_Group_Constant_Reference
    (Self  : aliased Optional_Group_Vector;
     Index : Positive)
      return Optional_Group_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Optional_Group_Constant_Reference;

   procedure Read_Optional_Group
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Optional_Group) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.A.Is_Set then
                  V.A := (True, others => <>);
               end if;
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.A.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Optional_Group;

   procedure Write_Optional_Group
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Optional_Group) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Optional_Group (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.A.Is_Set then
            WS.Write_Varint (1, V.A.Value);
         end if;
         if WS.End_Message then
            Write_Optional_Group (WS'Access, V);
         end if;
      end;
   end Write_Optional_Group;

end Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2;