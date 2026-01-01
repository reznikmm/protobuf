with Ada.Unchecked_Deallocation;
with Proto_Support.IO;
with Proto_Support.Internal;

package body Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2 is

   type Integer_Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Foreign_Enum_Proto_2 is
      range 0 .. 2
     with Size =>
       Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
         .Foreign_Enum_Proto_2'Size;

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Foreign_Enum_Proto_2_IO is
     new Proto_Support.IO.Enum_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Foreign_Enum_Proto_2,
        Integer_Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Foreign_Enum_Proto_2,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Foreign_Enum_Proto_2_Vectors);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Foreign_Message_Proto_2_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Foreign_Message_Proto_2,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Foreign_Message_Proto_2_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Test_All_Required_Types_Proto_2_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Test_All_Required_Types_Proto_2,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Test_All_Required_Types_Proto_2_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Test_All_Required_Types_Proto_2_Data_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Test_All_Required_Types_Proto_2_Data,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Test_All_Required_Types_Proto_2_Data_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   type Integer_Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Test_All_Required_Types_Proto_2_Nested_Enum is
      range  - 1 .. 2
     with Size =>
       Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
         .Test_All_Required_Types_Proto_2_Nested_Enum'Size;

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Test_All_Required_Types_Proto_2_Nested_Enum_IO is
     new Proto_Support.IO.Enum_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Test_All_Required_Types_Proto_2_Nested_Enum,
        Integer_Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Test_All_Required_Types_Proto_2_Nested_Enum,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Test_All_Required_Types_Proto_2_Nested_Enum_Vectors);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Test_All_Required_Types_Proto_2_Nested_Message_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Test_All_Required_Types_Proto_2_Nested_Message,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Test_All_Required_Types_Proto_2_Nested_Message_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Test_All_Types_Proto_2_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Test_All_Types_Proto_2,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Test_All_Types_Proto_2_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Data_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Data,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Data_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Bool_Bool_Entry_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Bool_Bool_Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Bool_Bool_Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Fixed_32Fixed_32Entry_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Fixed_32Fixed_32Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Fixed_32Fixed_32Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Fixed_64Fixed_64Entry_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Fixed_64Fixed_64Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Fixed_64Fixed_64Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Int_32Bool_Entry_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Int_32Bool_Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Int_32Bool_Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Int_32Double_Entry_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Int_32Double_Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Int_32Double_Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Int_32Float_Entry_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Int_32Float_Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Int_32Float_Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Int_32Int_32Entry_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Int_32Int_32Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Int_32Int_32Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Int_32Nested_Message_Entry_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Int_32Nested_Message_Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Int_32Nested_Message_Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Int_64Int_64Entry_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Int_64Int_64Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Int_64Int_64Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Sfixed_32Sfixed_32Entry_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Sfixed_32Sfixed_32Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Sfixed_32Sfixed_32Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Sfixed_64Sfixed_64Entry_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Sfixed_64Sfixed_64Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Sfixed_64Sfixed_64Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Sint_32Sint_32Entry_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Sint_32Sint_32Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Sint_32Sint_32Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Sint_64Sint_64Entry_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Sint_64Sint_64Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Sint_64Sint_64Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_String_Bytes_Entry_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_String_Bytes_Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_String_Bytes_Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_String_Foreign_Enum_Entry_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_String_Foreign_Enum_Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_String_Foreign_Enum_Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_String_Foreign_Message_Entry_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_String_Foreign_Message_Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_String_Foreign_Message_Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_String_Nested_Enum_Entry_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_String_Nested_Enum_Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_String_Nested_Enum_Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_String_Nested_Message_Entry_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_String_Nested_Message_Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_String_Nested_Message_Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_String_String_Entry_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_String_String_Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_String_String_Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Uint_32Uint_32Entry_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Uint_32Uint_32Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Uint_32Uint_32Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Uint_64Uint_64Entry_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Uint_64Uint_64Entry,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Map_Uint_64Uint_64Entry_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Message_Set_Correct_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Message_Set_Correct,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Message_Set_Correct_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Multi_Word_Group_Field_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Multi_Word_Group_Field,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Multi_Word_Group_Field_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   type Integer_Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Nested_Enum is
      range  - 1 .. 2
     with Size =>
       Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Nested_Enum'Size;

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Nested_Enum_IO is
     new Proto_Support.IO.Enum_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Nested_Enum,
        Integer_Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Nested_Enum,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Nested_Enum_Vectors);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Nested_Message_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Nested_Message,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Nested_Message_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_A1_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.A1,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.A1_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_A2_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.A2,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.A2_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_A3_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.A3,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.A3_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_A4_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.A4,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.A4_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_A5_IO is
     new Proto_Support.IO.Message_IO
       (Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.A5,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.A5_Vector,
        Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Append);

   package Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Optional_Group_IO is
     new Proto_Support.IO.Message_IO
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
      Aux_Data    : Test_All_Types_Proto_2_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Test_All_Types_Proto_2_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Test_All_Types_Proto_2_Array'
             (Self.Data.all
                & Test_All_Types_Proto_2_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Optional_Int_32.Is_Set then
                  V.Optional_Int_32 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Optional_Int_32.Value);
            when 2 =>
               if  not V.Optional_Int_64.Is_Set then
                  V.Optional_Int_64 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Optional_Int_64.Value);
            when 3 =>
               if  not V.Optional_Uint_32.Is_Set then
                  V.Optional_Uint_32 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Optional_Uint_32.Value);
            when 4 =>
               if  not V.Optional_Uint_64.Is_Set then
                  V.Optional_Uint_64 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Optional_Uint_64.Value);
            when 5 =>
               if  not V.Optional_Sint_32.Is_Set then
                  V.Optional_Sint_32 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Zigzag
                 (Stream, Key.Encoding, V.Optional_Sint_32.Value);
            when 6 =>
               if  not V.Optional_Sint_64.Is_Set then
                  V.Optional_Sint_64 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Zigzag
                 (Stream, Key.Encoding, V.Optional_Sint_64.Value);
            when 7 =>
               if  not V.Optional_Fixed_32.Is_Set then
                  V.Optional_Fixed_32 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Optional_Fixed_32.Value);
            when 8 =>
               if  not V.Optional_Fixed_64.Is_Set then
                  V.Optional_Fixed_64 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Optional_Fixed_64.Value);
            when 9 =>
               if  not V.Optional_Sfixed_32.Is_Set then
                  V.Optional_Sfixed_32 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Optional_Sfixed_32.Value);
            when 10 =>
               if  not V.Optional_Sfixed_64.Is_Set then
                  V.Optional_Sfixed_64 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Optional_Sfixed_64.Value);
            when 11 =>
               if  not V.Optional_Float.Is_Set then
                  V.Optional_Float := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Optional_Float.Value);
            when 12 =>
               if  not V.Optional_Double.Is_Set then
                  V.Optional_Double := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Optional_Double.Value);
            when 13 =>
               if  not V.Optional_Bool.Is_Set then
                  V.Optional_Bool := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Optional_Bool.Value);
            when 14 =>
               if  not V.Optional_String.Is_Set then
                  V.Optional_String := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Optional_String.Value);
            when 15 =>
               if  not V.Optional_Bytes.Is_Set then
                  V.Optional_Bytes := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Optional_Bytes.Value);
            when 18 =>
               if  not V.Optional_Nested_Message.Is_Set then
                  V.Optional_Nested_Message := (True, others => <>);
               end if;
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Nested_Message_IO
                 .Read
                 (Stream, Key.Encoding, V.Optional_Nested_Message.Value);
            when 19 =>
               if  not V.Optional_Foreign_Message.Is_Set then
                  V.Optional_Foreign_Message := (True, others => <>);
               end if;
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Foreign_Message_Proto_2_IO
                 .Read
                 (Stream, Key.Encoding, V.Optional_Foreign_Message.Value);
            when 21 =>
               if  not V.Optional_Nested_Enum.Is_Set then
                  V.Optional_Nested_Enum := (True, others => <>);
               end if;
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Nested_Enum_IO
                 .Read
                 (Stream, Key.Encoding, V.Optional_Nested_Enum.Value);
            when 22 =>
               if  not V.Optional_Foreign_Enum.Is_Set then
                  V.Optional_Foreign_Enum := (True, others => <>);
               end if;
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Foreign_Enum_Proto_2_IO
                 .Read
                 (Stream, Key.Encoding, V.Optional_Foreign_Enum.Value);
            when 24 =>
               if  not V.Optional_String_Piece.Is_Set then
                  V.Optional_String_Piece := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Optional_String_Piece.Value);
            when 25 =>
               if  not V.Optional_Cord.Is_Set then
                  V.Optional_Cord := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Optional_Cord.Value);
            when 27 =>
               if V.Recursive_Message.Length = 0 then
                  V.Recursive_Message.Append ((others => <>));
               end if;
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Test_All_Types_Proto_2_IO
                 .Read
                 (Stream, Key.Encoding, V.Recursive_Message (1));
            when 31 =>
               Proto_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Repeated_Int_32);
            when 32 =>
               Proto_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Repeated_Int_64);
            when 33 =>
               Proto_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Repeated_Uint_32);
            when 34 =>
               Proto_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Repeated_Uint_64);
            when 35 =>
               Proto_Support.IO.Read_Zigzag_Vector
                 (Stream, Key.Encoding, V.Repeated_Sint_32);
            when 36 =>
               Proto_Support.IO.Read_Zigzag_Vector
                 (Stream, Key.Encoding, V.Repeated_Sint_64);
            when 37 =>
               Proto_Support.IO.Read_Fixed_Vector
                 (Stream, Key.Encoding, V.Repeated_Fixed_32);
            when 38 =>
               Proto_Support.IO.Read_Fixed_Vector
                 (Stream, Key.Encoding, V.Repeated_Fixed_64);
            when 39 =>
               Proto_Support.IO.Read_Fixed_Vector
                 (Stream, Key.Encoding, V.Repeated_Sfixed_32);
            when 40 =>
               Proto_Support.IO.Read_Fixed_Vector
                 (Stream, Key.Encoding, V.Repeated_Sfixed_64);
            when 41 =>
               Proto_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Float);
            when 42 =>
               Proto_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Double);
            when 43 =>
               Proto_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Bool);
            when 44 =>
               Proto_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_String);
            when 45 =>
               Proto_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Bytes);
            when 48 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Nested_Message_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Nested_Message);
            when 49 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Foreign_Message_Proto_2_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Foreign_Message);
            when 51 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Nested_Enum_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Nested_Enum);
            when 52 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Foreign_Enum_Proto_2_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Foreign_Enum);
            when 54 =>
               Proto_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_String_Piece);
            when 55 =>
               Proto_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Repeated_Cord);
            when 75 =>
               Proto_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Packed_Int_32);
            when 76 =>
               Proto_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Packed_Int_64);
            when 77 =>
               Proto_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Packed_Uint_32);
            when 78 =>
               Proto_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Packed_Uint_64);
            when 79 =>
               Proto_Support.IO.Read_Zigzag_Vector
                 (Stream, Key.Encoding, V.Packed_Sint_32);
            when 80 =>
               Proto_Support.IO.Read_Zigzag_Vector
                 (Stream, Key.Encoding, V.Packed_Sint_64);
            when 81 =>
               Proto_Support.IO.Read_Fixed_Vector
                 (Stream, Key.Encoding, V.Packed_Fixed_32);
            when 82 =>
               Proto_Support.IO.Read_Fixed_Vector
                 (Stream, Key.Encoding, V.Packed_Fixed_64);
            when 83 =>
               Proto_Support.IO.Read_Fixed_Vector
                 (Stream, Key.Encoding, V.Packed_Sfixed_32);
            when 84 =>
               Proto_Support.IO.Read_Fixed_Vector
                 (Stream, Key.Encoding, V.Packed_Sfixed_64);
            when 85 =>
               Proto_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Packed_Float);
            when 86 =>
               Proto_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Packed_Double);
            when 87 =>
               Proto_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Packed_Bool);
            when 88 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Nested_Enum_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Packed_Nested_Enum);
            when 89 =>
               Proto_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Unpacked_Int_32);
            when 90 =>
               Proto_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Unpacked_Int_64);
            when 91 =>
               Proto_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Unpacked_Uint_32);
            when 92 =>
               Proto_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Unpacked_Uint_64);
            when 93 =>
               Proto_Support.IO.Read_Zigzag_Vector
                 (Stream, Key.Encoding, V.Unpacked_Sint_32);
            when 94 =>
               Proto_Support.IO.Read_Zigzag_Vector
                 (Stream, Key.Encoding, V.Unpacked_Sint_64);
            when 95 =>
               Proto_Support.IO.Read_Fixed_Vector
                 (Stream, Key.Encoding, V.Unpacked_Fixed_32);
            when 96 =>
               Proto_Support.IO.Read_Fixed_Vector
                 (Stream, Key.Encoding, V.Unpacked_Fixed_64);
            when 97 =>
               Proto_Support.IO.Read_Fixed_Vector
                 (Stream, Key.Encoding, V.Unpacked_Sfixed_32);
            when 98 =>
               Proto_Support.IO.Read_Fixed_Vector
                 (Stream, Key.Encoding, V.Unpacked_Sfixed_64);
            when 99 =>
               Proto_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Unpacked_Float);
            when 100 =>
               Proto_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Unpacked_Double);
            when 101 =>
               Proto_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Unpacked_Bool);
            when 102 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Nested_Enum_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Unpacked_Nested_Enum);
            when 56 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Int_32Int_32Entry_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Map_Int_32_Int_32);
            when 57 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Int_64Int_64Entry_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Map_Int_64_Int_64);
            when 58 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Uint_32Uint_32Entry_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Map_Uint_32_Uint_32);
            when 59 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Uint_64Uint_64Entry_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Map_Uint_64_Uint_64);
            when 60 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Sint_32Sint_32Entry_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Map_Sint_32_Sint_32);
            when 61 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Sint_64Sint_64Entry_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Map_Sint_64_Sint_64);
            when 62 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Fixed_32Fixed_32Entry_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Map_Fixed_32_Fixed_32);
            when 63 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Fixed_64Fixed_64Entry_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Map_Fixed_64_Fixed_64);
            when 64 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Sfixed_32Sfixed_32Entry_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Map_Sfixed_32_Sfixed_32);
            when 65 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Sfixed_64Sfixed_64Entry_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Map_Sfixed_64_Sfixed_64);
            when 104 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Int_32Bool_Entry_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Map_Int_32_Bool);
            when 66 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Int_32Float_Entry_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Map_Int_32_Float);
            when 67 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Int_32Double_Entry_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Map_Int_32_Double);
            when 103 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Int_32Nested_Message_Entry_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Map_Int_32_Nested_Message);
            when 68 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_Bool_Bool_Entry_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Map_Bool_Bool);
            when 69 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_String_String_Entry_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Map_String_String);
            when 70 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_String_Bytes_Entry_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Map_String_Bytes);
            when 71 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_String_Nested_Message_Entry_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Map_String_Nested_Message);
            when 72 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_String_Foreign_Message_Entry_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Map_String_Foreign_Message);
            when 73 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_String_Nested_Enum_Entry_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Map_String_Nested_Enum);
            when 74 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Map_String_Foreign_Enum_Entry_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Map_String_Foreign_Enum);
            when 111 =>
               if V.Variant.Oneof_Field /= Oneof_Uint_32_Kind then
                  V.Variant := (Oneof_Uint_32_Kind, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Variant.Oneof_Uint_32);
            when 112 =>
               if V.Variant.Oneof_Field /= Oneof_Nested_Message_Kind then
                  V.Variant := (Oneof_Nested_Message_Kind, others => <>);
               end if;
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Nested_Message_IO
                 .Read
                 (Stream, Key.Encoding, V.Variant.Oneof_Nested_Message);
            when 113 =>
               if V.Variant.Oneof_Field /= Oneof_String_Kind then
                  V.Variant := (Oneof_String_Kind, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Variant.Oneof_String);
            when 114 =>
               if V.Variant.Oneof_Field /= Oneof_Bytes_Kind then
                  V.Variant := (Oneof_Bytes_Kind, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Variant.Oneof_Bytes);
            when 115 =>
               if V.Variant.Oneof_Field /= Oneof_Bool_Kind then
                  V.Variant := (Oneof_Bool_Kind, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Variant.Oneof_Bool);
            when 116 =>
               if V.Variant.Oneof_Field /= Oneof_Uint_64_Kind then
                  V.Variant := (Oneof_Uint_64_Kind, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Variant.Oneof_Uint_64);
            when 117 =>
               if V.Variant.Oneof_Field /= Oneof_Float_Kind then
                  V.Variant := (Oneof_Float_Kind, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Variant.Oneof_Float);
            when 118 =>
               if V.Variant.Oneof_Field /= Oneof_Double_Kind then
                  V.Variant := (Oneof_Double_Kind, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Variant.Oneof_Double);
            when 119 =>
               if V.Variant.Oneof_Field /= Oneof_Enum_Kind then
                  V.Variant := (Oneof_Enum_Kind, others => <>);
               end if;
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Nested_Enum_IO
                 .Read
                 (Stream, Key.Encoding, V.Variant.Oneof_Enum);
            when 201 =>
               if  not V.Data.Is_Set then
                  V.Data := (True, others => <>);
               end if;
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Data_IO
                 .Read
                 (Stream, Key.Encoding, V.Data.Value);
            when 204 =>
               if  not V.Multiwordgroupfield.Is_Set then
                  V.Multiwordgroupfield := (True, others => <>);
               end if;
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Multi_Word_Group_Field_IO
                 .Read
                 (Stream, Key.Encoding, V.Multiwordgroupfield.Value);
            when 241 =>
               if  not V.Default_Int_32.Is_Set then
                  V.Default_Int_32 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Default_Int_32.Value);
            when 242 =>
               if  not V.Default_Int_64.Is_Set then
                  V.Default_Int_64 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Default_Int_64.Value);
            when 243 =>
               if  not V.Default_Uint_32.Is_Set then
                  V.Default_Uint_32 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Default_Uint_32.Value);
            when 244 =>
               if  not V.Default_Uint_64.Is_Set then
                  V.Default_Uint_64 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Default_Uint_64.Value);
            when 245 =>
               if  not V.Default_Sint_32.Is_Set then
                  V.Default_Sint_32 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Zigzag
                 (Stream, Key.Encoding, V.Default_Sint_32.Value);
            when 246 =>
               if  not V.Default_Sint_64.Is_Set then
                  V.Default_Sint_64 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Zigzag
                 (Stream, Key.Encoding, V.Default_Sint_64.Value);
            when 247 =>
               if  not V.Default_Fixed_32.Is_Set then
                  V.Default_Fixed_32 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Default_Fixed_32.Value);
            when 248 =>
               if  not V.Default_Fixed_64.Is_Set then
                  V.Default_Fixed_64 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Default_Fixed_64.Value);
            when 249 =>
               if  not V.Default_Sfixed_32.Is_Set then
                  V.Default_Sfixed_32 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Default_Sfixed_32.Value);
            when 250 =>
               if  not V.Default_Sfixed_64.Is_Set then
                  V.Default_Sfixed_64 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Default_Sfixed_64.Value);
            when 251 =>
               if  not V.Default_Float.Is_Set then
                  V.Default_Float := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Default_Float.Value);
            when 252 =>
               if  not V.Default_Double.Is_Set then
                  V.Default_Double := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Default_Double.Value);
            when 253 =>
               if  not V.Default_Bool.Is_Set then
                  V.Default_Bool := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Default_Bool.Value);
            when 254 =>
               if  not V.Default_String.Is_Set then
                  V.Default_String := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Default_String.Value);
            when 255 =>
               if  not V.Default_Bytes.Is_Set then
                  V.Default_Bytes := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Default_Bytes.Value);
            when 401 =>
               if  not V.Fieldname_1.Is_Set then
                  V.Fieldname_1 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Fieldname_1.Value);
            when 402 =>
               if  not V.Field_Name_2.Is_Set then
                  V.Field_Name_2 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_2.Value);
            when 403 =>
               if  not V.Field_Name_3.Is_Set then
                  V.Field_Name_3 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_3.Value);
            when 404 =>
               if  not V.Field_Name_4.Is_Set then
                  V.Field_Name_4 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_4.Value);
            when 405 =>
               if  not V.Field_0name_5.Is_Set then
                  V.Field_0name_5 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_0name_5.Value);
            when 406 =>
               if  not V.Field_0_Name_6.Is_Set then
                  V.Field_0_Name_6 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_0_Name_6.Value);
            when 407 =>
               if  not V.Field_Name_7.Is_Set then
                  V.Field_Name_7 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_7.Value);
            when 408 =>
               if  not V.Field_Name_8.Is_Set then
                  V.Field_Name_8 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_8.Value);
            when 409 =>
               if  not V.Field_Name_9.Is_Set then
                  V.Field_Name_9 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_9.Value);
            when 410 =>
               if  not V.Field_Name_10.Is_Set then
                  V.Field_Name_10 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_10.Value);
            when 411 =>
               if  not V.FIELD_NAME11.Is_Set then
                  V.FIELD_NAME11 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.FIELD_NAME11.Value);
            when 412 =>
               if  not V.FIELD_Name_12.Is_Set then
                  V.FIELD_Name_12 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.FIELD_Name_12.Value);
            when 413 =>
               if  not V.Field_Name_13.Is_Set then
                  V.Field_Name_13 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_13.Value);
            when 414 =>
               if  not V.Field_Name_14.Is_Set then
                  V.Field_Name_14 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_14.Value);
            when 415 =>
               if  not V.Field_Name_15.Is_Set then
                  V.Field_Name_15 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_15.Value);
            when 416 =>
               if  not V.Field_Name_16.Is_Set then
                  V.Field_Name_16 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_16.Value);
            when 417 =>
               if  not V.Field_Name_17.Is_Set then
                  V.Field_Name_17 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_17.Value);
            when 418 =>
               if  not V.Field_Name_18.Is_Set then
                  V.Field_Name_18 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Field_Name_18.Value);
            when 500 =>
               if  not V.Message_Set_Correct.Is_Set then
                  V.Message_Set_Correct := (True, others => <>);
               end if;
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Message_Set_Correct_IO
                 .Read
                 (Stream, Key.Encoding, V.Message_Set_Correct.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Test_All_Types_Proto_2;

   procedure Write_Test_All_Types_Proto_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Test_All_Types_Proto_2) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Test_All_Types_Proto_2 (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
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
            WS.Write_Key ((18, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Nested_Message'Write
              (Stream, V.Optional_Nested_Message.Value);
         end if;
         if V.Optional_Foreign_Message.Is_Set then
            WS.Write_Key ((19, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Foreign_Message_Proto_2'Write
              (Stream, V.Optional_Foreign_Message.Value);
         end if;
         if V.Optional_Nested_Enum.Is_Set then
            Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Nested_Enum_IO
              .Write
              (WS, 21, V.Optional_Nested_Enum.Value);
         end if;
         if V.Optional_Foreign_Enum.Is_Set then
            Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Foreign_Enum_Proto_2_IO
              .Write
              (WS, 22, V.Optional_Foreign_Enum.Value);
         end if;
         if V.Optional_String_Piece.Is_Set then
            WS.Write (24, V.Optional_String_Piece.Value);
         end if;
         if V.Optional_Cord.Is_Set then
            WS.Write (25, V.Optional_Cord.Value);
         end if;
         for J in 1 .. V.Recursive_Message.Length loop
            WS.Write_Key ((27, Proto_Support.Length_Delimited));
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
            WS.Write_Key ((48, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Nested_Message'Write
              (Stream, V.Repeated_Nested_Message (J));
         end loop;
         for J in 1 .. V.Repeated_Foreign_Message.Length loop
            WS.Write_Key ((49, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Foreign_Message_Proto_2'Write
              (Stream, V.Repeated_Foreign_Message (J));
         end loop;
         Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Nested_Enum_IO
           .Write
           (WS, 51, V.Repeated_Nested_Enum);
         Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Foreign_Enum_Proto_2_IO
           .Write
           (WS, 52, V.Repeated_Foreign_Enum);
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
         Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Nested_Enum_IO
           .Write_Packed
           (WS, 88, V.Packed_Nested_Enum);
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
         Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Nested_Enum_IO
           .Write
           (WS, 102, V.Unpacked_Nested_Enum);
         for J in 1 .. V.Map_Int_32_Int_32.Length loop
            WS.Write_Key ((56, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Int_32Int_32Entry'Write
              (Stream, V.Map_Int_32_Int_32 (J));
         end loop;
         for J in 1 .. V.Map_Int_64_Int_64.Length loop
            WS.Write_Key ((57, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Int_64Int_64Entry'Write
              (Stream, V.Map_Int_64_Int_64 (J));
         end loop;
         for J in 1 .. V.Map_Uint_32_Uint_32.Length loop
            WS.Write_Key ((58, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Uint_32Uint_32Entry'Write
              (Stream, V.Map_Uint_32_Uint_32 (J));
         end loop;
         for J in 1 .. V.Map_Uint_64_Uint_64.Length loop
            WS.Write_Key ((59, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Uint_64Uint_64Entry'Write
              (Stream, V.Map_Uint_64_Uint_64 (J));
         end loop;
         for J in 1 .. V.Map_Sint_32_Sint_32.Length loop
            WS.Write_Key ((60, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Sint_32Sint_32Entry'Write
              (Stream, V.Map_Sint_32_Sint_32 (J));
         end loop;
         for J in 1 .. V.Map_Sint_64_Sint_64.Length loop
            WS.Write_Key ((61, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Sint_64Sint_64Entry'Write
              (Stream, V.Map_Sint_64_Sint_64 (J));
         end loop;
         for J in 1 .. V.Map_Fixed_32_Fixed_32.Length loop
            WS.Write_Key ((62, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Fixed_32Fixed_32Entry'Write
              (Stream, V.Map_Fixed_32_Fixed_32 (J));
         end loop;
         for J in 1 .. V.Map_Fixed_64_Fixed_64.Length loop
            WS.Write_Key ((63, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Fixed_64Fixed_64Entry'Write
              (Stream, V.Map_Fixed_64_Fixed_64 (J));
         end loop;
         for J in 1 .. V.Map_Sfixed_32_Sfixed_32.Length loop
            WS.Write_Key ((64, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Sfixed_32Sfixed_32Entry'Write
              (Stream, V.Map_Sfixed_32_Sfixed_32 (J));
         end loop;
         for J in 1 .. V.Map_Sfixed_64_Sfixed_64.Length loop
            WS.Write_Key ((65, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Sfixed_64Sfixed_64Entry'Write
              (Stream, V.Map_Sfixed_64_Sfixed_64 (J));
         end loop;
         for J in 1 .. V.Map_Int_32_Bool.Length loop
            WS.Write_Key ((104, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Int_32Bool_Entry'Write
              (Stream, V.Map_Int_32_Bool (J));
         end loop;
         for J in 1 .. V.Map_Int_32_Float.Length loop
            WS.Write_Key ((66, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Int_32Float_Entry'Write
              (Stream, V.Map_Int_32_Float (J));
         end loop;
         for J in 1 .. V.Map_Int_32_Double.Length loop
            WS.Write_Key ((67, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Int_32Double_Entry'Write
              (Stream, V.Map_Int_32_Double (J));
         end loop;
         for J in 1 .. V.Map_Int_32_Nested_Message.Length loop
            WS.Write_Key ((103, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Int_32Nested_Message_Entry'Write
              (Stream, V.Map_Int_32_Nested_Message (J));
         end loop;
         for J in 1 .. V.Map_Bool_Bool.Length loop
            WS.Write_Key ((68, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_Bool_Bool_Entry'Write
              (Stream, V.Map_Bool_Bool (J));
         end loop;
         for J in 1 .. V.Map_String_String.Length loop
            WS.Write_Key ((69, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_String_String_Entry'Write
              (Stream, V.Map_String_String (J));
         end loop;
         for J in 1 .. V.Map_String_Bytes.Length loop
            WS.Write_Key ((70, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_String_Bytes_Entry'Write
              (Stream, V.Map_String_Bytes (J));
         end loop;
         for J in 1 .. V.Map_String_Nested_Message.Length loop
            WS.Write_Key ((71, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_String_Nested_Message_Entry'Write
              (Stream, V.Map_String_Nested_Message (J));
         end loop;
         for J in 1 .. V.Map_String_Foreign_Message.Length loop
            WS.Write_Key ((72, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_String_Foreign_Message_Entry'Write
              (Stream, V.Map_String_Foreign_Message (J));
         end loop;
         for J in 1 .. V.Map_String_Nested_Enum.Length loop
            WS.Write_Key ((73, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_String_Nested_Enum_Entry'Write
              (Stream, V.Map_String_Nested_Enum (J));
         end loop;
         for J in 1 .. V.Map_String_Foreign_Enum.Length loop
            WS.Write_Key ((74, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Map_String_Foreign_Enum_Entry'Write
              (Stream, V.Map_String_Foreign_Enum (J));
         end loop;
         if V.Data.Is_Set then
            WS.Write_Key ((201, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.Data'Write
              (Stream, V.Data.Value);
         end if;
         if V.Multiwordgroupfield.Is_Set then
            WS.Write_Key ((204, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Multi_Word_Group_Field'Write
              (Stream, V.Multiwordgroupfield.Value);
         end if;
         if V.Default_Int_32.Is_Set then
            WS.Write_Varint (241, V.Default_Int_32.Value);
         end if;
         if V.Default_Int_64.Is_Set then
            WS.Write_Varint (242, V.Default_Int_64.Value);
         end if;
         if V.Default_Uint_32.Is_Set then
            WS.Write_Varint (243, V.Default_Uint_32.Value);
         end if;
         if V.Default_Uint_64.Is_Set then
            WS.Write_Varint (244, V.Default_Uint_64.Value);
         end if;
         if V.Default_Sint_32.Is_Set then
            WS.Write_Zigzag (245, V.Default_Sint_32.Value);
         end if;
         if V.Default_Sint_64.Is_Set then
            WS.Write_Zigzag (246, V.Default_Sint_64.Value);
         end if;
         if V.Default_Fixed_32.Is_Set then
            WS.Write_Fixed (247, V.Default_Fixed_32.Value);
         end if;
         if V.Default_Fixed_64.Is_Set then
            WS.Write_Fixed (248, V.Default_Fixed_64.Value);
         end if;
         if V.Default_Sfixed_32.Is_Set then
            WS.Write_Fixed (249, V.Default_Sfixed_32.Value);
         end if;
         if V.Default_Sfixed_64.Is_Set then
            WS.Write_Fixed (250, V.Default_Sfixed_64.Value);
         end if;
         if V.Default_Float.Is_Set then
            WS.Write (251, V.Default_Float.Value);
         end if;
         if V.Default_Double.Is_Set then
            WS.Write (252, V.Default_Double.Value);
         end if;
         if V.Default_Bool.Is_Set then
            WS.Write (253, V.Default_Bool.Value);
         end if;
         if V.Default_String.Is_Set then
            WS.Write (254, V.Default_String.Value);
         end if;
         if V.Default_Bytes.Is_Set then
            WS.Write (255, V.Default_Bytes.Value);
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
         if V.Message_Set_Correct.Is_Set then
            WS.Write_Key ((500, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Message_Set_Correct'Write
              (Stream, V.Message_Set_Correct.Value);
         end if;
         case V.Variant.Oneof_Field is
            when Oneof_Uint_32_Kind =>
               WS.Write_Varint (111, V.Variant.Oneof_Uint_32);
            when Oneof_Nested_Message_Kind =>
               WS.Write_Key ((112, Proto_Support.Length_Delimited));
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
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Nested_Enum_IO
                 .Write
                 (WS, 119, V.Variant.Oneof_Enum);
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
      Aux_Data    : Nested_Message_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Nested_Message_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Nested_Message_Array'
             (Self.Data.all & Nested_Message_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.A.Is_Set then
                  V.A := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint (Stream, Key.Encoding, V.A.Value);
            when 2 =>
               if V.Corecursive.Length = 0 then
                  V.Corecursive.Append ((others => <>));
               end if;
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Test_All_Types_Proto_2_IO
                 .Read
                 (Stream, Key.Encoding, V.Corecursive (1));
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Nested_Message;

   procedure Write_Nested_Message
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Nested_Message) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Nested_Message (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.A.Is_Set then
            WS.Write_Varint (1, V.A.Value);
         end if;
         for J in 1 .. V.Corecursive.Length loop
            WS.Write_Key ((2, Proto_Support.Length_Delimited));
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
      Aux_Data    : Map_Int_32Int_32Entry_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Map_Int_32Int_32Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Map_Int_32Int_32Entry_Array'
             (Self.Data.all
                & Map_Int_32Int_32Entry_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Value.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Int_32Int_32Entry;

   procedure Write_Map_Int_32Int_32Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Int_32Int_32Entry) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Map_Int_32Int_32Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
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
      Aux_Data    : Map_Int_64Int_64Entry_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Map_Int_64Int_64Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Map_Int_64Int_64Entry_Array'
             (Self.Data.all
                & Map_Int_64Int_64Entry_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Value.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Int_64Int_64Entry;

   procedure Write_Map_Int_64Int_64Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Int_64Int_64Entry) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Map_Int_64Int_64Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
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
      Aux_Data    : Map_Uint_32Uint_32Entry_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Map_Uint_32Uint_32Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Map_Uint_32Uint_32Entry_Array'
             (Self.Data.all
                & Map_Uint_32Uint_32Entry_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Value.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Uint_32Uint_32Entry;

   procedure Write_Map_Uint_32Uint_32Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Uint_32Uint_32Entry) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Map_Uint_32Uint_32Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
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
      Aux_Data    : Map_Uint_64Uint_64Entry_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Map_Uint_64Uint_64Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Map_Uint_64Uint_64Entry_Array'
             (Self.Data.all
                & Map_Uint_64Uint_64Entry_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Value.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Uint_64Uint_64Entry;

   procedure Write_Map_Uint_64Uint_64Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Uint_64Uint_64Entry) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Map_Uint_64Uint_64Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
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
      Aux_Data    : Map_Sint_32Sint_32Entry_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Map_Sint_32Sint_32Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Map_Sint_32Sint_32Entry_Array'
             (Self.Data.all
                & Map_Sint_32Sint_32Entry_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Zigzag
                 (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Zigzag
                 (Stream, Key.Encoding, V.Value.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Sint_32Sint_32Entry;

   procedure Write_Map_Sint_32Sint_32Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Sint_32Sint_32Entry) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Map_Sint_32Sint_32Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
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
      Aux_Data    : Map_Sint_64Sint_64Entry_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Map_Sint_64Sint_64Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Map_Sint_64Sint_64Entry_Array'
             (Self.Data.all
                & Map_Sint_64Sint_64Entry_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Zigzag
                 (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Zigzag
                 (Stream, Key.Encoding, V.Value.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Sint_64Sint_64Entry;

   procedure Write_Map_Sint_64Sint_64Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Sint_64Sint_64Entry) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Map_Sint_64Sint_64Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
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
      Aux_Data    : Map_Fixed_32Fixed_32Entry_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Map_Fixed_32Fixed_32Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Map_Fixed_32Fixed_32Entry_Array'
             (Self.Data.all
                & Map_Fixed_32Fixed_32Entry_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Fixed (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Value.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Fixed_32Fixed_32Entry;

   procedure Write_Map_Fixed_32Fixed_32Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Fixed_32Fixed_32Entry) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Map_Fixed_32Fixed_32Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
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
      Aux_Data    : Map_Fixed_64Fixed_64Entry_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Map_Fixed_64Fixed_64Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Map_Fixed_64Fixed_64Entry_Array'
             (Self.Data.all
                & Map_Fixed_64Fixed_64Entry_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Fixed (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Value.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Fixed_64Fixed_64Entry;

   procedure Write_Map_Fixed_64Fixed_64Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Fixed_64Fixed_64Entry) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Map_Fixed_64Fixed_64Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
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
      Aux_Data    : Map_Sfixed_32Sfixed_32Entry_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=
            new Map_Sfixed_32Sfixed_32Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Map_Sfixed_32Sfixed_32Entry_Array'
             (Self.Data.all
                & Map_Sfixed_32Sfixed_32Entry_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Fixed (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Value.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Sfixed_32Sfixed_32Entry;

   procedure Write_Map_Sfixed_32Sfixed_32Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Sfixed_32Sfixed_32Entry) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Map_Sfixed_32Sfixed_32Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
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
      Aux_Data    : Map_Sfixed_64Sfixed_64Entry_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=
            new Map_Sfixed_64Sfixed_64Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Map_Sfixed_64Sfixed_64Entry_Array'
             (Self.Data.all
                & Map_Sfixed_64Sfixed_64Entry_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Fixed (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Value.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Sfixed_64Sfixed_64Entry;

   procedure Write_Map_Sfixed_64Sfixed_64Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Sfixed_64Sfixed_64Entry) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Map_Sfixed_64Sfixed_64Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
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

   function Length (Self : Map_Int_32Bool_Entry_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Map_Int_32Bool_Entry_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Map_Int_32Bool_Entry_Array, Map_Int_32Bool_Entry_Array_Access);

   procedure Append
    (Self : in out Map_Int_32Bool_Entry_Vector;
     V    : Map_Int_32Bool_Entry) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Map_Int_32Bool_Entry'Size);
      Aux_Data    : Map_Int_32Bool_Entry_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Map_Int_32Bool_Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Map_Int_32Bool_Entry_Array'
             (Self.Data.all
                & Map_Int_32Bool_Entry_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Map_Int_32Bool_Entry_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Map_Int_32Bool_Entry_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Map_Int_32Bool_Entry_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Map_Int_32Bool_Entry_Variable_Reference
    (Self  : aliased in out Map_Int_32Bool_Entry_Vector;
     Index : Positive)
      return Map_Int_32Bool_Entry_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Int_32Bool_Entry_Variable_Reference;

   not overriding function Get_Map_Int_32Bool_Entry_Constant_Reference
    (Self  : aliased Map_Int_32Bool_Entry_Vector;
     Index : Positive)
      return Map_Int_32Bool_Entry_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Int_32Bool_Entry_Constant_Reference;

   procedure Read_Map_Int_32Bool_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Int_32Bool_Entry) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Value.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Int_32Bool_Entry;

   procedure Write_Map_Int_32Bool_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Int_32Bool_Entry) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Map_Int_32Bool_Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Key.Is_Set then
            WS.Write_Varint (1, V.Key.Value);
         end if;
         if V.Value.Is_Set then
            WS.Write (2, V.Value.Value);
         end if;
         if WS.End_Message then
            Write_Map_Int_32Bool_Entry (WS'Access, V);
         end if;
      end;
   end Write_Map_Int_32Bool_Entry;

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
      Aux_Data    : Map_Int_32Float_Entry_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Map_Int_32Float_Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Map_Int_32Float_Entry_Array'
             (Self.Data.all
                & Map_Int_32Float_Entry_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Value.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Int_32Float_Entry;

   procedure Write_Map_Int_32Float_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Int_32Float_Entry) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Map_Int_32Float_Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
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
      Aux_Data    : Map_Int_32Double_Entry_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Map_Int_32Double_Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Map_Int_32Double_Entry_Array'
             (Self.Data.all
                & Map_Int_32Double_Entry_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Value.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Int_32Double_Entry;

   procedure Write_Map_Int_32Double_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Int_32Double_Entry) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Map_Int_32Double_Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
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

   function Length
    (Self : Map_Int_32Nested_Message_Entry_Vector)
      return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Map_Int_32Nested_Message_Entry_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Map_Int_32Nested_Message_Entry_Array,
      Map_Int_32Nested_Message_Entry_Array_Access);

   procedure Append
    (Self : in out Map_Int_32Nested_Message_Entry_Vector;
     V    : Map_Int_32Nested_Message_Entry) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Map_Int_32Nested_Message_Entry'Size);
      Aux_Data    : Map_Int_32Nested_Message_Entry_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=
            new Map_Int_32Nested_Message_Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Map_Int_32Nested_Message_Entry_Array'
             (Self.Data.all
                & Map_Int_32Nested_Message_Entry_Array'
                  (1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Map_Int_32Nested_Message_Entry_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Map_Int_32Nested_Message_Entry_Array'
             (Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Map_Int_32Nested_Message_Entry_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Map_Int_32Nested_Message_Entry_Variable_Reference
    (Self  : aliased in out Map_Int_32Nested_Message_Entry_Vector;
     Index : Positive)
      return Map_Int_32Nested_Message_Entry_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Int_32Nested_Message_Entry_Variable_Reference;

   not overriding function Get_Map_Int_32Nested_Message_Entry_Constant_Reference
    (Self  : aliased Map_Int_32Nested_Message_Entry_Vector;
     Index : Positive)
      return Map_Int_32Nested_Message_Entry_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Map_Int_32Nested_Message_Entry_Constant_Reference;

   procedure Read_Map_Int_32Nested_Message_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Int_32Nested_Message_Entry) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Nested_Message_IO
                 .Read
                 (Stream, Key.Encoding, V.Value.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Int_32Nested_Message_Entry;

   procedure Write_Map_Int_32Nested_Message_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Int_32Nested_Message_Entry) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Map_Int_32Nested_Message_Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Key.Is_Set then
            WS.Write_Varint (1, V.Key.Value);
         end if;
         if V.Value.Is_Set then
            WS.Write_Key ((2, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Nested_Message'Write
              (Stream, V.Value.Value);
         end if;
         if WS.End_Message then
            Write_Map_Int_32Nested_Message_Entry (WS'Access, V);
         end if;
      end;
   end Write_Map_Int_32Nested_Message_Entry;

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
      Aux_Data    : Map_Bool_Bool_Entry_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Map_Bool_Bool_Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Map_Bool_Bool_Entry_Array'
             (Self.Data.all
                & Map_Bool_Bool_Entry_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Value.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_Bool_Bool_Entry;

   procedure Write_Map_Bool_Bool_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Bool_Bool_Entry) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Map_Bool_Bool_Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
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
      Aux_Data    : Map_String_String_Entry_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Map_String_String_Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Map_String_String_Entry_Array'
             (Self.Data.all
                & Map_String_String_Entry_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Value.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_String_String_Entry;

   procedure Write_Map_String_String_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_String_String_Entry) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Map_String_String_Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
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
      Aux_Data    : Map_String_Bytes_Entry_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Map_String_Bytes_Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Map_String_Bytes_Entry_Array'
             (Self.Data.all
                & Map_String_Bytes_Entry_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Value.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_String_Bytes_Entry;

   procedure Write_Map_String_Bytes_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_String_Bytes_Entry) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Map_String_Bytes_Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
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
      Aux_Data    : Map_String_Nested_Message_Entry_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=
            new Map_String_Nested_Message_Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Map_String_Nested_Message_Entry_Array'
             (Self.Data.all
                & Map_String_Nested_Message_Entry_Array'
                  (1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Nested_Message_IO
                 .Read
                 (Stream, Key.Encoding, V.Value.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_String_Nested_Message_Entry;

   procedure Write_Map_String_Nested_Message_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_String_Nested_Message_Entry) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Map_String_Nested_Message_Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Key.Is_Set then
            WS.Write (1, V.Key.Value);
         end if;
         if V.Value.Is_Set then
            WS.Write_Key ((2, Proto_Support.Length_Delimited));
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
      Aux_Data    : Map_String_Foreign_Message_Entry_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=
            new Map_String_Foreign_Message_Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Map_String_Foreign_Message_Entry_Array'
             (Self.Data.all
                & Map_String_Foreign_Message_Entry_Array'
                  (1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Foreign_Message_Proto_2_IO
                 .Read
                 (Stream, Key.Encoding, V.Value.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_String_Foreign_Message_Entry;

   procedure Write_Map_String_Foreign_Message_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_String_Foreign_Message_Entry) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Map_String_Foreign_Message_Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Key.Is_Set then
            WS.Write (1, V.Key.Value);
         end if;
         if V.Value.Is_Set then
            WS.Write_Key ((2, Proto_Support.Length_Delimited));
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
      Aux_Data    : Map_String_Nested_Enum_Entry_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=
            new Map_String_Nested_Enum_Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Map_String_Nested_Enum_Entry_Array'
             (Self.Data.all
                & Map_String_Nested_Enum_Entry_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Nested_Enum_IO
                 .Read
                 (Stream, Key.Encoding, V.Value.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_String_Nested_Enum_Entry;

   procedure Write_Map_String_Nested_Enum_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_String_Nested_Enum_Entry) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Map_String_Nested_Enum_Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Key.Is_Set then
            WS.Write (1, V.Key.Value);
         end if;
         if V.Value.Is_Set then
            Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Nested_Enum_IO
              .Write
              (WS, 2, V.Value.Value);
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
      Aux_Data    : Map_String_Foreign_Enum_Entry_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=
            new Map_String_Foreign_Enum_Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Map_String_Foreign_Enum_Entry_Array'
             (Self.Data.all
                & Map_String_Foreign_Enum_Entry_Array'
                  (1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Key.Is_Set then
                  V.Key := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Key.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Foreign_Enum_Proto_2_IO
                 .Read
                 (Stream, Key.Encoding, V.Value.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Map_String_Foreign_Enum_Entry;

   procedure Write_Map_String_Foreign_Enum_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_String_Foreign_Enum_Entry) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Map_String_Foreign_Enum_Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Key.Is_Set then
            WS.Write (1, V.Key.Value);
         end if;
         if V.Value.Is_Set then
            Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Foreign_Enum_Proto_2_IO
              .Write
              (WS, 2, V.Value.Value);
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
      Aux_Data    : Data_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Data_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Data_Array'
             (Self.Data.all & Data_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 202 =>
               if  not V.Group_Int_32.Is_Set then
                  V.Group_Int_32 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Group_Int_32.Value);
            when 203 =>
               if  not V.Group_Uint_32.Is_Set then
                  V.Group_Uint_32 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Group_Uint_32.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Data;

   procedure Write_Data
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Data) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Data (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
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

   function Length (Self : Multi_Word_Group_Field_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Multi_Word_Group_Field_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Multi_Word_Group_Field_Array, Multi_Word_Group_Field_Array_Access);

   procedure Append
    (Self : in out Multi_Word_Group_Field_Vector;
     V    : Multi_Word_Group_Field) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Multi_Word_Group_Field'Size);
      Aux_Data    : Multi_Word_Group_Field_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Multi_Word_Group_Field_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Multi_Word_Group_Field_Array'
             (Self.Data.all
                & Multi_Word_Group_Field_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Multi_Word_Group_Field_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Multi_Word_Group_Field_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Multi_Word_Group_Field_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Multi_Word_Group_Field_Variable_Reference
    (Self  : aliased in out Multi_Word_Group_Field_Vector;
     Index : Positive)
      return Multi_Word_Group_Field_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Multi_Word_Group_Field_Variable_Reference;

   not overriding function Get_Multi_Word_Group_Field_Constant_Reference
    (Self  : aliased Multi_Word_Group_Field_Vector;
     Index : Positive)
      return Multi_Word_Group_Field_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Multi_Word_Group_Field_Constant_Reference;

   procedure Read_Multi_Word_Group_Field
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Multi_Word_Group_Field) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 205 =>
               if  not V.Group_Int_32.Is_Set then
                  V.Group_Int_32 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Group_Int_32.Value);
            when 206 =>
               if  not V.Group_Uint_32.Is_Set then
                  V.Group_Uint_32 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Group_Uint_32.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Multi_Word_Group_Field;

   procedure Write_Multi_Word_Group_Field
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Multi_Word_Group_Field) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Multi_Word_Group_Field (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Group_Int_32.Is_Set then
            WS.Write_Varint (205, V.Group_Int_32.Value);
         end if;
         if V.Group_Uint_32.Is_Set then
            WS.Write_Varint (206, V.Group_Uint_32.Value);
         end if;
         if WS.End_Message then
            Write_Multi_Word_Group_Field (WS'Access, V);
         end if;
      end;
   end Write_Multi_Word_Group_Field;

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
      Aux_Data    : Message_Set_Correct_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Message_Set_Correct_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Message_Set_Correct_Array'
             (Self.Data.all
                & Message_Set_Correct_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Message_Set_Correct;

   procedure Write_Message_Set_Correct
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Message_Set_Correct) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Message_Set_Correct (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
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
      Aux_Data    : Message_Set_Correct_Extension_1_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=
            new Message_Set_Correct_Extension_1_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Message_Set_Correct_Extension_1_Array'
             (Self.Data.all
                & Message_Set_Correct_Extension_1_Array'
                  (1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 25 =>
               if  not V.Str.Is_Set then
                  V.Str := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Str.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Message_Set_Correct_Extension_1;

   procedure Write_Message_Set_Correct_Extension_1
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Message_Set_Correct_Extension_1) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Message_Set_Correct_Extension_1 (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
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
      Aux_Data    : Message_Set_Correct_Extension_2_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=
            new Message_Set_Correct_Extension_2_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Message_Set_Correct_Extension_2_Array'
             (Self.Data.all
                & Message_Set_Correct_Extension_2_Array'
                  (1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 9 =>
               if  not V.I.Is_Set then
                  V.I := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint (Stream, Key.Encoding, V.I.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Message_Set_Correct_Extension_2;

   procedure Write_Message_Set_Correct_Extension_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Message_Set_Correct_Extension_2) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Message_Set_Correct_Extension_2 (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
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

   function Length (Self : Extension_With_Oneof_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Extension_With_Oneof_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Extension_With_Oneof_Array, Extension_With_Oneof_Array_Access);

   procedure Append
    (Self : in out Extension_With_Oneof_Vector;
     V    : Extension_With_Oneof) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Extension_With_Oneof'Size);
      Aux_Data    : Extension_With_Oneof_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Extension_With_Oneof_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Extension_With_Oneof_Array'
             (Self.Data.all
                & Extension_With_Oneof_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Extension_With_Oneof_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Extension_With_Oneof_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Extension_With_Oneof_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Extension_With_Oneof_Variable_Reference
    (Self  : aliased in out Extension_With_Oneof_Vector;
     Index : Positive)
      return Extension_With_Oneof_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Extension_With_Oneof_Variable_Reference;

   not overriding function Get_Extension_With_Oneof_Constant_Reference
    (Self  : aliased Extension_With_Oneof_Vector;
     Index : Positive)
      return Extension_With_Oneof_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Extension_With_Oneof_Constant_Reference;

   procedure Read_Extension_With_Oneof
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Extension_With_Oneof) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if V.Variant.Oneof_Field /= A_Kind then
                  V.Variant := (A_Kind, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Variant.A);
            when 2 =>
               if V.Variant.Oneof_Field /= B_Kind then
                  V.Variant := (B_Kind, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Variant.B);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Extension_With_Oneof;

   procedure Write_Extension_With_Oneof
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Extension_With_Oneof) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Extension_With_Oneof (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         case V.Variant.Oneof_Field is
            when A_Kind =>
               WS.Write_Varint (1, V.Variant.A);
            when B_Kind =>
               WS.Write_Varint (2, V.Variant.B);
            when Oneof_Field_Not_Set =>
               null;
         end case;
         if WS.End_Message then
            Write_Extension_With_Oneof (WS'Access, V);
         end if;
      end;
   end Write_Extension_With_Oneof;

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
      Aux_Data    : Foreign_Message_Proto_2_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Foreign_Message_Proto_2_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Foreign_Message_Proto_2_Array'
             (Self.Data.all
                & Foreign_Message_Proto_2_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.C.Is_Set then
                  V.C := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint (Stream, Key.Encoding, V.C.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Foreign_Message_Proto_2;

   procedure Write_Foreign_Message_Proto_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Foreign_Message_Proto_2) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Foreign_Message_Proto_2 (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
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

   function Length (Self : Group_Field_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Group_Field_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Group_Field_Array, Group_Field_Array_Access);

   procedure Append (Self : in out Group_Field_Vector; V    : Group_Field) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Group_Field'Size);
      Aux_Data    : Group_Field_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Group_Field_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Group_Field_Array'
             (Self.Data.all & Group_Field_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Group_Field_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Group_Field_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Group_Field_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Group_Field_Variable_Reference
    (Self  : aliased in out Group_Field_Vector;
     Index : Positive)
      return Group_Field_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Group_Field_Variable_Reference;

   not overriding function Get_Group_Field_Constant_Reference
    (Self  : aliased Group_Field_Vector;
     Index : Positive)
      return Group_Field_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Group_Field_Constant_Reference;

   procedure Read_Group_Field
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Group_Field) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 122 =>
               if  not V.Group_Int_32.Is_Set then
                  V.Group_Int_32 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Group_Int_32.Value);
            when 123 =>
               if  not V.Group_Uint_32.Is_Set then
                  V.Group_Uint_32 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Group_Uint_32.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Group_Field;

   procedure Write_Group_Field
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Group_Field) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Group_Field (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Group_Int_32.Is_Set then
            WS.Write_Varint (122, V.Group_Int_32.Value);
         end if;
         if V.Group_Uint_32.Is_Set then
            WS.Write_Varint (123, V.Group_Uint_32.Value);
         end if;
         if WS.End_Message then
            Write_Group_Field (WS'Access, V);
         end if;
      end;
   end Write_Group_Field;

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
      Aux_Data    : Unknown_To_Test_All_Types_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Unknown_To_Test_All_Types_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Unknown_To_Test_All_Types_Array'
             (Self.Data.all
                & Unknown_To_Test_All_Types_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1001 =>
               if  not V.Optional_Int_32.Is_Set then
                  V.Optional_Int_32 := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Optional_Int_32.Value);
            when 1002 =>
               if  not V.Optional_String.Is_Set then
                  V.Optional_String := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Optional_String.Value);
            when 1003 =>
               if  not V.Nested_Message.Is_Set then
                  V.Nested_Message := (True, others => <>);
               end if;
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Foreign_Message_Proto_2_IO
                 .Read
                 (Stream, Key.Encoding, V.Nested_Message.Value);
            when 1004 =>
               if  not V.Optionalgroup.Is_Set then
                  V.Optionalgroup := (True, others => <>);
               end if;
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Optional_Group_IO
                 .Read
                 (Stream, Key.Encoding, V.Optionalgroup.Value);
            when 1006 =>
               if  not V.Optional_Bool.Is_Set then
                  V.Optional_Bool := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Optional_Bool.Value);
            when 1011 =>
               Proto_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Repeated_Int_32);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Unknown_To_Test_All_Types;

   procedure Write_Unknown_To_Test_All_Types
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Unknown_To_Test_All_Types) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Unknown_To_Test_All_Types (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Optional_Int_32.Is_Set then
            WS.Write_Varint (1001, V.Optional_Int_32.Value);
         end if;
         if V.Optional_String.Is_Set then
            WS.Write (1002, V.Optional_String.Value);
         end if;
         if V.Nested_Message.Is_Set then
            WS.Write_Key ((1003, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Foreign_Message_Proto_2'Write
              (Stream, V.Nested_Message.Value);
         end if;
         if V.Optionalgroup.Is_Set then
            WS.Write_Key ((1004, Proto_Support.Length_Delimited));
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
      Aux_Data    : Optional_Group_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Optional_Group_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Optional_Group_Array'
             (Self.Data.all & Optional_Group_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
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
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.A.Is_Set then
                  V.A := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint (Stream, Key.Encoding, V.A.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Optional_Group;

   procedure Write_Optional_Group
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Optional_Group) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Optional_Group (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
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

   function Length (Self : Null_Hypothesis_Proto_2_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Null_Hypothesis_Proto_2_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Null_Hypothesis_Proto_2_Array, Null_Hypothesis_Proto_2_Array_Access);

   procedure Append
    (Self : in out Null_Hypothesis_Proto_2_Vector;
     V    : Null_Hypothesis_Proto_2) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Null_Hypothesis_Proto_2'Size);
      Aux_Data    : Null_Hypothesis_Proto_2_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Null_Hypothesis_Proto_2_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Null_Hypothesis_Proto_2_Array'
             (Self.Data.all
                & Null_Hypothesis_Proto_2_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Null_Hypothesis_Proto_2_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Null_Hypothesis_Proto_2_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Null_Hypothesis_Proto_2_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Null_Hypothesis_Proto_2_Variable_Reference
    (Self  : aliased in out Null_Hypothesis_Proto_2_Vector;
     Index : Positive)
      return Null_Hypothesis_Proto_2_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Null_Hypothesis_Proto_2_Variable_Reference;

   not overriding function Get_Null_Hypothesis_Proto_2_Constant_Reference
    (Self  : aliased Null_Hypothesis_Proto_2_Vector;
     Index : Positive)
      return Null_Hypothesis_Proto_2_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Null_Hypothesis_Proto_2_Constant_Reference;

   procedure Read_Null_Hypothesis_Proto_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Null_Hypothesis_Proto_2) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Null_Hypothesis_Proto_2;

   procedure Write_Null_Hypothesis_Proto_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Null_Hypothesis_Proto_2) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Null_Hypothesis_Proto_2 (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if WS.End_Message then
            Write_Null_Hypothesis_Proto_2 (WS'Access, V);
         end if;
      end;
   end Write_Null_Hypothesis_Proto_2;

   function Length (Self : Enum_Only_Proto_2_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Enum_Only_Proto_2_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Enum_Only_Proto_2_Array, Enum_Only_Proto_2_Array_Access);

   procedure Append
    (Self : in out Enum_Only_Proto_2_Vector;
     V    : Enum_Only_Proto_2) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Enum_Only_Proto_2'Size);
      Aux_Data    : Enum_Only_Proto_2_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Enum_Only_Proto_2_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Enum_Only_Proto_2_Array'
             (Self.Data.all
                & Enum_Only_Proto_2_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Enum_Only_Proto_2_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Enum_Only_Proto_2_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Enum_Only_Proto_2_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Enum_Only_Proto_2_Variable_Reference
    (Self  : aliased in out Enum_Only_Proto_2_Vector;
     Index : Positive)
      return Enum_Only_Proto_2_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Enum_Only_Proto_2_Variable_Reference;

   not overriding function Get_Enum_Only_Proto_2_Constant_Reference
    (Self  : aliased Enum_Only_Proto_2_Vector;
     Index : Positive)
      return Enum_Only_Proto_2_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Enum_Only_Proto_2_Constant_Reference;

   procedure Read_Enum_Only_Proto_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Enum_Only_Proto_2) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Enum_Only_Proto_2;

   procedure Write_Enum_Only_Proto_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Enum_Only_Proto_2) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Enum_Only_Proto_2 (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if WS.End_Message then
            Write_Enum_Only_Proto_2 (WS'Access, V);
         end if;
      end;
   end Write_Enum_Only_Proto_2;

   function Length (Self : One_String_Proto_2_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out One_String_Proto_2_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (One_String_Proto_2_Array, One_String_Proto_2_Array_Access);

   procedure Append
    (Self : in out One_String_Proto_2_Vector;
     V    : One_String_Proto_2) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / One_String_Proto_2'Size);
      Aux_Data    : One_String_Proto_2_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new One_String_Proto_2_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new One_String_Proto_2_Array'
             (Self.Data.all
                & One_String_Proto_2_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out One_String_Proto_2_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new One_String_Proto_2_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out One_String_Proto_2_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_One_String_Proto_2_Variable_Reference
    (Self  : aliased in out One_String_Proto_2_Vector;
     Index : Positive)
      return One_String_Proto_2_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_One_String_Proto_2_Variable_Reference;

   not overriding function Get_One_String_Proto_2_Constant_Reference
    (Self  : aliased One_String_Proto_2_Vector;
     Index : Positive)
      return One_String_Proto_2_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_One_String_Proto_2_Constant_Reference;

   procedure Read_One_String_Proto_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out One_String_Proto_2) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Data.Is_Set then
                  V.Data := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Data.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_One_String_Proto_2;

   procedure Write_One_String_Proto_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : One_String_Proto_2) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_One_String_Proto_2 (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Data.Is_Set then
            WS.Write (1, V.Data.Value);
         end if;
         if WS.End_Message then
            Write_One_String_Proto_2 (WS'Access, V);
         end if;
      end;
   end Write_One_String_Proto_2;

   function Length (Self : Proto_With_Keywords_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Proto_With_Keywords_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Proto_With_Keywords_Array, Proto_With_Keywords_Array_Access);

   procedure Append
    (Self : in out Proto_With_Keywords_Vector;
     V    : Proto_With_Keywords) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Proto_With_Keywords'Size);
      Aux_Data    : Proto_With_Keywords_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Proto_With_Keywords_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Proto_With_Keywords_Array'
             (Self.Data.all
                & Proto_With_Keywords_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Proto_With_Keywords_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Proto_With_Keywords_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Proto_With_Keywords_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Proto_With_Keywords_Variable_Reference
    (Self  : aliased in out Proto_With_Keywords_Vector;
     Index : Positive)
      return Proto_With_Keywords_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Proto_With_Keywords_Variable_Reference;

   not overriding function Get_Proto_With_Keywords_Constant_Reference
    (Self  : aliased Proto_With_Keywords_Vector;
     Index : Positive)
      return Proto_With_Keywords_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Proto_With_Keywords_Constant_Reference;

   procedure Read_Proto_With_Keywords
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Proto_With_Keywords) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Inline.Is_Set then
                  V.Inline := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Inline.Value);
            when 2 =>
               if  not V.Concept.Is_Set then
                  V.Concept := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Concept.Value);
            when 3 =>
               Proto_Support.IO.Read_Vector (Stream, Key.Encoding, V.Requires);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Proto_With_Keywords;

   procedure Write_Proto_With_Keywords
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Proto_With_Keywords) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Proto_With_Keywords (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Inline.Is_Set then
            WS.Write_Varint (1, V.Inline.Value);
         end if;
         if V.Concept.Is_Set then
            WS.Write (2, V.Concept.Value);
         end if;
         WS.Write (3, V.Requires);
         if WS.End_Message then
            Write_Proto_With_Keywords (WS'Access, V);
         end if;
      end;
   end Write_Proto_With_Keywords;

   function Length
    (Self : Test_All_Required_Types_Proto_2_Vector)
      return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Test_All_Required_Types_Proto_2_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Test_All_Required_Types_Proto_2_Array,
      Test_All_Required_Types_Proto_2_Array_Access);

   procedure Append
    (Self : in out Test_All_Required_Types_Proto_2_Vector;
     V    : Test_All_Required_Types_Proto_2) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Test_All_Required_Types_Proto_2'Size);
      Aux_Data    : Test_All_Required_Types_Proto_2_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=
            new Test_All_Required_Types_Proto_2_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Test_All_Required_Types_Proto_2_Array'
             (Self.Data.all
                & Test_All_Required_Types_Proto_2_Array'
                  (1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Test_All_Required_Types_Proto_2_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Test_All_Required_Types_Proto_2_Array'
             (Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Test_All_Required_Types_Proto_2_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Test_All_Required_Types_Proto_2_Variable_Reference
    (Self  : aliased in out Test_All_Required_Types_Proto_2_Vector;
     Index : Positive)
      return Test_All_Required_Types_Proto_2_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Test_All_Required_Types_Proto_2_Variable_Reference;

   not overriding function Get_Test_All_Required_Types_Proto_2_Constant_Reference
    (Self  : aliased Test_All_Required_Types_Proto_2_Vector;
     Index : Positive)
      return Test_All_Required_Types_Proto_2_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Test_All_Required_Types_Proto_2_Constant_Reference;

   procedure Read_Test_All_Required_Types_Proto_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Test_All_Required_Types_Proto_2) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Required_Int_32);
            when 2 =>
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Required_Int_64);
            when 3 =>
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Required_Uint_32);
            when 4 =>
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Required_Uint_64);
            when 5 =>
               Proto_Support.IO.Read_Zigzag
                 (Stream, Key.Encoding, V.Required_Sint_32);
            when 6 =>
               Proto_Support.IO.Read_Zigzag
                 (Stream, Key.Encoding, V.Required_Sint_64);
            when 7 =>
               Proto_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Required_Fixed_32);
            when 8 =>
               Proto_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Required_Fixed_64);
            when 9 =>
               Proto_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Required_Sfixed_32);
            when 10 =>
               Proto_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Required_Sfixed_64);
            when 11 =>
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Required_Float);
            when 12 =>
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Required_Double);
            when 13 =>
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Required_Bool);
            when 14 =>
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Required_String);
            when 15 =>
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Required_Bytes);
            when 18 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Test_All_Required_Types_Proto_2_Nested_Message_IO
                 .Read
                 (Stream, Key.Encoding, V.Required_Nested_Message);
            when 19 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Foreign_Message_Proto_2_IO
                 .Read
                 (Stream, Key.Encoding, V.Required_Foreign_Message);
            when 21 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Test_All_Required_Types_Proto_2_Nested_Enum_IO
                 .Read
                 (Stream, Key.Encoding, V.Required_Nested_Enum);
            when 22 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Foreign_Enum_Proto_2_IO
                 .Read
                 (Stream, Key.Encoding, V.Required_Foreign_Enum);
            when 24 =>
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Required_String_Piece);
            when 25 =>
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Required_Cord);
            when 27 =>
               if V.Recursive_Message.Length = 0 then
                  V.Recursive_Message.Append ((others => <>));
               end if;
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Test_All_Required_Types_Proto_2_IO
                 .Read
                 (Stream, Key.Encoding, V.Recursive_Message (1));
            when 28 =>
               if V.Optional_Recursive_Message.Length = 0 then
                  V.Optional_Recursive_Message.Append ((others => <>));
               end if;
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Test_All_Required_Types_Proto_2_IO
                 .Read
                 (Stream, Key.Encoding, V.Optional_Recursive_Message (1));
            when 201 =>
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Test_All_Required_Types_Proto_2_Data_IO
                 .Read
                 (Stream, Key.Encoding, V.Data);
            when 241 =>
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Default_Int_32);
            when 242 =>
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Default_Int_64);
            when 243 =>
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Default_Uint_32);
            when 244 =>
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Default_Uint_64);
            when 245 =>
               Proto_Support.IO.Read_Zigzag
                 (Stream, Key.Encoding, V.Default_Sint_32);
            when 246 =>
               Proto_Support.IO.Read_Zigzag
                 (Stream, Key.Encoding, V.Default_Sint_64);
            when 247 =>
               Proto_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Default_Fixed_32);
            when 248 =>
               Proto_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Default_Fixed_64);
            when 249 =>
               Proto_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Default_Sfixed_32);
            when 250 =>
               Proto_Support.IO.Read_Fixed
                 (Stream, Key.Encoding, V.Default_Sfixed_64);
            when 251 =>
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Default_Float);
            when 252 =>
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Default_Double);
            when 253 =>
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Default_Bool);
            when 254 =>
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Default_String);
            when 255 =>
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Default_Bytes);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Test_All_Required_Types_Proto_2;

   procedure Write_Test_All_Required_Types_Proto_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Test_All_Required_Types_Proto_2) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Test_All_Required_Types_Proto_2 (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Varint (1, V.Required_Int_32);
         WS.Write_Varint (2, V.Required_Int_64);
         WS.Write_Varint (3, V.Required_Uint_32);
         WS.Write_Varint (4, V.Required_Uint_64);
         WS.Write_Zigzag (5, V.Required_Sint_32);
         WS.Write_Zigzag (6, V.Required_Sint_64);
         WS.Write_Fixed (7, V.Required_Fixed_32);
         WS.Write_Fixed (8, V.Required_Fixed_64);
         WS.Write_Fixed (9, V.Required_Sfixed_32);
         WS.Write_Fixed (10, V.Required_Sfixed_64);
         WS.Write (11, V.Required_Float);
         WS.Write (12, V.Required_Double);
         WS.Write (13, V.Required_Bool);
         WS.Write (14, V.Required_String);
         WS.Write (15, V.Required_Bytes);
         WS.Write_Key ((18, Proto_Support.Length_Delimited));
         Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
           .Test_All_Required_Types_Proto_2_Nested_Message'Write
           (Stream, V.Required_Nested_Message);
         WS.Write_Key ((19, Proto_Support.Length_Delimited));
         Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
           .Foreign_Message_Proto_2'Write
           (Stream, V.Required_Foreign_Message);
         Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Test_All_Required_Types_Proto_2_Nested_Enum_IO
           .Write
           (WS, 21, V.Required_Nested_Enum);
         Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Foreign_Enum_Proto_2_IO
           .Write
           (WS, 22, V.Required_Foreign_Enum);
         WS.Write (24, V.Required_String_Piece);
         WS.Write (25, V.Required_Cord);
         for J in 1 .. V.Recursive_Message.Length loop
            WS.Write_Key ((27, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Test_All_Required_Types_Proto_2'Write
              (Stream, V.Recursive_Message (J));
         end loop;
         for J in 1 .. V.Optional_Recursive_Message.Length loop
            WS.Write_Key ((28, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Test_All_Required_Types_Proto_2'Write
              (Stream, V.Optional_Recursive_Message (J));
         end loop;
         WS.Write_Key ((201, Proto_Support.Length_Delimited));
         Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
           .Test_All_Required_Types_Proto_2_Data'Write
           (Stream, V.Data);
         WS.Write_Varint (241, V.Default_Int_32);
         WS.Write_Varint (242, V.Default_Int_64);
         WS.Write_Varint (243, V.Default_Uint_32);
         WS.Write_Varint (244, V.Default_Uint_64);
         WS.Write_Zigzag (245, V.Default_Sint_32);
         WS.Write_Zigzag (246, V.Default_Sint_64);
         WS.Write_Fixed (247, V.Default_Fixed_32);
         WS.Write_Fixed (248, V.Default_Fixed_64);
         WS.Write_Fixed (249, V.Default_Sfixed_32);
         WS.Write_Fixed (250, V.Default_Sfixed_64);
         WS.Write (251, V.Default_Float);
         WS.Write (252, V.Default_Double);
         WS.Write (253, V.Default_Bool);
         WS.Write (254, V.Default_String);
         WS.Write (255, V.Default_Bytes);
         if WS.End_Message then
            Write_Test_All_Required_Types_Proto_2 (WS'Access, V);
         end if;
      end;
   end Write_Test_All_Required_Types_Proto_2;

   function Length
    (Self : Test_All_Required_Types_Proto_2_Nested_Message_Vector)
      return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear
    (Self : in out Test_All_Required_Types_Proto_2_Nested_Message_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Test_All_Required_Types_Proto_2_Nested_Message_Array,
      Test_All_Required_Types_Proto_2_Nested_Message_Array_Access);

   procedure Append
    (Self : in out Test_All_Required_Types_Proto_2_Nested_Message_Vector;
     V    : Test_All_Required_Types_Proto_2_Nested_Message) is
      Init_Length : constant Positive :=
        Positive'Max
          (1, 256 / Test_All_Required_Types_Proto_2_Nested_Message'Size);
      Aux_Data    : Test_All_Required_Types_Proto_2_Nested_Message_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=

             new Test_All_Required_Types_Proto_2_Nested_Message_Array
               (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Test_All_Required_Types_Proto_2_Nested_Message_Array'
             (Self.Data.all
                & Test_All_Required_Types_Proto_2_Nested_Message_Array'
                  (1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Test_All_Required_Types_Proto_2_Nested_Message_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Test_All_Required_Types_Proto_2_Nested_Message_Array'
             (Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Test_All_Required_Types_Proto_2_Nested_Message_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Test_All_Required_Types_Proto_2_Nested_Message_Variable_Reference
    (Self  : aliased in out Test_All_Required_Types_Proto_2_Nested_Message_Vector;
     Index : Positive)
      return Test_All_Required_Types_Proto_2_Nested_Message_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Test_All_Required_Types_Proto_2_Nested_Message_Variable_Reference;

   not overriding function Get_Test_All_Required_Types_Proto_2_Nested_Message_Constant_Reference
    (Self  : aliased Test_All_Required_Types_Proto_2_Nested_Message_Vector;
     Index : Positive)
      return Test_All_Required_Types_Proto_2_Nested_Message_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Test_All_Required_Types_Proto_2_Nested_Message_Constant_Reference;

   procedure Read_Test_All_Required_Types_Proto_2_Nested_Message
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Test_All_Required_Types_Proto_2_Nested_Message) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               Proto_Support.IO.Read_Varint (Stream, Key.Encoding, V.A);
            when 2 =>
               if V.Corecursive.Length = 0 then
                  V.Corecursive.Append ((others => <>));
               end if;
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Test_All_Required_Types_Proto_2_IO
                 .Read
                 (Stream, Key.Encoding, V.Corecursive (1));
            when 3 =>
               if V.Optional_Corecursive.Length = 0 then
                  V.Optional_Corecursive.Append ((others => <>));
               end if;
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_Test_All_Required_Types_Proto_2_IO
                 .Read
                 (Stream, Key.Encoding, V.Optional_Corecursive (1));
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Test_All_Required_Types_Proto_2_Nested_Message;

   procedure Write_Test_All_Required_Types_Proto_2_Nested_Message
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Test_All_Required_Types_Proto_2_Nested_Message) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Test_All_Required_Types_Proto_2_Nested_Message
              (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Varint (1, V.A);
         for J in 1 .. V.Corecursive.Length loop
            WS.Write_Key ((2, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Test_All_Required_Types_Proto_2'Write
              (Stream, V.Corecursive (J));
         end loop;
         for J in 1 .. V.Optional_Corecursive.Length loop
            WS.Write_Key ((3, Proto_Support.Length_Delimited));
            Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
              .Test_All_Required_Types_Proto_2'Write
              (Stream, V.Optional_Corecursive (J));
         end loop;
         if WS.End_Message then
            Write_Test_All_Required_Types_Proto_2_Nested_Message
              (WS'Access, V);
         end if;
      end;
   end Write_Test_All_Required_Types_Proto_2_Nested_Message;

   function Length
    (Self : Test_All_Required_Types_Proto_2_Data_Vector)
      return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear
    (Self : in out Test_All_Required_Types_Proto_2_Data_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Test_All_Required_Types_Proto_2_Data_Array,
      Test_All_Required_Types_Proto_2_Data_Array_Access);

   procedure Append
    (Self : in out Test_All_Required_Types_Proto_2_Data_Vector;
     V    : Test_All_Required_Types_Proto_2_Data) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Test_All_Required_Types_Proto_2_Data'Size);
      Aux_Data    : Test_All_Required_Types_Proto_2_Data_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=
            new Test_All_Required_Types_Proto_2_Data_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Test_All_Required_Types_Proto_2_Data_Array'
             (Self.Data.all
                & Test_All_Required_Types_Proto_2_Data_Array'
                  (1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Test_All_Required_Types_Proto_2_Data_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Test_All_Required_Types_Proto_2_Data_Array'
             (Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Test_All_Required_Types_Proto_2_Data_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Test_All_Required_Types_Proto_2_Data_Variable_Reference
    (Self  : aliased in out Test_All_Required_Types_Proto_2_Data_Vector;
     Index : Positive)
      return Test_All_Required_Types_Proto_2_Data_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Test_All_Required_Types_Proto_2_Data_Variable_Reference;

   not overriding function Get_Test_All_Required_Types_Proto_2_Data_Constant_Reference
    (Self  : aliased Test_All_Required_Types_Proto_2_Data_Vector;
     Index : Positive)
      return Test_All_Required_Types_Proto_2_Data_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Test_All_Required_Types_Proto_2_Data_Constant_Reference;

   procedure Read_Test_All_Required_Types_Proto_2_Data
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Test_All_Required_Types_Proto_2_Data) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 202 =>
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Group_Int_32);
            when 203 =>
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Group_Uint_32);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Test_All_Required_Types_Proto_2_Data;

   procedure Write_Test_All_Required_Types_Proto_2_Data
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Test_All_Required_Types_Proto_2_Data) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Test_All_Required_Types_Proto_2_Data (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Varint (202, V.Group_Int_32);
         WS.Write_Varint (203, V.Group_Uint_32);
         if WS.End_Message then
            Write_Test_All_Required_Types_Proto_2_Data (WS'Access, V);
         end if;
      end;
   end Write_Test_All_Required_Types_Proto_2_Data;

   function Length
    (Self : Test_All_Required_Types_Proto_2_Message_Set_Correct_Vector)
      return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear
    (Self : in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Test_All_Required_Types_Proto_2_Message_Set_Correct_Array,
      Test_All_Required_Types_Proto_2_Message_Set_Correct_Array_Access);

   procedure Append
    (Self : in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Vector;
     V    : Test_All_Required_Types_Proto_2_Message_Set_Correct) is
      Init_Length : constant Positive :=
        Positive'Max
          (1, 256 / Test_All_Required_Types_Proto_2_Message_Set_Correct'Size);
      Aux_Data    : Test_All_Required_Types_Proto_2_Message_Set_Correct_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=

             new Test_All_Required_Types_Proto_2_Message_Set_Correct_Array
               (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Test_All_Required_Types_Proto_2_Message_Set_Correct_Array'
             (Self.Data.all
                & Test_All_Required_Types_Proto_2_Message_Set_Correct_Array'
                  (1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Test_All_Required_Types_Proto_2_Message_Set_Correct_Array'
             (Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Test_All_Required_Types_Proto_2_Message_Set_Correct_Variable_Reference
    (Self  : aliased in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Vector;
     Index : Positive)
      return Test_All_Required_Types_Proto_2_Message_Set_Correct_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Test_All_Required_Types_Proto_2_Message_Set_Correct_Variable_Reference;

   not overriding function Get_Test_All_Required_Types_Proto_2_Message_Set_Correct_Constant_Reference
    (Self  : aliased Test_All_Required_Types_Proto_2_Message_Set_Correct_Vector;
     Index : Positive)
      return Test_All_Required_Types_Proto_2_Message_Set_Correct_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Test_All_Required_Types_Proto_2_Message_Set_Correct_Constant_Reference;

   procedure Read_Test_All_Required_Types_Proto_2_Message_Set_Correct
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Test_All_Required_Types_Proto_2_Message_Set_Correct) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Test_All_Required_Types_Proto_2_Message_Set_Correct;

   procedure Write_Test_All_Required_Types_Proto_2_Message_Set_Correct
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Test_All_Required_Types_Proto_2_Message_Set_Correct) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Test_All_Required_Types_Proto_2_Message_Set_Correct
              (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if WS.End_Message then
            Write_Test_All_Required_Types_Proto_2_Message_Set_Correct
              (WS'Access, V);
         end if;
      end;
   end Write_Test_All_Required_Types_Proto_2_Message_Set_Correct;

   function Length
    (Self : Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Vector)
      return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear
    (Self : in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Array,
      Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Array_Access);

   procedure Append
    (Self : in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Vector;
     V    : Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1) is
      Init_Length : constant Positive :=
        Positive'Max
          (1,
           256
             / Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1'Size);
      Aux_Data    : Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=

             new Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Array
               (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Array'
             (Self.Data.all
                & Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Array'
                  (1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Array'
             (Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Variable_Reference
    (Self  : aliased in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Vector;
     Index : Positive)
      return Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Variable_Reference;

   not overriding function Get_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Constant_Reference
    (Self  : aliased Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Vector;
     Index : Positive)
      return Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Constant_Reference;

   procedure Read_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 25 =>
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Str);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1;

   procedure Write_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1
              (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write (25, V.Str);
         if WS.End_Message then
            Write_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1
              (WS'Access, V);
         end if;
      end;
   end Write_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1;

   function Length
    (Self : Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Vector)
      return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear
    (Self : in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Array,
      Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Array_Access);

   procedure Append
    (Self : in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Vector;
     V    : Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2) is
      Init_Length : constant Positive :=
        Positive'Max
          (1,
           256
             / Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2'Size);
      Aux_Data    : Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=

             new Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Array
               (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Array'
             (Self.Data.all
                & Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Array'
                  (1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Array'
             (Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Variable_Reference
    (Self  : aliased in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Vector;
     Index : Positive)
      return Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Variable_Reference;

   not overriding function Get_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Constant_Reference
    (Self  : aliased Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Vector;
     Index : Positive)
      return Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Constant_Reference;

   procedure Read_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 9 =>
               Proto_Support.IO.Read_Varint (Stream, Key.Encoding, V.I);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2;

   procedure Write_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2
              (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Varint (9, V.I);
         if WS.End_Message then
            Write_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2
              (WS'Access, V);
         end if;
      end;
   end Write_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2;

   function Length (Self : Test_Large_Oneof_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Test_Large_Oneof_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Test_Large_Oneof_Array, Test_Large_Oneof_Array_Access);

   procedure Append
    (Self : in out Test_Large_Oneof_Vector;
     V    : Test_Large_Oneof) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Test_Large_Oneof'Size);
      Aux_Data    : Test_Large_Oneof_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Test_Large_Oneof_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Test_Large_Oneof_Array'
             (Self.Data.all & Test_Large_Oneof_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Test_Large_Oneof_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Test_Large_Oneof_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Test_Large_Oneof_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Test_Large_Oneof_Variable_Reference
    (Self  : aliased in out Test_Large_Oneof_Vector;
     Index : Positive)
      return Test_Large_Oneof_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Test_Large_Oneof_Variable_Reference;

   not overriding function Get_Test_Large_Oneof_Constant_Reference
    (Self  : aliased Test_Large_Oneof_Vector;
     Index : Positive)
      return Test_Large_Oneof_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Test_Large_Oneof_Constant_Reference;

   procedure Read_Test_Large_Oneof
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Test_Large_Oneof) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if V.Variant.Large_Oneof /= A1_Kind then
                  V.Variant := (A1_Kind, others => <>);
               end if;
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_A1_IO.Read
                 (Stream, Key.Encoding, V.Variant.A1);
            when 2 =>
               if V.Variant.Large_Oneof /= A2_Kind then
                  V.Variant := (A2_Kind, others => <>);
               end if;
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_A2_IO.Read
                 (Stream, Key.Encoding, V.Variant.A2);
            when 3 =>
               if V.Variant.Large_Oneof /= A3_Kind then
                  V.Variant := (A3_Kind, others => <>);
               end if;
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_A3_IO.Read
                 (Stream, Key.Encoding, V.Variant.A3);
            when 4 =>
               if V.Variant.Large_Oneof /= A4_Kind then
                  V.Variant := (A4_Kind, others => <>);
               end if;
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_A4_IO.Read
                 (Stream, Key.Encoding, V.Variant.A4);
            when 5 =>
               if V.Variant.Large_Oneof /= A5_Kind then
                  V.Variant := (A5_Kind, others => <>);
               end if;
               Protobuf_Test_Messages_Proto_2_Test_Messages_Proto_2_A5_IO.Read
                 (Stream, Key.Encoding, V.Variant.A5);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Test_Large_Oneof;

   procedure Write_Test_Large_Oneof
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Test_Large_Oneof) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Test_Large_Oneof (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         case V.Variant.Large_Oneof is
            when A1_Kind =>
               WS.Write_Key ((1, Proto_Support.Length_Delimited));
               Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.A1'Write
                 (Stream, V.Variant.A1);
            when A2_Kind =>
               WS.Write_Key ((2, Proto_Support.Length_Delimited));
               Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.A2'Write
                 (Stream, V.Variant.A2);
            when A3_Kind =>
               WS.Write_Key ((3, Proto_Support.Length_Delimited));
               Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.A3'Write
                 (Stream, V.Variant.A3);
            when A4_Kind =>
               WS.Write_Key ((4, Proto_Support.Length_Delimited));
               Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.A4'Write
                 (Stream, V.Variant.A4);
            when A5_Kind =>
               WS.Write_Key ((5, Proto_Support.Length_Delimited));
               Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.A5'Write
                 (Stream, V.Variant.A5);
            when Large_Oneof_Not_Set =>
               null;
         end case;
         if WS.End_Message then
            Write_Test_Large_Oneof (WS'Access, V);
         end if;
      end;
   end Write_Test_Large_Oneof;

   function Length (Self : A1_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out A1_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (A1_Array, A1_Array_Access);

   procedure Append (Self : in out A1_Vector; V    : A1) is
      Init_Length : constant Positive := Positive'Max (1, 256 / A1'Size);
      Aux_Data    : A1_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new A1_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new A1_Array'(Self.Data.all & A1_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out A1_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new A1_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out A1_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_A1_Variable_Reference
    (Self  : aliased in out A1_Vector;
     Index : Positive)
      return A1_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_A1_Variable_Reference;

   not overriding function Get_A1_Constant_Reference
    (Self  : aliased A1_Vector;
     Index : Positive)
      return A1_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_A1_Constant_Reference;

   procedure Read_A1
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out A1) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_A1;

   procedure Write_A1
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : A1) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_A1 (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if WS.End_Message then
            Write_A1 (WS'Access, V);
         end if;
      end;
   end Write_A1;

   function Length (Self : A2_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out A2_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (A2_Array, A2_Array_Access);

   procedure Append (Self : in out A2_Vector; V    : A2) is
      Init_Length : constant Positive := Positive'Max (1, 256 / A2'Size);
      Aux_Data    : A2_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new A2_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new A2_Array'(Self.Data.all & A2_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out A2_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new A2_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out A2_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_A2_Variable_Reference
    (Self  : aliased in out A2_Vector;
     Index : Positive)
      return A2_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_A2_Variable_Reference;

   not overriding function Get_A2_Constant_Reference
    (Self  : aliased A2_Vector;
     Index : Positive)
      return A2_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_A2_Constant_Reference;

   procedure Read_A2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out A2) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_A2;

   procedure Write_A2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : A2) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_A2 (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if WS.End_Message then
            Write_A2 (WS'Access, V);
         end if;
      end;
   end Write_A2;

   function Length (Self : A3_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out A3_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (A3_Array, A3_Array_Access);

   procedure Append (Self : in out A3_Vector; V    : A3) is
      Init_Length : constant Positive := Positive'Max (1, 256 / A3'Size);
      Aux_Data    : A3_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new A3_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new A3_Array'(Self.Data.all & A3_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out A3_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new A3_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out A3_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_A3_Variable_Reference
    (Self  : aliased in out A3_Vector;
     Index : Positive)
      return A3_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_A3_Variable_Reference;

   not overriding function Get_A3_Constant_Reference
    (Self  : aliased A3_Vector;
     Index : Positive)
      return A3_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_A3_Constant_Reference;

   procedure Read_A3
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out A3) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_A3;

   procedure Write_A3
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : A3) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_A3 (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if WS.End_Message then
            Write_A3 (WS'Access, V);
         end if;
      end;
   end Write_A3;

   function Length (Self : A4_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out A4_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (A4_Array, A4_Array_Access);

   procedure Append (Self : in out A4_Vector; V    : A4) is
      Init_Length : constant Positive := Positive'Max (1, 256 / A4'Size);
      Aux_Data    : A4_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new A4_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new A4_Array'(Self.Data.all & A4_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out A4_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new A4_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out A4_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_A4_Variable_Reference
    (Self  : aliased in out A4_Vector;
     Index : Positive)
      return A4_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_A4_Variable_Reference;

   not overriding function Get_A4_Constant_Reference
    (Self  : aliased A4_Vector;
     Index : Positive)
      return A4_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_A4_Constant_Reference;

   procedure Read_A4
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out A4) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_A4;

   procedure Write_A4
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : A4) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_A4 (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if WS.End_Message then
            Write_A4 (WS'Access, V);
         end if;
      end;
   end Write_A4;

   function Length (Self : A5_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out A5_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (A5_Array, A5_Array_Access);

   procedure Append (Self : in out A5_Vector; V    : A5) is
      Init_Length : constant Positive := Positive'Max (1, 256 / A5'Size);
      Aux_Data    : A5_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new A5_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new A5_Array'(Self.Data.all & A5_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out A5_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new A5_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out A5_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_A5_Variable_Reference
    (Self  : aliased in out A5_Vector;
     Index : Positive)
      return A5_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_A5_Variable_Reference;

   not overriding function Get_A5_Constant_Reference
    (Self  : aliased A5_Vector;
     Index : Positive)
      return A5_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_A5_Constant_Reference;

   procedure Read_A5
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out A5) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_A5;

   procedure Write_A5
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : A5) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_A5 (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if WS.End_Message then
            Write_A5 (WS'Access, V);
         end if;
      end;
   end Write_A5;

end Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2;