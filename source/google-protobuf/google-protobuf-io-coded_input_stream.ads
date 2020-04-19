pragma Ada_2012;

with Google.Protobuf.Wire_Format;
with Ada.Streams;
with System;

limited with Google.Protobuf.Message;

-- limited with Google.Protobuf.Message; is a trick to avoid a circular unit
-- dependency caused by with-ing Google.Protobuf.IO.Coded_Input_Stream from
-- Google.Protobuf.Message. with Google.Protobuf.Message is used in body
-- since the incomplete view provided by limited with is not sufficient.

package Google.Protobuf.IO.Coded_Input_Stream is
   type Root_Stream_Access is access all Ada.Streams.Root_Stream_Type'Class;
   type Instance (Input_Stream : Root_Stream_Access) is tagged private;

   -- Consider replacing this use clause???
   use Google.Protobuf.Wire_Format;

   function Decode_Zig_Zag_32 (Value : in PB_UInt32) return PB_UInt32;

   function Decode_Zig_Zag_64 (Value : in PB_UInt64) return PB_UInt64;

   function Read_Boolean (This : in out Coded_Input_Stream.Instance) return PB_Bool;

   function Read_Double (This : in out Coded_Input_Stream.Instance) return PB_Double;

   function Read_Enumeration (This : in out Coded_Input_Stream.Instance) return PB_Int32;

   function Read_Fixed_32 (This : in out Coded_Input_Stream.Instance) return PB_UInt32;

   function Read_Fixed_64 (This : in out Coded_Input_Stream.Instance) return PB_UInt64;

   function Read_Float (This : in out Coded_Input_Stream.Instance) return PB_Float;

   function Read_Integer_32 (This : in out Coded_Input_Stream.Instance) return PB_Int32;

   function Read_Integer_64 (This : in out Coded_Input_Stream.Instance) return PB_Int64;

   function Read_Raw_Little_Endian_32 (This : in out Coded_Input_Stream.Instance) return PB_UInt32;

   function Read_Raw_Little_Endian_64 (This : in out Coded_Input_Stream.Instance) return PB_UInt64;

   function Read_Raw_Varint_32 (This : in out Coded_Input_Stream.Instance) return PB_UInt32;

   function Read_Raw_Varint_64 (This : in out Coded_Input_Stream.Instance) return PB_UInt64;

   function Read_Signed_Fixed_32 (This : in out Coded_Input_Stream.Instance) return PB_Int32;

   function Read_Signed_Fixed_64 (This : in out Coded_Input_Stream.Instance) return PB_Int64;

   function Read_Signed_Integer_32 (This : in out Coded_Input_Stream.Instance)return PB_Int32;

   function Read_Signed_Integer_64 (This : in out Coded_Input_Stream.Instance) return PB_Int64;

   function Read_String (This : in out Coded_Input_Stream.Instance) return PB_String_Access;

   function Read_Tag (This : in out Coded_Input_Stream.Instance) return PB_UInt32;

   function Read_Unsigned_Integer_32 (This : in out Coded_Input_Stream.Instance) return PB_UInt32;

   function Read_Unsigned_Integer_64 (This : in out Coded_Input_Stream.Instance) return PB_UInt64;

   function Skip_Field (This : in out Coded_Input_Stream.Instance; Tag : in PB_UInt32) return Boolean;

   procedure Check_Last_Tag_Was (This : in Coded_Input_Stream.Instance; Tag : in PB_UInt32);

   procedure Skip_Message (This : in out Coded_Input_Stream.Instance);

   procedure Read_Message (This : in out Coded_Input_Stream.Instance; Value : in out Google.Protobuf.Message.Instance'Class);

   -- ==========================================================================

   -- Consider replacing this use clause???
   use Ada.Streams;

   function Set_Size_Limit (This : in out Coded_Input_Stream.Instance; Limit : in Stream_Element_Count) return Stream_Element_Count;

   procedure Reset_Size_Counter (This : in out Coded_Input_Stream.Instance);

   function Push_Limit (This : in out Coded_Input_Stream.Instance; Byte_Limit : in Stream_Element_Count) return Stream_Element_Count;

   procedure Pop_Limit (This : in out Coded_Input_Stream.Instance; Old_Limit : in Stream_Element_Count);

   function Get_Bytes_Until_Limit (This : in Coded_Input_Stream.Instance) return Stream_Element_Offset;

   function Is_At_End (This : in out Coded_Input_Stream.Instance) return Boolean;

   function Get_Total_Bytes_Read (This : in Coded_Input_Stream.Instance) return Stream_Element_Count;

   function Read_Raw_Byte (This : in out Coded_Input_Stream.Instance) return PB_Byte;

   -- Consider changing type of Size to Stream_Element_Offset instead???
   function Read_Raw_Bytes (This : in out Coded_Input_Stream.Instance; Size : in Stream_Element_Count) return Stream_Element_Array;

   -- Consider changing type of Size to Stream_Element_Offset instead???
   procedure Skip_Raw_Bytes (This : in out Coded_Input_Stream.Instance; Size : in Stream_Element_Count);

private
   BUFFER_SIZE : constant := 4096;
   DEFAULT_RECURSION_LIMIT : constant := 64;
   DEFAULT_SIZE_LIMIT : constant := 2 ** 26; -- 64 MB

   Big_Endian : constant Boolean := System. "=" (System.Default_Bit_Order, System.High_Order_First);

   --Big_Endian_Not_Implemented : exception;

   -- Temporary types. Consider changing types??? Allow user to change defaults ...
   type Instance (Input_Stream : Root_Stream_Access) is tagged
      record
         Buffer                  : Stream_Element_Array (0 .. BUFFER_SIZE - 1);
         Buffer_Size             : Stream_Element_Count := 0;
         Buffer_Position         : Stream_Element_Offset := 0;
         Buffer_Size_After_Limit : Stream_Element_Count := 0;
         Total_Bytes_Retired     : Stream_Element_Offset := 0;
         Current_Limit           : Stream_Element_Count := Ada.Streams.Stream_Element_Count'Last;
         Recursion_Limit         : PB_UInt32 := DEFAULT_RECURSION_LIMIT;
         Recursion_Depth         : PB_UInt32 := 0;
         Size_Limit              : Stream_Element_Count := DEFAULT_SIZE_LIMIT;
         Last_Tag                : PB_UInt32;
      end record;

   function Refill_Buffer (This : in out Coded_Input_Stream.Instance; Must_Succeed : in Boolean) return Boolean;

   procedure Recompute_Buffer_Size_After_Limit (This : in out Coded_Input_Stream.Instance);

end Google.Protobuf.IO.Coded_Input_Stream;
