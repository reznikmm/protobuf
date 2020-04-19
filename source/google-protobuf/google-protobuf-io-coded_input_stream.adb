pragma Ada_2012;

with Interfaces;
with Ada.Unchecked_Conversion;
with Google.Protobuf.Message;
with Google.Protobuf.IO.Invalid_Protocol_Buffer_Exception;
with Ada.Containers.Indefinite_Vectors;

package body Google.Protobuf.IO.Coded_Input_Stream is

   -----------------------
   -- Decode_Zig_Zag_32 --
   -----------------------

   function Decode_Zig_Zag_32
     (Value : in PB_UInt32)
     return PB_UInt32
   is
      Value_To_Unsigned_32 : constant Interfaces.Unsigned_32 := Interfaces.Unsigned_32 (Value);
      use type Interfaces.Unsigned_32;
   begin
      return PB_UInt32 (Interfaces.Shift_Right (Value_To_Unsigned_32, 1) xor - (Value_To_Unsigned_32 and 1));
   end Decode_Zig_Zag_32;

   -----------------------
   -- Decode_Zig_Zag_64 --
   -----------------------

   function Decode_Zig_Zag_64
     (Value : in PB_UInt64)
     return PB_UInt64
   is
      Value_To_Unsigned_64 : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (Value);
      use type Interfaces.Unsigned_64;
   begin
      return PB_UInt64 (Interfaces.Shift_Right (Value_To_Unsigned_64, 1) xor - (Value_To_Unsigned_64 and 1));
   end Decode_Zig_Zag_64;

   ------------------
   -- Read_Boolean --
   ------------------

   function Read_Boolean
     (This : in out Coded_Input_Stream.Instance)
     return PB_Bool
   is
      function BOOLEAN_TO_PB_Bool is new
        Ada.Unchecked_Conversion (Source => Boolean,
                                  Target => PB_Bool);
      use type PB_Byte;
   begin
      return BOOLEAN_TO_PB_Bool (This.Read_Raw_Byte /= 0);
   end Read_Boolean;

   -----------------
   -- Read_Double --
   -----------------

   function Read_Double
     (This : in out Coded_Input_Stream.Instance)
     return PB_Double
   is
      function PB_UInt64_To_PB_Double is new
        Ada.Unchecked_Conversion (Source => PB_UInt64,
                                  Target => PB_Double);
   begin
      return PB_UInt64_To_PB_Double (This.Read_Raw_Little_Endian_64);
   end Read_Double;

   ----------------------
   -- Read_Enumeration --
   ----------------------

   function Read_Enumeration
     (This : in out Coded_Input_Stream.Instance)
     return PB_Int32
   is
      function PB_UInt32_To_PB_Int32 is new
        Ada.Unchecked_Conversion (Source => PB_UInt32,
                                  Target => PB_Int32);
   begin
      return PB_UInt32_To_PB_Int32 (This.Read_Raw_Varint_32);
   end Read_Enumeration;

   -------------------
   -- Read_Fixed_32 --
   -------------------

   function Read_Fixed_32
     (This : in out Coded_Input_Stream.Instance)
     return PB_UInt32
   is
   begin
      return This.Read_Raw_Little_Endian_32;
   end Read_Fixed_32;

   -------------------
   -- Read_Fixed_64 --
   -------------------

   function Read_Fixed_64
     (This : in out Coded_Input_Stream.Instance)
     return PB_UInt64
   is
   begin
      return This.Read_Raw_Little_Endian_64;
   end Read_Fixed_64;

   ----------------
   -- Read_Float --
   ----------------

   function Read_Float
     (This : in out Coded_Input_Stream.Instance)
     return PB_Float
   is
      function PB_UInt32_To_PB_Float is new
        Ada.Unchecked_Conversion (Source => PB_UInt32,
                                  Target => PB_Float);
   begin
      return PB_UInt32_To_PB_Float (This.Read_Raw_Little_Endian_32);
   end Read_Float;

   ---------------------
   -- Read_Integer_32 --
   ---------------------

   function Read_Integer_32
     (This : in out Coded_Input_Stream.Instance)
     return PB_Int32
   is
      function PB_UInt32_To_PB_Int32 is new
        Ada.Unchecked_Conversion (Source => PB_UInt32,
                                  Target => PB_Int32);
   begin
      return PB_UInt32_To_PB_Int32 (This.Read_Raw_Varint_32);
   end Read_Integer_32;

   ---------------------
   -- Read_Integer_64 --
   ---------------------

   function Read_Integer_64
     (This : in out Coded_Input_Stream.Instance)
     return PB_Int64
   is
      function PB_UInt64_To_PB_Int64 is new
        Ada.Unchecked_Conversion (Source => PB_UInt64,
                                  Target => PB_Int64);
   begin
      return PB_UInt64_To_PB_Int64 (This.Read_Raw_Varint_64);
   end Read_Integer_64;

   -------------------------------
   -- Read_Raw_Little_Endian_32 --
   -------------------------------

   function Read_Raw_Little_Endian_32
     (This : in out Coded_Input_Stream.Instance)
     return PB_UInt32
   is
      Value  : PB_UInt32;
      Byte_1 : constant PB_Byte := This.Read_Raw_Byte;
      Byte_2 : constant PB_Byte := This.Read_Raw_Byte;
      Byte_3 : constant PB_Byte := This.Read_Raw_Byte;
      Byte_4 : constant PB_Byte := This.Read_Raw_Byte;

      use type PB_UInt32;
   begin
      if Big_Endian then
         -- If we are on a big endian system like PowerPC, do something here
         --raise Big_Endian_Not_Implemented;
         Value := Pb_Uint32 (Byte_1);
         Value := Shift_Left (Value, 8);
         Value := Value or Pb_Uint32 (Byte_2);
         Value := Shift_Left (Value, 8);
         Value := Value or Pb_Uint32 (Byte_3);
         Value := Shift_Left (Value, 8);
         Value := Value or PB_UInt32 (Byte_4);
      else
         Value := Pb_Uint32 (Byte_4);
         Value := Shift_Left (Value, 8);
         Value := Value or Pb_Uint32 (Byte_3);
         Value := Shift_Left (Value, 8);
         Value := Value or Pb_Uint32 (Byte_2);
         Value := Shift_Left (Value, 8);
         Value := Value or PB_UInt32 (Byte_1);
      end if;
      return Value;
   end Read_Raw_Little_Endian_32;

   -------------------------------
   -- Read_Raw_Little_Endian_64 --
   -------------------------------

   function Read_Raw_Little_Endian_64
     (This : in out Coded_Input_Stream.Instance)
     return PB_UInt64
   is
      Value  : PB_UInt64;
      Byte_1 : constant PB_Byte := This.Read_Raw_Byte;
      Byte_2 : constant PB_Byte := This.Read_Raw_Byte;
      Byte_3 : constant PB_Byte := This.Read_Raw_Byte;
      Byte_4 : constant PB_Byte := This.Read_Raw_Byte;
      Byte_5 : constant PB_Byte := This.Read_Raw_Byte;
      Byte_6 : constant PB_Byte := This.Read_Raw_Byte;
      Byte_7 : constant PB_Byte := This.Read_Raw_Byte;
      Byte_8 : constant PB_Byte := This.Read_Raw_Byte;

      use type PB_UInt64;
   begin
      if Big_Endian then
         -- If we are on a big endian system like PowerPC, do something here
         --raise Big_Endian_Not_Implemented;
         Value := Pb_Uint64 (Byte_1);
         Value := Shift_Left (Value, 8);
         Value := Value or Pb_Uint64 (Byte_2);
         Value := Shift_Left (Value, 8);
         Value := Value or Pb_Uint64 (Byte_3);
         Value := Shift_Left (Value, 8);
         Value := Value or Pb_Uint64 (Byte_4);
         Value := Shift_Left (Value, 8);
         Value := Value or Pb_Uint64 (Byte_5);
         Value := Shift_Left (Value, 8);
         Value := Value or Pb_Uint64 (Byte_6);
         Value := Shift_Left (Value, 8);
         Value := Value or Pb_Uint64 (Byte_7);
         Value := Shift_Left (Value, 8);
         Value := Value or Pb_Uint64 (Byte_8);
      else
         Value := Pb_Uint64 (Byte_8);
         Value := Shift_Left (Value, 8);
         Value := Value or Pb_Uint64 (Byte_7);
         Value := Shift_Left (Value, 8);
         Value := Value or Pb_Uint64 (Byte_6);
         Value := Shift_Left (Value, 8);
         Value := Value or Pb_Uint64 (Byte_5);
         Value := Shift_Left (Value, 8);
         Value := Value or Pb_Uint64 (Byte_4);
         Value := Shift_Left (Value, 8);
         Value := Value or Pb_Uint64 (Byte_3);
         Value := Shift_Left (Value, 8);
         Value := Value or Pb_Uint64 (Byte_2);
         Value := Shift_Left (Value, 8);
         Value := Value or Pb_Uint64 (Byte_1);
      end if;
      return Value;
   end Read_Raw_Little_Endian_64;

   ------------------------
   -- Read_Raw_Varint_32 --
   ------------------------

   function Read_Raw_Varint_32
     (This : in out Coded_Input_Stream.Instance)
     return PB_UInt32
   is
      Result        : PB_UInt32 := 0;
      Temp          : PB_Byte := This.Read_Raw_Byte;
      Byte_MSB_Set  : constant := 16#80#;
      use type PB_Byte;
      use type PB_UInt32;
   begin
      -- MSB not set, which means that varint consist of only one byte. See Base 128 Varints:
      -- https://developers.google.com/protocol-buffers/docs/encoding
      if Temp < Byte_MSB_Set then
         return PB_UInt32 (Temp);
      end if;

      Result := PB_UInt32 (Temp and 16#7F#); -- (TMP and 16#7F#) == set MSB to 0
      Temp := This.Read_Raw_Byte;
      if Temp < Byte_MSB_Set then
         Result := Result or  Shift_Left (PB_UInt32 (Temp), 7);
      else
         Result := Result or Shift_Left (PB_UInt32 (Temp and 16#7F#), 7);
         Temp := This.Read_Raw_Byte;
         if Temp < Byte_MSB_Set then
            Result := Result or Shift_Left (PB_UInt32 (Temp), 14);
         else
            Result := Result or Shift_Left (PB_UInt32 (Temp and 16#7F#), 14);
            Temp := This.Read_Raw_Byte;
            if Temp < Byte_MSB_Set then
               Result := Result or Shift_Left (PB_UInt32 (Temp), 21);
            else
               Result := Result or Shift_Left (PB_UInt32 (Temp and 16#7F#), 21);
               Temp := This.Read_Raw_Byte;
               Result := Result or Shift_Left (PB_UInt32 (Temp), 28);

               -- Tests if last byte has MSB set in which case the varint is
               -- malformed, since it cannot be represented by a 32-bit type.
               if Temp >= Byte_MSB_Set then
                  -- Discard upper 32-bits
                  for I in 1 .. 5 loop
                     Temp := This.Read_Raw_Byte;
                     if Temp < Byte_MSB_Set then
                        return Result;
                     end if;
                  end loop;

                  Invalid_Protocol_Buffer_Exception.Malformed_Varint;

               end if;
            end if;
         end if;
      end if;

      return Result;
   end Read_Raw_Varint_32;

   ------------------------
   -- Read_Raw_Varint_64 --
   ------------------------

   function Read_Raw_Varint_64
     (This : in out Coded_Input_Stream.Instance)
     return PB_UInt64
   is
      Shift  : Natural := 0;
      Result : PB_UInt64 := 0;
      Tmp    : PB_Byte;

      Byte_MSB_Set : constant := 16#80#;

      use type PB_Byte;
      use type PB_UInt64;
   begin

      while Shift < 64 loop
         Tmp := This.Read_Raw_Byte;
         Result := Result or Shift_Left (PB_UInt64 (Tmp and 16#7F#), Shift);
         if (Tmp and Byte_MSB_Set) = 0 then
            return Result;
         end if;

         Shift := Shift + 7;
      end loop;

      Invalid_Protocol_Buffer_Exception.Malformed_Varint;
      return Result;
   end Read_Raw_Varint_64;

   --------------------------
   -- Read_Signed_Fixed_32 --
   --------------------------

   function Read_Signed_Fixed_32
     (This : in out Coded_Input_Stream.Instance)
     return PB_Int32
   is
      function PB_UInt32_To_PB_Int32 is new
        Ada.Unchecked_Conversion (Source => PB_UInt32,
                                  Target => PB_Int32);
   begin
      return PB_UInt32_To_PB_Int32 (This.Read_Raw_Little_Endian_32);
   end Read_Signed_Fixed_32;

   --------------------------
   -- Read_Signed_Fixed_64 --
   --------------------------

   function Read_Signed_Fixed_64
     (This : in out Coded_Input_Stream.Instance)
     return PB_Int64
   is
      function PB_UInt64_To_PB_Int64 is new
        Ada.Unchecked_Conversion (Source => PB_UInt64,
                                  Target => PB_Int64);
   begin
      return PB_UInt64_To_PB_Int64 (This.Read_Raw_Little_Endian_64);
   end Read_Signed_Fixed_64;

   ----------------------------
   -- Read_Signed_Integer_32 --
   ----------------------------

   function Read_Signed_Integer_32
     (This : in out Coded_Input_Stream.Instance)
     return PB_Int32
   is
      function PB_UInt32_To_PB_Int32 is new
        Ada.Unchecked_Conversion (Source => PB_UInt32,
                                  Target => PB_Int32);
   begin
      return PB_UInt32_To_PB_Int32 (Decode_Zig_Zag_32 (This.Read_Raw_Varint_32));
   end Read_Signed_Integer_32;

   ----------------------------
   -- Read_Signed_Integer_64 --
   ----------------------------

   function Read_Signed_Integer_64
     (This : in out Coded_Input_Stream.Instance)
     return PB_Int64
   is
      function PB_UInt64_To_PB_Int64 is new
        Ada.Unchecked_Conversion (Source => PB_UInt64,
                                  Target => PB_Int64);
   begin
      return PB_UInt64_To_PB_Int64 (Decode_Zig_Zag_64 (This.Read_Raw_Varint_64));
   end Read_Signed_Integer_64;

   -----------------
   -- Read_String --
   -----------------

   function Read_String
     (This : in out Coded_Input_Stream.Instance)
     return PB_String_Access
   is
      Size : constant Stream_Element_Offset := Stream_Element_Offset (This.Read_Raw_Varint_32);
      subtype Return_String_Type is PB_String (1 .. Integer (Size));
      function Convert is new Ada.Unchecked_Conversion (Stream_Element_Array, Return_String_Type);
   begin
      if Size <= (This.Buffer_Size - This.Buffer_Position) and then Size > 0 then
         -- Fast Path : We already have the bytes in a contiguous buffer, so
         --  just copy directly from it.
         declare
            Result : constant PB_String_Access :=
                       new PB_String'(Convert (
                                      This.Buffer
                                        (This.Buffer_Position .. This.Buffer_Position + Size - 1)));
         begin
            This.Buffer_Position := This.Buffer_Position + Size;
            return Result;
         end;
      else
         return Result : PB_String_Access do
            Result := new PB_String'(
                                     Convert (
                                       This.Read_Raw_Bytes (Size) (0 .. Size - 1)));
         end return;
      end if;
   end Read_String;

   --------------
   -- Read_Tag --
   --------------

   function Read_Tag
     (This : in out Coded_Input_Stream.Instance)
     return PB_UInt32
   is
      use type PB_UInt32;
   begin
      if This.Is_At_End then
         This.Last_Tag := 0;
         return 0;
      end if;
      This.Last_Tag := This.Read_Raw_Varint_32;
      if Get_Tag_Field_Number (This.Last_Tag) = 0 then
         Invalid_Protocol_Buffer_Exception.Invalid_Tag;
      end if;
      return This.Last_Tag;
   end Read_Tag;

   ------------------------------
   -- Read_Unsigned_Integer_32 --
   ------------------------------

   function Read_Unsigned_Integer_32
     (This : in out Coded_Input_Stream.Instance)
     return PB_UInt32
   is
   begin
      return This.Read_Raw_Varint_32;
   end Read_Unsigned_Integer_32;

   ------------------------------
   -- Read_Unsigned_Integer_64 --
   ------------------------------

   function Read_Unsigned_Integer_64
     (This : in out Coded_Input_Stream.Instance)
     return PB_UInt64
   is
   begin
      return This.Read_Raw_Varint_64;
   end Read_Unsigned_Integer_64;

   ----------------
   -- Skip_Field --
   ----------------

   function Skip_Field
     (This                   : in out Coded_Input_Stream.Instance;
      Tag                    : in PB_UInt32)
     return Boolean
   is
      Dummy_1 : PB_Int32;
      Dummy_2 : PB_UInt32;
      Dummy_3 : PB_UInt64;
      pragma Unreferenced (Dummy_1, Dummy_2, Dummy_3);
   begin
      case Get_Tag_Wire_Type (Tag) is
      when VARINT =>
         Dummy_1 := This.Read_Integer_32;
         return True;
      when FIXED_32 =>
         Dummy_2 := This.Read_Raw_Little_Endian_32;
         return True;
      when FIXED_64 =>
         Dummy_3 := This.Read_Raw_Little_Endian_64;
         return True;
      when LENGTH_DELIMITED =>
         declare
            Length : constant PB_UInt32 := This.Read_Raw_Varint_32;
         begin
            This.Skip_Raw_Bytes (Stream_Element_Count (Length));
         end;
         return True;
      when START_GROUP =>
         return True;
      when END_GROUP =>
         return False;
      end case;
   end Skip_Field;

   ------------------------
   -- Check_Last_Tag_Was --
   ------------------------

   procedure Check_Last_Tag_Was
     (This                   : in Coded_Input_Stream.Instance;
      Tag                    : in PB_UInt32)
   is
      use type PB_UInt32;
   begin
      if This.Last_Tag /= Tag then
         Invalid_Protocol_Buffer_Exception.Invalid_End_Tag;
      end if;
   end Check_Last_Tag_Was;

   ------------------
   -- Skip_Message --
   ------------------

   procedure Skip_Message
     (This : in out Coded_Input_Stream.Instance)
   is
      Tag : PB_UInt32;
      use type PB_UInt32;
   begin
      loop
         Tag := This.Read_Tag;

         if Tag = 0 or else (not This.Skip_Field (Tag)) then
            return;
         end if;
      end loop;
   end Skip_Message;

   ------------------
   -- Read_Message --
   ------------------

   procedure Read_Message
     (This                   : in out Coded_Input_Stream.Instance;
      Value                  : in out Google.Protobuf.Message.Instance'Class)
   is
      Length : constant PB_UInt32 := This.Read_Raw_Varint_32;

      use type PB_UInt32;
   begin
      if This.Recursion_Depth >= This.Recursion_Limit then
         Invalid_Protocol_Buffer_Exception.Recursion_Limit_Exceeded;
         return;
      end if;

      declare
         Old_Limit : constant Ada.Streams.Stream_Element_Count := This.Push_Limit (Stream_Element_Offset (Length));
      begin
         This.Recursion_Depth := This.Recursion_Depth + 1;
         Value.Merge_Partial_From_Coded_Input_Stream (This);
         This.Check_Last_Tag_Was (0);
         This.Recursion_Depth := This.Recursion_Depth - 1;
         This.Pop_Limit (Old_Limit);
      end;
   end Read_Message;

   -- ==========================================================================

   --------------------
   -- Set_Size_Limit --
   --------------------

   function Set_Size_Limit
     (This                   : in out Coded_Input_Stream.Instance;
      Limit                  : in Stream_Element_Count) return Stream_Element_Count
   is
      Old_Limit : constant Stream_Element_Count := This.Size_Limit;
   begin
      This.Size_Limit := Limit;
      return Old_Limit;
   end Set_Size_Limit;

   ------------------------
   -- Reset_Size_Counter --
   ------------------------

   procedure Reset_Size_Counter
     (This : in out Coded_Input_Stream.Instance)
   is
   begin
      This.Total_Bytes_Retired := -This.Buffer_Position;
   end Reset_Size_Counter;

   ----------------
   -- Push_Limit --
   ----------------

   function Push_Limit
     (This                   : in out Coded_Input_Stream.Instance;
      Byte_Limit             : in Stream_Element_Count) return Stream_Element_Count
   is
      New_Current_Limit : Stream_Element_Count;
      Old_Limit         : constant Stream_Element_Count := This.Current_Limit;
   begin
      New_Current_Limit := Byte_Limit + This.Total_Bytes_Retired + This.Buffer_Position;
      if Byte_Limit > Old_Limit then
         Invalid_Protocol_Buffer_Exception.Truncated_Message;
      end if;
      This.Current_Limit := New_Current_Limit;
      This.Recompute_Buffer_Size_After_Limit;
      return Old_Limit;
   end Push_Limit;

   ---------------
   -- Pop_Limit --
   ---------------

   procedure Pop_Limit
     (This                   : in out Coded_Input_Stream.Instance;
      Old_Limit              : in Stream_Element_Count)
   is
   begin
      This.Current_Limit := Old_Limit;
      This.Recompute_Buffer_Size_After_Limit;
   end Pop_Limit;

   ---------------------------
   -- Get_Bytes_Until_Limit --
   ---------------------------

   function Get_Bytes_Until_Limit
     (This : in Coded_Input_Stream.Instance) return Stream_Element_Offset
   is
   begin
      if This.Current_Limit = Stream_Element_Count'Last then
         return -1;
      end if;

      declare
         Current_Absolute_Position : constant Stream_Element_Offset :=
                                       This.Total_Bytes_Retired + This.Buffer_Position;
      begin
         return This.Current_Limit - Current_Absolute_Position;
      end;
   end Get_Bytes_Until_Limit;

   ---------------
   -- Is_At_End --
   ---------------

   function Is_At_End
     (This : in out Coded_Input_Stream.Instance) return Boolean
   is
   begin
      return (This.Buffer_Position = This.Buffer_Size)
        and then (not This.Refill_Buffer (False));
   end Is_At_End;

   --------------------------
   -- Get_Total_Bytes_Read --
   --------------------------

   function Get_Total_Bytes_Read
     (This : in Coded_Input_Stream.Instance) return Ada.Streams.Stream_Element_Count
   is
   begin
      return This.Total_Bytes_Retired + This.Buffer_Position;
   end Get_Total_Bytes_Read;

   -------------------
   -- Refill_Buffer --
   -------------------

   function Refill_Buffer
     (This                   : in out Coded_Input_Stream.Instance;
      Must_Succeed           : in Boolean) return Boolean
   is
   begin
      if This.Buffer_Position < This.Buffer_Size then
         raise Invalid_Protocol_Buffer_Exception.Protocol_Buffer_Exception with
           "Refill_Buffer called when buffer wasn't empty.";
         return False;
      end if;

      -- Have we hit the Current_Limit?
      if This.Total_Bytes_Retired + This.Buffer_Size = This.Current_Limit then
         if Must_Succeed then
            Invalid_Protocol_Buffer_Exception.Truncated_Message;
            return False;
         else
            return False;
         end if;
      end if;

      This.Total_Bytes_Retired := This.Total_Bytes_Retired + This.Buffer_Size;

      This.Buffer_Position := 0;

      declare
         Last             : Stream_Element_Offset;
         Total_Bytes_Read : Stream_Element_Count;
      begin
         This.Input_Stream.Read (This.Buffer, Last);
         -- Check for end of stream
         if Last < This.Buffer'First then
            This.Buffer_Size := 0;
            if Must_Succeed then
               Invalid_Protocol_Buffer_Exception.Truncated_Message;
               return False;
            else
               return False;
            end if;
         else
            This.Buffer_Size := Last + 1;
            This.Recompute_Buffer_Size_After_Limit;
            Total_Bytes_Read := This.Total_Bytes_Retired + This.Buffer_Size + This.Buffer_Size_After_Limit;
            if Total_Bytes_Read > This.Size_Limit then
               Invalid_Protocol_Buffer_Exception.Size_Limit_Exceeded;
               return False;
            end if;
            return True;
         end if;
      end;
   end Refill_Buffer;

   -------------------
   -- Read_Raw_Byte --
   -------------------

   function Read_Raw_Byte
     (This : in out Coded_Input_Stream.Instance) return PB_Byte
   is
      Dummy : Boolean;
      pragma Unreferenced (Dummy);
      -- Move???
      function Stream_Element_To_PB_Byte is new Ada.Unchecked_Conversion (Stream_Element, PB_Byte);
   begin
      if This.Buffer_Position = This.Buffer_Size then
         Dummy := This.Refill_Buffer (True);
      end if;

      declare
         Old_Buffer_Position : constant Stream_Element_Offset := This.Buffer_Position;
      begin
         This.Buffer_Position := This.Buffer_Position + 1;
         return Stream_Element_To_PB_Byte (This.Buffer (Old_Buffer_Position));
      end;
   end Read_Raw_Byte;

   ---------------------------------------
   -- Recompute_Buffer_Size_After_Limit --
   ---------------------------------------

   procedure Recompute_Buffer_Size_After_Limit
     (This : in out Coded_Input_Stream.Instance)
   is
   begin
      This.Buffer_Size := This.Buffer_Size + This.Buffer_Size_After_Limit;
      declare
         Buffer_End : constant Stream_Element_Count := This.Total_Bytes_Retired + This.Buffer_Size;
      begin
         if Buffer_End > This.Current_Limit then
            This.Buffer_Size_After_Limit := Buffer_End - This.Current_Limit;
            This.Buffer_Size := This.Buffer_Size - This.Buffer_Size_After_Limit;
         else
            This.Buffer_Size_After_Limit := 0;
         end if;
      end;
   end Recompute_Buffer_Size_After_Limit;

   --------------------
   -- Read_Raw_Bytes --
   --------------------

   function Read_Raw_Bytes
     (This                   : in out Coded_Input_Stream.Instance;
      Size                   : in Stream_Element_Count) return Stream_Element_Array
   is
      Dummy : Boolean;
      pragma Unreferenced (Dummy);
   begin
      -- Are we reading outside the current limit?
      if This.Total_Bytes_Retired + This.Buffer_Position + Size > This.Current_Limit then
         -- Skip bytes up to limit
         This.Skip_Raw_Bytes (This.Current_Limit - This.Total_Bytes_Retired - This.Buffer_Position);
         -- Fail here
         Invalid_Protocol_Buffer_Exception.Truncated_Message;
         return Stream_Element_Array'(0 => <>);
      end if;

      if Size <= This.Buffer_Size - This.Buffer_Position then
         -- All bytes needed are already buffered

         return Bytes : Stream_Element_Array (0 .. Size - 1) do
            Bytes := This.Buffer (This.Buffer_Position .. This.Buffer_Position + Size - 1);
            This.Buffer_Position := This.Buffer_Position + Size;
         end return;
      elsif Size < BUFFER_SIZE then
         -- Reading more bytes than are in the buffer, but not an excessive number
         -- of bytes. We can safely allocate the resulting array ahead of time.

         declare
            Bytes    : Stream_Element_Array (0 .. Size - 1);
            Position : Stream_Element_Offset := This.Buffer_Size - This.Buffer_Position;
         begin
            -- First copy what we have.
            Bytes (0 .. Position - 1) := This.Buffer (This.Buffer_Position .. This.Buffer_Position + Position - 1);
            This.Buffer_Position := This.Buffer_Size;

            Dummy := This.Refill_Buffer (True);

            while Size - Position > This.Buffer_Size loop
               Bytes (Position .. This.Buffer_Size - 1) := This.Buffer (0 .. This.Buffer_Size - 1);
               Position := Position + This.Buffer_Size;
               This.Buffer_Position := This.Buffer_Size;
               Dummy := This.Refill_Buffer (True);
            end loop;

            Bytes (Position .. Size - 1) := This.Buffer (0 .. Size - Position - 1);
            This.Buffer_Position := Size - Position;

            return Bytes;
         end;
      else
         -- The size is very large.  For security reasons, we can't allocate the
         -- entire byte array yet.  The size comes directly from the input, so a
         -- maliciously-crafted message could provide a bogus very large size in
         -- order to trick the app into allocating a lot of memory.  We avoid this
         -- by allocating and reading only a small chunk at a time, so that the
         -- malicious message must actually *be* extremely large to cause
         -- problems.  Meanwhile, we limit the allowed size of a message elsewhere.

         -- Remember the buffer markers since we'll have to copy the bytes out of
         -- it later.

         -- Consider replacing this code which might be _very_ inefficient!
         declare
            subtype Buffer_Type is Stream_Element_Array (0 .. BUFFER_SIZE - 1);
            package Buffer_Vector is new Ada.Containers.Indefinite_Vectors (Natural, Stream_Element_Array);
            Chunks : Buffer_Vector.Vector;

            Original_Buffer_Size     : constant Stream_Element_Count := This.Buffer_Size;
            Original_Buffer_Position : constant Stream_Element_Offset := This.Buffer_Position;
            Size_Left                : Stream_Element_Count := Size - (Original_Buffer_Size - Original_Buffer_Position);
         begin
            This.Total_Bytes_Retired := This.Total_Bytes_Retired + This.Buffer_Size;
            This.Buffer_Position := 0;
            This.Buffer_Size := 0;

            while Size_Left > 0 loop

               declare
                  Chunk     : Buffer_Type;
                  Position  : Stream_Element_Offset := 0;
                  Read_Size : constant Stream_Element_Count := Stream_Element_Count'Min (Size_Left, BUFFER_SIZE);
                  Last      : Stream_Element_Offset;
               begin

                  while Position < Read_Size loop
                     This.Input_Stream.Read (Chunk (Position .. Read_Size - Position - 1), Last);
                     if This.Buffer'First - 1 = Last then
                        Invalid_Protocol_Buffer_Exception.Truncated_Message;
                        return Stream_Element_Array'(0 => <>);
                     end if;
                     This.Total_Bytes_Retired := This.Total_Bytes_Retired + Last + 1;
                     Position := Last + 1;
                  end loop;

                  Size_Left := Size_Left - Read_Size;
                  Chunks.Append (Chunk (0 .. Read_Size - 1));
               end;

            end loop;

            -- OK, got everything.  Now concatenate it all into one buffer.
            declare
               Bytes    : Stream_Element_Array (0 .. Size - 1);
               Position : Stream_Element_Offset := Original_Buffer_Size - Original_Buffer_Position;
            begin
               -- Start by copying the leftover bytes from This.Buffer
               Bytes (0 .. Position - 1) := This.Buffer (Original_Buffer_Position .. Original_Buffer_Position + Position - 1);

               for E of Chunks loop
                  Bytes (Position .. Position + E'Length - 1) := E;
                  Position := Position + E'Length;
               end loop;

               return Bytes;
            end;
         end;
      end if;
   end Read_Raw_Bytes;

   --------------------
   -- Skip_Raw_Bytes --
   --------------------

   procedure Skip_Raw_Bytes
     (This                   : in out Coded_Input_Stream.Instance;
      Size                   : in Stream_Element_Count)
   is
      Dummy : Boolean;
      pragma Unreferenced (Dummy);
   begin
      -- Are we reading outside the current limit?
      if This.Total_Bytes_Retired + This.Buffer_Position + Size > This.Current_Limit then
         -- Skip bytes up to limit
         This.Skip_Raw_Bytes (This.Current_Limit - This.Total_Bytes_Retired - This.Buffer_Position);
         -- Fail here
         Invalid_Protocol_Buffer_Exception.Truncated_Message;
      end if;

      if Size <= This.Buffer_Size - This.Buffer_Position then
         -- All bytes needed are already buffered
         This.Buffer_Position := This.Buffer_Position + Size;
      else
         -- Skipping more bytes than are in the buffer.  First skip what we have.
         declare
            Position : Stream_Element_Offset := This.Buffer_Size - This.Buffer_Position;
         begin
            This.Buffer_Position := This.Buffer_Size;

            -- Keep refilling the buffer until we get to the point we wanted to skip
            -- to. This has the side effect of ensuring the limits are updated
            -- correctly.
            Dummy := This.Refill_Buffer (True);
            while Size - Position > This.Buffer_Size loop
               Position := Position + This.Buffer_Size;
               This.Buffer_Position := This.Buffer_Size;
               Dummy := This.Refill_Buffer (True);
            end loop;

            This.Buffer_Position := Size - Position;
         end;
      end if;
   end Skip_Raw_Bytes;

end Google.Protobuf.IO.Coded_Input_Stream;
