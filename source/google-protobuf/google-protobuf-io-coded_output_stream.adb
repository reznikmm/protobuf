pragma Ada_2012;

with Ada.Unchecked_Conversion;
with Google.Protobuf.Message;
with Interfaces;
with Gnat.Byte_Swapping;

package body Google.Protobuf.IO.Coded_Output_Stream is

   -----------------------
   -- Encode_Zig_Zag_32 --
   -----------------------

   function Encode_Zig_Zag_32
     (Value : in PB_Int32)
      return PB_UInt32
   is
      function PB_Int32_To_PB_UInt32 is new Ada.Unchecked_Conversion (Source => PB_Int32,
                                                                      Target => PB_UInt32);
      Value_To_PB_UInt32 : constant PB_UInt32 := PB_Int32_To_PB_UInt32 (Value);
      use type PB_UInt32;
   begin
      return Shift_Left (Value_To_PB_UInt32, 1) xor Shift_Right_Arithmetic (Value_To_PB_UInt32, 31);
   end Encode_Zig_Zag_32;

   -----------------------
   -- Encode_Zig_Zag_64 --
   -----------------------

   function Encode_Zig_Zag_64
     (Value : in PB_Int64)
      return PB_UInt64
   is
      function PB_Int64_To_PB_UInt64 is new Ada.Unchecked_Conversion (Source => PB_Int64,
                                                                      Target => PB_UInt64);
      Value_To_PB_UInt64 : constant PB_UInt64 := PB_Int64_To_PB_UInt64 (Value);
      use type PB_UInt64;
   begin
      return Shift_Left (Value_To_PB_UInt64, 1) xor Shift_Right_Arithmetic (Value_To_PB_UInt64, 63);
   end Encode_Zig_Zag_64;

   --------------------------------
   -- Compute_Raw_Varint_32_Size --
   --------------------------------

   function Compute_Raw_Varint_32_Size
     (Value : in PB_UInt32)
      return PB_Object_Size
   is
      Value_To_Unsigned_32 : constant Interfaces.Unsigned_32 := Interfaces.Unsigned_32 (Value);
      use type Interfaces.Unsigned_32;
   begin
      if Interfaces.Shift_Right (Value_To_Unsigned_32,  7) = 0 then return 1; end if;
      if Interfaces.Shift_Right (Value_To_Unsigned_32, 14) = 0 then return 2; end if;
      if Interfaces.Shift_Right (Value_To_Unsigned_32, 21) = 0 then return 3; end if;
      if Interfaces.Shift_Right (Value_To_Unsigned_32, 28) = 0 then return 4; end if;
      return 5;
   end Compute_Raw_Varint_32_Size;

   --------------------------------
   -- Compute_Raw_Varint_64_Size --
   --------------------------------

   function Compute_Raw_Varint_64_Size
     (Value : in PB_UInt64)
      return PB_Object_Size
   is
      Value_To_Unsigned_64 : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (Value);
      use type Interfaces.Unsigned_64;
   begin
      if Interfaces.Shift_Right (Value_To_Unsigned_64,  7) = 0 then return 1; end if;
      if Interfaces.Shift_Right (Value_To_Unsigned_64, 14) = 0 then return 2; end if;
      if Interfaces.Shift_Right (Value_To_Unsigned_64, 21) = 0 then return 3; end if;
      if Interfaces.Shift_Right (Value_To_Unsigned_64, 28) = 0 then return 4; end if;
      if Interfaces.Shift_Right (Value_To_Unsigned_64, 35) = 0 then return 5; end if;
      if Interfaces.Shift_Right (Value_To_Unsigned_64, 42) = 0 then return 6; end if;
      if Interfaces.Shift_Right (Value_To_Unsigned_64, 49) = 0 then return 7; end if;
      if Interfaces.Shift_Right (Value_To_Unsigned_64, 56) = 0 then return 8; end if;
      if Interfaces.Shift_Right (Value_To_Unsigned_64, 63) = 0 then return 9; end if;
      return 10;
   end Compute_Raw_Varint_64_Size;

   --------------------------
   -- Compute_Boolean_Size --
   --------------------------

   function Compute_Boolean_Size
     (Field_Number : in PB_Field_Type;
      Value        : in PB_Bool)
      return PB_Object_Size
   is
   begin
      return Compute_Tag_Size (Field_Number) + Compute_Boolean_Size_No_Tag (Value);
   end Compute_Boolean_Size;

   -------------------------
   -- Compute_Double_Size --
   -------------------------

   function Compute_Double_Size
     (Field_Number : in PB_Field_Type;
      Value        : in PB_Double)
      return PB_Object_Size
   is
   begin
      return Compute_Tag_Size (Field_Number) + Compute_Double_Size_No_Tag (Value);
   end Compute_Double_Size;

   ------------------------------
   -- Compute_Enumeration_Size --
   ------------------------------

   function Compute_Enumeration_Size
     (Field_Number : in PB_Field_Type;
      Value        : in PB_Int32)
      return PB_Object_Size
   is
   begin
      return Compute_Tag_Size (Field_Number) + Compute_Enumeration_Size_No_Tag (Value);
   end Compute_Enumeration_Size;

   ---------------------------
   -- Compute_Fixed_32_Size --
   ---------------------------

   function Compute_Fixed_32_Size
     (Field_Number : in PB_Field_Type;
      Value        : in PB_UInt32)
      return PB_Object_Size
   is
   begin
      return Compute_Tag_Size (Field_Number) + Compute_Fixed_32_Size_No_Tag (Value);
   end Compute_Fixed_32_Size;

   ---------------------------
   -- Compute_Fixed_64_Size --
   ---------------------------

   function Compute_Fixed_64_Size
     (Field_Number : in PB_Field_Type;
      Value        : in PB_UInt64)
      return PB_Object_Size
   is
   begin
      return Compute_Tag_Size (Field_Number) + Compute_Fixed_64_Size_No_Tag (Value);
   end Compute_Fixed_64_Size;

   ------------------------
   -- Compute_Float_Size --
   ------------------------

   function Compute_Float_Size
     (Field_Number : in PB_Field_Type;
      Value        : in PB_Float)
      return PB_Object_Size
   is
   begin
      return Compute_Tag_Size (Field_Number) + Compute_Float_Size_No_Tag (Value);
   end Compute_Float_Size;

   -----------------------------
   -- Compute_Integer_32_Size --
   -----------------------------

   function Compute_Integer_32_Size
     (Field_Number : in PB_Field_Type;
      Value        : in PB_Int32)
      return PB_Object_Size
   is
   begin
      return Compute_Tag_Size (Field_Number) + Compute_Integer_32_Size_No_Tag (Value);
   end Compute_Integer_32_Size;

   -----------------------------
   -- Compute_Integer_64_Size --
   -----------------------------

   function Compute_Integer_64_Size
     (Field_Number : in PB_Field_Type;
      Value        : in PB_Int64)
      return PB_Object_Size
   is
   begin
      return Compute_Tag_Size (Field_Number) + Compute_Integer_64_Size_No_Tag (Value);
   end Compute_Integer_64_Size;

   ----------------------------------
   -- Compute_Signed_Fixed_32_Size --
   ----------------------------------

   function Compute_Signed_Fixed_32_Size
     (Field_Number : in PB_Field_Type;
      Value        : in PB_Int32)
      return PB_Object_Size
   is
   begin
      return Compute_Tag_Size (Field_Number) + Compute_Signed_Fixed_32_Size_No_Tag (Value);
   end Compute_Signed_Fixed_32_Size;

   ----------------------------------
   -- Compute_Signed_Fixed_64_Size --
   ----------------------------------

   function Compute_Signed_Fixed_64_Size
     (Field_Number : in PB_Field_Type;
      Value        : in PB_Int64)
      return PB_Object_Size
   is
   begin
      return Compute_Tag_Size (Field_Number) + Compute_Signed_Fixed_64_Size_No_Tag (Value);
   end Compute_Signed_Fixed_64_Size;

   ------------------------------------
   -- Compute_Signed_Integer_32_Size --
   ------------------------------------

   function Compute_Signed_Integer_32_Size
     (Field_Number : in PB_Field_Type;
      Value        : in PB_Int32)
      return PB_Object_Size
   is
   begin
      return Compute_Tag_Size (Field_Number) + Compute_Signed_Integer_32_Size_No_Tag (Value);
   end Compute_Signed_Integer_32_Size;

   ------------------------------------
   -- Compute_Signed_Integer_64_Size --
   ------------------------------------

   function Compute_Signed_Integer_64_Size
     (Field_Number : in PB_Field_Type;
      Value        : in PB_Int64)
      return PB_Object_Size
   is
   begin
      return Compute_Tag_Size (Field_Number) + Compute_Signed_Integer_64_Size_No_Tag (Value);
   end Compute_Signed_Integer_64_Size;

   -------------------------
   -- Compute_String_Size --
   -------------------------

   function Compute_String_Size
     (Field_Number : in PB_Field_Type;
      Value        : in PB_String)
      return PB_Object_Size
   is
   begin
      return Compute_Tag_Size (Field_Number) + Compute_String_Size_No_Tag (Value);
   end Compute_String_Size;

   ----------------------
   -- Compute_Tag_Size --
   ----------------------

   function Compute_Tag_Size
     (Field_Number : PB_Field_Type)
      return PB_Object_Size
   is
      Tag : constant PB_UInt32 := Wire_Format.Make_Tag (Field_Number, PB_Wire_Type'Val (0));
   begin
      return Compute_Raw_Varint_32_Size (Tag);
   end Compute_Tag_Size;

   --------------------------------------
   -- Compute_Unsigned_Integer_32_Size --
   --------------------------------------

   function Compute_Unsigned_Integer_32_Size
     (Field_Number : in PB_Field_Type;
      Value        : in PB_UInt32)
      return PB_Object_Size
   is
   begin
      return Compute_Tag_Size (Field_Number) + Compute_Unsigned_Integer_32_Size_No_Tag (Value);
   end Compute_Unsigned_Integer_32_Size;

   --------------------------------------
   -- Compute_Unsigned_Integer_64_Size --
   --------------------------------------

   function Compute_Unsigned_Integer_64_Size
     (Field_Number : in PB_Field_Type;
      Value        : in PB_UInt64)
      return PB_Object_Size
   is
   begin
      return Compute_Tag_Size (Field_Number) + Compute_Unsigned_Integer_64_Size_No_Tag (Value);
   end Compute_Unsigned_Integer_64_Size;

   ---------------------------------
   -- Compute_Boolean_Size_No_Tag --
   ---------------------------------

   function Compute_Boolean_Size_No_Tag
     (Value : in PB_Bool)
      return PB_Object_Size
   is
      pragma Unreferenced (Value);
   begin
      return 1;
   end Compute_Boolean_Size_No_Tag;

   --------------------------------
   -- Compute_Double_Size_No_Tag --
   --------------------------------

   function Compute_Double_Size_No_Tag
     (Value : in PB_Double)
      return PB_Object_Size
   is
      pragma Unreferenced (Value);
   begin
      return TMP_LITTLE_ENDIAN_64_SIZE;
   end Compute_Double_Size_No_Tag;

   -------------------------------------
   -- Compute_Enumeration_Size_No_Tag --
   -------------------------------------

   function Compute_Enumeration_Size_No_Tag
     (Value : in PB_Int32)
      return PB_Object_Size
   is
   begin
      return Compute_Integer_32_Size_No_Tag (Value);
   end Compute_Enumeration_Size_No_Tag;

   ----------------------------------
   -- Compute_Fixed_32_Size_No_Tag --
   ----------------------------------

   function Compute_Fixed_32_Size_No_Tag
     (Value : in PB_UInt32)
      return PB_Object_Size
   is
      pragma Unreferenced (Value);
   begin
      return TMP_LITTLE_ENDIAN_32_SIZE;
   end Compute_Fixed_32_Size_No_Tag;

   ----------------------------------
   -- Compute_Fixed_64_Size_No_Tag --
   ----------------------------------

   function Compute_Fixed_64_Size_No_Tag
     (Value : in PB_UInt64)
      return PB_Object_Size
   is
      pragma Unreferenced (Value);
   begin
      return TMP_LITTLE_ENDIAN_64_SIZE;
   end Compute_Fixed_64_Size_No_Tag;

   -------------------------------
   -- Compute_Float_Size_No_Tag --
   -------------------------------

   function Compute_Float_Size_No_Tag
     (Value : in PB_Float)
      return PB_Object_Size
   is
      pragma Unreferenced (Value);
   begin
      return TMP_LITTLE_ENDIAN_32_SIZE;
   end Compute_Float_Size_No_Tag;

   ------------------------------------
   -- Compute_Integer_32_Size_No_Tag --
   ------------------------------------

   function Compute_Integer_32_Size_No_Tag
     (Value : in PB_Int32)
      return PB_Object_Size
   is
      function PB_Int32_To_PB_UInt32 is new Ada.Unchecked_Conversion (Source => PB_Int32,
                                                                      Target => PB_UInt32);
      use type Google.Protobuf.Wire_Format.PB_Int32;
   begin
      if Value >= 0 then
         return Compute_Raw_Varint_32_Size (PB_Int32_To_PB_UInt32 (Value));
      else
         -- Value must be sign-extended. See More Value Types
         -- https://developers.google.com/protocol-buffers/docs/encoding
         return 10;
      end if;
   end Compute_Integer_32_Size_No_Tag;

   ------------------------------------
   -- Compute_Integer_64_Size_No_Tag --
   ------------------------------------

   function Compute_Integer_64_Size_No_Tag
     (Value : in PB_Int64)
      return PB_Object_Size
   is
      function PB_Int64_To_PB_UInt64 is new Ada.Unchecked_Conversion (Source => PB_Int64,
                                                                      Target => PB_UInt64);
   begin
      return Compute_Raw_Varint_64_Size (PB_Int64_To_PB_UInt64 (Value));
   end Compute_Integer_64_Size_No_Tag;

   -----------------------------------------
   -- Compute_Signed_Fixed_32_Size_No_Tag --
   -----------------------------------------

   function Compute_Signed_Fixed_32_Size_No_Tag
     (Value : in PB_Int32)
      return PB_Object_Size
   is
      pragma Unreferenced (Value);
   begin
      return TMP_LITTLE_ENDIAN_32_SIZE;
   end Compute_Signed_Fixed_32_Size_No_Tag;

   -----------------------------------------
   -- Compute_Signed_Fixed_64_Size_No_Tag --
   -----------------------------------------

   function Compute_Signed_Fixed_64_Size_No_Tag
     (Value : in PB_Int64)
      return PB_Object_Size
   is
      pragma Unreferenced (Value);
   begin
      return TMP_LITTLE_ENDIAN_64_SIZE;
   end Compute_Signed_Fixed_64_Size_No_Tag;

   -------------------------------------------
   -- Compute_Signed_Integer_32_Size_No_Tag --
   -------------------------------------------

   function Compute_Signed_Integer_32_Size_No_Tag
     (Value : in PB_Int32)
      return PB_Object_Size
   is
   begin
      return Compute_Raw_Varint_32_Size (Encode_Zig_Zag_32 (Value));
   end Compute_Signed_Integer_32_Size_No_Tag;

   -------------------------------------------
   -- Compute_Signed_Integer_64_Size_No_Tag --
   -------------------------------------------

   function Compute_Signed_Integer_64_Size_No_Tag
     (Value : in PB_Int64)
      return PB_Object_Size
   is
   begin
      return Compute_Raw_Varint_64_Size (Encode_Zig_Zag_64 (Value));
   end Compute_Signed_Integer_64_Size_No_Tag;

   --------------------------------
   -- Compute_String_Size_No_Tag --
   --------------------------------

   function Compute_String_Size_No_Tag
     (Value : in PB_String)
      return PB_Object_Size
   is
   begin
      return Compute_Raw_Varint_32_Size (Value'Length) + PB_Object_Size (Value'Length);
   end Compute_String_Size_No_Tag;

   ---------------------------------------------
   -- Compute_Unsigned_Integer_32_Size_No_Tag --
   ---------------------------------------------

   function Compute_Unsigned_Integer_32_Size_No_Tag
     (Value : in PB_UInt32)
      return PB_Object_Size
   is
   begin
      return Compute_Raw_Varint_32_Size (Value);
   end Compute_Unsigned_Integer_32_Size_No_Tag;

   ---------------------------------------------
   -- Compute_Unsigned_Integer_64_Size_No_Tag --
   ---------------------------------------------

   function Compute_Unsigned_Integer_64_Size_No_Tag
     (Value : in PB_UInt64)
      return PB_Object_Size
   is
   begin
      return Compute_Raw_Varint_64_Size (Value);
   end Compute_Unsigned_Integer_64_Size_No_Tag;

   --------------------------
   -- Compute_Message_Size --
   --------------------------

   function Compute_Message_Size
     (Field_Number : in PB_Field_Type;
      Value        : in out Google.Protobuf.Message.Instance'Class)
      return PB_Object_Size
   is
   begin
      return Compute_Tag_Size (Field_Number) + Compute_Message_Size_No_Tag (Value);
   end Compute_Message_Size;

   --------------------------
   -- Compute_Message_Size --
   --------------------------

   function Compute_Message_Size_No_Tag
     (Value : in out Google.Protobuf.Message.Instance'Class)
      return PB_Object_Size
   is
      Size : PB_Object_Size;
   begin
      Size := Value.Byte_Size;
      -- Change to Unchecked_Conversion???
      return Compute_Raw_Varint_32_Size (PB_UInt32 (Size)) + Size;
   end Compute_Message_Size_No_Tag;

   -------------------
   -- Write_Boolean --
   -------------------

   procedure Write_Boolean
     (This                    : in Coded_Output_Stream.Instance;
      Field_Number            : in PB_Field_Type;
      Value                   : in PB_Bool)
   is
   begin
      This.Write_Tag (Field_Number, Wire_Format.VARINT);
      This.Write_Boolean_No_Tag (Value);
   end Write_Boolean;

   ------------------
   -- Write_Double --
   ------------------

   procedure Write_Double
     (This                    : in Coded_Output_Stream.Instance;
      Field_Number            : in PB_Field_Type;
      Value                   : in PB_Double)
   is
   begin
      This.Write_Tag (Field_Number, Wire_Format.FIXED_64);
      This.Write_Double_No_Tag (Value);
   end Write_Double;

   -----------------------
   -- Write_Enumeration --
   -----------------------

   procedure Write_Enumeration
     (This                    : in Coded_Output_Stream.Instance;
      Field_Number            : in PB_Field_Type;
      Value                   : in PB_Int32)
   is
   begin
      This.Write_Tag (Field_Number, Wire_Format.VARINT);
      This.Write_Enumeration_No_Tag (Value);
   end Write_Enumeration;

   --------------------
   -- Write_Fixed_32 --
   --------------------

   procedure Write_Fixed_32
     (This                    : in Coded_Output_Stream.Instance;
      Field_Number            : in PB_Field_Type;
      Value                   : in PB_UInt32)
   is
   begin
      This.Write_Tag (Field_Number, Wire_Format.FIXED_32);
      This.Write_Fixed_32_No_Tag (Value);
   end Write_Fixed_32;

   --------------------
   -- Write_Fixed_64 --
   --------------------

   procedure Write_Fixed_64
     (This                    : in Coded_Output_Stream.Instance;
      Field_Number            : in PB_Field_Type;
      Value                   : in PB_UInt64)
   is
   begin
      This.Write_Tag (Field_Number, Wire_Format.FIXED_64);
      This.Write_Fixed_64_No_Tag (Value);
   end Write_Fixed_64;

   -----------------
   -- Write_Float --
   -----------------

   procedure Write_Float
     (This                    : in Coded_Output_Stream.Instance;
      Field_Number            : in PB_Field_Type;
      Value                   : in PB_Float)
   is
   begin
      This.Write_Tag (Field_Number, Wire_Format.FIXED_32);
      This.Write_Float_No_Tag (Value);
   end Write_Float;

   ----------------------
   -- Write_Integer_32 --
   ----------------------

   procedure Write_Integer_32
     (This                    : in Coded_Output_Stream.Instance;
      Field_Number            : in PB_Field_Type;
      Value                   : in PB_Int32)
   is
   begin
      This.Write_Tag (Field_Number, Wire_Format.VARINT);
      This.Write_Integer_32_No_Tag (Value);
   end Write_Integer_32;

   ----------------------
   -- Write_Integer_64 --
   ----------------------

   procedure Write_Integer_64
     (This                    : in Coded_Output_Stream.Instance;
      Field_Number            : in PB_Field_Type;
      Value                   : in PB_Int64)
   is
   begin
      This.Write_Tag (Field_Number, Wire_Format.VARINT);
      This.Write_Integer_64_No_Tag (Value);
   end Write_Integer_64;

   ---------------------------
   -- Write_Signed_Fixed_32 --
   ---------------------------

   procedure Write_Signed_Fixed_32
     (This                    : in Coded_Output_Stream.Instance;
      Field_Number            : in PB_Field_Type;
      Value                   : in PB_Int32)
   is
   begin
      This.Write_Tag (Field_Number, Wire_Format.FIXED_32);
      This.Write_Signed_Fixed_32_No_Tag (Value);
   end Write_Signed_Fixed_32;

   ---------------------------
   -- Write_Signed_Fixed_64 --
   ---------------------------

   procedure Write_Signed_Fixed_64
     (This                    : in Coded_Output_Stream.Instance;
      Field_Number            : in PB_Field_Type;
      Value                   : in PB_Int64)
   is
   begin
      This.Write_Tag (Field_Number, Wire_Format.FIXED_64);
      This.Write_Signed_Fixed_64_No_Tag (Value);
   end Write_Signed_Fixed_64;

   -----------------------------
   -- Write_Signed_Integer_32 --
   -----------------------------

   procedure Write_Signed_Integer_32
     (This                    : in Coded_Output_Stream.Instance;
      Field_Number            : in PB_Field_Type;
      Value                   : in PB_Int32)
   is
   begin
      This.Write_Tag (Field_Number, Wire_Format.VARINT);
      This.Write_Signed_Integer_32_No_Tag (Value);
   end Write_Signed_Integer_32;

   -----------------------------
   -- Write_Signed_Integer_64 --
   -----------------------------

   procedure Write_Signed_Integer_64
     (This                    : in Coded_Output_Stream.Instance;
      Field_Number            : in PB_Field_Type;
      Value                   : in PB_Int64)
   is
   begin
      This.Write_Tag (Field_Number, Wire_Format.VARINT);
      This.Write_Signed_Integer_64_No_Tag (Value);
   end Write_Signed_Integer_64;

   ------------------
   -- Write_String --
   ------------------

   procedure Write_String
     (This                    : in Coded_Output_Stream.Instance;
      Field_Number            : in PB_Field_Type;
      Value                   : in PB_String)
   is
   begin
      This.Write_Tag (Field_Number, Wire_Format.LENGTH_DELIMITED);
      This.Write_String_No_Tag (Value);
   end Write_String;

   -------------------------------
   -- Write_Unsigned_Integer_32 --
   -------------------------------

   procedure Write_Unsigned_Integer_32
     (This                    : in Coded_Output_Stream.Instance;
      Field_Number            : in PB_Field_Type;
      Value                   : in PB_UInt32)
   is
   begin
      This.Write_Tag (Field_Number, Wire_Format.VARINT);
      This.Write_Unsigned_Integer_32_No_Tag (Value);
   end Write_Unsigned_Integer_32;

   -------------------------------
   -- Write_Unsigned_Integer_64 --
   -------------------------------

   procedure Write_Unsigned_Integer_64
     (This                    : in Coded_Output_Stream.Instance;
      Field_Number            : in PB_Field_Type;
      Value                   : in PB_UInt64)
   is
   begin
      This.Write_Tag (Field_Number, Wire_Format.VARINT);
      This.Write_Unsigned_Integer_64_No_Tag (Value);
   end Write_Unsigned_Integer_64;

   --------------------------
   -- Write_Boolean_No_Tag --
   --------------------------

   procedure Write_Boolean_No_Tag
     (This                    : in Coded_Output_Stream.Instance;
      Value                   : in PB_Bool)
   is
   begin
      if Value then
         This.Write_Raw_Byte (1);
      else
         This.Write_Raw_Byte (0);
      end if;
   end Write_Boolean_No_Tag;

   -------------------------
   -- Write_Double_No_Tag --
   -------------------------

   procedure Write_Double_No_Tag
     (This                    : in Coded_Output_Stream.Instance;
      Value                   : in PB_Double)
   is
      function PB_Double_To_PB_UInt64 is new Ada.Unchecked_Conversion (Source => PB_Double,
                                                                       Target => PB_UInt64);
   begin
      This.Write_Raw_Little_Endian_64 (PB_Double_To_PB_UInt64 (Value));
   end Write_Double_No_Tag;

   ------------------------------
   -- Write_Enumeration_No_Tag --
   ------------------------------

   procedure Write_Enumeration_No_Tag
     (This                    : in Coded_Output_Stream.Instance;
      Value                   : in PB_Int32)
   is
   begin
      This.Write_Integer_32_No_Tag (Value);
   end Write_Enumeration_No_Tag;

   ---------------------------
   -- Write_Fixed_32_No_Tag --
   ---------------------------

   procedure Write_Fixed_32_No_Tag
     (This                    : in Coded_Output_Stream.Instance;
      Value                   : in PB_UInt32)
   is
   begin
      This.Write_Raw_Little_Endian_32 (Value);
   end Write_Fixed_32_No_Tag;

   ---------------------------
   -- Write_Fixed_64_No_Tag --
   ---------------------------

   procedure Write_Fixed_64_No_Tag
     (This                    : in Coded_Output_Stream.Instance;
      Value                   : in PB_UInt64)
   is
   begin
      This.Write_Raw_Little_Endian_64 (Value);
   end Write_Fixed_64_No_Tag;

   ------------------------
   -- Write_Float_No_Tag --
   ------------------------

   procedure Write_Float_No_Tag
     (This                    : in Coded_Output_Stream.Instance;
      Value                   : in PB_Float)
   is
      function PB_Float_To_PB_UInt32 is new Ada.Unchecked_Conversion (Source => PB_Float,
                                                                      Target => PB_UInt32);
   begin
      This.Write_Raw_Little_Endian_32 (PB_Float_To_PB_UInt32 (Value));
   end Write_Float_No_Tag;

   -----------------------------
   -- Write_Integer_32_No_Tag --
   -----------------------------

   procedure Write_Integer_32_No_Tag
     (This                    : in Coded_Output_Stream.Instance;
      Value                   : in PB_Int32)
   is
      function PB_Int32_To_PB_UInt32 is new Ada.Unchecked_Conversion (Source => PB_Int32,
                                                                      Target => PB_UInt32);

      function PB_Int64_To_PB_UInt64 is new Ada.Unchecked_Conversion (Source => PB_Int64,
                                                                      Target => PB_UInt64);

      Value_As_Unsigned_Integer : constant PB_UInt32 := PB_Int32_To_PB_UInt32 (Value);
      Value_As_Long             : constant PB_Int64 := PB_Int64 (Value);
      Value_As_Unsigned_Long    : constant PB_UInt64 := PB_Int64_To_PB_UInt64 (Value_As_Long);

      use type Google.Protobuf.Wire_Format.PB_Int32;
   begin
      if Value >= 0 then
         This.Write_Raw_Varint_32 (Value_As_Unsigned_Integer);
      else
         This.Write_Raw_Varint_64 (Value_As_Unsigned_Long);
      end if;
   end Write_Integer_32_No_Tag;

   -----------------------------
   -- Write_Integer_64_No_Tag --
   -----------------------------

   procedure Write_Integer_64_No_Tag
     (This                    : in Coded_Output_Stream.Instance;
      Value                   : in PB_Int64)
   is
      function PB_Int64_To_PB_UInt64 is new Ada.Unchecked_Conversion (Source => PB_Int64,
                                                                      Target => PB_UInt64);
   begin
      This.Write_Raw_Varint_64 (PB_Int64_To_PB_UInt64 (Value));
   end Write_Integer_64_No_Tag;

   ----------------------------------
   -- Write_Signed_Fixed_32_No_Tag --
   ----------------------------------

   procedure Write_Signed_Fixed_32_No_Tag
     (This                    : in Coded_Output_Stream.Instance;
      Value                   : in PB_Int32)
   is
      function PB_Int32_To_PB_UInt32 is new Ada.Unchecked_Conversion (Source => PB_Int32,
                                                                      Target => PB_UInt32);
   begin
      This.Write_Raw_Little_Endian_32 (PB_Int32_To_PB_UInt32 (Value));
   end Write_Signed_Fixed_32_No_Tag;

   ----------------------------------
   -- Write_Signed_Fixed_64_No_Tag --
   ----------------------------------

   procedure Write_Signed_Fixed_64_No_Tag
     (This                    : in Coded_Output_Stream.Instance;
      Value                   : in PB_Int64)
   is
      function PB_Int64_To_PB_UInt64 is new Ada.Unchecked_Conversion (Source => PB_Int64,
                                                                      Target => PB_UInt64);
   begin
      This.Write_Raw_Little_Endian_64 (PB_Int64_To_PB_UInt64 (Value));
   end Write_Signed_Fixed_64_No_Tag;

   ------------------------------------
   -- Write_Signed_Integer_32_No_Tag --
   ------------------------------------

   procedure Write_Signed_Integer_32_No_Tag
     (This                    : in Coded_Output_Stream.Instance;
      Value                   : in PB_Int32)
   is
   begin
      This.Write_Raw_Varint_32 (Encode_Zig_Zag_32 (Value));
   end Write_Signed_Integer_32_No_Tag;

   ------------------------------------
   -- Write_Signed_Integer_64_No_Tag --
   ------------------------------------

   procedure Write_Signed_Integer_64_No_Tag
     (This                    : in Coded_Output_Stream.Instance;
      Value                   : in PB_Int64)
   is
   begin
      This.Write_Raw_Varint_64 (Encode_Zig_Zag_64 (Value));
   end Write_Signed_Integer_64_No_Tag;

   -------------------------
   -- Write_String_No_Tag --
   -------------------------

   procedure Write_String_No_Tag
     (This                    : in Coded_Output_Stream.Instance;
      Value                   : in PB_String)
   is
   begin
      -- Temporary implementation consider changing ???
      This.Write_Raw_Varint_32 (Value'Length);
      PB_String'Write (This.Output_Stream, Value);
   end Write_String_No_Tag;

   --------------------------------------
   -- Write_Unsigned_Integer_32_No_Tag --
   --------------------------------------

   procedure Write_Unsigned_Integer_32_No_Tag
     (This                    : in Coded_Output_Stream.Instance;
      Value                   : in PB_UInt32)
   is
   begin
      This.Write_Raw_Varint_32 (Value);
   end Write_Unsigned_Integer_32_No_Tag;

   --------------------------------------
   -- Write_Unsigned_Integer_64_No_Tag --
   --------------------------------------

   procedure Write_Unsigned_Integer_64_No_Tag
     (This                    : in Coded_Output_Stream.Instance;
      Value                   : in PB_UInt64)
   is
   begin
      This.Write_Raw_Varint_64 (Value);
   end Write_Unsigned_Integer_64_No_Tag;

   ---------------
   -- Write_Tag --
   ---------------

   procedure Write_Tag
     (This                    : in Coded_Output_Stream.Instance;
      Field_Number            : in PB_Field_Type;
      Wire_Type               : in PB_Wire_Type)
   is

      Tag : constant PB_UInt32 := Wire_Format.Make_Tag (Field_Number => Field_Number,
                                                        Wire_Type    => Wire_Type);
   begin
      This.Write_Raw_Varint_32 (Tag);
   end Write_Tag;

   --------------------------------
   -- Write_Raw_Little_Endian_32 --
   --------------------------------

   procedure Write_Raw_Little_Endian_32
     (This                    : in Coded_Output_Stream.Instance;
      Value                   : in PB_UInt32)
   is
      Value_Copy : Pb_Uint32 := Value;
   begin
      if Big_Endian then
         -- If we are on a big endian system like PowerPC, do something here
         --raise Big_Endian_Not_Implemented;
         Gnat.Byte_Swapping.Swap4 (Value_Copy'Address);
         Pb_Uint32'Write (This.Output_Stream, Value_Copy);
      else
         Pb_Uint32'Write (This.Output_Stream, Value);
      end if;
   end Write_Raw_Little_Endian_32;

   --------------------------------
   -- Write_Raw_Little_Endian_64 --
   --------------------------------

   procedure Write_Raw_Little_Endian_64
     (This                    : in Coded_Output_Stream.Instance;
      Value                   : in PB_UInt64)
   is
      Value_Copy : Pb_Uint64 := Value;
   begin
      if Big_Endian then
         -- If we are on a big endian system like PowerPC, do something here
         --raise Big_Endian_Not_Implemented;
         Gnat.Byte_Swapping.Swap8 (Value_Copy'Address);
         Pb_Uint64'Write (This.Output_Stream, Value_Copy);
      else
         Pb_Uint64'Write (This.Output_Stream, Value);
      end if;
   end Write_Raw_Little_Endian_64;

   -------------------------
   -- Write_Raw_Varint_32 --
   -------------------------

   procedure Write_Raw_Varint_32
     (This                    : in Coded_Output_Stream.Instance;
      Value                   : in PB_UInt32)
   is
      Value_To_Unsigned_32 : Interfaces.Unsigned_32 := Interfaces.Unsigned_32 (Value);
      pragma Warnings (Off, "types for unchecked conversion have different sizes");
      function Unsigned_32_To_PB_Byte is new Ada.Unchecked_Conversion (Source => Interfaces.Unsigned_32,
                                                                       Target => PB_Byte);
      pragma Warnings (On, "types for unchecked conversion have different sizes");
      use type Interfaces.Unsigned_32;
   begin
      loop
         if (Value_To_Unsigned_32 and (not 16#7F#)) = 0 then
            This.Write_Raw_Byte (Unsigned_32_To_PB_Byte (Value_To_Unsigned_32));
            return;
         else
            This.Write_Raw_Byte (Unsigned_32_To_PB_Byte ((Value_To_Unsigned_32 and 16#7f#) or 16#80#));
            Value_To_Unsigned_32 := Interfaces.Shift_Right (Value  => Value_To_Unsigned_32,
                                                            Amount => 7);
         end if;
      end loop;
   end Write_Raw_Varint_32;

   -------------------------
   -- Write_Raw_Varint_64 --
   -------------------------

   procedure Write_Raw_Varint_64
     (This                    : in Coded_Output_Stream.Instance;
      Value                   : in PB_UInt64)
   is
      Value_To_Unsigned_64 : Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (Value);
      pragma Warnings (Off, "types for unchecked conversion have different sizes");
      function Unsigned_64_To_PB_Byte is new Ada.Unchecked_Conversion (Source => Interfaces.Unsigned_64,
                                                                       Target => PB_Byte);
      pragma Warnings (On, "types for unchecked conversion have different sizes");
      use type Interfaces.Unsigned_64;
   begin
      loop
         if (Value_To_Unsigned_64 and (not 16#7F#)) = 0 then
            This.Write_Raw_Byte (Unsigned_64_To_PB_Byte (Value_To_Unsigned_64));
            return;
         else
            This.Write_Raw_Byte (Unsigned_64_To_PB_Byte ((Value_To_Unsigned_64 and 16#7f#) or 16#80#));
            Value_To_Unsigned_64 := Interfaces.Shift_Right (Value  => Value_To_Unsigned_64,
                                                            Amount => 7);
         end if;
      end loop;
   end Write_Raw_Varint_64;

   --------------------
   -- Write_Raw_Byte --
   --------------------

   procedure Write_Raw_Byte
     (This                    : in Coded_Output_Stream.Instance;
      Value                   : in PB_Byte)
   is
   begin
      PB_Byte'Write (This.Output_Stream, Value);
   end Write_Raw_Byte;

   -------------------
   -- Write_Message --
   -------------------

   procedure Write_Message
     (This                    : in Coded_Output_Stream.Instance;
      Field_Number            : in PB_Field_Type;
      Value                   : in Google.Protobuf.Message.Instance'Class)
   is
   begin
      This.Write_Tag (Field_Number, LENGTH_DELIMITED);
      This.Write_Message_No_Tag (Value);
   end Write_Message;

   --------------------------
   -- Write_Message_No_Tag --
   --------------------------

   procedure Write_Message_No_Tag
     (This                    : in Coded_Output_Stream.Instance;
      Value                   : in Google.Protobuf.Message.Instance'Class)
   is
   begin
      -- Change to Unchecked_Conversion
      This.Write_Raw_Varint_32 (PB_UInt32 (Value.Get_Cached_Size));
      Value.Serialize_With_Cached_Sizes (This);
   end Write_Message_No_Tag;

end Google.Protobuf.IO.Coded_Output_Stream;
