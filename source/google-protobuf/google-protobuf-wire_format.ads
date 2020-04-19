pragma Ada_2012;

with Interfaces;
with Ada.Containers.Vectors;

package Google.Protobuf.Wire_Format is
-- These are temporary types that should be replaced
-- with something more portable. ???
   subtype PB_String is String;
   subtype PB_Byte is Interfaces.Unsigned_8;
   subtype PB_UInt32 is Interfaces.Unsigned_32;
   subtype PB_UInt64 is Interfaces.Unsigned_64;
   subtype PB_Double is Interfaces.IEEE_Float_64;
   subtype PB_Float is Interfaces.IEEE_Float_32;
   subtype PB_Bool is Boolean;
   subtype PB_Int32 is Interfaces.Integer_32;
   subtype PB_Int64 is Interfaces.Integer_64;

   type PB_String_Access is access all PB_String;

   subtype PB_Field_Type is Interfaces.Unsigned_32;
   subtype PB_Object_Size is Natural;
   type PB_Wire_Type is (VARINT, FIXED_64, LENGTH_DELIMITED, START_GROUP, END_GROUP, FIXED_32);

   type Has_Bits_Array_Type is array (PB_UInt32 range <>) of PB_UInt32;

   --     -- Temporary implementation for repeated fields, consider replacing???
   --     -- Remove use clauses.
   use type PB_UInt32;
   use type PB_UInt64;
   use type PB_Double;
   use type PB_Float;
   use type PB_Bool;
   use type PB_Int32;
   use type PB_Int64;

   -- Avoid instantiating all generic packages???
   package PB_UInt32_Vector is new Ada.Containers.Vectors (PB_Object_Size, PB_UInt32);
   package PB_UInt64_Vector is new Ada.Containers.Vectors (PB_Object_Size, PB_UInt64);
   package PB_Double_Vector is new Ada.Containers.Vectors (PB_Object_Size, PB_Double);
   package PB_Float_Vector is new Ada.Containers.Vectors (PB_Object_Size, PB_Float);
   package PB_Bool_Vector is new Ada.Containers.Vectors (PB_Object_Size, PB_Bool);
   package PB_Int32_Vector is new Ada.Containers.Vectors (PB_Object_Size, PB_Int32);
   package PB_Int64_Vector is new Ada.Containers.Vectors (PB_Object_Size, PB_Int64);
   package PB_String_Access_Vector is new Ada.Containers.Vectors (PB_Object_Size, PB_String_Access);

   function Make_Tag (Field_Number : in PB_Field_Type; Wire_Type : in PB_Wire_Type) return PB_UInt32;
   function Get_Tag_Wire_Type (Tag : in PB_UInt32) return PB_Wire_Type;
   function Get_Tag_Field_Number (Tag : in PB_UInt32) return PB_Field_Type;

   function Shift_Left (Value : in Interfaces.Unsigned_8; Amount : in Natural) return Interfaces.Unsigned_8 renames Interfaces.Shift_Left;
   function Shift_Left (Value : in Interfaces.Unsigned_32; Amount : in Natural) return Interfaces.Unsigned_32 renames Interfaces.Shift_Left;
   function Shift_Left (Value : in Interfaces.Unsigned_64; Amount : in Natural) return Interfaces.Unsigned_64 renames Interfaces.Shift_Left;
   function Shift_Right (Value : in Interfaces.Unsigned_8; Amount : in Natural) return Interfaces.Unsigned_8 renames Interfaces.Shift_Right;
   function Shift_Right (Value : in Interfaces.Unsigned_32; Amount : in Natural) return Interfaces.Unsigned_32 renames Interfaces.Shift_Right;
   function Shift_Right (Value : in Interfaces.Unsigned_64; Amount : in Natural) return Interfaces.Unsigned_64 renames Interfaces.Shift_Right;
   function Shift_Right_Arithmetic (Value : in Interfaces.Unsigned_8; Amount : in Natural) return Interfaces.Unsigned_8 renames Interfaces.Shift_Right_Arithmetic;
   function Shift_Right_Arithmetic (Value : in Interfaces.Unsigned_32; Amount : in Natural) return Interfaces.Unsigned_32 renames Interfaces.Shift_Right_Arithmetic;
   function Shift_Right_Arithmetic (Value : in Interfaces.Unsigned_64; Amount : in Natural) return Interfaces.Unsigned_64 renames Interfaces.Shift_Right_Arithmetic;

   TAG_TYPE_BITS : constant := 3;
   TAG_TYPE_MASK : PB_UInt32 := Interfaces."-" (Shift_Left (1, TAG_TYPE_BITS) , 1);

end Google.Protobuf.Wire_Format;
