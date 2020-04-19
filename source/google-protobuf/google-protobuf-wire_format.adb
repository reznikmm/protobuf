pragma Ada_2012;

with Google.Protobuf.IO.Invalid_Protocol_Buffer_Exception;

package body Google.Protobuf.Wire_Format is

   --------------
   -- Make_Tag --
   --------------

   function Make_Tag
     (Field_Number : in PB_Field_Type;
      Wire_Type    : in PB_Wire_Type)
     return PB_UInt32
   is
   begin
      return PB_UInt32 (Shift_Left (Field_Number, TAG_TYPE_BITS)) or
        PB_UInt32 (PB_Wire_Type'Pos (Wire_Type));
   end Make_Tag;

   -----------------------
   -- Get_Tag_Wire_Type --
   -----------------------

   function Get_Tag_Wire_Type
     (Tag : in PB_UInt32)
     return PB_Wire_Type
   is
   begin
      declare
         Result : constant PB_Wire_Type := PB_Wire_Type'Val (Tag and TAG_TYPE_MASK);
      begin
         return Result;
      end;
   exception
      when Constraint_Error =>
         Google.Protobuf.IO.Invalid_Protocol_Buffer_Exception.Invalid_Wire_Type;
         return VARINT;
   end Get_Tag_Wire_Type;

   --------------------------
   -- Get_Tag_Field_Number --
   --------------------------

   function Get_Tag_Field_Number
     (Tag : in PB_UInt32)
     return PB_Field_Type
   is
   begin
      return PB_Field_Type (Shift_Right (Tag, TAG_TYPE_BITS));
   end Get_Tag_Field_Number;

end Google.Protobuf.Wire_Format;
