with Ada.Finalization;
with Ada.Streams;
with League.String_Vectors;

package Google.Protobuf.Field_Mask is

   type Field_Mask_Vector is tagged private;

   type Field_Mask is
     record
        Paths : League.String_Vectors.Universal_String_Vector;
     end record;

   type Optional_Field_Mask (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Field_Mask.Field_Mask;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Field_Mask_Vector) return Natural;

   function Get
    (Self  : Field_Mask_Vector;
     Index : Positive)
      return Field_Mask;

   procedure Clear (Self : in out Field_Mask_Vector);

   procedure Append (Self : in out Field_Mask_Vector; V    : Field_Mask);
private

   procedure Read_Field_Mask
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Field_Mask);

   procedure Write_Field_Mask
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Field_Mask);

   for Field_Mask'Read use Read_Field_Mask;

   for Field_Mask'Write use Write_Field_Mask;

   type Field_Mask_Array is array (Positive range <>) of Field_Mask;

   type Field_Mask_Array_Access is access Field_Mask_Array;

   type Field_Mask_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Field_Mask_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Field_Mask_Vector);

   overriding procedure Finalize (Self : in out Field_Mask_Vector);

end Google.Protobuf.Field_Mask;