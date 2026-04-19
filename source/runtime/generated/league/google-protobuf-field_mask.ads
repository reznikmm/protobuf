with Ada.Finalization;
with Ada.Streams;
with League.String_Vectors;

package Google.Protobuf.Field_Mask is

   type Field_Mask_Vector is tagged private
     with Variable_Indexing => Get_Field_Mask_Variable_Reference,
     Constant_Indexing => Get_Field_Mask_Constant_Reference;

   type Field_Mask is
     record
        Paths : League.String_Vectors.Universal_String_Vector;
     end record;

   type Optional_Field_Mask  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Field_Mask.Field_Mask;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Field_Mask_Vector) return Natural;

   procedure Clear (Self : in out Field_Mask_Vector);

   procedure Append (Self : in out Field_Mask_Vector; V    : Field_Mask);

   type Field_Mask_Variable_Reference
     (Element : not null access Field_Mask) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Field_Mask_Variable_Reference
    (Self  : aliased in out Field_Mask_Vector;
     Index : Positive)
      return Field_Mask_Variable_Reference
     with Inline;

   type Field_Mask_Constant_Reference
     (Element : not null access constant Field_Mask) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Field_Mask_Constant_Reference
    (Self  : aliased Field_Mask_Vector;
     Index : Positive)
      return Field_Mask_Constant_Reference
     with Inline;
private

   procedure Read_Field_Mask
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Field_Mask);

   procedure Write_Field_Mask
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Field_Mask);

   for Field_Mask'Read use Read_Field_Mask;

   for Field_Mask'Write use Write_Field_Mask;

   type Field_Mask_Array is array (Positive range <>) of aliased Field_Mask;

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