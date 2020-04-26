with Ada.Finalization;

generic
   type Element_Type is private;

package PB_Support.Vectors is
   type Vector is tagged private;

   function Length (Self : Vector) return Natural
     with Inline;

   function Get (Self : Vector; Index : Positive) return Element_Type
     with Inline;

   procedure Clear (Self : in out Vector)
     with Inline;

   procedure Append (Self : in out Vector; Value : Element_Type);

private
   type Element_Array is array (Positive range <>) of Element_Type;
   type Element_Array_Access is access Element_Array;

   type Vector is new Ada.Finalization.Controlled with record
      Data   : Element_Array_Access;
      Length : Natural;
   end record;

   overriding procedure Adjust (Self : in out Vector);
   overriding procedure Finalize (Self : in out Vector);

end PB_Support.Vectors;
