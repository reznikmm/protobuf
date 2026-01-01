--  SPDX-FileCopyrightText: 2020-2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--

with Ada.Finalization;

generic
   type Element_Type is private;

package Proto_Support.Vectors is
   pragma Preelaborate;

   type Vector is tagged private;
   pragma Preelaborable_Initialization (Vector);

   function Length (Self : Vector) return Natural
     with Inline;

   function Get (Self : Vector; Index : Positive) return Element_Type
     with Inline;

   procedure Clear (Self : in out Vector)
     with Inline;

   procedure Append (Self : in out Vector; Value : Element_Type);

   Empty_Vector : constant Vector;

private
   type Element_Array is array (Positive range <>) of Element_Type;
   type Element_Array_Access is access Element_Array;

   type Vector is new Ada.Finalization.Controlled with record
      Data   : Element_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out Vector);
   overriding procedure Finalize (Self : in out Vector);

   Empty_Vector : constant Vector :=
     (Ada.Finalization.Controlled with null, 0);

end Proto_Support.Vectors;
