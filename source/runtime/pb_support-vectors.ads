--  MIT License
--
--  Copyright (c) 2020 Max Reznik
--
--  Permission is hereby granted, free of charge, to any person obtaining a
--  copy of this software and associated documentation files (the "Software"),
--  to deal in the Software without restriction, including without limitation
--  the rights to use, copy, modify, merge, publish, distribute, sublicense,
--  and/or sell copies of the Software, and to permit persons to whom the
--  Software is furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
--  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
--  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
--  DEALINGS IN THE SOFTWARE.

with Ada.Finalization;

generic
   type Element_Type is private;

package PB_Support.Vectors is
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

   type Option (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : Element_Type;
         when False =>
            null;
      end case;
   end record;

private
   type Element_Array is array (Positive range <>) of Element_Type;
   type Element_Array_Access is access Element_Array;

   type Vector is new Ada.Finalization.Controlled with record
      Data   : Element_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out Vector);
   overriding procedure Finalize (Self : in out Vector);

end PB_Support.Vectors;
