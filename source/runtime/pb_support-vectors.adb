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

with Ada.Unchecked_Deallocation;

package body PB_Support.Vectors is

   procedure Free is new Ada.Unchecked_Deallocation
     (Element_Array, Element_Array_Access);

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Element_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   ------------
   -- Append --
   ------------

   procedure Append (Self : in out Vector; Value : Element_Type) is
      Old : Element_Array_Access := Self.Data;
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Natural'Max (1, Element_Type'Size));
   begin
      if Self.Length = 0 then
         Self.Data := new Element_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data := new Element_Array'
           (Old.all & (1 .. Self.Length => <>));
         Free (Old);
      end if;

      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Vector) is
   begin
      Self.Length := 0;
   end Clear;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   ---------
   -- Get --
   ---------

   function Get (Self : Vector; Index : Positive) return Element_Type is
   begin
      return Self.Data (Index);
   end Get;

   ------------
   -- Length --
   ------------

   function Length (Self : Vector) return Natural is
   begin
      return Self.Length;
   end Length;

end PB_Support.Vectors;
