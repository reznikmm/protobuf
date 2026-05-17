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

package body PB_Support.Memory_Streams is

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Memory_Stream'Class) is
   begin
      Self.Data.Clear;
      Self.Read := 0;
   end Clear;

   ----------
   -- Data --
   ----------

   function Data (Self : Memory_Stream'Class)
     return League.Stream_Element_Vectors.Stream_Element_Vector is
   begin
      return Self.Data;
   end Data;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self : in out Memory_Stream;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Count : constant Ada.Streams.Stream_Element_Offset :=
        Ada.Streams.Stream_Element_Offset'Min
          (Item'Length, Self.Data.Length - Self.Read);
   begin
      for J in 1 .. Count loop
         Item (Item'First + J - 1) := Self.Data.Element (Self.Read + J);
      end loop;

      Last := Item'First + Count - 1;
      Self.Read := Self.Read + Count;
   end Read;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : in out Memory_Stream;
      Item : Ada.Streams.Stream_Element_Array) is
   begin
      Self.Data.Append (Item);
   end Write;

   -------------
   -- Written --
   -------------

   function Written
     (Self : Memory_Stream'Class) return Ada.Streams.Stream_Element_Count is
   begin
      return Self.Data.Length;
   end Written;

end PB_Support.Memory_Streams;
