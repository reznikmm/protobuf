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

with Ada.Streams;
with League.Stream_Element_Vectors;

package PB_Support.Memory_Streams is

   type Memory_Stream is new Ada.Streams.Root_Stream_Type with private;

   procedure Clear (Self : in out Memory_Stream'Class);

   function Written
     (Self : Memory_Stream'Class) return Ada.Streams.Stream_Element_Count;

   function Data (Self : Memory_Stream'Class)
     return League.Stream_Element_Vectors.Stream_Element_Vector;

private
   type Memory_Stream is new Ada.Streams.Root_Stream_Type with record
      Data : League.Stream_Element_Vectors.Stream_Element_Vector;
      Read : Ada.Streams.Stream_Element_Count := 0;
   end record;

   overriding procedure Read
     (Self : in out Memory_Stream;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (Self : in out Memory_Stream;
      Item : Ada.Streams.Stream_Element_Array);

end PB_Support.Memory_Streams;
