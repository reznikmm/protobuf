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
with Interfaces.C_Streams;

package PB_Support.Stdio_Streams is

   type Stdio_Stream is new Ada.Streams.Root_Stream_Type with private;

   procedure Initialize
     (Self   : in out Stdio_Stream'Class;
      stdin  : Interfaces.C_Streams.FILEs := Interfaces.C_Streams.stdin;
      stdout : Interfaces.C_Streams.FILEs := Interfaces.C_Streams.stdout);

   procedure Flush (Self : in out Stdio_Stream'Class);

private
   type Stdio_Stream is new Ada.Streams.Root_Stream_Type with record
      stdin  : Interfaces.C_Streams.FILEs := Interfaces.C_Streams.stdin;
      stdout : Interfaces.C_Streams.FILEs := Interfaces.C_Streams.stdout;
   end record;

   overriding procedure Read
     (Self : in out Stdio_Stream;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (Self : in out Stdio_Stream;
      Item : Ada.Streams.Stream_Element_Array);

end PB_Support.Stdio_Streams;
