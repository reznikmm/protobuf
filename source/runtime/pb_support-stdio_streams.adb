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

package body PB_Support.Stdio_Streams is

   -----------
   -- Flush --
   -----------

   procedure Flush (Self : in out Stdio_Stream'Class) is
      Ignore : Interfaces.C_Streams.int;
   begin
      Ignore := Interfaces.C_Streams.fflush (Self.stdout);
   end Flush;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : in out Stdio_Stream'Class;
      stdin  : Interfaces.C_Streams.FILEs := Interfaces.C_Streams.stdin;
      stdout : Interfaces.C_Streams.FILEs := Interfaces.C_Streams.stdout) is
   begin
      Self.stdin := stdin;
      Self.stdout := stdout;
   end Initialize;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self : in out Stdio_Stream;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Count;

      Size : constant Interfaces.C_Streams.size_t :=
        Interfaces.C_Streams.fread
          (buffer => Item'Address,
           size   => 1,
           count  => Item'Length,
           stream => Self.stdin);
   begin
      Last := Item'First + Ada.Streams.Stream_Element_Count (Size) - 1;
   end Read;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : in out Stdio_Stream;
      Item : Ada.Streams.Stream_Element_Array)
   is
      use type Interfaces.C_Streams.size_t;

      Size : constant Interfaces.C_Streams.size_t :=
        Interfaces.C_Streams.fwrite
          (buffer => Item'Address,
           size   => 1,
           count  => Item'Length,
           stream => Self.stdout);
   begin
      pragma Assert (Size = Item'Length);
   end Write;

end PB_Support.Stdio_Streams;
