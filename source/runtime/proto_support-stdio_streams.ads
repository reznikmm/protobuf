--  SPDX-FileCopyrightText: 2020-2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--

with Ada.Streams;
with Interfaces.C_Streams;

package Proto_Support.Stdio_Streams is

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

end Proto_Support.Stdio_Streams;
