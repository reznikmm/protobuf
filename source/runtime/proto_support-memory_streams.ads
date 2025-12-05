--  SPDX-FileCopyrightText: 2020-2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--

with Ada.Streams;
with League.Stream_Element_Vectors;

package Proto_Support.Memory_Streams is

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

end Proto_Support.Memory_Streams;
