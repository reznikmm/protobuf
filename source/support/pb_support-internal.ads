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

--  This is internal unit, don't use it in an application.

with Ada.Streams;
with Ada.Containers.Hashed_Maps;
with Interfaces;
with System;

with League.Stream_Element_Vectors;
with League.String_Vectors;
with League.Strings;

with PB_Support.Boolean_Vectors;
with PB_Support.Unsigned_32_Vectors;

package PB_Support.Internal is
   pragma Preelaborate;

   type Stream
     (Parent : not null access Ada.Streams.Root_Stream_Type'Class)
       is new Ada.Streams.Root_Stream_Type with private;
   --  This is internal type, don't use it in an application.
   --
   --  This stream works in two passes. During the first pass (riffling)
   --  the stream ignores all written data and just calculate total size
   --  of messages. During the second pass (writting) actuall data is written
   --  to Parent.

   pragma Preelaborable_Initialization (Stream);

   not overriding function Written
     (Self : Stream) return Ada.Streams.Stream_Element_Count
       with Inline;
   --  How much data has been written to the stream (in riffling pass).

   not overriding procedure Start_Message
     (Self    : in out Stream;
      Message : System.Address)
        with Inline;
   --  Increment nested depth. In writting pass emit message length.

   not overriding function End_Message
     (Self    : in out Stream;
      Message : System.Address;
      Offset  : Ada.Streams.Stream_Element_Count) return Boolean
        with Inline;
   --  Decrement nested depth. In riffling pass calculate and remember
   --  message size. It returns True after completion of riffling pass.

   not overriding procedure Write
     (Self  : in out Stream;
      Field : Field_Number;
      Value : Boolean)
        with Inline;

   not overriding procedure Write
     (Self  : in out Stream;
      Field : Field_Number;
      Value : PB_Support.Boolean_Vectors.Vector);

   not overriding procedure Write
     (Self  : in out Stream;
      Field : Field_Number;
      Value : Interfaces.IEEE_Float_32)
        with Inline;

   not overriding procedure Write
     (Self  : in out Stream;
      Field : Field_Number;
      Value : Interfaces.IEEE_Float_64)
        with Inline;

   not overriding procedure Write
     (Self  : in out Stream;
      Field : Field_Number;
      Value : League.Strings.Universal_String)
        with Inline;

   not overriding procedure Write
     (Self  : in out Stream;
      Field : Field_Number;
      Value : League.String_Vectors.Universal_String_Vector)
        with Inline;

   not overriding procedure Write
     (Self  : in out Stream;
      Field : Field_Number;
      Value : League.Stream_Element_Vectors.Stream_Element_Vector)
        with Inline;

   not overriding procedure Write_Varint
     (Self  : in out Stream;
      Field : Field_Number;
      Value : Interfaces.Unsigned_32)
        with Inline;

   not overriding procedure Write_Varint
     (Self  : in out Stream;
      Field : Field_Number;
      Value : PB_Support.Unsigned_32_Vectors.Vector);

   not overriding procedure Write_Varint
     (Self  : in out Stream;
      Field : Field_Number;
      Value : Interfaces.Unsigned_64)
        with Inline;

   not overriding procedure Write_Varint
     (Self  : in out Stream;
      Field : Field_Number;
      Value : Interfaces.Integer_32)
        with Inline;

   not overriding procedure Write_Varint
     (Self  : in out Stream;
      Value : Interfaces.Integer_32)
        with Inline;

   not overriding procedure Write_Varint
     (Self  : in out Stream;
      Field : Field_Number;
      Value : Interfaces.Integer_64)
        with Inline;

   not overriding procedure Write_Key
     (Self  : in out Stream;
      Value : Key)
        with Inline;

private

   type Message_Id is record
      Address : System.Address;
      Level   : Natural;
   end record;

   function Hash (Value : Message_Id) return Ada.Containers.Hash_Type
     with Inline;

   package Size_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Message_Id,
      Element_Type    => Ada.Streams.Stream_Element_Count,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => Ada.Streams."=");

   type Stream
     (Parent   : not null access Ada.Streams.Root_Stream_Type'Class)
   is new Ada.Streams.Root_Stream_Type with record
      Level    : Natural := 0;  --  Current depth of writting message
      Size     : Size_Maps.Map;  --  Memorized message sizes
      Riffling : Boolean := False;
      --  In riffling mode the stream ignores all written data and just
      --  calculate total size of messages
      Written  : Ada.Streams.Stream_Element_Count := 0;
      --  Total amount of written data during riffling mode
   end record;

   overriding procedure Read
     (Stream : in out Internal.Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset) is null;

   overriding procedure Write
     (Stream : in out Internal.Stream;
      Item   : Ada.Streams.Stream_Element_Array);

end PB_Support.Internal;
