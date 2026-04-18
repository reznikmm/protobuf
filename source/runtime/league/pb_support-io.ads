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
with Interfaces;

with League.Strings;
with League.Stream_Element_Vectors;
with League.String_Vectors;

with PB_Support.Boolean_Vectors;
with PB_Support.IEEE_Float_32_Vectors;
with PB_Support.IEEE_Float_64_Vectors;
with PB_Support.Integer_32_Vectors;
with PB_Support.Integer_64_Vectors;
with PB_Support.Internal;
with PB_Support.Stream_Element_Vector_Vectors;
with PB_Support.Unsigned_32_Vectors;
with PB_Support.Unsigned_64_Vectors;
with PB_Support.Vectors;

package PB_Support.IO is
   pragma Preelaborate;

   subtype Key is PB_Support.Key;

   function Read_Key
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Result : access Key) return Boolean;

   function Read_Length
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Ada.Streams.Stream_Element_Count;

   function Read_Array_Length
     (Stream    : not null access Ada.Streams.Root_Stream_Type'Class;
      Item_Size : Ada.Streams.Stream_Element_Count)
      return Natural;

   procedure Read
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out League.Strings.Universal_String);

   procedure Read_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out League.String_Vectors.Universal_String_Vector);

   procedure Read
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out League.Stream_Element_Vectors.Stream_Element_Vector);

   procedure Read_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out PB_Support.Stream_Element_Vector_Vectors.Vector);

   procedure Read
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Boolean);

   procedure Read_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out PB_Support.Boolean_Vectors.Vector);

   procedure Read
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Interfaces.IEEE_Float_64);

   procedure Read_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out PB_Support.IEEE_Float_64_Vectors.Vector);

   procedure Read
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Interfaces.IEEE_Float_32);

   procedure Read_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out PB_Support.IEEE_Float_32_Vectors.Vector);

   procedure Read_Varint
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Interfaces.Integer_64);

   procedure Read_Varint_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out PB_Support.Integer_64_Vectors.Vector);

   procedure Read_Varint
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Interfaces.Unsigned_64);

   procedure Read_Varint_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out PB_Support.Unsigned_64_Vectors.Vector);

   procedure Read_Varint
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Interfaces.Integer_32);

   procedure Read_Varint_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out PB_Support.Integer_32_Vectors.Vector);

   procedure Read_Varint
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Interfaces.Unsigned_32);

   procedure Read_Varint_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out PB_Support.Unsigned_32_Vectors.Vector);

   procedure Read_Zigzag
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Interfaces.Integer_32);

   procedure Read_Zigzag_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out PB_Support.Integer_32_Vectors.Vector);

   procedure Read_Zigzag
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Interfaces.Integer_64);

   procedure Read_Zigzag_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out PB_Support.Integer_64_Vectors.Vector);

   procedure Read_Fixed
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Interfaces.Integer_64) renames Read_Varint;

   procedure Read_Fixed_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out PB_Support.Integer_64_Vectors.Vector);

   procedure Read_Fixed
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Interfaces.Unsigned_64) renames Read_Varint;

   procedure Read_Fixed_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out PB_Support.Unsigned_64_Vectors.Vector);

   procedure Read_Fixed
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Interfaces.Integer_32) renames Read_Varint;

   procedure Read_Fixed_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out PB_Support.Integer_32_Vectors.Vector);

   procedure Read_Fixed
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Interfaces.Unsigned_32) renames Read_Varint;

   procedure Read_Fixed_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out PB_Support.Unsigned_32_Vectors.Vector);

   procedure Unknown_Field
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type);

   generic
      type Element is (<>);
      type Integer_Element is range <>;
      with package Vectors is new PB_Support.Vectors (Element);
   package Enum_IO is
      procedure Read
        (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
         Encoding : Wire_Type;
         Value    : out Element);

      procedure Read_Vector
        (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
         Encoding : Wire_Type;
         Value    : in out Vectors.Vector);

      procedure Write
        (Stream : in out Internal.Stream;
         Field  : Field_Number;
         Value  : Element);

      procedure Write
        (Stream : in out Internal.Stream;
         Field  : Field_Number;
         Value  : Vectors.Vector);

      procedure Write_Packed
        (Stream : in out Internal.Stream;
         Field  : Field_Number;
         Value  : Vectors.Vector);

      procedure Write_Option
        (Stream  : in out Internal.Stream;
         Field   : Field_Number;
         Value   : Element;
         Default : Element);

   end Enum_IO;

   generic
      type Element is private;
      type Vector is private;
      with procedure Append
        (Self  : in out Vector;
         Value : Element);
   package Message_IO is
      procedure Read
        (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
         Encoding : Wire_Type;
         Value    : out Element);

      procedure Read_Vector
        (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
         Encoding : Wire_Type;
         Value    : in out Vector);
   end Message_IO;

end PB_Support.IO;
