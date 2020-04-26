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

with PB_Support.Vectors;
with PB_Support.Boolean_Vectors;
with PB_Support.Unsigned_32_Vectors;

package PB_Support.IO is

   type Field_Number is range 1 .. 2 ** 29 - 1;

   type Wire_Type is
     (Var_Int,
      Fixed_64,
      Length_Delimited,
      Start_Group,
      End_Group,
      Fixed_32);

   type Key is record
      Field    : Field_Number;
      Encoding : Wire_Type;
   end record;

   function Read_Key
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Result : access Key) return Boolean;

   function Read_Length
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Ada.Streams.Stream_Element_Count;

   procedure Read_Universal_String
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out League.Strings.Universal_String);

   procedure Read_Universal_String_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out League.String_Vectors.Universal_String_Vector);

   procedure Read_Stream_Element_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out League.Stream_Element_Vectors.Stream_Element_Vector);

   procedure Read_Boolean
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Boolean);

   procedure Read_Boolean_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out PB_Support.Boolean_Vectors.Vector);

   procedure Read_IEEE_Float_64
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Interfaces.IEEE_Float_64);

   procedure Read_IEEE_Float_32
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Interfaces.IEEE_Float_32);

   procedure Read_Integer_64
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Interfaces.Integer_64);

   procedure Read_Unsigned_64
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Interfaces.Unsigned_64);

   procedure Read_Integer_32
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Interfaces.Integer_32);

   procedure Read_Unsigned_32
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Interfaces.Unsigned_32);

   procedure Read_Unsigned_32_Vector
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
