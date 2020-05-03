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

with Ada.Unchecked_Conversion;
with System.Storage_Elements;

with League.Text_Codecs;

package body PB_Support.Internal is

   use type Ada.Streams.Stream_Element_Count;

   procedure Write
     (Self  : in out Stream;
      Value : Boolean)
        with Inline;

   procedure Write
     (Self  : in out Stream;
      Value : Interfaces.IEEE_Float_32)
        with Inline;

   procedure Write
     (Self  : in out Stream;
      Value : Interfaces.IEEE_Float_64)
        with Inline;

   procedure Write
     (Self  : in out Stream;
      Value : League.Strings.Universal_String)
        with Inline;

   procedure Write
     (Self  : in out Stream;
      Value : League.Stream_Element_Vectors.Stream_Element_Vector)
        with Inline;

   procedure Write_Varint
     (Self  : in out Stream;
      Value : Interfaces.Unsigned_32)
        with Inline;

   procedure Write_Varint
     (Self  : in out Stream;
      Value : Interfaces.Unsigned_64)
        with Inline;

   procedure Write_Varint
     (Self  : in out Stream;
      Value : Interfaces.Integer_64)
        with Inline;

   procedure Write
     (Self  : in out Stream;
      Value : Ada.Streams.Stream_Element_Count)
        with Inline;

   -------------
   -- Written --
   -------------

   not overriding function Written
     (Self : Stream) return Ada.Streams.Stream_Element_Count is
   begin
      return Self.Written;
   end Written;

   -------------------
   -- Start_Message --
   -------------------

   not overriding procedure Start_Message
     (Self    : in out Stream;
      Message : System.Address)
   is
   begin
      Self.Level := Self.Level + 1;

      if Self.Level = 2 then
         Self.Riffling := not Self.Riffling;
      elsif not Self.Riffling then
         Self.Write (Self.Size ((Message, Self.Level)));
      end if;
   end Start_Message;

   -----------------
   -- End_Message --
   -----------------

   not overriding function End_Message
     (Self    : in out Stream;
      Message : System.Address;
      Offset  : Ada.Streams.Stream_Element_Count) return Boolean is
   begin
      if Self.Riffling and Self.Level > 1 then
         Self.Size.Insert ((Message, Self.Level), Self.Written - Offset);
      end if;

      Self.Level := Self.Level - 1;

      return Self.Level = 1 and Self.Riffling;
   end End_Message;

   -----------
   -- Write --
   -----------

   not overriding procedure Write
     (Self  : in out Stream;
      Field : Field_Number;
      Value : Boolean) is
   begin
      Self.Write_Key ((Field, Var_Int));
      Self.Write (Value);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write (Self : in out Stream; Value : Boolean) is
   begin
      if Self.Riffling then
         Self.Written := Self.Written + 1;
      elsif Value then
         Self.Parent.Write ((1 => 1));
      else
         Self.Parent.Write ((1 => 0));
      end if;
   end Write;

   -----------
   -- Write --
   -----------

   not overriding procedure Write
     (Self  : in out Stream;
      Field : Field_Number;
      Value : PB_Support.Boolean_Vectors.Vector) is
   begin
      for J in 1 .. Value.Length loop
         Self.Write (Field, Value.Get (J));
      end loop;
   end Write;

   -----------
   -- Write --
   -----------

   not overriding procedure Write
     (Self  : in out Stream;
      Field : Field_Number;
      Value : Interfaces.IEEE_Float_32)
   is
   begin
      Self.Write_Key ((Field, Fixed_32));
      Self.Write (Value);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (Self  : in out Stream;
      Value : Interfaces.IEEE_Float_32) is
   begin
      if Self.Riffling then
         Self.Written := Self.Written + 4;
      else
         Interfaces.IEEE_Float_32'Write (Self.Parent, Value);
      end if;
   end Write;

   -----------
   -- Write --
   -----------

   not overriding procedure Write
     (Self  : in out Stream;
      Field : Field_Number;
      Value : Interfaces.IEEE_Float_64)
   is
   begin
      Self.Write_Key ((Field, Fixed_64));
      Self.Write (Value);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (Self  : in out Stream;
      Value : Interfaces.IEEE_Float_64) is
   begin
      if Self.Riffling then
         Self.Written := Self.Written + 8;
      else
         Interfaces.IEEE_Float_64'Write (Self.Parent, Value);
      end if;
   end Write;

   -----------
   -- Write --
   -----------

   not overriding procedure Write
     (Self  : in out Stream;
      Field : Field_Number;
      Value : League.Strings.Universal_String) is
   begin
      Self.Write_Key ((Field, Length_Delimited));
      Self.Write (Value);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (Self  : in out Stream;
      Value : League.Strings.Universal_String)
   is
      Codec : constant League.Text_Codecs.Text_Codec :=
        League.Text_Codecs.Codec
          (League.Strings.To_Universal_String ("utf-8"));
      Data  : constant League.Stream_Element_Vectors.Stream_Element_Vector :=
        Codec.Encode (Value);
   begin
      Self.Write (Data);
   end Write;

   -----------
   -- Write --
   -----------

   not overriding procedure Write
     (Self  : in out Stream;
      Field : Field_Number;
      Value : League.String_Vectors.Universal_String_Vector) is
   begin
      for J in 1 .. Value.Length loop
         Self.Write (Field, Value.Element (J));
      end loop;
   end Write;

   -----------
   -- Write --
   -----------

   not overriding procedure Write
     (Self  : in out Stream;
      Field : Field_Number;
      Value : League.Stream_Element_Vectors.Stream_Element_Vector) is
   begin
      Self.Write_Key ((Field, Length_Delimited));
      Self.Write (Value);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (Self  : in out Stream;
      Value : League.Stream_Element_Vectors.Stream_Element_Vector)
   is
   begin
      Self.Write (Value.Length);

      if Self.Riffling then
         Self.Written := Self.Written + Value.Length;
      else
         Self.Parent.Write (Value.To_Stream_Element_Array);
      end if;
   end Write;

   ------------------
   -- Write_Varint --
   ------------------

   not overriding procedure Write_Varint
     (Self  : in out Stream;
      Field : Field_Number;
      Value : Interfaces.Unsigned_32) is
   begin
      Self.Write_Key ((Field, Var_Int));
      Self.Write_Varint (Value);
   end Write_Varint;

   ------------------
   -- Write_Varint --
   ------------------

   procedure Write_Varint
     (Self  : in out Stream;
      Value : Interfaces.Unsigned_32)
   is
      use type Interfaces.Unsigned_32;

      Left : Interfaces.Unsigned_32 := Value;
      Data : Ada.Streams.Stream_Element_Array (1 .. 32 / 7 + 1);
      Last : Ada.Streams.Stream_Element_Count := 0;
   begin
      while Left >= 16#80# loop
         Last := Last + 1;
         Data (Last) := Ada.Streams.Stream_Element
           ((Left and 16#7F#) + 16#80#);
         Left := Interfaces.Shift_Right (Left, 7);
      end loop;

      Last := Last + 1;

      if Self.Riffling then
         Self.Written := Self.Written + Last;
      else
         Data (Last) := Ada.Streams.Stream_Element'Mod (Left);
         Self.Parent.Write (Data (1 .. Last));
      end if;
   end Write_Varint;

   ------------------
   -- Write_Varint --
   ------------------

   not overriding procedure Write_Varint
     (Self  : in out Stream;
      Field : Field_Number;
      Value : PB_Support.Unsigned_32_Vectors.Vector) is
   begin
      for J in 1 .. Value.Length loop
         Self.Write_Varint (Field, Value.Get (J));
      end loop;
   end Write_Varint;

   ------------------
   -- Write_Varint --
   ------------------

   not overriding procedure Write_Varint
     (Self  : in out Stream;
      Field : Field_Number;
      Value : Interfaces.Unsigned_64) is
   begin
      Self.Write_Key ((Field, Var_Int));
      Self.Write_Varint (Value);
   end Write_Varint;

   ------------------
   -- Write_Varint --
   ------------------

   procedure Write_Varint
     (Self  : in out Stream;
      Value : Interfaces.Unsigned_64)
   is
      use type Interfaces.Unsigned_64;

      Left : Interfaces.Unsigned_64 := Value;
      Data : Ada.Streams.Stream_Element_Array (1 .. 64 / 7 + 1);
      Last : Ada.Streams.Stream_Element_Count := 0;
   begin
      while Left >= 16#80# loop
         Last := Last + 1;
         Data (Last) := Ada.Streams.Stream_Element
           ((Left and 16#7F#) + 16#80#);
         Left := Interfaces.Shift_Right (Left, 7);
      end loop;

      Last := Last + 1;

      if Self.Riffling then
         Self.Written := Self.Written + Last;
      else
         Data (Last) := Ada.Streams.Stream_Element'Mod (Left);
         Self.Parent.Write (Data (1 .. Last));
      end if;
   end Write_Varint;

   ------------------
   -- Write_Varint --
   ------------------

   not overriding procedure Write_Varint
     (Self  : in out Stream;
      Field : Field_Number;
      Value : Interfaces.Integer_32) is
   begin
      Self.Write_Key ((Field, Var_Int));
      Self.Write_Varint (Value);
   end Write_Varint;

   ------------------
   -- Write_Varint --
   ------------------

   not overriding procedure Write_Varint
     (Self  : in out Stream;
      Value : Interfaces.Integer_32)
   is
      function Cast is new Ada.Unchecked_Conversion
        (Interfaces.Integer_32, Interfaces.Unsigned_32);
   begin
      Self.Write_Varint (Cast (Value));
   end Write_Varint;

   ------------------
   -- Write_Varint --
   ------------------

   not overriding procedure Write_Varint
     (Self  : in out Stream;
      Field : Field_Number;
      Value : Interfaces.Integer_64) is
   begin
      Self.Write_Key ((Field, Var_Int));
      Self.Write_Varint (Value);
   end Write_Varint;

   ------------------
   -- Write_Varint --
   ------------------

   procedure Write_Varint
     (Self  : in out Stream;
      Value : Interfaces.Integer_64)
   is
      function Cast is new Ada.Unchecked_Conversion
        (Interfaces.Integer_64, Interfaces.Unsigned_64);
   begin
      Self.Write_Varint (Cast (Value));
   end Write_Varint;

   -----------
   -- Write --
   -----------

   procedure Write
     (Self  : in out Stream;
      Value : Ada.Streams.Stream_Element_Count) is
   begin
      Self.Write_Varint (Interfaces.Unsigned_32 (Value));
   end Write;

   ----------
   -- Hash --
   ----------

   function Hash (Value : Message_Id) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;

      Int : constant System.Storage_Elements.Integer_Address :=
        System.Storage_Elements.To_Integer (Value.Address);
   begin
      return Ada.Containers.Hash_Type'Mod (Int) +
        Ada.Containers.Hash_Type (Value.Level);
   end Hash;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out Internal.Stream;
      Item   : Ada.Streams.Stream_Element_Array)
   is
   begin
      raise Program_Error with "Unexpected call to Write procedure";
   end Write;

   ---------------
   -- Write_Key --
   ---------------

   not overriding procedure Write_Key
     (Self  : in out Stream;
      Value : Key)
   is
      use type Interfaces.Unsigned_32;

      Integer : constant Interfaces.Unsigned_32 :=
        Interfaces.Shift_Left (Interfaces.Unsigned_32 (Value.Field), 3) +
          Wire_Type'Pos (Value.Encoding);
   begin
      Self.Write_Varint (Integer);
   end Write_Key;

end PB_Support.Internal;
