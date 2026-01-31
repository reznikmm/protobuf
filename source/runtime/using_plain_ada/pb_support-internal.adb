--  MIT License
--
--  Copyright (c) 2020-2023 Max Reznik
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
      Value : Ada.Strings.Unbounded.Unbounded_String)
        with Inline;

   procedure Write
     (Self  : in out Stream;
      Value : PB_Support.Basics.Stream_Element_Vectors.Vector)
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

   procedure Write_Zigzag
     (Self  : in out Stream;
      Value : Interfaces.Integer_32)
        with Inline;

   procedure Write_Zigzag
     (Self  : in out Stream;
      Value : Interfaces.Integer_64)
        with Inline;

   procedure Write_Fixed
     (Self  : in out Stream;
      Value : Interfaces.Integer_32)
        with Inline;

   procedure Write_Fixed
     (Self  : in out Stream;
      Value : Interfaces.Integer_64)
        with Inline;

   procedure Write_Fixed
     (Self  : in out Stream;
      Value : Interfaces.Unsigned_32)
        with Inline;

   procedure Write_Fixed
     (Self  : in out Stream;
      Value : Interfaces.Unsigned_64)
        with Inline;

   ----------
   -- Size --
   ----------

   function Size (Value : Interfaces.Unsigned_32)
     return Ada.Streams.Stream_Element_Count
   is
      use type Interfaces.Unsigned_32;

      Left : Interfaces.Unsigned_32 := Value;
      Last : Ada.Streams.Stream_Element_Count := 0;
   begin
      while Left >= 16#80# loop
         Last := Last + 1;
         Left := Interfaces.Shift_Right (Left, 7);
      end loop;

      Last := Last + 1;
      return Last;
   end Size;

   function Size (Value : Interfaces.Unsigned_64)
     return Ada.Streams.Stream_Element_Count
   is
      use type Interfaces.Unsigned_64;

      Left : Interfaces.Unsigned_64 := Value;
      Last : Ada.Streams.Stream_Element_Count := 0;
   begin
      while Left >= 16#80# loop
         Last := Last + 1;
         Left := Interfaces.Shift_Right (Left, 7);
      end loop;

      Last := Last + 1;
      return Last;
   end Size;

   function Size (Value : Interfaces.Integer_32)
     return Ada.Streams.Stream_Element_Count
   is
      use type Interfaces.Integer_32;
   begin
      if Value < 0 then
         return 10;
      else
         return Size (Interfaces.Unsigned_32 (Value));
      end if;
   end Size;

   function Size (Value : Interfaces.Integer_64)
     return Ada.Streams.Stream_Element_Count
   is
      use type Interfaces.Integer_64;
   begin
      if Value < 0 then
         return 10;
      else
         return Size (Interfaces.Unsigned_64 (Value));
      end if;
   end Size;

   -------------------
   -- Start_Message --
   -------------------

   not overriding procedure Start_Message
     (Self : in out Stream)
   is
   begin
      Self.Level := Self.Level + 1;

      if Self.Level = 2 then
         Self.Riffling := not Self.Riffling;

         if Self.Riffling then
            Self.Size.Clear;
            Self.Size.Set_Length (0);
            Self.Index := 1;
         end if;
      end if;

      if Self.Riffling then
         Self.Size.Append (Self.Written);
         Self.Stack.Append (Self.Size.Last_Index);
      elsif Self.Level > 1 then
         Self.Write (Self.Size (Self.Index));
         Self.Index := Self.Index + 1;
      end if;
   end Start_Message;

   -----------------
   -- End_Message --
   -----------------

   not overriding function End_Message
     (Self : in out Stream) return Boolean
   is
      Id : Message_Id;
   begin
      if Self.Riffling then
         Id := Self.Stack.Last_Element;
         Self.Stack.Delete_Last;
         Self.Size (Id) := Self.Written - Self.Size (Id);
         Self.Write (Self.Size (Id));
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
      Value : PB_Support.IEEE_Float_32_Vectors.Vector) is
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
      Value : PB_Support.IEEE_Float_64_Vectors.Vector) is
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
      Value : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Self.Write_Key ((Field, Length_Delimited));
      Self.Write (Value);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (Self  : in out Stream;
      Value : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      declare
         Data : PB_Support.Basics.Stream_Element_Vectors.Vector;
         use type Ada.Containers.Count_Type;
      begin
         Data.Set_Length
            (Ada.Containers.Count_Type (Ada.Strings.Unbounded.Length (Value)));
         if Data.Length > 0 then
            for I in 1 .. Field_Number (Data.Length) loop
               --  This loop is probably performance-challenged because
               --  of the call to Replace_Element for each iteration...
               Data.Replace_Element
                  (I, Character'Pos (Ada.Strings.Unbounded.Element (Value, Integer (I))));
            end loop;
         end if;
         Self.Write (Data);
      end;
   end Write;

   -----------
   -- Write --
   -----------

   not overriding procedure Write
     (Self  : in out Stream;
      Field : Field_Number;
      Value : PB_Support.Unbounded_String_Vectors.Vector) is
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
      Value : PB_Support.Stream_Element_Vector_Vectors.Vector) is
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
      Value : PB_Support.Basics.Stream_Element_Vectors.Vector) is
   begin
      Self.Write_Key ((Field, Length_Delimited));
      Self.Write (Value);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (Self  : in out Stream;
      Value : PB_Support.Basics.Stream_Element_Vectors.Vector)
   is
   begin
      Self.Write (Ada.Streams.Stream_Element_Count (Value.Length));

      if Self.Riffling then
         Self.Written := Self.Written + Ada.Streams.Stream_Element_Count (Value.Length);
      else
         declare
            A : Ada.Streams.Stream_Element_Array
               (1 .. Ada.Streams.Stream_Element_Count (Value.Length));
         begin
            for I in A'Range loop
               A (I) := Value (Field_Number (I));
            end loop;
            Self.Parent.Write (A);
         end;
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
      Value : Interfaces.Integer_32) is
   begin
      --  "If you use int32 or int64 as the type for a negative number, the
      --  resulting varint is always ten bytes long"
      Self.Write_Varint (Interfaces.Integer_64 (Value));
   end Write_Varint;

   ------------------
   -- Write_Varint --
   ------------------

   not overriding procedure Write_Varint
     (Self  : in out Stream;
      Field : Field_Number;
      Value : PB_Support.Integer_32_Vectors.Vector) is
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
      Value : PB_Support.Integer_64_Vectors.Vector) is
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
      Value : PB_Support.Unsigned_64_Vectors.Vector) is
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

   not overriding procedure Write_Varint_Option
     (Self    : in out Stream;
      Field   : Field_Number;
      Value   : Interfaces.Unsigned_32;
      Default : Interfaces.Unsigned_32)
   is
      use type Interfaces.Unsigned_32;
   begin
      if Value /= Default then
         Self.Write_Varint (Field, Value);
      end if;
   end Write_Varint_Option;

   not overriding procedure Write_Varint_Option
     (Self    : in out Stream;
      Field   : Field_Number;
      Value   : Interfaces.Unsigned_64;
      Default : Interfaces.Unsigned_64)
   is
      use type Interfaces.Unsigned_64;
   begin
      if Value /= Default then
         Self.Write_Varint (Field, Value);
      end if;
   end Write_Varint_Option;

   not overriding procedure Write_Varint_Option
     (Self    : in out Stream;
      Field   : Field_Number;
      Value   : Interfaces.Integer_32;
      Default : Interfaces.Integer_32)
   is
      use type Interfaces.Integer_32;
   begin
      if Value /= Default then
         Self.Write_Varint (Field, Value);
      end if;
   end Write_Varint_Option;

   not overriding procedure Write_Varint_Option
     (Self    : in out Stream;
      Field   : Field_Number;
      Value   : Interfaces.Integer_64;
      Default : Interfaces.Integer_64)
   is
      use type Interfaces.Integer_64;
   begin
      if Value /= Default then
         Self.Write_Varint (Field, Value);
      end if;
   end Write_Varint_Option;

   not overriding procedure Write_Varint_Packed
     (Self  : in out Stream;
      Field : Field_Number;
      Value : PB_Support.Unsigned_32_Vectors.Vector)
   is
      Length : Ada.Streams.Stream_Element_Count := 0;
   begin
      if Value.Length = 0 then
         return;
      end if;

      for J in 1 .. Value.Length loop
         Length := Length + Size (Value.Get (J));
      end loop;

      Self.Write_Key ((Field, Length_Delimited));
      Self.Write (Length);

      if Self.Riffling then
         Self.Written := Self.Written + Length;
      else
         for J in 1 .. Value.Length loop
            Self.Write_Varint (Value.Get (J));
         end loop;
      end if;
   end Write_Varint_Packed;

   not overriding procedure Write_Varint_Packed
     (Self  : in out Stream;
      Field : Field_Number;
      Value : PB_Support.Unsigned_64_Vectors.Vector)
   is
      Length : Ada.Streams.Stream_Element_Count := 0;
   begin
      if Value.Length = 0 then
         return;
      end if;

      for J in 1 .. Value.Length loop
         Length := Length + Size (Value.Get (J));
      end loop;

      Self.Write_Key ((Field, Length_Delimited));
      Self.Write (Length);

      if Self.Riffling then
         Self.Written := Self.Written + Length;
      else
         for J in 1 .. Value.Length loop
            Self.Write_Varint (Value.Get (J));
         end loop;
      end if;
   end Write_Varint_Packed;

   not overriding procedure Write_Varint_Packed
     (Self  : in out Stream;
      Field : Field_Number;
      Value : PB_Support.Integer_32_Vectors.Vector)
   is
      Length : Ada.Streams.Stream_Element_Count := 0;
   begin
      if Value.Length = 0 then
         return;
      end if;

      for J in 1 .. Value.Length loop
         Length := Length + Size (Value.Get (J));
      end loop;

      Self.Write_Key ((Field, Length_Delimited));
      Self.Write (Length);

      if Self.Riffling then
         Self.Written := Self.Written + Length;
      else
         for J in 1 .. Value.Length loop
            Self.Write_Varint (Value.Get (J));
         end loop;
      end if;
   end Write_Varint_Packed;

   not overriding procedure Write_Varint_Packed
     (Self  : in out Stream;
      Field : Field_Number;
      Value : PB_Support.Integer_64_Vectors.Vector)
   is
      Length : Ada.Streams.Stream_Element_Count := 0;
   begin
      if Value.Length = 0 then
         return;
      end if;

      for J in 1 .. Value.Length loop
         Length := Length + Size (Value.Get (J));
      end loop;

      Self.Write_Key ((Field, Length_Delimited));
      Self.Write (Length);

      if Self.Riffling then
         Self.Written := Self.Written + Length;
      else
         for J in 1 .. Value.Length loop
            Self.Write_Varint (Value.Get (J));
         end loop;
      end if;
   end Write_Varint_Packed;

   -----------
   -- Write --
   -----------

   procedure Write
     (Self  : in out Stream;
      Value : Ada.Streams.Stream_Element_Count) is
   begin
      Self.Write_Varint (Interfaces.Unsigned_32 (Value));
   end Write;

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

   not overriding procedure Write_Option
     (Self    : in out Stream;
      Field   : Field_Number;
      Value   : Boolean;
      Default : Boolean) is
   begin
      if Value /= Default then
         Self.Write (Field, Value);
      end if;
   end Write_Option;

   not overriding procedure Write_Option
     (Self    : in out Stream;
      Field   : Field_Number;
      Value   : Interfaces.IEEE_Float_32;
      Default : Interfaces.IEEE_Float_32)
   is
      use type Interfaces.IEEE_Float_32;
   begin
      if Value /= Default then
         Self.Write (Field, Value);
      end if;
   end Write_Option;

   not overriding procedure Write_Option
     (Self    : in out Stream;
      Field   : Field_Number;
      Value   : Interfaces.IEEE_Float_64;
      Default : Interfaces.IEEE_Float_64)
   is
      use type Interfaces.IEEE_Float_64;
   begin
      if Value /= Default then
         Self.Write (Field, Value);
      end if;
   end Write_Option;

   not overriding procedure Write_Option
     (Self    : in out Stream;
      Field   : Field_Number;
      Value   : Ada.Strings.Unbounded.Unbounded_String;
      Default : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Null_Unbounded_String)
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      if Value /= Default then
         Self.Write (Field, Value);
      end if;
   end Write_Option;

   not overriding procedure Write_Option
     (Self    : in out Stream;
      Field   : Field_Number;
      Value   : PB_Support.Basics.Stream_Element_Vectors.Vector;
      Default : PB_Support.Basics.Stream_Element_Vectors.Vector :=
        PB_Support.Basics.Stream_Element_Vectors.Empty_Vector)
   is
      use type PB_Support.Basics.Stream_Element_Vectors.Vector;
   begin
      if Value /= Default then
         Self.Write (Field, Value);
      end if;
   end Write_Option;

   not overriding procedure Write_Packed
     (Self  : in out Stream;
      Field : Field_Number;
      Value : PB_Support.Boolean_Vectors.Vector)
   is
      Length : constant Ada.Streams.Stream_Element_Count :=
        Ada.Streams.Stream_Element_Count (Value.Length);
      Data   : Ada.Streams.Stream_Element_Array (1 .. Length);
   begin
      if Value.Length = 0 then
         return;
      end if;

      Self.Write_Key ((Field, Length_Delimited));
      Self.Write (Length);

      if Self.Riffling then
         Self.Written := Self.Written + Length;
      else
         for J in Data'Range loop
            Data (J) := Boolean'Pos (Value.Get (Positive (J)));
         end loop;

         Self.Parent.Write (Data);
      end if;
   end Write_Packed;

   not overriding procedure Write_Packed
     (Self  : in out Stream;
      Field : Field_Number;
      Value : PB_Support.IEEE_Float_32_Vectors.Vector)
   is
      Length : constant Ada.Streams.Stream_Element_Count :=
        Ada.Streams.Stream_Element_Count (Value.Length) * 4;
   begin
      if Value.Length = 0 then
         return;
      end if;

      Self.Write_Key ((Field, Length_Delimited));
      Self.Write (Length);

      if Self.Riffling then
         Self.Written := Self.Written + Length;
      else
         for J in 1 .. Value.Length loop
            Self.Write (Value.Get (J));
         end loop;
      end if;
   end Write_Packed;

   not overriding procedure Write_Packed
     (Self  : in out Stream;
      Field : Field_Number;
      Value : PB_Support.IEEE_Float_64_Vectors.Vector)
   is
      Length : constant Ada.Streams.Stream_Element_Count :=
        Ada.Streams.Stream_Element_Count (Value.Length) * 8;
   begin
      if Value.Length = 0 then
         return;
      end if;

      Self.Write_Key ((Field, Length_Delimited));
      Self.Write (Length);

      if Self.Riffling then
         Self.Written := Self.Written + Length;
      else
         for J in 1 .. Value.Length loop
            Self.Write (Value.Get (J));
         end loop;
      end if;
   end Write_Packed;

   procedure Write_Zigzag
     (Self  : in out Stream;
      Value : Interfaces.Integer_32)
   is
      use Interfaces;
      Unsigned : constant Interfaces.Unsigned_32 :=
        2 * Interfaces.Unsigned_32 (abs Value) +
        Boolean'Pos (Value < 0);
   begin
      Self.Write_Varint (Unsigned);
   end Write_Zigzag;

   procedure Write_Zigzag
     (Self  : in out Stream;
      Value : Interfaces.Integer_64)
   is
      use Interfaces;
      Unsigned : constant Interfaces.Unsigned_64 :=
        2 * Interfaces.Unsigned_64 (abs Value) +
        Boolean'Pos (Value < 0);
   begin
      Self.Write_Varint (Unsigned);
   end Write_Zigzag;

   ------------------
   -- Write_Zigzag --
   ------------------

   not overriding procedure Write_Zigzag
     (Self  : in out Stream;
      Field : Field_Number;
      Value : Interfaces.Integer_32) is
   begin
      Self.Write_Key ((Field, Var_Int));
      Self.Write_Zigzag (Value);
   end Write_Zigzag;

   not overriding procedure Write_Zigzag
     (Self  : in out Stream;
      Field : Field_Number;
      Value : Interfaces.Integer_64) is
   begin
      Self.Write_Key ((Field, Var_Int));
      Self.Write_Zigzag (Value);
   end Write_Zigzag;

   not overriding procedure Write_Zigzag
     (Self  : in out Stream;
      Field : Field_Number;
      Value : PB_Support.Integer_32_Vectors.Vector) is
   begin
      for J in 1 .. Value.Length loop
         Self.Write_Zigzag (Field, Value.Get (J));
      end loop;
   end Write_Zigzag;

   not overriding procedure Write_Zigzag
     (Self  : in out Stream;
      Field : Field_Number;
      Value : PB_Support.Integer_64_Vectors.Vector) is
   begin
      for J in 1 .. Value.Length loop
         Self.Write_Zigzag (Field, Value.Get (J));
      end loop;
   end Write_Zigzag;

   not overriding procedure Write_Zigzag_Option
     (Self    : in out Stream;
      Field   : Field_Number;
      Value   : Interfaces.Integer_32;
      Default : Interfaces.Integer_32)
   is
      use type Interfaces.Integer_32;
   begin
      if Value /= Default then
         Self.Write_Zigzag (Field, Value);
      end if;
   end Write_Zigzag_Option;

   not overriding procedure Write_Zigzag_Option
     (Self    : in out Stream;
      Field   : Field_Number;
      Value   : Interfaces.Integer_64;
      Default : Interfaces.Integer_64)
   is
      use type Interfaces.Integer_64;
   begin
      if Value /= Default then
         Self.Write_Zigzag (Field, Value);
      end if;
   end Write_Zigzag_Option;

   not overriding procedure Write_Zigzag_Packed
     (Self  : in out Stream;
      Field : Field_Number;
      Value : PB_Support.Integer_32_Vectors.Vector)
   is
      use type Interfaces.Integer_32;
      use type Interfaces.Unsigned_32;
      Length : Ada.Streams.Stream_Element_Count := 0;
   begin
      if Value.Length = 0 then
         return;
      end if;

      for J in 1 .. Value.Length loop
         Length := Length +
           Size (2 * Interfaces.Unsigned_32 (abs Value.Get (J)));
      end loop;

      Self.Write_Key ((Field, Length_Delimited));
      Self.Write (Length);

      if Self.Riffling then
         Self.Written := Self.Written + Length;
      else
         for J in 1 .. Value.Length loop
            Self.Write_Zigzag (Value.Get (J));
         end loop;
      end if;
   end Write_Zigzag_Packed;

   not overriding procedure Write_Zigzag_Packed
     (Self  : in out Stream;
      Field : Field_Number;
      Value : PB_Support.Integer_64_Vectors.Vector)
   is
      use type Interfaces.Integer_64;
      use type Interfaces.Unsigned_64;
      Length : Ada.Streams.Stream_Element_Count := 0;
   begin
      if Value.Length = 0 then
         return;
      end if;

      for J in 1 .. Value.Length loop
         Length := Length +
           Size (2 * Interfaces.Unsigned_64 (abs Value.Get (J)));
      end loop;

      Self.Write_Key ((Field, Length_Delimited));
      Self.Write (Length);

      if Self.Riffling then
         Self.Written := Self.Written + Length;
      else
         for J in 1 .. Value.Length loop
            Self.Write_Zigzag (Value.Get (J));
         end loop;
      end if;
   end Write_Zigzag_Packed;

   -----------------
   -- Write_Fixed --
   -----------------

   procedure Write_Fixed
     (Self  : in out Stream;
      Value : Interfaces.Integer_32) is
   begin
      if Self.Riffling then
         Self.Written := Self.Written + 4;
      else
         Interfaces.Integer_32'Write (Self.Parent, Value);
      end if;
   end Write_Fixed;

   procedure Write_Fixed
     (Self  : in out Stream;
      Value : Interfaces.Integer_64) is
   begin
      if Self.Riffling then
         Self.Written := Self.Written + 8;
      else
         Interfaces.Integer_64'Write (Self.Parent, Value);
      end if;
   end Write_Fixed;

   procedure Write_Fixed
     (Self  : in out Stream;
      Value : Interfaces.Unsigned_32) is
   begin
      if Self.Riffling then
         Self.Written := Self.Written + 4;
      else
         Interfaces.Unsigned_32'Write (Self.Parent, Value);
      end if;
   end Write_Fixed;

   procedure Write_Fixed
     (Self  : in out Stream;
      Value : Interfaces.Unsigned_64) is
   begin
      if Self.Riffling then
         Self.Written := Self.Written + 8;
      else
         Interfaces.Unsigned_64'Write (Self.Parent, Value);
      end if;
   end Write_Fixed;

   not overriding procedure Write_Fixed
     (Self  : in out Stream;
      Field : Field_Number;
      Value : Interfaces.Integer_32) is
   begin
      Self.Write_Key ((Field, Fixed_32));
      Self.Write_Fixed (Value);
   end Write_Fixed;

   not overriding procedure Write_Fixed
     (Self  : in out Stream;
      Field : Field_Number;
      Value : Interfaces.Integer_64) is
   begin
      Self.Write_Key ((Field, Fixed_64));
      Self.Write_Fixed (Value);
   end Write_Fixed;

   not overriding procedure Write_Fixed
     (Self  : in out Stream;
      Field : Field_Number;
      Value : Interfaces.Unsigned_32) is
   begin
      Self.Write_Key ((Field, Fixed_32));
      Self.Write_Fixed (Value);
   end Write_Fixed;

   not overriding procedure Write_Fixed
     (Self  : in out Stream;
      Field : Field_Number;
      Value : Interfaces.Unsigned_64) is
   begin
      Self.Write_Key ((Field, Fixed_64));
      Self.Write_Fixed (Value);
   end Write_Fixed;

   not overriding procedure Write_Fixed
     (Self  : in out Stream;
      Field : Field_Number;
      Value : PB_Support.Integer_32_Vectors.Vector) is
   begin
      for J in 1 .. Value.Length loop
         Self.Write_Fixed (Field, Value.Get (J));
      end loop;
   end Write_Fixed;

   not overriding procedure Write_Fixed
     (Self  : in out Stream;
      Field : Field_Number;
      Value : PB_Support.Integer_64_Vectors.Vector) is
   begin
      for J in 1 .. Value.Length loop
         Self.Write_Fixed (Field, Value.Get (J));
      end loop;
   end Write_Fixed;

   not overriding procedure Write_Fixed
     (Self  : in out Stream;
      Field : Field_Number;
      Value : PB_Support.Unsigned_32_Vectors.Vector) is
   begin
      for J in 1 .. Value.Length loop
         Self.Write_Fixed (Field, Value.Get (J));
      end loop;
   end Write_Fixed;

   not overriding procedure Write_Fixed
     (Self  : in out Stream;
      Field : Field_Number;
      Value : PB_Support.Unsigned_64_Vectors.Vector) is
   begin
      for J in 1 .. Value.Length loop
         Self.Write_Fixed (Field, Value.Get (J));
      end loop;
   end Write_Fixed;

   not overriding procedure Write_Fixed_Option
     (Self    : in out Stream;
      Field   : Field_Number;
      Value   : Interfaces.Unsigned_32;
      Default : Interfaces.Unsigned_32)
   is
      use type Interfaces.Unsigned_32;
   begin
      if Value /= Default then
         Self.Write_Fixed (Field, Value);
      end if;
   end Write_Fixed_Option;

   not overriding procedure Write_Fixed_Option
     (Self    : in out Stream;
      Field   : Field_Number;
      Value   : Interfaces.Unsigned_64;
      Default : Interfaces.Unsigned_64)
   is
      use type Interfaces.Unsigned_64;
   begin
      if Value /= Default then
         Self.Write_Fixed (Field, Value);
      end if;
   end Write_Fixed_Option;

   not overriding procedure Write_Fixed_Option
     (Self    : in out Stream;
      Field   : Field_Number;
      Value   : Interfaces.Integer_32;
      Default : Interfaces.Integer_32)
   is
      use type Interfaces.Integer_32;
   begin
      if Value /= Default then
         Self.Write_Fixed (Field, Value);
      end if;
   end Write_Fixed_Option;

   not overriding procedure Write_Fixed_Option
     (Self    : in out Stream;
      Field   : Field_Number;
      Value   : Interfaces.Integer_64;
      Default : Interfaces.Integer_64)
   is
      use type Interfaces.Integer_64;
   begin
      if Value /= Default then
         Self.Write_Fixed (Field, Value);
      end if;
   end Write_Fixed_Option;

   not overriding procedure Write_Fixed_Packed
     (Self  : in out Stream;
      Field : Field_Number;
      Value : PB_Support.Integer_32_Vectors.Vector)
   is
      Length : constant Ada.Streams.Stream_Element_Count :=
        Ada.Streams.Stream_Element_Count (Value.Length) * 4;
   begin
      if Value.Length = 0 then
         return;
      end if;

      Self.Write_Key ((Field, Length_Delimited));
      Self.Write (Length);

      if Self.Riffling then
         Self.Written := Self.Written + Length;
      else
         for J in 1 .. Value.Length loop
            Self.Write_Fixed (Value.Get (J));
         end loop;
      end if;
   end Write_Fixed_Packed;

   not overriding procedure Write_Fixed_Packed
     (Self  : in out Stream;
      Field : Field_Number;
      Value : PB_Support.Integer_64_Vectors.Vector)
   is
      Length : constant Ada.Streams.Stream_Element_Count :=
        Ada.Streams.Stream_Element_Count (Value.Length) * 8;
   begin
      if Value.Length = 0 then
         return;
      end if;

      Self.Write_Key ((Field, Length_Delimited));
      Self.Write (Length);

      if Self.Riffling then
         Self.Written := Self.Written + Length;
      else
         for J in 1 .. Value.Length loop
            Self.Write_Fixed (Value.Get (J));
         end loop;
      end if;
   end Write_Fixed_Packed;

   not overriding procedure Write_Fixed_Packed
     (Self  : in out Stream;
      Field : Field_Number;
      Value : PB_Support.Unsigned_32_Vectors.Vector)
   is
      Length : constant Ada.Streams.Stream_Element_Count :=
        Ada.Streams.Stream_Element_Count (Value.Length) * 4;
   begin
      if Value.Length = 0 then
         return;
      end if;

      Self.Write_Key ((Field, Length_Delimited));
      Self.Write (Length);

      if Self.Riffling then
         Self.Written := Self.Written + Length;
      else
         for J in 1 .. Value.Length loop
            Self.Write_Fixed (Value.Get (J));
         end loop;
      end if;
   end Write_Fixed_Packed;

   not overriding procedure Write_Fixed_Packed
     (Self  : in out Stream;
      Field : Field_Number;
      Value : PB_Support.Unsigned_64_Vectors.Vector)
   is
      Length : constant Ada.Streams.Stream_Element_Count :=
        Ada.Streams.Stream_Element_Count (Value.Length) * 8;
   begin
      if Value.Length = 0 then
         return;
      end if;

      Self.Write_Key ((Field, Length_Delimited));
      Self.Write (Length);

      if Self.Riffling then
         Self.Written := Self.Written + Length;
      else
         for J in 1 .. Value.Length loop
            Self.Write_Fixed (Value.Get (J));
         end loop;
      end if;
   end Write_Fixed_Packed;

end PB_Support.Internal;
