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

with League.Text_Codecs;

--  with Ada.Streams.Stream_IO;

package body PB_Support.IO is

   use type Ada.Streams.Stream_Element;
   use type Ada.Streams.Stream_Element_Count;

   type Stop_Stream is new Ada.Streams.Root_Stream_Type with record
      Left   : Ada.Streams.Stream_Element_Count;
      Parent : not null access Ada.Streams.Root_Stream_Type'Class;
   end record;

   overriding procedure Read
     (Self : in out Stop_Stream;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (Self : in out Stop_Stream;
      Item : Ada.Streams.Stream_Element_Array) is null;

   -------------
   -- Enum_IO --
   -------------

   package body Enum_IO is

      function Cast is new Ada.Unchecked_Conversion
        (Element, Integer_Element);

      ----------
      -- Read --
      ----------

      procedure Read
        (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
         Encoding : Wire_Type;
         Value    : out Element)
      is
         function Cast is new Ada.Unchecked_Conversion
           (Integer_Element, Element);
         Int : Integer_Element;
      begin
         Read_Varint (Stream, Encoding, Interfaces.Integer_32 (Int));
         Value := Cast (Int);
      end Read;

      -----------------
      -- Read_Vector --
      -----------------

      procedure Read_Vector
        (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
         Encoding : Wire_Type;
         Value    : in out Vectors.Vector)
      is
         Item : Element;
      begin
         if Encoding = Length_Delimited then
            declare
               Stop : aliased Stop_Stream :=
                 (Ada.Streams.Root_Stream_Type with
                    Left   => Read_Length (Stream),
                    Parent => Stream);
            begin
               while Stop.Left > 0 loop
                  Read (Stop'Unchecked_Access, Var_Int, Item);
                  Vectors.Append (Value, Item);
               end loop;
            end;
         else
            Read (Stream, Encoding, Item);
            Vectors.Append (Value, Item);
         end if;
      end Read_Vector;

      -----------
      -- Write --
      -----------

      procedure Write
        (Stream : in out Internal.Stream;
         Field  : Field_Number;
         Value  : Element)
      is
         Int : constant Integer_Element := Cast (Value);
      begin
         Stream.Write_Varint (Field, Interfaces.Integer_32 (Int));
      end Write;

      -----------
      -- Write --
      -----------

      procedure Write
        (Stream : in out Internal.Stream;
         Field  : Field_Number;
         Value  : Vectors.Vector) is
      begin
         for J in 1 .. Value.Length loop
            Write (Stream, Field, Value.Get (J));
         end loop;
      end Write;

      ------------------
      -- Write_Option --
      ------------------

      procedure Write_Option
        (Stream  : in out Internal.Stream;
         Field   : Field_Number;
         Value   : Element;
         Default : Element) is
      begin
         if Value /= Default then
            Write (Stream, Field, Value);
         end if;
      end Write_Option;

      procedure Write_Packed
        (Stream : in out Internal.Stream;
         Field  : Field_Number;
         Value  : Vectors.Vector)
      is
         Length : Ada.Streams.Stream_Element_Count := 0;
      begin
         if Value.Length = 0 then
            return;
         end if;

         for J in 1 .. Value.Length loop
            Length := Length + PB_Support.Internal.Size
              (Interfaces.Integer_32 (Cast (Value.Get (J))));
         end loop;

         Stream.Write_Key ((Field, Length_Delimited));
         Stream.Write (Length);

         for J in 1 .. Value.Length loop
            Stream.Write_Varint
              (Interfaces.Integer_32 (Cast (Value.Get (J))));
         end loop;
      end Write_Packed;

   end Enum_IO;

   ----------------
   -- Message_IO --
   ----------------

   package body Message_IO is

      ----------
      -- Read --
      ----------

      procedure Read
        (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
         Encoding : Wire_Type;
         Value    : out Element)
      is
         Stop : aliased Stop_Stream :=
           (Ada.Streams.Root_Stream_Type with
            Left   => Read_Length (Stream),
            Parent => Stream);
      begin
         pragma Assert (Encoding = Length_Delimited);
         Element'Read (Stop'Unchecked_Access, Value);
      end Read;

      -----------------
      -- Read_Vector --
      -----------------

      procedure Read_Vector
        (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
         Encoding : Wire_Type;
         Value    : in out Vector)
      is
         Item : Element;
      begin
         Read (Stream, Encoding, Item);
         Append (Value, Item);
      end Read_Vector;

   end Message_IO;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self : in out Stop_Stream;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
   begin
      if Self.Left >= Item'Length then
         Self.Parent.Read (Item, Last);
      elsif Self.Left > 0 then
         Self.Parent.Read
           (Item (Item'First .. Item'First + Self.Left - 1), Last);
      else
         Last := Item'First - 1;
      end if;

      Self.Left := Self.Left - (Last - Item'First + 1);
   end Read;

   -----------------------
   -- Read_Array_Length --
   -----------------------

   function Read_Array_Length
     (Stream    : not null access Ada.Streams.Root_Stream_Type'Class;
      Item_Size : Ada.Streams.Stream_Element_Count)
      return Natural
   is
      Length : constant Ada.Streams.Stream_Element_Count :=
        Read_Length (Stream);
   begin
      if Length mod Item_Size = 0 then
         return Natural (Length / Item_Size);
      else
         raise Constraint_Error with "Unexpected array length";
      end if;
   end Read_Array_Length;

   ------------------
   -- Read_Boolean --
   ------------------

   procedure Read
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Boolean)
   is
      Data : Ada.Streams.Stream_Element := 16#80#;
   begin
      pragma Assert (Encoding = Var_Int);
      Value := False;

      while Data >= 16#80# loop
         Ada.Streams.Stream_Element'Read (Stream, Data);
         Value := Value or ((Data and 16#7F#) /= 0);
      end loop;
   end Read;

   -----------------------
   -- Read_Fixed_Vector --
   -----------------------

   procedure Read_Fixed_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out PB_Support.Integer_64_Vectors.Vector)
   is
      Item : Interfaces.Integer_64;
   begin
      if Encoding = Fixed_64 then
         Read_Fixed (Stream, Encoding, Item);
         Value.Append (Item);
      elsif Encoding = Length_Delimited then
         declare
            type Integer_64_Array is
              array (Positive range <>) of Interfaces.Integer_64;

            Data  : Integer_64_Array (1 .. Read_Array_Length (Stream, 8));
         begin
            Integer_64_Array'Read (Stream, Data);

            for J of Data loop
               Value.Append (J);
            end loop;
         end;
      else
         raise Constraint_Error with "Unexpected encoding";
      end if;
   end Read_Fixed_Vector;

   procedure Read_Fixed_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out PB_Support.Integer_32_Vectors.Vector)
   is
      Item : Interfaces.Integer_32;
   begin
      if Encoding = Fixed_32 then
         Read_Fixed (Stream, Encoding, Item);
         Value.Append (Item);
      elsif Encoding = Length_Delimited then
         declare
            type Integer_32_Array is
              array (Positive range <>) of Interfaces.Integer_32;

            Data  : Integer_32_Array (1 .. Read_Array_Length (Stream, 4));
         begin
            Integer_32_Array'Read (Stream, Data);

            for J of Data loop
               Value.Append (J);
            end loop;
         end;
      else
         raise Constraint_Error with "Unexpected encoding";
      end if;
   end Read_Fixed_Vector;

   procedure Read_Fixed_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out PB_Support.Unsigned_64_Vectors.Vector)
   is
      Item : Interfaces.Unsigned_64;
   begin
      if Encoding = Fixed_64 then
         Read_Fixed (Stream, Encoding, Item);
         Value.Append (Item);
      elsif Encoding = Length_Delimited then
         declare
            type Unsigned_64_Array is
              array (Positive range <>) of Interfaces.Unsigned_64;

            Data  : Unsigned_64_Array (1 .. Read_Array_Length (Stream, 8));
         begin
            Unsigned_64_Array'Read (Stream, Data);

            for J of Data loop
               Value.Append (J);
            end loop;
         end;
      else
         raise Constraint_Error with "Unexpected encoding";
      end if;
   end Read_Fixed_Vector;

   procedure Read_Fixed_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out PB_Support.Unsigned_32_Vectors.Vector)
   is
      Item : Interfaces.Unsigned_32;
   begin
      if Encoding = Fixed_32 then
         Read_Fixed (Stream, Encoding, Item);
         Value.Append (Item);
      elsif Encoding = Length_Delimited then
         declare
            type Unsigned_32_Array is
              array (Positive range <>) of Interfaces.Unsigned_32;

            Data  : Unsigned_32_Array (1 .. Read_Array_Length (Stream, 4));
         begin
            Unsigned_32_Array'Read (Stream, Data);

            for J of Data loop
               Value.Append (J);
            end loop;
         end;
      else
         raise Constraint_Error with "Unexpected encoding";
      end if;
   end Read_Fixed_Vector;

   -------------------------
   -- Read_Vector --
   -------------------------

   procedure Read_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out PB_Support.Boolean_Vectors.Vector)
   is
      Item : Boolean := False;
   begin
      if Encoding = Var_Int then
         Read (Stream, Encoding, Item);
         Value.Append (Item);
      elsif Encoding = Length_Delimited then
         declare
            Data : Ada.Streams.Stream_Element_Array
              (1 .. Read_Length (Stream));
         begin
            Ada.Streams.Stream_Element_Array'Read (Stream, Data);
            pragma Assert ((Data (Data'Last) and 16#80#) = 0);

            for J of Data loop
               Item := Item or ((J and 16#7F#) /= 0);

               if (J and 16#80#) = 0 then
                  Value.Append (Item);
                  Item := False;
               end if;
            end loop;
         end;
      end if;
   end Read_Vector;

   ------------------------
   -- Read_IEEE_Float_32 --
   ------------------------

   procedure Read
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Interfaces.IEEE_Float_32) is
   begin
      if Encoding = Fixed_32 then
         --  FIX this for big-endian
         Interfaces.IEEE_Float_32'Read (Stream, Value);
      elsif Encoding = Fixed_64 then
         Read
           (Stream, Encoding, Interfaces.IEEE_Float_64 (Value));
      else
         raise Constraint_Error with "unexpected wire encoding";
      end if;
   end Read;

   ------------------------
   -- Read_IEEE_Float_64 --
   ------------------------

   procedure Read
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Interfaces.IEEE_Float_64) is
   begin
      if Encoding = Fixed_64 then
         --  FIX this for big-endian
         Interfaces.IEEE_Float_64'Read (Stream, Value);
      elsif Encoding = Fixed_32 then
         Read
           (Stream, Encoding, Interfaces.IEEE_Float_32 (Value));
      else
         raise Constraint_Error with "unexpected wire encoding";
      end if;
   end Read;

   -------------------------------
   -- Read_Vector --
   -------------------------------

   procedure Read_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out PB_Support.IEEE_Float_64_Vectors.Vector)
   is
      Item : Interfaces.IEEE_Float_64;
   begin
      if Encoding = Fixed_64 then
         Read (Stream, Encoding, Item);
         Value.Append (Item);
      elsif Encoding = Length_Delimited then
         declare
            type IEEE_Float_64_Array is array
              (Positive range <>) of Interfaces.IEEE_Float_64;

            Data  : IEEE_Float_64_Array (1 .. Read_Array_Length (Stream, 8));
         begin
            IEEE_Float_64_Array'Read (Stream, Data);

            for J of Data loop
               Value.Append (J);
            end loop;
         end;
      else
         raise Constraint_Error with "Unexpected encoding";
      end if;
   end Read_Vector;

   -------------------------------
   -- Read_IEEE_Float_32_Vector --
   -------------------------------

   procedure Read_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out PB_Support.IEEE_Float_32_Vectors.Vector)
   is
      Item : Interfaces.IEEE_Float_32;
   begin
      if Encoding = Fixed_32 then
         Read (Stream, Encoding, Item);
         Value.Append (Item);
      elsif Encoding = Length_Delimited then
         declare
            type IEEE_Float_32_Array is array
              (Positive range <>) of
                Interfaces.IEEE_Float_32;

            Data  : IEEE_Float_32_Array (1 .. Read_Array_Length (Stream, 4));
         begin
            IEEE_Float_32_Array'Read (Stream, Data);

            for J of Data loop
               Value.Append (J);
            end loop;
         end;
      else
         raise Constraint_Error with "Unexpected encoding";
      end if;
   end Read_Vector;

   ---------------------
   -- Read_Integer_32 --
   ---------------------

   procedure Read_Varint
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Interfaces.Integer_32)
   is
      function Cast is new Ada.Unchecked_Conversion
        (Interfaces.Unsigned_32, Interfaces.Integer_32);

      Data  : Interfaces.Unsigned_32;
   begin
      Read_Varint (Stream, Encoding, Data);
      Value := Cast (Data);
   end Read_Varint;

   ----------------------------
   -- Read_Vector --
   ----------------------------

   procedure Read_Varint_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out PB_Support.Integer_32_Vectors.Vector)
   is
      function Cast is new Ada.Unchecked_Conversion
        (Interfaces.Unsigned_32, Interfaces.Integer_32);

      Item : Interfaces.Unsigned_32 := 0;
   begin
      if Encoding = Var_Int then
         Read_Varint (Stream, Encoding, Item);
         Value.Append (Cast (Item));
      elsif Encoding = Length_Delimited then
         declare
            use type Interfaces.Unsigned_32;

            Shift : Natural := 0;
            Data  : Ada.Streams.Stream_Element_Array
              (1 .. Read_Length (Stream));
         begin
            Ada.Streams.Stream_Element_Array'Read (Stream, Data);
            pragma Assert ((Data (Data'Last) and 16#80#) = 0);

            for J of Data loop
               Item := Item or
                 Interfaces.Shift_Left
                   (Interfaces.Unsigned_32 (J and 16#7F#), Shift);

               Shift := Shift + 7;

               if (J and 16#80#) = 0 then
                  Value.Append (Cast (Item));
                  Item := 0;
                  Shift := 0;
               end if;
            end loop;
         end;
      end if;
   end Read_Varint_Vector;

   procedure Read_Varint_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out PB_Support.Integer_64_Vectors.Vector)
   is
      ----------
      -- Cast --
      ----------

      function Cast is new Ada.Unchecked_Conversion
        (Interfaces.Unsigned_64, Interfaces.Integer_64);

      Item : Interfaces.Unsigned_64 := 0;
   begin
      if Encoding = Var_Int then
         Read_Varint (Stream, Encoding, Item);
         Value.Append (Cast (Item));
      elsif Encoding = Length_Delimited then
         declare
            use type Interfaces.Unsigned_64;

            Shift : Natural := 0;
            Data  : Ada.Streams.Stream_Element_Array
              (1 .. Read_Length (Stream));
         begin
            Ada.Streams.Stream_Element_Array'Read (Stream, Data);
            pragma Assert ((Data (Data'Last) and 16#80#) = 0);

            for J of Data loop
               Item := Item or
                 Interfaces.Shift_Left
                   (Interfaces.Unsigned_64 (J and 16#7F#), Shift);

               Shift := Shift + 7;

               if (J and 16#80#) = 0 then
                  Value.Append (Cast (Item));
                  Item := 0;
                  Shift := 0;
               end if;
            end loop;
         end;
      end if;
   end Read_Varint_Vector;

   ---------------------
   -- Read_Integer_64 --
   ---------------------

   procedure Read_Varint
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Interfaces.Integer_64)
   is
      function Cast is new Ada.Unchecked_Conversion
        (Interfaces.Unsigned_64, Interfaces.Integer_64);

      Data  : Interfaces.Unsigned_64;
   begin
      Read_Varint (Stream, Encoding, Data);
      Value := Cast (Data);
   end Read_Varint;

   --------------
   -- Read_Key --
   --------------

   function Read_Key
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Result : access Key) return Boolean
   is
      use type Interfaces.Unsigned_32;

      Last  : Ada.Streams.Stream_Element_Count;
      Data  : Ada.Streams.Stream_Element_Array (1 .. 1);
      Item  : Ada.Streams.Stream_Element;
      Shift : Natural := 4;  --  7 bits/item - 3 bit for wire type
      Field : Interfaces.Unsigned_32;
   begin
      Stream.Read (Data, Last);

      if Last = 0 then
         return False;
      end if;

      Item := Data (1);
      Result.Encoding := Wire_Type'Val (Item and 7);

      Field := Interfaces.Shift_Right
        (Interfaces.Unsigned_32 (Item and 16#7F#), 3);

      while Item >= 16#80# loop
         Ada.Streams.Stream_Element'Read (Stream, Item);

         Field := Field or Interfaces.Shift_Left
           (Interfaces.Unsigned_32 (Item and 16#7F#), Shift);

         Shift := Shift + 7;
      end loop;

      Result.Field := Field_Number'Val (Field);

      return True;
   end Read_Key;

   -----------------
   -- Read_Length --
   -----------------

   function Read_Length
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Ada.Streams.Stream_Element_Count
   is
      Result : Interfaces.Unsigned_64;
   begin
      Read_Varint (Stream, Var_Int, Result);

      return Ada.Streams.Stream_Element_Count (Result);
   end Read_Length;

   --------------------------------
   -- Read_Stream_Element_Vector --
   --------------------------------

   procedure Read
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out League.Stream_Element_Vectors.Stream_Element_Vector)
   is
   begin
      pragma Assert (Encoding = Length_Delimited);

      declare
         Data : Ada.Streams.Stream_Element_Array (1 .. Read_Length (Stream));
      begin
         --  FIX: avoid large stack usage here
         Ada.Streams.Stream_Element_Array'Read (Stream, Data);
         Value.Clear;
         Value.Append (Data);
      end;
   end Read;

   ---------------------------
   -- Read_Universal_String --
   ---------------------------

   procedure Read
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out League.Strings.Universal_String)
   is
      Data  : League.Stream_Element_Vectors.Stream_Element_Vector;
      Codec : constant League.Text_Codecs.Text_Codec :=
        League.Text_Codecs.Codec
          (League.Strings.To_Universal_String ("utf-8"));
   begin
      Read (Stream, Encoding, Data);
      Value := Codec.Decode (Data);
   end Read;

   ----------------------------------
   -- Read_Vector --
   ----------------------------------

   procedure Read_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out League.String_Vectors.Universal_String_Vector)
   is
      Item : League.Strings.Universal_String;
   begin
      --  FIXME: For now, unpacked vector only
      Read (Stream, Encoding, Item);
      Value.Append (Item);
   end Read_Vector;

   ---------------------------------------
   -- Read_Vector --
   ---------------------------------------

   procedure Read_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out PB_Support.Stream_Element_Vector_Vectors.Vector)
   is
      Item : League.Stream_Element_Vectors.Stream_Element_Vector;
   begin
      --  FIXME: For now, unpacked vector only
      Read (Stream, Encoding, Item);
      Value.Append (Item);
   end Read_Vector;

   ----------------------
   -- Read_Unsigned_32 --
   ----------------------

   procedure Read_Varint
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Interfaces.Unsigned_32)
   is
      use type Interfaces.Unsigned_32;
   begin
      if Encoding = Var_Int then
         declare
            Data  : Ada.Streams.Stream_Element := 16#80#;
            Shift : Natural := 0;
         begin
            Value := 0;
            while Data >= 16#80# loop
               Ada.Streams.Stream_Element'Read (Stream, Data);
               Value := Value or Interfaces.Shift_Left
                 (Interfaces.Unsigned_32 (Data and 16#7F#), Shift);

               Shift := Shift + 7;
            end loop;
         end;
      elsif Encoding = Fixed_32 then
         --  FIX this for big-endian
         Interfaces.Unsigned_32'Read (Stream, Value);
      elsif Encoding = Fixed_64 then
         Read_Varint
           (Stream, Encoding, Interfaces.Unsigned_64 (Value));
      else
         raise Constraint_Error with "unexpected wire encoding";
      end if;
   end Read_Varint;

   -----------------------------
   -- Read_Vector --
   -----------------------------

   procedure Read_Varint_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out PB_Support.Unsigned_32_Vectors.Vector)
   is
      Item : Interfaces.Unsigned_32 := 0;
   begin
      if Encoding = Var_Int then
         Read_Varint (Stream, Encoding, Item);
         Value.Append (Item);
      elsif Encoding = Length_Delimited then
         declare
            use type Interfaces.Unsigned_32;

            Shift : Natural := 0;
            Data  : Ada.Streams.Stream_Element_Array
              (1 .. Read_Length (Stream));
         begin
            Ada.Streams.Stream_Element_Array'Read (Stream, Data);
            pragma Assert ((Data (Data'Last) and 16#80#) = 0);

            for J of Data loop
               Item := Item or
                 Interfaces.Shift_Left
                   (Interfaces.Unsigned_32 (J and 16#7F#), Shift);

               Shift := Shift + 7;

               if (J and 16#80#) = 0 then
                  Value.Append (Item);
                  Item := 0;
                  Shift := 0;
               end if;
            end loop;
         end;
      end if;
   end Read_Varint_Vector;

   -----------------------------
   -- Read_Vector --
   -----------------------------

   procedure Read_Varint_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : in out PB_Support.Unsigned_64_Vectors.Vector)
   is
      Item : Interfaces.Unsigned_64 := 0;
   begin
      if Encoding = Var_Int then
         Read_Varint (Stream, Encoding, Item);
         Value.Append (Item);
      elsif Encoding = Length_Delimited then
         declare
            use type Interfaces.Unsigned_64;

            Shift : Natural := 0;
            Data  : Ada.Streams.Stream_Element_Array
              (1 .. Read_Length (Stream));
         begin
            Ada.Streams.Stream_Element_Array'Read (Stream, Data);
            pragma Assert ((Data (Data'Last) and 16#80#) = 0);

            for J of Data loop
               Item := Item or
                 Interfaces.Shift_Left
                   (Interfaces.Unsigned_64 (J and 16#7F#), Shift);

               Shift := Shift + 7;

               if (J and 16#80#) = 0 then
                  Value.Append (Item);
                  Item := 0;
                  Shift := 0;
               end if;
            end loop;
         end;
      end if;
   end Read_Varint_Vector;

   ----------------------
   -- Read_Unsigned_64 --
   ----------------------

   procedure Read_Varint
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Interfaces.Unsigned_64)
   is
      use type Interfaces.Unsigned_64;
   begin
      if Encoding = Var_Int then
         declare
            Data  : Ada.Streams.Stream_Element := 16#80#;
            Shift : Natural := 0;
         begin
            Value := 0;
            while Data >= 16#80# loop
               Ada.Streams.Stream_Element'Read (Stream, Data);
               Value := Value or Interfaces.Shift_Left
                 (Interfaces.Unsigned_64 (Data and 16#7F#), Shift);

               Shift := Shift + 7;
            end loop;
         end;
      elsif Encoding = Fixed_64 then
         --  FIX this for big-endian
         Interfaces.Unsigned_64'Read (Stream, Value);
      elsif Encoding = Fixed_32 then
         Read_Varint
           (Stream, Encoding, Interfaces.Unsigned_32 (Value));
      else
         raise Constraint_Error with "unexpected wire encoding";
      end if;
   end Read_Varint;

   -----------------
   -- Read_Zigzag --
   -----------------

   procedure Read_Zigzag
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Interfaces.Integer_32)
   is
      use type Interfaces.Integer_32;
      use type Interfaces.Unsigned_32;

      Temp : Interfaces.Unsigned_32;
   begin
      Read_Varint (Stream, Encoding, Temp);
      Value := Interfaces.Integer_32 (Temp / 2);

      if (Temp and 1) > 0 then
         Value := -Value;
      end if;
   end Read_Zigzag;

   -----------------
   -- Read_Zigzag --
   -----------------

   procedure Read_Zigzag
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out Interfaces.Integer_64)
   is
      use type Interfaces.Integer_64;
      use type Interfaces.Unsigned_64;

      Temp : Interfaces.Unsigned_64;
   begin
      Read_Varint (Stream, Encoding, Temp);
      Value := Interfaces.Integer_64 (Temp / 2);

      if (Temp and 1) > 0 then
         Value := -Value;
      end if;
   end Read_Zigzag;

   ------------------------
   -- Read_Zigzag_Vector --
   ------------------------

   procedure Read_Zigzag_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out PB_Support.Integer_32_Vectors.Vector) is
   begin
      if Encoding = Var_Int then
         declare
            Item : Interfaces.Integer_32 := 0;
         begin
            Read_Zigzag (Stream, Encoding, Item);
            Value.Append (Item);
         end;
      elsif Encoding = Length_Delimited then
         declare
            use type Interfaces.Integer_32;
            use type Interfaces.Unsigned_32;

            Item  : Interfaces.Unsigned_32 := 0;
            Shift : Natural := 0;
            Data  : Ada.Streams.Stream_Element_Array
              (1 .. Read_Length (Stream));
         begin
            Ada.Streams.Stream_Element_Array'Read (Stream, Data);
            pragma Assert ((Data (Data'Last) and 16#80#) = 0);

            for J of Data loop
               Item := Item or
                 Interfaces.Shift_Left
                   (Interfaces.Unsigned_32 (J and 16#7F#), Shift);

               Shift := Shift + 7;

               if (J and 16#80#) = 0 then
                  if (Item and 1) = 0 then
                     Value.Append (Interfaces.Integer_32 (Item / 2));
                  else
                     Value.Append (-Interfaces.Integer_32 (Item / 2));
                  end if;

                  Item := 0;
                  Shift := 0;
               end if;
            end loop;
         end;
      end if;
   end Read_Zigzag_Vector;

   ------------------------
   -- Read_Zigzag_Vector --
   ------------------------

   procedure Read_Zigzag_Vector
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type;
      Value    : out PB_Support.Integer_64_Vectors.Vector)
   is
   begin
      if Encoding = Var_Int then
         declare
            Item : Interfaces.Integer_64 := 0;
         begin
            Read_Zigzag (Stream, Encoding, Item);
            Value.Append (Item);
         end;
      elsif Encoding = Length_Delimited then
         declare
            use type Interfaces.Integer_64;
            use type Interfaces.Unsigned_64;

            Item  : Interfaces.Unsigned_64 := 0;
            Shift : Natural := 0;
            Data  : Ada.Streams.Stream_Element_Array
              (1 .. Read_Length (Stream));
         begin
            Ada.Streams.Stream_Element_Array'Read (Stream, Data);
            pragma Assert ((Data (Data'Last) and 16#80#) = 0);

            for J of Data loop
               Item := Item or
                 Interfaces.Shift_Left
                   (Interfaces.Unsigned_64 (J and 16#7F#), Shift);

               Shift := Shift + 7;

               if (J and 16#80#) = 0 then
                  if (Item and 1) = 0 then
                     Value.Append (Interfaces.Integer_64 (Item / 2));
                  else
                     Value.Append (-Interfaces.Integer_64 (Item / 2));
                  end if;

                  Item := 0;
                  Shift := 0;
               end if;
            end loop;
         end;
      end if;
   end Read_Zigzag_Vector;

   -------------------
   -- Unknown_Field --
   -------------------

   procedure Unknown_Field
     (Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Encoding : Wire_Type)
   is
   begin
      case Encoding is
         when Var_Int =>
            declare
               Item : Ada.Streams.Stream_Element := 16#80#;
            begin
               while Item >= 16#80# loop
                  Ada.Streams.Stream_Element'Read (Stream, Item);
               end loop;
            end;
      when Fixed_64 =>
            declare
               Ignore : Interfaces.Unsigned_64;
            begin
               Interfaces.Unsigned_64'Read (Stream, Ignore);
            end;
      when Length_Delimited =>
            declare
               Ignore : Ada.Streams.Stream_Element_Array
                 (1 .. Read_Length (Stream));
            begin
               Ada.Streams.Stream_Element_Array'Read (Stream, Ignore);
            end;
         when Start_Group | End_Group =>
            raise Constraint_Error with "Group encoding is unimplemented";
      when Fixed_32 =>
            declare
               Ignore : Interfaces.Unsigned_32;
            begin
               Interfaces.Unsigned_32'Read (Stream, Ignore);
            end;
      end case;
   end Unknown_Field;

end PB_Support.IO;
