with Ada.Characters.Latin_1;  
with Ada.Strings.Fixed;
with Ada.Text_IO;

with PB_Support.Common_JSON;

package body PB_Support.JSON is

   use all type Ada.Strings.Unbounded.Unbounded_String;

   ------------------
   -- Start_Object --
   ------------------

   procedure Start_Object (Self : in out JSON_Writer) is
   begin
      if Self.Needs_Comma then
         Append (Self.Text, ",");
      end if;
      Append (Self.Text, "{");
      Self.Needs_Comma := False;
   end Start_Object;

   ----------------
   -- End_Object --
   ----------------

   procedure End_Object (Self : in out JSON_Writer) is
   begin
      Append (Self.Text, "}");
      Self.Needs_Comma := True;
   end End_Object;

   -----------------
   -- Start_Array --
   -----------------

   procedure Start_Array (Self : in out JSON_Writer) is
   begin
      if Self.Needs_Comma then
         Append (Self.Text, ",");
      end if;
      Append (Self.Text, "[");
      Self.Needs_Comma := False;
   end Start_Array;

   ---------------
   -- End_Array --
   ---------------

   procedure End_Array (Self : in out JSON_Writer) is
   begin
      Append (Self.Text, "]");
      Self.Needs_Comma := True;
   end End_Array;

   ---------------
   -- Write_Key --
   ---------------

   procedure Write_Key (Self : in out JSON_Writer; Name : String) is
   begin
      if Self.Needs_Comma then
         Append (Self.Text, ",");
      end if;
      Append (Self.Text, '"');
      Append (Self.Text, Name);
      Append (Self.Text, """:");
      Self.Needs_Comma := False;
   end Write_Key;

   ------------------
   -- Write_String --
   ------------------

   procedure Write_String (Self : in out JSON_Writer; Value : String)
   is

      function Escape_JSON (S : String) return String;

      function Escape_JSON (S : String) return String is

         package Latin_1 renames Ada.Characters.Latin_1;

         Result : Ada.Strings.Unbounded.Unbounded_String :=
           Ada.Strings.Unbounded.Null_Unbounded_String;
         function Hex (N : Natural) return Character;

         function Hex (N : Natural) return Character is
         begin
            if N < 10 then
               return Character'Val (Character'Pos ('0') + N);
            else
               return Character'Val (Character'Pos ('A') + (N - 10));
            end if;
         end Hex;

      begin
         for C of S loop
            case C is
               when '"' =>
                  Append (Result, "\""");

               when '\' =>
                  Append (Result, "\\");

               when Latin_1.BS =>
                  Append (Result, "\b");

               when Latin_1.HT =>
                  Append (Result, "\t");

               when Latin_1.LF =>
                  Append (Result, "\n");

               when Latin_1.FF =>
                  Append (Result, "\f");

               when Latin_1.CR =>
                  Append (Result, "\r");

               when Character'Val (0) .. Character'Val (7) |
                    Character'Val (14) .. Character'Val (31) =>
                  declare
                     V : constant Natural := Character'Pos (C);
                  begin
                     Append (Result, "\u00");
                     Append (Result, Hex (V / 16));
                     Append (Result, Hex (V mod 16));
                  end;

               when others =>
                  Append (Result, C);
            end case;
         end loop;

         return To_String (Result);
      end Escape_JSON;

   begin
      if Self.Needs_Comma then
         Append (Self.Text, ",");
      end if;
      Append (Self.Text, '"');
      Append (Self.Text, Escape_JSON (Value));
      Append (Self.Text, '"');
      Self.Needs_Comma := True;
   end Write_String;

   -----------------
   -- Write_Bytes --
   -----------------


   procedure Write_Bytes
     (Self : in out JSON_Writer;
      Value : PB_Support.Basics.Stream_Element_Vector)
   is

      function To_Base_64
      (Data : PB_Support.Basics.Stream_Element_Vector)
         return String;

      function To_Base_64
      (Data : PB_Support.Basics.Stream_Element_Vector)
         return String
      is
         Base64_Table : constant String :=
           "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

         Result : String (1 .. ((Natural (Data.Length) + 2) / 3) * 4);
         R      : Natural := 1;
         I      : Natural := Natural (Data.First_Index);
      begin
         while I <= Natural (Data.Last_Index) loop
            declare
               B1, B2, B3 : Natural := 0;
               Pad        : Natural := 0;
            begin
               B1 := Natural (Data.Element (I));
               if I + 1 <= Data.Last_Index then
                  B2 := Natural (Data.Element (I + 1));
               else
                  Pad := Pad + 1;
               end if;

               if I + 2 <= Data.Last_Index then
                  B3 := Natural (Data.Element (I + 2));
               else
                  Pad := Pad + 1;
               end if;

               declare
                  C1 : constant Natural :=  B1 / 4;
                  C2 : constant Natural := (B1 mod 4) * 16 + B2 / 16;
                  C3 : constant Natural := (B2 mod 16) * 4 + B3 / 64;
                  C4 : constant Natural :=  B3 mod 64;
               begin
                  Result (R) := Base64_Table (C1 + 1);
                  R := R + 1;
                  Result (R) := Base64_Table (C2 + 1);
                  R := R + 1;

                  if Pad < 2 then
                     Result (R) := Base64_Table (C3 + 1);
                  else
                     Result (R) := '=';
                  end if;
                  R := R + 1;

                  if Pad < 1 then
                     Result (R) := Base64_Table (C4 + 1);
                  else
                     Result (R) := '=';
                  end if;
                  R := R + 1;
               end;

               I := I + 3;
            end;
         end loop;

         return Result;
      end To_Base_64;
   begin
      Write_String (Self, To_Base_64 (Value));
   end Write_Bytes;

   -------------------
   -- Write_Integer --
   -------------------

   procedure Write_Integer
     (Self  : in out JSON_Writer;
      Value : Long_Long_Integer)
   is
   begin
      Append
           (Self.Text, Ada.Strings.Fixed.Trim (Value'Image, Ada.Strings.Left));
      Self.Needs_Comma := True;
   end Write_Integer;

   -------------------
   -- Write_Integer --
   -------------------

   procedure Write_Integer
      (Self  : in out JSON_Writer;
       Value : Interfaces.Integer_64)
   is
   begin
      --  64 bit types are quoted by default
      Write_String
        (Self, Ada.Strings.Fixed.Trim (Value'Image, Ada.Strings.Left));
   end Write_Integer;

   -------------------
   -- Write_Integer --
   -------------------

   procedure Write_Integer
      (Self  : in out JSON_Writer;
       Value : Interfaces.Unsigned_64)
   is
   begin
      --  64 bit types are quoted by default
      Write_String
        (Self, Ada.Strings.Fixed.Trim (Value'Image, Ada.Strings.Left));
   end Write_Integer;

   -----------------
   -- Write_Float --
   -----------------

   procedure Write_Float
     (Self : in out JSON_Writer;
      Value : Interfaces.IEEE_Float_64)
   is
      use type Interfaces.IEEE_Float_64;
   begin
      if Self.Needs_Comma then
         Append (Self.Text, ",");
      end if;

      Append
         (Self.Text, PB_Support.Common_JSON.Float_Image (Value));
      Self.Needs_Comma := True;
   end Write_Float;

   -------------------
   -- Write_Boolean --
   -------------------

   procedure Write_Boolean (Self : in out JSON_Writer; Value : Boolean) is
   begin
      if Self.Needs_Comma then
         Append (Self.Text, ",");
      end if;
      if Value then
         Append (Self.Text, "true");
      else
         Append (Self.Text, "false");
      end if;
      Self.Needs_Comma := True;
   end Write_Boolean;

   ----------------
   -- Write_Null --
   ----------------

   procedure Write_Null (Self : in out JSON_Writer) is
   begin
      if Self.Needs_Comma then
         Append (Self.Text, ",");
      end if;
      Append (Self.Text, "null");
      Self.Needs_Comma := True;
   end Write_Null;

   ---------------------
   -- Write_Timestamp --
   ---------------------

   procedure Write_Timestamp
     (Self    : in out JSON_Writer;
      Seconds : Interfaces.Integer_64;
      Nanos   : Interfaces.Integer_32)
   is
   begin
      Write_String
        (Self  => Self,
         Value => PB_Support.Common_JSON.Timestamp_Image (Seconds, Nanos));
   end Write_Timestamp;

   --------------------
   -- Write_Duration --
   --------------------

   procedure Write_Duration
     (Self    : in out JSON_Writer;
      Seconds : Interfaces.Integer_64;
      Nanos   : Interfaces.Integer_32)
   is
   begin
      Write_String
        (Self => Self,
         Value => PB_Support.Common_JSON.Duration_Image (Seconds, Nanos));
   end Write_Duration;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : JSON_Writer) return String is
   begin
      return To_String (Self.Text);
   end To_String;

end PB_Support.JSON;
