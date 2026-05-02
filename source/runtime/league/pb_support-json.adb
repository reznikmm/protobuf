with Ada.Calendar.Formatting;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with League.Base_Codecs;
with League.Holders;
with League.JSON.Documents;

package body PB_Support.JSON is

   function "+" (Text : String)
      return League.Strings.Universal_String
            renames League.Strings.From_UTF_8_String;

   package IEEE_Float_64_Text_IO is new Ada.Text_IO.Float_IO
      (Interfaces.IEEE_Float_64);

   procedure Push_Value
     (Self  : in out JSON_Writer;
      Value : League.JSON.Values.JSON_Value);

   ----------------
   -- Push_Value --
   ----------------

   procedure Push_Value
     (Self  : in out JSON_Writer;
      Value : League.JSON.Values.JSON_Value)
   is
   begin
      if Self.Stack.Is_Empty then
         if Self.Has_Root then
            raise JSON_Format_Error with "Multiple top-level JSON values";
         end if;

         Self.Root_Value := Value;
         Self.Has_Root := True;
         return;
      end if;

      declare
         Parent : Container := Self.Stack.Last_Element;
      begin
         case Parent.Kind is
            when Object_Container =>
               if not Self.Has_Pending_Key then
                  raise JSON_Format_Error with "JSON object key is missing";
               end if;

               Parent.Object_Value.Insert (Self.Pending_Key, Value);
               Self.Has_Pending_Key := False;
               Self.Pending_Key := League.Strings.Empty_Universal_String;

            when Array_Container =>
               Parent.Array_Value.Append (Value);
         end case;

         Self.Stack.Replace_Element (Self.Stack.Last_Index, Parent);
      end;
   end Push_Value;

   ------------------
   -- Start_Object --
   ------------------

   procedure Start_Object (Self : in out JSON_Writer) is
      Child : Container :=
        (Kind            => Object_Container,
         Object_Value    => League.JSON.Objects.Empty_JSON_Object,
         Pending_Key     => League.Strings.Empty_Universal_String,
         Has_Pending_Key => False);
   begin
      if Self.Has_Pending_Key then
         Child.Pending_Key := Self.Pending_Key;
         Child.Has_Pending_Key := True;
         Self.Pending_Key := League.Strings.Empty_Universal_String;
         Self.Has_Pending_Key := False;
      end if;

      Self.Stack.Append (New_Item => Child);
   end Start_Object;

   ----------------
   -- End_Object --
   ----------------

   procedure End_Object (Self : in out JSON_Writer) is
   begin
      if Self.Stack.Is_Empty or else
         Self.Stack.Last_Element.Kind /= Object_Container
      then
         raise JSON_Format_Error with "Mismatched JSON object end";
      end if;

      declare
         Child : constant Container := Self.Stack.Last_Element;
         Value : constant League.JSON.Values.JSON_Value :=
           Child.Object_Value.To_JSON_Value;
      begin
         Self.Stack.Delete_Last;
         if Child.Has_Pending_Key then
            Self.Pending_Key := Child.Pending_Key;
            Self.Has_Pending_Key := True;
         else
            Self.Pending_Key := League.Strings.Empty_Universal_String;
            Self.Has_Pending_Key := False;
         end if;

         Push_Value (Self, Value);
      end;
   end End_Object;

   -----------------
   -- Start_Array --
   -----------------

   procedure Start_Array (Self : in out JSON_Writer) is
      Child : Container :=
        (Kind => Array_Container,
         Array_Value => League.JSON.Arrays.Empty_JSON_Array,
         Has_Pending_Key => Self.Has_Pending_Key,
         Pending_Key => Self.Pending_Key);
   begin
      if Self.Has_Pending_Key then
         Self.Pending_Key := League.Strings.Empty_Universal_String;
         Self.Has_Pending_Key := False;
      end if;

      Self.Stack.Append (New_Item => Child);
   end Start_Array;

   ---------------
   -- End_Array --
   ---------------

   procedure End_Array (Self : in out JSON_Writer) is
   begin
      if Self.Stack.Is_Empty or else
         Self.Stack.Last_Element.Kind /= Array_Container
      then
         raise JSON_Format_Error with "Mismatched JSON array end";
      end if;

      declare
         Child : constant Container := Self.Stack.Last_Element;
         Value : constant League.JSON.Values.JSON_Value :=
           Child.Array_Value.To_JSON_Value;
      begin
         Self.Stack.Delete_Last;
         if Child.Has_Pending_Key then
            Self.Pending_Key := Child.Pending_Key;
            Self.Has_Pending_Key := True;
         else
            Self.Pending_Key := League.Strings.Empty_Universal_String;
            Self.Has_Pending_Key := False;
         end if;

         Push_Value (Self, Value);
      end;
   end End_Array;

   ---------------
   -- Write_Key --
   ---------------

   procedure Write_Key (Self : in out JSON_Writer; Name : String) is
   begin
      if Self.Stack.Is_Empty or else
         Self.Stack.Last_Element.Kind /= Object_Container
      then
         raise JSON_Format_Error with "JSON key outside object";
      end if;

      Self.Pending_Key := +Name;
      Self.Has_Pending_Key := True;
   end Write_Key;

   -------------------------
   -- To_Universal_String --
   -------------------------

   function To_Universal_String
     (Self : JSON_Writer) return League.Strings.Universal_String is
      Doc : League.JSON.Documents.JSON_Document;
   begin
      if not Self.Stack.Is_Empty then
         raise JSON_Format_Error with "JSON document is not closed";
      end if;

      if not Self.Has_Root then
         return League.Strings.Empty_Universal_String;
      end if;

      if Self.Root_Value.Is_Object then
         Doc.Set_Object (Self.Root_Value.To_Object);
      elsif Self.Root_Value.Is_Array then
         Doc.Set_Array (Self.Root_Value.To_Array);
      else
         raise JSON_Format_Error
           with "Top-level JSON value must be object or array";
      end if;

      return Doc.To_JSON;
   end To_Universal_String;

   ------------------
   -- Write_String --
   ------------------

   procedure Write_String (Self : in out JSON_Writer; Value : String) is
   begin
      --  TODO: do it in a proper typed way.
      if Value = "PB_NULL_VALUE" then
         Push_Value (Self, League.JSON.Values.Null_JSON_Value);
      else
         Push_Value (Self, League.JSON.Values.To_JSON_Value (+Value));
      end if;
   end Write_String;

   -----------------
   -- Write_Bytes --
   -----------------

   procedure Write_Bytes
     (Self  : in out JSON_Writer;
      Value : League.Stream_Element_Vectors.Stream_Element_Vector)
   is
      Encoded : constant League.Strings.Universal_String :=
        League.Base_Codecs.To_Base_64 (Value);
   begin
      Push_Value (Self, League.JSON.Values.To_JSON_Value (Encoded));
   end Write_Bytes;

   -------------------
   -- Write_Integer --
   -------------------

   procedure Write_Integer
      (Self  : in out JSON_Writer;
       Value : Long_Long_Integer) is
   begin
      Push_Value
        (Self,
         League.JSON.Values.To_JSON_Value
           (League.Holders.Universal_Integer (Value)));
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
      --  Protobuf JSON requires NaN and +/-Infinity to be serialized as strings.
      --  NaN is detected portably via IEEE‑754 semantics: (X /= X).
      --  We cannot rely on 'Image or Text_IO output, which is
      --  implementation-defined.
      --  See: protobuf JSON mapping
      --       https://protobuf.dev/programming-guides/json/

      if Value /= Value then
         Push_Value (Self, League.JSON.Values.To_JSON_Value
                      (League.Strings.To_Universal_String("NaN")));
      elsif Value > Interfaces.IEEE_Float_64'Last then
         Push_Value (Self, League.JSON.Values.To_JSON_Value
                      (League.Strings.To_Universal_String("Infinity")));
      elsif Value < Interfaces.IEEE_Float_64'First then
         Push_Value (Self, League.JSON.Values.To_JSON_Value
                      (League.Strings.To_Universal_String("-Infinity")));
      else
         declare
            Float_Image : String (1 .. 40);
         begin

            IEEE_Float_64_Text_IO.Put
              (To => Float_Image, Item => Value, Aft => 16, Exp => 3);

            Push_Value
              (Self,
               League.JSON.Values.To_JSON_Value
                 (League.Strings.From_UTF_8_String
                    (Ada.Strings.Fixed.Trim (Float_Image,
                                             Side => Ada.Strings.Left))));
         end;
      end if;
   end Write_Float;

   -------------------
   -- Write_Boolean --
   -------------------

   procedure Write_Boolean (Self : in out JSON_Writer; Value : Boolean) is
   begin
      Push_Value (Self, League.JSON.Values.To_JSON_Value (Value));
   end Write_Boolean;

   ----------------
   -- Write_Null --
   ----------------

   procedure Write_Null (Self : in out JSON_Writer) is
   begin
      Push_Value (Self, League.JSON.Values.Null_JSON_Value);
   end Write_Null;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : JSON_Writer) return String is
   begin
      return To_Universal_String (Self).To_UTF_8_String;
   end To_String;

   ------------------------
   -- Validate_Timestamp --
   ------------------------

   procedure Validate_Timestamp
     (Seconds : Interfaces.Integer_64; Nanos : Interfaces.Integer_32)
   is
      use type Interfaces.Integer_64;
      use type Interfaces.Integer_32;
   begin
      -- seconds range: 0001-01-01T00:00:00Z .. 9999-12-31T23:59:59Z
      if Seconds not in -62135596800 .. 253402300799 then
         raise Constraint_Error
           with "google.protobuf.Timestamp.seconds out of range";
      end if;

      -- nanos range: [0 .. 999_999_999]
      if Nanos not in 0 .. 999_999_999 then
         raise Constraint_Error
           with "google.protobuf.Timestamp.nanos out of range";
      end if;
   end Validate_Timestamp;

   ---------------------
   -- Write_Timestamp --
   ---------------------

   procedure Write_Timestamp
     (Self    : in out JSON_Writer;
      Seconds : Interfaces.Integer_64;
      Nanos   : Interfaces.Integer_32)
   is
   begin
      Validate_Timestamp (Seconds, Nanos);

      declare
         use type Ada.Calendar.Time;
         use type Interfaces.Integer_64;
         use type Interfaces.Integer_32;
         --  Represents seconds of UTC time since Unix epoch
         --  1970-01-01T00:00:00Z.
         Unix_Epoch       : constant Ada.Calendar.Time :=
           Ada.Calendar.Time_Of
             (Year => 1970, Month => 1, Day => 1, Seconds => 0.0);
         Timestamp_Time   : constant Ada.Calendar.Time :=
           Unix_Epoch + Duration (Seconds) + Duration (Nanos / 1_000_000_000);
         --  Ada format is "YYYY-MM-DD HH:MM:SS.ss"
         Timestamp_String : String :=
           Ada.Calendar.Formatting.Image
             (Date => Timestamp_Time, Include_Time_Fraction => True);
         Space_Index      : constant := 10;
      begin
         --  Protobuf JSON requires "T" separator and "Z" suffix for UTC time.
         --  > Uses RFC 3339 (see clarification). Generated output will always
         --  > be Z-normalized with 0, 3, 6 or 9 fractional digits. Offsets
         --  > other than "Z" are also accepted.

         Timestamp_String (Space_Index) := 'T';
         Write_String (Self => Self, Value => Timestamp_String & "Z");
      end;
   end Write_Timestamp;

   -----------------------
   -- Validate_Duration --
   -----------------------

   procedure Validate_Duration
     (Seconds : Interfaces.Integer_64; Nanos : Interfaces.Integer_32)
   is
      use type Interfaces.Integer_64;
      use type Interfaces.Integer_32;
   begin
      -- Seconds must be within +/-315,576,000,000 (about +/-10,000 years)
      if Seconds not in -315_576_000_000 .. 315_576_000_000 then
         raise Constraint_Error
           with "google.protobuf.Duration.seconds out of range";
      end if;

      -- Nanos must be within +/-999,999,999
      if Nanos not in -999_999_999 .. 999_999_999 then
         raise Constraint_Error
           with "google.protobuf.Duration.nanos out of range";
      end if;

      -- Sign coherence between seconds and nanos
      if (Seconds > 0 and then Nanos < 0)
        or else (Seconds < 0 and then Nanos > 0)
      then
         raise Constraint_Error
           with "google.protobuf.Duration inconsistent seconds/nanos signs";
      end if;
   end Validate_Duration;

   --------------------
   -- Write_Duration --
   --------------------

   procedure Write_Duration
     (Self    : in out JSON_Writer;
      Seconds : Interfaces.Integer_64;
      Nanos   : Interfaces.Integer_32)
   is
   begin
      Validate_Duration (Seconds, Nanos);

      declare
         use type Interfaces.Integer_64;
         use type Interfaces.Integer_32;

         --  Protobuf JSON requires the duration to be a string in the format
         --  of "-?([0-9]+)\\.(\\d{3,9})?s", where the seconds and fractional
         --  seconds are separated by a decimal point and suffixed with "s".
         --  The fractional part is optional and must have between 3 and 9 digits
         --  if present. The duration must always include at least seconds or
         --  nanos (i.e. it cannot be zero).
         Seconds_String  : String :=
           Ada.Strings.Fixed.Trim
             (Interfaces.Integer_64'Image (Seconds), Ada.Strings.Left);
         Duration_String : String :=
           Seconds_String
           & (if Nanos /= 0
              then
                "."
                & Ada.Strings.Fixed.Trim
                    (Interfaces.Integer_32'Image (Nanos), Ada.Strings.Left)
              else "")
           & "s";
      begin
         Write_String (Self => Self, Value => Duration_String);
      end;
   end Write_Duration;

end PB_Support.JSON;