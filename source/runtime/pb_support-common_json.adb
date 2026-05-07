with Ada.Calendar.Formatting;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body PB_Support.Common_JSON is


   package IEEE_Float_64_Text_IO is new
     Ada.Text_IO.Float_IO (Interfaces.IEEE_Float_64);

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
    -- Timestamp_Image --
    ---------------------

    function Timestamp_Image
       (Seconds : Interfaces.Integer_64;
        Nanos   : Interfaces.Integer_32) return String
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
               Unix_Epoch
               + Duration (Seconds)
               + Duration (Nanos / 1_000_000_000);
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
            return Timestamp_String & "Z";
        end;
    end Timestamp_Image;

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
               with
                  "google.protobuf.Duration inconsistent seconds/nanos signs";
        end if;
    end Validate_Duration;

    --------------------
    -- Duration_Image --
    --------------------

    function Duration_Image
       (Seconds : Interfaces.Integer_64;
        Nanos   : Interfaces.Integer_32) return String
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
                          (Interfaces.Integer_32'Image (Nanos),
                           Ada.Strings.Left)
                  else "")
               & "s";
        begin
            return Duration_String;
        end;
    end Duration_Image;

   -----------------
   -- Float_Image --
   -----------------

   function Float_Image
     (Value : Interfaces.IEEE_Float_64)
      return String
   is
      use type Interfaces.IEEE_Float_64;
   begin

      --  Protobuf JSON requires NaN and +/-Infinity to be serialized as
      --  strings.
      --  NaN is detected portably via IEEE‑754 semantics: (X /= X).
      --  We cannot rely on 'Image or Text_IO output, which is
      --  implementation-defined.
      --  See: protobuf JSON mapping
      --       https://protobuf.dev/programming-guides/json/

      if Value /= Value then
         return "NaN";
      elsif Value > Interfaces.IEEE_Float_64'Last then
         return """Infinity""";
      elsif Value < Interfaces.IEEE_Float_64'First then
         return """-Infinity""";
      else
         declare
            Float_Image : String (1 .. 40);
         begin

            IEEE_Float_64_Text_IO.Put
              (To => Float_Image, Item => Value, Aft => 16, Exp => 3);

            return 
               Ada.Strings.Fixed.Trim (Float_Image, Side => Ada.Strings.Left);
         end;
      end if;
   end Float_Image;

end PB_Support.Common_JSON;