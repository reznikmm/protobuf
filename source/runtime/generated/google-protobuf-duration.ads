with Ada.Finalization;
with Ada.Streams;
with Interfaces;

package Google.Protobuf.Duration is

   type Duration_Vector is tagged private;

   type Duration is
     record
        Seconds : Interfaces.Integer_64 := 0;
        Nanos   : Interfaces.Integer_32 := 0;
     end record;

   type Optional_Duration (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Duration.Duration;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Duration_Vector) return Natural;

   function Get (Self  : Duration_Vector; Index : Positive) return Duration;

   procedure Clear (Self : in out Duration_Vector);

   procedure Append (Self : in out Duration_Vector; V    : Duration);
private

   procedure Read_Duration
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Duration);

   procedure Write_Duration
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Duration);

   for Duration'Read use Read_Duration;

   for Duration'Write use Write_Duration;

   type Duration_Array is array (Positive range <>) of Duration;

   type Duration_Array_Access is access Duration_Array;

   type Duration_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Duration_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Duration_Vector);

   overriding procedure Finalize (Self : in out Duration_Vector);

end Google.Protobuf.Duration;