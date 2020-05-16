with Ada.Finalization;
with Ada.Streams;
with Interfaces;

package Google.Protobuf.Timestamp is

   type Timestamp_Vector is tagged private;

   type Timestamp is
     record
        Seconds : Interfaces.Integer_64 := 0;
        Nanos   : Interfaces.Integer_32 := 0;
     end record;

   type Optional_Timestamp (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Timestamp.Timestamp;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Timestamp_Vector) return Natural;

   function Get (Self  : Timestamp_Vector; Index : Positive) return Timestamp;

   procedure Clear (Self : in out Timestamp_Vector);

   procedure Append (Self : in out Timestamp_Vector; V    : Timestamp);
private

   procedure Read_Timestamp
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Timestamp);

   procedure Write_Timestamp
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Timestamp);

   for Timestamp'Read use Read_Timestamp;

   for Timestamp'Write use Write_Timestamp;

   type Timestamp_Array is array (Positive range <>) of Timestamp;

   type Timestamp_Array_Access is access Timestamp_Array;

   type Timestamp_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Timestamp_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Timestamp_Vector);

   overriding procedure Finalize (Self : in out Timestamp_Vector);

end Google.Protobuf.Timestamp;