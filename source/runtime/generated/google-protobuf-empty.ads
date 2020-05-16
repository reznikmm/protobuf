with Ada.Finalization;
with Ada.Streams;

package Google.Protobuf.Empty is

   type Empty_Vector is tagged private;

   type Empty is null record;

   type Optional_Empty (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Empty.Empty;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Empty_Vector) return Natural;

   function Get (Self  : Empty_Vector; Index : Positive) return Empty;

   procedure Clear (Self : in out Empty_Vector);

   procedure Append (Self : in out Empty_Vector; V    : Empty);
private

   procedure Read_Empty
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Empty);

   procedure Write_Empty
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Empty);

   for Empty'Read use Read_Empty;

   for Empty'Write use Write_Empty;

   type Empty_Array is array (Positive range <>) of Empty;

   type Empty_Array_Access is access Empty_Array;

   type Empty_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Empty_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Empty_Vector);

   overriding procedure Finalize (Self : in out Empty_Vector);

end Google.Protobuf.Empty;