with Ada.Finalization;
with Ada.Streams;
with Interfaces;

package Google.Protobuf.Duration is

   type Duration_Vector is tagged private
     with Variable_Indexing => Get_Duration_Variable_Reference,
     Constant_Indexing => Get_Duration_Constant_Reference;

   type Duration is
     record
        Seconds : Interfaces.Integer_64 := 0;
        Nanos   : Interfaces.Integer_32 := 0;
     end record;

   type Optional_Duration  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Duration.Duration;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Duration_Vector) return Natural;

   procedure Clear (Self : in out Duration_Vector);

   procedure Append (Self : in out Duration_Vector; V    : Duration);

   type Duration_Variable_Reference  (Element : not null access Duration) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Duration_Variable_Reference
    (Self  : aliased in out Duration_Vector;
     Index : Positive)
      return Duration_Variable_Reference
     with Inline;

   type Duration_Constant_Reference
     (Element : not null access constant Duration) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Duration_Constant_Reference
    (Self  : aliased Duration_Vector;
     Index : Positive)
      return Duration_Constant_Reference
     with Inline;
private

   procedure Read_Duration
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Duration);

   procedure Write_Duration
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Duration);

   for Duration'Read use Read_Duration;

   for Duration'Write use Write_Duration;

   type Duration_Array is array (Positive range <>) of aliased Duration;

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