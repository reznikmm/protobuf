with Ada.Finalization;
with Ada.Streams;

package Google.Protobuf.Empty is

   type Empty_Vector is tagged private
     with Variable_Indexing => Get_Empty_Variable_Reference,
     Constant_Indexing => Get_Empty_Constant_Reference;

   type Empty is null record;

   type Optional_Empty  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Empty.Empty;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Empty_Vector) return Natural;

   procedure Clear (Self : in out Empty_Vector);

   procedure Append (Self : in out Empty_Vector; V    : Empty);

   type Empty_Variable_Reference  (Element : not null access Empty) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Empty_Variable_Reference
    (Self  : aliased in out Empty_Vector;
     Index : Positive)
      return Empty_Variable_Reference
     with Inline;

   type Empty_Constant_Reference  (Element : not null access constant Empty) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Empty_Constant_Reference
    (Self  : aliased Empty_Vector;
     Index : Positive)
      return Empty_Constant_Reference
     with Inline;
private

   procedure Read_Empty
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Empty);

   procedure Write_Empty
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Empty);

   for Empty'Read use Read_Empty;

   for Empty'Write use Write_Empty;

   type Empty_Array is array (Positive range <>) of aliased Empty;

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