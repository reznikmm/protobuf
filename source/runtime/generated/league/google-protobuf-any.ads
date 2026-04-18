with Ada.Finalization;
with Ada.Streams;
with League.Stream_Element_Vectors;
with League.Strings;

package Google.Protobuf.Any is

   type Any_Vector is tagged private
     with Variable_Indexing => Get_Any_Variable_Reference,
     Constant_Indexing => Get_Any_Constant_Reference;

   type Any is
     record
        Type_Url : League.Strings.Universal_String;
        Value    : League.Stream_Element_Vectors.Stream_Element_Vector;
     end record;

   type Optional_Any  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Any.Any;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Any_Vector) return Natural;

   procedure Clear (Self : in out Any_Vector);

   procedure Append (Self : in out Any_Vector; V    : Any);

   type Any_Variable_Reference  (Element : not null access Any) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Any_Variable_Reference
    (Self  : aliased in out Any_Vector;
     Index : Positive)
      return Any_Variable_Reference
     with Inline;

   type Any_Constant_Reference  (Element : not null access constant Any) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Any_Constant_Reference
    (Self  : aliased Any_Vector;
     Index : Positive)
      return Any_Constant_Reference
     with Inline;
private

   procedure Read_Any
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Any);

   procedure Write_Any
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Any);

   for Any'Read use Read_Any;

   for Any'Write use Write_Any;

   type Any_Array is array (Positive range <>) of aliased Any;

   type Any_Array_Access is access Any_Array;

   type Any_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Any_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Any_Vector);

   overriding procedure Finalize (Self : in out Any_Vector);

end Google.Protobuf.Any;