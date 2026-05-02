--  Runtime package for Protobuf JSON serialization and deserialization

with Ada.Containers.Vectors;

with Interfaces;

with League.JSON.Arrays;
with League.JSON.Objects;
with League.JSON.Values;
with League.Stream_Element_Vectors;
with League.Strings;

package PB_Support.JSON is

   type Formatter_Options is record
      Preserve_Proto_Field_Names : Boolean := False;
      Always_Print_Enums_As_Ints : Boolean := False;
      Emit_Default_Values        : Boolean := False;
   end record;

   Default_Formatter_Options : constant Formatter_Options :=
     (Preserve_Proto_Field_Names => False,
      Always_Print_Enums_As_Ints => False,
      Emit_Default_Values        => False);

   type Parser_Options is record
      Ignore_Unknown_Fields : Boolean := False;
   end record;

   Default_Parser_Options : constant Parser_Options :=
     (Ignore_Unknown_Fields => False);

   --  Exceptions that can be raised during JSON processing
   JSON_Parse_Error : exception;
   JSON_Format_Error : exception;

   type JSON_Writer is tagged private;

   --  Write basic elements
   procedure Start_Object (Self : in out JSON_Writer);
   procedure End_Object (Self : in out JSON_Writer);
   procedure Start_Array (Self : in out JSON_Writer);
   procedure End_Array (Self : in out JSON_Writer);

   procedure Write_Key (Self : in out JSON_Writer; Name : String);
   procedure Write_String (Self : in out JSON_Writer; Value : String);
   procedure Write_Bytes
      (Self : in out JSON_Writer;
       Value : League.Stream_Element_Vectors.Stream_Element_Vector);

   procedure Write_Integer
      (Self  : in out JSON_Writer;
       Value : Long_Long_Integer);

   procedure Write_Integer
      (Self  : in out JSON_Writer;
       Value : Interfaces.Integer_64);

   procedure Write_Integer
      (Self  : in out JSON_Writer;
       Value : Interfaces.Unsigned_64);

   procedure Write_Float
     (Self  : in out JSON_Writer;
      Value : Interfaces.IEEE_Float_64);

   procedure Write_Boolean (Self : in out JSON_Writer; Value : Boolean);
   procedure Write_Null (Self : in out JSON_Writer);

    function To_Universal_String
       (Self : JSON_Writer) return League.Strings.Universal_String;
   --  Extract written JSON as a Universal_String

   function To_String (Self : JSON_Writer) return String;
   --  Extract written JSON as a String encoded in UTF-8

   procedure Write_Timestamp
     (Self    : in out JSON_Writer;
      Seconds : Interfaces.Integer_64;
      Nanos   : Interfaces.Integer_32);
   --  Write the Timestamp in the format required by Protobuf JSON output.
   --  Validate that the given seconds and nanos values are within the valid
   --  range for a Timestamp in Protobuf JSON format.
   --  Raises Constraint_Error if the values are out of range.

   procedure Write_Duration
     (Self    : in out JSON_Writer;
      Seconds : Interfaces.Integer_64;
      Nanos   : Interfaces.Integer_32);
   --  Write the duration in the format required by Protobuf JSON output.
   --  Validate that the given seconds and nanos values are within the valid
   --  range for a Duration in Protobuf JSON format.
   --  Raises Constraint_Error if the values are out of range.

private

   type Container_Kind is (Object_Container, Array_Container);

   type Container (Kind : Container_Kind := Object_Container) is record
      Pending_Key     : League.Strings.Universal_String;
      Has_Pending_Key : Boolean := False;
      case Kind is
         when Object_Container =>
            Object_Value    : League.JSON.Objects.JSON_Object;
         when Array_Container =>
            Array_Value : League.JSON.Arrays.JSON_Array;
      end case;
   end record;

   package Container_Vectors is new Ada.Containers.Vectors
     (Positive, Container);

   type JSON_Writer is tagged record
      Root_Value      : League.JSON.Values.JSON_Value;
      Has_Root        : Boolean := False;
      Stack           : Container_Vectors.Vector;
      Pending_Key     : League.Strings.Universal_String;
      Has_Pending_Key : Boolean := False;
   end record;

end PB_Support.JSON;
