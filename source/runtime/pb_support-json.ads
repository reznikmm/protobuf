--  Runtime package for Protobuf JSON serialization and deserialization

with Ada.Strings.Unbounded;

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
   
   procedure Write_Integer
      (Self  : in out JSON_Writer;
       Value : Long_Long_Integer);
   procedure Write_Float (Self : in out JSON_Writer; Value : Long_Float);
   procedure Write_Boolean (Self : in out JSON_Writer; Value : Boolean);
   procedure Write_Null (Self : in out JSON_Writer);
   
   -- Extract written JSON
   function To_String (Self : JSON_Writer) return String;
   
private

   type JSON_Writer is tagged record
      Text        : Ada.Strings.Unbounded.Unbounded_String;
      Needs_Comma : Boolean := False;
   end record;

end PB_Support.JSON;
