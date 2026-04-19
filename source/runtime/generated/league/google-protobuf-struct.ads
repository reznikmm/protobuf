with Ada.Finalization;
with Ada.Streams;
with Interfaces;
with League.Strings;
with PB_Support.Vectors;

package Google.Protobuf.Struct is

   type Null_Value is (PB_NULL_VALUE);

   for Null_Value use (PB_NULL_VALUE => 0);

   package Null_Value_Vectors is new PB_Support.Vectors (Null_Value);

   type Struct_Vector is tagged private
     with Variable_Indexing => Get_Struct_Variable_Reference,
     Constant_Indexing => Get_Struct_Constant_Reference;

   type Fields_Entry_Vector is tagged private
     with Variable_Indexing => Get_Fields_Entry_Variable_Reference,
     Constant_Indexing => Get_Fields_Entry_Constant_Reference;

   type Value_Vector is tagged private
     with Variable_Indexing => Get_Value_Variable_Reference,
     Constant_Indexing => Get_Value_Constant_Reference;

   type List_Value_Vector is tagged private
     with Variable_Indexing => Get_List_Value_Variable_Reference,
     Constant_Indexing => Get_List_Value_Constant_Reference;

   type Struct is
     record
        Fields : Google.Protobuf.Struct.Fields_Entry_Vector;
     end record;

   type Optional_Struct  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Struct.Struct;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Struct_Vector) return Natural;

   procedure Clear (Self : in out Struct_Vector);

   procedure Append (Self : in out Struct_Vector; V    : Struct);

   type Struct_Variable_Reference  (Element : not null access Struct) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Struct_Variable_Reference
    (Self  : aliased in out Struct_Vector;
     Index : Positive)
      return Struct_Variable_Reference
     with Inline;

   type Struct_Constant_Reference
     (Element : not null access constant Struct) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Struct_Constant_Reference
    (Self  : aliased Struct_Vector;
     Index : Positive)
      return Struct_Constant_Reference
     with Inline;

   type List_Value is
     record
        Values : Google.Protobuf.Struct.Value_Vector;
     end record;

   type Optional_List_Value  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Struct.List_Value;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : List_Value_Vector) return Natural;

   procedure Clear (Self : in out List_Value_Vector);

   procedure Append (Self : in out List_Value_Vector; V    : List_Value);

   type List_Value_Variable_Reference
     (Element : not null access List_Value) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_List_Value_Variable_Reference
    (Self  : aliased in out List_Value_Vector;
     Index : Positive)
      return List_Value_Variable_Reference
     with Inline;

   type List_Value_Constant_Reference
     (Element : not null access constant List_Value) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_List_Value_Constant_Reference
    (Self  : aliased List_Value_Vector;
     Index : Positive)
      return List_Value_Constant_Reference
     with Inline;

   type Value_Variant_Kind is
     (Kind_Not_Set,
      Null_Value_Kind,
      Number_Value_Kind,
      String_Value_Kind,
      Bool_Value_Kind,
      Struct_Value_Kind,
      List_Value_Kind);

   type Value_Variant  (Kind : Value_Variant_Kind := Kind_Not_Set) is
     record
        case Kind is
           when Kind_Not_Set =>
              null;
           when Null_Value_Kind =>
              Null_Value : Google.Protobuf.Struct.Null_Value :=
                Google.Protobuf.Struct.PB_NULL_VALUE;
           when Number_Value_Kind =>
              Number_Value : Interfaces.IEEE_Float_64 := 0.0;
           when String_Value_Kind =>
              String_Value : League.Strings.Universal_String;
           when Bool_Value_Kind =>
              Bool_Value : Boolean := False;
           when Struct_Value_Kind =>
              Struct_Value : Google.Protobuf.Struct.Struct;
           when List_Value_Kind =>
              List_Value : Google.Protobuf.Struct.List_Value;
        end case;
     end record;

   type Value is record Variant : Value_Variant; end record;

   type Optional_Value  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Struct.Value;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Value_Vector) return Natural;

   procedure Clear (Self : in out Value_Vector);

   procedure Append (Self : in out Value_Vector; V    : Value);

   type Value_Variable_Reference  (Element : not null access Value) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Value_Variable_Reference
    (Self  : aliased in out Value_Vector;
     Index : Positive)
      return Value_Variable_Reference
     with Inline;

   type Value_Constant_Reference  (Element : not null access constant Value) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Value_Constant_Reference
    (Self  : aliased Value_Vector;
     Index : Positive)
      return Value_Constant_Reference
     with Inline;

   type Fields_Entry is
     record
        Key   : League.Strings.Universal_String;
        Value : Google.Protobuf.Struct.Optional_Value;
     end record;

   type Optional_Fields_Entry  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Struct.Fields_Entry;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Fields_Entry_Vector) return Natural;

   procedure Clear (Self : in out Fields_Entry_Vector);

   procedure Append (Self : in out Fields_Entry_Vector; V    : Fields_Entry);

   type Fields_Entry_Variable_Reference
     (Element : not null access Fields_Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Fields_Entry_Variable_Reference
    (Self  : aliased in out Fields_Entry_Vector;
     Index : Positive)
      return Fields_Entry_Variable_Reference
     with Inline;

   type Fields_Entry_Constant_Reference
     (Element : not null access constant Fields_Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Fields_Entry_Constant_Reference
    (Self  : aliased Fields_Entry_Vector;
     Index : Positive)
      return Fields_Entry_Constant_Reference
     with Inline;
private

   procedure Read_Fields_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Fields_Entry);

   procedure Write_Fields_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Fields_Entry);

   for Fields_Entry'Read use Read_Fields_Entry;

   for Fields_Entry'Write use Write_Fields_Entry;

   type Fields_Entry_Array is
     array (Positive range <>) of aliased Fields_Entry;

   type Fields_Entry_Array_Access is access Fields_Entry_Array;

   type Fields_Entry_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Fields_Entry_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Fields_Entry_Vector);

   overriding procedure Finalize (Self : in out Fields_Entry_Vector);

   procedure Read_Struct
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Struct);

   procedure Write_Struct
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Struct);

   for Struct'Read use Read_Struct;

   for Struct'Write use Write_Struct;

   type Struct_Array is array (Positive range <>) of aliased Struct;

   type Struct_Array_Access is access Struct_Array;

   type Struct_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Struct_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Struct_Vector);

   overriding procedure Finalize (Self : in out Struct_Vector);

   procedure Read_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Value);

   procedure Write_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Value);

   for Value'Read use Read_Value;

   for Value'Write use Write_Value;

   type Value_Array is array (Positive range <>) of aliased Value;

   type Value_Array_Access is access Value_Array;

   type Value_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Value_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Value_Vector);

   overriding procedure Finalize (Self : in out Value_Vector);

   procedure Read_List_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out List_Value);

   procedure Write_List_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : List_Value);

   for List_Value'Read use Read_List_Value;

   for List_Value'Write use Write_List_Value;

   type List_Value_Array is array (Positive range <>) of aliased List_Value;

   type List_Value_Array_Access is access List_Value_Array;

   type List_Value_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : List_Value_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out List_Value_Vector);

   overriding procedure Finalize (Self : in out List_Value_Vector);

end Google.Protobuf.Struct;