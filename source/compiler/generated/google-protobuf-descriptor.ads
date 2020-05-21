with Ada.Finalization;
with Ada.Streams;
with League.String_Vectors;
with League.Strings;
with PB_Support.Boolean_Vectors;
with PB_Support.IEEE_Float_64_Vectors;
with PB_Support.Integer_32_Vectors;
with PB_Support.Integer_64_Vectors;
with PB_Support.Stream_Element_Vector_Vectors;
with PB_Support.Universal_String_Vectors;
with PB_Support.Unsigned_64_Vectors;
with PB_Support.Vectors;

package Google.Protobuf.Descriptor is

   type PB_Type is
     (TYPE_DOUBLE, TYPE_FLOAT, TYPE_INT64, TYPE_UINT64, TYPE_INT32,
      TYPE_FIXED64, TYPE_FIXED32, TYPE_BOOL, TYPE_STRING, TYPE_GROUP,
      TYPE_MESSAGE, TYPE_BYTES, TYPE_UINT32, TYPE_ENUM, TYPE_SFIXED32,
      TYPE_SFIXED64, TYPE_SINT32, TYPE_SINT64);

   for PB_Type use
     (TYPE_DOUBLE   => 1, TYPE_FLOAT    => 2, TYPE_INT64    => 3,
      TYPE_UINT64   => 4, TYPE_INT32    => 5, TYPE_FIXED64  => 6,
      TYPE_FIXED32  => 7, TYPE_BOOL     => 8, TYPE_STRING   => 9,
      TYPE_GROUP    => 10, TYPE_MESSAGE  => 11, TYPE_BYTES    => 12,
      TYPE_UINT32   => 13, TYPE_ENUM     => 14, TYPE_SFIXED32 => 15,
      TYPE_SFIXED64 => 16, TYPE_SINT32   => 17, TYPE_SINT64   => 18);

   package PB_Type_Vectors is new PB_Support.Vectors (PB_Type);

   type Label is (LABEL_OPTIONAL, LABEL_REQUIRED, LABEL_REPEATED);

   for Label use
     (LABEL_OPTIONAL => 1, LABEL_REQUIRED => 2, LABEL_REPEATED => 3);

   package Label_Vectors is new PB_Support.Vectors (Label);

   type Optimize_Mode is (SPEED, CODE_SIZE, LITE_RUNTIME);

   for Optimize_Mode use
     (SPEED        => 1, CODE_SIZE    => 2, LITE_RUNTIME => 3);

   package Optimize_Mode_Vectors is new PB_Support.Vectors (Optimize_Mode);

   type CType is (STRING, CORD, STRING_PIECE);

   for CType use (STRING       => 0, CORD         => 1, STRING_PIECE => 2);

   package CType_Vectors is new PB_Support.Vectors (CType);

   type JSType is (JS_NORMAL, JS_STRING, JS_NUMBER);

   for JSType use (JS_NORMAL => 0, JS_STRING => 1, JS_NUMBER => 2);

   package JSType_Vectors is new PB_Support.Vectors (JSType);

   type File_Descriptor_Set_Vector is tagged private
     with Variable_Indexing => Get_File_Descriptor_Set_Variable_Reference,
     Constant_Indexing => Get_File_Descriptor_Set_Constant_Reference;

   type File_Descriptor_Proto_Vector is tagged private
     with Variable_Indexing => Get_File_Descriptor_Proto_Variable_Reference,
     Constant_Indexing => Get_File_Descriptor_Proto_Constant_Reference;

   type Descriptor_Proto_Vector is tagged private
     with Variable_Indexing => Get_Descriptor_Proto_Variable_Reference,
     Constant_Indexing => Get_Descriptor_Proto_Constant_Reference;

   type Extension_Range_Vector is tagged private
     with Variable_Indexing => Get_Extension_Range_Variable_Reference,
     Constant_Indexing => Get_Extension_Range_Constant_Reference;

   type Reserved_Range_Vector is tagged private
     with Variable_Indexing => Get_Reserved_Range_Variable_Reference,
     Constant_Indexing => Get_Reserved_Range_Constant_Reference;

   type Field_Descriptor_Proto_Vector is tagged private
     with Variable_Indexing => Get_Field_Descriptor_Proto_Variable_Reference,
     Constant_Indexing => Get_Field_Descriptor_Proto_Constant_Reference;

   type Oneof_Descriptor_Proto_Vector is tagged private
     with Variable_Indexing => Get_Oneof_Descriptor_Proto_Variable_Reference,
     Constant_Indexing => Get_Oneof_Descriptor_Proto_Constant_Reference;

   type Enum_Descriptor_Proto_Vector is tagged private
     with Variable_Indexing => Get_Enum_Descriptor_Proto_Variable_Reference,
     Constant_Indexing => Get_Enum_Descriptor_Proto_Constant_Reference;

   type Enum_Value_Descriptor_Proto_Vector is tagged private
     with Variable_Indexing =>
       Get_Enum_Value_Descriptor_Proto_Variable_Reference,
     Constant_Indexing => Get_Enum_Value_Descriptor_Proto_Constant_Reference;

   type Service_Descriptor_Proto_Vector is tagged private
     with Variable_Indexing => Get_Service_Descriptor_Proto_Variable_Reference,
     Constant_Indexing => Get_Service_Descriptor_Proto_Constant_Reference;

   type Method_Descriptor_Proto_Vector is tagged private
     with Variable_Indexing => Get_Method_Descriptor_Proto_Variable_Reference,
     Constant_Indexing => Get_Method_Descriptor_Proto_Constant_Reference;

   type File_Options_Vector is tagged private
     with Variable_Indexing => Get_File_Options_Variable_Reference,
     Constant_Indexing => Get_File_Options_Constant_Reference;

   type Message_Options_Vector is tagged private
     with Variable_Indexing => Get_Message_Options_Variable_Reference,
     Constant_Indexing => Get_Message_Options_Constant_Reference;

   type Field_Options_Vector is tagged private
     with Variable_Indexing => Get_Field_Options_Variable_Reference,
     Constant_Indexing => Get_Field_Options_Constant_Reference;

   type Oneof_Options_Vector is tagged private
     with Variable_Indexing => Get_Oneof_Options_Variable_Reference,
     Constant_Indexing => Get_Oneof_Options_Constant_Reference;

   type Enum_Options_Vector is tagged private
     with Variable_Indexing => Get_Enum_Options_Variable_Reference,
     Constant_Indexing => Get_Enum_Options_Constant_Reference;

   type Enum_Value_Options_Vector is tagged private
     with Variable_Indexing => Get_Enum_Value_Options_Variable_Reference,
     Constant_Indexing => Get_Enum_Value_Options_Constant_Reference;

   type Service_Options_Vector is tagged private
     with Variable_Indexing => Get_Service_Options_Variable_Reference,
     Constant_Indexing => Get_Service_Options_Constant_Reference;

   type Method_Options_Vector is tagged private
     with Variable_Indexing => Get_Method_Options_Variable_Reference,
     Constant_Indexing => Get_Method_Options_Constant_Reference;

   type Uninterpreted_Option_Vector is tagged private
     with Variable_Indexing => Get_Uninterpreted_Option_Variable_Reference,
     Constant_Indexing => Get_Uninterpreted_Option_Constant_Reference;

   type Name_Part_Vector is tagged private
     with Variable_Indexing => Get_Name_Part_Variable_Reference,
     Constant_Indexing => Get_Name_Part_Constant_Reference;

   type Source_Code_Info_Vector is tagged private
     with Variable_Indexing => Get_Source_Code_Info_Variable_Reference,
     Constant_Indexing => Get_Source_Code_Info_Constant_Reference;

   type Location_Vector is tagged private
     with Variable_Indexing => Get_Location_Variable_Reference,
     Constant_Indexing => Get_Location_Constant_Reference;

   type Generated_Code_Info_Vector is tagged private
     with Variable_Indexing => Get_Generated_Code_Info_Variable_Reference,
     Constant_Indexing => Get_Generated_Code_Info_Constant_Reference;

   type Annotation_Vector is tagged private
     with Variable_Indexing => Get_Annotation_Variable_Reference,
     Constant_Indexing => Get_Annotation_Constant_Reference;

   type File_Descriptor_Set is
     record
        File : Google.Protobuf.Descriptor.File_Descriptor_Proto_Vector;
     end record;

   type Optional_File_Descriptor_Set  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.File_Descriptor_Set;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : File_Descriptor_Set_Vector) return Natural;

   procedure Clear (Self : in out File_Descriptor_Set_Vector);

   procedure Append
    (Self : in out File_Descriptor_Set_Vector;
     V    : File_Descriptor_Set);

   type File_Descriptor_Set_Variable_Reference
     (Element : not null access File_Descriptor_Set) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_File_Descriptor_Set_Variable_Reference
    (Self  : aliased in out File_Descriptor_Set_Vector;
     Index : Positive)
      return File_Descriptor_Set_Variable_Reference
     with Inline;

   type File_Descriptor_Set_Constant_Reference
     (Element : not null access constant File_Descriptor_Set) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_File_Descriptor_Set_Constant_Reference
    (Self  : aliased File_Descriptor_Set_Vector;
     Index : Positive)
      return File_Descriptor_Set_Constant_Reference
     with Inline;

   type Extension_Range is
     record
        Start  : PB_Support.Integer_32_Vectors.Option;
        PB_End : PB_Support.Integer_32_Vectors.Option;
     end record;

   type Optional_Extension_Range  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Extension_Range;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Extension_Range_Vector) return Natural;

   procedure Clear (Self : in out Extension_Range_Vector);

   procedure Append
    (Self : in out Extension_Range_Vector;
     V    : Extension_Range);

   type Extension_Range_Variable_Reference
     (Element : not null access Extension_Range) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Extension_Range_Variable_Reference
    (Self  : aliased in out Extension_Range_Vector;
     Index : Positive)
      return Extension_Range_Variable_Reference
     with Inline;

   type Extension_Range_Constant_Reference
     (Element : not null access constant Extension_Range) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Extension_Range_Constant_Reference
    (Self  : aliased Extension_Range_Vector;
     Index : Positive)
      return Extension_Range_Constant_Reference
     with Inline;

   type Reserved_Range is
     record
        Start  : PB_Support.Integer_32_Vectors.Option;
        PB_End : PB_Support.Integer_32_Vectors.Option;
     end record;

   type Optional_Reserved_Range  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Reserved_Range;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Reserved_Range_Vector) return Natural;

   procedure Clear (Self : in out Reserved_Range_Vector);

   procedure Append
    (Self : in out Reserved_Range_Vector;
     V    : Reserved_Range);

   type Reserved_Range_Variable_Reference
     (Element : not null access Reserved_Range) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Reserved_Range_Variable_Reference
    (Self  : aliased in out Reserved_Range_Vector;
     Index : Positive)
      return Reserved_Range_Variable_Reference
     with Inline;

   type Reserved_Range_Constant_Reference
     (Element : not null access constant Reserved_Range) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Reserved_Range_Constant_Reference
    (Self  : aliased Reserved_Range_Vector;
     Index : Positive)
      return Reserved_Range_Constant_Reference
     with Inline;

   type File_Options is
     record
        Java_Package                  : PB_Support.Universal_String_Vectors
          .Option;
        Java_Outer_Classname          : PB_Support.Universal_String_Vectors
          .Option;
        Java_Multiple_Files           : PB_Support.Boolean_Vectors.Option;
        Java_Generate_Equals_And_Hash : PB_Support.Boolean_Vectors.Option;
        Java_String_Check_Utf_8       : PB_Support.Boolean_Vectors.Option;
        Optimize_For                  : Google.Protobuf.Descriptor
          .Optimize_Mode_Vectors.Option;
        Go_Package                    : PB_Support.Universal_String_Vectors
          .Option;
        Cc_Generic_Services           : PB_Support.Boolean_Vectors.Option;
        Java_Generic_Services         : PB_Support.Boolean_Vectors.Option;
        Py_Generic_Services           : PB_Support.Boolean_Vectors.Option;
        Deprecated                    : PB_Support.Boolean_Vectors.Option;
        Cc_Enable_Arenas              : PB_Support.Boolean_Vectors.Option;
        Objc_Class_Prefix             : PB_Support.Universal_String_Vectors
          .Option;
        Csharp_Namespace              : PB_Support.Universal_String_Vectors
          .Option;
        Uninterpreted_Option          : Google.Protobuf.Descriptor
          .Uninterpreted_Option_Vector;
     end record;

   type Optional_File_Options  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.File_Options;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : File_Options_Vector) return Natural;

   procedure Clear (Self : in out File_Options_Vector);

   procedure Append (Self : in out File_Options_Vector; V    : File_Options);

   type File_Options_Variable_Reference
     (Element : not null access File_Options) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_File_Options_Variable_Reference
    (Self  : aliased in out File_Options_Vector;
     Index : Positive)
      return File_Options_Variable_Reference
     with Inline;

   type File_Options_Constant_Reference
     (Element : not null access constant File_Options) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_File_Options_Constant_Reference
    (Self  : aliased File_Options_Vector;
     Index : Positive)
      return File_Options_Constant_Reference
     with Inline;

   type Message_Options is
     record
        Message_Set_Wire_Format         : PB_Support.Boolean_Vectors.Option;
        No_Standard_Descriptor_Accessor : PB_Support.Boolean_Vectors.Option;
        Deprecated                      : PB_Support.Boolean_Vectors.Option;
        Map_Entry                       : PB_Support.Boolean_Vectors.Option;
        Uninterpreted_Option            : Google.Protobuf.Descriptor
          .Uninterpreted_Option_Vector;
     end record;

   type Optional_Message_Options  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Message_Options;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Message_Options_Vector) return Natural;

   procedure Clear (Self : in out Message_Options_Vector);

   procedure Append
    (Self : in out Message_Options_Vector;
     V    : Message_Options);

   type Message_Options_Variable_Reference
     (Element : not null access Message_Options) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Message_Options_Variable_Reference
    (Self  : aliased in out Message_Options_Vector;
     Index : Positive)
      return Message_Options_Variable_Reference
     with Inline;

   type Message_Options_Constant_Reference
     (Element : not null access constant Message_Options) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Message_Options_Constant_Reference
    (Self  : aliased Message_Options_Vector;
     Index : Positive)
      return Message_Options_Constant_Reference
     with Inline;

   type Field_Options is
     record
        Ctype                : Google.Protobuf.Descriptor.CType_Vectors.Option;
        Packed               : PB_Support.Boolean_Vectors.Option;
        Jstype               : Google.Protobuf.Descriptor.JSType_Vectors
          .Option;
        Lazy                 : PB_Support.Boolean_Vectors.Option;
        Deprecated           : PB_Support.Boolean_Vectors.Option;
        Weak                 : PB_Support.Boolean_Vectors.Option;
        Uninterpreted_Option : Google.Protobuf.Descriptor
          .Uninterpreted_Option_Vector;
     end record;

   type Optional_Field_Options  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Field_Options;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Field_Options_Vector) return Natural;

   procedure Clear (Self : in out Field_Options_Vector);

   procedure Append (Self : in out Field_Options_Vector; V    : Field_Options);

   type Field_Options_Variable_Reference
     (Element : not null access Field_Options) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Field_Options_Variable_Reference
    (Self  : aliased in out Field_Options_Vector;
     Index : Positive)
      return Field_Options_Variable_Reference
     with Inline;

   type Field_Options_Constant_Reference
     (Element : not null access constant Field_Options) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Field_Options_Constant_Reference
    (Self  : aliased Field_Options_Vector;
     Index : Positive)
      return Field_Options_Constant_Reference
     with Inline;

   type Oneof_Options is
     record
        Uninterpreted_Option : Google.Protobuf.Descriptor
          .Uninterpreted_Option_Vector;
     end record;

   type Optional_Oneof_Options  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Oneof_Options;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Oneof_Options_Vector) return Natural;

   procedure Clear (Self : in out Oneof_Options_Vector);

   procedure Append (Self : in out Oneof_Options_Vector; V    : Oneof_Options);

   type Oneof_Options_Variable_Reference
     (Element : not null access Oneof_Options) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Oneof_Options_Variable_Reference
    (Self  : aliased in out Oneof_Options_Vector;
     Index : Positive)
      return Oneof_Options_Variable_Reference
     with Inline;

   type Oneof_Options_Constant_Reference
     (Element : not null access constant Oneof_Options) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Oneof_Options_Constant_Reference
    (Self  : aliased Oneof_Options_Vector;
     Index : Positive)
      return Oneof_Options_Constant_Reference
     with Inline;

   type Enum_Options is
     record
        Allow_Alias          : PB_Support.Boolean_Vectors.Option;
        Deprecated           : PB_Support.Boolean_Vectors.Option;
        Uninterpreted_Option : Google.Protobuf.Descriptor
          .Uninterpreted_Option_Vector;
     end record;

   type Optional_Enum_Options  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Enum_Options;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Enum_Options_Vector) return Natural;

   procedure Clear (Self : in out Enum_Options_Vector);

   procedure Append (Self : in out Enum_Options_Vector; V    : Enum_Options);

   type Enum_Options_Variable_Reference
     (Element : not null access Enum_Options) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Enum_Options_Variable_Reference
    (Self  : aliased in out Enum_Options_Vector;
     Index : Positive)
      return Enum_Options_Variable_Reference
     with Inline;

   type Enum_Options_Constant_Reference
     (Element : not null access constant Enum_Options) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Enum_Options_Constant_Reference
    (Self  : aliased Enum_Options_Vector;
     Index : Positive)
      return Enum_Options_Constant_Reference
     with Inline;

   type Enum_Value_Options is
     record
        Deprecated           : PB_Support.Boolean_Vectors.Option;
        Uninterpreted_Option : Google.Protobuf.Descriptor
          .Uninterpreted_Option_Vector;
     end record;

   type Optional_Enum_Value_Options  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Enum_Value_Options;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Enum_Value_Options_Vector) return Natural;

   procedure Clear (Self : in out Enum_Value_Options_Vector);

   procedure Append
    (Self : in out Enum_Value_Options_Vector;
     V    : Enum_Value_Options);

   type Enum_Value_Options_Variable_Reference
     (Element : not null access Enum_Value_Options) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Enum_Value_Options_Variable_Reference
    (Self  : aliased in out Enum_Value_Options_Vector;
     Index : Positive)
      return Enum_Value_Options_Variable_Reference
     with Inline;

   type Enum_Value_Options_Constant_Reference
     (Element : not null access constant Enum_Value_Options) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Enum_Value_Options_Constant_Reference
    (Self  : aliased Enum_Value_Options_Vector;
     Index : Positive)
      return Enum_Value_Options_Constant_Reference
     with Inline;

   type Service_Options is
     record
        Deprecated           : PB_Support.Boolean_Vectors.Option;
        Uninterpreted_Option : Google.Protobuf.Descriptor
          .Uninterpreted_Option_Vector;
     end record;

   type Optional_Service_Options  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Service_Options;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Service_Options_Vector) return Natural;

   procedure Clear (Self : in out Service_Options_Vector);

   procedure Append
    (Self : in out Service_Options_Vector;
     V    : Service_Options);

   type Service_Options_Variable_Reference
     (Element : not null access Service_Options) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Service_Options_Variable_Reference
    (Self  : aliased in out Service_Options_Vector;
     Index : Positive)
      return Service_Options_Variable_Reference
     with Inline;

   type Service_Options_Constant_Reference
     (Element : not null access constant Service_Options) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Service_Options_Constant_Reference
    (Self  : aliased Service_Options_Vector;
     Index : Positive)
      return Service_Options_Constant_Reference
     with Inline;

   type Method_Options is
     record
        Deprecated           : PB_Support.Boolean_Vectors.Option;
        Uninterpreted_Option : Google.Protobuf.Descriptor
          .Uninterpreted_Option_Vector;
     end record;

   type Optional_Method_Options  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Method_Options;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Method_Options_Vector) return Natural;

   procedure Clear (Self : in out Method_Options_Vector);

   procedure Append
    (Self : in out Method_Options_Vector;
     V    : Method_Options);

   type Method_Options_Variable_Reference
     (Element : not null access Method_Options) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Method_Options_Variable_Reference
    (Self  : aliased in out Method_Options_Vector;
     Index : Positive)
      return Method_Options_Variable_Reference
     with Inline;

   type Method_Options_Constant_Reference
     (Element : not null access constant Method_Options) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Method_Options_Constant_Reference
    (Self  : aliased Method_Options_Vector;
     Index : Positive)
      return Method_Options_Constant_Reference
     with Inline;

   type Name_Part is
     record
        Name_Part    : League.Strings.Universal_String;
        Is_Extension : Boolean := False;
     end record;

   type Optional_Name_Part  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Name_Part;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Name_Part_Vector) return Natural;

   procedure Clear (Self : in out Name_Part_Vector);

   procedure Append (Self : in out Name_Part_Vector; V    : Name_Part);

   type Name_Part_Variable_Reference  (Element : not null access Name_Part) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Name_Part_Variable_Reference
    (Self  : aliased in out Name_Part_Vector;
     Index : Positive)
      return Name_Part_Variable_Reference
     with Inline;

   type Name_Part_Constant_Reference
     (Element : not null access constant Name_Part) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Name_Part_Constant_Reference
    (Self  : aliased Name_Part_Vector;
     Index : Positive)
      return Name_Part_Constant_Reference
     with Inline;

   type Uninterpreted_Option is
     record
        Name               : Google.Protobuf.Descriptor.Name_Part_Vector;
        Identifier_Value   : PB_Support.Universal_String_Vectors.Option;
        Positive_Int_Value : PB_Support.Unsigned_64_Vectors.Option;
        Negative_Int_Value : PB_Support.Integer_64_Vectors.Option;
        Double_Value       : PB_Support.IEEE_Float_64_Vectors.Option;
        String_Value       : PB_Support.Stream_Element_Vector_Vectors.Option;
        Aggregate_Value    : PB_Support.Universal_String_Vectors.Option;
     end record;

   type Optional_Uninterpreted_Option  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Uninterpreted_Option;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Uninterpreted_Option_Vector) return Natural;

   procedure Clear (Self : in out Uninterpreted_Option_Vector);

   procedure Append
    (Self : in out Uninterpreted_Option_Vector;
     V    : Uninterpreted_Option);

   type Uninterpreted_Option_Variable_Reference
     (Element : not null access Uninterpreted_Option) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Uninterpreted_Option_Variable_Reference
    (Self  : aliased in out Uninterpreted_Option_Vector;
     Index : Positive)
      return Uninterpreted_Option_Variable_Reference
     with Inline;

   type Uninterpreted_Option_Constant_Reference
     (Element : not null access constant Uninterpreted_Option) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Uninterpreted_Option_Constant_Reference
    (Self  : aliased Uninterpreted_Option_Vector;
     Index : Positive)
      return Uninterpreted_Option_Constant_Reference
     with Inline;

   type Location is
     record
        Path                      : PB_Support.Integer_32_Vectors.Vector;
        Span                      : PB_Support.Integer_32_Vectors.Vector;
        Leading_Comments          : PB_Support.Universal_String_Vectors.Option;
        Trailing_Comments         : PB_Support.Universal_String_Vectors.Option;
        Leading_Detached_Comments : League.String_Vectors
          .Universal_String_Vector;
     end record;

   type Optional_Location  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Location;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Location_Vector) return Natural;

   procedure Clear (Self : in out Location_Vector);

   procedure Append (Self : in out Location_Vector; V    : Location);

   type Location_Variable_Reference  (Element : not null access Location) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Location_Variable_Reference
    (Self  : aliased in out Location_Vector;
     Index : Positive)
      return Location_Variable_Reference
     with Inline;

   type Location_Constant_Reference
     (Element : not null access constant Location) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Location_Constant_Reference
    (Self  : aliased Location_Vector;
     Index : Positive)
      return Location_Constant_Reference
     with Inline;

   type Source_Code_Info is
     record
        Location : Google.Protobuf.Descriptor.Location_Vector;
     end record;

   type Optional_Source_Code_Info  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Source_Code_Info;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Source_Code_Info_Vector) return Natural;

   procedure Clear (Self : in out Source_Code_Info_Vector);

   procedure Append
    (Self : in out Source_Code_Info_Vector;
     V    : Source_Code_Info);

   type Source_Code_Info_Variable_Reference
     (Element : not null access Source_Code_Info) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Source_Code_Info_Variable_Reference
    (Self  : aliased in out Source_Code_Info_Vector;
     Index : Positive)
      return Source_Code_Info_Variable_Reference
     with Inline;

   type Source_Code_Info_Constant_Reference
     (Element : not null access constant Source_Code_Info) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Source_Code_Info_Constant_Reference
    (Self  : aliased Source_Code_Info_Vector;
     Index : Positive)
      return Source_Code_Info_Constant_Reference
     with Inline;

   type Annotation is
     record
        Path        : PB_Support.Integer_32_Vectors.Vector;
        Source_File : PB_Support.Universal_String_Vectors.Option;
        PB_Begin    : PB_Support.Integer_32_Vectors.Option;
        PB_End      : PB_Support.Integer_32_Vectors.Option;
     end record;

   type Optional_Annotation  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Annotation;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Annotation_Vector) return Natural;

   procedure Clear (Self : in out Annotation_Vector);

   procedure Append (Self : in out Annotation_Vector; V    : Annotation);

   type Annotation_Variable_Reference
     (Element : not null access Annotation) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Annotation_Variable_Reference
    (Self  : aliased in out Annotation_Vector;
     Index : Positive)
      return Annotation_Variable_Reference
     with Inline;

   type Annotation_Constant_Reference
     (Element : not null access constant Annotation) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Annotation_Constant_Reference
    (Self  : aliased Annotation_Vector;
     Index : Positive)
      return Annotation_Constant_Reference
     with Inline;

   type Generated_Code_Info is
     record
        Annotation : Google.Protobuf.Descriptor.Annotation_Vector;
     end record;

   type Optional_Generated_Code_Info  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Generated_Code_Info;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Generated_Code_Info_Vector) return Natural;

   procedure Clear (Self : in out Generated_Code_Info_Vector);

   procedure Append
    (Self : in out Generated_Code_Info_Vector;
     V    : Generated_Code_Info);

   type Generated_Code_Info_Variable_Reference
     (Element : not null access Generated_Code_Info) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Generated_Code_Info_Variable_Reference
    (Self  : aliased in out Generated_Code_Info_Vector;
     Index : Positive)
      return Generated_Code_Info_Variable_Reference
     with Inline;

   type Generated_Code_Info_Constant_Reference
     (Element : not null access constant Generated_Code_Info) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Generated_Code_Info_Constant_Reference
    (Self  : aliased Generated_Code_Info_Vector;
     Index : Positive)
      return Generated_Code_Info_Constant_Reference
     with Inline;

   type File_Descriptor_Proto is
     record
        Name              : PB_Support.Universal_String_Vectors.Option;
        PB_Package        : PB_Support.Universal_String_Vectors.Option;
        Dependency        : League.String_Vectors.Universal_String_Vector;
        Public_Dependency : PB_Support.Integer_32_Vectors.Vector;
        Weak_Dependency   : PB_Support.Integer_32_Vectors.Vector;
        Message_Type      : Google.Protobuf.Descriptor.Descriptor_Proto_Vector;
        Enum_Type         : Google.Protobuf.Descriptor
          .Enum_Descriptor_Proto_Vector;
        Service           : Google.Protobuf.Descriptor
          .Service_Descriptor_Proto_Vector;
        Extension         : Google.Protobuf.Descriptor
          .Field_Descriptor_Proto_Vector;
        Options           : Google.Protobuf.Descriptor.Optional_File_Options;
        Source_Code_Info  : Google.Protobuf.Descriptor
          .Optional_Source_Code_Info;
        Syntax            : PB_Support.Universal_String_Vectors.Option;
     end record;

   type Optional_File_Descriptor_Proto  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.File_Descriptor_Proto;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : File_Descriptor_Proto_Vector) return Natural;

   procedure Clear (Self : in out File_Descriptor_Proto_Vector);

   procedure Append
    (Self : in out File_Descriptor_Proto_Vector;
     V    : File_Descriptor_Proto);

   type File_Descriptor_Proto_Variable_Reference
     (Element : not null access File_Descriptor_Proto) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_File_Descriptor_Proto_Variable_Reference
    (Self  : aliased in out File_Descriptor_Proto_Vector;
     Index : Positive)
      return File_Descriptor_Proto_Variable_Reference
     with Inline;

   type File_Descriptor_Proto_Constant_Reference
     (Element : not null access constant File_Descriptor_Proto) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_File_Descriptor_Proto_Constant_Reference
    (Self  : aliased File_Descriptor_Proto_Vector;
     Index : Positive)
      return File_Descriptor_Proto_Constant_Reference
     with Inline;

   type Descriptor_Proto is
     record
        Name            : PB_Support.Universal_String_Vectors.Option;
        Field           : Google.Protobuf.Descriptor
          .Field_Descriptor_Proto_Vector;
        Extension       : Google.Protobuf.Descriptor
          .Field_Descriptor_Proto_Vector;
        Nested_Type     : Google.Protobuf.Descriptor.Descriptor_Proto_Vector;
        Enum_Type       : Google.Protobuf.Descriptor
          .Enum_Descriptor_Proto_Vector;
        Extension_Range : Google.Protobuf.Descriptor.Extension_Range_Vector;
        Oneof_Decl      : Google.Protobuf.Descriptor
          .Oneof_Descriptor_Proto_Vector;
        Options         : Google.Protobuf.Descriptor.Optional_Message_Options;
        Reserved_Range  : Google.Protobuf.Descriptor.Reserved_Range_Vector;
        Reserved_Name   : League.String_Vectors.Universal_String_Vector;
     end record;

   type Optional_Descriptor_Proto  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Descriptor_Proto;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Descriptor_Proto_Vector) return Natural;

   procedure Clear (Self : in out Descriptor_Proto_Vector);

   procedure Append
    (Self : in out Descriptor_Proto_Vector;
     V    : Descriptor_Proto);

   type Descriptor_Proto_Variable_Reference
     (Element : not null access Descriptor_Proto) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Descriptor_Proto_Variable_Reference
    (Self  : aliased in out Descriptor_Proto_Vector;
     Index : Positive)
      return Descriptor_Proto_Variable_Reference
     with Inline;

   type Descriptor_Proto_Constant_Reference
     (Element : not null access constant Descriptor_Proto) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Descriptor_Proto_Constant_Reference
    (Self  : aliased Descriptor_Proto_Vector;
     Index : Positive)
      return Descriptor_Proto_Constant_Reference
     with Inline;

   type Field_Descriptor_Proto is
     record
        Name          : PB_Support.Universal_String_Vectors.Option;
        Number        : PB_Support.Integer_32_Vectors.Option;
        Label         : Google.Protobuf.Descriptor.Label_Vectors.Option;
        PB_Type       : Google.Protobuf.Descriptor.PB_Type_Vectors.Option;
        Type_Name     : PB_Support.Universal_String_Vectors.Option;
        Extendee      : PB_Support.Universal_String_Vectors.Option;
        Default_Value : PB_Support.Universal_String_Vectors.Option;
        Oneof_Index   : PB_Support.Integer_32_Vectors.Option;
        Json_Name     : PB_Support.Universal_String_Vectors.Option;
        Options       : Google.Protobuf.Descriptor.Optional_Field_Options;
     end record;

   type Optional_Field_Descriptor_Proto  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Field_Descriptor_Proto_Vector) return Natural;

   procedure Clear (Self : in out Field_Descriptor_Proto_Vector);

   procedure Append
    (Self : in out Field_Descriptor_Proto_Vector;
     V    : Field_Descriptor_Proto);

   type Field_Descriptor_Proto_Variable_Reference
     (Element : not null access Field_Descriptor_Proto) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Field_Descriptor_Proto_Variable_Reference
    (Self  : aliased in out Field_Descriptor_Proto_Vector;
     Index : Positive)
      return Field_Descriptor_Proto_Variable_Reference
     with Inline;

   type Field_Descriptor_Proto_Constant_Reference
     (Element : not null access constant Field_Descriptor_Proto) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Field_Descriptor_Proto_Constant_Reference
    (Self  : aliased Field_Descriptor_Proto_Vector;
     Index : Positive)
      return Field_Descriptor_Proto_Constant_Reference
     with Inline;

   type Oneof_Descriptor_Proto is
     record
        Name    : PB_Support.Universal_String_Vectors.Option;
        Options : Google.Protobuf.Descriptor.Optional_Oneof_Options;
     end record;

   type Optional_Oneof_Descriptor_Proto  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Oneof_Descriptor_Proto;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Oneof_Descriptor_Proto_Vector) return Natural;

   procedure Clear (Self : in out Oneof_Descriptor_Proto_Vector);

   procedure Append
    (Self : in out Oneof_Descriptor_Proto_Vector;
     V    : Oneof_Descriptor_Proto);

   type Oneof_Descriptor_Proto_Variable_Reference
     (Element : not null access Oneof_Descriptor_Proto) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Oneof_Descriptor_Proto_Variable_Reference
    (Self  : aliased in out Oneof_Descriptor_Proto_Vector;
     Index : Positive)
      return Oneof_Descriptor_Proto_Variable_Reference
     with Inline;

   type Oneof_Descriptor_Proto_Constant_Reference
     (Element : not null access constant Oneof_Descriptor_Proto) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Oneof_Descriptor_Proto_Constant_Reference
    (Self  : aliased Oneof_Descriptor_Proto_Vector;
     Index : Positive)
      return Oneof_Descriptor_Proto_Constant_Reference
     with Inline;

   type Enum_Descriptor_Proto is
     record
        Name    : PB_Support.Universal_String_Vectors.Option;
        Value   : Google.Protobuf.Descriptor
          .Enum_Value_Descriptor_Proto_Vector;
        Options : Google.Protobuf.Descriptor.Optional_Enum_Options;
     end record;

   type Optional_Enum_Descriptor_Proto  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Enum_Descriptor_Proto;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Enum_Descriptor_Proto_Vector) return Natural;

   procedure Clear (Self : in out Enum_Descriptor_Proto_Vector);

   procedure Append
    (Self : in out Enum_Descriptor_Proto_Vector;
     V    : Enum_Descriptor_Proto);

   type Enum_Descriptor_Proto_Variable_Reference
     (Element : not null access Enum_Descriptor_Proto) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Enum_Descriptor_Proto_Variable_Reference
    (Self  : aliased in out Enum_Descriptor_Proto_Vector;
     Index : Positive)
      return Enum_Descriptor_Proto_Variable_Reference
     with Inline;

   type Enum_Descriptor_Proto_Constant_Reference
     (Element : not null access constant Enum_Descriptor_Proto) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Enum_Descriptor_Proto_Constant_Reference
    (Self  : aliased Enum_Descriptor_Proto_Vector;
     Index : Positive)
      return Enum_Descriptor_Proto_Constant_Reference
     with Inline;

   type Enum_Value_Descriptor_Proto is
     record
        Name    : PB_Support.Universal_String_Vectors.Option;
        Number  : PB_Support.Integer_32_Vectors.Option;
        Options : Google.Protobuf.Descriptor.Optional_Enum_Value_Options;
     end record;

   type Optional_Enum_Value_Descriptor_Proto  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Enum_Value_Descriptor_Proto;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Enum_Value_Descriptor_Proto_Vector) return Natural;

   procedure Clear (Self : in out Enum_Value_Descriptor_Proto_Vector);

   procedure Append
    (Self : in out Enum_Value_Descriptor_Proto_Vector;
     V    : Enum_Value_Descriptor_Proto);

   type Enum_Value_Descriptor_Proto_Variable_Reference
     (Element : not null access Enum_Value_Descriptor_Proto) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Enum_Value_Descriptor_Proto_Variable_Reference
    (Self  : aliased in out Enum_Value_Descriptor_Proto_Vector;
     Index : Positive)
      return Enum_Value_Descriptor_Proto_Variable_Reference
     with Inline;

   type Enum_Value_Descriptor_Proto_Constant_Reference
     (Element : not null access constant Enum_Value_Descriptor_Proto) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Enum_Value_Descriptor_Proto_Constant_Reference
    (Self  : aliased Enum_Value_Descriptor_Proto_Vector;
     Index : Positive)
      return Enum_Value_Descriptor_Proto_Constant_Reference
     with Inline;

   type Service_Descriptor_Proto is
     record
        Name    : PB_Support.Universal_String_Vectors.Option;
        Method  : Google.Protobuf.Descriptor.Method_Descriptor_Proto_Vector;
        Options : Google.Protobuf.Descriptor.Optional_Service_Options;
     end record;

   type Optional_Service_Descriptor_Proto  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Service_Descriptor_Proto;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Service_Descriptor_Proto_Vector) return Natural;

   procedure Clear (Self : in out Service_Descriptor_Proto_Vector);

   procedure Append
    (Self : in out Service_Descriptor_Proto_Vector;
     V    : Service_Descriptor_Proto);

   type Service_Descriptor_Proto_Variable_Reference
     (Element : not null access Service_Descriptor_Proto) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Service_Descriptor_Proto_Variable_Reference
    (Self  : aliased in out Service_Descriptor_Proto_Vector;
     Index : Positive)
      return Service_Descriptor_Proto_Variable_Reference
     with Inline;

   type Service_Descriptor_Proto_Constant_Reference
     (Element : not null access constant Service_Descriptor_Proto) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Service_Descriptor_Proto_Constant_Reference
    (Self  : aliased Service_Descriptor_Proto_Vector;
     Index : Positive)
      return Service_Descriptor_Proto_Constant_Reference
     with Inline;

   type Method_Descriptor_Proto is
     record
        Name             : PB_Support.Universal_String_Vectors.Option;
        Input_Type       : PB_Support.Universal_String_Vectors.Option;
        Output_Type      : PB_Support.Universal_String_Vectors.Option;
        Options          : Google.Protobuf.Descriptor.Optional_Method_Options;
        Client_Streaming : PB_Support.Boolean_Vectors.Option;
        Server_Streaming : PB_Support.Boolean_Vectors.Option;
     end record;

   type Optional_Method_Descriptor_Proto  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Method_Descriptor_Proto;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Method_Descriptor_Proto_Vector) return Natural;

   procedure Clear (Self : in out Method_Descriptor_Proto_Vector);

   procedure Append
    (Self : in out Method_Descriptor_Proto_Vector;
     V    : Method_Descriptor_Proto);

   type Method_Descriptor_Proto_Variable_Reference
     (Element : not null access Method_Descriptor_Proto) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Method_Descriptor_Proto_Variable_Reference
    (Self  : aliased in out Method_Descriptor_Proto_Vector;
     Index : Positive)
      return Method_Descriptor_Proto_Variable_Reference
     with Inline;

   type Method_Descriptor_Proto_Constant_Reference
     (Element : not null access constant Method_Descriptor_Proto) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Method_Descriptor_Proto_Constant_Reference
    (Self  : aliased Method_Descriptor_Proto_Vector;
     Index : Positive)
      return Method_Descriptor_Proto_Constant_Reference
     with Inline;
private

   procedure Read_File_Descriptor_Set
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out File_Descriptor_Set);

   procedure Write_File_Descriptor_Set
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : File_Descriptor_Set);

   for File_Descriptor_Set'Read use Read_File_Descriptor_Set;

   for File_Descriptor_Set'Write use Write_File_Descriptor_Set;

   type File_Descriptor_Set_Array is
     array (Positive range <>) of aliased File_Descriptor_Set;

   type File_Descriptor_Set_Array_Access is access File_Descriptor_Set_Array;

   type File_Descriptor_Set_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : File_Descriptor_Set_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out File_Descriptor_Set_Vector);

   overriding procedure Finalize (Self : in out File_Descriptor_Set_Vector);

   procedure Read_File_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out File_Descriptor_Proto);

   procedure Write_File_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : File_Descriptor_Proto);

   for File_Descriptor_Proto'Read use Read_File_Descriptor_Proto;

   for File_Descriptor_Proto'Write use Write_File_Descriptor_Proto;

   type File_Descriptor_Proto_Array is
     array (Positive range <>) of aliased File_Descriptor_Proto;

   type File_Descriptor_Proto_Array_Access is
     access File_Descriptor_Proto_Array;

   type File_Descriptor_Proto_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : File_Descriptor_Proto_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out File_Descriptor_Proto_Vector);

   overriding procedure Finalize (Self : in out File_Descriptor_Proto_Vector);

   procedure Read_Extension_Range
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Extension_Range);

   procedure Write_Extension_Range
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Extension_Range);

   for Extension_Range'Read use Read_Extension_Range;

   for Extension_Range'Write use Write_Extension_Range;

   type Extension_Range_Array is
     array (Positive range <>) of aliased Extension_Range;

   type Extension_Range_Array_Access is access Extension_Range_Array;

   type Extension_Range_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Extension_Range_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Extension_Range_Vector);

   overriding procedure Finalize (Self : in out Extension_Range_Vector);

   procedure Read_Reserved_Range
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Reserved_Range);

   procedure Write_Reserved_Range
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Reserved_Range);

   for Reserved_Range'Read use Read_Reserved_Range;

   for Reserved_Range'Write use Write_Reserved_Range;

   type Reserved_Range_Array is
     array (Positive range <>) of aliased Reserved_Range;

   type Reserved_Range_Array_Access is access Reserved_Range_Array;

   type Reserved_Range_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Reserved_Range_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Reserved_Range_Vector);

   overriding procedure Finalize (Self : in out Reserved_Range_Vector);

   procedure Read_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Descriptor_Proto);

   procedure Write_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Descriptor_Proto);

   for Descriptor_Proto'Read use Read_Descriptor_Proto;

   for Descriptor_Proto'Write use Write_Descriptor_Proto;

   type Descriptor_Proto_Array is
     array (Positive range <>) of aliased Descriptor_Proto;

   type Descriptor_Proto_Array_Access is access Descriptor_Proto_Array;

   type Descriptor_Proto_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Descriptor_Proto_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Descriptor_Proto_Vector);

   overriding procedure Finalize (Self : in out Descriptor_Proto_Vector);

   procedure Read_Field_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Field_Descriptor_Proto);

   procedure Write_Field_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Field_Descriptor_Proto);

   for Field_Descriptor_Proto'Read use Read_Field_Descriptor_Proto;

   for Field_Descriptor_Proto'Write use Write_Field_Descriptor_Proto;

   type Field_Descriptor_Proto_Array is
     array (Positive range <>) of aliased Field_Descriptor_Proto;

   type Field_Descriptor_Proto_Array_Access is
     access Field_Descriptor_Proto_Array;

   type Field_Descriptor_Proto_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Field_Descriptor_Proto_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Field_Descriptor_Proto_Vector);

   overriding procedure Finalize (Self : in out Field_Descriptor_Proto_Vector);

   procedure Read_Oneof_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Oneof_Descriptor_Proto);

   procedure Write_Oneof_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Oneof_Descriptor_Proto);

   for Oneof_Descriptor_Proto'Read use Read_Oneof_Descriptor_Proto;

   for Oneof_Descriptor_Proto'Write use Write_Oneof_Descriptor_Proto;

   type Oneof_Descriptor_Proto_Array is
     array (Positive range <>) of aliased Oneof_Descriptor_Proto;

   type Oneof_Descriptor_Proto_Array_Access is
     access Oneof_Descriptor_Proto_Array;

   type Oneof_Descriptor_Proto_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Oneof_Descriptor_Proto_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Oneof_Descriptor_Proto_Vector);

   overriding procedure Finalize (Self : in out Oneof_Descriptor_Proto_Vector);

   procedure Read_Enum_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Enum_Descriptor_Proto);

   procedure Write_Enum_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Enum_Descriptor_Proto);

   for Enum_Descriptor_Proto'Read use Read_Enum_Descriptor_Proto;

   for Enum_Descriptor_Proto'Write use Write_Enum_Descriptor_Proto;

   type Enum_Descriptor_Proto_Array is
     array (Positive range <>) of aliased Enum_Descriptor_Proto;

   type Enum_Descriptor_Proto_Array_Access is
     access Enum_Descriptor_Proto_Array;

   type Enum_Descriptor_Proto_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Enum_Descriptor_Proto_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Enum_Descriptor_Proto_Vector);

   overriding procedure Finalize (Self : in out Enum_Descriptor_Proto_Vector);

   procedure Read_Enum_Value_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Enum_Value_Descriptor_Proto);

   procedure Write_Enum_Value_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Enum_Value_Descriptor_Proto);

   for Enum_Value_Descriptor_Proto'Read use Read_Enum_Value_Descriptor_Proto;

   for Enum_Value_Descriptor_Proto'Write use Write_Enum_Value_Descriptor_Proto;

   type Enum_Value_Descriptor_Proto_Array is
     array (Positive range <>) of aliased Enum_Value_Descriptor_Proto;

   type Enum_Value_Descriptor_Proto_Array_Access is
     access Enum_Value_Descriptor_Proto_Array;

   type Enum_Value_Descriptor_Proto_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Enum_Value_Descriptor_Proto_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust
    (Self : in out Enum_Value_Descriptor_Proto_Vector);

   overriding procedure Finalize
    (Self : in out Enum_Value_Descriptor_Proto_Vector);

   procedure Read_Service_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Service_Descriptor_Proto);

   procedure Write_Service_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Service_Descriptor_Proto);

   for Service_Descriptor_Proto'Read use Read_Service_Descriptor_Proto;

   for Service_Descriptor_Proto'Write use Write_Service_Descriptor_Proto;

   type Service_Descriptor_Proto_Array is
     array (Positive range <>) of aliased Service_Descriptor_Proto;

   type Service_Descriptor_Proto_Array_Access is
     access Service_Descriptor_Proto_Array;

   type Service_Descriptor_Proto_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Service_Descriptor_Proto_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Service_Descriptor_Proto_Vector);

   overriding procedure Finalize
    (Self : in out Service_Descriptor_Proto_Vector);

   procedure Read_Method_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Method_Descriptor_Proto);

   procedure Write_Method_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Method_Descriptor_Proto);

   for Method_Descriptor_Proto'Read use Read_Method_Descriptor_Proto;

   for Method_Descriptor_Proto'Write use Write_Method_Descriptor_Proto;

   type Method_Descriptor_Proto_Array is
     array (Positive range <>) of aliased Method_Descriptor_Proto;

   type Method_Descriptor_Proto_Array_Access is
     access Method_Descriptor_Proto_Array;

   type Method_Descriptor_Proto_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Method_Descriptor_Proto_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Method_Descriptor_Proto_Vector);

   overriding procedure Finalize
    (Self : in out Method_Descriptor_Proto_Vector);

   procedure Read_File_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out File_Options);

   procedure Write_File_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : File_Options);

   for File_Options'Read use Read_File_Options;

   for File_Options'Write use Write_File_Options;

   type File_Options_Array is
     array (Positive range <>) of aliased File_Options;

   type File_Options_Array_Access is access File_Options_Array;

   type File_Options_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : File_Options_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out File_Options_Vector);

   overriding procedure Finalize (Self : in out File_Options_Vector);

   procedure Read_Message_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Message_Options);

   procedure Write_Message_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Message_Options);

   for Message_Options'Read use Read_Message_Options;

   for Message_Options'Write use Write_Message_Options;

   type Message_Options_Array is
     array (Positive range <>) of aliased Message_Options;

   type Message_Options_Array_Access is access Message_Options_Array;

   type Message_Options_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Message_Options_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Message_Options_Vector);

   overriding procedure Finalize (Self : in out Message_Options_Vector);

   procedure Read_Field_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Field_Options);

   procedure Write_Field_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Field_Options);

   for Field_Options'Read use Read_Field_Options;

   for Field_Options'Write use Write_Field_Options;

   type Field_Options_Array is
     array (Positive range <>) of aliased Field_Options;

   type Field_Options_Array_Access is access Field_Options_Array;

   type Field_Options_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Field_Options_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Field_Options_Vector);

   overriding procedure Finalize (Self : in out Field_Options_Vector);

   procedure Read_Oneof_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Oneof_Options);

   procedure Write_Oneof_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Oneof_Options);

   for Oneof_Options'Read use Read_Oneof_Options;

   for Oneof_Options'Write use Write_Oneof_Options;

   type Oneof_Options_Array is
     array (Positive range <>) of aliased Oneof_Options;

   type Oneof_Options_Array_Access is access Oneof_Options_Array;

   type Oneof_Options_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Oneof_Options_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Oneof_Options_Vector);

   overriding procedure Finalize (Self : in out Oneof_Options_Vector);

   procedure Read_Enum_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Enum_Options);

   procedure Write_Enum_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Enum_Options);

   for Enum_Options'Read use Read_Enum_Options;

   for Enum_Options'Write use Write_Enum_Options;

   type Enum_Options_Array is
     array (Positive range <>) of aliased Enum_Options;

   type Enum_Options_Array_Access is access Enum_Options_Array;

   type Enum_Options_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Enum_Options_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Enum_Options_Vector);

   overriding procedure Finalize (Self : in out Enum_Options_Vector);

   procedure Read_Enum_Value_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Enum_Value_Options);

   procedure Write_Enum_Value_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Enum_Value_Options);

   for Enum_Value_Options'Read use Read_Enum_Value_Options;

   for Enum_Value_Options'Write use Write_Enum_Value_Options;

   type Enum_Value_Options_Array is
     array (Positive range <>) of aliased Enum_Value_Options;

   type Enum_Value_Options_Array_Access is access Enum_Value_Options_Array;

   type Enum_Value_Options_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Enum_Value_Options_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Enum_Value_Options_Vector);

   overriding procedure Finalize (Self : in out Enum_Value_Options_Vector);

   procedure Read_Service_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Service_Options);

   procedure Write_Service_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Service_Options);

   for Service_Options'Read use Read_Service_Options;

   for Service_Options'Write use Write_Service_Options;

   type Service_Options_Array is
     array (Positive range <>) of aliased Service_Options;

   type Service_Options_Array_Access is access Service_Options_Array;

   type Service_Options_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Service_Options_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Service_Options_Vector);

   overriding procedure Finalize (Self : in out Service_Options_Vector);

   procedure Read_Method_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Method_Options);

   procedure Write_Method_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Method_Options);

   for Method_Options'Read use Read_Method_Options;

   for Method_Options'Write use Write_Method_Options;

   type Method_Options_Array is
     array (Positive range <>) of aliased Method_Options;

   type Method_Options_Array_Access is access Method_Options_Array;

   type Method_Options_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Method_Options_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Method_Options_Vector);

   overriding procedure Finalize (Self : in out Method_Options_Vector);

   procedure Read_Name_Part
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Name_Part);

   procedure Write_Name_Part
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Name_Part);

   for Name_Part'Read use Read_Name_Part;

   for Name_Part'Write use Write_Name_Part;

   type Name_Part_Array is array (Positive range <>) of aliased Name_Part;

   type Name_Part_Array_Access is access Name_Part_Array;

   type Name_Part_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Name_Part_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Name_Part_Vector);

   overriding procedure Finalize (Self : in out Name_Part_Vector);

   procedure Read_Uninterpreted_Option
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Uninterpreted_Option);

   procedure Write_Uninterpreted_Option
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Uninterpreted_Option);

   for Uninterpreted_Option'Read use Read_Uninterpreted_Option;

   for Uninterpreted_Option'Write use Write_Uninterpreted_Option;

   type Uninterpreted_Option_Array is
     array (Positive range <>) of aliased Uninterpreted_Option;

   type Uninterpreted_Option_Array_Access is access Uninterpreted_Option_Array;

   type Uninterpreted_Option_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Uninterpreted_Option_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Uninterpreted_Option_Vector);

   overriding procedure Finalize (Self : in out Uninterpreted_Option_Vector);

   procedure Read_Location
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Location);

   procedure Write_Location
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Location);

   for Location'Read use Read_Location;

   for Location'Write use Write_Location;

   type Location_Array is array (Positive range <>) of aliased Location;

   type Location_Array_Access is access Location_Array;

   type Location_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Location_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Location_Vector);

   overriding procedure Finalize (Self : in out Location_Vector);

   procedure Read_Source_Code_Info
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Source_Code_Info);

   procedure Write_Source_Code_Info
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Source_Code_Info);

   for Source_Code_Info'Read use Read_Source_Code_Info;

   for Source_Code_Info'Write use Write_Source_Code_Info;

   type Source_Code_Info_Array is
     array (Positive range <>) of aliased Source_Code_Info;

   type Source_Code_Info_Array_Access is access Source_Code_Info_Array;

   type Source_Code_Info_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Source_Code_Info_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Source_Code_Info_Vector);

   overriding procedure Finalize (Self : in out Source_Code_Info_Vector);

   procedure Read_Annotation
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Annotation);

   procedure Write_Annotation
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Annotation);

   for Annotation'Read use Read_Annotation;

   for Annotation'Write use Write_Annotation;

   type Annotation_Array is array (Positive range <>) of aliased Annotation;

   type Annotation_Array_Access is access Annotation_Array;

   type Annotation_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Annotation_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Annotation_Vector);

   overriding procedure Finalize (Self : in out Annotation_Vector);

   procedure Read_Generated_Code_Info
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Generated_Code_Info);

   procedure Write_Generated_Code_Info
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Generated_Code_Info);

   for Generated_Code_Info'Read use Read_Generated_Code_Info;

   for Generated_Code_Info'Write use Write_Generated_Code_Info;

   type Generated_Code_Info_Array is
     array (Positive range <>) of aliased Generated_Code_Info;

   type Generated_Code_Info_Array_Access is access Generated_Code_Info_Array;

   type Generated_Code_Info_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Generated_Code_Info_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Generated_Code_Info_Vector);

   overriding procedure Finalize (Self : in out Generated_Code_Info_Vector);

end Google.Protobuf.Descriptor;