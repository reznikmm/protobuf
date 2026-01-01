with Ada.Finalization;
with Ada.Streams;
with League.String_Vectors;
with League.Strings;
with Proto_Support.Boolean_Options;
with Proto_Support.IEEE_Float_64_Options;
with Proto_Support.Integer_32_Options;
with Proto_Support.Integer_32_Vectors;
with Proto_Support.Integer_64_Options;
with Proto_Support.Options;
with Proto_Support.Stream_Element_Vector_Options;
with Proto_Support.Universal_String_Options;
with Proto_Support.Unsigned_64_Options;
with Proto_Support.Vectors;

package Google.Protobuf.Descriptor is

   type Edition is
     (EDITION_UNKNOWN, EDITION_1_TEST_ONLY, EDITION_2_TEST_ONLY,
      EDITION_LEGACY, EDITION_PROTO2, EDITION_PROTO3, EDITION_2023,
      EDITION_2024, EDITION_UNSTABLE, EDITION_99997_TEST_ONLY,
      EDITION_99998_TEST_ONLY, EDITION_99999_TEST_ONLY, EDITION_MAX);

   for Edition use
     (EDITION_UNKNOWN         => 0, EDITION_1_TEST_ONLY     => 1,
      EDITION_2_TEST_ONLY     => 2, EDITION_LEGACY          => 900,
      EDITION_PROTO2          => 998, EDITION_PROTO3          => 999,
      EDITION_2023            => 1000, EDITION_2024            => 1001,
      EDITION_UNSTABLE        => 9999, EDITION_99997_TEST_ONLY => 99997,
      EDITION_99998_TEST_ONLY => 99998, EDITION_99999_TEST_ONLY => 99999,
      EDITION_MAX             => 2147483647);

   package Edition_Vectors is new Proto_Support.Vectors (Edition);

   package Edition_Options is new Proto_Support.Options (Edition);

   type Symbol_Visibility is
     (VISIBILITY_UNSET, VISIBILITY_LOCAL, VISIBILITY_EXPORT);

   for Symbol_Visibility use
     (VISIBILITY_UNSET  => 0, VISIBILITY_LOCAL  => 1, VISIBILITY_EXPORT => 2);

   package Symbol_Visibility_Vectors is
     new Proto_Support.Vectors (Symbol_Visibility);

   package Symbol_Visibility_Options is
     new Proto_Support.Options (Symbol_Visibility);

   type Verification_State is (EXTENSION_DECLARATION, UNVERIFIED);

   for Verification_State use (EXTENSION_DECLARATION => 0, UNVERIFIED  => 1);

   package Verification_State_Vectors is
     new Proto_Support.Vectors (Verification_State);

   package Verification_State_Options is
     new Proto_Support.Options (Verification_State);

   type Proto_Type is
     (TYPE_DOUBLE, TYPE_FLOAT, TYPE_INT64, TYPE_UINT64, TYPE_INT32,
      TYPE_FIXED64, TYPE_FIXED32, TYPE_BOOL, TYPE_STRING, TYPE_GROUP,
      TYPE_MESSAGE, TYPE_BYTES, TYPE_UINT32, TYPE_ENUM, TYPE_SFIXED32,
      TYPE_SFIXED64, TYPE_SINT32, TYPE_SINT64);

   for Proto_Type use
     (TYPE_DOUBLE   => 1, TYPE_FLOAT    => 2, TYPE_INT64    => 3,
      TYPE_UINT64   => 4, TYPE_INT32    => 5, TYPE_FIXED64  => 6,
      TYPE_FIXED32  => 7, TYPE_BOOL     => 8, TYPE_STRING   => 9,
      TYPE_GROUP    => 10, TYPE_MESSAGE  => 11, TYPE_BYTES    => 12,
      TYPE_UINT32   => 13, TYPE_ENUM     => 14, TYPE_SFIXED32 => 15,
      TYPE_SFIXED64 => 16, TYPE_SINT32   => 17, TYPE_SINT64   => 18);

   package Proto_Type_Vectors is new Proto_Support.Vectors (Proto_Type);

   package Proto_Type_Options is new Proto_Support.Options (Proto_Type);

   type Label is (LABEL_OPTIONAL, LABEL_REQUIRED, LABEL_REPEATED);

   for Label use
     (LABEL_OPTIONAL => 1, LABEL_REQUIRED => 2, LABEL_REPEATED => 3);

   package Label_Vectors is new Proto_Support.Vectors (Label);

   package Label_Options is new Proto_Support.Options (Label);

   type Optimize_Mode is (SPEED, CODE_SIZE, LITE_RUNTIME);

   for Optimize_Mode use
     (SPEED        => 1, CODE_SIZE    => 2, LITE_RUNTIME => 3);

   package Optimize_Mode_Vectors is new Proto_Support.Vectors (Optimize_Mode);

   package Optimize_Mode_Options is new Proto_Support.Options (Optimize_Mode);

   type CType is (STRING, CORD, STRING_PIECE);

   for CType use (STRING       => 0, CORD         => 1, STRING_PIECE => 2);

   package CType_Vectors is new Proto_Support.Vectors (CType);

   package CType_Options is new Proto_Support.Options (CType);

   type JSType is (JS_NORMAL, JS_STRING, JS_NUMBER);

   for JSType use (JS_NORMAL => 0, JS_STRING => 1, JS_NUMBER => 2);

   package JSType_Vectors is new Proto_Support.Vectors (JSType);

   package JSType_Options is new Proto_Support.Options (JSType);

   type Option_Retention is
     (RETENTION_UNKNOWN, RETENTION_RUNTIME, RETENTION_SOURCE);

   for Option_Retention use
     (RETENTION_UNKNOWN => 0, RETENTION_RUNTIME => 1, RETENTION_SOURCE  => 2);

   package Option_Retention_Vectors is
     new Proto_Support.Vectors (Option_Retention);

   package Option_Retention_Options is
     new Proto_Support.Options (Option_Retention);

   type Option_Target_Type is
     (TARGET_TYPE_UNKNOWN, TARGET_TYPE_FILE, TARGET_TYPE_EXTENSION_RANGE,
      TARGET_TYPE_MESSAGE, TARGET_TYPE_FIELD, TARGET_TYPE_ONEOF,
      TARGET_TYPE_ENUM, TARGET_TYPE_ENUM_ENTRY, TARGET_TYPE_SERVICE,
      TARGET_TYPE_METHOD);

   for Option_Target_Type use
     (TARGET_TYPE_UNKNOWN         => 0, TARGET_TYPE_FILE            => 1,
      TARGET_TYPE_EXTENSION_RANGE => 2, TARGET_TYPE_MESSAGE         => 3,
      TARGET_TYPE_FIELD           => 4, TARGET_TYPE_ONEOF           => 5,
      TARGET_TYPE_ENUM            => 6, TARGET_TYPE_ENUM_ENTRY      => 7,
      TARGET_TYPE_SERVICE         => 8, TARGET_TYPE_METHOD          => 9);

   package Option_Target_Type_Vectors is
     new Proto_Support.Vectors (Option_Target_Type);

   package Option_Target_Type_Options is
     new Proto_Support.Options (Option_Target_Type);

   type Idempotency_Level is
     (IDEMPOTENCY_UNKNOWN, NO_SIDE_EFFECTS, IDEMPOTENT);

   for Idempotency_Level use
     (IDEMPOTENCY_UNKNOWN => 0, NO_SIDE_EFFECTS     => 1,
      IDEMPOTENT          => 2);

   package Idempotency_Level_Vectors is
     new Proto_Support.Vectors (Idempotency_Level);

   package Idempotency_Level_Options is
     new Proto_Support.Options (Idempotency_Level);

   type Field_Presence is
     (FIELD_PRESENCE_UNKNOWN, EXPLICIT, IMPLICIT, LEGACY_REQUIRED);

   for Field_Presence use
     (FIELD_PRESENCE_UNKNOWN => 0, EXPLICIT               => 1,
      IMPLICIT               => 2, LEGACY_REQUIRED        => 3);

   package Field_Presence_Vectors is
     new Proto_Support.Vectors (Field_Presence);

   package Field_Presence_Options is
     new Proto_Support.Options (Field_Presence);

   type Enum_Type is (ENUM_TYPE_UNKNOWN, OPEN, CLOSED);

   for Enum_Type use
     (ENUM_TYPE_UNKNOWN => 0, OPEN              => 1, CLOSED            => 2);

   package Enum_Type_Vectors is new Proto_Support.Vectors (Enum_Type);

   package Enum_Type_Options is new Proto_Support.Options (Enum_Type);

   type Repeated_Field_Encoding is
     (REPEATED_FIELD_ENCODING_UNKNOWN, PACKED, EXPANDED);

   for Repeated_Field_Encoding use
     (REPEATED_FIELD_ENCODING_UNKNOWN => 0,
      PACKED                          => 1,
      EXPANDED                        => 2);

   package Repeated_Field_Encoding_Vectors is
     new Proto_Support.Vectors (Repeated_Field_Encoding);

   package Repeated_Field_Encoding_Options is
     new Proto_Support.Options (Repeated_Field_Encoding);

   type Utf_8Validation is (UTF8_VALIDATION_UNKNOWN, VERIFY, NONE);

   for Utf_8Validation use
     (UTF8_VALIDATION_UNKNOWN => 0, VERIFY                  => 2,
      NONE                    => 3);

   package Utf_8Validation_Vectors is
     new Proto_Support.Vectors (Utf_8Validation);

   package Utf_8Validation_Options is
     new Proto_Support.Options (Utf_8Validation);

   type Message_Encoding is
     (MESSAGE_ENCODING_UNKNOWN, LENGTH_PREFIXED, DELIMITED);

   for Message_Encoding use
     (MESSAGE_ENCODING_UNKNOWN => 0, LENGTH_PREFIXED          => 1,
      DELIMITED                => 2);

   package Message_Encoding_Vectors is
     new Proto_Support.Vectors (Message_Encoding);

   package Message_Encoding_Options is
     new Proto_Support.Options (Message_Encoding);

   type Json_Format is (JSON_FORMAT_UNKNOWN, ALLOW, LEGACY_BEST_EFFORT);

   for Json_Format use
     (JSON_FORMAT_UNKNOWN => 0, ALLOW               => 1,
      LEGACY_BEST_EFFORT  => 2);

   package Json_Format_Vectors is new Proto_Support.Vectors (Json_Format);

   package Json_Format_Options is new Proto_Support.Options (Json_Format);

   type Enforce_Naming_Style is
     (ENFORCE_NAMING_STYLE_UNKNOWN, STYLE2024, STYLE_LEGACY);

   for Enforce_Naming_Style use
     (ENFORCE_NAMING_STYLE_UNKNOWN => 0, STYLE2024                    => 1,
      STYLE_LEGACY                 => 2);

   package Enforce_Naming_Style_Vectors is
     new Proto_Support.Vectors (Enforce_Naming_Style);

   package Enforce_Naming_Style_Options is
     new Proto_Support.Options (Enforce_Naming_Style);

   type Default_Symbol_Visibility is
     (DEFAULT_SYMBOL_VISIBILITY_UNKNOWN, EXPORT_ALL, EXPORT_TOP_LEVEL,
      LOCAL_ALL, STRICT);

   for Default_Symbol_Visibility use
     (DEFAULT_SYMBOL_VISIBILITY_UNKNOWN => 0,
      EXPORT_ALL                        => 1,
      EXPORT_TOP_LEVEL                  => 2,
      LOCAL_ALL                         => 3,
      STRICT                            => 4);

   package Default_Symbol_Visibility_Vectors is
     new Proto_Support.Vectors (Default_Symbol_Visibility);

   package Default_Symbol_Visibility_Options is
     new Proto_Support.Options (Default_Symbol_Visibility);

   type Semantic is (NONE, SET, ALIAS);

   for Semantic use (NONE  => 0, SET   => 1, ALIAS => 2);

   package Semantic_Vectors is new Proto_Support.Vectors (Semantic);

   package Semantic_Options is new Proto_Support.Options (Semantic);

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

   type Extension_Range_Options_Vector is tagged private
     with Variable_Indexing => Get_Extension_Range_Options_Variable_Reference,
     Constant_Indexing => Get_Extension_Range_Options_Constant_Reference;

   type Declaration_Vector is tagged private
     with Variable_Indexing => Get_Declaration_Variable_Reference,
     Constant_Indexing => Get_Declaration_Constant_Reference;

   type Field_Descriptor_Proto_Vector is tagged private
     with Variable_Indexing => Get_Field_Descriptor_Proto_Variable_Reference,
     Constant_Indexing => Get_Field_Descriptor_Proto_Constant_Reference;

   type Oneof_Descriptor_Proto_Vector is tagged private
     with Variable_Indexing => Get_Oneof_Descriptor_Proto_Variable_Reference,
     Constant_Indexing => Get_Oneof_Descriptor_Proto_Constant_Reference;

   type Enum_Descriptor_Proto_Vector is tagged private
     with Variable_Indexing => Get_Enum_Descriptor_Proto_Variable_Reference,
     Constant_Indexing => Get_Enum_Descriptor_Proto_Constant_Reference;

   type Enum_Reserved_Range_Vector is tagged private
     with Variable_Indexing => Get_Enum_Reserved_Range_Variable_Reference,
     Constant_Indexing => Get_Enum_Reserved_Range_Constant_Reference;

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

   type Edition_Default_Vector is tagged private
     with Variable_Indexing => Get_Edition_Default_Variable_Reference,
     Constant_Indexing => Get_Edition_Default_Constant_Reference;

   type Feature_Support_Vector is tagged private
     with Variable_Indexing => Get_Feature_Support_Variable_Reference,
     Constant_Indexing => Get_Feature_Support_Constant_Reference;

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

   type Feature_Set_Vector is tagged private
     with Variable_Indexing => Get_Feature_Set_Variable_Reference,
     Constant_Indexing => Get_Feature_Set_Constant_Reference;

   type Visibility_Feature_Vector is tagged private
     with Variable_Indexing => Get_Visibility_Feature_Variable_Reference,
     Constant_Indexing => Get_Visibility_Feature_Constant_Reference;

   type Feature_Set_Defaults_Vector is tagged private
     with Variable_Indexing => Get_Feature_Set_Defaults_Variable_Reference,
     Constant_Indexing => Get_Feature_Set_Defaults_Constant_Reference;

   type Feature_Set_Edition_Default_Vector is tagged private
     with Variable_Indexing =>
       Get_Feature_Set_Edition_Default_Variable_Reference,
     Constant_Indexing => Get_Feature_Set_Edition_Default_Constant_Reference;

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

   type Reserved_Range is
     record
        Start     : Proto_Support.Integer_32_Options.Option;
        Proto_End : Proto_Support.Integer_32_Options.Option;
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

   type Declaration is
     record
        Number     : Proto_Support.Integer_32_Options.Option;
        Full_Name  : Proto_Support.Universal_String_Options.Option;
        Proto_Type : Proto_Support.Universal_String_Options.Option;
        Reserved   : Proto_Support.Boolean_Options.Option;
        Repeated   : Proto_Support.Boolean_Options.Option;
     end record;

   type Optional_Declaration  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Declaration;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Declaration_Vector) return Natural;

   procedure Clear (Self : in out Declaration_Vector);

   procedure Append (Self : in out Declaration_Vector; V    : Declaration);

   type Declaration_Variable_Reference
     (Element : not null access Declaration) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Declaration_Variable_Reference
    (Self  : aliased in out Declaration_Vector;
     Index : Positive)
      return Declaration_Variable_Reference
     with Inline;

   type Declaration_Constant_Reference
     (Element : not null access constant Declaration) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Declaration_Constant_Reference
    (Self  : aliased Declaration_Vector;
     Index : Positive)
      return Declaration_Constant_Reference
     with Inline;

   type Enum_Reserved_Range is
     record
        Start     : Proto_Support.Integer_32_Options.Option;
        Proto_End : Proto_Support.Integer_32_Options.Option;
     end record;

   type Optional_Enum_Reserved_Range  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Enum_Reserved_Range;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Enum_Reserved_Range_Vector) return Natural;

   procedure Clear (Self : in out Enum_Reserved_Range_Vector);

   procedure Append
    (Self : in out Enum_Reserved_Range_Vector;
     V    : Enum_Reserved_Range);

   type Enum_Reserved_Range_Variable_Reference
     (Element : not null access Enum_Reserved_Range) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Enum_Reserved_Range_Variable_Reference
    (Self  : aliased in out Enum_Reserved_Range_Vector;
     Index : Positive)
      return Enum_Reserved_Range_Variable_Reference
     with Inline;

   type Enum_Reserved_Range_Constant_Reference
     (Element : not null access constant Enum_Reserved_Range) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Enum_Reserved_Range_Constant_Reference
    (Self  : aliased Enum_Reserved_Range_Vector;
     Index : Positive)
      return Enum_Reserved_Range_Constant_Reference
     with Inline;

   type Edition_Default is
     record
        Edition : Google.Protobuf.Descriptor.Edition_Options.Option;
        Value   : Proto_Support.Universal_String_Options.Option;
     end record;

   type Optional_Edition_Default  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Edition_Default;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Edition_Default_Vector) return Natural;

   procedure Clear (Self : in out Edition_Default_Vector);

   procedure Append
    (Self : in out Edition_Default_Vector;
     V    : Edition_Default);

   type Edition_Default_Variable_Reference
     (Element : not null access Edition_Default) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Edition_Default_Variable_Reference
    (Self  : aliased in out Edition_Default_Vector;
     Index : Positive)
      return Edition_Default_Variable_Reference
     with Inline;

   type Edition_Default_Constant_Reference
     (Element : not null access constant Edition_Default) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Edition_Default_Constant_Reference
    (Self  : aliased Edition_Default_Vector;
     Index : Positive)
      return Edition_Default_Constant_Reference
     with Inline;

   type Feature_Support is
     record
        Edition_Introduced  : Google.Protobuf.Descriptor.Edition_Options
          .Option;
        Edition_Deprecated  : Google.Protobuf.Descriptor.Edition_Options
          .Option;
        Deprecation_Warning : Proto_Support.Universal_String_Options.Option;
        Edition_Removed     : Google.Protobuf.Descriptor.Edition_Options
          .Option;
        Removal_Error       : Proto_Support.Universal_String_Options.Option;
     end record;

   type Optional_Feature_Support  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Feature_Support;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Feature_Support_Vector) return Natural;

   procedure Clear (Self : in out Feature_Support_Vector);

   procedure Append
    (Self : in out Feature_Support_Vector;
     V    : Feature_Support);

   type Feature_Support_Variable_Reference
     (Element : not null access Feature_Support) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Feature_Support_Variable_Reference
    (Self  : aliased in out Feature_Support_Vector;
     Index : Positive)
      return Feature_Support_Variable_Reference
     with Inline;

   type Feature_Support_Constant_Reference
     (Element : not null access constant Feature_Support) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Feature_Support_Constant_Reference
    (Self  : aliased Feature_Support_Vector;
     Index : Positive)
      return Feature_Support_Constant_Reference
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
        Identifier_Value   : Proto_Support.Universal_String_Options.Option;
        Positive_Int_Value : Proto_Support.Unsigned_64_Options.Option;
        Negative_Int_Value : Proto_Support.Integer_64_Options.Option;
        Double_Value       : Proto_Support.IEEE_Float_64_Options.Option;
        String_Value       : Proto_Support.Stream_Element_Vector_Options
          .Option;
        Aggregate_Value    : Proto_Support.Universal_String_Options.Option;
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

   type Visibility_Feature is null record;

   type Optional_Visibility_Feature  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Visibility_Feature;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Visibility_Feature_Vector) return Natural;

   procedure Clear (Self : in out Visibility_Feature_Vector);

   procedure Append
    (Self : in out Visibility_Feature_Vector;
     V    : Visibility_Feature);

   type Visibility_Feature_Variable_Reference
     (Element : not null access Visibility_Feature) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Visibility_Feature_Variable_Reference
    (Self  : aliased in out Visibility_Feature_Vector;
     Index : Positive)
      return Visibility_Feature_Variable_Reference
     with Inline;

   type Visibility_Feature_Constant_Reference
     (Element : not null access constant Visibility_Feature) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Visibility_Feature_Constant_Reference
    (Self  : aliased Visibility_Feature_Vector;
     Index : Positive)
      return Visibility_Feature_Constant_Reference
     with Inline;

   type Feature_Set is
     record
        Field_Presence            : Google.Protobuf.Descriptor
          .Field_Presence_Options.Option;
        Enum_Type                 : Google.Protobuf.Descriptor
          .Enum_Type_Options.Option;
        Repeated_Field_Encoding   : Google.Protobuf.Descriptor
          .Repeated_Field_Encoding_Options.Option;
        Utf_8_Validation          : Google.Protobuf.Descriptor
          .Utf_8Validation_Options.Option;
        Message_Encoding          : Google.Protobuf.Descriptor
          .Message_Encoding_Options.Option;
        Json_Format               : Google.Protobuf.Descriptor
          .Json_Format_Options.Option;
        Enforce_Naming_Style      : Google.Protobuf.Descriptor
          .Enforce_Naming_Style_Options.Option;
        Default_Symbol_Visibility : Google.Protobuf.Descriptor
          .Default_Symbol_Visibility_Options.Option;
     end record;

   type Optional_Feature_Set  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Feature_Set;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Feature_Set_Vector) return Natural;

   procedure Clear (Self : in out Feature_Set_Vector);

   procedure Append (Self : in out Feature_Set_Vector; V    : Feature_Set);

   type Feature_Set_Variable_Reference
     (Element : not null access Feature_Set) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Feature_Set_Variable_Reference
    (Self  : aliased in out Feature_Set_Vector;
     Index : Positive)
      return Feature_Set_Variable_Reference
     with Inline;

   type Feature_Set_Constant_Reference
     (Element : not null access constant Feature_Set) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Feature_Set_Constant_Reference
    (Self  : aliased Feature_Set_Vector;
     Index : Positive)
      return Feature_Set_Constant_Reference
     with Inline;

   type Feature_Set_Edition_Default is
     record
        Edition              : Google.Protobuf.Descriptor.Edition_Options
          .Option;
        Overridable_Features : Google.Protobuf.Descriptor.Optional_Feature_Set;
        Fixed_Features       : Google.Protobuf.Descriptor.Optional_Feature_Set;
     end record;

   type Optional_Feature_Set_Edition_Default  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Feature_Set_Edition_Default;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Feature_Set_Edition_Default_Vector) return Natural;

   procedure Clear (Self : in out Feature_Set_Edition_Default_Vector);

   procedure Append
    (Self : in out Feature_Set_Edition_Default_Vector;
     V    : Feature_Set_Edition_Default);

   type Feature_Set_Edition_Default_Variable_Reference
     (Element : not null access Feature_Set_Edition_Default) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Feature_Set_Edition_Default_Variable_Reference
    (Self  : aliased in out Feature_Set_Edition_Default_Vector;
     Index : Positive)
      return Feature_Set_Edition_Default_Variable_Reference
     with Inline;

   type Feature_Set_Edition_Default_Constant_Reference
     (Element : not null access constant Feature_Set_Edition_Default) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Feature_Set_Edition_Default_Constant_Reference
    (Self  : aliased Feature_Set_Edition_Default_Vector;
     Index : Positive)
      return Feature_Set_Edition_Default_Constant_Reference
     with Inline;

   type Feature_Set_Defaults is
     record
        Defaults        : Google.Protobuf.Descriptor
          .Feature_Set_Edition_Default_Vector;
        Minimum_Edition : Google.Protobuf.Descriptor.Edition_Options.Option;
        Maximum_Edition : Google.Protobuf.Descriptor.Edition_Options.Option;
     end record;

   type Optional_Feature_Set_Defaults  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Feature_Set_Defaults;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Feature_Set_Defaults_Vector) return Natural;

   procedure Clear (Self : in out Feature_Set_Defaults_Vector);

   procedure Append
    (Self : in out Feature_Set_Defaults_Vector;
     V    : Feature_Set_Defaults);

   type Feature_Set_Defaults_Variable_Reference
     (Element : not null access Feature_Set_Defaults) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Feature_Set_Defaults_Variable_Reference
    (Self  : aliased in out Feature_Set_Defaults_Vector;
     Index : Positive)
      return Feature_Set_Defaults_Variable_Reference
     with Inline;

   type Feature_Set_Defaults_Constant_Reference
     (Element : not null access constant Feature_Set_Defaults) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Feature_Set_Defaults_Constant_Reference
    (Self  : aliased Feature_Set_Defaults_Vector;
     Index : Positive)
      return Feature_Set_Defaults_Constant_Reference
     with Inline;

   type Location is
     record
        Path                      : Proto_Support.Integer_32_Vectors.Vector;
        Span                      : Proto_Support.Integer_32_Vectors.Vector;
        Leading_Comments          : Proto_Support.Universal_String_Options
          .Option;
        Trailing_Comments         : Proto_Support.Universal_String_Options
          .Option;
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
        Path        : Proto_Support.Integer_32_Vectors.Vector;
        Source_File : Proto_Support.Universal_String_Options.Option;
        Proto_Begin : Proto_Support.Integer_32_Options.Option;
        Proto_End   : Proto_Support.Integer_32_Options.Option;
        Semantic    : Google.Protobuf.Descriptor.Semantic_Options.Option;
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

   type Extension_Range_Options is
     record
        Uninterpreted_Option : Google.Protobuf.Descriptor
          .Uninterpreted_Option_Vector;
        Declaration          : Google.Protobuf.Descriptor.Declaration_Vector;
        Features             : Google.Protobuf.Descriptor.Optional_Feature_Set;
        Verification         : Google.Protobuf.Descriptor
          .Verification_State_Options.Option;
     end record;

   type Optional_Extension_Range_Options  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Descriptor.Extension_Range_Options;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Extension_Range_Options_Vector) return Natural;

   procedure Clear (Self : in out Extension_Range_Options_Vector);

   procedure Append
    (Self : in out Extension_Range_Options_Vector;
     V    : Extension_Range_Options);

   type Extension_Range_Options_Variable_Reference
     (Element : not null access Extension_Range_Options) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Extension_Range_Options_Variable_Reference
    (Self  : aliased in out Extension_Range_Options_Vector;
     Index : Positive)
      return Extension_Range_Options_Variable_Reference
     with Inline;

   type Extension_Range_Options_Constant_Reference
     (Element : not null access constant Extension_Range_Options) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Extension_Range_Options_Constant_Reference
    (Self  : aliased Extension_Range_Options_Vector;
     Index : Positive)
      return Extension_Range_Options_Constant_Reference
     with Inline;

   type File_Options is
     record
        Java_Package                  : Proto_Support.Universal_String_Options
          .Option;
        Java_Outer_Classname          : Proto_Support.Universal_String_Options
          .Option;
        Java_Multiple_Files           : Proto_Support.Boolean_Options.Option;
        Java_Generate_Equals_And_Hash : Proto_Support.Boolean_Options.Option;
        Java_String_Check_Utf_8       : Proto_Support.Boolean_Options.Option;
        Optimize_For                  : Google.Protobuf.Descriptor
          .Optimize_Mode_Options.Option;
        Go_Package                    : Proto_Support.Universal_String_Options
          .Option;
        Cc_Generic_Services           : Proto_Support.Boolean_Options.Option;
        Java_Generic_Services         : Proto_Support.Boolean_Options.Option;
        Py_Generic_Services           : Proto_Support.Boolean_Options.Option;
        Deprecated                    : Proto_Support.Boolean_Options.Option;
        Cc_Enable_Arenas              : Proto_Support.Boolean_Options.Option;
        Objc_Class_Prefix             : Proto_Support.Universal_String_Options
          .Option;
        Csharp_Namespace              : Proto_Support.Universal_String_Options
          .Option;
        Swift_Prefix                  : Proto_Support.Universal_String_Options
          .Option;
        Php_Class_Prefix              : Proto_Support.Universal_String_Options
          .Option;
        Php_Namespace                 : Proto_Support.Universal_String_Options
          .Option;
        Php_Metadata_Namespace        : Proto_Support.Universal_String_Options
          .Option;
        Ruby_Package                  : Proto_Support.Universal_String_Options
          .Option;
        Features                      : Google.Protobuf.Descriptor
          .Optional_Feature_Set;
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
        Message_Set_Wire_Format                : Proto_Support.Boolean_Options
          .Option;
        No_Standard_Descriptor_Accessor        : Proto_Support.Boolean_Options
          .Option;
        Deprecated                             : Proto_Support.Boolean_Options
          .Option;
        Map_Entry                              : Proto_Support.Boolean_Options
          .Option;
        Deprecated_Legacy_Json_Field_Conflicts : Proto_Support.Boolean_Options
          .Option;
        Features                               : Google.Protobuf.Descriptor
          .Optional_Feature_Set;
        Uninterpreted_Option                   : Google.Protobuf.Descriptor
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
        Ctype                : Google.Protobuf.Descriptor.CType_Options.Option;
        Packed               : Proto_Support.Boolean_Options.Option;
        Jstype               : Google.Protobuf.Descriptor.JSType_Options
          .Option;
        Lazy                 : Proto_Support.Boolean_Options.Option;
        Unverified_Lazy      : Proto_Support.Boolean_Options.Option;
        Deprecated           : Proto_Support.Boolean_Options.Option;
        Weak                 : Proto_Support.Boolean_Options.Option;
        Debug_Redact         : Proto_Support.Boolean_Options.Option;
        Retention            : Google.Protobuf.Descriptor
          .Option_Retention_Options.Option;
        Targets              : Google.Protobuf.Descriptor
          .Option_Target_Type_Vectors.Vector;
        Edition_Defaults     : Google.Protobuf.Descriptor
          .Edition_Default_Vector;
        Features             : Google.Protobuf.Descriptor.Optional_Feature_Set;
        Feature_Support      : Google.Protobuf.Descriptor
          .Optional_Feature_Support;
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
        Features             : Google.Protobuf.Descriptor.Optional_Feature_Set;
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
        Allow_Alias                            : Proto_Support.Boolean_Options
          .Option;
        Deprecated                             : Proto_Support.Boolean_Options
          .Option;
        Deprecated_Legacy_Json_Field_Conflicts : Proto_Support.Boolean_Options
          .Option;
        Features                               : Google.Protobuf.Descriptor
          .Optional_Feature_Set;
        Uninterpreted_Option                   : Google.Protobuf.Descriptor
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
        Deprecated           : Proto_Support.Boolean_Options.Option;
        Features             : Google.Protobuf.Descriptor.Optional_Feature_Set;
        Debug_Redact         : Proto_Support.Boolean_Options.Option;
        Feature_Support      : Google.Protobuf.Descriptor
          .Optional_Feature_Support;
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
        Features             : Google.Protobuf.Descriptor.Optional_Feature_Set;
        Deprecated           : Proto_Support.Boolean_Options.Option;
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
        Deprecated           : Proto_Support.Boolean_Options.Option;
        Idempotency_Level    : Google.Protobuf.Descriptor
          .Idempotency_Level_Options.Option;
        Features             : Google.Protobuf.Descriptor.Optional_Feature_Set;
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

   type File_Descriptor_Proto is
     record
        Name              : Proto_Support.Universal_String_Options.Option;
        Proto_Package     : Proto_Support.Universal_String_Options.Option;
        Dependency        : League.String_Vectors.Universal_String_Vector;
        Public_Dependency : Proto_Support.Integer_32_Vectors.Vector;
        Weak_Dependency   : Proto_Support.Integer_32_Vectors.Vector;
        Option_Dependency : League.String_Vectors.Universal_String_Vector;
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
        Syntax            : Proto_Support.Universal_String_Options.Option;
        Edition           : Google.Protobuf.Descriptor.Edition_Options.Option;
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

   type Extension_Range is
     record
        Start     : Proto_Support.Integer_32_Options.Option;
        Proto_End : Proto_Support.Integer_32_Options.Option;
        Options   : Google.Protobuf.Descriptor
          .Optional_Extension_Range_Options;
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

   type Descriptor_Proto is
     record
        Name            : Proto_Support.Universal_String_Options.Option;
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
        Visibility      : Google.Protobuf.Descriptor.Symbol_Visibility_Options
          .Option;
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
        Name             : Proto_Support.Universal_String_Options.Option;
        Number           : Proto_Support.Integer_32_Options.Option;
        Label            : Google.Protobuf.Descriptor.Label_Options.Option;
        Proto_Type       : Google.Protobuf.Descriptor.Proto_Type_Options
          .Option;
        Type_Name        : Proto_Support.Universal_String_Options.Option;
        Extendee         : Proto_Support.Universal_String_Options.Option;
        Default_Value    : Proto_Support.Universal_String_Options.Option;
        Oneof_Index      : Proto_Support.Integer_32_Options.Option;
        Json_Name        : Proto_Support.Universal_String_Options.Option;
        Options          : Google.Protobuf.Descriptor.Optional_Field_Options;
        Proto_3_Optional : Proto_Support.Boolean_Options.Option;
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
        Name    : Proto_Support.Universal_String_Options.Option;
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
        Name           : Proto_Support.Universal_String_Options.Option;
        Value          : Google.Protobuf.Descriptor
          .Enum_Value_Descriptor_Proto_Vector;
        Options        : Google.Protobuf.Descriptor.Optional_Enum_Options;
        Reserved_Range : Google.Protobuf.Descriptor.Enum_Reserved_Range_Vector;
        Reserved_Name  : League.String_Vectors.Universal_String_Vector;
        Visibility     : Google.Protobuf.Descriptor.Symbol_Visibility_Options
          .Option;
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
        Name    : Proto_Support.Universal_String_Options.Option;
        Number  : Proto_Support.Integer_32_Options.Option;
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
        Name    : Proto_Support.Universal_String_Options.Option;
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
        Name             : Proto_Support.Universal_String_Options.Option;
        Input_Type       : Proto_Support.Universal_String_Options.Option;
        Output_Type      : Proto_Support.Universal_String_Options.Option;
        Options          : Google.Protobuf.Descriptor.Optional_Method_Options;
        Client_Streaming : Proto_Support.Boolean_Options.Option;
        Server_Streaming : Proto_Support.Boolean_Options.Option;
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

   procedure Read_Declaration
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Declaration);

   procedure Write_Declaration
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Declaration);

   for Declaration'Read use Read_Declaration;

   for Declaration'Write use Write_Declaration;

   type Declaration_Array is array (Positive range <>) of aliased Declaration;

   type Declaration_Array_Access is access Declaration_Array;

   type Declaration_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Declaration_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Declaration_Vector);

   overriding procedure Finalize (Self : in out Declaration_Vector);

   procedure Read_Extension_Range_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Extension_Range_Options);

   procedure Write_Extension_Range_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Extension_Range_Options);

   for Extension_Range_Options'Read use Read_Extension_Range_Options;

   for Extension_Range_Options'Write use Write_Extension_Range_Options;

   type Extension_Range_Options_Array is
     array (Positive range <>) of aliased Extension_Range_Options;

   type Extension_Range_Options_Array_Access is
     access Extension_Range_Options_Array;

   type Extension_Range_Options_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Extension_Range_Options_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Extension_Range_Options_Vector);

   overriding procedure Finalize
    (Self : in out Extension_Range_Options_Vector);

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

   procedure Read_Enum_Reserved_Range
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Enum_Reserved_Range);

   procedure Write_Enum_Reserved_Range
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Enum_Reserved_Range);

   for Enum_Reserved_Range'Read use Read_Enum_Reserved_Range;

   for Enum_Reserved_Range'Write use Write_Enum_Reserved_Range;

   type Enum_Reserved_Range_Array is
     array (Positive range <>) of aliased Enum_Reserved_Range;

   type Enum_Reserved_Range_Array_Access is access Enum_Reserved_Range_Array;

   type Enum_Reserved_Range_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Enum_Reserved_Range_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Enum_Reserved_Range_Vector);

   overriding procedure Finalize (Self : in out Enum_Reserved_Range_Vector);

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

   procedure Read_Edition_Default
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Edition_Default);

   procedure Write_Edition_Default
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Edition_Default);

   for Edition_Default'Read use Read_Edition_Default;

   for Edition_Default'Write use Write_Edition_Default;

   type Edition_Default_Array is
     array (Positive range <>) of aliased Edition_Default;

   type Edition_Default_Array_Access is access Edition_Default_Array;

   type Edition_Default_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Edition_Default_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Edition_Default_Vector);

   overriding procedure Finalize (Self : in out Edition_Default_Vector);

   procedure Read_Feature_Support
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Feature_Support);

   procedure Write_Feature_Support
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Feature_Support);

   for Feature_Support'Read use Read_Feature_Support;

   for Feature_Support'Write use Write_Feature_Support;

   type Feature_Support_Array is
     array (Positive range <>) of aliased Feature_Support;

   type Feature_Support_Array_Access is access Feature_Support_Array;

   type Feature_Support_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Feature_Support_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Feature_Support_Vector);

   overriding procedure Finalize (Self : in out Feature_Support_Vector);

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

   procedure Read_Visibility_Feature
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Visibility_Feature);

   procedure Write_Visibility_Feature
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Visibility_Feature);

   for Visibility_Feature'Read use Read_Visibility_Feature;

   for Visibility_Feature'Write use Write_Visibility_Feature;

   type Visibility_Feature_Array is
     array (Positive range <>) of aliased Visibility_Feature;

   type Visibility_Feature_Array_Access is access Visibility_Feature_Array;

   type Visibility_Feature_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Visibility_Feature_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Visibility_Feature_Vector);

   overriding procedure Finalize (Self : in out Visibility_Feature_Vector);

   procedure Read_Feature_Set
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Feature_Set);

   procedure Write_Feature_Set
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Feature_Set);

   for Feature_Set'Read use Read_Feature_Set;

   for Feature_Set'Write use Write_Feature_Set;

   type Feature_Set_Array is array (Positive range <>) of aliased Feature_Set;

   type Feature_Set_Array_Access is access Feature_Set_Array;

   type Feature_Set_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Feature_Set_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Feature_Set_Vector);

   overriding procedure Finalize (Self : in out Feature_Set_Vector);

   procedure Read_Feature_Set_Edition_Default
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Feature_Set_Edition_Default);

   procedure Write_Feature_Set_Edition_Default
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Feature_Set_Edition_Default);

   for Feature_Set_Edition_Default'Read use Read_Feature_Set_Edition_Default;

   for Feature_Set_Edition_Default'Write use Write_Feature_Set_Edition_Default;

   type Feature_Set_Edition_Default_Array is
     array (Positive range <>) of aliased Feature_Set_Edition_Default;

   type Feature_Set_Edition_Default_Array_Access is
     access Feature_Set_Edition_Default_Array;

   type Feature_Set_Edition_Default_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Feature_Set_Edition_Default_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust
    (Self : in out Feature_Set_Edition_Default_Vector);

   overriding procedure Finalize
    (Self : in out Feature_Set_Edition_Default_Vector);

   procedure Read_Feature_Set_Defaults
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Feature_Set_Defaults);

   procedure Write_Feature_Set_Defaults
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Feature_Set_Defaults);

   for Feature_Set_Defaults'Read use Read_Feature_Set_Defaults;

   for Feature_Set_Defaults'Write use Write_Feature_Set_Defaults;

   type Feature_Set_Defaults_Array is
     array (Positive range <>) of aliased Feature_Set_Defaults;

   type Feature_Set_Defaults_Array_Access is access Feature_Set_Defaults_Array;

   type Feature_Set_Defaults_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Feature_Set_Defaults_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Feature_Set_Defaults_Vector);

   overriding procedure Finalize (Self : in out Feature_Set_Defaults_Vector);

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
