with Ada.Finalization;
with Ada.Streams;
with Interfaces;
with League.Stream_Element_Vectors;
with League.String_Vectors;
with League.Strings;
with PB_Support.Unsigned_32_Vectors;
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

   type File_Descriptor_Set_Vector is tagged private;

   type File_Descriptor_Proto_Vector is tagged private;

   type Descriptor_Proto_Vector is tagged private;

   type Extension_Range_Vector is tagged private;

   type Reserved_Range_Vector is tagged private;

   type Field_Descriptor_Proto_Vector is tagged private;

   type Oneof_Descriptor_Proto_Vector is tagged private;

   type Enum_Descriptor_Proto_Vector is tagged private;

   type Enum_Value_Descriptor_Proto_Vector is tagged private;

   type Service_Descriptor_Proto_Vector is tagged private;

   type Method_Descriptor_Proto_Vector is tagged private;

   type File_Options_Vector is tagged private;

   type Message_Options_Vector is tagged private;

   type Field_Options_Vector is tagged private;

   type Oneof_Options_Vector is tagged private;

   type Enum_Options_Vector is tagged private;

   type Enum_Value_Options_Vector is tagged private;

   type Service_Options_Vector is tagged private;

   type Method_Options_Vector is tagged private;

   type Uninterpreted_Option_Vector is tagged private;

   type Name_Part_Vector is tagged private;

   type Source_Code_Info_Vector is tagged private;

   type Location_Vector is tagged private;

   type Generated_Code_Info_Vector is tagged private;

   type Annotation_Vector is tagged private;

   type File_Descriptor_Set is
     record
        File : Google.Protobuf.Descriptor.File_Descriptor_Proto_Vector;
     end record;

   type Optional_File_Descriptor_Set (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : File_Descriptor_Set;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : File_Descriptor_Set_Vector) return Natural;

   function Get
    (Self  : File_Descriptor_Set_Vector;
     Index : Positive)
      return File_Descriptor_Set;

   procedure Clear (Self : in out File_Descriptor_Set_Vector);

   procedure Append
    (Self  : in out File_Descriptor_Set_Vector;
     Value : File_Descriptor_Set);

   type Extension_Range is
     record
        Start  : Interfaces.Unsigned_32 := 0;
        PB_End : Interfaces.Unsigned_32 := 0;
     end record;

   type Optional_Extension_Range (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Extension_Range;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Extension_Range_Vector) return Natural;

   function Get
    (Self  : Extension_Range_Vector;
     Index : Positive)
      return Extension_Range;

   procedure Clear (Self : in out Extension_Range_Vector);

   procedure Append
    (Self  : in out Extension_Range_Vector;
     Value : Extension_Range);

   type Reserved_Range is
     record
        Start  : Interfaces.Unsigned_32 := 0;
        PB_End : Interfaces.Unsigned_32 := 0;
     end record;

   type Optional_Reserved_Range (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Reserved_Range;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Reserved_Range_Vector) return Natural;

   function Get
    (Self  : Reserved_Range_Vector;
     Index : Positive)
      return Reserved_Range;

   procedure Clear (Self : in out Reserved_Range_Vector);

   procedure Append
    (Self  : in out Reserved_Range_Vector;
     Value : Reserved_Range);

   type File_Options is
     record
        Java_Package                  : League.Strings.Universal_String;
        Java_Outer_Classname          : League.Strings.Universal_String;
        Java_Multiple_Files           : Boolean := False;
        Java_Generate_Equals_And_Hash : Boolean := False;
        Java_String_Check_Utf_8       : Boolean := False;
        Optimize_For                  : Google.Protobuf.Descriptor
          .Optimize_Mode := Google.Protobuf.Descriptor.SPEED;
        Go_Package                    : League.Strings.Universal_String;
        Cc_Generic_Services           : Boolean := False;
        Java_Generic_Services         : Boolean := False;
        Py_Generic_Services           : Boolean := False;
        Deprecated                    : Boolean := False;
        Cc_Enable_Arenas              : Boolean := False;
        Objc_Class_Prefix             : League.Strings.Universal_String;
        Csharp_Namespace              : League.Strings.Universal_String;
        Uninterpreted_Option          : Google.Protobuf.Descriptor
          .Uninterpreted_Option_Vector;
     end record;

   type Optional_File_Options (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : File_Options;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : File_Options_Vector) return Natural;

   function Get
    (Self  : File_Options_Vector;
     Index : Positive)
      return File_Options;

   procedure Clear (Self : in out File_Options_Vector);

   procedure Append (Self  : in out File_Options_Vector; Value : File_Options);

   type Message_Options is
     record
        Message_Set_Wire_Format         : Boolean := False;
        No_Standard_Descriptor_Accessor : Boolean := False;
        Deprecated                      : Boolean := False;
        Map_Entry                       : Boolean := False;
        Uninterpreted_Option            : Google.Protobuf.Descriptor
          .Uninterpreted_Option_Vector;
     end record;

   type Optional_Message_Options (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Message_Options;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Message_Options_Vector) return Natural;

   function Get
    (Self  : Message_Options_Vector;
     Index : Positive)
      return Message_Options;

   procedure Clear (Self : in out Message_Options_Vector);

   procedure Append
    (Self  : in out Message_Options_Vector;
     Value : Message_Options);

   type Field_Options is
     record
        Ctype                : Google.Protobuf.Descriptor.CType :=
          Google.Protobuf.Descriptor.STRING;
        Packed               : Boolean := False;
        Jstype               : Google.Protobuf.Descriptor.JSType :=
          Google.Protobuf.Descriptor.JS_NORMAL;
        Lazy                 : Boolean := False;
        Deprecated           : Boolean := False;
        Weak                 : Boolean := False;
        Uninterpreted_Option : Google.Protobuf.Descriptor
          .Uninterpreted_Option_Vector;
     end record;

   type Optional_Field_Options (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Field_Options;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Field_Options_Vector) return Natural;

   function Get
    (Self  : Field_Options_Vector;
     Index : Positive)
      return Field_Options;

   procedure Clear (Self : in out Field_Options_Vector);

   procedure Append
    (Self  : in out Field_Options_Vector;
     Value : Field_Options);

   type Oneof_Options is
     record
        Uninterpreted_Option : Google.Protobuf.Descriptor
          .Uninterpreted_Option_Vector;
     end record;

   type Optional_Oneof_Options (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Oneof_Options;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Oneof_Options_Vector) return Natural;

   function Get
    (Self  : Oneof_Options_Vector;
     Index : Positive)
      return Oneof_Options;

   procedure Clear (Self : in out Oneof_Options_Vector);

   procedure Append
    (Self  : in out Oneof_Options_Vector;
     Value : Oneof_Options);

   type Enum_Options is
     record
        Allow_Alias          : Boolean := False;
        Deprecated           : Boolean := False;
        Uninterpreted_Option : Google.Protobuf.Descriptor
          .Uninterpreted_Option_Vector;
     end record;

   type Optional_Enum_Options (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Enum_Options;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Enum_Options_Vector) return Natural;

   function Get
    (Self  : Enum_Options_Vector;
     Index : Positive)
      return Enum_Options;

   procedure Clear (Self : in out Enum_Options_Vector);

   procedure Append (Self  : in out Enum_Options_Vector; Value : Enum_Options);

   type Enum_Value_Options is
     record
        Deprecated           : Boolean := False;
        Uninterpreted_Option : Google.Protobuf.Descriptor
          .Uninterpreted_Option_Vector;
     end record;

   type Optional_Enum_Value_Options (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Enum_Value_Options;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Enum_Value_Options_Vector) return Natural;

   function Get
    (Self  : Enum_Value_Options_Vector;
     Index : Positive)
      return Enum_Value_Options;

   procedure Clear (Self : in out Enum_Value_Options_Vector);

   procedure Append
    (Self  : in out Enum_Value_Options_Vector;
     Value : Enum_Value_Options);

   type Service_Options is
     record
        Deprecated           : Boolean := False;
        Uninterpreted_Option : Google.Protobuf.Descriptor
          .Uninterpreted_Option_Vector;
     end record;

   type Optional_Service_Options (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Service_Options;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Service_Options_Vector) return Natural;

   function Get
    (Self  : Service_Options_Vector;
     Index : Positive)
      return Service_Options;

   procedure Clear (Self : in out Service_Options_Vector);

   procedure Append
    (Self  : in out Service_Options_Vector;
     Value : Service_Options);

   type Method_Options is
     record
        Deprecated           : Boolean := False;
        Uninterpreted_Option : Google.Protobuf.Descriptor
          .Uninterpreted_Option_Vector;
     end record;

   type Optional_Method_Options (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Method_Options;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Method_Options_Vector) return Natural;

   function Get
    (Self  : Method_Options_Vector;
     Index : Positive)
      return Method_Options;

   procedure Clear (Self : in out Method_Options_Vector);

   procedure Append
    (Self  : in out Method_Options_Vector;
     Value : Method_Options);

   type Name_Part is
     record
        Name_Part    : League.Strings.Universal_String;
        Is_Extension : Boolean := False;
     end record;

   type Optional_Name_Part (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Name_Part;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Name_Part_Vector) return Natural;

   function Get (Self  : Name_Part_Vector; Index : Positive) return Name_Part;

   procedure Clear (Self : in out Name_Part_Vector);

   procedure Append (Self  : in out Name_Part_Vector; Value : Name_Part);

   type Uninterpreted_Option is
     record
        Name               : Google.Protobuf.Descriptor.Name_Part_Vector;
        Identifier_Value   : League.Strings.Universal_String;
        Positive_Int_Value : Interfaces.Unsigned_64 := 0;
        Negative_Int_Value : Interfaces.Integer_64 := 0;
        Double_Value       : Interfaces.IEEE_Float_64 := 0.0;
        String_Value       : League.Stream_Element_Vectors
          .Stream_Element_Vector;
        Aggregate_Value    : League.Strings.Universal_String;
     end record;

   type Optional_Uninterpreted_Option (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Uninterpreted_Option;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Uninterpreted_Option_Vector) return Natural;

   function Get
    (Self  : Uninterpreted_Option_Vector;
     Index : Positive)
      return Uninterpreted_Option;

   procedure Clear (Self : in out Uninterpreted_Option_Vector);

   procedure Append
    (Self  : in out Uninterpreted_Option_Vector;
     Value : Uninterpreted_Option);

   type Location is
     record
        Path                      : PB_Support.Unsigned_32_Vectors.Vector;
        Span                      : PB_Support.Unsigned_32_Vectors.Vector;
        Leading_Comments          : League.Strings.Universal_String;
        Trailing_Comments         : League.Strings.Universal_String;
        Leading_Detached_Comments : League.String_Vectors
          .Universal_String_Vector;
     end record;

   type Optional_Location (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Location;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Location_Vector) return Natural;

   function Get (Self  : Location_Vector; Index : Positive) return Location;

   procedure Clear (Self : in out Location_Vector);

   procedure Append (Self  : in out Location_Vector; Value : Location);

   type Source_Code_Info is
     record
        Location : Google.Protobuf.Descriptor.Location_Vector;
     end record;

   type Optional_Source_Code_Info (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Source_Code_Info;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Source_Code_Info_Vector) return Natural;

   function Get
    (Self  : Source_Code_Info_Vector;
     Index : Positive)
      return Source_Code_Info;

   procedure Clear (Self : in out Source_Code_Info_Vector);

   procedure Append
    (Self  : in out Source_Code_Info_Vector;
     Value : Source_Code_Info);

   type Annotation is
     record
        Path        : PB_Support.Unsigned_32_Vectors.Vector;
        Source_File : League.Strings.Universal_String;
        PB_Begin    : Interfaces.Unsigned_32 := 0;
        PB_End      : Interfaces.Unsigned_32 := 0;
     end record;

   type Optional_Annotation (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Annotation;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Annotation_Vector) return Natural;

   function Get
    (Self  : Annotation_Vector;
     Index : Positive)
      return Annotation;

   procedure Clear (Self : in out Annotation_Vector);

   procedure Append (Self  : in out Annotation_Vector; Value : Annotation);

   type Generated_Code_Info is
     record
        Annotation : Google.Protobuf.Descriptor.Annotation_Vector;
     end record;

   type Optional_Generated_Code_Info (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Generated_Code_Info;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Generated_Code_Info_Vector) return Natural;

   function Get
    (Self  : Generated_Code_Info_Vector;
     Index : Positive)
      return Generated_Code_Info;

   procedure Clear (Self : in out Generated_Code_Info_Vector);

   procedure Append
    (Self  : in out Generated_Code_Info_Vector;
     Value : Generated_Code_Info);

   type File_Descriptor_Proto is
     record
        Name              : League.Strings.Universal_String;
        PB_Package        : League.Strings.Universal_String;
        Dependency        : League.String_Vectors.Universal_String_Vector;
        Public_Dependency : PB_Support.Unsigned_32_Vectors.Vector;
        Weak_Dependency   : PB_Support.Unsigned_32_Vectors.Vector;
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
        Syntax            : League.Strings.Universal_String;
     end record;

   type Optional_File_Descriptor_Proto (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : File_Descriptor_Proto;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : File_Descriptor_Proto_Vector) return Natural;

   function Get
    (Self  : File_Descriptor_Proto_Vector;
     Index : Positive)
      return File_Descriptor_Proto;

   procedure Clear (Self : in out File_Descriptor_Proto_Vector);

   procedure Append
    (Self  : in out File_Descriptor_Proto_Vector;
     Value : File_Descriptor_Proto);

   type Descriptor_Proto is
     record
        Name            : League.Strings.Universal_String;
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

   type Optional_Descriptor_Proto (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Descriptor_Proto;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Descriptor_Proto_Vector) return Natural;

   function Get
    (Self  : Descriptor_Proto_Vector;
     Index : Positive)
      return Descriptor_Proto;

   procedure Clear (Self : in out Descriptor_Proto_Vector);

   procedure Append
    (Self  : in out Descriptor_Proto_Vector;
     Value : Descriptor_Proto);

   type Field_Descriptor_Proto is
     record
        Name          : League.Strings.Universal_String;
        Number        : Interfaces.Unsigned_32 := 0;
        Label         : Google.Protobuf.Descriptor.Label :=
          Google.Protobuf.Descriptor.LABEL_OPTIONAL;
        PB_Type       : Google.Protobuf.Descriptor.PB_Type :=
          Google.Protobuf.Descriptor.TYPE_DOUBLE;
        Type_Name     : League.Strings.Universal_String;
        Extendee      : League.Strings.Universal_String;
        Default_Value : League.Strings.Universal_String;
        Oneof_Index   : Interfaces.Unsigned_32 := 0;
        Json_Name     : League.Strings.Universal_String;
        Options       : Google.Protobuf.Descriptor.Optional_Field_Options;
     end record;

   type Optional_Field_Descriptor_Proto (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Field_Descriptor_Proto;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Field_Descriptor_Proto_Vector) return Natural;

   function Get
    (Self  : Field_Descriptor_Proto_Vector;
     Index : Positive)
      return Field_Descriptor_Proto;

   procedure Clear (Self : in out Field_Descriptor_Proto_Vector);

   procedure Append
    (Self  : in out Field_Descriptor_Proto_Vector;
     Value : Field_Descriptor_Proto);

   type Oneof_Descriptor_Proto is
     record
        Name    : League.Strings.Universal_String;
        Options : Google.Protobuf.Descriptor.Optional_Oneof_Options;
     end record;

   type Optional_Oneof_Descriptor_Proto (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Oneof_Descriptor_Proto;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Oneof_Descriptor_Proto_Vector) return Natural;

   function Get
    (Self  : Oneof_Descriptor_Proto_Vector;
     Index : Positive)
      return Oneof_Descriptor_Proto;

   procedure Clear (Self : in out Oneof_Descriptor_Proto_Vector);

   procedure Append
    (Self  : in out Oneof_Descriptor_Proto_Vector;
     Value : Oneof_Descriptor_Proto);

   type Enum_Descriptor_Proto is
     record
        Name    : League.Strings.Universal_String;
        Value   : Google.Protobuf.Descriptor
          .Enum_Value_Descriptor_Proto_Vector;
        Options : Google.Protobuf.Descriptor.Optional_Enum_Options;
     end record;

   type Optional_Enum_Descriptor_Proto (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Enum_Descriptor_Proto;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Enum_Descriptor_Proto_Vector) return Natural;

   function Get
    (Self  : Enum_Descriptor_Proto_Vector;
     Index : Positive)
      return Enum_Descriptor_Proto;

   procedure Clear (Self : in out Enum_Descriptor_Proto_Vector);

   procedure Append
    (Self  : in out Enum_Descriptor_Proto_Vector;
     Value : Enum_Descriptor_Proto);

   type Enum_Value_Descriptor_Proto is
     record
        Name    : League.Strings.Universal_String;
        Number  : Interfaces.Unsigned_32 := 0;
        Options : Google.Protobuf.Descriptor.Optional_Enum_Value_Options;
     end record;

   type Optional_Enum_Value_Descriptor_Proto (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Enum_Value_Descriptor_Proto;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Enum_Value_Descriptor_Proto_Vector) return Natural;

   function Get
    (Self  : Enum_Value_Descriptor_Proto_Vector;
     Index : Positive)
      return Enum_Value_Descriptor_Proto;

   procedure Clear (Self : in out Enum_Value_Descriptor_Proto_Vector);

   procedure Append
    (Self  : in out Enum_Value_Descriptor_Proto_Vector;
     Value : Enum_Value_Descriptor_Proto);

   type Service_Descriptor_Proto is
     record
        Name    : League.Strings.Universal_String;
        Method  : Google.Protobuf.Descriptor.Method_Descriptor_Proto_Vector;
        Options : Google.Protobuf.Descriptor.Optional_Service_Options;
     end record;

   type Optional_Service_Descriptor_Proto (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Service_Descriptor_Proto;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Service_Descriptor_Proto_Vector) return Natural;

   function Get
    (Self  : Service_Descriptor_Proto_Vector;
     Index : Positive)
      return Service_Descriptor_Proto;

   procedure Clear (Self : in out Service_Descriptor_Proto_Vector);

   procedure Append
    (Self  : in out Service_Descriptor_Proto_Vector;
     Value : Service_Descriptor_Proto);

   type Method_Descriptor_Proto is
     record
        Name             : League.Strings.Universal_String;
        Input_Type       : League.Strings.Universal_String;
        Output_Type      : League.Strings.Universal_String;
        Options          : Google.Protobuf.Descriptor.Optional_Method_Options;
        Client_Streaming : Boolean := False;
        Server_Streaming : Boolean := False;
     end record;

   type Optional_Method_Descriptor_Proto (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Method_Descriptor_Proto;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Method_Descriptor_Proto_Vector) return Natural;

   function Get
    (Self  : Method_Descriptor_Proto_Vector;
     Index : Positive)
      return Method_Descriptor_Proto;

   procedure Clear (Self : in out Method_Descriptor_Proto_Vector);

   procedure Append
    (Self  : in out Method_Descriptor_Proto_Vector;
     Value : Method_Descriptor_Proto);
private

   procedure Read_File_Descriptor_Set
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : out File_Descriptor_Set);

   procedure Write_File_Descriptor_Set
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : File_Descriptor_Set);

   for File_Descriptor_Set'Read use Read_File_Descriptor_Set;

   for File_Descriptor_Set'Write use Write_File_Descriptor_Set;

   type File_Descriptor_Set_Array is
     array (Positive range <>) of File_Descriptor_Set;

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
     Value  : out File_Descriptor_Proto);

   procedure Write_File_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : File_Descriptor_Proto);

   for File_Descriptor_Proto'Read use Read_File_Descriptor_Proto;

   for File_Descriptor_Proto'Write use Write_File_Descriptor_Proto;

   type File_Descriptor_Proto_Array is
     array (Positive range <>) of File_Descriptor_Proto;

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
     Value  : out Extension_Range);

   procedure Write_Extension_Range
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Extension_Range);

   for Extension_Range'Read use Read_Extension_Range;

   for Extension_Range'Write use Write_Extension_Range;

   type Extension_Range_Array is array (Positive range <>) of Extension_Range;

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
     Value  : out Reserved_Range);

   procedure Write_Reserved_Range
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Reserved_Range);

   for Reserved_Range'Read use Read_Reserved_Range;

   for Reserved_Range'Write use Write_Reserved_Range;

   type Reserved_Range_Array is array (Positive range <>) of Reserved_Range;

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
     Value  : out Descriptor_Proto);

   procedure Write_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Descriptor_Proto);

   for Descriptor_Proto'Read use Read_Descriptor_Proto;

   for Descriptor_Proto'Write use Write_Descriptor_Proto;

   type Descriptor_Proto_Array is
     array (Positive range <>) of Descriptor_Proto;

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
     Value  : out Field_Descriptor_Proto);

   procedure Write_Field_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Field_Descriptor_Proto);

   for Field_Descriptor_Proto'Read use Read_Field_Descriptor_Proto;

   for Field_Descriptor_Proto'Write use Write_Field_Descriptor_Proto;

   type Field_Descriptor_Proto_Array is
     array (Positive range <>) of Field_Descriptor_Proto;

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
     Value  : out Oneof_Descriptor_Proto);

   procedure Write_Oneof_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Oneof_Descriptor_Proto);

   for Oneof_Descriptor_Proto'Read use Read_Oneof_Descriptor_Proto;

   for Oneof_Descriptor_Proto'Write use Write_Oneof_Descriptor_Proto;

   type Oneof_Descriptor_Proto_Array is
     array (Positive range <>) of Oneof_Descriptor_Proto;

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
     Value  : out Enum_Descriptor_Proto);

   procedure Write_Enum_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Enum_Descriptor_Proto);

   for Enum_Descriptor_Proto'Read use Read_Enum_Descriptor_Proto;

   for Enum_Descriptor_Proto'Write use Write_Enum_Descriptor_Proto;

   type Enum_Descriptor_Proto_Array is
     array (Positive range <>) of Enum_Descriptor_Proto;

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
     Value  : out Enum_Value_Descriptor_Proto);

   procedure Write_Enum_Value_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Enum_Value_Descriptor_Proto);

   for Enum_Value_Descriptor_Proto'Read use Read_Enum_Value_Descriptor_Proto;

   for Enum_Value_Descriptor_Proto'Write use Write_Enum_Value_Descriptor_Proto;

   type Enum_Value_Descriptor_Proto_Array is
     array (Positive range <>) of Enum_Value_Descriptor_Proto;

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
     Value  : out Service_Descriptor_Proto);

   procedure Write_Service_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Service_Descriptor_Proto);

   for Service_Descriptor_Proto'Read use Read_Service_Descriptor_Proto;

   for Service_Descriptor_Proto'Write use Write_Service_Descriptor_Proto;

   type Service_Descriptor_Proto_Array is
     array (Positive range <>) of Service_Descriptor_Proto;

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
     Value  : out Method_Descriptor_Proto);

   procedure Write_Method_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Method_Descriptor_Proto);

   for Method_Descriptor_Proto'Read use Read_Method_Descriptor_Proto;

   for Method_Descriptor_Proto'Write use Write_Method_Descriptor_Proto;

   type Method_Descriptor_Proto_Array is
     array (Positive range <>) of Method_Descriptor_Proto;

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
     Value  : out File_Options);

   procedure Write_File_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : File_Options);

   for File_Options'Read use Read_File_Options;

   for File_Options'Write use Write_File_Options;

   type File_Options_Array is array (Positive range <>) of File_Options;

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
     Value  : out Message_Options);

   procedure Write_Message_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Message_Options);

   for Message_Options'Read use Read_Message_Options;

   for Message_Options'Write use Write_Message_Options;

   type Message_Options_Array is array (Positive range <>) of Message_Options;

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
     Value  : out Field_Options);

   procedure Write_Field_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Field_Options);

   for Field_Options'Read use Read_Field_Options;

   for Field_Options'Write use Write_Field_Options;

   type Field_Options_Array is array (Positive range <>) of Field_Options;

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
     Value  : out Oneof_Options);

   procedure Write_Oneof_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Oneof_Options);

   for Oneof_Options'Read use Read_Oneof_Options;

   for Oneof_Options'Write use Write_Oneof_Options;

   type Oneof_Options_Array is array (Positive range <>) of Oneof_Options;

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
     Value  : out Enum_Options);

   procedure Write_Enum_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Enum_Options);

   for Enum_Options'Read use Read_Enum_Options;

   for Enum_Options'Write use Write_Enum_Options;

   type Enum_Options_Array is array (Positive range <>) of Enum_Options;

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
     Value  : out Enum_Value_Options);

   procedure Write_Enum_Value_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Enum_Value_Options);

   for Enum_Value_Options'Read use Read_Enum_Value_Options;

   for Enum_Value_Options'Write use Write_Enum_Value_Options;

   type Enum_Value_Options_Array is
     array (Positive range <>) of Enum_Value_Options;

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
     Value  : out Service_Options);

   procedure Write_Service_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Service_Options);

   for Service_Options'Read use Read_Service_Options;

   for Service_Options'Write use Write_Service_Options;

   type Service_Options_Array is array (Positive range <>) of Service_Options;

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
     Value  : out Method_Options);

   procedure Write_Method_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Method_Options);

   for Method_Options'Read use Read_Method_Options;

   for Method_Options'Write use Write_Method_Options;

   type Method_Options_Array is array (Positive range <>) of Method_Options;

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
     Value  : out Name_Part);

   procedure Write_Name_Part
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Name_Part);

   for Name_Part'Read use Read_Name_Part;

   for Name_Part'Write use Write_Name_Part;

   type Name_Part_Array is array (Positive range <>) of Name_Part;

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
     Value  : out Uninterpreted_Option);

   procedure Write_Uninterpreted_Option
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Uninterpreted_Option);

   for Uninterpreted_Option'Read use Read_Uninterpreted_Option;

   for Uninterpreted_Option'Write use Write_Uninterpreted_Option;

   type Uninterpreted_Option_Array is
     array (Positive range <>) of Uninterpreted_Option;

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
     Value  : out Location);

   procedure Write_Location
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Location);

   for Location'Read use Read_Location;

   for Location'Write use Write_Location;

   type Location_Array is array (Positive range <>) of Location;

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
     Value  : out Source_Code_Info);

   procedure Write_Source_Code_Info
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Source_Code_Info);

   for Source_Code_Info'Read use Read_Source_Code_Info;

   for Source_Code_Info'Write use Write_Source_Code_Info;

   type Source_Code_Info_Array is
     array (Positive range <>) of Source_Code_Info;

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
     Value  : out Annotation);

   procedure Write_Annotation
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Annotation);

   for Annotation'Read use Read_Annotation;

   for Annotation'Write use Write_Annotation;

   type Annotation_Array is array (Positive range <>) of Annotation;

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
     Value  : out Generated_Code_Info);

   procedure Write_Generated_Code_Info
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     Value  : Generated_Code_Info);

   for Generated_Code_Info'Read use Read_Generated_Code_Info;

   for Generated_Code_Info'Write use Write_Generated_Code_Info;

   type Generated_Code_Info_Array is
     array (Positive range <>) of Generated_Code_Info;

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