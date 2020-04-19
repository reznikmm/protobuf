--  begin read only
pragma Warnings (Off);
pragma Ada_2012;
with Google.Protobuf.Wire_Format;
with Ada.Unchecked_Conversion;
with Google.Protobuf.Message;
with Google.Protobuf.IO.Coded_Output_Stream;
with Google.Protobuf.IO.Coded_Input_Stream;
with Google.Protobuf.Generated_Message_Utilities;
with Ada.Streams.Stream_IO;
with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;

package Google_Protobuf is
  use type Google.Protobuf.Wire_Format.PB_String;
  use type Google.Protobuf.Wire_Format.PB_Byte;
  use type Google.Protobuf.Wire_Format.PB_UInt32;
  use type Google.Protobuf.Wire_Format.PB_UInt64;
  use type Google.Protobuf.Wire_Format.PB_Double;
  use type Google.Protobuf.Wire_Format.PB_Float;
  use type Google.Protobuf.Wire_Format.PB_Bool;
  use type Google.Protobuf.Wire_Format.PB_Int32;
  use type Google.Protobuf.Wire_Format.PB_Int64;
  use type Google.Protobuf.Wire_Format.PB_Field_Type;
  use type Google.Protobuf.Wire_Format.PB_Wire_Type;
  use type Google.Protobuf.Wire_Format.PB_Object_Size;
  use type Google.Protobuf.Wire_Format.PB_String_Access;

  package Enumeration is
    package FileDescriptorSet is
    end FileDescriptorSet;
    package FileDescriptorProto is
    end FileDescriptorProto;
    package DescriptorProto is
      package ExtensionRange is
      end ExtensionRange;
      package ReservedRange is
      end ReservedRange;
    end DescriptorProto;
    package FieldDescriptorProto is

      type TypeX is (TYPE_DOUBLE, TYPE_FLOAT, TYPE_INT64, TYPE_UINT64, TYPE_INT32, TYPE_FIXED64, TYPE_FIXED32, TYPE_BOOL, TYPE_STRING, TYPE_GROUP, TYPE_MESSAGE, TYPE_BYTES, TYPE_UINT32, TYPE_ENUM, TYPE_SFIXED32, TYPE_SFIXED64, TYPE_SINT32, TYPE_SINT64);
      for TypeX'Size use 32;
      for TypeX use (TYPE_DOUBLE => 1, TYPE_FLOAT => 2, TYPE_INT64 => 3, TYPE_UINT64 => 4, TYPE_INT32 => 5, TYPE_FIXED64 => 6, TYPE_FIXED32 => 7, TYPE_BOOL => 8, TYPE_STRING => 9, TYPE_GROUP => 10, TYPE_MESSAGE => 11, TYPE_BYTES => 12, TYPE_UINT32 => 13, TYPE_ENUM => 14, TYPE_SFIXED32 => 15, TYPE_SFIXED64 => 16, TYPE_SINT32 => 17, TYPE_SINT64 => 18);
      function Enumeration_To_PB_Int32 is new Ada.Unchecked_Conversion (TypeX, Google.Protobuf.Wire_Format.PB_Int32);
      function PB_Int32_To_Enumeration is new Ada.Unchecked_Conversion (Google.Protobuf.Wire_Format.PB_Int32, TypeX);


      type Label is (LABEL_OPTIONAL, LABEL_REQUIRED, LABEL_REPEATED);
      for Label'Size use 32;
      for Label use (LABEL_OPTIONAL => 1, LABEL_REQUIRED => 2, LABEL_REPEATED => 3);
      function Enumeration_To_PB_Int32 is new Ada.Unchecked_Conversion (Label, Google.Protobuf.Wire_Format.PB_Int32);
      function PB_Int32_To_Enumeration is new Ada.Unchecked_Conversion (Google.Protobuf.Wire_Format.PB_Int32, Label);

    end FieldDescriptorProto;
    package OneofDescriptorProto is
    end OneofDescriptorProto;
    package EnumDescriptorProto is
    end EnumDescriptorProto;
    package EnumValueDescriptorProto is
    end EnumValueDescriptorProto;
    package ServiceDescriptorProto is
    end ServiceDescriptorProto;
    package MethodDescriptorProto is
    end MethodDescriptorProto;
    package FileOptions is

      type OptimizeMode is (SPEED, CODE_SIZE, LITE_RUNTIME);
      for OptimizeMode'Size use 32;
      for OptimizeMode use (SPEED => 1, CODE_SIZE => 2, LITE_RUNTIME => 3);
      function Enumeration_To_PB_Int32 is new Ada.Unchecked_Conversion (OptimizeMode, Google.Protobuf.Wire_Format.PB_Int32);
      function PB_Int32_To_Enumeration is new Ada.Unchecked_Conversion (Google.Protobuf.Wire_Format.PB_Int32, OptimizeMode);

    end FileOptions;
    package MessageOptions is
    end MessageOptions;
    package FieldOptions is

      type CType is (STRING, CORD, STRING_PIECE);
      for CType'Size use 32;
      for CType use (STRING => 0, CORD => 1, STRING_PIECE => 2);
      function Enumeration_To_PB_Int32 is new Ada.Unchecked_Conversion (CType, Google.Protobuf.Wire_Format.PB_Int32);
      function PB_Int32_To_Enumeration is new Ada.Unchecked_Conversion (Google.Protobuf.Wire_Format.PB_Int32, CType);


      type JSType is (JS_NORMAL, JS_STRING, JS_NUMBER);
      for JSType'Size use 32;
      for JSType use (JS_NORMAL => 0, JS_STRING => 1, JS_NUMBER => 2);
      function Enumeration_To_PB_Int32 is new Ada.Unchecked_Conversion (JSType, Google.Protobuf.Wire_Format.PB_Int32);
      function PB_Int32_To_Enumeration is new Ada.Unchecked_Conversion (Google.Protobuf.Wire_Format.PB_Int32, JSType);

    end FieldOptions;
    package OneofOptions is
    end OneofOptions;
    package EnumOptions is
    end EnumOptions;
    package EnumValueOptions is
    end EnumValueOptions;
    package ServiceOptions is
    end ServiceOptions;
    package MethodOptions is
    end MethodOptions;
    package UninterpretedOption is
      package NamePart is
      end NamePart;
    end UninterpretedOption;
    package SourceCodeInfo is
      package Location is
      end Location;
    end SourceCodeInfo;
    package GeneratedCodeInfo is
      package Annotation is
      end Annotation;
    end GeneratedCodeInfo;
  end Enumeration;
end Google_Protobuf;
--  end read only
