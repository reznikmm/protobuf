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

package Google_Protobuf.Compiler is
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
    package CodeGeneratorRequest is
    end CodeGeneratorRequest;
    package CodeGeneratorResponse is
      package File is
      end File;
    end CodeGeneratorResponse;
  end Enumeration;
end Google_Protobuf.Compiler;
--  end read only
