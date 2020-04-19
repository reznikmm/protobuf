with Google_Protobuf.FieldDescriptorProto;
with Google_Protobuf.DescriptorProto;

with Ada_Pretty;

with League.Strings;

package Compiler.FieldDescriptorProto is

   function Public_Spec
     (Self    : Google_Protobuf.FieldDescriptorProto.Instance;
      Message : Google_Protobuf.DescriptorProto.Instance)
     return Ada_Pretty.Node_Access;

   function Field_Name
     (Self : Google_Protobuf.FieldDescriptorProto.Instance)
      return League.Strings.Universal_String;

   function Type_Name
     (Self : Google_Protobuf.FieldDescriptorProto.Instance)
      return League.Strings.Universal_String;

   function Is_Repeated
     (Self : Google_Protobuf.FieldDescriptorProto.Instance)
      return Boolean;

end Compiler.FieldDescriptorProto;
