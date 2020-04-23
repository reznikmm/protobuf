with Compiler.Contexts;

with Google_Protobuf.FieldDescriptorProto;

with Ada_Pretty;

with League.Strings;

package Compiler.FieldDescriptorProto is

   function Dependency
     (Self : Google_Protobuf.FieldDescriptorProto.Instance)
      return Compiler.Contexts.String_Sets.Set;

   function Component
     (Self : Google_Protobuf.FieldDescriptorProto.Instance)
     return Ada_Pretty.Node_Access;

   function Field_Name
     (Self : Google_Protobuf.FieldDescriptorProto.Instance)
      return League.Strings.Universal_String;

   function Type_Name
     (Self : Google_Protobuf.FieldDescriptorProto.Instance)
      return Compiler.Contexts.Ada_Type;

   function PB_Type_Name
     (Self : Google_Protobuf.FieldDescriptorProto.Instance)
      return League.Strings.Universal_String;

   function Is_Repeated
     (Self : Google_Protobuf.FieldDescriptorProto.Instance)
      return Boolean;

   function Is_Optional
     (Self : Google_Protobuf.FieldDescriptorProto.Instance)
      return Boolean;

end Compiler.FieldDescriptorProto;
