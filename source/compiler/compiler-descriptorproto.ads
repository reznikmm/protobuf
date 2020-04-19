with Google_Protobuf.DescriptorProto;

with Ada_Pretty;

with League.Strings;

package Compiler.DescriptorProto is

   function Public_Spec
     (Self : Google_Protobuf.DescriptorProto.Instance)
     return Ada_Pretty.Node_Access;

   function Private_Spec
     (Self : Google_Protobuf.DescriptorProto.Instance)
     return Ada_Pretty.Node_Access;

   function Type_Name
     (Self : Google_Protobuf.DescriptorProto.Instance)
      return League.Strings.Universal_String;

end Compiler.DescriptorProto;
