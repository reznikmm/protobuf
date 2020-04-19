with Google_Protobuf.EnumDescriptorProto;

with League.Strings;

package Compiler.EnumDescriptorProto is

   function Type_Name
     (Self : Google_Protobuf.EnumDescriptorProto.Instance)
      return League.Strings.Universal_String;

   function Proto_Type_Name
     (Self : Google_Protobuf.EnumDescriptorProto.Instance)
      return League.Strings.Universal_String;

end Compiler.EnumDescriptorProto;
