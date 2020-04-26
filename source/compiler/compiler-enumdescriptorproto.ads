with Google_Protobuf.EnumDescriptorProto;

with League.Strings;

with Ada_Pretty;

package Compiler.EnumDescriptorProto is

   function Type_Name
     (Self : Google_Protobuf.EnumDescriptorProto.Instance)
      return League.Strings.Universal_String;

   function Default
     (Self : Google_Protobuf.EnumDescriptorProto.Instance)
      return League.Strings.Universal_String;

   function Public_Spec
     (Self : Google_Protobuf.EnumDescriptorProto.Instance)
     return not null Ada_Pretty.Node_Access;

   function Proto_Type_Name
     (Self : Google_Protobuf.EnumDescriptorProto.Instance)
      return League.Strings.Universal_String;

   function Min_Value
     (Self : Google_Protobuf.EnumDescriptorProto.Instance)
      return Integer;

   function Max_Value
     (Self : Google_Protobuf.EnumDescriptorProto.Instance)
      return Integer;

end Compiler.EnumDescriptorProto;
