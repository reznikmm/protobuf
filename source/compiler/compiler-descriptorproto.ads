with Google_Protobuf.DescriptorProto;

with Ada_Pretty;

with League.Strings;

package Compiler.DescriptorProto is

   procedure Populate_Type_Map
     (Self        : Google_Protobuf.DescriptorProto.Instance;
      PB_Package  : League.Strings.Universal_String;
      Ada_Package : League.Strings.Universal_String);

   function Enum_Types
     (Self : Google_Protobuf.DescriptorProto.Instance)
     return Ada_Pretty.Node_Access;

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
