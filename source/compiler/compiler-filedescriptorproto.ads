with League.Strings;
with Google_Protobuf.FileDescriptorProto;

package Compiler.FileDescriptorProto is

   function File_Name
     (Self : Google_Protobuf.FileDescriptorProto.Instance)
      return League.Strings.Universal_String;

   function Package_Name
     (Self : Google_Protobuf.FileDescriptorProto.Instance)
      return League.Strings.Universal_String;

   function File_Text
     (Self : Google_Protobuf.FileDescriptorProto.Instance)
      return League.Strings.Universal_String;

end Compiler.FileDescriptorProto;
