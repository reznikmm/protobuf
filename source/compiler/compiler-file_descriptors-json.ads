--  SPDX-License-Identifier: MIT

with League.Strings;

with Google.Protobuf.Compiler.Plugin;
with Google.Protobuf.Descriptor;

--  JSON output support generator.
package Compiler.File_Descriptors.JSON is

   function Specification_Text
     (Self    : Google.Protobuf.Descriptor.File_Descriptor_Proto;
      Request : Google.Protobuf.Compiler.Plugin.Code_Generator_Request)
      return League.Strings.Universal_String;

   function Body_Text
     (Self    : Google.Protobuf.Descriptor.File_Descriptor_Proto;
      Request : Google.Protobuf.Compiler.Plugin.Code_Generator_Request)
      return League.Strings.Universal_String;

end Compiler.File_Descriptors.JSON;
