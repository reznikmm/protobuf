--  SPDX-FileCopyrightText: 2020-2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--

with League.Strings;

with Google.Protobuf.Descriptor;
with Google.Protobuf.Compiler.Plugin;

with Compiler.Context;

package Compiler.File_Descriptors is

   procedure Populate_Named_Types
     (Self : Google.Protobuf.Descriptor.File_Descriptor_Proto;
      Map  : in out Compiler.Context.Named_Type_Maps.Map);
   --  Fill Map with type information found in a file descriptor

   function File_Name
     (Self : Google.Protobuf.Descriptor.File_Descriptor_Proto)
      return League.Strings.Universal_String;
   --  Return base name for Ada package

   function Specification_Text
     (Self    : Google.Protobuf.Descriptor.File_Descriptor_Proto;
      Request : Google.Protobuf.Compiler.Plugin.Code_Generator_Request)
      return League.Strings.Universal_String;
   --  Return base name for Ada package

   function Body_Text
     (Self : Google.Protobuf.Descriptor.File_Descriptor_Proto)
      return League.Strings.Universal_String;

end Compiler.File_Descriptors;
