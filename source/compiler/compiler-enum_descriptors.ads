--  SPDX-FileCopyrightText: 2020-2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--

with Ada_Pretty;
with League.Strings;

with Google.Protobuf.Descriptor;

with Compiler.Context;

package Compiler.Enum_Descriptors is

   procedure Populate_Named_Types
     (Self        : Google.Protobuf.Descriptor.Enum_Descriptor_Proto;
      Prefix      : League.Strings.Universal_String;
      Ada_Package : League.Strings.Universal_String;
      Map         : in out Compiler.Context.Named_Type_Maps.Map;
      Used        : in out Compiler.Context.String_Sets.Set);

   function Public_Spec
     (Self   : Google.Protobuf.Descriptor.Enum_Descriptor_Proto;
      Prefix : League.Strings.Universal_String)
      return Ada_Pretty.Node_Access;

   function Get_Literal (Value : Integer) return Ada_Pretty.Node_Access;

end Compiler.Enum_Descriptors;
