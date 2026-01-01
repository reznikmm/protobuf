--  SPDX-FileCopyrightText: 2020-2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--

with Ada_Pretty;
with League.Strings;

with Google.Protobuf.Descriptor;

with Compiler.Context;

package Compiler.Descriptors is

   procedure Populate_Named_Types
     (Self        : Google.Protobuf.Descriptor.Descriptor_Proto;
      Prefix      : League.Strings.Universal_String;
      Ada_Package : League.Strings.Universal_String;
      Map         : in out Compiler.Context.Named_Type_Maps.Map;
      Used        : in out Compiler.Context.String_Sets.Set);
   --  Fill Map with type information found in a message descriptor

   procedure Populate_Nested_Types
     (Self        : Google.Protobuf.Descriptor.Descriptor_Proto;
      Prefix      : League.Strings.Universal_String;
      Ada_Package : League.Strings.Universal_String;
      Map         : in out Compiler.Context.Named_Type_Maps.Map;
      Used        : in out Compiler.Context.String_Sets.Set);
   --  Fill Map with type information found in any nested type in a message

   procedure Dependency
     (Self   : Google.Protobuf.Descriptor.Descriptor_Proto;
      Result : in out Compiler.Context.String_Sets.Set);
   --  Append dependency names to Result

   procedure Get_Used_Types
     (Self   : Google.Protobuf.Descriptor.Descriptor_Proto;
      Result : in out Compiler.Context.String_Sets.Set);

   function Enum_Types
     (Self   : Google.Protobuf.Descriptor.Descriptor_Proto;
      Prefix : League.Strings.Universal_String)
      return Ada_Pretty.Node_Access;
   --  Return list of enumetation type declared inside a message

   procedure Public_Spec
     (Self   : Google.Protobuf.Descriptor.Descriptor_Proto;
      Pkg    : League.Strings.Universal_String;
      Prefix : League.Strings.Universal_String;
      Result : out Ada_Pretty.Node_Access;
      Again  : in out Boolean;
      Done   : in out Compiler.Context.String_Sets.Set;
      Force  : in out Natural);

   function Vector_Declarations
     (Self   : Google.Protobuf.Descriptor.Descriptor_Proto;
      Prefix : League.Strings.Universal_String)
        return Ada_Pretty.Node_Access;
   --  Return list of vector type declarations

   function Private_Spec
     (Self   : Google.Protobuf.Descriptor.Descriptor_Proto;
      Prefix : League.Strings.Universal_String)
        return Ada_Pretty.Node_Access;
   --  Return list of private part declarations

   function Subprograms
     (Self   : Google.Protobuf.Descriptor.Descriptor_Proto;
      Pkg    : League.Strings.Universal_String;
      Prefix : League.Strings.Universal_String)
        return Ada_Pretty.Node_Access;
   --  Return implementation of type subprograms

end Compiler.Descriptors;
