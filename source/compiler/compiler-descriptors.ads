--  MIT License
--
--  Copyright (c) 2020 Max Reznik
--
--  Permission is hereby granted, free of charge, to any person obtaining a
--  copy of this software and associated documentation files (the "Software"),
--  to deal in the Software without restriction, including without limitation
--  the rights to use, copy, modify, merge, publish, distribute, sublicense,
--  and/or sell copies of the Software, and to permit persons to whom the
--  Software is furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
--  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
--  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
--  DEALINGS IN THE SOFTWARE.

with Ada_Pretty;
with League.Strings;

with Google.Protobuf.Descriptor;

with Compiler.Context;

package Compiler.Descriptors is

   procedure Populate_Named_Types
     (Self        : Google.Protobuf.Descriptor.Descriptor_Proto;
      PB_Prefix   : League.Strings.Universal_String;
      Ada_Package : League.Strings.Universal_String;
      Map         : in out Compiler.Context.Named_Type_Maps.Map);
   --  Fill Map with type information found in a message descriptor

   procedure Dependency
     (Self   : Google.Protobuf.Descriptor.Descriptor_Proto;
      Result : in out Compiler.Context.String_Sets.Set);
   --  Append dependency names to Result

   procedure Get_Used_Types
     (Self   : Google.Protobuf.Descriptor.Descriptor_Proto;
      Result : in out Compiler.Context.String_Sets.Set);

   function Enum_Types
     (Self : Google.Protobuf.Descriptor.Descriptor_Proto)
      return Ada_Pretty.Node_Access;
   --  Return list of enumetation type declared inside a message

   procedure Public_Spec
     (Self   : Google.Protobuf.Descriptor.Descriptor_Proto;
      Pkg    : League.Strings.Universal_String;
      Result : out Ada_Pretty.Node_Access;
      Again  : in out Boolean;
      Done   : in out Compiler.Context.String_Sets.Set;
      Force  : in out Natural);

   function Vector_Declarations
     (Self : Google.Protobuf.Descriptor.Descriptor_Proto)
      return Ada_Pretty.Node_Access;
   --  Return list of vector type declarations

   function Private_Spec
     (Self : Google.Protobuf.Descriptor.Descriptor_Proto)
      return Ada_Pretty.Node_Access;
   --  Return list of private part declarations

   function Subprograms
     (Self : Google.Protobuf.Descriptor.Descriptor_Proto;
      Pkg  : League.Strings.Universal_String)
      return Ada_Pretty.Node_Access;
   --  Return implementation of type subprograms

end Compiler.Descriptors;
