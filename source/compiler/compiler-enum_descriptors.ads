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

package Compiler.Enum_Descriptors is

   procedure Populate_Named_Types
     (Self        : Google.Protobuf.Descriptor.Enum_Descriptor_Proto;
      PB_Prefix   : League.Strings.Universal_String;
      Ada_Package : League.Strings.Universal_String;
      Map         : in out Compiler.Context.Named_Type_Maps.Map);

   function Public_Spec
     (Self : Google.Protobuf.Descriptor.Enum_Descriptor_Proto)
      return Ada_Pretty.Node_Access;

   function Get_Literal (Value : Integer) return Ada_Pretty.Node_Access;

end Compiler.Enum_Descriptors;
