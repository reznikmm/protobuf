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

with League.Strings;

with Google.Protobuf;
with Google.Protobuf.Compiler;

with Compiler.Context;

package Compiler.File_Descriptors is

   procedure Populate_Named_Types
     (Self : Google.Protobuf.File_Descriptor_Proto;
      Map  : in out Compiler.Context.Named_Type_Maps.Map);
   --  Fill Map with type information found in a file descriptor

   function File_Name
     (Self : Google.Protobuf.File_Descriptor_Proto)
      return League.Strings.Universal_String;
   --  Return base name for Ada package

   function Specification_Text
     (Self    : Google.Protobuf.File_Descriptor_Proto;
      Request : Google.Protobuf.Compiler.Code_Generator_Request)
      return League.Strings.Universal_String;
   --  Return base name for Ada package

end Compiler.File_Descriptors;
