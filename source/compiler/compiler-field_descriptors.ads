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

package Compiler.Field_Descriptors is

   procedure Dependency
     (Self   : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
      Result : in out Compiler.Context.String_Sets.Set);
   --  Append dependency names to Result

   function Component
     (Self : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
      Pkg  : League.Strings.Universal_String;
      Tipe : League.Strings.Universal_String;
      Fake : Compiler.Context.String_Sets.Set)
      return Ada_Pretty.Node_Access;

   function Read_Case
     (Self  : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
      Pkg   : League.Strings.Universal_String;
      Tipe  : League.Strings.Universal_String;
      Fake  : Compiler.Context.String_Sets.Set;
      Oneof : League.Strings.Universal_String)
      return Ada_Pretty.Node_Access;

   function Case_Path
     (Self : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
      Pkg  : League.Strings.Universal_String;
      Tipe : League.Strings.Universal_String;
      Fake : Compiler.Context.String_Sets.Set)
      return Ada_Pretty.Node_Access;

   function Write_Call
     (Self : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
      Pkg  : League.Strings.Universal_String;
      Tipe : League.Strings.Universal_String;
      Fake : Compiler.Context.String_Sets.Set)
      return Ada_Pretty.Node_Access;

   procedure Get_Used_Types
     (Self   : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
      Result : in out Compiler.Context.String_Sets.Set);

   function Unique_Id
     (Self : Google.Protobuf.Descriptor.Field_Descriptor_Proto;
      Pkg  : League.Strings.Universal_String;
      Tipe : League.Strings.Universal_String)
        return League.Strings.Universal_String;

end Compiler.Field_Descriptors;
