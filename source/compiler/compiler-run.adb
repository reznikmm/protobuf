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

with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Wide_Wide_Text_IO;

with League.Strings;

with Google.Protobuf.Compiler;

with Compiler.Context;
with Compiler.File_Descriptors;

procedure Compiler.Run is
   use type Compiler.Context.Ada_Type_Name;
   Input  : Ada.Streams.Stream_IO.File_Type;
   Stream : Ada.Streams.Stream_IO.Stream_Access;
   Output : Ada.Streams.Stream_IO.File_Type;
   Request : Google.Protobuf.Compiler.Code_Generator_Request;
   Result  : Google.Protobuf.Compiler.Code_Generator_Response;
begin
   Ada.Streams.Stream_IO.Open
     (File => Input,
      Mode => Ada.Streams.Stream_IO.In_File,
      Name => Ada.Command_Line.Argument (1));

   Stream := Ada.Streams.Stream_IO.Stream (Input);
   Google.Protobuf.Compiler.Code_Generator_Request'Read (Stream, Request);

   Compiler.Context.Populate_Named_Types
     (Request, Compiler.Context.Named_Types);

   for J in Compiler.Context.Named_Types.Iterate loop
      Ada.Wide_Wide_Text_IO.Put
        (Compiler.Context.Named_Type_Maps.Key (J).To_Wide_Wide_String);
      Ada.Wide_Wide_Text_IO.Put (" => ");
      Ada.Wide_Wide_Text_IO.Put_Line
        (League.Strings.To_Wide_Wide_String
           (+Compiler.Context.Named_Type_Maps.Element (J).Ada_Type));
   end loop;

   for J in 1 .. Request.File_To_Generate.Length loop
      declare
         use type League.Strings.Universal_String;
         Name : constant League.Strings.Universal_String :=
           Request.File_To_Generate.Element (J);
         File : constant Google.Protobuf.File_Descriptor_Proto :=
           Compiler.Context.Get_File (Request, Name);
         Base : constant League.Strings.Universal_String :=
           Compiler.File_Descriptors.File_Name (File);
      begin
         declare
            Item : Google.Protobuf.Compiler.File;
         begin
            Item.Name := Base & ".ads";
            Item.Content := Compiler.File_Descriptors.Specification_Text
              (File, Request);
            Result.File.Append (Item);
         end;

         declare
            Item : Google.Protobuf.Compiler.File;
         begin
            Item.Name := Base & ".adb";
            Item.Content := Compiler.File_Descriptors.Body_Text (File);
            Result.File.Append (Item);
         end;
      end;
   end loop;

   Ada.Streams.Stream_IO.Create
     (File => Output,
      Name => Ada.Command_Line.Argument (1) & ".out");

   Stream := Ada.Streams.Stream_IO.Stream (Output);
   Google.Protobuf.Compiler.Code_Generator_Response'Write (Stream, Result);
end Compiler.Run;
