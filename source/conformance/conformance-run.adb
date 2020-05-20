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

with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Streams;
with Interfaces;

with League.Strings;

with PB_Support.Memory_Streams;
with PB_Support.Stdio_Streams;

with Conformance.Conformance;
with Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2;
with Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3;

procedure Conformance.Run is

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   procedure Do_Test
     (Request  : Conformance.Conformance_Request;
      Responce : out Conformance.Conformance_Response);

   procedure Do_Test
     (Request  : Conformance.Conformance_Request;
      Responce : out Conformance.Conformance_Response)
   is
      use type League.Strings.Universal_String;
      use type Conformance.Conformance_Request_Variant_Kind;
      use type Conformance.Wire_Format;

      Output : aliased PB_Support.Memory_Streams.Memory_Stream;
      Input  : aliased PB_Support.Memory_Streams.Memory_Stream;
   begin
      if Request.Variant.Payload /= Conformance.Protobuf_Payload_Kind then
         Responce.Variant :=
           (Conformance.Skipped_Kind,
            +"Unsupported payload:" &
              Conformance.Conformance_Request_Variant_Kind'Wide_Wide_Image
                (Request.Variant.Payload));
         return;
      elsif Request.Requested_Output_Format /= Conformance.PROTOBUF then
         Responce.Variant :=
           (Conformance.Skipped_Kind,
            +"Unsupported output format:" &
              Conformance.Wire_Format'Wide_Wide_Image
                (Request.Requested_Output_Format));
         return;
      end if;

      Ada.Streams.Stream_Element_Array'Write
        (Input'Access,
         Request.Variant.Protobuf_Payload.To_Stream_Element_Array);

      if Request.Message_Type = +"conformance.FailureSet" then
         declare
            Message : Conformance.Failure_Set;
         begin
            Conformance.Failure_Set'Read (Input'Access, Message);
            Conformance.Failure_Set'Write (Output'Access, Message);
         exception
            when E : others =>
               Responce.Variant :=
                 (Conformance.Parse_Error_Kind,
                  "Parse_Error:"
                  & League.Strings.From_UTF_8_String
                    (Ada.Exceptions.Exception_Information (E)));
               return;
         end;
      elsif Request.Message_Type =
        +"protobuf_test_messages.proto2.TestAllTypesProto2"
      then
         declare
            use Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2;
            Message : Test_All_Types_Proto_2;
         begin
            Test_All_Types_Proto_2'Read (Input'Unchecked_Access, Message);
            Test_All_Types_Proto_2'Write (Output'Access, Message);
         exception
            when E : others =>
               Responce.Variant :=
                 (Conformance.Parse_Error_Kind,
                  "Parse_Error:"
                  & League.Strings.From_UTF_8_String
                    (Ada.Exceptions.Exception_Information (E)));
               return;
         end;
      elsif Request.Message_Type =
        +"protobuf_test_messages.proto3.TestAllTypesProto3"
      then
         declare
            use Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3;
            Message : Test_All_Types_Proto_3;
         begin
            Test_All_Types_Proto_3'Read (Input'Unchecked_Access, Message);
            Test_All_Types_Proto_3'Write (Output'Access, Message);
         exception
            when E : others =>
               Responce.Variant :=
                 (Conformance.Parse_Error_Kind,
                  "Parse_Error:"
                  & League.Strings.From_UTF_8_String
                    (Ada.Exceptions.Exception_Information (E)));
               return;
         end;
      else
         Responce.Variant :=
           (Conformance.Skipped_Kind,
            "Unsupported message_type:" & Request.Message_Type);
         return;
      end if;

      Responce.Variant :=
        (Conformance.Protobuf_Payload_Kind,
         Output.Data);
   end Do_Test;

   Stream : aliased PB_Support.Stdio_Streams.Stdio_Stream;

begin

   loop
      declare
         Size     : Interfaces.Unsigned_32;
         Request  : Conformance.Conformance_Request;
         Responce : Conformance.Conformance_Response;
      begin
         begin
            Interfaces.Unsigned_32'Read (Stream'Access, Size);
         exception
            when Ada.IO_Exceptions.End_Error =>
               exit;
         end;

         declare
            Last   : constant Ada.Streams.Stream_Element_Count :=
              Ada.Streams.Stream_Element_Count (Size);
            Buffer : Ada.Streams.Stream_Element_Array (1 .. Last);
            Memory : aliased PB_Support.Memory_Streams.Memory_Stream;
         begin
            Ada.Streams.Stream_Element_Array'Read (Stream'Access, Buffer);
            Memory.Write (Buffer);
            Conformance.Conformance_Request'Read (Memory'Access, Request);
         end;

         declare
            Last   : Ada.Streams.Stream_Element_Count;
            Memory : aliased PB_Support.Memory_Streams.Memory_Stream;
         begin
            Do_Test (Request, Responce);

            Conformance.Conformance_Response'Write (Memory'Access, Responce);
            Last := Memory.Written;

            declare
               Buffer : Ada.Streams.Stream_Element_Array (1 .. Last);
            begin
               Ada.Streams.Stream_Element_Array'Read (Memory'Access, Buffer);
               Interfaces.Unsigned_32'Write (Stream'Access, Buffer'Length);
               Ada.Streams.Stream_Element_Array'Write (Stream'Access, Buffer);
               Stream.Flush;
            end;
         end;
      end;
   end loop;
end Conformance.Run;
