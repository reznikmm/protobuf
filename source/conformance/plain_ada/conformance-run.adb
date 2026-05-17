--  SPDX-License-Identifier: MIT

with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Streams;
with Interfaces;

with Ada.Strings.Unbounded;

with PB_Support.Memory_Streams;
with PB_Support.Stdio_Streams;

with Conformance.Conformance;
with Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2;
with Protobuf_Test_Messages.Proto_3.Test_Messages_Proto_3;

procedure Conformance.Run is

   function "+" (Text : String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   procedure Do_Test
     (Request  : Conformance.Conformance_Request;
      Response : out Conformance.Conformance_Response);

   procedure Do_Test
     (Request  : Conformance.Conformance_Request;
      Response : out Conformance.Conformance_Response)
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
      use type Conformance.Conformance_Request_Variant_Kind;
      use type Conformance.Wire_Format;

      Output : aliased PB_Support.Memory_Streams.Memory_Stream;
      Input  : aliased PB_Support.Memory_Streams.Memory_Stream;
   begin
      if Request.Variant.Payload /= Conformance.Protobuf_Payload_Kind then
         Response.Variant :=
           (Conformance.Skipped_Kind,
            +"Unsupported payload:" &
              Conformance.Conformance_Request_Variant_Kind'Image
                (Request.Variant.Payload));
         return;
      elsif Request.Requested_Output_Format /= Conformance.PROTOBUF then
         Response.Variant :=
           (Conformance.Skipped_Kind,
            +"Unsupported output format:" &
              Conformance.Wire_Format'Image
                (Request.Requested_Output_Format));
         return;
      end if;

      declare
         Payload_Array : Ada.Streams.Stream_Element_Array
           (1 .. Ada.Streams.Stream_Element_Offset (Request.Variant.Protobuf_Payload.Length));
      begin
         for J in Payload_Array'Range loop
            Payload_Array (J) := Request.Variant.Protobuf_Payload.Element (Integer (J));
         end loop;
         Ada.Streams.Stream_Element_Array'Write (Input'Access, Payload_Array);
      end;

      if Request.Message_Type = +"conformance.FailureSet" then
         declare
            Message : Conformance.Failure_Set;
         begin
            Conformance.Failure_Set'Read (Input'Access, Message);
            Conformance.Failure_Set'Write (Output'Access, Message);
         exception
            when E : others =>
               Response.Variant :=
                 (Conformance.Parse_Error_Kind,
                  +"Parse_Error:"
                  & Ada.Exceptions.Exception_Information (E));
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
               Response.Variant :=
                 (Conformance.Parse_Error_Kind,
                  +"Parse_Error:"
                  & Ada.Exceptions.Exception_Information (E));
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
               Response.Variant :=
                 (Conformance.Parse_Error_Kind,
                  +"Parse_Error:"
                  & Ada.Exceptions.Exception_Information (E));
               return;
         end;
      else
         Response.Variant :=
           (Conformance.Skipped_Kind,
            +"Unsupported message_type:" & Request.Message_Type);
         return;
      end if;

      Response.Variant :=
        (Conformance.Protobuf_Payload_Kind,
         Output.Data);
   end Do_Test;

   Stream : aliased PB_Support.Stdio_Streams.Stdio_Stream;

begin
   Stream.Initialize;

   loop
      declare
         Size     : Interfaces.Unsigned_32;
         Request  : Conformance.Conformance_Request;
         Response : Conformance.Conformance_Response;
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
            Do_Test (Request, Response);

            Conformance.Conformance_Response'Write (Memory'Access, Response);
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
