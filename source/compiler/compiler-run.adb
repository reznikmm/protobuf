--  SPDX-FileCopyrightText: 2020-2025 Max Reznik <reznikmm@gmail.com>
--
--  Copyright (c) 2020-2025 Max Reznik
--

with League.Strings;

with Google.Protobuf.Compiler.Plugin;
with Google.Protobuf.Descriptor;

with Proto_Support.Stdio_Streams;
with Compiler.Context;
with Compiler.File_Descriptors;

procedure Compiler.Run is
   Stream  : aliased Proto_Support.Stdio_Streams.Stdio_Stream;
   Request : Google.Protobuf.Compiler.Plugin.Code_Generator_Request;
   Result  : Google.Protobuf.Compiler.Plugin.Code_Generator_Response;
begin
   Stream.Initialize;

   Google.Protobuf.Compiler.Plugin.Code_Generator_Request'Read
     (Stream'Unchecked_Access, Request);

   Compiler.Context.Populate_Named_Types
     (Request, Compiler.Context.Named_Types);

   for J in 1 .. Request.File_To_Generate.Length loop
      declare
         use type League.Strings.Universal_String;
         Name : constant League.Strings.Universal_String :=
           Request.File_To_Generate.Element (J);
         File : constant Google.Protobuf.Descriptor.File_Descriptor_Proto :=
           Compiler.Context.Get_File (Request, Name);
         Base : constant League.Strings.Universal_String :=
           Compiler.File_Descriptors.File_Name (File);
      begin
         Compiler.Context.Fake.Clear;

         declare
            Item : Google.Protobuf.Compiler.Plugin.File;
         begin
            Item.Name := (True, Base & ".ads");
            Item.Content :=
              (Is_Set => True,
               Value  => Compiler.File_Descriptors.Specification_Text
                 (File, Request));
            Result.File.Append (Item);
         end;

         if File.Message_Type.Length > 0 then
            declare
               Item : Google.Protobuf.Compiler.Plugin.File;
            begin
               Item.Name := (True, Base & ".adb");
               Item.Content :=
                 (Is_Set => True,
                  Value  => Compiler.File_Descriptors.Body_Text (File));
               Result.File.Append (Item);
            end;
         end if;
      end;
   end loop;

   Result.Supported_Features := (True, 1);  --  FEATURE_PROTO3_OPTIONAL

   Google.Protobuf.Compiler.Plugin.Code_Generator_Response'Write
     (Stream'Unchecked_Access, Result);
end Compiler.Run;
