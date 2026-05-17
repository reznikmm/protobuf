--  MIT License
--
--  Copyright (c) 2020-2025 Max Reznik
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
with League.String_Vectors;

with Google.Protobuf.Compiler.Plugin;
with Google.Protobuf.Descriptor;

with PB_Support.Stdio_Streams;
with Compiler.Context;
with Compiler.File_Descriptors.JSON;

procedure Compiler.Run is
   Stream  : aliased PB_Support.Stdio_Streams.Stdio_Stream;
   Request : Google.Protobuf.Compiler.Plugin.Code_Generator_Request;
   Result  : Google.Protobuf.Compiler.Plugin.Code_Generator_Response;

   function "+" (Text : Wide_Wide_String)
      return League.Strings.Universal_String
        renames League.Strings.To_Universal_String;

   --  Plugin options are passed as a comma-separated list in --ada_opt.
   League_Option : constant League.Strings.Universal_String :=
     +"runtime=league";
   Plain_Ada_Option : constant League.Strings.Universal_String :=
     +"runtime=plain_ada";
   Generate_JSON : constant League.Strings.Universal_String :=
     +"generate_json";
   Preserve_Proto_Field_Names : constant League.Strings.Universal_String :=
     +"preserve_proto_field_names";
   Always_Print_Enums_As_Ints : constant League.Strings.Universal_String :=
      +"always_print_enums_as_ints";

begin
   Stream.Initialize;

   Google.Protobuf.Compiler.Plugin.Code_Generator_Request'Read
     (Stream'Unchecked_Access, Request);

   if Request.Parameter.Is_Set then
      declare
         use type League.Strings.Universal_String;

         Param : constant League.Strings.Universal_String :=
           Request.Parameter.Value;
         Options : constant League.String_Vectors.Universal_String_Vector :=
           Param.Split (',', League.Strings.Skip_Empty);
      begin
         for J in 1 .. Options.Length loop
            declare
               Option : League.Strings.Universal_String renames
                 Options.Element (J);
               Option_Parts :
                 constant League.String_Vectors.Universal_String_Vector :=
                   Option.Split ('=', League.Strings.Keep_Empty);
            begin
               if Option = League_Option then
                  Compiler.Context.Runtime_Dep := Compiler.Runtime_League;
               elsif Option = Plain_Ada_Option then
                  Compiler.Context.Runtime_Dep := Compiler.Runtime_Plain_Ada;
               elsif Option_Parts.Length = 1 then

                  --  If the option is specified without value, True is
                  --  assumed.
                  if Option = Generate_JSON then
                     Compiler.Context.Generate_JSON := True;
                  elsif Option = Preserve_Proto_Field_Names then
                     Compiler.Context.Preserve_Proto_Field_Names := True;
                  elsif Option = Always_Print_Enums_As_Ints then
                     Compiler.Context.Always_Print_Enums_As_Ints := True;
                  end if;

               elsif Option_Parts.Length = 2 then

                  if Option_Parts.Element (1) = Generate_JSON then
                     Compiler.Context.Generate_JSON :=
                       Boolean'Wide_Wide_Value
                         (Option_Parts.Element (2).To_Wide_Wide_String);
                  elsif Option_Parts.Element (1) = Preserve_Proto_Field_Names
                  then
                     Compiler.Context.Preserve_Proto_Field_Names :=
                       Boolean'Wide_Wide_Value
                         (Option_Parts.Element (2).To_Wide_Wide_String);
                  elsif Option_Parts.Element (1) = Always_Print_Enums_As_Ints
                  then
                     Compiler.Context.Always_Print_Enums_As_Ints :=
                       Boolean'Wide_Wide_Value
                         (Option_Parts.Element (2).To_Wide_Wide_String);
                  end if;

               end if;
            end;
         end loop;
      end;
   end if;

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
                  Value  => Compiler.File_Descriptors.Body_Text
                    (File, Request));
               Result.File.Append (Item);
            end;

            --  Generate JSON support
            if Compiler.Context.Generate_JSON then
               declare
                  Item : Google.Protobuf.Compiler.Plugin.File;
                  Content : constant League.Strings.Universal_String :=
                    Compiler.File_Descriptors.JSON.Specification_Text
                      (File, Request);
               begin
                  Item.Name := (True, Base & "-json.ads");
                  Item.Content :=
                    (Is_Set => True,
                               Value  => Content);
                  Result.File.Append (Item);
               end;

               declare
                  Item : Google.Protobuf.Compiler.Plugin.File;
               begin
                  Item.Name := (True, Base & "-json.adb");
                  Item.Content :=
                    (Is_Set => True,
                     Value  => Compiler.File_Descriptors.JSON.Body_Text
                       (File, Request));
                  Result.File.Append (Item);
               end;
            end if;
         end if;
      end;
   end loop;

   Result.Supported_Features := (True,
    Google.Protobuf.Compiler.Plugin.FEATURE_PROTO3_OPTIONAL'Enum_Rep);

   Google.Protobuf.Compiler.Plugin.Code_Generator_Response'Write
     (Stream'Unchecked_Access, Result);
end Compiler.Run;
