--  SPDX-FileCopyrightText: 2020-2025 Max Reznik <reznikmm@gmail.com>
--
--  Copyright (c) 2020-2025 Max Reznik
--

with Ada_Pretty;

with League.Strings;

with Google.Protobuf.Descriptor;

with Compiler.Context;

package Compiler.Field_Descriptors is

   function Is_One_Of
     (Self : Google.Protobuf.Descriptor.Field_Descriptor_Proto)
       return Boolean;
   --  Return True if Field is a "real" one-of field

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
