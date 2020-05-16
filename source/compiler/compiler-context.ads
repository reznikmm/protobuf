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

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Sets;

with Ada_Pretty;

with League.Strings;
with League.Strings.Hash;

with Google.Protobuf.Descriptor;
with Google.Protobuf.Compiler.Plugin;

package Compiler.Context is

   Is_Proto_2 : Boolean := True;
   --  Proto version of current file

   Factory : aliased Ada_Pretty.Factory;
   --  Node factory for pretty printing generated sources

   package String_Sets is new Ada.Containers.Ordered_Sets
     (League.Strings.Universal_String,
      "<" => League.Strings."<",
      "=" => League.Strings."=");
   --  Set of strings

   Fake : String_Sets.Set;
   --  Set of id for each field that breaks circular usage.

   type Ada_Type_Name is record
      Package_Name : League.Strings.Universal_String;
      Type_Name    : League.Strings.Universal_String;
   end record;

   function "+" (Self : Ada_Type_Name) return League.Strings.Universal_String;
   --  Join package name (if any) and type name into selected name.

   function Relative_Name
     (Full_Name : League.Strings.Universal_String;
      Current_Package : League.Strings.Universal_String)
      return League.Strings.Universal_String;
   --  Try to avoid wrong interpretation of selected Full_Name when it's
   --  used from Current_Package. Example:
   --  Full_Name => Conformance.Conformance.Type_Name
   --  Current_Package => Conformance.Conformance
   --  Result => Conformance.Type_Name

   type Enumeration_Information is record
      Min, Max : Integer;  --  Minimum and maximum representation value
      Default  : League.Strings.Universal_String;  --  Default value
   end record;

   type Named_Type (Is_Enumeration : Boolean := False) is record
      Ada_Type : Ada_Type_Name;

      case Is_Enumeration is
         when True =>
            Enum : Enumeration_Information;
         when False =>
            null;
            --  Message : Google.Protobuf.Descriptor_Proto;
      end case;
   end record;

   package Named_Type_Maps is new Ada.Containers.Hashed_Maps
     (League.Strings.Universal_String,
      Named_Type,
      League.Strings.Hash,
      League.Strings."=");

   Named_Types : Named_Type_Maps.Map;

   procedure Populate_Named_Types
     (Request : Google.Protobuf.Compiler.Plugin.Code_Generator_Request;
      Map     : in out Compiler.Context.Named_Type_Maps.Map);
   --  Fill Map with type information found in Request

   function Get_File
     (Request : Google.Protobuf.Compiler.Plugin.Code_Generator_Request;
      Name    : League.Strings.Universal_String)
      return Google.Protobuf.Descriptor.File_Descriptor_Proto;
   --  Find file by Name in the Request

   function To_Ada_Name
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String;
   --  Convert text to look like an Ada Name

   function To_Selected_Ada_Name
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String;
   --  Convert text to look like a selected Ada Name

end Compiler.Context;
