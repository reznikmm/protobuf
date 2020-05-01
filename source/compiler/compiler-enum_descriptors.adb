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

package body Compiler.Enum_Descriptors is

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   function Type_Name
     (Self : Google.Protobuf.Enum_Descriptor_Proto)
      return League.Strings.Universal_String;
   --  Return Ada type (simple) name

   function Default
     (Self : Google.Protobuf.Enum_Descriptor_Proto)
      return League.Strings.Universal_String;
   --  Return default value for given enum type as string

   function Max_Value
     (Self : Google.Protobuf.Enum_Descriptor_Proto) return Integer;
   --  Maximum representation value

   function Min_Value
     (Self : Google.Protobuf.Enum_Descriptor_Proto) return Integer;
   --  Minimum representation value

   -------------
   -- Default --
   -------------

   function Default
     (Self : Google.Protobuf.Enum_Descriptor_Proto)
      return League.Strings.Universal_String is
   begin
      return Self.Value.Get (1).Name;
   end Default;

   ---------------
   -- Max_Value --
   ---------------

   function Max_Value
     (Self : Google.Protobuf.Enum_Descriptor_Proto) return Integer
   is
      Result : Integer := Integer (Self.Value.Get (1).Number);
      Next   : Integer;
   begin
      for J in 2 .. Self.Value.Length loop
         Next := Integer (Self.Value.Get (J).Number);
         Result := Integer'Max (Result, Next);
      end loop;

      return Result;
   end Max_Value;

   ---------------
   -- Min_Value --
   ---------------

   function Min_Value
     (Self : Google.Protobuf.Enum_Descriptor_Proto) return Integer
   is
      Result : Integer := Integer (Self.Value.Get (1).Number);
      Next   : Integer;
   begin
      for J in 2 .. Self.Value.Length loop
         Next := Integer (Self.Value.Get (J).Number);
         Result := Integer'Min (Result, Next);
      end loop;

      return Result;
   end Min_Value;


   --------------------------
   -- Populate_Named_Types --
   --------------------------

   procedure Populate_Named_Types
     (Self        :        Google.Protobuf.Enum_Descriptor_Proto;
      PB_Prefix   :        League.Strings.Universal_String;
      Ada_Package :        League.Strings.Universal_String;
      Map         : in out Compiler.Context.Named_Type_Maps.Map)
   is
      Name  : constant League.Strings.Universal_String := Type_Name (Self);
      Key   : League.Strings.Universal_String := PB_Prefix;
      Value : constant Compiler.Context.Named_Type :=
        (Is_Enumeration => True,
         Ada_Type       =>
           (Package_Name => Ada_Package,
            Type_Name    => Name),
         Enum           =>
           (Min => Min_Value (Self),
            Max => Max_Value (Self),
            Default => Default (Self)));
   begin
      Key.Append (".");
      Key.Append (Self.Name);
      Map.Insert (Key, Value);
   end Populate_Named_Types;

   ---------------
   -- Type_Name --
   ---------------

   function Type_Name
     (Self : Google.Protobuf.Enum_Descriptor_Proto)
      return League.Strings.Universal_String
   is
      Result : constant League.Strings.Universal_String :=
        Compiler.Context.To_Ada_Name (Self.Name);
   begin
      if Result.Is_Empty then
         return +"Enum";
      else
         return Result;
      end if;
   end Type_Name;

end Compiler.Enum_Descriptors;
