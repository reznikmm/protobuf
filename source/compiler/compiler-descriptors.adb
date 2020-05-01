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

with Compiler.Enum_Descriptors;
with Compiler.Field_Descriptors;

package body Compiler.Descriptors is

   F : Ada_Pretty.Factory renames Compiler.Context.Factory;

   use type Ada_Pretty.Node_Access;

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   function Type_Name
     (Self : Google.Protobuf.Descriptor_Proto)
      return League.Strings.Universal_String;
   --  Return Ada type (simple) name

   ----------------
   -- Enum_Types --
   ----------------

   function Enum_Types
     (Self : Google.Protobuf.Descriptor_Proto)
      return Ada_Pretty.Node_Access
   is
      Result : Ada_Pretty.Node_Access;
      Item   : Ada_Pretty.Node_Access;
   begin
      for J in 1 .. Self.Enum_Type.Length loop
         Item := Compiler.Enum_Descriptors.Public_Spec
           (Self.Enum_Type.Get (J));

         Result := F.New_List (Result, Item);
      end loop;

      for J in 1 .. Self.Nested_Type.Length loop
         Item := Enum_Types (Self.Nested_Type.Get (J));

         if Item /= null then
            Result := F.New_List (Result, Item);
         end if;
      end loop;

      return Result;
   end Enum_Types;

   ----------------
   -- Dependency --
   ----------------

   procedure Dependency
     (Self   : Google.Protobuf.Descriptor_Proto;
      Result : in out Compiler.Context.String_Sets.Set)
   is
   begin
      Result.Include (+"Ada.Finalization");
      Result.Include (+"Ada.Streams");

      if Self.Enum_Type.Length > 0 then
         Result.Include (+"PB_Support.Vectors");
      end if;

      for J in 1 .. Self.Field.Length loop
         Compiler.Field_Descriptors.Dependency (Self.Field.Get (J), Result);
      end loop;

      for J in 1 .. Self.Nested_Type.Length loop
         Dependency (Self.Nested_Type.Get (J), Result);
      end loop;
   end Dependency;

   --------------------------
   -- Populate_Named_Types --
   --------------------------

   procedure Populate_Named_Types
     (Self        : Google.Protobuf.Descriptor_Proto;
      PB_Prefix   : League.Strings.Universal_String;
      Ada_Package : League.Strings.Universal_String;
      Map         : in out Compiler.Context.Named_Type_Maps.Map)
   is
      Name  : constant League.Strings.Universal_String := Type_Name (Self);
      Key   : League.Strings.Universal_String := PB_Prefix;
      Value : constant Compiler.Context.Named_Type :=
        (Is_Enumeration => False,
         Ada_Type       =>
           (Package_Name => Ada_Package,
            Type_Name    => Name));
   begin
      Key.Append (".");
      Key.Append (Self.Name);
      Map.Insert (Key, Value);

      for J in 1 .. Self.Nested_Type.Length loop
         Populate_Named_Types
           (Self        => Self.Nested_Type.Get (J),
            PB_Prefix   => Key,
            Ada_Package => Ada_Package,
            Map         => Map);
      end loop;

      for J in 1 .. Self.Enum_Type.Length loop
         Compiler.Enum_Descriptors.Populate_Named_Types
           (Self        => Self.Enum_Type.Get (J),
            PB_Prefix   => Key,
            Ada_Package => Ada_Package,
            Map         => Map);
      end loop;
   end Populate_Named_Types;

   ---------------
   -- Type_Name --
   ---------------

   function Type_Name
     (Self : Google.Protobuf.Descriptor_Proto)
      return League.Strings.Universal_String
   is
      Result : constant League.Strings.Universal_String :=
        Compiler.Context.To_Ada_Name (Self.Name);
   begin
      if Result.Is_Empty then
         return +"Message";
      else
         return Result;
      end if;
   end Type_Name;

end Compiler.Descriptors;
