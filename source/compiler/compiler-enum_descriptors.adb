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

   F : Ada_Pretty.Factory renames Compiler.Context.Factory;

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   function Type_Name
     (Self : Google.Protobuf.Descriptor.Enum_Descriptor_Proto)
      return League.Strings.Universal_String;
   --  Return Ada type (simple) name

   function Default
     (Self : Google.Protobuf.Descriptor.Enum_Descriptor_Proto)
      return League.Strings.Universal_String;
   --  Return default value for given enum type as string

   function Literal_Name
     (Self  : Google.Protobuf.Descriptor.Enum_Descriptor_Proto;
      Index : Positive)
      return League.Strings.Universal_String;
   --  Return default value for given enum type as string

   function Max_Value
     (Self : Google.Protobuf.Descriptor.Enum_Descriptor_Proto) return Integer;
   --  Maximum representation value

   function Min_Value
     (Self : Google.Protobuf.Descriptor.Enum_Descriptor_Proto) return Integer;
   --  Minimum representation value

   -------------
   -- Default --
   -------------

   function Default
     (Self : Google.Protobuf.Descriptor.Enum_Descriptor_Proto)
      return League.Strings.Universal_String is
   begin
      return Literal_Name (Self, 1);
   end Default;

   ------------------
   -- Literal_Name --
   ------------------

   function Literal_Name
     (Self  : Google.Protobuf.Descriptor.Enum_Descriptor_Proto;
      Index : Positive)
      return League.Strings.Universal_String
   is
      use type League.Strings.Universal_String;

      Name  : constant League.Strings.Universal_String := Type_Name (Self);
      Literal : League.Strings.Universal_String :=
        Self.Value.Get (Index).Name.Value;
   begin
      if Literal.To_Lowercase = Name.To_Lowercase then
         Literal.Prepend ("PB_");
      end if;

      return Literal;
   end Literal_Name;

   ---------------
   -- Max_Value --
   ---------------

   function Max_Value
     (Self : Google.Protobuf.Descriptor.Enum_Descriptor_Proto) return Integer
   is
      Result : Integer := Integer (Self.Value.Get (1).Number.Value);
      Next   : Integer;
   begin
      for J in 2 .. Self.Value.Length loop
         Next := Integer (Self.Value.Get (J).Number.Value);
         Result := Integer'Max (Result, Next);
      end loop;

      return Result;
   end Max_Value;

   ---------------
   -- Min_Value --
   ---------------

   function Min_Value
     (Self : Google.Protobuf.Descriptor.Enum_Descriptor_Proto) return Integer
   is
      Result : Integer := Integer (Self.Value.Get (1).Number.Value);
      Next   : Integer;
   begin
      for J in 2 .. Self.Value.Length loop
         Next := Integer (Self.Value.Get (J).Number.Value);
         Result := Integer'Min (Result, Next);
      end loop;

      return Result;
   end Min_Value;


   --------------------------
   -- Populate_Named_Types --
   --------------------------

   procedure Populate_Named_Types
     (Self        :        Google.Protobuf.Descriptor.Enum_Descriptor_Proto;
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

      if Self.Name.Is_Set then
         Key.Append (Self.Name.Value);
      end if;

      Map.Insert (Key, Value);
   end Populate_Named_Types;

   -----------------
   -- Public_Spec --
   -----------------

   function Public_Spec
     (Self : Google.Protobuf.Descriptor.Enum_Descriptor_Proto)
      return Ada_Pretty.Node_Access
   is
      use type League.Strings.Universal_String;

      Name   : constant League.Strings.Universal_String := Type_Name (Self);
      Result : Ada_Pretty.Node_Access;
      Clause : Ada_Pretty.Node_Access;
      Item   : Ada_Pretty.Node_Access;
   begin
      for J in 1 .. Self.Value.Length loop
         declare
            Next : constant
              Google.Protobuf.Descriptor.Enum_Value_Descriptor_Proto :=
                Self.Value.Get (J);

            Literal : constant League.Strings.Universal_String :=
              Literal_Name (Self, J);
         begin
            Item := F.New_Argument_Association (F.New_Name (Literal));
            Result := F.New_List (Result, Item);

            Clause := F.New_List
              (Clause,
               F.New_Argument_Association
                 (Choice => F.New_Name (Literal),
                  Value  => F.New_Literal (Natural (Next.Number.Value))));

         end;
      end loop;

      Clause := F.New_Statement
        (F.New_Apply
          (F.New_Name ("for " & Name & " use"),
           Clause));

      Result := F.New_Type
        (Name       => F.New_Name (Name),
         Definition => F.New_Parentheses (Result));

      Item := F.New_Package_Instantiation
        (Name        => F.New_Name (Name & "_Vectors"),
         Template    => F.New_Selected_Name (+"PB_Support.Vectors"),
           Actual_Part => F.New_Name (Name));

      Result := F.New_List ((Result, Clause, Item));

      return Result;
   end Public_Spec;

   ---------------
   -- Type_Name --
   ---------------

   function Type_Name
     (Self : Google.Protobuf.Descriptor.Enum_Descriptor_Proto)
      return League.Strings.Universal_String is
   begin
      if Self.Name.Is_Set then
         return Compiler.Context.To_Ada_Name (Self.Name.Value);
      else
         return +"Enum";
      end if;
   end Type_Name;

end Compiler.Enum_Descriptors;
