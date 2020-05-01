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

with League.Strings;

package body Compiler.Field_Descriptors is

   use all type Google.Protobuf.Label;

   F : Ada_Pretty.Factory renames Compiler.Context.Factory;

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   function Type_Name
     (Self        : Google.Protobuf.Field_Descriptor_Proto;
      Is_Repeated : Boolean) return Compiler.Context.Ada_Type_Name;

   function Default (X : Google.Protobuf.PB_Type)
     return League.Strings.Universal_String;
   --  Default value for a predefined type

   function Default (Self : Google.Protobuf.Field_Descriptor_Proto)
     return Ada_Pretty.Node_Access;
   --  Default value for a field

   function Map (X : Google.Protobuf.PB_Type)
     return League.Strings.Universal_String;

   ---------------
   -- Component --
   ---------------

   function Component
     (Self : Google.Protobuf.Field_Descriptor_Proto)
      return Ada_Pretty.Node_Access
   is
      use type Compiler.Context.Ada_Type_Name;
      Result : Ada_Pretty.Node_Access;
      Name : constant League.Strings.Universal_String :=
        Compiler.Context.To_Ada_Name (Self.Name);
      Is_Vector : constant Boolean := Self.Label = LABEL_REPEATED;
   begin
      Result := F.New_Variable
        (Name            => F.New_Name (Name),
         Type_Definition => F.New_Selected_Name (+Type_Name (Self, Is_Vector)),
         Initialization  => Default (Self));

      return Result;
   end Component;

   -------------
   -- Default --
   -------------

   function Default (X : Google.Protobuf.PB_Type)
     return League.Strings.Universal_String
   is
      use all type Google.Protobuf.PB_Type;
   begin
      case X is
         when TYPE_DOUBLE   => return +"0.0";
         when TYPE_FLOAT    => return +"0.0";
         when TYPE_INT64    => return +"0";
         when TYPE_UINT64   => return +"0";
         when TYPE_INT32    => return +"0";
         when TYPE_FIXED64  => return +"0";
         when TYPE_FIXED32  => return +"0";
         when TYPE_BOOL     => return +"False";
         when TYPE_STRING   => return League.Strings.Empty_Universal_String;
         when TYPE_GROUP    => return League.Strings.Empty_Universal_String;
         when TYPE_MESSAGE  => return League.Strings.Empty_Universal_String;
         when TYPE_BYTES    => return League.Strings.Empty_Universal_String;
         when TYPE_UINT32   => return +"0";
         when TYPE_ENUM     => return League.Strings.Empty_Universal_String;
         when TYPE_SFIXED32 => return +"0";
         when TYPE_SFIXED64 => return +"0";
         when TYPE_SINT32   => return +"0";
         when TYPE_SINT64   => return +"0";
      end case;
   end Default;

   -------------
   -- Default --
   -------------

   function Default
     (Self : Google.Protobuf.Field_Descriptor_Proto)
      return Ada_Pretty.Node_Access
   is
      Result : Ada_Pretty.Node_Access;
   begin
      if Self.Label = LABEL_REPEATED then
         null;
      elsif not Self.Type_Name.Is_Empty then
         declare
            Value : constant League.Strings.Universal_String := Self.Type_Name;
         begin
            if Compiler.Context.Named_Types.Contains (Value) then
               declare
                  Full    : League.Strings.Universal_String;
                  Element : constant Compiler.Context.Named_Type :=
                    Compiler.Context.Named_Types (Value);
               begin
                  if Element.Is_Enumeration then
                     Full := Element.Ada_Type.Package_Name;
                     Full.Append (".");
                     Full.Append (Element.Enum.Default);
                     Result := F.New_Selected_Name (Full);
                  end if;
               end;
            else
               raise Constraint_Error with
                  "Type not found: " & Value.To_UTF_8_String;
            end if;
         end;
      else
         declare
            Value : constant League.Strings.Universal_String :=
              Default (Self.PB_Type);
         begin
            if not Value.Is_Empty then
               Result := F.New_Name (Value);
            end if;
         end;
      end if;

      return Result;
   end Default;

   ----------------
   -- Dependency --
   ----------------

   procedure Dependency
     (Self   : Google.Protobuf.Field_Descriptor_Proto;
      Result : in out Compiler.Context.String_Sets.Set)
   is
      Is_Vector : constant Boolean :=
        Self.Label = LABEL_REPEATED;
      My_Pkg    : constant League.Strings.Universal_String :=
        Type_Name (Self, Is_Vector).Package_Name;
   begin
      if not My_Pkg.Is_Empty then
         Result.Include (My_Pkg);
      end if;
   end Dependency;

   ---------
   -- Map --
   ---------

   function Map (X : Google.Protobuf.PB_Type)
     return League.Strings.Universal_String
   is
      use all type Google.Protobuf.PB_Type;
   begin
      case X is
         when TYPE_DOUBLE   => return +"Interfaces.IEEE_Float_64";
         when TYPE_FLOAT    => return +"Interfaces.IEEE_Float_32";
         when TYPE_INT64    => return +"Interfaces.Integer_64";
         when TYPE_UINT64   => return +"Interfaces.Unsigned_64";
         when TYPE_INT32    => return +"Interfaces.Unsigned_32";
         when TYPE_FIXED64  => return +"Interfaces.Unsigned_64";
         when TYPE_FIXED32  => return +"Interfaces.Unsigned_32";
         when TYPE_BOOL     => return +"Boolean";
         when TYPE_STRING   => return +"League.Strings.Universal_String";
         when TYPE_GROUP    => return +"group";
         when TYPE_MESSAGE  => return +"message";
         when TYPE_BYTES    => return +("League.Stream_Element_Vectors" &
              ".Stream_Element_Vector");
         when TYPE_UINT32   => return +"Interfaces.Unsigned_32";
         when TYPE_ENUM     => return +"enum";
         when TYPE_SFIXED32 => return +"Interfaces.Integer_32";
         when TYPE_SFIXED64 => return +"Interfaces.Integer_64";
         when TYPE_SINT32   => return +"Interfaces.Integer_32";
         when TYPE_SINT64   => return +"Interfaces.Integer_64";
      end case;
   end Map;

   ---------------
   -- Type_Name --
   ---------------

   function Type_Name
     (Self        : Google.Protobuf.Field_Descriptor_Proto;
      Is_Repeated : Boolean)
      return Compiler.Context.Ada_Type_Name
   is
      use type League.Strings.Universal_String;
      use all type Google.Protobuf.PB_Type;
      Result : Compiler.Context.Ada_Type_Name;
   begin
      if not Self.Type_Name.Is_Empty then
         declare
            Value : constant League.Strings.Universal_String := Self.Type_Name;
         begin
            if Compiler.Context.Named_Types.Contains (Value) then
               declare
                  Element : constant Compiler.Context.Named_Type :=
                    Compiler.Context.Named_Types (Value);
               begin
                  Result := Element.Ada_Type;

                  if Is_Repeated then
                     Result.Type_Name.Append ("_Vector");
                  elsif Self.Label = LABEL_OPTIONAL
                    and Self.PB_Type /= TYPE_ENUM
                  then
                     Result.Type_Name.Prepend ("Optional_");
                  end if;
               end;
            else
               raise Constraint_Error with
                 "Type not found: " & Value.To_UTF_8_String;
            end if;
         end;
      else
         declare
            Text : constant League.Strings.Universal_String :=
              Map (Self.PB_Type);
         begin
            if Text.Index ('.') > 0 then
               Result.Package_Name := Text.Head_To (Text.Last_Index ('.') - 1);
               Result.Type_Name := Text.Tail_From (Text.Last_Index ('.') + 1);
            else
               Result.Type_Name := Text;
            end if;

            if Is_Repeated then
               if Self.PB_Type = TYPE_STRING then
                  Result.Package_Name := +"League.String_Vectors";
                  Result.Type_Name := +"Universal_String_Vector";
               else
                  Result.Package_Name :=
                    "PB_Support." & Result.Type_Name & "_Vectors";
                  Result.Type_Name := +"Vector";
               end if;
            end if;
         end;
      end if;

      return Result;
   end Type_Name;

end Compiler.Field_Descriptors;
