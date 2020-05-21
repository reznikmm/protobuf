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

with League.String_Vectors;

with Compiler.File_Descriptors;

package body Compiler.Context is

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   Reserved : String_Sets.Set;

   ---------
   -- "+" --
   ---------

   function "+"
     (Self : Ada_Type_Name) return League.Strings.Universal_String
   is
      use type League.Strings.Universal_String;
   begin
      if Self.Package_Name.Is_Empty then
         return Self.Type_Name;
      else
         return Self.Package_Name & "." & Self.Type_Name;
      end if;
   end "+";

   --------------
   -- Get_File --
   --------------

   function Get_File
     (Request : Google.Protobuf.Compiler.Plugin.Code_Generator_Request;
      Name    : League.Strings.Universal_String)
      return Google.Protobuf.Descriptor.File_Descriptor_Proto
   is
      use type League.Strings.Universal_String;
   begin
      for J in 1 .. Request.Proto_File.Length loop
         declare
            Result : constant Google.Protobuf.Descriptor.File_Descriptor_Proto
              := Request.Proto_File (J);
         begin
            if Result.Name.Is_Set and then Result.Name.Value = Name then
               return Result;
            end if;
         end;
      end loop;

      raise Constraint_Error;
   end Get_File;

   --------------------------
   -- Populate_Named_Types --
   --------------------------

   procedure Populate_Named_Types
     (Request : Google.Protobuf.Compiler.Plugin.Code_Generator_Request;
      Map     : in out Compiler.Context.Named_Type_Maps.Map) is
   begin
      for J in 1 .. Request.Proto_File.Length loop
         Compiler.File_Descriptors.Populate_Named_Types
           (Request.Proto_File (J), Map);
      end loop;
   end Populate_Named_Types;

   -------------------
   -- Relative_Name --
   -------------------

   function Relative_Name
     (Full_Name : League.Strings.Universal_String;
      Current_Package : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is
      use type League.Strings.Universal_String;
      FN : constant League.String_Vectors.Universal_String_Vector :=
        Full_Name.Split ('.');
      CP : constant League.String_Vectors.Universal_String_Vector :=
        Current_Package.Split ('.');

   begin
      for J in 1 .. FN.Length - 1 loop
         declare
            Prefix : constant League.Strings.Universal_String :=
              FN.Element (J);
            Ok : Boolean := True;
         begin
            for K in J + 1 .. CP.Length loop
               Ok := Prefix /= CP.Element (K);
               exit when not Ok;
            end loop;

            if Ok then
               declare
                  Result : League.String_Vectors.Universal_String_Vector;
               begin
                  for K in J .. FN.Length loop
                     Result.Append (FN.Element (K));
                  end loop;

                  return Result.Join ('.');
               end;
            end if;
         end;
      end loop;

      return FN.Element (FN.Length);
   end Relative_Name;

   -----------------
   -- To_Ada_Name --
   -----------------

   function To_Ada_Name
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is
      use type League.Strings.Universal_String;

      Allow_Underscore : Boolean := False;
      Force_Upper      : Boolean := True;
      Last_Was_Upper   : Boolean := True;
      Result : League.Strings.Universal_String;
   begin
      if Reserved.Contains (Text.To_Lowercase) then
         return To_Ada_Name ("PB_" & Text);
      elsif Text.Ends_With ("_") then
         return To_Ada_Name (Text.Head (Text.Length - 1));
      end if;

      for J in 1 .. Text.Length loop
         if Text.Element (J).To_Wide_Wide_Character = '_' then
            if Allow_Underscore then
               Result.Append (Text.Element (J));
            end if;

            Force_Upper := True;
         elsif Force_Upper then
            Result.Append (Text.Slice (J, J).To_Uppercase);
            Force_Upper := False;
            Last_Was_Upper := True;
         elsif Text.Slice (J, J).To_Uppercase /= Text.Slice (J, J) then
            Last_Was_Upper := False;
            Result.Append (Text.Element (J));
         elsif not Last_Was_Upper then
            Last_Was_Upper := True;
            Result.Append ("_");
            Result.Append (Text.Element (J));
         else
            Result.Append (Text.Element (J));
         end if;

         Allow_Underscore := Text.Element (J).To_Wide_Wide_Character /= '_';
      end loop;

      return Result;
   end To_Ada_Name;

   --------------------------
   -- To_Selected_Ada_Name --
   --------------------------

   function To_Selected_Ada_Name
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is
      List : League.String_Vectors.Universal_String_Vector := Text.Split ('.');
   begin
      for J in 1 .. List.Length loop
         List.Replace (J, To_Ada_Name (List (J)));
      end loop;

      return List.Join ('.');
   end To_Selected_Ada_Name;

begin
   Reserved.Insert (+"begin");
   Reserved.Insert (+"end");
   Reserved.Insert (+"package");
   Reserved.Insert (+"type");
end Compiler.Context;
