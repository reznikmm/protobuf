--  SPDX-FileCopyrightText: 2020-2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT

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


   -------------------
   -- Compound_Name --
   -------------------

   function Compound_Name
     (Self            : Ada_Type_Name;
      Current_Package : League.Strings.Universal_String)
     return League.Strings.Universal_String
   is
      use type League.Strings.Universal_String;
      Result : League.Strings.Universal_String := +Self;
   begin
      Result := Relative_Name (Result, Current_Package);
      return Result.Split ('.', League.Strings.Skip_Empty).Join ("_");
   end Compound_Name;

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

      raise Constraint_Error with
         "Request.Proto_File.Length = " & Request.Proto_File.Length'Image;
   end Get_File;

   ----------------------
   -- Is_Reserved_Word --
   ----------------------

   function Is_Reserved_Word
     (Name : League.Strings.Universal_String) return Boolean is
   begin
      return Reserved.Contains (Name.To_Lowercase);
   end Is_Reserved_Word;

   ----------
   -- Join --
   ----------

   function Join
     (Prefix : League.Strings.Universal_String;
      Name   : Proto_Support.Universal_String_Vectors.Option)
      return League.Strings.Universal_String
   is
      Result : League.Strings.Universal_String := Prefix;
   begin
      Result.Append (".");

      if Name.Is_Set then
         Result.Append (Name.Value);
      end if;

      return Result;
   end Join;

   -------------------
   -- New_Type_Name --
   -------------------

   function New_Type_Name
     (Name    : Proto_Support.Universal_String_Vectors.Option;
      Default : League.Strings.Universal_String;
      Prefix  : League.Strings.Universal_String;
      Local   : Compiler.Context.Named_Type_Maps.Map;
      Used    : Compiler.Context.String_Sets.Set)
     return League.Strings.Universal_String
   is
      use type League.Strings.Universal_String;

      Result : League.Strings.Universal_String :=
        (if Name.Is_Set
         then Compiler.Context.To_Ada_Name (Name.Value)
         else Default);

      Fallback : constant League.Strings.Universal_String :=
        (if Named_Types.Contains (Prefix) then
           Named_Types (Prefix).Ada_Type.Type_Name & "_" & Result
         elsif Local.Contains (Prefix) then
           Local (Prefix).Ada_Type.Type_Name & "_" & Result
         else Result);

      Count : Positive := 1;
   begin
      if Used.Contains (Result) then
         Result := Fallback;
      end if;

      while Used.Contains (Result) loop
         declare
            Image : Wide_Wide_String := Count'Wide_Wide_Image;
         begin
            Image (1) := '_';
            Result := Fallback & Image;
            Count := Count + 1;
         end;
      end loop;

      return Result;
   end New_Type_Name;

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
         return To_Ada_Name ("Proto_" & Text);
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
   Reserved.Insert (+"abort");
   Reserved.Insert (+"abs");
   Reserved.Insert (+"abstract");
   Reserved.Insert (+"accept");
   Reserved.Insert (+"access");
   Reserved.Insert (+"aliased");
   Reserved.Insert (+"all");
   Reserved.Insert (+"and");
   Reserved.Insert (+"array");
   Reserved.Insert (+"at");
   Reserved.Insert (+"begin");
   Reserved.Insert (+"body");
   Reserved.Insert (+"case");
   Reserved.Insert (+"constant");
   Reserved.Insert (+"declare");
   Reserved.Insert (+"delay");
   Reserved.Insert (+"delta");
   Reserved.Insert (+"digits");
   Reserved.Insert (+"do");
   Reserved.Insert (+"else");
   Reserved.Insert (+"elsif");
   Reserved.Insert (+"end");
   Reserved.Insert (+"entry");
   Reserved.Insert (+"exception");
   Reserved.Insert (+"exit");
   Reserved.Insert (+"for");
   Reserved.Insert (+"function");
   Reserved.Insert (+"generic");
   Reserved.Insert (+"goto");
   Reserved.Insert (+"if");
   Reserved.Insert (+"in");
   Reserved.Insert (+"interface");
   Reserved.Insert (+"is");
   Reserved.Insert (+"limited");
   Reserved.Insert (+"loop");
   Reserved.Insert (+"mod");
   Reserved.Insert (+"new");
   Reserved.Insert (+"not");
   Reserved.Insert (+"null");
   Reserved.Insert (+"of");
   Reserved.Insert (+"or");
   Reserved.Insert (+"others");
   Reserved.Insert (+"out");
   Reserved.Insert (+"overriding");
   Reserved.Insert (+"package");
   Reserved.Insert (+"pragma");
   Reserved.Insert (+"private");
   Reserved.Insert (+"procedure");
   Reserved.Insert (+"protected");
   Reserved.Insert (+"raise");
   Reserved.Insert (+"range");
   Reserved.Insert (+"record");
   Reserved.Insert (+"rem");
   Reserved.Insert (+"renames");
   Reserved.Insert (+"requeue");
   Reserved.Insert (+"return");
   Reserved.Insert (+"reverse");
   Reserved.Insert (+"select");
   Reserved.Insert (+"separate");
   Reserved.Insert (+"some");
   Reserved.Insert (+"subtype");
   Reserved.Insert (+"synchronized");
   Reserved.Insert (+"tagged");
   Reserved.Insert (+"task");
   Reserved.Insert (+"terminate");
   Reserved.Insert (+"then");
   Reserved.Insert (+"type");
   Reserved.Insert (+"until");
   Reserved.Insert (+"use");
   Reserved.Insert (+"when");
   Reserved.Insert (+"while");
   Reserved.Insert (+"with");
   Reserved.Insert (+"xor");
end Compiler.Context;
