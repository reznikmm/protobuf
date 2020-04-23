with Ada.Containers.Hashed_Sets;

with League.String_Vectors;
with League.Strings.Hash;

package body Compiler.Tools is

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   package String_Sets is new Ada.Containers.Hashed_Sets
     (League.Strings.Universal_String,
      League.Strings.Hash,
      League.Strings."=",
      League.Strings."=");

   Reserved : String_Sets.Set;

   -----------------
   -- To_Ada_Name --
   -----------------

   function To_Ada_Name
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is
      use type League.Strings.Universal_String;

      Force_Upper    : Boolean := True;
      Last_Was_Upper : Boolean := True;
      Result : League.Strings.Universal_String;
   begin
      if Reserved.Contains (Text.To_Lowercase) then
         return To_Ada_Name ("PB_" & Text);
      end if;

      for J in 1 .. Text.Length loop
         if Force_Upper then
            Result.Append (Text.Slice (J, J).To_Uppercase);
            Force_Upper := False;
            Last_Was_Upper := True;
         elsif Text.Element (J).To_Wide_Wide_Character = '_' then
            Force_Upper := True;
            Result.Append (Text.Element (J));
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
end Compiler.Tools;
