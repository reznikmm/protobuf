with League.Strings;

package Compiler.Tools is

   function To_Ada_Name
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String;

   function To_Selected_Ada_Name
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String;

end Compiler.Tools;
