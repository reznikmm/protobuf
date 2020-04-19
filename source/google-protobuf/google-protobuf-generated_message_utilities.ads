-- Package used by generated code to generate special default values
-- (Inf, -Inf and NaN) for PB_Float and PB_Double. This might not be portable,
-- consider replacing with something that is???

pragma Ada_2012;

with Google.Protobuf.Wire_Format;

package Google.Protobuf.Generated_Message_Utilities is

   -- Google.Protobuf.Wire_Format.PB_Float
   function Positive_Infinity return Google.Protobuf.Wire_Format.PB_Float with
     Convention => C,
     Import => True,
     Link_Name => "positive_infinity_float";
   function Negative_Infinity return Google.Protobuf.Wire_Format.PB_Float with
     Convention => C,
     Import => True,
     Link_Name => "negative_infinity_float";
   function NaN return Google.Protobuf.Wire_Format.PB_Float with
     Convention => C,
     Import => True,
     Link_Name => "nan_float";

   -- Google.Protobuf.Wire_Format.PB_Double
   function Positive_Infinity return Google.Protobuf.Wire_Format.PB_Double with
     Convention => C,
     Import => True,
     Link_Name => "positive_infinity_double";
   function Negative_Infinity return Google.Protobuf.Wire_Format.PB_Double with
     Convention => C,
     Import => True,
     Link_Name => "negative_infinity_double";
   function NaN return Google.Protobuf.Wire_Format.PB_Double with
     Convention => C,
     Import => True,
     Link_Name => "nan_double";

   EMPTY_STRING : aliased Google.Protobuf.Wire_Format.PB_String := "";

end Google.Protobuf.Generated_Message_Utilities;
