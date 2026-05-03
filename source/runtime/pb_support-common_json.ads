with Interfaces;

--  Common JSON support for Protobuf JSON output, used by both runtimes.
package PB_Support.Common_JSON is

    function Timestamp_Image
       (Seconds : Interfaces.Integer_64; Nanos : Interfaces.Integer_32)
        return String;
    --  Get timestamp as a string in the format required by Protobuf JSON
    --  output.
    --  Validate that the given seconds and nanos values are within the valid
    --  range for a Timestamp in Protobuf JSON format.
    --  Raises Constraint_Error if the values are out of range.

    function Duration_Image
       (Seconds : Interfaces.Integer_64; Nanos : Interfaces.Integer_32)
        return String;
    --  Get duration as a string in the format required by Protobuf JSON
    --  output.
    --  Validate that the given seconds and nanos values are within the valid
    --  range for a Duration in Protobuf JSON format.
    --  Raises Constraint_Error if the values are out of range.

   function Float_Image
     (Value : Interfaces.IEEE_Float_64)
      return String;
   --  Get the given float value as a string in the format required by
   --  Protobuf JSON output. This includes handling special values like NaN and
   --  infinity according to the Protobuf JSON specification.

end PB_Support.Common_JSON;
