pragma Ada_2012;

package Google.Protobuf.IO.Invalid_Protocol_Buffer_Exception is
   Protocol_Buffer_Exception : exception;

   procedure Truncated_Message;
   procedure Malformed_Varint;
   procedure Invalid_Tag;
   procedure Invalid_End_Tag;
   procedure Invalid_Wire_Type;
   procedure Recursion_Limit_Exceeded;
   procedure Size_Limit_Exceeded;

   Truncated_Message_Message : constant String :=
                                 "While parsing a protocol message, the input ended unexpectedly " &
                                 "in the middle of a field.  This could mean either than the " &
                                 "input has been truncated or that an embedded message " &
                                 "misreported its own length.";

   Malformed_Varint_Message : constant String :=
                                "CodedInputStream encountered a malformed varint.";

   Invalid_Tag_Message : constant String :=
                           "Protocol message contained an invalid tag (zero).";

   Invalid_End_Tag_Message : constant String :=
                               "Protocol message end-group tag did not match expected tag.";

   Invalid_Wire_Type_Message : constant String :=
                                 "Protocol message tag had invalid wire type.";

   Recursion_Limit_Exceeded_Message : constant String :=
                                        "Protocol message had too many levels of nesting. May be malicious. " &
                                        "Use Google.Protobuf.IO.Coded_Input_Stream.setRecursionLimit(...) to " &
                                        "increase the depth limit.";

   Size_Limit_Exceeded_Message : constant String :=
                                   "Protocol message was too large. May be malicious. " &
                                   "Use Google.Protobuf.IO.Coded_Input_Stream.setSizeLimit(...) to " &
                                   "increase the size limit.";
end Google.Protobuf.IO.Invalid_Protocol_Buffer_Exception;
