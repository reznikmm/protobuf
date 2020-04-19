pragma Ada_2012;

package body Google.Protobuf.IO.Invalid_Protocol_Buffer_Exception is

   -----------------------
   -- Truncated_Message --
   -----------------------

   procedure Truncated_Message is
   begin
      raise Protocol_Buffer_Exception with Truncated_Message_Message;
   end Truncated_Message;

   ----------------------
   -- Malformed_Varint --
   ----------------------

   procedure Malformed_Varint is
   begin
      raise Protocol_Buffer_Exception with Malformed_Varint_Message;
   end Malformed_Varint;

   ---------------------
   -- Invalid_End_Tag --
   ---------------------

   procedure Invalid_End_Tag is
   begin
      raise Protocol_Buffer_Exception with Invalid_End_Tag_Message;
   end Invalid_End_Tag;

   -----------------
   -- Invalid_Tag --
   -----------------

   procedure Invalid_Tag is
   begin
      raise Protocol_Buffer_Exception with Invalid_Tag_Message;
   end Invalid_Tag;

   -----------------------
   -- Invalid_Wire_Type --
   -----------------------

   procedure Invalid_Wire_Type is
   begin
      raise Protocol_Buffer_Exception with Invalid_Wire_Type_Message;
   end Invalid_Wire_Type;

   ----------------------------
   -- Recursion_Limit_Exceed --
   ----------------------------

   procedure Recursion_Limit_Exceeded is
   begin
      raise Protocol_Buffer_Exception with Recursion_Limit_Exceeded_Message;
   end Recursion_Limit_Exceeded;

   -------------------------
   -- Size_Limit_Exceeded --
   -------------------------

   procedure Size_Limit_Exceeded is
   begin
      raise Protocol_Buffer_Exception with Size_Limit_Exceeded_Message;
   end Size_Limit_Exceeded;

end Google.Protobuf.IO.Invalid_Protocol_Buffer_Exception;
