pragma Ada_2012;

with Ada.Text_IO;

package body Google.Protobuf.Message is

   ----------------------------------------
   -- Print_Initialization_Error_Message --
   ----------------------------------------

   procedure Print_Initialization_Error_Message
     (Action      : in String;
      The_Message : in Message.Instance'Class)
   is
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                            "Can't " & Action & " message of type " &
                              The_Message.Get_Type_Name & " because it is " &
                              "missing required fields.");
   end Print_Initialization_Error_Message;

   ------------------------------------------
   -- Inline_Merge_From_Coded_Input_Stream --
   ------------------------------------------

   procedure Inline_Merge_From_Coded_Input_Stream
     (The_Message            : in out Message.Instance'Class;
      The_Coded_Input_Stream : in out
        Google.Protobuf.IO.Coded_Input_Stream.Instance)
   is
   begin
      The_Message.Merge_Partial_From_Coded_Input_Stream (The_Coded_Input_Stream);
      if not The_Message.Is_Initialized then
         Print_Initialization_Error_Message ("parse", The_Message);
      end if;
   end Inline_Merge_From_Coded_Input_Stream;


   --------------------------------
   -- Serialize_To_Output_Stream --
   --------------------------------

   procedure Serialize_To_Output_Stream
     (The_Message   : in out Message.Instance'Class;
      Output_Stream : not null access
        Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      if not The_Message.Is_Initialized then
         Print_Initialization_Error_Message ("serialize", The_Message);
      end if;
      The_Message.Serialize_Partial_To_Output_Stream (Output_Stream);
   end Serialize_To_Output_Stream;

   ----------------------------------------
   -- Serialize_To_Coded_Output_Stream --
   ----------------------------------------

   procedure Serialize_To_Coded_Output_Stream
     (The_Message             : in out Message.Instance'Class;
      The_Coded_Output_Stream : in out
        Google.Protobuf.IO.Coded_Output_Stream.Instance)
   is
   begin
      if not The_Message.Is_Initialized then
         Print_Initialization_Error_Message ("serialize", The_Message);
      end if;
      The_Message.Serialize_Partial_To_Coded_Output_Stream (The_Coded_Output_Stream);
   end Serialize_To_Coded_Output_Stream;

   ----------------------------------------
   -- Serialize_Partial_To_Output_Stream --
   ----------------------------------------

   procedure Serialize_Partial_To_Output_Stream
     (The_Message   : in out Message.Instance'Class;
      Output_Stream : not null access
        Ada.Streams.Root_Stream_Type'Class)
   is
      A_Coded_Output_Stream : Google.Protobuf.IO.Coded_Output_Stream.Instance
        (Google.Protobuf.IO.Coded_Output_Stream.Root_Stream_Access (Output_Stream));
      Size                  : Google.Protobuf.Wire_Format.PB_Object_Size;
      pragma Unreferenced (Size);
   begin
      Size := The_Message.Byte_Size; -- Force caching of message size.
      The_Message.Serialize_With_Cached_Sizes (A_Coded_Output_Stream);
   end Serialize_Partial_To_Output_Stream;

   ----------------------------------------
   -- Serialize_Partial_To_Coded_Output_Stream --
   ----------------------------------------

   procedure Serialize_Partial_To_Coded_Output_Stream
     (The_Message             : in out Message.Instance'Class;
      The_Coded_Output_Stream : in out
        Google.Protobuf.IO.Coded_Output_Stream.Instance)
   is
      Size                  : Google.Protobuf.Wire_Format.PB_Object_Size;
      pragma Unreferenced (Size);
   begin
      Size := The_Message.Byte_Size; -- Force caching of message size.
      The_Message.Serialize_With_Cached_Sizes (The_Coded_Output_Stream);
   end Serialize_Partial_To_Coded_Output_Stream;

   -----------------------------
   -- Parse_From_Input_Stream --
   -----------------------------

   procedure Parse_From_Input_Stream
     (The_Message  : in out Message.Instance'Class;
      Input_Stream : not null access
        Ada.Streams.Root_Stream_Type'Class)
   is
      A_Coded_Input_Stream : Google.Protobuf.IO.Coded_Input_Stream.Instance
        (Google.Protobuf.IO.Coded_Input_Stream.Root_Stream_Access (Input_Stream));
   begin
      The_Message.Clear;
      Inline_Merge_From_Coded_Input_Stream (The_Message, A_Coded_Input_Stream);
   end Parse_From_Input_Stream;

   -----------------------------------
   -- Parse_From_Coded_Input_Stream --
   -----------------------------------

   procedure Parse_From_Coded_Input_Stream
     (The_Message            : in out Message.Instance'Class;
      The_Coded_Input_Stream : in out
        Google.Protobuf.IO.Coded_Input_Stream.Instance)
   is
   begin
      The_Message.Clear;
      Inline_Merge_From_Coded_Input_Stream (The_Message, The_Coded_Input_Stream);
   end Parse_From_Coded_Input_Stream;

   -------------------------------------
   -- Parse_Partial_From_Input_Stream --
   -------------------------------------

   procedure Parse_Partial_From_Input_Stream
     (The_Message  : in out Message.Instance'Class;
      Input_Stream : not null access
        Ada.Streams.Root_Stream_Type'Class)
   is
      A_Coded_Input_Stream : Google.Protobuf.IO.Coded_Input_Stream.Instance
        (Google.Protobuf.IO.Coded_Input_Stream.Root_Stream_Access (Input_Stream));
   begin
      The_Message.Clear;
      The_Message.Merge_Partial_From_Coded_Input_Stream (A_Coded_Input_Stream);
   end Parse_Partial_From_Input_Stream;

   -------------------------------------------
   -- Parse_Partial_From_Coded_Input_Stream --
   -------------------------------------------

   procedure Parse_Partial_From_Coded_Input_Stream
     (The_Message            : in out Message.Instance'Class;
      The_Coded_Input_Stream : in out
        Google.Protobuf.IO.Coded_Input_Stream.Instance)
   is
   begin
      The_Message.Clear;
      The_Message.Merge_Partial_From_Coded_Input_Stream (The_Coded_Input_Stream);
   end Parse_Partial_From_Coded_Input_Stream;

   -----------------------------
   -- Merge_From_Input_Stream --
   -----------------------------

   procedure Merge_From_Input_Stream
     (The_Message  : in out Message.Instance'Class;
      Input_Stream : not null access
        Ada.Streams.Root_Stream_Type'Class)
   is
      A_Coded_Input_Stream : Google.Protobuf.IO.Coded_Input_Stream.Instance
        (Google.Protobuf.IO.Coded_Input_Stream.Root_Stream_Access (Input_Stream));
   begin
      Inline_Merge_From_Coded_Input_Stream (The_Message, A_Coded_Input_Stream);
   end Merge_From_Input_Stream;

   -----------------------------------
   -- Merge_From_Coded_Input_Stream --
   -----------------------------------

   procedure Merge_From_Coded_Input_Stream
     (The_Message            : in out Message.Instance'Class;
      The_Coded_Input_Stream : in out
        Google.Protobuf.IO.Coded_Input_Stream.Instance)
   is
   begin
      Inline_Merge_From_Coded_Input_Stream (The_Message, The_Coded_Input_Stream);
   end Merge_From_Coded_Input_Stream;

   -------------------------------------
   -- Merge_Partial_From_Input_Stream --
   -------------------------------------

   procedure Merge_Partial_From_Input_Stream
     (The_Message  : in out Message.Instance'Class;
      Input_Stream : not null access
        Ada.Streams.Root_Stream_Type'Class)
   is
      A_Coded_Input_Stream : Google.Protobuf.IO.Coded_Input_Stream.Instance
        (Google.Protobuf.IO.Coded_Input_Stream.Root_Stream_Access (Input_Stream));
   begin
      The_Message.Merge_Partial_From_Coded_Input_Stream (A_Coded_Input_Stream);
   end Merge_Partial_From_Input_Stream;

end Google.Protobuf.Message;
