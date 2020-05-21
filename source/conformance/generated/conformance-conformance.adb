with Ada.Unchecked_Deallocation;
with PB_Support.IO;
with PB_Support.Internal;

package body Conformance.Conformance is

   package Jspb_Encoding_Config_IO is
     new PB_Support.IO.Message_IO
       (Conformance.Jspb_Encoding_Config,
        Conformance.Jspb_Encoding_Config_Vector, Conformance.Append);

   type Integer_Test_Category is  range 0 .. 5
     with Size => Conformance.Test_Category'Size;

   package Test_Category_IO is
     new PB_Support.IO.Enum_IO
       (Conformance.Test_Category, Integer_Test_Category,
        Conformance.Test_Category_Vectors);

   type Integer_Wire_Format is  range 0 .. 4
     with Size => Conformance.Wire_Format'Size;

   package Wire_Format_IO is
     new PB_Support.IO.Enum_IO
       (Conformance.Wire_Format, Integer_Wire_Format,
        Conformance.Wire_Format_Vectors);

   function Length (Self : Failure_Set_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Failure_Set_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Failure_Set_Array, Failure_Set_Array_Access);

   procedure Append (Self : in out Failure_Set_Vector; V    : Failure_Set) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Failure_Set'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Failure_Set_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Failure_Set_Array'
             (Self.Data.all & Failure_Set_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Failure_Set_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Failure_Set_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Failure_Set_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Failure_Set_Variable_Reference
    (Self  : aliased in out Failure_Set_Vector;
     Index : Positive)
      return Failure_Set_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Failure_Set_Variable_Reference;

   not overriding function Get_Failure_Set_Constant_Reference
    (Self  : aliased Failure_Set_Vector;
     Index : Positive)
      return Failure_Set_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Failure_Set_Constant_Reference;

   procedure Read_Failure_Set
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Failure_Set) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Vector (Stream, Key.Encoding, V.Failure);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Failure_Set;

   procedure Write_Failure_Set
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Failure_Set) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Failure_Set (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write (1, V.Failure);
         if WS.End_Message then
            Write_Failure_Set (WS'Access, V);
         end if;
      end;
   end Write_Failure_Set;

   function Length (Self : Conformance_Request_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Conformance_Request_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Conformance_Request_Array, Conformance_Request_Array_Access);

   procedure Append
    (Self : in out Conformance_Request_Vector;
     V    : Conformance_Request) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Conformance_Request'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Conformance_Request_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Conformance_Request_Array'
             (Self.Data.all
                & Conformance_Request_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Conformance_Request_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Conformance_Request_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Conformance_Request_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Conformance_Request_Variable_Reference
    (Self  : aliased in out Conformance_Request_Vector;
     Index : Positive)
      return Conformance_Request_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Conformance_Request_Variable_Reference;

   not overriding function Get_Conformance_Request_Constant_Reference
    (Self  : aliased Conformance_Request_Vector;
     Index : Positive)
      return Conformance_Request_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Conformance_Request_Constant_Reference;

   procedure Read_Conformance_Request
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Conformance_Request) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if V.Variant.Payload /= Protobuf_Payload_Kind then
                  V.Variant := (Protobuf_Payload_Kind, others => <>);
               end if;
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Variant.Protobuf_Payload);
            when 2 =>
               if V.Variant.Payload /= Json_Payload_Kind then
                  V.Variant := (Json_Payload_Kind, others => <>);
               end if;
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Variant.Json_Payload);
            when 7 =>
               if V.Variant.Payload /= Jspb_Payload_Kind then
                  V.Variant := (Jspb_Payload_Kind, others => <>);
               end if;
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Variant.Jspb_Payload);
            when 8 =>
               if V.Variant.Payload /= Text_Payload_Kind then
                  V.Variant := (Text_Payload_Kind, others => <>);
               end if;
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Variant.Text_Payload);
            when 3 =>
               Wire_Format_IO.Read
                 (Stream, Key.Encoding, V.Requested_Output_Format);
            when 4 =>
               PB_Support.IO.Read (Stream, Key.Encoding, V.Message_Type);
            when 5 =>
               Test_Category_IO.Read (Stream, Key.Encoding, V.Test_Category);
            when 6 =>
               if  not V.Jspb_Encoding_Options.Is_Set then
                  V.Jspb_Encoding_Options := (True, others => <>);
               end if;
               Jspb_Encoding_Config_IO.Read
                 (Stream, Key.Encoding, V.Jspb_Encoding_Options.Value);
            when 9 =>
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Print_Unknown_Fields);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Conformance_Request;

   procedure Write_Conformance_Request
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Conformance_Request) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Conformance_Request (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         Wire_Format_IO.Write_Option
           (WS, 3, V.Requested_Output_Format, Conformance.UNSPECIFIED);
         WS.Write_Option (4, V.Message_Type);
         Test_Category_IO.Write_Option
           (WS, 5, V.Test_Category, Conformance.UNSPECIFIED_TEST);
         if V.Jspb_Encoding_Options.Is_Set then
            WS.Write_Key ((6, PB_Support.Length_Delimited));
            Conformance.Jspb_Encoding_Config'Write
              (Stream, V.Jspb_Encoding_Options.Value);
         end if;
         WS.Write_Option (9, V.Print_Unknown_Fields, False);
         case V.Variant.Payload is
            when Protobuf_Payload_Kind =>
               WS.Write (1, V.Variant.Protobuf_Payload);
            when Json_Payload_Kind =>
               WS.Write (2, V.Variant.Json_Payload);
            when Jspb_Payload_Kind =>
               WS.Write (7, V.Variant.Jspb_Payload);
            when Text_Payload_Kind =>
               WS.Write (8, V.Variant.Text_Payload);
            when Payload_Not_Set =>
               null;
         end case;
         if WS.End_Message then
            Write_Conformance_Request (WS'Access, V);
         end if;
      end;
   end Write_Conformance_Request;

   function Length (Self : Conformance_Response_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Conformance_Response_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Conformance_Response_Array, Conformance_Response_Array_Access);

   procedure Append
    (Self : in out Conformance_Response_Vector;
     V    : Conformance_Response) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Conformance_Response'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Conformance_Response_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Conformance_Response_Array'
             (Self.Data.all
                & Conformance_Response_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Conformance_Response_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Conformance_Response_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Conformance_Response_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Conformance_Response_Variable_Reference
    (Self  : aliased in out Conformance_Response_Vector;
     Index : Positive)
      return Conformance_Response_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Conformance_Response_Variable_Reference;

   not overriding function Get_Conformance_Response_Constant_Reference
    (Self  : aliased Conformance_Response_Vector;
     Index : Positive)
      return Conformance_Response_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Conformance_Response_Constant_Reference;

   procedure Read_Conformance_Response
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Conformance_Response) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if V.Variant.Result /= Parse_Error_Kind then
                  V.Variant := (Parse_Error_Kind, others => <>);
               end if;
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Variant.Parse_Error);
            when 6 =>
               if V.Variant.Result /= Serialize_Error_Kind then
                  V.Variant := (Serialize_Error_Kind, others => <>);
               end if;
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Variant.Serialize_Error);
            when 2 =>
               if V.Variant.Result /= Runtime_Error_Kind then
                  V.Variant := (Runtime_Error_Kind, others => <>);
               end if;
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Variant.Runtime_Error);
            when 3 =>
               if V.Variant.Result /= Protobuf_Payload_Kind then
                  V.Variant := (Protobuf_Payload_Kind, others => <>);
               end if;
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Variant.Protobuf_Payload);
            when 4 =>
               if V.Variant.Result /= Json_Payload_Kind then
                  V.Variant := (Json_Payload_Kind, others => <>);
               end if;
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Variant.Json_Payload);
            when 5 =>
               if V.Variant.Result /= Skipped_Kind then
                  V.Variant := (Skipped_Kind, others => <>);
               end if;
               PB_Support.IO.Read (Stream, Key.Encoding, V.Variant.Skipped);
            when 7 =>
               if V.Variant.Result /= Jspb_Payload_Kind then
                  V.Variant := (Jspb_Payload_Kind, others => <>);
               end if;
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Variant.Jspb_Payload);
            when 8 =>
               if V.Variant.Result /= Text_Payload_Kind then
                  V.Variant := (Text_Payload_Kind, others => <>);
               end if;
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Variant.Text_Payload);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Conformance_Response;

   procedure Write_Conformance_Response
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Conformance_Response) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Conformance_Response (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         case V.Variant.Result is
            when Parse_Error_Kind =>
               WS.Write (1, V.Variant.Parse_Error);
            when Serialize_Error_Kind =>
               WS.Write (6, V.Variant.Serialize_Error);
            when Runtime_Error_Kind =>
               WS.Write (2, V.Variant.Runtime_Error);
            when Protobuf_Payload_Kind =>
               WS.Write (3, V.Variant.Protobuf_Payload);
            when Json_Payload_Kind =>
               WS.Write (4, V.Variant.Json_Payload);
            when Skipped_Kind =>
               WS.Write (5, V.Variant.Skipped);
            when Jspb_Payload_Kind =>
               WS.Write (7, V.Variant.Jspb_Payload);
            when Text_Payload_Kind =>
               WS.Write (8, V.Variant.Text_Payload);
            when Result_Not_Set =>
               null;
         end case;
         if WS.End_Message then
            Write_Conformance_Response (WS'Access, V);
         end if;
      end;
   end Write_Conformance_Response;

   function Length (Self : Jspb_Encoding_Config_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Jspb_Encoding_Config_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Jspb_Encoding_Config_Array, Jspb_Encoding_Config_Array_Access);

   procedure Append
    (Self : in out Jspb_Encoding_Config_Vector;
     V    : Jspb_Encoding_Config) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Jspb_Encoding_Config'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Jspb_Encoding_Config_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Jspb_Encoding_Config_Array'
             (Self.Data.all
                & Jspb_Encoding_Config_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Jspb_Encoding_Config_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Jspb_Encoding_Config_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Jspb_Encoding_Config_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Jspb_Encoding_Config_Variable_Reference
    (Self  : aliased in out Jspb_Encoding_Config_Vector;
     Index : Positive)
      return Jspb_Encoding_Config_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Jspb_Encoding_Config_Variable_Reference;

   not overriding function Get_Jspb_Encoding_Config_Constant_Reference
    (Self  : aliased Jspb_Encoding_Config_Vector;
     Index : Positive)
      return Jspb_Encoding_Config_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Jspb_Encoding_Config_Constant_Reference;

   procedure Read_Jspb_Encoding_Config
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Jspb_Encoding_Config) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Use_Jspb_Array_Any_Format);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Jspb_Encoding_Config;

   procedure Write_Jspb_Encoding_Config
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Jspb_Encoding_Config) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Jspb_Encoding_Config (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Option (1, V.Use_Jspb_Array_Any_Format, False);
         if WS.End_Message then
            Write_Jspb_Encoding_Config (WS'Access, V);
         end if;
      end;
   end Write_Jspb_Encoding_Config;

end Conformance.Conformance;