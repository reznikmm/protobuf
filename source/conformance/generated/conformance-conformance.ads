with Ada.Finalization;
with Ada.Streams;
with League.Stream_Element_Vectors;
with League.String_Vectors;
with League.Strings;
with PB_Support.Vectors;

package Conformance.Conformance is

   type Wire_Format is (UNSPECIFIED, PROTOBUF, JSON, JSPB, TEXT_FORMAT);

   for Wire_Format use
     (UNSPECIFIED => 0, PROTOBUF    => 1, JSON        => 2, JSPB        => 3,
      TEXT_FORMAT => 4);

   package Wire_Format_Vectors is new PB_Support.Vectors (Wire_Format);

   type Test_Category is
     (UNSPECIFIED_TEST, BINARY_TEST, JSON_TEST,
      JSON_IGNORE_UNKNOWN_PARSING_TEST, JSPB_TEST, TEXT_FORMAT_TEST);

   for Test_Category use
     (UNSPECIFIED_TEST                 => 0,
      BINARY_TEST                      => 1,
      JSON_TEST                        => 2,
      JSON_IGNORE_UNKNOWN_PARSING_TEST => 3,
      JSPB_TEST                        => 4,
      TEXT_FORMAT_TEST                 => 5);

   package Test_Category_Vectors is new PB_Support.Vectors (Test_Category);

   type Failure_Set_Vector is tagged private
     with Variable_Indexing => Get_Failure_Set_Variable_Reference,
     Constant_Indexing => Get_Failure_Set_Constant_Reference;

   type Conformance_Request_Vector is tagged private
     with Variable_Indexing => Get_Conformance_Request_Variable_Reference,
     Constant_Indexing => Get_Conformance_Request_Constant_Reference;

   type Conformance_Response_Vector is tagged private
     with Variable_Indexing => Get_Conformance_Response_Variable_Reference,
     Constant_Indexing => Get_Conformance_Response_Constant_Reference;

   type Jspb_Encoding_Config_Vector is tagged private
     with Variable_Indexing => Get_Jspb_Encoding_Config_Variable_Reference,
     Constant_Indexing => Get_Jspb_Encoding_Config_Constant_Reference;

   type Failure_Set is
     record
        Failure : League.String_Vectors.Universal_String_Vector;
     end record;

   type Optional_Failure_Set  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Conformance.Failure_Set;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Failure_Set_Vector) return Natural;

   procedure Clear (Self : in out Failure_Set_Vector);

   procedure Append (Self : in out Failure_Set_Vector; V    : Failure_Set);

   type Failure_Set_Variable_Reference
     (Element : not null access Failure_Set) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Failure_Set_Variable_Reference
    (Self  : aliased in out Failure_Set_Vector;
     Index : Positive)
      return Failure_Set_Variable_Reference
     with Inline;

   type Failure_Set_Constant_Reference
     (Element : not null access constant Failure_Set) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Failure_Set_Constant_Reference
    (Self  : aliased Failure_Set_Vector;
     Index : Positive)
      return Failure_Set_Constant_Reference
     with Inline;

   type Conformance_Response_Variant_Kind is
     (Result_Not_Set,
      Parse_Error_Kind,
      Serialize_Error_Kind,
      Runtime_Error_Kind,
      Protobuf_Payload_Kind,
      Json_Payload_Kind,
      Skipped_Kind  ,
      Jspb_Payload_Kind,
      Text_Payload_Kind);

   type Conformance_Response_Variant
     (Result : Conformance_Response_Variant_Kind := Result_Not_Set) is
     record
        case Result is
           when Result_Not_Set =>
              null;
           when Parse_Error_Kind =>
              Parse_Error : League.Strings.Universal_String;
           when Serialize_Error_Kind =>
              Serialize_Error : League.Strings.Universal_String;
           when Runtime_Error_Kind =>
              Runtime_Error : League.Strings.Universal_String;
           when Protobuf_Payload_Kind =>
              Protobuf_Payload : League.Stream_Element_Vectors
                .Stream_Element_Vector;
           when Json_Payload_Kind =>
              Json_Payload : League.Strings.Universal_String;
           when Skipped_Kind =>
              Skipped : League.Strings.Universal_String;
           when Jspb_Payload_Kind =>
              Jspb_Payload : League.Strings.Universal_String;
           when Text_Payload_Kind =>
              Text_Payload : League.Strings.Universal_String;
        end case;
     end record;

   type Conformance_Response is
     record
        Variant : Conformance_Response_Variant;
     end record;

   type Optional_Conformance_Response  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Conformance.Conformance_Response;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Conformance_Response_Vector) return Natural;

   procedure Clear (Self : in out Conformance_Response_Vector);

   procedure Append
    (Self : in out Conformance_Response_Vector;
     V    : Conformance_Response);

   type Conformance_Response_Variable_Reference
     (Element : not null access Conformance_Response) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Conformance_Response_Variable_Reference
    (Self  : aliased in out Conformance_Response_Vector;
     Index : Positive)
      return Conformance_Response_Variable_Reference
     with Inline;

   type Conformance_Response_Constant_Reference
     (Element : not null access constant Conformance_Response) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Conformance_Response_Constant_Reference
    (Self  : aliased Conformance_Response_Vector;
     Index : Positive)
      return Conformance_Response_Constant_Reference
     with Inline;

   type Jspb_Encoding_Config is
     record
        Use_Jspb_Array_Any_Format : Boolean := False;
     end record;

   type Optional_Jspb_Encoding_Config  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Conformance.Jspb_Encoding_Config;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Jspb_Encoding_Config_Vector) return Natural;

   procedure Clear (Self : in out Jspb_Encoding_Config_Vector);

   procedure Append
    (Self : in out Jspb_Encoding_Config_Vector;
     V    : Jspb_Encoding_Config);

   type Jspb_Encoding_Config_Variable_Reference
     (Element : not null access Jspb_Encoding_Config) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Jspb_Encoding_Config_Variable_Reference
    (Self  : aliased in out Jspb_Encoding_Config_Vector;
     Index : Positive)
      return Jspb_Encoding_Config_Variable_Reference
     with Inline;

   type Jspb_Encoding_Config_Constant_Reference
     (Element : not null access constant Jspb_Encoding_Config) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Jspb_Encoding_Config_Constant_Reference
    (Self  : aliased Jspb_Encoding_Config_Vector;
     Index : Positive)
      return Jspb_Encoding_Config_Constant_Reference
     with Inline;

   type Conformance_Request_Variant_Kind is
     (Payload_Not_Set,
      Protobuf_Payload_Kind,
      Json_Payload_Kind,
      Jspb_Payload_Kind,
      Text_Payload_Kind);

   type Conformance_Request_Variant
     (Payload : Conformance_Request_Variant_Kind := Payload_Not_Set) is
     record
        case Payload is
           when Payload_Not_Set =>
              null;
           when Protobuf_Payload_Kind =>
              Protobuf_Payload : League.Stream_Element_Vectors
                .Stream_Element_Vector;
           when Json_Payload_Kind =>
              Json_Payload : League.Strings.Universal_String;
           when Jspb_Payload_Kind =>
              Jspb_Payload : League.Strings.Universal_String;
           when Text_Payload_Kind =>
              Text_Payload : League.Strings.Universal_String;
        end case;
     end record;

   type Conformance_Request is
     record
        Requested_Output_Format : Conformance.Wire_Format :=
          Conformance.UNSPECIFIED;
        Message_Type            : League.Strings.Universal_String;
        Test_Category           : Conformance.Test_Category :=
          Conformance.UNSPECIFIED_TEST;
        Jspb_Encoding_Options   : Conformance.Optional_Jspb_Encoding_Config;
        Print_Unknown_Fields    : Boolean := False;
        Variant                 : Conformance_Request_Variant;
     end record;

   type Optional_Conformance_Request  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Conformance.Conformance_Request;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Conformance_Request_Vector) return Natural;

   procedure Clear (Self : in out Conformance_Request_Vector);

   procedure Append
    (Self : in out Conformance_Request_Vector;
     V    : Conformance_Request);

   type Conformance_Request_Variable_Reference
     (Element : not null access Conformance_Request) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Conformance_Request_Variable_Reference
    (Self  : aliased in out Conformance_Request_Vector;
     Index : Positive)
      return Conformance_Request_Variable_Reference
     with Inline;

   type Conformance_Request_Constant_Reference
     (Element : not null access constant Conformance_Request) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Conformance_Request_Constant_Reference
    (Self  : aliased Conformance_Request_Vector;
     Index : Positive)
      return Conformance_Request_Constant_Reference
     with Inline;
private

   procedure Read_Failure_Set
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Failure_Set);

   procedure Write_Failure_Set
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Failure_Set);

   for Failure_Set'Read use Read_Failure_Set;

   for Failure_Set'Write use Write_Failure_Set;

   type Failure_Set_Array is array (Positive range <>) of aliased Failure_Set;

   type Failure_Set_Array_Access is access Failure_Set_Array;

   type Failure_Set_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Failure_Set_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Failure_Set_Vector);

   overriding procedure Finalize (Self : in out Failure_Set_Vector);

   procedure Read_Conformance_Request
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Conformance_Request);

   procedure Write_Conformance_Request
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Conformance_Request);

   for Conformance_Request'Read use Read_Conformance_Request;

   for Conformance_Request'Write use Write_Conformance_Request;

   type Conformance_Request_Array is
     array (Positive range <>) of aliased Conformance_Request;

   type Conformance_Request_Array_Access is access Conformance_Request_Array;

   type Conformance_Request_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Conformance_Request_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Conformance_Request_Vector);

   overriding procedure Finalize (Self : in out Conformance_Request_Vector);

   procedure Read_Conformance_Response
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Conformance_Response);

   procedure Write_Conformance_Response
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Conformance_Response);

   for Conformance_Response'Read use Read_Conformance_Response;

   for Conformance_Response'Write use Write_Conformance_Response;

   type Conformance_Response_Array is
     array (Positive range <>) of aliased Conformance_Response;

   type Conformance_Response_Array_Access is access Conformance_Response_Array;

   type Conformance_Response_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Conformance_Response_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Conformance_Response_Vector);

   overriding procedure Finalize (Self : in out Conformance_Response_Vector);

   procedure Read_Jspb_Encoding_Config
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Jspb_Encoding_Config);

   procedure Write_Jspb_Encoding_Config
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Jspb_Encoding_Config);

   for Jspb_Encoding_Config'Read use Read_Jspb_Encoding_Config;

   for Jspb_Encoding_Config'Write use Write_Jspb_Encoding_Config;

   type Jspb_Encoding_Config_Array is
     array (Positive range <>) of aliased Jspb_Encoding_Config;

   type Jspb_Encoding_Config_Array_Access is access Jspb_Encoding_Config_Array;

   type Jspb_Encoding_Config_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Jspb_Encoding_Config_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Jspb_Encoding_Config_Vector);

   overriding procedure Finalize (Self : in out Jspb_Encoding_Config_Vector);

end Conformance.Conformance;