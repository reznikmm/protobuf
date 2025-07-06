with Ada.Finalization;
with Ada.Streams;
with Google.Protobuf.Descriptor;
with League.String_Vectors;
with Proto_Support.Integer_32_Vectors;
with Proto_Support.Universal_String_Vectors;
with Proto_Support.Unsigned_64_Vectors;
with Proto_Support.Vectors;

package Google.Protobuf.Compiler.Plugin is

   type Feature is
     (FEATURE_NONE, FEATURE_PROTO3_OPTIONAL, FEATURE_SUPPORTS_EDITIONS);

   for Feature use
     (FEATURE_NONE              => 0, FEATURE_PROTO3_OPTIONAL   => 1,
      FEATURE_SUPPORTS_EDITIONS => 2);

   package Feature_Vectors is new Proto_Support.Vectors (Feature);

   type Version_Vector is tagged private
     with Variable_Indexing => Get_Version_Variable_Reference,
     Constant_Indexing => Get_Version_Constant_Reference;

   type Code_Generator_Request_Vector is tagged private
     with Variable_Indexing => Get_Code_Generator_Request_Variable_Reference,
     Constant_Indexing => Get_Code_Generator_Request_Constant_Reference;

   type Code_Generator_Response_Vector is tagged private
     with Variable_Indexing => Get_Code_Generator_Response_Variable_Reference,
     Constant_Indexing => Get_Code_Generator_Response_Constant_Reference;

   type File_Vector is tagged private
     with Variable_Indexing => Get_File_Variable_Reference,
     Constant_Indexing => Get_File_Constant_Reference;

   type Version is
     record
        Major  : Proto_Support.Integer_32_Vectors.Option;
        Minor  : Proto_Support.Integer_32_Vectors.Option;
        Patch  : Proto_Support.Integer_32_Vectors.Option;
        Suffix : Proto_Support.Universal_String_Vectors.Option;
     end record;

   type Optional_Version  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Compiler.Plugin.Version;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Version_Vector) return Natural;

   procedure Clear (Self : in out Version_Vector);

   procedure Append (Self : in out Version_Vector; V    : Version);

   type Version_Variable_Reference  (Element : not null access Version) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Version_Variable_Reference
    (Self  : aliased in out Version_Vector;
     Index : Positive)
      return Version_Variable_Reference
     with Inline;

   type Version_Constant_Reference
     (Element : not null access constant Version) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Version_Constant_Reference
    (Self  : aliased Version_Vector;
     Index : Positive)
      return Version_Constant_Reference
     with Inline;

   type Code_Generator_Request is
     record
        File_To_Generate        : League.String_Vectors
          .Universal_String_Vector;
        Parameter               : Proto_Support.Universal_String_Vectors.Option;
        Proto_File              : Google.Protobuf.Descriptor
          .File_Descriptor_Proto_Vector;
        Source_File_Descriptors : Google.Protobuf.Descriptor
          .File_Descriptor_Proto_Vector;
        Compiler_Version        : Google.Protobuf.Compiler.Plugin
          .Optional_Version;
     end record;

   type Optional_Code_Generator_Request  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Compiler.Plugin.Code_Generator_Request;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Code_Generator_Request_Vector) return Natural;

   procedure Clear (Self : in out Code_Generator_Request_Vector);

   procedure Append
    (Self : in out Code_Generator_Request_Vector;
     V    : Code_Generator_Request);

   type Code_Generator_Request_Variable_Reference
     (Element : not null access Code_Generator_Request) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Code_Generator_Request_Variable_Reference
    (Self  : aliased in out Code_Generator_Request_Vector;
     Index : Positive)
      return Code_Generator_Request_Variable_Reference
     with Inline;

   type Code_Generator_Request_Constant_Reference
     (Element : not null access constant Code_Generator_Request) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Code_Generator_Request_Constant_Reference
    (Self  : aliased Code_Generator_Request_Vector;
     Index : Positive)
      return Code_Generator_Request_Constant_Reference
     with Inline;

   type File is
     record
        Name                : Proto_Support.Universal_String_Vectors.Option;
        Insertion_Point     : Proto_Support.Universal_String_Vectors.Option;
        Content             : Proto_Support.Universal_String_Vectors.Option;
        Generated_Code_Info : Google.Protobuf.Descriptor
          .Optional_Generated_Code_Info;
     end record;

   type Optional_File  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Compiler.Plugin.File;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : File_Vector) return Natural;

   procedure Clear (Self : in out File_Vector);

   procedure Append (Self : in out File_Vector; V    : File);

   type File_Variable_Reference  (Element : not null access File) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_File_Variable_Reference
    (Self  : aliased in out File_Vector;
     Index : Positive)
      return File_Variable_Reference
     with Inline;

   type File_Constant_Reference  (Element : not null access constant File) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_File_Constant_Reference
    (Self  : aliased File_Vector;
     Index : Positive)
      return File_Constant_Reference
     with Inline;

   type Code_Generator_Response is
     record
        Error              : Proto_Support.Universal_String_Vectors.Option;
        Supported_Features : Proto_Support.Unsigned_64_Vectors.Option;
        Minimum_Edition    : Proto_Support.Integer_32_Vectors.Option;
        Maximum_Edition    : Proto_Support.Integer_32_Vectors.Option;
        File               : Google.Protobuf.Compiler.Plugin.File_Vector;
     end record;

   type Optional_Code_Generator_Response  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Compiler.Plugin.Code_Generator_Response;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Code_Generator_Response_Vector) return Natural;

   procedure Clear (Self : in out Code_Generator_Response_Vector);

   procedure Append
    (Self : in out Code_Generator_Response_Vector;
     V    : Code_Generator_Response);

   type Code_Generator_Response_Variable_Reference
     (Element : not null access Code_Generator_Response) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Code_Generator_Response_Variable_Reference
    (Self  : aliased in out Code_Generator_Response_Vector;
     Index : Positive)
      return Code_Generator_Response_Variable_Reference
     with Inline;

   type Code_Generator_Response_Constant_Reference
     (Element : not null access constant Code_Generator_Response) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Code_Generator_Response_Constant_Reference
    (Self  : aliased Code_Generator_Response_Vector;
     Index : Positive)
      return Code_Generator_Response_Constant_Reference
     with Inline;
private

   procedure Read_Version
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Version);

   procedure Write_Version
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Version);

   for Version'Read use Read_Version;

   for Version'Write use Write_Version;

   type Version_Array is array (Positive range <>) of aliased Version;

   type Version_Array_Access is access Version_Array;

   type Version_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Version_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Version_Vector);

   overriding procedure Finalize (Self : in out Version_Vector);

   procedure Read_Code_Generator_Request
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Code_Generator_Request);

   procedure Write_Code_Generator_Request
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Code_Generator_Request);

   for Code_Generator_Request'Read use Read_Code_Generator_Request;

   for Code_Generator_Request'Write use Write_Code_Generator_Request;

   type Code_Generator_Request_Array is
     array (Positive range <>) of aliased Code_Generator_Request;

   type Code_Generator_Request_Array_Access is
     access Code_Generator_Request_Array;

   type Code_Generator_Request_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Code_Generator_Request_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Code_Generator_Request_Vector);

   overriding procedure Finalize (Self : in out Code_Generator_Request_Vector);

   procedure Read_File
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out File);

   procedure Write_File
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : File);

   for File'Read use Read_File;

   for File'Write use Write_File;

   type File_Array is array (Positive range <>) of aliased File;

   type File_Array_Access is access File_Array;

   type File_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : File_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out File_Vector);

   overriding procedure Finalize (Self : in out File_Vector);

   procedure Read_Code_Generator_Response
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Code_Generator_Response);

   procedure Write_Code_Generator_Response
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Code_Generator_Response);

   for Code_Generator_Response'Read use Read_Code_Generator_Response;

   for Code_Generator_Response'Write use Write_Code_Generator_Response;

   type Code_Generator_Response_Array is
     array (Positive range <>) of aliased Code_Generator_Response;

   type Code_Generator_Response_Array_Access is
     access Code_Generator_Response_Array;

   type Code_Generator_Response_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Code_Generator_Response_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Code_Generator_Response_Vector);

   overriding procedure Finalize
    (Self : in out Code_Generator_Response_Vector);

end Google.Protobuf.Compiler.Plugin;