with Ada.Finalization;
with Ada.Streams;
with Interfaces;
with League.Stream_Element_Vectors;
with League.Strings;

package Google.Protobuf.Wrappers is

   type Double_Value_Vector is tagged private;

   type Float_Value_Vector is tagged private;

   type Int_64Value_Vector is tagged private;

   type UInt_64Value_Vector is tagged private;

   type Int_32Value_Vector is tagged private;

   type UInt_32Value_Vector is tagged private;

   type Bool_Value_Vector is tagged private;

   type String_Value_Vector is tagged private;

   type Bytes_Value_Vector is tagged private;

   type Double_Value is
     record
        Value : Interfaces.IEEE_Float_64 := 0.0;
     end record;

   type Optional_Double_Value (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Wrappers.Double_Value;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Double_Value_Vector) return Natural;

   function Get
    (Self  : Double_Value_Vector;
     Index : Positive)
      return Double_Value;

   procedure Clear (Self : in out Double_Value_Vector);

   procedure Append (Self : in out Double_Value_Vector; V    : Double_Value);

   type Float_Value is
     record
        Value : Interfaces.IEEE_Float_32 := 0.0;
     end record;

   type Optional_Float_Value (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Wrappers.Float_Value;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Float_Value_Vector) return Natural;

   function Get
    (Self  : Float_Value_Vector;
     Index : Positive)
      return Float_Value;

   procedure Clear (Self : in out Float_Value_Vector);

   procedure Append (Self : in out Float_Value_Vector; V    : Float_Value);

   type Int_64Value is record Value : Interfaces.Integer_64 := 0; end record;

   type Optional_Int_64Value (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Wrappers.Int_64Value;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Int_64Value_Vector) return Natural;

   function Get
    (Self  : Int_64Value_Vector;
     Index : Positive)
      return Int_64Value;

   procedure Clear (Self : in out Int_64Value_Vector);

   procedure Append (Self : in out Int_64Value_Vector; V    : Int_64Value);

   type UInt_64Value is record Value : Interfaces.Unsigned_64 := 0; end record;

   type Optional_UInt_64Value (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Wrappers.UInt_64Value;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : UInt_64Value_Vector) return Natural;

   function Get
    (Self  : UInt_64Value_Vector;
     Index : Positive)
      return UInt_64Value;

   procedure Clear (Self : in out UInt_64Value_Vector);

   procedure Append (Self : in out UInt_64Value_Vector; V    : UInt_64Value);

   type Int_32Value is record Value : Interfaces.Integer_32 := 0; end record;

   type Optional_Int_32Value (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Wrappers.Int_32Value;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Int_32Value_Vector) return Natural;

   function Get
    (Self  : Int_32Value_Vector;
     Index : Positive)
      return Int_32Value;

   procedure Clear (Self : in out Int_32Value_Vector);

   procedure Append (Self : in out Int_32Value_Vector; V    : Int_32Value);

   type UInt_32Value is record Value : Interfaces.Unsigned_32 := 0; end record;

   type Optional_UInt_32Value (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Wrappers.UInt_32Value;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : UInt_32Value_Vector) return Natural;

   function Get
    (Self  : UInt_32Value_Vector;
     Index : Positive)
      return UInt_32Value;

   procedure Clear (Self : in out UInt_32Value_Vector);

   procedure Append (Self : in out UInt_32Value_Vector; V    : UInt_32Value);

   type Bool_Value is record Value : Boolean := False; end record;

   type Optional_Bool_Value (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Wrappers.Bool_Value;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Bool_Value_Vector) return Natural;

   function Get
    (Self  : Bool_Value_Vector;
     Index : Positive)
      return Bool_Value;

   procedure Clear (Self : in out Bool_Value_Vector);

   procedure Append (Self : in out Bool_Value_Vector; V    : Bool_Value);

   type String_Value is
     record
        Value : League.Strings.Universal_String;
     end record;

   type Optional_String_Value (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Wrappers.String_Value;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : String_Value_Vector) return Natural;

   function Get
    (Self  : String_Value_Vector;
     Index : Positive)
      return String_Value;

   procedure Clear (Self : in out String_Value_Vector);

   procedure Append (Self : in out String_Value_Vector; V    : String_Value);

   type Bytes_Value is
     record
        Value : League.Stream_Element_Vectors.Stream_Element_Vector;
     end record;

   type Optional_Bytes_Value (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Wrappers.Bytes_Value;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Bytes_Value_Vector) return Natural;

   function Get
    (Self  : Bytes_Value_Vector;
     Index : Positive)
      return Bytes_Value;

   procedure Clear (Self : in out Bytes_Value_Vector);

   procedure Append (Self : in out Bytes_Value_Vector; V    : Bytes_Value);
private

   procedure Read_Double_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Double_Value);

   procedure Write_Double_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Double_Value);

   for Double_Value'Read use Read_Double_Value;

   for Double_Value'Write use Write_Double_Value;

   type Double_Value_Array is array (Positive range <>) of Double_Value;

   type Double_Value_Array_Access is access Double_Value_Array;

   type Double_Value_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Double_Value_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Double_Value_Vector);

   overriding procedure Finalize (Self : in out Double_Value_Vector);

   procedure Read_Float_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Float_Value);

   procedure Write_Float_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Float_Value);

   for Float_Value'Read use Read_Float_Value;

   for Float_Value'Write use Write_Float_Value;

   type Float_Value_Array is array (Positive range <>) of Float_Value;

   type Float_Value_Array_Access is access Float_Value_Array;

   type Float_Value_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Float_Value_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Float_Value_Vector);

   overriding procedure Finalize (Self : in out Float_Value_Vector);

   procedure Read_Int_64Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Int_64Value);

   procedure Write_Int_64Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Int_64Value);

   for Int_64Value'Read use Read_Int_64Value;

   for Int_64Value'Write use Write_Int_64Value;

   type Int_64Value_Array is array (Positive range <>) of Int_64Value;

   type Int_64Value_Array_Access is access Int_64Value_Array;

   type Int_64Value_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Int_64Value_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Int_64Value_Vector);

   overriding procedure Finalize (Self : in out Int_64Value_Vector);

   procedure Read_UInt_64Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out UInt_64Value);

   procedure Write_UInt_64Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : UInt_64Value);

   for UInt_64Value'Read use Read_UInt_64Value;

   for UInt_64Value'Write use Write_UInt_64Value;

   type UInt_64Value_Array is array (Positive range <>) of UInt_64Value;

   type UInt_64Value_Array_Access is access UInt_64Value_Array;

   type UInt_64Value_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : UInt_64Value_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out UInt_64Value_Vector);

   overriding procedure Finalize (Self : in out UInt_64Value_Vector);

   procedure Read_Int_32Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Int_32Value);

   procedure Write_Int_32Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Int_32Value);

   for Int_32Value'Read use Read_Int_32Value;

   for Int_32Value'Write use Write_Int_32Value;

   type Int_32Value_Array is array (Positive range <>) of Int_32Value;

   type Int_32Value_Array_Access is access Int_32Value_Array;

   type Int_32Value_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Int_32Value_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Int_32Value_Vector);

   overriding procedure Finalize (Self : in out Int_32Value_Vector);

   procedure Read_UInt_32Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out UInt_32Value);

   procedure Write_UInt_32Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : UInt_32Value);

   for UInt_32Value'Read use Read_UInt_32Value;

   for UInt_32Value'Write use Write_UInt_32Value;

   type UInt_32Value_Array is array (Positive range <>) of UInt_32Value;

   type UInt_32Value_Array_Access is access UInt_32Value_Array;

   type UInt_32Value_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : UInt_32Value_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out UInt_32Value_Vector);

   overriding procedure Finalize (Self : in out UInt_32Value_Vector);

   procedure Read_Bool_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Bool_Value);

   procedure Write_Bool_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Bool_Value);

   for Bool_Value'Read use Read_Bool_Value;

   for Bool_Value'Write use Write_Bool_Value;

   type Bool_Value_Array is array (Positive range <>) of Bool_Value;

   type Bool_Value_Array_Access is access Bool_Value_Array;

   type Bool_Value_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Bool_Value_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Bool_Value_Vector);

   overriding procedure Finalize (Self : in out Bool_Value_Vector);

   procedure Read_String_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out String_Value);

   procedure Write_String_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : String_Value);

   for String_Value'Read use Read_String_Value;

   for String_Value'Write use Write_String_Value;

   type String_Value_Array is array (Positive range <>) of String_Value;

   type String_Value_Array_Access is access String_Value_Array;

   type String_Value_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : String_Value_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out String_Value_Vector);

   overriding procedure Finalize (Self : in out String_Value_Vector);

   procedure Read_Bytes_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Bytes_Value);

   procedure Write_Bytes_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Bytes_Value);

   for Bytes_Value'Read use Read_Bytes_Value;

   for Bytes_Value'Write use Write_Bytes_Value;

   type Bytes_Value_Array is array (Positive range <>) of Bytes_Value;

   type Bytes_Value_Array_Access is access Bytes_Value_Array;

   type Bytes_Value_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Bytes_Value_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Bytes_Value_Vector);

   overriding procedure Finalize (Self : in out Bytes_Value_Vector);

end Google.Protobuf.Wrappers;