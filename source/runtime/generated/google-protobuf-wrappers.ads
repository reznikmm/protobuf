with Ada.Finalization;
with Ada.Streams;
with Interfaces;
with League.Stream_Element_Vectors;
with League.Strings;

package Google.Protobuf.Wrappers is

   type Double_Value_Vector is tagged private
     with Variable_Indexing => Get_Double_Value_Variable_Reference,
     Constant_Indexing => Get_Double_Value_Constant_Reference;

   type Float_Value_Vector is tagged private
     with Variable_Indexing => Get_Float_Value_Variable_Reference,
     Constant_Indexing => Get_Float_Value_Constant_Reference;

   type Int_64Value_Vector is tagged private
     with Variable_Indexing => Get_Int_64Value_Variable_Reference,
     Constant_Indexing => Get_Int_64Value_Constant_Reference;

   type UInt_64Value_Vector is tagged private
     with Variable_Indexing => Get_UInt_64Value_Variable_Reference,
     Constant_Indexing => Get_UInt_64Value_Constant_Reference;

   type Int_32Value_Vector is tagged private
     with Variable_Indexing => Get_Int_32Value_Variable_Reference,
     Constant_Indexing => Get_Int_32Value_Constant_Reference;

   type UInt_32Value_Vector is tagged private
     with Variable_Indexing => Get_UInt_32Value_Variable_Reference,
     Constant_Indexing => Get_UInt_32Value_Constant_Reference;

   type Bool_Value_Vector is tagged private
     with Variable_Indexing => Get_Bool_Value_Variable_Reference,
     Constant_Indexing => Get_Bool_Value_Constant_Reference;

   type String_Value_Vector is tagged private
     with Variable_Indexing => Get_String_Value_Variable_Reference,
     Constant_Indexing => Get_String_Value_Constant_Reference;

   type Bytes_Value_Vector is tagged private
     with Variable_Indexing => Get_Bytes_Value_Variable_Reference,
     Constant_Indexing => Get_Bytes_Value_Constant_Reference;

   type Double_Value is
     record
        Value : Interfaces.IEEE_Float_64 := 0.0;
     end record;

   type Optional_Double_Value  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Wrappers.Double_Value;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Double_Value_Vector) return Natural;

   procedure Clear (Self : in out Double_Value_Vector);

   procedure Append (Self : in out Double_Value_Vector; V    : Double_Value);

   type Double_Value_Variable_Reference
     (Element : not null access Double_Value) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Double_Value_Variable_Reference
    (Self  : aliased in out Double_Value_Vector;
     Index : Positive)
      return Double_Value_Variable_Reference
     with Inline;

   type Double_Value_Constant_Reference
     (Element : not null access constant Double_Value) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Double_Value_Constant_Reference
    (Self  : aliased Double_Value_Vector;
     Index : Positive)
      return Double_Value_Constant_Reference
     with Inline;

   type Float_Value is
     record
        Value : Interfaces.IEEE_Float_32 := 0.0;
     end record;

   type Optional_Float_Value  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Wrappers.Float_Value;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Float_Value_Vector) return Natural;

   procedure Clear (Self : in out Float_Value_Vector);

   procedure Append (Self : in out Float_Value_Vector; V    : Float_Value);

   type Float_Value_Variable_Reference
     (Element : not null access Float_Value) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Float_Value_Variable_Reference
    (Self  : aliased in out Float_Value_Vector;
     Index : Positive)
      return Float_Value_Variable_Reference
     with Inline;

   type Float_Value_Constant_Reference
     (Element : not null access constant Float_Value) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Float_Value_Constant_Reference
    (Self  : aliased Float_Value_Vector;
     Index : Positive)
      return Float_Value_Constant_Reference
     with Inline;

   type Int_64Value is record Value : Interfaces.Integer_64 := 0; end record;

   type Optional_Int_64Value  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Wrappers.Int_64Value;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Int_64Value_Vector) return Natural;

   procedure Clear (Self : in out Int_64Value_Vector);

   procedure Append (Self : in out Int_64Value_Vector; V    : Int_64Value);

   type Int_64Value_Variable_Reference
     (Element : not null access Int_64Value) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Int_64Value_Variable_Reference
    (Self  : aliased in out Int_64Value_Vector;
     Index : Positive)
      return Int_64Value_Variable_Reference
     with Inline;

   type Int_64Value_Constant_Reference
     (Element : not null access constant Int_64Value) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Int_64Value_Constant_Reference
    (Self  : aliased Int_64Value_Vector;
     Index : Positive)
      return Int_64Value_Constant_Reference
     with Inline;

   type UInt_64Value is record Value : Interfaces.Unsigned_64 := 0; end record;

   type Optional_UInt_64Value  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Wrappers.UInt_64Value;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : UInt_64Value_Vector) return Natural;

   procedure Clear (Self : in out UInt_64Value_Vector);

   procedure Append (Self : in out UInt_64Value_Vector; V    : UInt_64Value);

   type UInt_64Value_Variable_Reference
     (Element : not null access UInt_64Value) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_UInt_64Value_Variable_Reference
    (Self  : aliased in out UInt_64Value_Vector;
     Index : Positive)
      return UInt_64Value_Variable_Reference
     with Inline;

   type UInt_64Value_Constant_Reference
     (Element : not null access constant UInt_64Value) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_UInt_64Value_Constant_Reference
    (Self  : aliased UInt_64Value_Vector;
     Index : Positive)
      return UInt_64Value_Constant_Reference
     with Inline;

   type Int_32Value is record Value : Interfaces.Integer_32 := 0; end record;

   type Optional_Int_32Value  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Wrappers.Int_32Value;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Int_32Value_Vector) return Natural;

   procedure Clear (Self : in out Int_32Value_Vector);

   procedure Append (Self : in out Int_32Value_Vector; V    : Int_32Value);

   type Int_32Value_Variable_Reference
     (Element : not null access Int_32Value) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Int_32Value_Variable_Reference
    (Self  : aliased in out Int_32Value_Vector;
     Index : Positive)
      return Int_32Value_Variable_Reference
     with Inline;

   type Int_32Value_Constant_Reference
     (Element : not null access constant Int_32Value) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Int_32Value_Constant_Reference
    (Self  : aliased Int_32Value_Vector;
     Index : Positive)
      return Int_32Value_Constant_Reference
     with Inline;

   type UInt_32Value is record Value : Interfaces.Unsigned_32 := 0; end record;

   type Optional_UInt_32Value  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Wrappers.UInt_32Value;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : UInt_32Value_Vector) return Natural;

   procedure Clear (Self : in out UInt_32Value_Vector);

   procedure Append (Self : in out UInt_32Value_Vector; V    : UInt_32Value);

   type UInt_32Value_Variable_Reference
     (Element : not null access UInt_32Value) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_UInt_32Value_Variable_Reference
    (Self  : aliased in out UInt_32Value_Vector;
     Index : Positive)
      return UInt_32Value_Variable_Reference
     with Inline;

   type UInt_32Value_Constant_Reference
     (Element : not null access constant UInt_32Value) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_UInt_32Value_Constant_Reference
    (Self  : aliased UInt_32Value_Vector;
     Index : Positive)
      return UInt_32Value_Constant_Reference
     with Inline;

   type Bool_Value is record Value : Boolean := False; end record;

   type Optional_Bool_Value  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Wrappers.Bool_Value;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Bool_Value_Vector) return Natural;

   procedure Clear (Self : in out Bool_Value_Vector);

   procedure Append (Self : in out Bool_Value_Vector; V    : Bool_Value);

   type Bool_Value_Variable_Reference
     (Element : not null access Bool_Value) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Bool_Value_Variable_Reference
    (Self  : aliased in out Bool_Value_Vector;
     Index : Positive)
      return Bool_Value_Variable_Reference
     with Inline;

   type Bool_Value_Constant_Reference
     (Element : not null access constant Bool_Value) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Bool_Value_Constant_Reference
    (Self  : aliased Bool_Value_Vector;
     Index : Positive)
      return Bool_Value_Constant_Reference
     with Inline;

   type String_Value is
     record
        Value : League.Strings.Universal_String;
     end record;

   type Optional_String_Value  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Wrappers.String_Value;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : String_Value_Vector) return Natural;

   procedure Clear (Self : in out String_Value_Vector);

   procedure Append (Self : in out String_Value_Vector; V    : String_Value);

   type String_Value_Variable_Reference
     (Element : not null access String_Value) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_String_Value_Variable_Reference
    (Self  : aliased in out String_Value_Vector;
     Index : Positive)
      return String_Value_Variable_Reference
     with Inline;

   type String_Value_Constant_Reference
     (Element : not null access constant String_Value) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_String_Value_Constant_Reference
    (Self  : aliased String_Value_Vector;
     Index : Positive)
      return String_Value_Constant_Reference
     with Inline;

   type Bytes_Value is
     record
        Value : League.Stream_Element_Vectors.Stream_Element_Vector;
     end record;

   type Optional_Bytes_Value  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Google.Protobuf.Wrappers.Bytes_Value;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Bytes_Value_Vector) return Natural;

   procedure Clear (Self : in out Bytes_Value_Vector);

   procedure Append (Self : in out Bytes_Value_Vector; V    : Bytes_Value);

   type Bytes_Value_Variable_Reference
     (Element : not null access Bytes_Value) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Bytes_Value_Variable_Reference
    (Self  : aliased in out Bytes_Value_Vector;
     Index : Positive)
      return Bytes_Value_Variable_Reference
     with Inline;

   type Bytes_Value_Constant_Reference
     (Element : not null access constant Bytes_Value) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Bytes_Value_Constant_Reference
    (Self  : aliased Bytes_Value_Vector;
     Index : Positive)
      return Bytes_Value_Constant_Reference
     with Inline;
private

   procedure Read_Double_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Double_Value);

   procedure Write_Double_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Double_Value);

   for Double_Value'Read use Read_Double_Value;

   for Double_Value'Write use Write_Double_Value;

   type Double_Value_Array is
     array (Positive range <>) of aliased Double_Value;

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

   type Float_Value_Array is array (Positive range <>) of aliased Float_Value;

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

   type Int_64Value_Array is array (Positive range <>) of aliased Int_64Value;

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

   type UInt_64Value_Array is
     array (Positive range <>) of aliased UInt_64Value;

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

   type Int_32Value_Array is array (Positive range <>) of aliased Int_32Value;

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

   type UInt_32Value_Array is
     array (Positive range <>) of aliased UInt_32Value;

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

   type Bool_Value_Array is array (Positive range <>) of aliased Bool_Value;

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

   type String_Value_Array is
     array (Positive range <>) of aliased String_Value;

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

   type Bytes_Value_Array is array (Positive range <>) of aliased Bytes_Value;

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