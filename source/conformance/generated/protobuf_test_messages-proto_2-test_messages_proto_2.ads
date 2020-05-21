with Ada.Finalization;
with Ada.Streams;
with Interfaces;
with League.Stream_Element_Vectors;
with League.String_Vectors;
with League.Strings;
with PB_Support.Boolean_Vectors;
with PB_Support.IEEE_Float_32_Vectors;
with PB_Support.IEEE_Float_64_Vectors;
with PB_Support.Integer_32_Vectors;
with PB_Support.Integer_64_Vectors;
with PB_Support.Stream_Element_Vector_Vectors;
with PB_Support.Universal_String_Vectors;
with PB_Support.Unsigned_32_Vectors;
with PB_Support.Unsigned_64_Vectors;
with PB_Support.Vectors;

package Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2 is

   type Foreign_Enum_Proto_2 is (FOREIGN_FOO, FOREIGN_BAR, FOREIGN_BAZ);

   for Foreign_Enum_Proto_2 use
     (FOREIGN_FOO => 0, FOREIGN_BAR => 1, FOREIGN_BAZ => 2);

   package Foreign_Enum_Proto_2_Vectors is
     new PB_Support.Vectors (Foreign_Enum_Proto_2);

   type Nested_Enum is (NEG, FOO, BAR, BAZ);

   for Nested_Enum use (NEG =>  - 1, FOO => 0, BAR => 1, BAZ => 2);

   package Nested_Enum_Vectors is new PB_Support.Vectors (Nested_Enum);

   type Test_All_Types_Proto_2_Vector is tagged private
     with Variable_Indexing => Get_Test_All_Types_Proto_2_Variable_Reference,
     Constant_Indexing => Get_Test_All_Types_Proto_2_Constant_Reference;

   type Nested_Message_Vector is tagged private
     with Variable_Indexing => Get_Nested_Message_Variable_Reference,
     Constant_Indexing => Get_Nested_Message_Constant_Reference;

   type Map_Int_32Int_32Entry_Vector is tagged private
     with Variable_Indexing => Get_Map_Int_32Int_32Entry_Variable_Reference,
     Constant_Indexing => Get_Map_Int_32Int_32Entry_Constant_Reference;

   type Map_Int_64Int_64Entry_Vector is tagged private
     with Variable_Indexing => Get_Map_Int_64Int_64Entry_Variable_Reference,
     Constant_Indexing => Get_Map_Int_64Int_64Entry_Constant_Reference;

   type Map_Uint_32Uint_32Entry_Vector is tagged private
     with Variable_Indexing => Get_Map_Uint_32Uint_32Entry_Variable_Reference,
     Constant_Indexing => Get_Map_Uint_32Uint_32Entry_Constant_Reference;

   type Map_Uint_64Uint_64Entry_Vector is tagged private
     with Variable_Indexing => Get_Map_Uint_64Uint_64Entry_Variable_Reference,
     Constant_Indexing => Get_Map_Uint_64Uint_64Entry_Constant_Reference;

   type Map_Sint_32Sint_32Entry_Vector is tagged private
     with Variable_Indexing => Get_Map_Sint_32Sint_32Entry_Variable_Reference,
     Constant_Indexing => Get_Map_Sint_32Sint_32Entry_Constant_Reference;

   type Map_Sint_64Sint_64Entry_Vector is tagged private
     with Variable_Indexing => Get_Map_Sint_64Sint_64Entry_Variable_Reference,
     Constant_Indexing => Get_Map_Sint_64Sint_64Entry_Constant_Reference;

   type Map_Fixed_32Fixed_32Entry_Vector is tagged private
     with Variable_Indexing =>
       Get_Map_Fixed_32Fixed_32Entry_Variable_Reference,
     Constant_Indexing => Get_Map_Fixed_32Fixed_32Entry_Constant_Reference;

   type Map_Fixed_64Fixed_64Entry_Vector is tagged private
     with Variable_Indexing =>
       Get_Map_Fixed_64Fixed_64Entry_Variable_Reference,
     Constant_Indexing => Get_Map_Fixed_64Fixed_64Entry_Constant_Reference;

   type Map_Sfixed_32Sfixed_32Entry_Vector is tagged private
     with Variable_Indexing =>
       Get_Map_Sfixed_32Sfixed_32Entry_Variable_Reference,
     Constant_Indexing => Get_Map_Sfixed_32Sfixed_32Entry_Constant_Reference;

   type Map_Sfixed_64Sfixed_64Entry_Vector is tagged private
     with Variable_Indexing =>
       Get_Map_Sfixed_64Sfixed_64Entry_Variable_Reference,
     Constant_Indexing => Get_Map_Sfixed_64Sfixed_64Entry_Constant_Reference;

   type Map_Int_32Float_Entry_Vector is tagged private
     with Variable_Indexing => Get_Map_Int_32Float_Entry_Variable_Reference,
     Constant_Indexing => Get_Map_Int_32Float_Entry_Constant_Reference;

   type Map_Int_32Double_Entry_Vector is tagged private
     with Variable_Indexing => Get_Map_Int_32Double_Entry_Variable_Reference,
     Constant_Indexing => Get_Map_Int_32Double_Entry_Constant_Reference;

   type Map_Bool_Bool_Entry_Vector is tagged private
     with Variable_Indexing => Get_Map_Bool_Bool_Entry_Variable_Reference,
     Constant_Indexing => Get_Map_Bool_Bool_Entry_Constant_Reference;

   type Map_String_String_Entry_Vector is tagged private
     with Variable_Indexing => Get_Map_String_String_Entry_Variable_Reference,
     Constant_Indexing => Get_Map_String_String_Entry_Constant_Reference;

   type Map_String_Bytes_Entry_Vector is tagged private
     with Variable_Indexing => Get_Map_String_Bytes_Entry_Variable_Reference,
     Constant_Indexing => Get_Map_String_Bytes_Entry_Constant_Reference;

   type Map_String_Nested_Message_Entry_Vector is tagged private
     with Variable_Indexing =>
       Get_Map_String_Nested_Message_Entry_Variable_Reference,
     Constant_Indexing =>
       Get_Map_String_Nested_Message_Entry_Constant_Reference;

   type Map_String_Foreign_Message_Entry_Vector is tagged private
     with Variable_Indexing =>
       Get_Map_String_Foreign_Message_Entry_Variable_Reference,
     Constant_Indexing =>
       Get_Map_String_Foreign_Message_Entry_Constant_Reference;

   type Map_String_Nested_Enum_Entry_Vector is tagged private
     with Variable_Indexing =>
       Get_Map_String_Nested_Enum_Entry_Variable_Reference,
     Constant_Indexing => Get_Map_String_Nested_Enum_Entry_Constant_Reference;

   type Map_String_Foreign_Enum_Entry_Vector is tagged private
     with Variable_Indexing =>
       Get_Map_String_Foreign_Enum_Entry_Variable_Reference,
     Constant_Indexing => Get_Map_String_Foreign_Enum_Entry_Constant_Reference;

   type Data_Vector is tagged private
     with Variable_Indexing => Get_Data_Variable_Reference,
     Constant_Indexing => Get_Data_Constant_Reference;

   type Message_Set_Correct_Vector is tagged private
     with Variable_Indexing => Get_Message_Set_Correct_Variable_Reference,
     Constant_Indexing => Get_Message_Set_Correct_Constant_Reference;

   type Message_Set_Correct_Extension_1_Vector is tagged private
     with Variable_Indexing =>
       Get_Message_Set_Correct_Extension_1_Variable_Reference,
     Constant_Indexing =>
       Get_Message_Set_Correct_Extension_1_Constant_Reference;

   type Message_Set_Correct_Extension_2_Vector is tagged private
     with Variable_Indexing =>
       Get_Message_Set_Correct_Extension_2_Variable_Reference,
     Constant_Indexing =>
       Get_Message_Set_Correct_Extension_2_Constant_Reference;

   type Foreign_Message_Proto_2_Vector is tagged private
     with Variable_Indexing => Get_Foreign_Message_Proto_2_Variable_Reference,
     Constant_Indexing => Get_Foreign_Message_Proto_2_Constant_Reference;

   type Unknown_To_Test_All_Types_Vector is tagged private
     with Variable_Indexing =>
       Get_Unknown_To_Test_All_Types_Variable_Reference,
     Constant_Indexing => Get_Unknown_To_Test_All_Types_Constant_Reference;

   type Optional_Group_Vector is tagged private
     with Variable_Indexing => Get_Optional_Group_Variable_Reference,
     Constant_Indexing => Get_Optional_Group_Constant_Reference;

   type Map_Int_32Int_32Entry is
     record
        Key   : PB_Support.Integer_32_Vectors.Option;
        Value : PB_Support.Integer_32_Vectors.Option;
     end record;

   type Optional_Map_Int_32Int_32Entry  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Map_Int_32Int_32Entry;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Map_Int_32Int_32Entry_Vector) return Natural;

   procedure Clear (Self : in out Map_Int_32Int_32Entry_Vector);

   procedure Append
    (Self : in out Map_Int_32Int_32Entry_Vector;
     V    : Map_Int_32Int_32Entry);

   type Map_Int_32Int_32Entry_Variable_Reference
     (Element : not null access Map_Int_32Int_32Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Int_32Int_32Entry_Variable_Reference
    (Self  : aliased in out Map_Int_32Int_32Entry_Vector;
     Index : Positive)
      return Map_Int_32Int_32Entry_Variable_Reference
     with Inline;

   type Map_Int_32Int_32Entry_Constant_Reference
     (Element : not null access constant Map_Int_32Int_32Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Int_32Int_32Entry_Constant_Reference
    (Self  : aliased Map_Int_32Int_32Entry_Vector;
     Index : Positive)
      return Map_Int_32Int_32Entry_Constant_Reference
     with Inline;

   type Map_Int_64Int_64Entry is
     record
        Key   : PB_Support.Integer_64_Vectors.Option;
        Value : PB_Support.Integer_64_Vectors.Option;
     end record;

   type Optional_Map_Int_64Int_64Entry  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Map_Int_64Int_64Entry;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Map_Int_64Int_64Entry_Vector) return Natural;

   procedure Clear (Self : in out Map_Int_64Int_64Entry_Vector);

   procedure Append
    (Self : in out Map_Int_64Int_64Entry_Vector;
     V    : Map_Int_64Int_64Entry);

   type Map_Int_64Int_64Entry_Variable_Reference
     (Element : not null access Map_Int_64Int_64Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Int_64Int_64Entry_Variable_Reference
    (Self  : aliased in out Map_Int_64Int_64Entry_Vector;
     Index : Positive)
      return Map_Int_64Int_64Entry_Variable_Reference
     with Inline;

   type Map_Int_64Int_64Entry_Constant_Reference
     (Element : not null access constant Map_Int_64Int_64Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Int_64Int_64Entry_Constant_Reference
    (Self  : aliased Map_Int_64Int_64Entry_Vector;
     Index : Positive)
      return Map_Int_64Int_64Entry_Constant_Reference
     with Inline;

   type Map_Uint_32Uint_32Entry is
     record
        Key   : PB_Support.Unsigned_32_Vectors.Option;
        Value : PB_Support.Unsigned_32_Vectors.Option;
     end record;

   type Optional_Map_Uint_32Uint_32Entry  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Map_Uint_32Uint_32Entry;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Map_Uint_32Uint_32Entry_Vector) return Natural;

   procedure Clear (Self : in out Map_Uint_32Uint_32Entry_Vector);

   procedure Append
    (Self : in out Map_Uint_32Uint_32Entry_Vector;
     V    : Map_Uint_32Uint_32Entry);

   type Map_Uint_32Uint_32Entry_Variable_Reference
     (Element : not null access Map_Uint_32Uint_32Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Uint_32Uint_32Entry_Variable_Reference
    (Self  : aliased in out Map_Uint_32Uint_32Entry_Vector;
     Index : Positive)
      return Map_Uint_32Uint_32Entry_Variable_Reference
     with Inline;

   type Map_Uint_32Uint_32Entry_Constant_Reference
     (Element : not null access constant Map_Uint_32Uint_32Entry) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Uint_32Uint_32Entry_Constant_Reference
    (Self  : aliased Map_Uint_32Uint_32Entry_Vector;
     Index : Positive)
      return Map_Uint_32Uint_32Entry_Constant_Reference
     with Inline;

   type Map_Uint_64Uint_64Entry is
     record
        Key   : PB_Support.Unsigned_64_Vectors.Option;
        Value : PB_Support.Unsigned_64_Vectors.Option;
     end record;

   type Optional_Map_Uint_64Uint_64Entry  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Map_Uint_64Uint_64Entry;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Map_Uint_64Uint_64Entry_Vector) return Natural;

   procedure Clear (Self : in out Map_Uint_64Uint_64Entry_Vector);

   procedure Append
    (Self : in out Map_Uint_64Uint_64Entry_Vector;
     V    : Map_Uint_64Uint_64Entry);

   type Map_Uint_64Uint_64Entry_Variable_Reference
     (Element : not null access Map_Uint_64Uint_64Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Uint_64Uint_64Entry_Variable_Reference
    (Self  : aliased in out Map_Uint_64Uint_64Entry_Vector;
     Index : Positive)
      return Map_Uint_64Uint_64Entry_Variable_Reference
     with Inline;

   type Map_Uint_64Uint_64Entry_Constant_Reference
     (Element : not null access constant Map_Uint_64Uint_64Entry) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Uint_64Uint_64Entry_Constant_Reference
    (Self  : aliased Map_Uint_64Uint_64Entry_Vector;
     Index : Positive)
      return Map_Uint_64Uint_64Entry_Constant_Reference
     with Inline;

   type Map_Sint_32Sint_32Entry is
     record
        Key   : PB_Support.Integer_32_Vectors.Option;
        Value : PB_Support.Integer_32_Vectors.Option;
     end record;

   type Optional_Map_Sint_32Sint_32Entry  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Map_Sint_32Sint_32Entry;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Map_Sint_32Sint_32Entry_Vector) return Natural;

   procedure Clear (Self : in out Map_Sint_32Sint_32Entry_Vector);

   procedure Append
    (Self : in out Map_Sint_32Sint_32Entry_Vector;
     V    : Map_Sint_32Sint_32Entry);

   type Map_Sint_32Sint_32Entry_Variable_Reference
     (Element : not null access Map_Sint_32Sint_32Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Sint_32Sint_32Entry_Variable_Reference
    (Self  : aliased in out Map_Sint_32Sint_32Entry_Vector;
     Index : Positive)
      return Map_Sint_32Sint_32Entry_Variable_Reference
     with Inline;

   type Map_Sint_32Sint_32Entry_Constant_Reference
     (Element : not null access constant Map_Sint_32Sint_32Entry) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Sint_32Sint_32Entry_Constant_Reference
    (Self  : aliased Map_Sint_32Sint_32Entry_Vector;
     Index : Positive)
      return Map_Sint_32Sint_32Entry_Constant_Reference
     with Inline;

   type Map_Sint_64Sint_64Entry is
     record
        Key   : PB_Support.Integer_64_Vectors.Option;
        Value : PB_Support.Integer_64_Vectors.Option;
     end record;

   type Optional_Map_Sint_64Sint_64Entry  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Map_Sint_64Sint_64Entry;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Map_Sint_64Sint_64Entry_Vector) return Natural;

   procedure Clear (Self : in out Map_Sint_64Sint_64Entry_Vector);

   procedure Append
    (Self : in out Map_Sint_64Sint_64Entry_Vector;
     V    : Map_Sint_64Sint_64Entry);

   type Map_Sint_64Sint_64Entry_Variable_Reference
     (Element : not null access Map_Sint_64Sint_64Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Sint_64Sint_64Entry_Variable_Reference
    (Self  : aliased in out Map_Sint_64Sint_64Entry_Vector;
     Index : Positive)
      return Map_Sint_64Sint_64Entry_Variable_Reference
     with Inline;

   type Map_Sint_64Sint_64Entry_Constant_Reference
     (Element : not null access constant Map_Sint_64Sint_64Entry) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Sint_64Sint_64Entry_Constant_Reference
    (Self  : aliased Map_Sint_64Sint_64Entry_Vector;
     Index : Positive)
      return Map_Sint_64Sint_64Entry_Constant_Reference
     with Inline;

   type Map_Fixed_32Fixed_32Entry is
     record
        Key   : PB_Support.Unsigned_32_Vectors.Option;
        Value : PB_Support.Unsigned_32_Vectors.Option;
     end record;

   type Optional_Map_Fixed_32Fixed_32Entry  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Map_Fixed_32Fixed_32Entry;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Map_Fixed_32Fixed_32Entry_Vector) return Natural;

   procedure Clear (Self : in out Map_Fixed_32Fixed_32Entry_Vector);

   procedure Append
    (Self : in out Map_Fixed_32Fixed_32Entry_Vector;
     V    : Map_Fixed_32Fixed_32Entry);

   type Map_Fixed_32Fixed_32Entry_Variable_Reference
     (Element : not null access Map_Fixed_32Fixed_32Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Fixed_32Fixed_32Entry_Variable_Reference
    (Self  : aliased in out Map_Fixed_32Fixed_32Entry_Vector;
     Index : Positive)
      return Map_Fixed_32Fixed_32Entry_Variable_Reference
     with Inline;

   type Map_Fixed_32Fixed_32Entry_Constant_Reference
     (Element : not null access constant Map_Fixed_32Fixed_32Entry) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Fixed_32Fixed_32Entry_Constant_Reference
    (Self  : aliased Map_Fixed_32Fixed_32Entry_Vector;
     Index : Positive)
      return Map_Fixed_32Fixed_32Entry_Constant_Reference
     with Inline;

   type Map_Fixed_64Fixed_64Entry is
     record
        Key   : PB_Support.Unsigned_64_Vectors.Option;
        Value : PB_Support.Unsigned_64_Vectors.Option;
     end record;

   type Optional_Map_Fixed_64Fixed_64Entry  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Map_Fixed_64Fixed_64Entry;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Map_Fixed_64Fixed_64Entry_Vector) return Natural;

   procedure Clear (Self : in out Map_Fixed_64Fixed_64Entry_Vector);

   procedure Append
    (Self : in out Map_Fixed_64Fixed_64Entry_Vector;
     V    : Map_Fixed_64Fixed_64Entry);

   type Map_Fixed_64Fixed_64Entry_Variable_Reference
     (Element : not null access Map_Fixed_64Fixed_64Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Fixed_64Fixed_64Entry_Variable_Reference
    (Self  : aliased in out Map_Fixed_64Fixed_64Entry_Vector;
     Index : Positive)
      return Map_Fixed_64Fixed_64Entry_Variable_Reference
     with Inline;

   type Map_Fixed_64Fixed_64Entry_Constant_Reference
     (Element : not null access constant Map_Fixed_64Fixed_64Entry) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Fixed_64Fixed_64Entry_Constant_Reference
    (Self  : aliased Map_Fixed_64Fixed_64Entry_Vector;
     Index : Positive)
      return Map_Fixed_64Fixed_64Entry_Constant_Reference
     with Inline;

   type Map_Sfixed_32Sfixed_32Entry is
     record
        Key   : PB_Support.Integer_32_Vectors.Option;
        Value : PB_Support.Integer_32_Vectors.Option;
     end record;

   type Optional_Map_Sfixed_32Sfixed_32Entry  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Map_Sfixed_32Sfixed_32Entry;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Map_Sfixed_32Sfixed_32Entry_Vector) return Natural;

   procedure Clear (Self : in out Map_Sfixed_32Sfixed_32Entry_Vector);

   procedure Append
    (Self : in out Map_Sfixed_32Sfixed_32Entry_Vector;
     V    : Map_Sfixed_32Sfixed_32Entry);

   type Map_Sfixed_32Sfixed_32Entry_Variable_Reference
     (Element : not null access Map_Sfixed_32Sfixed_32Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Sfixed_32Sfixed_32Entry_Variable_Reference
    (Self  : aliased in out Map_Sfixed_32Sfixed_32Entry_Vector;
     Index : Positive)
      return Map_Sfixed_32Sfixed_32Entry_Variable_Reference
     with Inline;

   type Map_Sfixed_32Sfixed_32Entry_Constant_Reference
     (Element : not null access constant Map_Sfixed_32Sfixed_32Entry) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Sfixed_32Sfixed_32Entry_Constant_Reference
    (Self  : aliased Map_Sfixed_32Sfixed_32Entry_Vector;
     Index : Positive)
      return Map_Sfixed_32Sfixed_32Entry_Constant_Reference
     with Inline;

   type Map_Sfixed_64Sfixed_64Entry is
     record
        Key   : PB_Support.Integer_64_Vectors.Option;
        Value : PB_Support.Integer_64_Vectors.Option;
     end record;

   type Optional_Map_Sfixed_64Sfixed_64Entry  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Map_Sfixed_64Sfixed_64Entry;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Map_Sfixed_64Sfixed_64Entry_Vector) return Natural;

   procedure Clear (Self : in out Map_Sfixed_64Sfixed_64Entry_Vector);

   procedure Append
    (Self : in out Map_Sfixed_64Sfixed_64Entry_Vector;
     V    : Map_Sfixed_64Sfixed_64Entry);

   type Map_Sfixed_64Sfixed_64Entry_Variable_Reference
     (Element : not null access Map_Sfixed_64Sfixed_64Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Sfixed_64Sfixed_64Entry_Variable_Reference
    (Self  : aliased in out Map_Sfixed_64Sfixed_64Entry_Vector;
     Index : Positive)
      return Map_Sfixed_64Sfixed_64Entry_Variable_Reference
     with Inline;

   type Map_Sfixed_64Sfixed_64Entry_Constant_Reference
     (Element : not null access constant Map_Sfixed_64Sfixed_64Entry) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Sfixed_64Sfixed_64Entry_Constant_Reference
    (Self  : aliased Map_Sfixed_64Sfixed_64Entry_Vector;
     Index : Positive)
      return Map_Sfixed_64Sfixed_64Entry_Constant_Reference
     with Inline;

   type Map_Int_32Float_Entry is
     record
        Key   : PB_Support.Integer_32_Vectors.Option;
        Value : PB_Support.IEEE_Float_32_Vectors.Option;
     end record;

   type Optional_Map_Int_32Float_Entry  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Map_Int_32Float_Entry;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Map_Int_32Float_Entry_Vector) return Natural;

   procedure Clear (Self : in out Map_Int_32Float_Entry_Vector);

   procedure Append
    (Self : in out Map_Int_32Float_Entry_Vector;
     V    : Map_Int_32Float_Entry);

   type Map_Int_32Float_Entry_Variable_Reference
     (Element : not null access Map_Int_32Float_Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Int_32Float_Entry_Variable_Reference
    (Self  : aliased in out Map_Int_32Float_Entry_Vector;
     Index : Positive)
      return Map_Int_32Float_Entry_Variable_Reference
     with Inline;

   type Map_Int_32Float_Entry_Constant_Reference
     (Element : not null access constant Map_Int_32Float_Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Int_32Float_Entry_Constant_Reference
    (Self  : aliased Map_Int_32Float_Entry_Vector;
     Index : Positive)
      return Map_Int_32Float_Entry_Constant_Reference
     with Inline;

   type Map_Int_32Double_Entry is
     record
        Key   : PB_Support.Integer_32_Vectors.Option;
        Value : PB_Support.IEEE_Float_64_Vectors.Option;
     end record;

   type Optional_Map_Int_32Double_Entry  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Map_Int_32Double_Entry;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Map_Int_32Double_Entry_Vector) return Natural;

   procedure Clear (Self : in out Map_Int_32Double_Entry_Vector);

   procedure Append
    (Self : in out Map_Int_32Double_Entry_Vector;
     V    : Map_Int_32Double_Entry);

   type Map_Int_32Double_Entry_Variable_Reference
     (Element : not null access Map_Int_32Double_Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Int_32Double_Entry_Variable_Reference
    (Self  : aliased in out Map_Int_32Double_Entry_Vector;
     Index : Positive)
      return Map_Int_32Double_Entry_Variable_Reference
     with Inline;

   type Map_Int_32Double_Entry_Constant_Reference
     (Element : not null access constant Map_Int_32Double_Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Int_32Double_Entry_Constant_Reference
    (Self  : aliased Map_Int_32Double_Entry_Vector;
     Index : Positive)
      return Map_Int_32Double_Entry_Constant_Reference
     with Inline;

   type Map_Bool_Bool_Entry is
     record
        Key   : PB_Support.Boolean_Vectors.Option;
        Value : PB_Support.Boolean_Vectors.Option;
     end record;

   type Optional_Map_Bool_Bool_Entry  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Map_Bool_Bool_Entry;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Map_Bool_Bool_Entry_Vector) return Natural;

   procedure Clear (Self : in out Map_Bool_Bool_Entry_Vector);

   procedure Append
    (Self : in out Map_Bool_Bool_Entry_Vector;
     V    : Map_Bool_Bool_Entry);

   type Map_Bool_Bool_Entry_Variable_Reference
     (Element : not null access Map_Bool_Bool_Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Bool_Bool_Entry_Variable_Reference
    (Self  : aliased in out Map_Bool_Bool_Entry_Vector;
     Index : Positive)
      return Map_Bool_Bool_Entry_Variable_Reference
     with Inline;

   type Map_Bool_Bool_Entry_Constant_Reference
     (Element : not null access constant Map_Bool_Bool_Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Bool_Bool_Entry_Constant_Reference
    (Self  : aliased Map_Bool_Bool_Entry_Vector;
     Index : Positive)
      return Map_Bool_Bool_Entry_Constant_Reference
     with Inline;

   type Map_String_String_Entry is
     record
        Key   : PB_Support.Universal_String_Vectors.Option;
        Value : PB_Support.Universal_String_Vectors.Option;
     end record;

   type Optional_Map_String_String_Entry  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Map_String_String_Entry;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Map_String_String_Entry_Vector) return Natural;

   procedure Clear (Self : in out Map_String_String_Entry_Vector);

   procedure Append
    (Self : in out Map_String_String_Entry_Vector;
     V    : Map_String_String_Entry);

   type Map_String_String_Entry_Variable_Reference
     (Element : not null access Map_String_String_Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_String_String_Entry_Variable_Reference
    (Self  : aliased in out Map_String_String_Entry_Vector;
     Index : Positive)
      return Map_String_String_Entry_Variable_Reference
     with Inline;

   type Map_String_String_Entry_Constant_Reference
     (Element : not null access constant Map_String_String_Entry) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_String_String_Entry_Constant_Reference
    (Self  : aliased Map_String_String_Entry_Vector;
     Index : Positive)
      return Map_String_String_Entry_Constant_Reference
     with Inline;

   type Map_String_Bytes_Entry is
     record
        Key   : PB_Support.Universal_String_Vectors.Option;
        Value : PB_Support.Stream_Element_Vector_Vectors.Option;
     end record;

   type Optional_Map_String_Bytes_Entry  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Map_String_Bytes_Entry;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Map_String_Bytes_Entry_Vector) return Natural;

   procedure Clear (Self : in out Map_String_Bytes_Entry_Vector);

   procedure Append
    (Self : in out Map_String_Bytes_Entry_Vector;
     V    : Map_String_Bytes_Entry);

   type Map_String_Bytes_Entry_Variable_Reference
     (Element : not null access Map_String_Bytes_Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_String_Bytes_Entry_Variable_Reference
    (Self  : aliased in out Map_String_Bytes_Entry_Vector;
     Index : Positive)
      return Map_String_Bytes_Entry_Variable_Reference
     with Inline;

   type Map_String_Bytes_Entry_Constant_Reference
     (Element : not null access constant Map_String_Bytes_Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_String_Bytes_Entry_Constant_Reference
    (Self  : aliased Map_String_Bytes_Entry_Vector;
     Index : Positive)
      return Map_String_Bytes_Entry_Constant_Reference
     with Inline;

   type Map_String_Nested_Enum_Entry is
     record
        Key   : PB_Support.Universal_String_Vectors.Option;
        Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Nested_Enum_Vectors.Option;
     end record;

   type Optional_Map_String_Nested_Enum_Entry  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Map_String_Nested_Enum_Entry;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Map_String_Nested_Enum_Entry_Vector) return Natural;

   procedure Clear (Self : in out Map_String_Nested_Enum_Entry_Vector);

   procedure Append
    (Self : in out Map_String_Nested_Enum_Entry_Vector;
     V    : Map_String_Nested_Enum_Entry);

   type Map_String_Nested_Enum_Entry_Variable_Reference
     (Element : not null access Map_String_Nested_Enum_Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_String_Nested_Enum_Entry_Variable_Reference
    (Self  : aliased in out Map_String_Nested_Enum_Entry_Vector;
     Index : Positive)
      return Map_String_Nested_Enum_Entry_Variable_Reference
     with Inline;

   type Map_String_Nested_Enum_Entry_Constant_Reference
     (Element : not null access constant Map_String_Nested_Enum_Entry) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_String_Nested_Enum_Entry_Constant_Reference
    (Self  : aliased Map_String_Nested_Enum_Entry_Vector;
     Index : Positive)
      return Map_String_Nested_Enum_Entry_Constant_Reference
     with Inline;

   type Map_String_Foreign_Enum_Entry is
     record
        Key   : PB_Support.Universal_String_Vectors.Option;
        Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Foreign_Enum_Proto_2_Vectors.Option;
     end record;

   type Optional_Map_String_Foreign_Enum_Entry  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Map_String_Foreign_Enum_Entry;
           when False =>
              null;
        end case;
     end record;

   function Length
    (Self : Map_String_Foreign_Enum_Entry_Vector)
      return Natural;

   procedure Clear (Self : in out Map_String_Foreign_Enum_Entry_Vector);

   procedure Append
    (Self : in out Map_String_Foreign_Enum_Entry_Vector;
     V    : Map_String_Foreign_Enum_Entry);

   type Map_String_Foreign_Enum_Entry_Variable_Reference
     (Element : not null access Map_String_Foreign_Enum_Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_String_Foreign_Enum_Entry_Variable_Reference
    (Self  : aliased in out Map_String_Foreign_Enum_Entry_Vector;
     Index : Positive)
      return Map_String_Foreign_Enum_Entry_Variable_Reference
     with Inline;

   type Map_String_Foreign_Enum_Entry_Constant_Reference
     (Element : not null access constant Map_String_Foreign_Enum_Entry) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_String_Foreign_Enum_Entry_Constant_Reference
    (Self  : aliased Map_String_Foreign_Enum_Entry_Vector;
     Index : Positive)
      return Map_String_Foreign_Enum_Entry_Constant_Reference
     with Inline;

   type Data is
     record
        Group_Int_32  : PB_Support.Integer_32_Vectors.Option;
        Group_Uint_32 : PB_Support.Unsigned_32_Vectors.Option;
     end record;

   type Optional_Data  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Data;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Data_Vector) return Natural;

   procedure Clear (Self : in out Data_Vector);

   procedure Append (Self : in out Data_Vector; V    : Data);

   type Data_Variable_Reference  (Element : not null access Data) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Data_Variable_Reference
    (Self  : aliased in out Data_Vector;
     Index : Positive)
      return Data_Variable_Reference
     with Inline;

   type Data_Constant_Reference  (Element : not null access constant Data) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Data_Constant_Reference
    (Self  : aliased Data_Vector;
     Index : Positive)
      return Data_Constant_Reference
     with Inline;

   type Message_Set_Correct is null record;

   type Optional_Message_Set_Correct  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Message_Set_Correct;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Message_Set_Correct_Vector) return Natural;

   procedure Clear (Self : in out Message_Set_Correct_Vector);

   procedure Append
    (Self : in out Message_Set_Correct_Vector;
     V    : Message_Set_Correct);

   type Message_Set_Correct_Variable_Reference
     (Element : not null access Message_Set_Correct) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Message_Set_Correct_Variable_Reference
    (Self  : aliased in out Message_Set_Correct_Vector;
     Index : Positive)
      return Message_Set_Correct_Variable_Reference
     with Inline;

   type Message_Set_Correct_Constant_Reference
     (Element : not null access constant Message_Set_Correct) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Message_Set_Correct_Constant_Reference
    (Self  : aliased Message_Set_Correct_Vector;
     Index : Positive)
      return Message_Set_Correct_Constant_Reference
     with Inline;

   type Message_Set_Correct_Extension_1 is
     record
        Str : PB_Support.Universal_String_Vectors.Option;
     end record;

   type Optional_Message_Set_Correct_Extension_1
     (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Message_Set_Correct_Extension_1;
           when False =>
              null;
        end case;
     end record;

   function Length
    (Self : Message_Set_Correct_Extension_1_Vector)
      return Natural;

   procedure Clear (Self : in out Message_Set_Correct_Extension_1_Vector);

   procedure Append
    (Self : in out Message_Set_Correct_Extension_1_Vector;
     V    : Message_Set_Correct_Extension_1);

   type Message_Set_Correct_Extension_1_Variable_Reference
     (Element : not null access Message_Set_Correct_Extension_1) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Message_Set_Correct_Extension_1_Variable_Reference
    (Self  : aliased in out Message_Set_Correct_Extension_1_Vector;
     Index : Positive)
      return Message_Set_Correct_Extension_1_Variable_Reference
     with Inline;

   type Message_Set_Correct_Extension_1_Constant_Reference
     (Element : not null access constant Message_Set_Correct_Extension_1) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Message_Set_Correct_Extension_1_Constant_Reference
    (Self  : aliased Message_Set_Correct_Extension_1_Vector;
     Index : Positive)
      return Message_Set_Correct_Extension_1_Constant_Reference
     with Inline;

   type Message_Set_Correct_Extension_2 is
     record
        I : PB_Support.Integer_32_Vectors.Option;
     end record;

   type Optional_Message_Set_Correct_Extension_2
     (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Message_Set_Correct_Extension_2;
           when False =>
              null;
        end case;
     end record;

   function Length
    (Self : Message_Set_Correct_Extension_2_Vector)
      return Natural;

   procedure Clear (Self : in out Message_Set_Correct_Extension_2_Vector);

   procedure Append
    (Self : in out Message_Set_Correct_Extension_2_Vector;
     V    : Message_Set_Correct_Extension_2);

   type Message_Set_Correct_Extension_2_Variable_Reference
     (Element : not null access Message_Set_Correct_Extension_2) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Message_Set_Correct_Extension_2_Variable_Reference
    (Self  : aliased in out Message_Set_Correct_Extension_2_Vector;
     Index : Positive)
      return Message_Set_Correct_Extension_2_Variable_Reference
     with Inline;

   type Message_Set_Correct_Extension_2_Constant_Reference
     (Element : not null access constant Message_Set_Correct_Extension_2) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Message_Set_Correct_Extension_2_Constant_Reference
    (Self  : aliased Message_Set_Correct_Extension_2_Vector;
     Index : Positive)
      return Message_Set_Correct_Extension_2_Constant_Reference
     with Inline;

   type Foreign_Message_Proto_2 is
     record
        C : PB_Support.Integer_32_Vectors.Option;
     end record;

   type Optional_Foreign_Message_Proto_2  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Foreign_Message_Proto_2;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Foreign_Message_Proto_2_Vector) return Natural;

   procedure Clear (Self : in out Foreign_Message_Proto_2_Vector);

   procedure Append
    (Self : in out Foreign_Message_Proto_2_Vector;
     V    : Foreign_Message_Proto_2);

   type Foreign_Message_Proto_2_Variable_Reference
     (Element : not null access Foreign_Message_Proto_2) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Foreign_Message_Proto_2_Variable_Reference
    (Self  : aliased in out Foreign_Message_Proto_2_Vector;
     Index : Positive)
      return Foreign_Message_Proto_2_Variable_Reference
     with Inline;

   type Foreign_Message_Proto_2_Constant_Reference
     (Element : not null access constant Foreign_Message_Proto_2) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Foreign_Message_Proto_2_Constant_Reference
    (Self  : aliased Foreign_Message_Proto_2_Vector;
     Index : Positive)
      return Foreign_Message_Proto_2_Constant_Reference
     with Inline;

   type Optional_Group is
     record
        A : PB_Support.Integer_32_Vectors.Option;
     end record;

   type Optional_Optional_Group  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Optional_Group;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Optional_Group_Vector) return Natural;

   procedure Clear (Self : in out Optional_Group_Vector);

   procedure Append
    (Self : in out Optional_Group_Vector;
     V    : Optional_Group);

   type Optional_Group_Variable_Reference
     (Element : not null access Optional_Group) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Optional_Group_Variable_Reference
    (Self  : aliased in out Optional_Group_Vector;
     Index : Positive)
      return Optional_Group_Variable_Reference
     with Inline;

   type Optional_Group_Constant_Reference
     (Element : not null access constant Optional_Group) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Optional_Group_Constant_Reference
    (Self  : aliased Optional_Group_Vector;
     Index : Positive)
      return Optional_Group_Constant_Reference
     with Inline;

   type Unknown_To_Test_All_Types is
     record
        Optional_Int_32 : PB_Support.Integer_32_Vectors.Option;
        Optional_String : PB_Support.Universal_String_Vectors.Option;
        Nested_Message  : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Optional_Foreign_Message_Proto_2;
        Optionalgroup   : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Optional_Optional_Group;
        Optional_Bool   : PB_Support.Boolean_Vectors.Option;
        Repeated_Int_32 : PB_Support.Integer_32_Vectors.Vector;
     end record;

   type Optional_Unknown_To_Test_All_Types  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Unknown_To_Test_All_Types;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Unknown_To_Test_All_Types_Vector) return Natural;

   procedure Clear (Self : in out Unknown_To_Test_All_Types_Vector);

   procedure Append
    (Self : in out Unknown_To_Test_All_Types_Vector;
     V    : Unknown_To_Test_All_Types);

   type Unknown_To_Test_All_Types_Variable_Reference
     (Element : not null access Unknown_To_Test_All_Types) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Unknown_To_Test_All_Types_Variable_Reference
    (Self  : aliased in out Unknown_To_Test_All_Types_Vector;
     Index : Positive)
      return Unknown_To_Test_All_Types_Variable_Reference
     with Inline;

   type Unknown_To_Test_All_Types_Constant_Reference
     (Element : not null access constant Unknown_To_Test_All_Types) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Unknown_To_Test_All_Types_Constant_Reference
    (Self  : aliased Unknown_To_Test_All_Types_Vector;
     Index : Positive)
      return Unknown_To_Test_All_Types_Constant_Reference
     with Inline;

   type Map_String_Foreign_Message_Entry is
     record
        Key   : PB_Support.Universal_String_Vectors.Option;
        Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Optional_Foreign_Message_Proto_2;
     end record;

   type Optional_Map_String_Foreign_Message_Entry
     (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Map_String_Foreign_Message_Entry;
           when False =>
              null;
        end case;
     end record;

   function Length
    (Self : Map_String_Foreign_Message_Entry_Vector)
      return Natural;

   procedure Clear (Self : in out Map_String_Foreign_Message_Entry_Vector);

   procedure Append
    (Self : in out Map_String_Foreign_Message_Entry_Vector;
     V    : Map_String_Foreign_Message_Entry);

   type Map_String_Foreign_Message_Entry_Variable_Reference
     (Element : not null access Map_String_Foreign_Message_Entry) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_String_Foreign_Message_Entry_Variable_Reference
    (Self  : aliased in out Map_String_Foreign_Message_Entry_Vector;
     Index : Positive)
      return Map_String_Foreign_Message_Entry_Variable_Reference
     with Inline;

   type Map_String_Foreign_Message_Entry_Constant_Reference
     (Element : not null access constant Map_String_Foreign_Message_Entry) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_String_Foreign_Message_Entry_Constant_Reference
    (Self  : aliased Map_String_Foreign_Message_Entry_Vector;
     Index : Positive)
      return Map_String_Foreign_Message_Entry_Constant_Reference
     with Inline;

   type Nested_Message is
     record
        A           : PB_Support.Integer_32_Vectors.Option;
        Corecursive : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Test_All_Types_Proto_2_Vector;
     end record;

   type Optional_Nested_Message  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Nested_Message;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Nested_Message_Vector) return Natural;

   procedure Clear (Self : in out Nested_Message_Vector);

   procedure Append
    (Self : in out Nested_Message_Vector;
     V    : Nested_Message);

   type Nested_Message_Variable_Reference
     (Element : not null access Nested_Message) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Nested_Message_Variable_Reference
    (Self  : aliased in out Nested_Message_Vector;
     Index : Positive)
      return Nested_Message_Variable_Reference
     with Inline;

   type Nested_Message_Constant_Reference
     (Element : not null access constant Nested_Message) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Nested_Message_Constant_Reference
    (Self  : aliased Nested_Message_Vector;
     Index : Positive)
      return Nested_Message_Constant_Reference
     with Inline;

   type Map_String_Nested_Message_Entry is
     record
        Key   : PB_Support.Universal_String_Vectors.Option;
        Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Optional_Nested_Message;
     end record;

   type Optional_Map_String_Nested_Message_Entry
     (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Map_String_Nested_Message_Entry;
           when False =>
              null;
        end case;
     end record;

   function Length
    (Self : Map_String_Nested_Message_Entry_Vector)
      return Natural;

   procedure Clear (Self : in out Map_String_Nested_Message_Entry_Vector);

   procedure Append
    (Self : in out Map_String_Nested_Message_Entry_Vector;
     V    : Map_String_Nested_Message_Entry);

   type Map_String_Nested_Message_Entry_Variable_Reference
     (Element : not null access Map_String_Nested_Message_Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_String_Nested_Message_Entry_Variable_Reference
    (Self  : aliased in out Map_String_Nested_Message_Entry_Vector;
     Index : Positive)
      return Map_String_Nested_Message_Entry_Variable_Reference
     with Inline;

   type Map_String_Nested_Message_Entry_Constant_Reference
     (Element : not null access constant Map_String_Nested_Message_Entry) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_String_Nested_Message_Entry_Constant_Reference
    (Self  : aliased Map_String_Nested_Message_Entry_Vector;
     Index : Positive)
      return Map_String_Nested_Message_Entry_Constant_Reference
     with Inline;

   type Test_All_Types_Proto_2_Variant_Kind is
     (Oneof_Field_Not_Set,
      Oneof_Uint_32_Kind ,
      Oneof_Nested_Message_Kind,
      Oneof_String_Kind  ,
      Oneof_Bytes_Kind   ,
      Oneof_Bool_Kind    ,
      Oneof_Uint_64_Kind ,
      Oneof_Float_Kind   ,
      Oneof_Double_Kind  ,
      Oneof_Enum_Kind    );

   type Test_All_Types_Proto_2_Variant
     (Oneof_Field : Test_All_Types_Proto_2_Variant_Kind := Oneof_Field_Not_Set) is
     record
        case Oneof_Field is
           when Oneof_Field_Not_Set =>
              null;
           when Oneof_Uint_32_Kind =>
              Oneof_Uint_32 : Interfaces.Unsigned_32 := 0;
           when Oneof_Nested_Message_Kind =>
              Oneof_Nested_Message : Protobuf_Test_Messages.Proto_2
                .Test_Messages_Proto_2.Nested_Message;
           when Oneof_String_Kind =>
              Oneof_String : League.Strings.Universal_String;
           when Oneof_Bytes_Kind =>
              Oneof_Bytes : League.Stream_Element_Vectors
                .Stream_Element_Vector;
           when Oneof_Bool_Kind =>
              Oneof_Bool : Boolean := False;
           when Oneof_Uint_64_Kind =>
              Oneof_Uint_64 : Interfaces.Unsigned_64 := 0;
           when Oneof_Float_Kind =>
              Oneof_Float : Interfaces.IEEE_Float_32 := 0.0;
           when Oneof_Double_Kind =>
              Oneof_Double : Interfaces.IEEE_Float_64 := 0.0;
           when Oneof_Enum_Kind =>
              Oneof_Enum : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Nested_Enum :=
                Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.FOO;
        end case;
     end record;

   type Test_All_Types_Proto_2 is
     record
        Optional_Int_32            : PB_Support.Integer_32_Vectors.Option;
        Optional_Int_64            : PB_Support.Integer_64_Vectors.Option;
        Optional_Uint_32           : PB_Support.Unsigned_32_Vectors.Option;
        Optional_Uint_64           : PB_Support.Unsigned_64_Vectors.Option;
        Optional_Sint_32           : PB_Support.Integer_32_Vectors.Option;
        Optional_Sint_64           : PB_Support.Integer_64_Vectors.Option;
        Optional_Fixed_32          : PB_Support.Unsigned_32_Vectors.Option;
        Optional_Fixed_64          : PB_Support.Unsigned_64_Vectors.Option;
        Optional_Sfixed_32         : PB_Support.Integer_32_Vectors.Option;
        Optional_Sfixed_64         : PB_Support.Integer_64_Vectors.Option;
        Optional_Float             : PB_Support.IEEE_Float_32_Vectors.Option;
        Optional_Double            : PB_Support.IEEE_Float_64_Vectors.Option;
        Optional_Bool              : PB_Support.Boolean_Vectors.Option;
        Optional_String            : PB_Support.Universal_String_Vectors
          .Option;
        Optional_Bytes             : PB_Support.Stream_Element_Vector_Vectors
          .Option;
        Optional_Nested_Message    : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Optional_Nested_Message;
        Optional_Foreign_Message   : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Optional_Foreign_Message_Proto_2;
        Optional_Nested_Enum       : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Nested_Enum_Vectors.Option;
        Optional_Foreign_Enum      : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Foreign_Enum_Proto_2_Vectors.Option;
        Optional_String_Piece      : PB_Support.Universal_String_Vectors
          .Option;
        Optional_Cord              : PB_Support.Universal_String_Vectors
          .Option;
        Recursive_Message          : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Test_All_Types_Proto_2_Vector;
        Repeated_Int_32            : PB_Support.Integer_32_Vectors.Vector;
        Repeated_Int_64            : PB_Support.Integer_64_Vectors.Vector;
        Repeated_Uint_32           : PB_Support.Unsigned_32_Vectors.Vector;
        Repeated_Uint_64           : PB_Support.Unsigned_64_Vectors.Vector;
        Repeated_Sint_32           : PB_Support.Integer_32_Vectors.Vector;
        Repeated_Sint_64           : PB_Support.Integer_64_Vectors.Vector;
        Repeated_Fixed_32          : PB_Support.Unsigned_32_Vectors.Vector;
        Repeated_Fixed_64          : PB_Support.Unsigned_64_Vectors.Vector;
        Repeated_Sfixed_32         : PB_Support.Integer_32_Vectors.Vector;
        Repeated_Sfixed_64         : PB_Support.Integer_64_Vectors.Vector;
        Repeated_Float             : PB_Support.IEEE_Float_32_Vectors.Vector;
        Repeated_Double            : PB_Support.IEEE_Float_64_Vectors.Vector;
        Repeated_Bool              : PB_Support.Boolean_Vectors.Vector;
        Repeated_String            : League.String_Vectors
          .Universal_String_Vector;
        Repeated_Bytes             : PB_Support.Stream_Element_Vector_Vectors
          .Vector;
        Repeated_Nested_Message    : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Nested_Message_Vector;
        Repeated_Foreign_Message   : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Foreign_Message_Proto_2_Vector;
        Repeated_Nested_Enum       : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Nested_Enum_Vectors.Vector;
        Repeated_Foreign_Enum      : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Foreign_Enum_Proto_2_Vectors.Vector;
        Repeated_String_Piece      : League.String_Vectors
          .Universal_String_Vector;
        Repeated_Cord              : League.String_Vectors
          .Universal_String_Vector;
        Packed_Int_32              : PB_Support.Integer_32_Vectors.Vector;
        Packed_Int_64              : PB_Support.Integer_64_Vectors.Vector;
        Packed_Uint_32             : PB_Support.Unsigned_32_Vectors.Vector;
        Packed_Uint_64             : PB_Support.Unsigned_64_Vectors.Vector;
        Packed_Sint_32             : PB_Support.Integer_32_Vectors.Vector;
        Packed_Sint_64             : PB_Support.Integer_64_Vectors.Vector;
        Packed_Fixed_32            : PB_Support.Unsigned_32_Vectors.Vector;
        Packed_Fixed_64            : PB_Support.Unsigned_64_Vectors.Vector;
        Packed_Sfixed_32           : PB_Support.Integer_32_Vectors.Vector;
        Packed_Sfixed_64           : PB_Support.Integer_64_Vectors.Vector;
        Packed_Float               : PB_Support.IEEE_Float_32_Vectors.Vector;
        Packed_Double              : PB_Support.IEEE_Float_64_Vectors.Vector;
        Packed_Bool                : PB_Support.Boolean_Vectors.Vector;
        Packed_Nested_Enum         : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Nested_Enum_Vectors.Vector;
        Unpacked_Int_32            : PB_Support.Integer_32_Vectors.Vector;
        Unpacked_Int_64            : PB_Support.Integer_64_Vectors.Vector;
        Unpacked_Uint_32           : PB_Support.Unsigned_32_Vectors.Vector;
        Unpacked_Uint_64           : PB_Support.Unsigned_64_Vectors.Vector;
        Unpacked_Sint_32           : PB_Support.Integer_32_Vectors.Vector;
        Unpacked_Sint_64           : PB_Support.Integer_64_Vectors.Vector;
        Unpacked_Fixed_32          : PB_Support.Unsigned_32_Vectors.Vector;
        Unpacked_Fixed_64          : PB_Support.Unsigned_64_Vectors.Vector;
        Unpacked_Sfixed_32         : PB_Support.Integer_32_Vectors.Vector;
        Unpacked_Sfixed_64         : PB_Support.Integer_64_Vectors.Vector;
        Unpacked_Float             : PB_Support.IEEE_Float_32_Vectors.Vector;
        Unpacked_Double            : PB_Support.IEEE_Float_64_Vectors.Vector;
        Unpacked_Bool              : PB_Support.Boolean_Vectors.Vector;
        Unpacked_Nested_Enum       : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Nested_Enum_Vectors.Vector;
        Map_Int_32_Int_32          : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Map_Int_32Int_32Entry_Vector;
        Map_Int_64_Int_64          : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Map_Int_64Int_64Entry_Vector;
        Map_Uint_32_Uint_32        : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Map_Uint_32Uint_32Entry_Vector;
        Map_Uint_64_Uint_64        : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Map_Uint_64Uint_64Entry_Vector;
        Map_Sint_32_Sint_32        : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Map_Sint_32Sint_32Entry_Vector;
        Map_Sint_64_Sint_64        : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Map_Sint_64Sint_64Entry_Vector;
        Map_Fixed_32_Fixed_32      : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Map_Fixed_32Fixed_32Entry_Vector;
        Map_Fixed_64_Fixed_64      : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Map_Fixed_64Fixed_64Entry_Vector;
        Map_Sfixed_32_Sfixed_32    : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Map_Sfixed_32Sfixed_32Entry_Vector;
        Map_Sfixed_64_Sfixed_64    : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Map_Sfixed_64Sfixed_64Entry_Vector;
        Map_Int_32_Float           : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Map_Int_32Float_Entry_Vector;
        Map_Int_32_Double          : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Map_Int_32Double_Entry_Vector;
        Map_Bool_Bool              : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Map_Bool_Bool_Entry_Vector;
        Map_String_String          : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Map_String_String_Entry_Vector;
        Map_String_Bytes           : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Map_String_Bytes_Entry_Vector;
        Map_String_Nested_Message  : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Map_String_Nested_Message_Entry_Vector;
        Map_String_Foreign_Message : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Map_String_Foreign_Message_Entry_Vector;
        Map_String_Nested_Enum     : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Map_String_Nested_Enum_Entry_Vector;
        Map_String_Foreign_Enum    : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Map_String_Foreign_Enum_Entry_Vector;
        Data                       : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Optional_Data;
        Fieldname_1                : PB_Support.Integer_32_Vectors.Option;
        Field_Name_2               : PB_Support.Integer_32_Vectors.Option;
        Field_Name_3               : PB_Support.Integer_32_Vectors.Option;
        Field_Name_4               : PB_Support.Integer_32_Vectors.Option;
        Field_0name_5              : PB_Support.Integer_32_Vectors.Option;
        Field_0_Name_6             : PB_Support.Integer_32_Vectors.Option;
        Field_Name_7               : PB_Support.Integer_32_Vectors.Option;
        Field_Name_8               : PB_Support.Integer_32_Vectors.Option;
        Field_Name_9               : PB_Support.Integer_32_Vectors.Option;
        Field_Name_10              : PB_Support.Integer_32_Vectors.Option;
        FIELD_NAME11               : PB_Support.Integer_32_Vectors.Option;
        FIELD_Name_12              : PB_Support.Integer_32_Vectors.Option;
        Field_Name_13              : PB_Support.Integer_32_Vectors.Option;
        Field_Name_14              : PB_Support.Integer_32_Vectors.Option;
        Field_Name_15              : PB_Support.Integer_32_Vectors.Option;
        Field_Name_16              : PB_Support.Integer_32_Vectors.Option;
        Field_Name_17              : PB_Support.Integer_32_Vectors.Option;
        Field_Name_18              : PB_Support.Integer_32_Vectors.Option;
        Variant                    : Test_All_Types_Proto_2_Variant;
     end record;

   type Optional_Test_All_Types_Proto_2  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Test_All_Types_Proto_2;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Test_All_Types_Proto_2_Vector) return Natural;

   procedure Clear (Self : in out Test_All_Types_Proto_2_Vector);

   procedure Append
    (Self : in out Test_All_Types_Proto_2_Vector;
     V    : Test_All_Types_Proto_2);

   type Test_All_Types_Proto_2_Variable_Reference
     (Element : not null access Test_All_Types_Proto_2) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Test_All_Types_Proto_2_Variable_Reference
    (Self  : aliased in out Test_All_Types_Proto_2_Vector;
     Index : Positive)
      return Test_All_Types_Proto_2_Variable_Reference
     with Inline;

   type Test_All_Types_Proto_2_Constant_Reference
     (Element : not null access constant Test_All_Types_Proto_2) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Test_All_Types_Proto_2_Constant_Reference
    (Self  : aliased Test_All_Types_Proto_2_Vector;
     Index : Positive)
      return Test_All_Types_Proto_2_Constant_Reference
     with Inline;
private

   procedure Read_Nested_Message
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Nested_Message);

   procedure Write_Nested_Message
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Nested_Message);

   for Nested_Message'Read use Read_Nested_Message;

   for Nested_Message'Write use Write_Nested_Message;

   type Nested_Message_Array is
     array (Positive range <>) of aliased Nested_Message;

   type Nested_Message_Array_Access is access Nested_Message_Array;

   type Nested_Message_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Nested_Message_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Nested_Message_Vector);

   overriding procedure Finalize (Self : in out Nested_Message_Vector);

   procedure Read_Map_Int_32Int_32Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Int_32Int_32Entry);

   procedure Write_Map_Int_32Int_32Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Int_32Int_32Entry);

   for Map_Int_32Int_32Entry'Read use Read_Map_Int_32Int_32Entry;

   for Map_Int_32Int_32Entry'Write use Write_Map_Int_32Int_32Entry;

   type Map_Int_32Int_32Entry_Array is
     array (Positive range <>) of aliased Map_Int_32Int_32Entry;

   type Map_Int_32Int_32Entry_Array_Access is
     access Map_Int_32Int_32Entry_Array;

   type Map_Int_32Int_32Entry_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Map_Int_32Int_32Entry_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Map_Int_32Int_32Entry_Vector);

   overriding procedure Finalize (Self : in out Map_Int_32Int_32Entry_Vector);

   procedure Read_Map_Int_64Int_64Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Int_64Int_64Entry);

   procedure Write_Map_Int_64Int_64Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Int_64Int_64Entry);

   for Map_Int_64Int_64Entry'Read use Read_Map_Int_64Int_64Entry;

   for Map_Int_64Int_64Entry'Write use Write_Map_Int_64Int_64Entry;

   type Map_Int_64Int_64Entry_Array is
     array (Positive range <>) of aliased Map_Int_64Int_64Entry;

   type Map_Int_64Int_64Entry_Array_Access is
     access Map_Int_64Int_64Entry_Array;

   type Map_Int_64Int_64Entry_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Map_Int_64Int_64Entry_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Map_Int_64Int_64Entry_Vector);

   overriding procedure Finalize (Self : in out Map_Int_64Int_64Entry_Vector);

   procedure Read_Map_Uint_32Uint_32Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Uint_32Uint_32Entry);

   procedure Write_Map_Uint_32Uint_32Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Uint_32Uint_32Entry);

   for Map_Uint_32Uint_32Entry'Read use Read_Map_Uint_32Uint_32Entry;

   for Map_Uint_32Uint_32Entry'Write use Write_Map_Uint_32Uint_32Entry;

   type Map_Uint_32Uint_32Entry_Array is
     array (Positive range <>) of aliased Map_Uint_32Uint_32Entry;

   type Map_Uint_32Uint_32Entry_Array_Access is
     access Map_Uint_32Uint_32Entry_Array;

   type Map_Uint_32Uint_32Entry_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Map_Uint_32Uint_32Entry_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Map_Uint_32Uint_32Entry_Vector);

   overriding procedure Finalize
    (Self : in out Map_Uint_32Uint_32Entry_Vector);

   procedure Read_Map_Uint_64Uint_64Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Uint_64Uint_64Entry);

   procedure Write_Map_Uint_64Uint_64Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Uint_64Uint_64Entry);

   for Map_Uint_64Uint_64Entry'Read use Read_Map_Uint_64Uint_64Entry;

   for Map_Uint_64Uint_64Entry'Write use Write_Map_Uint_64Uint_64Entry;

   type Map_Uint_64Uint_64Entry_Array is
     array (Positive range <>) of aliased Map_Uint_64Uint_64Entry;

   type Map_Uint_64Uint_64Entry_Array_Access is
     access Map_Uint_64Uint_64Entry_Array;

   type Map_Uint_64Uint_64Entry_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Map_Uint_64Uint_64Entry_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Map_Uint_64Uint_64Entry_Vector);

   overriding procedure Finalize
    (Self : in out Map_Uint_64Uint_64Entry_Vector);

   procedure Read_Map_Sint_32Sint_32Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Sint_32Sint_32Entry);

   procedure Write_Map_Sint_32Sint_32Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Sint_32Sint_32Entry);

   for Map_Sint_32Sint_32Entry'Read use Read_Map_Sint_32Sint_32Entry;

   for Map_Sint_32Sint_32Entry'Write use Write_Map_Sint_32Sint_32Entry;

   type Map_Sint_32Sint_32Entry_Array is
     array (Positive range <>) of aliased Map_Sint_32Sint_32Entry;

   type Map_Sint_32Sint_32Entry_Array_Access is
     access Map_Sint_32Sint_32Entry_Array;

   type Map_Sint_32Sint_32Entry_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Map_Sint_32Sint_32Entry_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Map_Sint_32Sint_32Entry_Vector);

   overriding procedure Finalize
    (Self : in out Map_Sint_32Sint_32Entry_Vector);

   procedure Read_Map_Sint_64Sint_64Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Sint_64Sint_64Entry);

   procedure Write_Map_Sint_64Sint_64Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Sint_64Sint_64Entry);

   for Map_Sint_64Sint_64Entry'Read use Read_Map_Sint_64Sint_64Entry;

   for Map_Sint_64Sint_64Entry'Write use Write_Map_Sint_64Sint_64Entry;

   type Map_Sint_64Sint_64Entry_Array is
     array (Positive range <>) of aliased Map_Sint_64Sint_64Entry;

   type Map_Sint_64Sint_64Entry_Array_Access is
     access Map_Sint_64Sint_64Entry_Array;

   type Map_Sint_64Sint_64Entry_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Map_Sint_64Sint_64Entry_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Map_Sint_64Sint_64Entry_Vector);

   overriding procedure Finalize
    (Self : in out Map_Sint_64Sint_64Entry_Vector);

   procedure Read_Map_Fixed_32Fixed_32Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Fixed_32Fixed_32Entry);

   procedure Write_Map_Fixed_32Fixed_32Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Fixed_32Fixed_32Entry);

   for Map_Fixed_32Fixed_32Entry'Read use Read_Map_Fixed_32Fixed_32Entry;

   for Map_Fixed_32Fixed_32Entry'Write use Write_Map_Fixed_32Fixed_32Entry;

   type Map_Fixed_32Fixed_32Entry_Array is
     array (Positive range <>) of aliased Map_Fixed_32Fixed_32Entry;

   type Map_Fixed_32Fixed_32Entry_Array_Access is
     access Map_Fixed_32Fixed_32Entry_Array;

   type Map_Fixed_32Fixed_32Entry_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Map_Fixed_32Fixed_32Entry_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust
    (Self : in out Map_Fixed_32Fixed_32Entry_Vector);

   overriding procedure Finalize
    (Self : in out Map_Fixed_32Fixed_32Entry_Vector);

   procedure Read_Map_Fixed_64Fixed_64Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Fixed_64Fixed_64Entry);

   procedure Write_Map_Fixed_64Fixed_64Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Fixed_64Fixed_64Entry);

   for Map_Fixed_64Fixed_64Entry'Read use Read_Map_Fixed_64Fixed_64Entry;

   for Map_Fixed_64Fixed_64Entry'Write use Write_Map_Fixed_64Fixed_64Entry;

   type Map_Fixed_64Fixed_64Entry_Array is
     array (Positive range <>) of aliased Map_Fixed_64Fixed_64Entry;

   type Map_Fixed_64Fixed_64Entry_Array_Access is
     access Map_Fixed_64Fixed_64Entry_Array;

   type Map_Fixed_64Fixed_64Entry_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Map_Fixed_64Fixed_64Entry_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust
    (Self : in out Map_Fixed_64Fixed_64Entry_Vector);

   overriding procedure Finalize
    (Self : in out Map_Fixed_64Fixed_64Entry_Vector);

   procedure Read_Map_Sfixed_32Sfixed_32Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Sfixed_32Sfixed_32Entry);

   procedure Write_Map_Sfixed_32Sfixed_32Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Sfixed_32Sfixed_32Entry);

   for Map_Sfixed_32Sfixed_32Entry'Read use Read_Map_Sfixed_32Sfixed_32Entry;

   for Map_Sfixed_32Sfixed_32Entry'Write use Write_Map_Sfixed_32Sfixed_32Entry;

   type Map_Sfixed_32Sfixed_32Entry_Array is
     array (Positive range <>) of aliased Map_Sfixed_32Sfixed_32Entry;

   type Map_Sfixed_32Sfixed_32Entry_Array_Access is
     access Map_Sfixed_32Sfixed_32Entry_Array;

   type Map_Sfixed_32Sfixed_32Entry_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Map_Sfixed_32Sfixed_32Entry_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust
    (Self : in out Map_Sfixed_32Sfixed_32Entry_Vector);

   overriding procedure Finalize
    (Self : in out Map_Sfixed_32Sfixed_32Entry_Vector);

   procedure Read_Map_Sfixed_64Sfixed_64Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Sfixed_64Sfixed_64Entry);

   procedure Write_Map_Sfixed_64Sfixed_64Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Sfixed_64Sfixed_64Entry);

   for Map_Sfixed_64Sfixed_64Entry'Read use Read_Map_Sfixed_64Sfixed_64Entry;

   for Map_Sfixed_64Sfixed_64Entry'Write use Write_Map_Sfixed_64Sfixed_64Entry;

   type Map_Sfixed_64Sfixed_64Entry_Array is
     array (Positive range <>) of aliased Map_Sfixed_64Sfixed_64Entry;

   type Map_Sfixed_64Sfixed_64Entry_Array_Access is
     access Map_Sfixed_64Sfixed_64Entry_Array;

   type Map_Sfixed_64Sfixed_64Entry_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Map_Sfixed_64Sfixed_64Entry_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust
    (Self : in out Map_Sfixed_64Sfixed_64Entry_Vector);

   overriding procedure Finalize
    (Self : in out Map_Sfixed_64Sfixed_64Entry_Vector);

   procedure Read_Map_Int_32Float_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Int_32Float_Entry);

   procedure Write_Map_Int_32Float_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Int_32Float_Entry);

   for Map_Int_32Float_Entry'Read use Read_Map_Int_32Float_Entry;

   for Map_Int_32Float_Entry'Write use Write_Map_Int_32Float_Entry;

   type Map_Int_32Float_Entry_Array is
     array (Positive range <>) of aliased Map_Int_32Float_Entry;

   type Map_Int_32Float_Entry_Array_Access is
     access Map_Int_32Float_Entry_Array;

   type Map_Int_32Float_Entry_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Map_Int_32Float_Entry_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Map_Int_32Float_Entry_Vector);

   overriding procedure Finalize (Self : in out Map_Int_32Float_Entry_Vector);

   procedure Read_Map_Int_32Double_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Int_32Double_Entry);

   procedure Write_Map_Int_32Double_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Int_32Double_Entry);

   for Map_Int_32Double_Entry'Read use Read_Map_Int_32Double_Entry;

   for Map_Int_32Double_Entry'Write use Write_Map_Int_32Double_Entry;

   type Map_Int_32Double_Entry_Array is
     array (Positive range <>) of aliased Map_Int_32Double_Entry;

   type Map_Int_32Double_Entry_Array_Access is
     access Map_Int_32Double_Entry_Array;

   type Map_Int_32Double_Entry_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Map_Int_32Double_Entry_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Map_Int_32Double_Entry_Vector);

   overriding procedure Finalize (Self : in out Map_Int_32Double_Entry_Vector);

   procedure Read_Map_Bool_Bool_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Bool_Bool_Entry);

   procedure Write_Map_Bool_Bool_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Bool_Bool_Entry);

   for Map_Bool_Bool_Entry'Read use Read_Map_Bool_Bool_Entry;

   for Map_Bool_Bool_Entry'Write use Write_Map_Bool_Bool_Entry;

   type Map_Bool_Bool_Entry_Array is
     array (Positive range <>) of aliased Map_Bool_Bool_Entry;

   type Map_Bool_Bool_Entry_Array_Access is access Map_Bool_Bool_Entry_Array;

   type Map_Bool_Bool_Entry_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Map_Bool_Bool_Entry_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Map_Bool_Bool_Entry_Vector);

   overriding procedure Finalize (Self : in out Map_Bool_Bool_Entry_Vector);

   procedure Read_Map_String_String_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_String_String_Entry);

   procedure Write_Map_String_String_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_String_String_Entry);

   for Map_String_String_Entry'Read use Read_Map_String_String_Entry;

   for Map_String_String_Entry'Write use Write_Map_String_String_Entry;

   type Map_String_String_Entry_Array is
     array (Positive range <>) of aliased Map_String_String_Entry;

   type Map_String_String_Entry_Array_Access is
     access Map_String_String_Entry_Array;

   type Map_String_String_Entry_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Map_String_String_Entry_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Map_String_String_Entry_Vector);

   overriding procedure Finalize
    (Self : in out Map_String_String_Entry_Vector);

   procedure Read_Map_String_Bytes_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_String_Bytes_Entry);

   procedure Write_Map_String_Bytes_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_String_Bytes_Entry);

   for Map_String_Bytes_Entry'Read use Read_Map_String_Bytes_Entry;

   for Map_String_Bytes_Entry'Write use Write_Map_String_Bytes_Entry;

   type Map_String_Bytes_Entry_Array is
     array (Positive range <>) of aliased Map_String_Bytes_Entry;

   type Map_String_Bytes_Entry_Array_Access is
     access Map_String_Bytes_Entry_Array;

   type Map_String_Bytes_Entry_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Map_String_Bytes_Entry_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Map_String_Bytes_Entry_Vector);

   overriding procedure Finalize (Self : in out Map_String_Bytes_Entry_Vector);

   procedure Read_Map_String_Nested_Message_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_String_Nested_Message_Entry);

   procedure Write_Map_String_Nested_Message_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_String_Nested_Message_Entry);

   for Map_String_Nested_Message_Entry'Read use Read_Map_String_Nested_Message_Entry;

   for Map_String_Nested_Message_Entry'Write use Write_Map_String_Nested_Message_Entry;

   type Map_String_Nested_Message_Entry_Array is
     array (Positive range <>) of aliased Map_String_Nested_Message_Entry;

   type Map_String_Nested_Message_Entry_Array_Access is
     access Map_String_Nested_Message_Entry_Array;

   type Map_String_Nested_Message_Entry_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Map_String_Nested_Message_Entry_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust
    (Self : in out Map_String_Nested_Message_Entry_Vector);

   overriding procedure Finalize
    (Self : in out Map_String_Nested_Message_Entry_Vector);

   procedure Read_Map_String_Foreign_Message_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_String_Foreign_Message_Entry);

   procedure Write_Map_String_Foreign_Message_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_String_Foreign_Message_Entry);

   for Map_String_Foreign_Message_Entry'Read use Read_Map_String_Foreign_Message_Entry;

   for Map_String_Foreign_Message_Entry'Write use Write_Map_String_Foreign_Message_Entry;

   type Map_String_Foreign_Message_Entry_Array is
     array (Positive range <>) of aliased Map_String_Foreign_Message_Entry;

   type Map_String_Foreign_Message_Entry_Array_Access is
     access Map_String_Foreign_Message_Entry_Array;

   type Map_String_Foreign_Message_Entry_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Map_String_Foreign_Message_Entry_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust
    (Self : in out Map_String_Foreign_Message_Entry_Vector);

   overriding procedure Finalize
    (Self : in out Map_String_Foreign_Message_Entry_Vector);

   procedure Read_Map_String_Nested_Enum_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_String_Nested_Enum_Entry);

   procedure Write_Map_String_Nested_Enum_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_String_Nested_Enum_Entry);

   for Map_String_Nested_Enum_Entry'Read use Read_Map_String_Nested_Enum_Entry;

   for Map_String_Nested_Enum_Entry'Write use Write_Map_String_Nested_Enum_Entry;

   type Map_String_Nested_Enum_Entry_Array is
     array (Positive range <>) of aliased Map_String_Nested_Enum_Entry;

   type Map_String_Nested_Enum_Entry_Array_Access is
     access Map_String_Nested_Enum_Entry_Array;

   type Map_String_Nested_Enum_Entry_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Map_String_Nested_Enum_Entry_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust
    (Self : in out Map_String_Nested_Enum_Entry_Vector);

   overriding procedure Finalize
    (Self : in out Map_String_Nested_Enum_Entry_Vector);

   procedure Read_Map_String_Foreign_Enum_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_String_Foreign_Enum_Entry);

   procedure Write_Map_String_Foreign_Enum_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_String_Foreign_Enum_Entry);

   for Map_String_Foreign_Enum_Entry'Read use Read_Map_String_Foreign_Enum_Entry;

   for Map_String_Foreign_Enum_Entry'Write use Write_Map_String_Foreign_Enum_Entry;

   type Map_String_Foreign_Enum_Entry_Array is
     array (Positive range <>) of aliased Map_String_Foreign_Enum_Entry;

   type Map_String_Foreign_Enum_Entry_Array_Access is
     access Map_String_Foreign_Enum_Entry_Array;

   type Map_String_Foreign_Enum_Entry_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Map_String_Foreign_Enum_Entry_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust
    (Self : in out Map_String_Foreign_Enum_Entry_Vector);

   overriding procedure Finalize
    (Self : in out Map_String_Foreign_Enum_Entry_Vector);

   procedure Read_Data
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Data);

   procedure Write_Data
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Data);

   for Data'Read use Read_Data;

   for Data'Write use Write_Data;

   type Data_Array is array (Positive range <>) of aliased Data;

   type Data_Array_Access is access Data_Array;

   type Data_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Data_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Data_Vector);

   overriding procedure Finalize (Self : in out Data_Vector);

   procedure Read_Message_Set_Correct
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Message_Set_Correct);

   procedure Write_Message_Set_Correct
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Message_Set_Correct);

   for Message_Set_Correct'Read use Read_Message_Set_Correct;

   for Message_Set_Correct'Write use Write_Message_Set_Correct;

   type Message_Set_Correct_Array is
     array (Positive range <>) of aliased Message_Set_Correct;

   type Message_Set_Correct_Array_Access is access Message_Set_Correct_Array;

   type Message_Set_Correct_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Message_Set_Correct_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Message_Set_Correct_Vector);

   overriding procedure Finalize (Self : in out Message_Set_Correct_Vector);

   procedure Read_Message_Set_Correct_Extension_1
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Message_Set_Correct_Extension_1);

   procedure Write_Message_Set_Correct_Extension_1
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Message_Set_Correct_Extension_1);

   for Message_Set_Correct_Extension_1'Read use Read_Message_Set_Correct_Extension_1;

   for Message_Set_Correct_Extension_1'Write use Write_Message_Set_Correct_Extension_1;

   type Message_Set_Correct_Extension_1_Array is
     array (Positive range <>) of aliased Message_Set_Correct_Extension_1;

   type Message_Set_Correct_Extension_1_Array_Access is
     access Message_Set_Correct_Extension_1_Array;

   type Message_Set_Correct_Extension_1_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Message_Set_Correct_Extension_1_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust
    (Self : in out Message_Set_Correct_Extension_1_Vector);

   overriding procedure Finalize
    (Self : in out Message_Set_Correct_Extension_1_Vector);

   procedure Read_Message_Set_Correct_Extension_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Message_Set_Correct_Extension_2);

   procedure Write_Message_Set_Correct_Extension_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Message_Set_Correct_Extension_2);

   for Message_Set_Correct_Extension_2'Read use Read_Message_Set_Correct_Extension_2;

   for Message_Set_Correct_Extension_2'Write use Write_Message_Set_Correct_Extension_2;

   type Message_Set_Correct_Extension_2_Array is
     array (Positive range <>) of aliased Message_Set_Correct_Extension_2;

   type Message_Set_Correct_Extension_2_Array_Access is
     access Message_Set_Correct_Extension_2_Array;

   type Message_Set_Correct_Extension_2_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Message_Set_Correct_Extension_2_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust
    (Self : in out Message_Set_Correct_Extension_2_Vector);

   overriding procedure Finalize
    (Self : in out Message_Set_Correct_Extension_2_Vector);

   procedure Read_Test_All_Types_Proto_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Test_All_Types_Proto_2);

   procedure Write_Test_All_Types_Proto_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Test_All_Types_Proto_2);

   for Test_All_Types_Proto_2'Read use Read_Test_All_Types_Proto_2;

   for Test_All_Types_Proto_2'Write use Write_Test_All_Types_Proto_2;

   type Test_All_Types_Proto_2_Array is
     array (Positive range <>) of aliased Test_All_Types_Proto_2;

   type Test_All_Types_Proto_2_Array_Access is
     access Test_All_Types_Proto_2_Array;

   type Test_All_Types_Proto_2_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Test_All_Types_Proto_2_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Test_All_Types_Proto_2_Vector);

   overriding procedure Finalize (Self : in out Test_All_Types_Proto_2_Vector);

   procedure Read_Foreign_Message_Proto_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Foreign_Message_Proto_2);

   procedure Write_Foreign_Message_Proto_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Foreign_Message_Proto_2);

   for Foreign_Message_Proto_2'Read use Read_Foreign_Message_Proto_2;

   for Foreign_Message_Proto_2'Write use Write_Foreign_Message_Proto_2;

   type Foreign_Message_Proto_2_Array is
     array (Positive range <>) of aliased Foreign_Message_Proto_2;

   type Foreign_Message_Proto_2_Array_Access is
     access Foreign_Message_Proto_2_Array;

   type Foreign_Message_Proto_2_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Foreign_Message_Proto_2_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Foreign_Message_Proto_2_Vector);

   overriding procedure Finalize
    (Self : in out Foreign_Message_Proto_2_Vector);

   procedure Read_Optional_Group
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Optional_Group);

   procedure Write_Optional_Group
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Optional_Group);

   for Optional_Group'Read use Read_Optional_Group;

   for Optional_Group'Write use Write_Optional_Group;

   type Optional_Group_Array is
     array (Positive range <>) of aliased Optional_Group;

   type Optional_Group_Array_Access is access Optional_Group_Array;

   type Optional_Group_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Optional_Group_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Optional_Group_Vector);

   overriding procedure Finalize (Self : in out Optional_Group_Vector);

   procedure Read_Unknown_To_Test_All_Types
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Unknown_To_Test_All_Types);

   procedure Write_Unknown_To_Test_All_Types
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Unknown_To_Test_All_Types);

   for Unknown_To_Test_All_Types'Read use Read_Unknown_To_Test_All_Types;

   for Unknown_To_Test_All_Types'Write use Write_Unknown_To_Test_All_Types;

   type Unknown_To_Test_All_Types_Array is
     array (Positive range <>) of aliased Unknown_To_Test_All_Types;

   type Unknown_To_Test_All_Types_Array_Access is
     access Unknown_To_Test_All_Types_Array;

   type Unknown_To_Test_All_Types_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Unknown_To_Test_All_Types_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust
    (Self : in out Unknown_To_Test_All_Types_Vector);

   overriding procedure Finalize
    (Self : in out Unknown_To_Test_All_Types_Vector);

end Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2;