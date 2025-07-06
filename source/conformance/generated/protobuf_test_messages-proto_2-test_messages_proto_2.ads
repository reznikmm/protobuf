with Ada.Finalization;
with Ada.Streams;
with Interfaces;
with League.Stream_Element_Vectors;
with League.String_Vectors;
with League.Strings;
with Proto_Support.Boolean_Vectors;
with Proto_Support.IEEE_Float_32_Vectors;
with Proto_Support.IEEE_Float_64_Vectors;
with Proto_Support.Integer_32_Vectors;
with Proto_Support.Integer_64_Vectors;
with Proto_Support.Stream_Element_Vector_Vectors;
with Proto_Support.Universal_String_Vectors;
with Proto_Support.Unsigned_32_Vectors;
with Proto_Support.Unsigned_64_Vectors;
with Proto_Support.Vectors;

package Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2 is

   type Foreign_Enum_Proto_2 is (FOREIGN_FOO, FOREIGN_BAR, FOREIGN_BAZ);

   for Foreign_Enum_Proto_2 use
     (FOREIGN_FOO => 0, FOREIGN_BAR => 1, FOREIGN_BAZ => 2);

   package Foreign_Enum_Proto_2_Vectors is
     new Proto_Support.Vectors (Foreign_Enum_Proto_2);

   type Nested_Enum is (NEG, FOO, BAR, BAZ);

   for Nested_Enum use (NEG =>  - 1, FOO => 0, BAR => 1, BAZ => 2);

   package Nested_Enum_Vectors is new Proto_Support.Vectors (Nested_Enum);

   type Bool is (kFalse, kTrue);

   for Bool use (kFalse => 0, kTrue  => 1);

   package Bool_Vectors is new Proto_Support.Vectors (Bool);

   type Test_All_Required_Types_Proto_2_Nested_Enum is (NEG, FOO, BAR, BAZ);

   for Test_All_Required_Types_Proto_2_Nested_Enum use
     (NEG =>  - 1, FOO => 0, BAR => 1, BAZ => 2);

   package Test_All_Required_Types_Proto_2_Nested_Enum_Vectors is
     new Proto_Support.Vectors (Test_All_Required_Types_Proto_2_Nested_Enum);

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

   type Map_Int_32Bool_Entry_Vector is tagged private
     with Variable_Indexing => Get_Map_Int_32Bool_Entry_Variable_Reference,
     Constant_Indexing => Get_Map_Int_32Bool_Entry_Constant_Reference;

   type Map_Int_32Float_Entry_Vector is tagged private
     with Variable_Indexing => Get_Map_Int_32Float_Entry_Variable_Reference,
     Constant_Indexing => Get_Map_Int_32Float_Entry_Constant_Reference;

   type Map_Int_32Double_Entry_Vector is tagged private
     with Variable_Indexing => Get_Map_Int_32Double_Entry_Variable_Reference,
     Constant_Indexing => Get_Map_Int_32Double_Entry_Constant_Reference;

   type Map_Int_32Nested_Message_Entry_Vector is tagged private
     with Variable_Indexing =>
       Get_Map_Int_32Nested_Message_Entry_Variable_Reference,
     Constant_Indexing =>
       Get_Map_Int_32Nested_Message_Entry_Constant_Reference;

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

   type Multi_Word_Group_Field_Vector is tagged private
     with Variable_Indexing => Get_Multi_Word_Group_Field_Variable_Reference,
     Constant_Indexing => Get_Multi_Word_Group_Field_Constant_Reference;

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

   type Extension_With_Oneof_Vector is tagged private
     with Variable_Indexing => Get_Extension_With_Oneof_Variable_Reference,
     Constant_Indexing => Get_Extension_With_Oneof_Constant_Reference;

   type Foreign_Message_Proto_2_Vector is tagged private
     with Variable_Indexing => Get_Foreign_Message_Proto_2_Variable_Reference,
     Constant_Indexing => Get_Foreign_Message_Proto_2_Constant_Reference;

   type Group_Field_Vector is tagged private
     with Variable_Indexing => Get_Group_Field_Variable_Reference,
     Constant_Indexing => Get_Group_Field_Constant_Reference;

   type Unknown_To_Test_All_Types_Vector is tagged private
     with Variable_Indexing =>
       Get_Unknown_To_Test_All_Types_Variable_Reference,
     Constant_Indexing => Get_Unknown_To_Test_All_Types_Constant_Reference;

   type Optional_Group_Vector is tagged private
     with Variable_Indexing => Get_Optional_Group_Variable_Reference,
     Constant_Indexing => Get_Optional_Group_Constant_Reference;

   type Null_Hypothesis_Proto_2_Vector is tagged private
     with Variable_Indexing => Get_Null_Hypothesis_Proto_2_Variable_Reference,
     Constant_Indexing => Get_Null_Hypothesis_Proto_2_Constant_Reference;

   type Enum_Only_Proto_2_Vector is tagged private
     with Variable_Indexing => Get_Enum_Only_Proto_2_Variable_Reference,
     Constant_Indexing => Get_Enum_Only_Proto_2_Constant_Reference;

   type One_String_Proto_2_Vector is tagged private
     with Variable_Indexing => Get_One_String_Proto_2_Variable_Reference,
     Constant_Indexing => Get_One_String_Proto_2_Constant_Reference;

   type Proto_With_Keywords_Vector is tagged private
     with Variable_Indexing => Get_Proto_With_Keywords_Variable_Reference,
     Constant_Indexing => Get_Proto_With_Keywords_Constant_Reference;

   type Test_All_Required_Types_Proto_2_Vector is tagged private
     with Variable_Indexing =>
       Get_Test_All_Required_Types_Proto_2_Variable_Reference,
     Constant_Indexing =>
       Get_Test_All_Required_Types_Proto_2_Constant_Reference;

   type Test_All_Required_Types_Proto_2_Nested_Message_Vector is tagged private
     with Variable_Indexing =>
       Get_Test_All_Required_Types_Proto_2_Nested_Message_Variable_Reference,
     Constant_Indexing =>
       Get_Test_All_Required_Types_Proto_2_Nested_Message_Constant_Reference;

   type Test_All_Required_Types_Proto_2_Data_Vector is tagged private
     with Variable_Indexing =>
       Get_Test_All_Required_Types_Proto_2_Data_Variable_Reference,
     Constant_Indexing =>
       Get_Test_All_Required_Types_Proto_2_Data_Constant_Reference;

   type Test_All_Required_Types_Proto_2_Message_Set_Correct_Vector is
     tagged private
     with Variable_Indexing =>
       Get_Test_All_Required_Types_Proto_2_Message_Set_Correct_Variable_Reference,
     Constant_Indexing =>
       Get_Test_All_Required_Types_Proto_2_Message_Set_Correct_Constant_Reference;

   type Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Vector is
     tagged private
     with Variable_Indexing =>
       Get_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Variable_Reference,
     Constant_Indexing =>
       Get_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Constant_Reference;

   type Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Vector is
     tagged private
     with Variable_Indexing =>
       Get_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Variable_Reference,
     Constant_Indexing =>
       Get_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Constant_Reference;

   type Test_Large_Oneof_Vector is tagged private
     with Variable_Indexing => Get_Test_Large_Oneof_Variable_Reference,
     Constant_Indexing => Get_Test_Large_Oneof_Constant_Reference;

   type A1_Vector is tagged private
     with Variable_Indexing => Get_A1_Variable_Reference,
     Constant_Indexing => Get_A1_Constant_Reference;

   type A2_Vector is tagged private
     with Variable_Indexing => Get_A2_Variable_Reference,
     Constant_Indexing => Get_A2_Constant_Reference;

   type A3_Vector is tagged private
     with Variable_Indexing => Get_A3_Variable_Reference,
     Constant_Indexing => Get_A3_Constant_Reference;

   type A4_Vector is tagged private
     with Variable_Indexing => Get_A4_Variable_Reference,
     Constant_Indexing => Get_A4_Constant_Reference;

   type A5_Vector is tagged private
     with Variable_Indexing => Get_A5_Variable_Reference,
     Constant_Indexing => Get_A5_Constant_Reference;

   type Map_Int_32Int_32Entry is
     record
        Key   : Proto_Support.Integer_32_Vectors.Option;
        Value : Proto_Support.Integer_32_Vectors.Option;
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
        Key   : Proto_Support.Integer_64_Vectors.Option;
        Value : Proto_Support.Integer_64_Vectors.Option;
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
        Key   : Proto_Support.Unsigned_32_Vectors.Option;
        Value : Proto_Support.Unsigned_32_Vectors.Option;
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
        Key   : Proto_Support.Unsigned_64_Vectors.Option;
        Value : Proto_Support.Unsigned_64_Vectors.Option;
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
        Key   : Proto_Support.Integer_32_Vectors.Option;
        Value : Proto_Support.Integer_32_Vectors.Option;
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
        Key   : Proto_Support.Integer_64_Vectors.Option;
        Value : Proto_Support.Integer_64_Vectors.Option;
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
        Key   : Proto_Support.Unsigned_32_Vectors.Option;
        Value : Proto_Support.Unsigned_32_Vectors.Option;
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
        Key   : Proto_Support.Unsigned_64_Vectors.Option;
        Value : Proto_Support.Unsigned_64_Vectors.Option;
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
        Key   : Proto_Support.Integer_32_Vectors.Option;
        Value : Proto_Support.Integer_32_Vectors.Option;
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
        Key   : Proto_Support.Integer_64_Vectors.Option;
        Value : Proto_Support.Integer_64_Vectors.Option;
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

   type Map_Int_32Bool_Entry is
     record
        Key   : Proto_Support.Integer_32_Vectors.Option;
        Value : Proto_Support.Boolean_Vectors.Option;
     end record;

   type Optional_Map_Int_32Bool_Entry  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Map_Int_32Bool_Entry;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Map_Int_32Bool_Entry_Vector) return Natural;

   procedure Clear (Self : in out Map_Int_32Bool_Entry_Vector);

   procedure Append
    (Self : in out Map_Int_32Bool_Entry_Vector;
     V    : Map_Int_32Bool_Entry);

   type Map_Int_32Bool_Entry_Variable_Reference
     (Element : not null access Map_Int_32Bool_Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Int_32Bool_Entry_Variable_Reference
    (Self  : aliased in out Map_Int_32Bool_Entry_Vector;
     Index : Positive)
      return Map_Int_32Bool_Entry_Variable_Reference
     with Inline;

   type Map_Int_32Bool_Entry_Constant_Reference
     (Element : not null access constant Map_Int_32Bool_Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Int_32Bool_Entry_Constant_Reference
    (Self  : aliased Map_Int_32Bool_Entry_Vector;
     Index : Positive)
      return Map_Int_32Bool_Entry_Constant_Reference
     with Inline;

   type Map_Int_32Float_Entry is
     record
        Key   : Proto_Support.Integer_32_Vectors.Option;
        Value : Proto_Support.IEEE_Float_32_Vectors.Option;
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
        Key   : Proto_Support.Integer_32_Vectors.Option;
        Value : Proto_Support.IEEE_Float_64_Vectors.Option;
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
        Key   : Proto_Support.Boolean_Vectors.Option;
        Value : Proto_Support.Boolean_Vectors.Option;
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
        Key   : Proto_Support.Universal_String_Vectors.Option;
        Value : Proto_Support.Universal_String_Vectors.Option;
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
        Key   : Proto_Support.Universal_String_Vectors.Option;
        Value : Proto_Support.Stream_Element_Vector_Vectors.Option;
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
        Key   : Proto_Support.Universal_String_Vectors.Option;
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
        Key   : Proto_Support.Universal_String_Vectors.Option;
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
        Group_Int_32  : Proto_Support.Integer_32_Vectors.Option;
        Group_Uint_32 : Proto_Support.Unsigned_32_Vectors.Option;
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

   type Multi_Word_Group_Field is
     record
        Group_Int_32  : Proto_Support.Integer_32_Vectors.Option;
        Group_Uint_32 : Proto_Support.Unsigned_32_Vectors.Option;
     end record;

   type Optional_Multi_Word_Group_Field  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Multi_Word_Group_Field;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Multi_Word_Group_Field_Vector) return Natural;

   procedure Clear (Self : in out Multi_Word_Group_Field_Vector);

   procedure Append
    (Self : in out Multi_Word_Group_Field_Vector;
     V    : Multi_Word_Group_Field);

   type Multi_Word_Group_Field_Variable_Reference
     (Element : not null access Multi_Word_Group_Field) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Multi_Word_Group_Field_Variable_Reference
    (Self  : aliased in out Multi_Word_Group_Field_Vector;
     Index : Positive)
      return Multi_Word_Group_Field_Variable_Reference
     with Inline;

   type Multi_Word_Group_Field_Constant_Reference
     (Element : not null access constant Multi_Word_Group_Field) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Multi_Word_Group_Field_Constant_Reference
    (Self  : aliased Multi_Word_Group_Field_Vector;
     Index : Positive)
      return Multi_Word_Group_Field_Constant_Reference
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
        Str : Proto_Support.Universal_String_Vectors.Option;
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
        I : Proto_Support.Integer_32_Vectors.Option;
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

   type Extension_With_Oneof_Variant_Kind is
     (Oneof_Field_Not_Set, A_Kind             , B_Kind             );

   type Extension_With_Oneof_Variant
     (Oneof_Field : Extension_With_Oneof_Variant_Kind := Oneof_Field_Not_Set) is
     record
        case Oneof_Field is
           when Oneof_Field_Not_Set =>
              null;
           when A_Kind =>
              A : Interfaces.Integer_32 := 0;
           when B_Kind =>
              B : Interfaces.Integer_32 := 0;
        end case;
     end record;

   type Extension_With_Oneof is
     record
        Variant : Extension_With_Oneof_Variant;
     end record;

   type Optional_Extension_With_Oneof  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Extension_With_Oneof;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Extension_With_Oneof_Vector) return Natural;

   procedure Clear (Self : in out Extension_With_Oneof_Vector);

   procedure Append
    (Self : in out Extension_With_Oneof_Vector;
     V    : Extension_With_Oneof);

   type Extension_With_Oneof_Variable_Reference
     (Element : not null access Extension_With_Oneof) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Extension_With_Oneof_Variable_Reference
    (Self  : aliased in out Extension_With_Oneof_Vector;
     Index : Positive)
      return Extension_With_Oneof_Variable_Reference
     with Inline;

   type Extension_With_Oneof_Constant_Reference
     (Element : not null access constant Extension_With_Oneof) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Extension_With_Oneof_Constant_Reference
    (Self  : aliased Extension_With_Oneof_Vector;
     Index : Positive)
      return Extension_With_Oneof_Constant_Reference
     with Inline;

   type Foreign_Message_Proto_2 is
     record
        C : Proto_Support.Integer_32_Vectors.Option;
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

   type Group_Field is
     record
        Group_Int_32  : Proto_Support.Integer_32_Vectors.Option;
        Group_Uint_32 : Proto_Support.Unsigned_32_Vectors.Option;
     end record;

   type Optional_Group_Field  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Group_Field;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Group_Field_Vector) return Natural;

   procedure Clear (Self : in out Group_Field_Vector);

   procedure Append (Self : in out Group_Field_Vector; V    : Group_Field);

   type Group_Field_Variable_Reference
     (Element : not null access Group_Field) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Group_Field_Variable_Reference
    (Self  : aliased in out Group_Field_Vector;
     Index : Positive)
      return Group_Field_Variable_Reference
     with Inline;

   type Group_Field_Constant_Reference
     (Element : not null access constant Group_Field) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Group_Field_Constant_Reference
    (Self  : aliased Group_Field_Vector;
     Index : Positive)
      return Group_Field_Constant_Reference
     with Inline;

   type Optional_Group is
     record
        A : Proto_Support.Integer_32_Vectors.Option;
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
        Optional_Int_32 : Proto_Support.Integer_32_Vectors.Option;
        Optional_String : Proto_Support.Universal_String_Vectors.Option;
        Nested_Message  : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Optional_Foreign_Message_Proto_2;
        Optionalgroup   : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Optional_Optional_Group;
        Optional_Bool   : Proto_Support.Boolean_Vectors.Option;
        Repeated_Int_32 : Proto_Support.Integer_32_Vectors.Vector;
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

   type Null_Hypothesis_Proto_2 is null record;

   type Optional_Null_Hypothesis_Proto_2  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Null_Hypothesis_Proto_2;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Null_Hypothesis_Proto_2_Vector) return Natural;

   procedure Clear (Self : in out Null_Hypothesis_Proto_2_Vector);

   procedure Append
    (Self : in out Null_Hypothesis_Proto_2_Vector;
     V    : Null_Hypothesis_Proto_2);

   type Null_Hypothesis_Proto_2_Variable_Reference
     (Element : not null access Null_Hypothesis_Proto_2) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Null_Hypothesis_Proto_2_Variable_Reference
    (Self  : aliased in out Null_Hypothesis_Proto_2_Vector;
     Index : Positive)
      return Null_Hypothesis_Proto_2_Variable_Reference
     with Inline;

   type Null_Hypothesis_Proto_2_Constant_Reference
     (Element : not null access constant Null_Hypothesis_Proto_2) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Null_Hypothesis_Proto_2_Constant_Reference
    (Self  : aliased Null_Hypothesis_Proto_2_Vector;
     Index : Positive)
      return Null_Hypothesis_Proto_2_Constant_Reference
     with Inline;

   type Enum_Only_Proto_2 is null record;

   type Optional_Enum_Only_Proto_2  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Enum_Only_Proto_2;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Enum_Only_Proto_2_Vector) return Natural;

   procedure Clear (Self : in out Enum_Only_Proto_2_Vector);

   procedure Append
    (Self : in out Enum_Only_Proto_2_Vector;
     V    : Enum_Only_Proto_2);

   type Enum_Only_Proto_2_Variable_Reference
     (Element : not null access Enum_Only_Proto_2) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Enum_Only_Proto_2_Variable_Reference
    (Self  : aliased in out Enum_Only_Proto_2_Vector;
     Index : Positive)
      return Enum_Only_Proto_2_Variable_Reference
     with Inline;

   type Enum_Only_Proto_2_Constant_Reference
     (Element : not null access constant Enum_Only_Proto_2) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Enum_Only_Proto_2_Constant_Reference
    (Self  : aliased Enum_Only_Proto_2_Vector;
     Index : Positive)
      return Enum_Only_Proto_2_Constant_Reference
     with Inline;

   type One_String_Proto_2 is
     record
        Data : Proto_Support.Universal_String_Vectors.Option;
     end record;

   type Optional_One_String_Proto_2  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .One_String_Proto_2;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : One_String_Proto_2_Vector) return Natural;

   procedure Clear (Self : in out One_String_Proto_2_Vector);

   procedure Append
    (Self : in out One_String_Proto_2_Vector;
     V    : One_String_Proto_2);

   type One_String_Proto_2_Variable_Reference
     (Element : not null access One_String_Proto_2) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_One_String_Proto_2_Variable_Reference
    (Self  : aliased in out One_String_Proto_2_Vector;
     Index : Positive)
      return One_String_Proto_2_Variable_Reference
     with Inline;

   type One_String_Proto_2_Constant_Reference
     (Element : not null access constant One_String_Proto_2) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_One_String_Proto_2_Constant_Reference
    (Self  : aliased One_String_Proto_2_Vector;
     Index : Positive)
      return One_String_Proto_2_Constant_Reference
     with Inline;

   type Proto_With_Keywords is
     record
        Inline   : Proto_Support.Integer_32_Vectors.Option;
        Concept  : Proto_Support.Universal_String_Vectors.Option;
        Requires : League.String_Vectors.Universal_String_Vector;
     end record;

   type Optional_Proto_With_Keywords  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Proto_With_Keywords;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Proto_With_Keywords_Vector) return Natural;

   procedure Clear (Self : in out Proto_With_Keywords_Vector);

   procedure Append
    (Self : in out Proto_With_Keywords_Vector;
     V    : Proto_With_Keywords);

   type Proto_With_Keywords_Variable_Reference
     (Element : not null access Proto_With_Keywords) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Proto_With_Keywords_Variable_Reference
    (Self  : aliased in out Proto_With_Keywords_Vector;
     Index : Positive)
      return Proto_With_Keywords_Variable_Reference
     with Inline;

   type Proto_With_Keywords_Constant_Reference
     (Element : not null access constant Proto_With_Keywords) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Proto_With_Keywords_Constant_Reference
    (Self  : aliased Proto_With_Keywords_Vector;
     Index : Positive)
      return Proto_With_Keywords_Constant_Reference
     with Inline;

   type Test_All_Required_Types_Proto_2_Data is
     record
        Group_Int_32  : Interfaces.Integer_32 := 0;
        Group_Uint_32 : Interfaces.Unsigned_32 := 0;
     end record;

   type Optional_Test_All_Required_Types_Proto_2_Data
     (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Test_All_Required_Types_Proto_2_Data;
           when False =>
              null;
        end case;
     end record;

   function Length
    (Self : Test_All_Required_Types_Proto_2_Data_Vector)
      return Natural;

   procedure Clear (Self : in out Test_All_Required_Types_Proto_2_Data_Vector);

   procedure Append
    (Self : in out Test_All_Required_Types_Proto_2_Data_Vector;
     V    : Test_All_Required_Types_Proto_2_Data);

   type Test_All_Required_Types_Proto_2_Data_Variable_Reference
     (Element : not null access Test_All_Required_Types_Proto_2_Data) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Test_All_Required_Types_Proto_2_Data_Variable_Reference
    (Self  : aliased in out Test_All_Required_Types_Proto_2_Data_Vector;
     Index : Positive)
      return Test_All_Required_Types_Proto_2_Data_Variable_Reference
     with Inline;

   type Test_All_Required_Types_Proto_2_Data_Constant_Reference
     (Element : not null access constant Test_All_Required_Types_Proto_2_Data) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Test_All_Required_Types_Proto_2_Data_Constant_Reference
    (Self  : aliased Test_All_Required_Types_Proto_2_Data_Vector;
     Index : Positive)
      return Test_All_Required_Types_Proto_2_Data_Constant_Reference
     with Inline;

   type Test_All_Required_Types_Proto_2_Message_Set_Correct is null record;

   type Optional_Test_All_Required_Types_Proto_2_Message_Set_Correct
     (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Test_All_Required_Types_Proto_2_Message_Set_Correct;
           when False =>
              null;
        end case;
     end record;

   function Length
    (Self : Test_All_Required_Types_Proto_2_Message_Set_Correct_Vector)
      return Natural;

   procedure Clear
    (Self : in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Vector);

   procedure Append
    (Self : in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Vector;
     V    : Test_All_Required_Types_Proto_2_Message_Set_Correct);

   type Test_All_Required_Types_Proto_2_Message_Set_Correct_Variable_Reference
     (Element : not null access Test_All_Required_Types_Proto_2_Message_Set_Correct) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Test_All_Required_Types_Proto_2_Message_Set_Correct_Variable_Reference
    (Self  : aliased in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Vector;
     Index : Positive)
      return Test_All_Required_Types_Proto_2_Message_Set_Correct_Variable_Reference
     with Inline;

   type Test_All_Required_Types_Proto_2_Message_Set_Correct_Constant_Reference
     (Element : not null access constant Test_All_Required_Types_Proto_2_Message_Set_Correct) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Test_All_Required_Types_Proto_2_Message_Set_Correct_Constant_Reference
    (Self  : aliased Test_All_Required_Types_Proto_2_Message_Set_Correct_Vector;
     Index : Positive)
      return Test_All_Required_Types_Proto_2_Message_Set_Correct_Constant_Reference
     with Inline;

   type Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1 is
     record
        Str : League.Strings.Universal_String;
     end record;

   type Optional_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1
     (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1;
           when False =>
              null;
        end case;
     end record;

   function Length
    (Self : Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Vector)
      return Natural;

   procedure Clear
    (Self : in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Vector);

   procedure Append
    (Self : in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Vector;
     V    : Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1);

   type Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Variable_Reference
     (Element : not null access Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Variable_Reference
    (Self  : aliased in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Vector;
     Index : Positive)
      return Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Variable_Reference
     with Inline;

   type Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Constant_Reference
     (Element : not null access constant Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Constant_Reference
    (Self  : aliased Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Vector;
     Index : Positive)
      return Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Constant_Reference
     with Inline;

   type Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2 is
     record
        I : Interfaces.Integer_32 := 0;
     end record;

   type Optional_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2
     (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2;
           when False =>
              null;
        end case;
     end record;

   function Length
    (Self : Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Vector)
      return Natural;

   procedure Clear
    (Self : in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Vector);

   procedure Append
    (Self : in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Vector;
     V    : Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2);

   type Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Variable_Reference
     (Element : not null access Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Variable_Reference
    (Self  : aliased in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Vector;
     Index : Positive)
      return Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Variable_Reference
     with Inline;

   type Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Constant_Reference
     (Element : not null access constant Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Constant_Reference
    (Self  : aliased Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Vector;
     Index : Positive)
      return Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Constant_Reference
     with Inline;

   type A1 is null record;

   type Optional_A1  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.A1;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : A1_Vector) return Natural;

   procedure Clear (Self : in out A1_Vector);

   procedure Append (Self : in out A1_Vector; V    : A1);

   type A1_Variable_Reference  (Element : not null access A1) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_A1_Variable_Reference
    (Self  : aliased in out A1_Vector;
     Index : Positive)
      return A1_Variable_Reference
     with Inline;

   type A1_Constant_Reference  (Element : not null access constant A1) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_A1_Constant_Reference
    (Self  : aliased A1_Vector;
     Index : Positive)
      return A1_Constant_Reference
     with Inline;

   type A2 is null record;

   type Optional_A2  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.A2;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : A2_Vector) return Natural;

   procedure Clear (Self : in out A2_Vector);

   procedure Append (Self : in out A2_Vector; V    : A2);

   type A2_Variable_Reference  (Element : not null access A2) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_A2_Variable_Reference
    (Self  : aliased in out A2_Vector;
     Index : Positive)
      return A2_Variable_Reference
     with Inline;

   type A2_Constant_Reference  (Element : not null access constant A2) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_A2_Constant_Reference
    (Self  : aliased A2_Vector;
     Index : Positive)
      return A2_Constant_Reference
     with Inline;

   type A3 is null record;

   type Optional_A3  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.A3;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : A3_Vector) return Natural;

   procedure Clear (Self : in out A3_Vector);

   procedure Append (Self : in out A3_Vector; V    : A3);

   type A3_Variable_Reference  (Element : not null access A3) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_A3_Variable_Reference
    (Self  : aliased in out A3_Vector;
     Index : Positive)
      return A3_Variable_Reference
     with Inline;

   type A3_Constant_Reference  (Element : not null access constant A3) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_A3_Constant_Reference
    (Self  : aliased A3_Vector;
     Index : Positive)
      return A3_Constant_Reference
     with Inline;

   type A4 is null record;

   type Optional_A4  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.A4;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : A4_Vector) return Natural;

   procedure Clear (Self : in out A4_Vector);

   procedure Append (Self : in out A4_Vector; V    : A4);

   type A4_Variable_Reference  (Element : not null access A4) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_A4_Variable_Reference
    (Self  : aliased in out A4_Vector;
     Index : Positive)
      return A4_Variable_Reference
     with Inline;

   type A4_Constant_Reference  (Element : not null access constant A4) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_A4_Constant_Reference
    (Self  : aliased A4_Vector;
     Index : Positive)
      return A4_Constant_Reference
     with Inline;

   type A5 is null record;

   type Optional_A5  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.A5;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : A5_Vector) return Natural;

   procedure Clear (Self : in out A5_Vector);

   procedure Append (Self : in out A5_Vector; V    : A5);

   type A5_Variable_Reference  (Element : not null access A5) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_A5_Variable_Reference
    (Self  : aliased in out A5_Vector;
     Index : Positive)
      return A5_Variable_Reference
     with Inline;

   type A5_Constant_Reference  (Element : not null access constant A5) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_A5_Constant_Reference
    (Self  : aliased A5_Vector;
     Index : Positive)
      return A5_Constant_Reference
     with Inline;

   type Test_Large_Oneof_Variant_Kind is
     (Large_Oneof_Not_Set,
      A1_Kind            ,
      A2_Kind            ,
      A3_Kind            ,
      A4_Kind            ,
      A5_Kind            );

   type Test_Large_Oneof_Variant
     (Large_Oneof : Test_Large_Oneof_Variant_Kind := Large_Oneof_Not_Set) is
     record
        case Large_Oneof is
           when Large_Oneof_Not_Set =>
              null;
           when A1_Kind =>
              A1 : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.A1;
           when A2_Kind =>
              A2 : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.A2;
           when A3_Kind =>
              A3 : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.A3;
           when A4_Kind =>
              A4 : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.A4;
           when A5_Kind =>
              A5 : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.A5;
        end case;
     end record;

   type Test_Large_Oneof is
     record
        Variant : Test_Large_Oneof_Variant;
     end record;

   type Optional_Test_Large_Oneof  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Test_Large_Oneof;
           when False =>
              null;
        end case;
     end record;

   function Length (Self : Test_Large_Oneof_Vector) return Natural;

   procedure Clear (Self : in out Test_Large_Oneof_Vector);

   procedure Append
    (Self : in out Test_Large_Oneof_Vector;
     V    : Test_Large_Oneof);

   type Test_Large_Oneof_Variable_Reference
     (Element : not null access Test_Large_Oneof) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Test_Large_Oneof_Variable_Reference
    (Self  : aliased in out Test_Large_Oneof_Vector;
     Index : Positive)
      return Test_Large_Oneof_Variable_Reference
     with Inline;

   type Test_Large_Oneof_Constant_Reference
     (Element : not null access constant Test_Large_Oneof) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Test_Large_Oneof_Constant_Reference
    (Self  : aliased Test_Large_Oneof_Vector;
     Index : Positive)
      return Test_Large_Oneof_Constant_Reference
     with Inline;

   type Map_String_Foreign_Message_Entry is
     record
        Key   : Proto_Support.Universal_String_Vectors.Option;
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
        A           : Proto_Support.Integer_32_Vectors.Option;
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

   type Map_Int_32Nested_Message_Entry is
     record
        Key   : Proto_Support.Integer_32_Vectors.Option;
        Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
          .Optional_Nested_Message;
     end record;

   type Optional_Map_Int_32Nested_Message_Entry  (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Map_Int_32Nested_Message_Entry;
           when False =>
              null;
        end case;
     end record;

   function Length
    (Self : Map_Int_32Nested_Message_Entry_Vector)
      return Natural;

   procedure Clear (Self : in out Map_Int_32Nested_Message_Entry_Vector);

   procedure Append
    (Self : in out Map_Int_32Nested_Message_Entry_Vector;
     V    : Map_Int_32Nested_Message_Entry);

   type Map_Int_32Nested_Message_Entry_Variable_Reference
     (Element : not null access Map_Int_32Nested_Message_Entry) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Int_32Nested_Message_Entry_Variable_Reference
    (Self  : aliased in out Map_Int_32Nested_Message_Entry_Vector;
     Index : Positive)
      return Map_Int_32Nested_Message_Entry_Variable_Reference
     with Inline;

   type Map_Int_32Nested_Message_Entry_Constant_Reference
     (Element : not null access constant Map_Int_32Nested_Message_Entry) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Map_Int_32Nested_Message_Entry_Constant_Reference
    (Self  : aliased Map_Int_32Nested_Message_Entry_Vector;
     Index : Positive)
      return Map_Int_32Nested_Message_Entry_Constant_Reference
     with Inline;

   type Map_String_Nested_Message_Entry is
     record
        Key   : Proto_Support.Universal_String_Vectors.Option;
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
        Optional_Int_32            : Proto_Support.Integer_32_Vectors.Option;
        Optional_Int_64            : Proto_Support.Integer_64_Vectors.Option;
        Optional_Uint_32           : Proto_Support.Unsigned_32_Vectors.Option;
        Optional_Uint_64           : Proto_Support.Unsigned_64_Vectors.Option;
        Optional_Sint_32           : Proto_Support.Integer_32_Vectors.Option;
        Optional_Sint_64           : Proto_Support.Integer_64_Vectors.Option;
        Optional_Fixed_32          : Proto_Support.Unsigned_32_Vectors.Option;
        Optional_Fixed_64          : Proto_Support.Unsigned_64_Vectors.Option;
        Optional_Sfixed_32         : Proto_Support.Integer_32_Vectors.Option;
        Optional_Sfixed_64         : Proto_Support.Integer_64_Vectors.Option;
        Optional_Float             : Proto_Support.IEEE_Float_32_Vectors
          .Option;
        Optional_Double            : Proto_Support.IEEE_Float_64_Vectors
          .Option;
        Optional_Bool              : Proto_Support.Boolean_Vectors.Option;
        Optional_String            : Proto_Support.Universal_String_Vectors
          .Option;
        Optional_Bytes             : Proto_Support
          .Stream_Element_Vector_Vectors.Option;
        Optional_Nested_Message    : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Optional_Nested_Message;
        Optional_Foreign_Message   : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Optional_Foreign_Message_Proto_2;
        Optional_Nested_Enum       : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Nested_Enum_Vectors.Option;
        Optional_Foreign_Enum      : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Foreign_Enum_Proto_2_Vectors.Option;
        Optional_String_Piece      : Proto_Support.Universal_String_Vectors
          .Option;
        Optional_Cord              : Proto_Support.Universal_String_Vectors
          .Option;
        Recursive_Message          : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Test_All_Types_Proto_2_Vector;
        Repeated_Int_32            : Proto_Support.Integer_32_Vectors.Vector;
        Repeated_Int_64            : Proto_Support.Integer_64_Vectors.Vector;
        Repeated_Uint_32           : Proto_Support.Unsigned_32_Vectors.Vector;
        Repeated_Uint_64           : Proto_Support.Unsigned_64_Vectors.Vector;
        Repeated_Sint_32           : Proto_Support.Integer_32_Vectors.Vector;
        Repeated_Sint_64           : Proto_Support.Integer_64_Vectors.Vector;
        Repeated_Fixed_32          : Proto_Support.Unsigned_32_Vectors.Vector;
        Repeated_Fixed_64          : Proto_Support.Unsigned_64_Vectors.Vector;
        Repeated_Sfixed_32         : Proto_Support.Integer_32_Vectors.Vector;
        Repeated_Sfixed_64         : Proto_Support.Integer_64_Vectors.Vector;
        Repeated_Float             : Proto_Support.IEEE_Float_32_Vectors
          .Vector;
        Repeated_Double            : Proto_Support.IEEE_Float_64_Vectors
          .Vector;
        Repeated_Bool              : Proto_Support.Boolean_Vectors.Vector;
        Repeated_String            : League.String_Vectors
          .Universal_String_Vector;
        Repeated_Bytes             : Proto_Support
          .Stream_Element_Vector_Vectors.Vector;
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
        Packed_Int_32              : Proto_Support.Integer_32_Vectors.Vector;
        Packed_Int_64              : Proto_Support.Integer_64_Vectors.Vector;
        Packed_Uint_32             : Proto_Support.Unsigned_32_Vectors.Vector;
        Packed_Uint_64             : Proto_Support.Unsigned_64_Vectors.Vector;
        Packed_Sint_32             : Proto_Support.Integer_32_Vectors.Vector;
        Packed_Sint_64             : Proto_Support.Integer_64_Vectors.Vector;
        Packed_Fixed_32            : Proto_Support.Unsigned_32_Vectors.Vector;
        Packed_Fixed_64            : Proto_Support.Unsigned_64_Vectors.Vector;
        Packed_Sfixed_32           : Proto_Support.Integer_32_Vectors.Vector;
        Packed_Sfixed_64           : Proto_Support.Integer_64_Vectors.Vector;
        Packed_Float               : Proto_Support.IEEE_Float_32_Vectors
          .Vector;
        Packed_Double              : Proto_Support.IEEE_Float_64_Vectors
          .Vector;
        Packed_Bool                : Proto_Support.Boolean_Vectors.Vector;
        Packed_Nested_Enum         : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Nested_Enum_Vectors.Vector;
        Unpacked_Int_32            : Proto_Support.Integer_32_Vectors.Vector;
        Unpacked_Int_64            : Proto_Support.Integer_64_Vectors.Vector;
        Unpacked_Uint_32           : Proto_Support.Unsigned_32_Vectors.Vector;
        Unpacked_Uint_64           : Proto_Support.Unsigned_64_Vectors.Vector;
        Unpacked_Sint_32           : Proto_Support.Integer_32_Vectors.Vector;
        Unpacked_Sint_64           : Proto_Support.Integer_64_Vectors.Vector;
        Unpacked_Fixed_32          : Proto_Support.Unsigned_32_Vectors.Vector;
        Unpacked_Fixed_64          : Proto_Support.Unsigned_64_Vectors.Vector;
        Unpacked_Sfixed_32         : Proto_Support.Integer_32_Vectors.Vector;
        Unpacked_Sfixed_64         : Proto_Support.Integer_64_Vectors.Vector;
        Unpacked_Float             : Proto_Support.IEEE_Float_32_Vectors
          .Vector;
        Unpacked_Double            : Proto_Support.IEEE_Float_64_Vectors
          .Vector;
        Unpacked_Bool              : Proto_Support.Boolean_Vectors.Vector;
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
        Map_Int_32_Bool            : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Map_Int_32Bool_Entry_Vector;
        Map_Int_32_Float           : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Map_Int_32Float_Entry_Vector;
        Map_Int_32_Double          : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Map_Int_32Double_Entry_Vector;
        Map_Int_32_Nested_Message  : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Map_Int_32Nested_Message_Entry_Vector;
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
        Multiwordgroupfield        : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Optional_Multi_Word_Group_Field;
        Default_Int_32             : Proto_Support.Integer_32_Vectors.Option;
        Default_Int_64             : Proto_Support.Integer_64_Vectors.Option;
        Default_Uint_32            : Proto_Support.Unsigned_32_Vectors.Option;
        Default_Uint_64            : Proto_Support.Unsigned_64_Vectors.Option;
        Default_Sint_32            : Proto_Support.Integer_32_Vectors.Option;
        Default_Sint_64            : Proto_Support.Integer_64_Vectors.Option;
        Default_Fixed_32           : Proto_Support.Unsigned_32_Vectors.Option;
        Default_Fixed_64           : Proto_Support.Unsigned_64_Vectors.Option;
        Default_Sfixed_32          : Proto_Support.Integer_32_Vectors.Option;
        Default_Sfixed_64          : Proto_Support.Integer_64_Vectors.Option;
        Default_Float              : Proto_Support.IEEE_Float_32_Vectors
          .Option;
        Default_Double             : Proto_Support.IEEE_Float_64_Vectors
          .Option;
        Default_Bool               : Proto_Support.Boolean_Vectors.Option;
        Default_String             : Proto_Support.Universal_String_Vectors
          .Option;
        Default_Bytes              : Proto_Support
          .Stream_Element_Vector_Vectors.Option;
        Fieldname_1                : Proto_Support.Integer_32_Vectors.Option;
        Field_Name_2               : Proto_Support.Integer_32_Vectors.Option;
        Field_Name_3               : Proto_Support.Integer_32_Vectors.Option;
        Field_Name_4               : Proto_Support.Integer_32_Vectors.Option;
        Field_0name_5              : Proto_Support.Integer_32_Vectors.Option;
        Field_0_Name_6             : Proto_Support.Integer_32_Vectors.Option;
        Field_Name_7               : Proto_Support.Integer_32_Vectors.Option;
        Field_Name_8               : Proto_Support.Integer_32_Vectors.Option;
        Field_Name_9               : Proto_Support.Integer_32_Vectors.Option;
        Field_Name_10              : Proto_Support.Integer_32_Vectors.Option;
        FIELD_NAME11               : Proto_Support.Integer_32_Vectors.Option;
        FIELD_Name_12              : Proto_Support.Integer_32_Vectors.Option;
        Field_Name_13              : Proto_Support.Integer_32_Vectors.Option;
        Field_Name_14              : Proto_Support.Integer_32_Vectors.Option;
        Field_Name_15              : Proto_Support.Integer_32_Vectors.Option;
        Field_Name_16              : Proto_Support.Integer_32_Vectors.Option;
        Field_Name_17              : Proto_Support.Integer_32_Vectors.Option;
        Field_Name_18              : Proto_Support.Integer_32_Vectors.Option;
        Message_Set_Correct        : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Optional_Message_Set_Correct;
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

   type Test_All_Required_Types_Proto_2_Nested_Message is
     record
        A                    : Interfaces.Integer_32 := 0;
        Corecursive          : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Test_All_Required_Types_Proto_2_Vector;
        Optional_Corecursive : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Test_All_Required_Types_Proto_2_Vector;
     end record;

   type Optional_Test_All_Required_Types_Proto_2_Nested_Message
     (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Test_All_Required_Types_Proto_2_Nested_Message;
           when False =>
              null;
        end case;
     end record;

   function Length
    (Self : Test_All_Required_Types_Proto_2_Nested_Message_Vector)
      return Natural;

   procedure Clear
    (Self : in out Test_All_Required_Types_Proto_2_Nested_Message_Vector);

   procedure Append
    (Self : in out Test_All_Required_Types_Proto_2_Nested_Message_Vector;
     V    : Test_All_Required_Types_Proto_2_Nested_Message);

   type Test_All_Required_Types_Proto_2_Nested_Message_Variable_Reference
     (Element : not null access Test_All_Required_Types_Proto_2_Nested_Message) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Test_All_Required_Types_Proto_2_Nested_Message_Variable_Reference
    (Self  : aliased in out Test_All_Required_Types_Proto_2_Nested_Message_Vector;
     Index : Positive)
      return Test_All_Required_Types_Proto_2_Nested_Message_Variable_Reference
     with Inline;

   type Test_All_Required_Types_Proto_2_Nested_Message_Constant_Reference
     (Element : not null access constant Test_All_Required_Types_Proto_2_Nested_Message) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Test_All_Required_Types_Proto_2_Nested_Message_Constant_Reference
    (Self  : aliased Test_All_Required_Types_Proto_2_Nested_Message_Vector;
     Index : Positive)
      return Test_All_Required_Types_Proto_2_Nested_Message_Constant_Reference
     with Inline;

   type Test_All_Required_Types_Proto_2 is
     record
        Required_Int_32            : Interfaces.Integer_32 := 0;
        Required_Int_64            : Interfaces.Integer_64 := 0;
        Required_Uint_32           : Interfaces.Unsigned_32 := 0;
        Required_Uint_64           : Interfaces.Unsigned_64 := 0;
        Required_Sint_32           : Interfaces.Integer_32 := 0;
        Required_Sint_64           : Interfaces.Integer_64 := 0;
        Required_Fixed_32          : Interfaces.Unsigned_32 := 0;
        Required_Fixed_64          : Interfaces.Unsigned_64 := 0;
        Required_Sfixed_32         : Interfaces.Integer_32 := 0;
        Required_Sfixed_64         : Interfaces.Integer_64 := 0;
        Required_Float             : Interfaces.IEEE_Float_32 := 0.0;
        Required_Double            : Interfaces.IEEE_Float_64 := 0.0;
        Required_Bool              : Boolean := False;
        Required_String            : League.Strings.Universal_String;
        Required_Bytes             : League.Stream_Element_Vectors
          .Stream_Element_Vector;
        Required_Nested_Message    : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2
          .Test_All_Required_Types_Proto_2_Nested_Message;
        Required_Foreign_Message   : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Foreign_Message_Proto_2;
        Required_Nested_Enum       : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Test_All_Required_Types_Proto_2_Nested_Enum :=
          Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.FOO;
        Required_Foreign_Enum      : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Foreign_Enum_Proto_2 :=
          Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2.FOREIGN_FOO;
        Required_String_Piece      : League.Strings.Universal_String;
        Required_Cord              : League.Strings.Universal_String;
        Recursive_Message          : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Test_All_Required_Types_Proto_2_Vector;
        Optional_Recursive_Message : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Test_All_Required_Types_Proto_2_Vector;
        Data                       : Protobuf_Test_Messages.Proto_2
          .Test_Messages_Proto_2.Test_All_Required_Types_Proto_2_Data;
        Default_Int_32             : Interfaces.Integer_32 := 0;
        Default_Int_64             : Interfaces.Integer_64 := 0;
        Default_Uint_32            : Interfaces.Unsigned_32 := 0;
        Default_Uint_64            : Interfaces.Unsigned_64 := 0;
        Default_Sint_32            : Interfaces.Integer_32 := 0;
        Default_Sint_64            : Interfaces.Integer_64 := 0;
        Default_Fixed_32           : Interfaces.Unsigned_32 := 0;
        Default_Fixed_64           : Interfaces.Unsigned_64 := 0;
        Default_Sfixed_32          : Interfaces.Integer_32 := 0;
        Default_Sfixed_64          : Interfaces.Integer_64 := 0;
        Default_Float              : Interfaces.IEEE_Float_32 := 0.0;
        Default_Double             : Interfaces.IEEE_Float_64 := 0.0;
        Default_Bool               : Boolean := False;
        Default_String             : League.Strings.Universal_String;
        Default_Bytes              : League.Stream_Element_Vectors
          .Stream_Element_Vector;
     end record;

   type Optional_Test_All_Required_Types_Proto_2
     (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2
                .Test_All_Required_Types_Proto_2;
           when False =>
              null;
        end case;
     end record;

   function Length
    (Self : Test_All_Required_Types_Proto_2_Vector)
      return Natural;

   procedure Clear (Self : in out Test_All_Required_Types_Proto_2_Vector);

   procedure Append
    (Self : in out Test_All_Required_Types_Proto_2_Vector;
     V    : Test_All_Required_Types_Proto_2);

   type Test_All_Required_Types_Proto_2_Variable_Reference
     (Element : not null access Test_All_Required_Types_Proto_2) is null record
     with Implicit_Dereference => Element;

   not overriding function Get_Test_All_Required_Types_Proto_2_Variable_Reference
    (Self  : aliased in out Test_All_Required_Types_Proto_2_Vector;
     Index : Positive)
      return Test_All_Required_Types_Proto_2_Variable_Reference
     with Inline;

   type Test_All_Required_Types_Proto_2_Constant_Reference
     (Element : not null access constant Test_All_Required_Types_Proto_2) is
     null record
     with Implicit_Dereference => Element;

   not overriding function Get_Test_All_Required_Types_Proto_2_Constant_Reference
    (Self  : aliased Test_All_Required_Types_Proto_2_Vector;
     Index : Positive)
      return Test_All_Required_Types_Proto_2_Constant_Reference
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

   procedure Read_Map_Int_32Bool_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Int_32Bool_Entry);

   procedure Write_Map_Int_32Bool_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Int_32Bool_Entry);

   for Map_Int_32Bool_Entry'Read use Read_Map_Int_32Bool_Entry;

   for Map_Int_32Bool_Entry'Write use Write_Map_Int_32Bool_Entry;

   type Map_Int_32Bool_Entry_Array is
     array (Positive range <>) of aliased Map_Int_32Bool_Entry;

   type Map_Int_32Bool_Entry_Array_Access is access Map_Int_32Bool_Entry_Array;

   type Map_Int_32Bool_Entry_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Map_Int_32Bool_Entry_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Map_Int_32Bool_Entry_Vector);

   overriding procedure Finalize (Self : in out Map_Int_32Bool_Entry_Vector);

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

   procedure Read_Map_Int_32Nested_Message_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Map_Int_32Nested_Message_Entry);

   procedure Write_Map_Int_32Nested_Message_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Map_Int_32Nested_Message_Entry);

   for Map_Int_32Nested_Message_Entry'Read use Read_Map_Int_32Nested_Message_Entry;

   for Map_Int_32Nested_Message_Entry'Write use Write_Map_Int_32Nested_Message_Entry;

   type Map_Int_32Nested_Message_Entry_Array is
     array (Positive range <>) of aliased Map_Int_32Nested_Message_Entry;

   type Map_Int_32Nested_Message_Entry_Array_Access is
     access Map_Int_32Nested_Message_Entry_Array;

   type Map_Int_32Nested_Message_Entry_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Map_Int_32Nested_Message_Entry_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust
    (Self : in out Map_Int_32Nested_Message_Entry_Vector);

   overriding procedure Finalize
    (Self : in out Map_Int_32Nested_Message_Entry_Vector);

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

   procedure Read_Multi_Word_Group_Field
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Multi_Word_Group_Field);

   procedure Write_Multi_Word_Group_Field
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Multi_Word_Group_Field);

   for Multi_Word_Group_Field'Read use Read_Multi_Word_Group_Field;

   for Multi_Word_Group_Field'Write use Write_Multi_Word_Group_Field;

   type Multi_Word_Group_Field_Array is
     array (Positive range <>) of aliased Multi_Word_Group_Field;

   type Multi_Word_Group_Field_Array_Access is
     access Multi_Word_Group_Field_Array;

   type Multi_Word_Group_Field_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Multi_Word_Group_Field_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Multi_Word_Group_Field_Vector);

   overriding procedure Finalize (Self : in out Multi_Word_Group_Field_Vector);

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

   procedure Read_Extension_With_Oneof
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Extension_With_Oneof);

   procedure Write_Extension_With_Oneof
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Extension_With_Oneof);

   for Extension_With_Oneof'Read use Read_Extension_With_Oneof;

   for Extension_With_Oneof'Write use Write_Extension_With_Oneof;

   type Extension_With_Oneof_Array is
     array (Positive range <>) of aliased Extension_With_Oneof;

   type Extension_With_Oneof_Array_Access is access Extension_With_Oneof_Array;

   type Extension_With_Oneof_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Extension_With_Oneof_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Extension_With_Oneof_Vector);

   overriding procedure Finalize (Self : in out Extension_With_Oneof_Vector);

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

   procedure Read_Group_Field
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Group_Field);

   procedure Write_Group_Field
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Group_Field);

   for Group_Field'Read use Read_Group_Field;

   for Group_Field'Write use Write_Group_Field;

   type Group_Field_Array is array (Positive range <>) of aliased Group_Field;

   type Group_Field_Array_Access is access Group_Field_Array;

   type Group_Field_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Group_Field_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Group_Field_Vector);

   overriding procedure Finalize (Self : in out Group_Field_Vector);

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

   procedure Read_Null_Hypothesis_Proto_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Null_Hypothesis_Proto_2);

   procedure Write_Null_Hypothesis_Proto_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Null_Hypothesis_Proto_2);

   for Null_Hypothesis_Proto_2'Read use Read_Null_Hypothesis_Proto_2;

   for Null_Hypothesis_Proto_2'Write use Write_Null_Hypothesis_Proto_2;

   type Null_Hypothesis_Proto_2_Array is
     array (Positive range <>) of aliased Null_Hypothesis_Proto_2;

   type Null_Hypothesis_Proto_2_Array_Access is
     access Null_Hypothesis_Proto_2_Array;

   type Null_Hypothesis_Proto_2_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Null_Hypothesis_Proto_2_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Null_Hypothesis_Proto_2_Vector);

   overriding procedure Finalize
    (Self : in out Null_Hypothesis_Proto_2_Vector);

   procedure Read_Enum_Only_Proto_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Enum_Only_Proto_2);

   procedure Write_Enum_Only_Proto_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Enum_Only_Proto_2);

   for Enum_Only_Proto_2'Read use Read_Enum_Only_Proto_2;

   for Enum_Only_Proto_2'Write use Write_Enum_Only_Proto_2;

   type Enum_Only_Proto_2_Array is
     array (Positive range <>) of aliased Enum_Only_Proto_2;

   type Enum_Only_Proto_2_Array_Access is access Enum_Only_Proto_2_Array;

   type Enum_Only_Proto_2_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Enum_Only_Proto_2_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Enum_Only_Proto_2_Vector);

   overriding procedure Finalize (Self : in out Enum_Only_Proto_2_Vector);

   procedure Read_One_String_Proto_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out One_String_Proto_2);

   procedure Write_One_String_Proto_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : One_String_Proto_2);

   for One_String_Proto_2'Read use Read_One_String_Proto_2;

   for One_String_Proto_2'Write use Write_One_String_Proto_2;

   type One_String_Proto_2_Array is
     array (Positive range <>) of aliased One_String_Proto_2;

   type One_String_Proto_2_Array_Access is access One_String_Proto_2_Array;

   type One_String_Proto_2_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : One_String_Proto_2_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out One_String_Proto_2_Vector);

   overriding procedure Finalize (Self : in out One_String_Proto_2_Vector);

   procedure Read_Proto_With_Keywords
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Proto_With_Keywords);

   procedure Write_Proto_With_Keywords
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Proto_With_Keywords);

   for Proto_With_Keywords'Read use Read_Proto_With_Keywords;

   for Proto_With_Keywords'Write use Write_Proto_With_Keywords;

   type Proto_With_Keywords_Array is
     array (Positive range <>) of aliased Proto_With_Keywords;

   type Proto_With_Keywords_Array_Access is access Proto_With_Keywords_Array;

   type Proto_With_Keywords_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Proto_With_Keywords_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Proto_With_Keywords_Vector);

   overriding procedure Finalize (Self : in out Proto_With_Keywords_Vector);

   procedure Read_Test_All_Required_Types_Proto_2_Nested_Message
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Test_All_Required_Types_Proto_2_Nested_Message);

   procedure Write_Test_All_Required_Types_Proto_2_Nested_Message
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Test_All_Required_Types_Proto_2_Nested_Message);

   for Test_All_Required_Types_Proto_2_Nested_Message'Read use Read_Test_All_Required_Types_Proto_2_Nested_Message;

   for Test_All_Required_Types_Proto_2_Nested_Message'Write use Write_Test_All_Required_Types_Proto_2_Nested_Message;

   type Test_All_Required_Types_Proto_2_Nested_Message_Array is
     array
       (Positive range <>) of
     aliased Test_All_Required_Types_Proto_2_Nested_Message;

   type Test_All_Required_Types_Proto_2_Nested_Message_Array_Access is
     access Test_All_Required_Types_Proto_2_Nested_Message_Array;

   type Test_All_Required_Types_Proto_2_Nested_Message_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Test_All_Required_Types_Proto_2_Nested_Message_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust
    (Self : in out Test_All_Required_Types_Proto_2_Nested_Message_Vector);

   overriding procedure Finalize
    (Self : in out Test_All_Required_Types_Proto_2_Nested_Message_Vector);

   procedure Read_Test_All_Required_Types_Proto_2_Data
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Test_All_Required_Types_Proto_2_Data);

   procedure Write_Test_All_Required_Types_Proto_2_Data
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Test_All_Required_Types_Proto_2_Data);

   for Test_All_Required_Types_Proto_2_Data'Read use Read_Test_All_Required_Types_Proto_2_Data;

   for Test_All_Required_Types_Proto_2_Data'Write use Write_Test_All_Required_Types_Proto_2_Data;

   type Test_All_Required_Types_Proto_2_Data_Array is
     array (Positive range <>) of aliased Test_All_Required_Types_Proto_2_Data;

   type Test_All_Required_Types_Proto_2_Data_Array_Access is
     access Test_All_Required_Types_Proto_2_Data_Array;

   type Test_All_Required_Types_Proto_2_Data_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Test_All_Required_Types_Proto_2_Data_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust
    (Self : in out Test_All_Required_Types_Proto_2_Data_Vector);

   overriding procedure Finalize
    (Self : in out Test_All_Required_Types_Proto_2_Data_Vector);

   procedure Read_Test_All_Required_Types_Proto_2_Message_Set_Correct
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Test_All_Required_Types_Proto_2_Message_Set_Correct);

   procedure Write_Test_All_Required_Types_Proto_2_Message_Set_Correct
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Test_All_Required_Types_Proto_2_Message_Set_Correct);

   for Test_All_Required_Types_Proto_2_Message_Set_Correct'Read use Read_Test_All_Required_Types_Proto_2_Message_Set_Correct;

   for Test_All_Required_Types_Proto_2_Message_Set_Correct'Write use Write_Test_All_Required_Types_Proto_2_Message_Set_Correct;

   type Test_All_Required_Types_Proto_2_Message_Set_Correct_Array is
     array
       (Positive range <>) of
     aliased Test_All_Required_Types_Proto_2_Message_Set_Correct;

   type Test_All_Required_Types_Proto_2_Message_Set_Correct_Array_Access is
     access Test_All_Required_Types_Proto_2_Message_Set_Correct_Array;

   type Test_All_Required_Types_Proto_2_Message_Set_Correct_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Test_All_Required_Types_Proto_2_Message_Set_Correct_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust
    (Self : in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Vector);

   overriding procedure Finalize
    (Self : in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Vector);

   procedure Read_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1);

   procedure Write_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1);

   for Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1'Read use Read_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1;

   for Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1'Write use Write_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1;

   type Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Array is
     array
       (Positive range <>) of
     aliased Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1;

   type Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Array_Access is
     access Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Array;

   type Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust
    (Self : in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Vector);

   overriding procedure Finalize
    (Self : in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_1_Vector);

   procedure Read_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2);

   procedure Write_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2);

   for Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2'Read use Read_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2;

   for Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2'Write use Write_Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2;

   type Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Array is
     array
       (Positive range <>) of
     aliased Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2;

   type Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Array_Access is
     access Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Array;

   type Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust
    (Self : in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Vector);

   overriding procedure Finalize
    (Self : in out Test_All_Required_Types_Proto_2_Message_Set_Correct_Extension_2_Vector);

   procedure Read_Test_All_Required_Types_Proto_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Test_All_Required_Types_Proto_2);

   procedure Write_Test_All_Required_Types_Proto_2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Test_All_Required_Types_Proto_2);

   for Test_All_Required_Types_Proto_2'Read use Read_Test_All_Required_Types_Proto_2;

   for Test_All_Required_Types_Proto_2'Write use Write_Test_All_Required_Types_Proto_2;

   type Test_All_Required_Types_Proto_2_Array is
     array (Positive range <>) of aliased Test_All_Required_Types_Proto_2;

   type Test_All_Required_Types_Proto_2_Array_Access is
     access Test_All_Required_Types_Proto_2_Array;

   type Test_All_Required_Types_Proto_2_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Test_All_Required_Types_Proto_2_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust
    (Self : in out Test_All_Required_Types_Proto_2_Vector);

   overriding procedure Finalize
    (Self : in out Test_All_Required_Types_Proto_2_Vector);

   procedure Read_A1
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out A1);

   procedure Write_A1
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : A1);

   for A1'Read use Read_A1;

   for A1'Write use Write_A1;

   type A1_Array is array (Positive range <>) of aliased A1;

   type A1_Array_Access is access A1_Array;

   type A1_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : A1_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out A1_Vector);

   overriding procedure Finalize (Self : in out A1_Vector);

   procedure Read_A2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out A2);

   procedure Write_A2
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : A2);

   for A2'Read use Read_A2;

   for A2'Write use Write_A2;

   type A2_Array is array (Positive range <>) of aliased A2;

   type A2_Array_Access is access A2_Array;

   type A2_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : A2_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out A2_Vector);

   overriding procedure Finalize (Self : in out A2_Vector);

   procedure Read_A3
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out A3);

   procedure Write_A3
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : A3);

   for A3'Read use Read_A3;

   for A3'Write use Write_A3;

   type A3_Array is array (Positive range <>) of aliased A3;

   type A3_Array_Access is access A3_Array;

   type A3_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : A3_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out A3_Vector);

   overriding procedure Finalize (Self : in out A3_Vector);

   procedure Read_A4
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out A4);

   procedure Write_A4
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : A4);

   for A4'Read use Read_A4;

   for A4'Write use Write_A4;

   type A4_Array is array (Positive range <>) of aliased A4;

   type A4_Array_Access is access A4_Array;

   type A4_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : A4_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out A4_Vector);

   overriding procedure Finalize (Self : in out A4_Vector);

   procedure Read_A5
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out A5);

   procedure Write_A5
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : A5);

   for A5'Read use Read_A5;

   for A5'Write use Write_A5;

   type A5_Array is array (Positive range <>) of aliased A5;

   type A5_Array_Access is access A5_Array;

   type A5_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : A5_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out A5_Vector);

   overriding procedure Finalize (Self : in out A5_Vector);

   procedure Read_Test_Large_Oneof
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Test_Large_Oneof);

   procedure Write_Test_Large_Oneof
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Test_Large_Oneof);

   for Test_Large_Oneof'Read use Read_Test_Large_Oneof;

   for Test_Large_Oneof'Write use Write_Test_Large_Oneof;

   type Test_Large_Oneof_Array is
     array (Positive range <>) of aliased Test_Large_Oneof;

   type Test_Large_Oneof_Array_Access is access Test_Large_Oneof_Array;

   type Test_Large_Oneof_Vector is
     new Ada.Finalization.Controlled
     with record
        Data   : Test_Large_Oneof_Array_Access;
        Length : Natural := 0;
     end record;

   overriding procedure Adjust (Self : in out Test_Large_Oneof_Vector);

   overriding procedure Finalize (Self : in out Test_Large_Oneof_Vector);

end Protobuf_Test_Messages.Proto_2.Test_Messages_Proto_2;