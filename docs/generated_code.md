Ada Generated Code
==================

## Compiler Invocation

The protocol buffer compiler produces Ada output when invoked with
the `--ada_out=` command-line flag (be sure the `protoc-gen-ada`
plugin in the `PATH`).  The compiler creates a specification file
(`.ads`) and an implementation file (`.adb`) for each `.proto` file
input. The names of the output files are computed by taking the name
of the `.proto` file and a name from a `package` directive, if any.

## Packages

If a `.proto` file contains a package declaration, then it will be used as
a prefix of the corresponding Ada package.
For example, given the `package` declaration in `xxx.proto` file: 

```protobuf
package foo.bar;
```

All declarations in the file will reside in the `Foo.Bar.Xxx` package. 

## Messages

Given a simple message declaration: 
```protobuf
message Blah {}
```

The protocol buffer compiler generates a record type called `Blah`
and corresponding `'Read/'Write` aspects to serialization to/from
`Ada.Streams.Root_Stream_Type`. For parsing from and serializing
to a binary stream use corresponding Ada streaming:

```ada
declare
   Stream : Ada.Streams.Stream_IO.Stream_Access :=
      Ada.Streams.Stream_IO.Stream (Input);
   Data : Foo.Bar.Blah;
begin
   Foo.Bar.Blah'Read (Stream, Data);   --  Parsing
   Foo.Bar.Blah'Write (Stream, Data);  --  Serializing
end;
```

Also, for each message declaration the compiler creates corresponding
_vector_ and _optiional_ types. Optional type looks like this:

```ada
   type Optional_Blah (Is_Set : Boolean := False) is
     record
        case Is_Set is
           when True =>
              Value : Blah;
           when False =>
              null;
        end case;
     end record;
```
A user can check if optional field is present by checking its discriminant.

```ada
procedure Process (V : in out Optional_Blah) is
begin
   if V.Is_Set then  --  Check if the field present
      Print (V.Value);  --  Access the field value
      V := (Is_Set => False);  --  Clear the field
   else
      V := (Is_Set => True, Value => 123);  --  Assign
   end if; 
end Process;
```
Vector type a bit more complex. To allow indexing on the vector compiler
generates Ada 2012 `Variable_Indexing`, `Constant_Indexing`
aspects and all related constructs.
These are mostly meaningless for the user, but makes Ada compiler happy.
It aliso generate `Length`, `Clear` and `Append` routines.

```ada
   type Blah_Vector is tagged private
     with
       Variable_Indexing => Get_Blah_Variable_Reference,
       Constant_Indexing => Get_Blah_Constant_Reference;
   
   type Blah_Variable_Reference
     (Element : not null access Blah) is null record
       with Implicit_Dereference => Element;
   
   function Get_Blah_Variable_Reference
    (Self  : aliased in out Blah_Vector;
     Index : Positive) return Blah_Variable_Reference
       with Inline;

   type Blah_Constant_Reference
     (Element : not null access constant Blah) is
       null record
         with Implicit_Dereference => Element;
   
   function Get_Blah_Constant_Reference
    (Self  : aliased in out Blah_Vector;
     Index : Positive) return Blah_Constant_Reference
       with Inline;

   function Length (Self : Blah_Vector) return Natural;

   procedure Clear (Self : in out Blah_Vector);

   procedure Append
    (Self  : in out Blah_Vector;
     Value : Blah);
```

With these declarations a user can process vectors like this:
```ada
procedure Process (V : in out Blah_Vector) is
begin
   for J in 1 .. V.Length loop
      Print (V (J));
   end loop;
   V.Clear;
   V.Append (123);
end Process;
```


## Nested Types
A message can be declared inside another message. For example:
`message Foo { message Bar { } }`. In this case, the compiler generates
two types: `Foo` and `Bar`. Compiler can change declaration order to
follow forward-declare restriction.

## Fields
For each message field the protocol buffer compiler generates
a record component. The component has default initialization when
this is required.

### Fields of Predefined Types
The compiler uses this mapping for predefined types:

| .proto type | Ada type      |
| ----------- | ------------- |
| double      | Interfaces.IEEE_Float_64 |
| float       | Interfaces.IEEE_Float_32 |
| int32/64    | Interfaces.Integer_32/64 |
| uint32/64   | Interfaces.Unsigned_32/64 |
| sint32/64   | Interfaces.Integer_32/64 (unimplemented) |
| fixed32/64  | Interfaces.Unsigned_32/64 (little endian only)|
| sfixed32/64 | Interfaces.Integer_32/64 (little endian only)|
| bool        | Boolean |
| string      | League.Strings.Universal_String |
| bytes       | League.Stream_Element_Vectors.Stream_Element_Vector |

For singular `proto3` fields and required `proto2` fields the compiler
generates just a component of the corresponding type. For instance:

```protobuf
int32 foo = 1; # proto3
required int32 foo = 1;  # proto2
````

```ada
type Blah is record
   Foo : Interfaces.Integer_32 := 0;
```

For optional proto2 fields the compiler uses an optional type from
corresponding generic package instantiation. Such instantiations are
parts of a protobuf support library provided by `protobuf_runtime.gpr`
project. All optional types have the same shape:
```ada
   type Option (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : Element_Type;
         when False =>
            null;
      end case;
   end record;
```

### Singular Enum Fields

Given the enum type:
```protobuf
enum Bar {
  BAR_VALUE = 0;
  OTHER_VALUE = 1;
}
```

For this field definitions:
```protobuf
Bar foo = 1;
```

The compiler will generate the following component: 
```ada
type Blah is record
   Foo : Bar := BAR_VALUE;
end record;
```

Note: enumeration parsing procedure uses Ada.Unchecked_Convention, so
read value could be invalid when `enum` definition changed and new literals
are added. To detect such situation you can check the component value
with `X.Foo'Valid` attribute before using.


### Singular Embedded Message Fields
Given the message type `Bar`, for any of these field definitions:

```protobuf
optional Bar foo1 = 1;
required Bar foo2 = 1;
```

The compiler will generate the following accessor components:
```ada
type Blah is record
  Foo_1 : Bar;
  Foo_2 : Optional_Bar;
end record;
```

#### Partial support for mutual dependent messages.
With such representation we can't have mutual dependent message declarations,
because this is not allowed in Ada. But if the field is repeated then all
works fine, because a vector type breaks circular dependency.
To give at least a minimum support for such case the compiler tries to
break a circle by replacing a singular message fiels with a repeated one.
User should ensure that the vector contains required number of elements
(exactly one for `required` field and zero-or-one for `optional` field).

### Repeated Fields of Predefined Types
For repeated fields of predefined types the compiler uses corresponding
vector type from a package instantiation provided by support library,
like this:

```ada
type Location is record
   Path : PB_Support.Unsigned_32_Vectors.Vector;
```

There is an exception for the string type.

### Repeated String Fields
For repeated fields of the string type the compiler uses
`Universal_String_Vector` type declared in `League.String_Vectors` package.

### Repeated Enum Fields
For repeated fields of an `enum` type the compiler uses
a vector type from corresponding generic instantiation.

### Repeated Embedded Message Fields
For repeated fields of a message type the compiler uses
a vector type declared with the message type.

### Oneof Fields
For now we support only single `Oneof` construction per message.
Compiler generates a component with name `Variant` of a corresponding type.
This component has a discriminant with name of `Oneof`.
There is a single component for each disciminant value.

### Map Fields
There is no special support for map fields for now.

## Any
`Any` type isn't provided yet.

## Oneof
Currently we support only single `Oneof` construction per message.
For a such construction the compiler generates an enumeration type and
a descriminated record type. Inside a message it generates one component
with name `Variant`. For example:
```protobuf
message Value {
  // The kind of value.
  oneof kind {
    // Represents a null value.
    NullValue null_value = 1;
    // Represents a double value.
    double number_value = 2;
  }
}
```

Generated code looks like:
```ada
type Value_Variant_Kind is
  (Kind_Not_Set,
   Null_Value_Kind,
   Number_Value_Kind);

type Value_Variant (Kind : Value_Variant_Kind := Kind_Not_Set) is record
   case Kind is
      when Kind_Not_Set =>
         null;
      when Null_Value_Kind =>
         Null_Value : Google.Protobuf.Struct.Null_Value :=
           Google.Protobuf.Struct.PB_NULL_VALUE;
      when Number_Value_Kind =>
         Number_Value : Interfaces.IEEE_Float_64 := 0.0;
   end case;
end record;

type Value is record
   Variant : Value_Variant;
end record;
```

A dedicated enumeration literal `<oneof_name>_Not_Set` represents a case
when none of filed is set.

## Enumerations
Given an enum definition like:

```protobuf
enum Foo {
  VALUE_A = 0;
  VALUE_B = 5;
  VALUE_C = 1234;
}
```

The protocol buffer compiler will generate a Ada enumeration type
called `Foo` with the same set of values. It also generates a
corresponding vector instantiation:
```ada
type Foo is (VALUE_A, VALUE_B, VALUE_C);
for Foo use (VALUE_A => 0, VALUE_B => 5, VALUE_C => 1234);

package Foo_Vectors is new PB_Support.Vectors (Foo);
```