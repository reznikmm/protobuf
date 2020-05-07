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
_vector_ and _optiional_ types:

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

   type Blah_Vector is tagged private;

   function Length (Self : Blah_Vector) return Natural;

   function Get
    (Self  : Blah_Vector;
     Index : Positive)
      return Blah;

   procedure Clear (Self : in out Blah_Vector);

   procedure Append
    (Self  : in out Blah_Vector;
     Value : Blah);
```

An implementation of a vector type provides deep copy semantic.

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
end record
```

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
Oneof fields isn't implemented yet.

### Map Fields
There is no special support for map fields for now.

## Any
`Any` type isn't provided yet.

## Oneof
`Oneof` construction isn't supported yet.

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