with Ada.Finalization;
with Ada.Streams;
with Ada.Containers.Vectors;

-- At present the package relies on Stream_Element being 8-bits in size, but
-- size of a Stream_Element is implementation defined. Better fix this???

package Google.Protobuf.Buffered_Byte_Stream is
   type Stream_Access is access all Ada.Streams.Root_Stream_Type'Class;

   type Byte_Buffer (Size : Ada.Containers.Count_Type) is new Ada.Finalization.Limited_Controlled with private;

   type Byte_Stream (Size : Ada.Containers.Count_Type) is new Ada.Streams.Root_Stream_Type with record
      Byte_Stream_Buffer : Byte_Buffer (Size);
   end record;

   type Byte_Stream_Access is access all Byte_Stream;

   overriding
   procedure Read (Stream : in out Byte_Stream;
                   Item   : out Ada.Streams.Stream_Element_Array;
                   Last   : out Ada.Streams.Stream_Element_Offset);

   overriding
   procedure Write (Stream : in out Byte_Stream;
                    Item   : in Ada.Streams.Stream_Element_Array);

   procedure Reset (Stream : in Byte_Stream_Access);

   function To_Stream_Element_Array (Stream : in Byte_Stream_Access) return Ada.Streams.Stream_Element_Array;

   procedure Empty_And_Load_Buffer (Stream : in Byte_Stream_Access; Byte_Array : in Ada.Streams.Stream_Element_Array);

   procedure Free (Stream : in out Byte_Stream_Access);

private

   package Stream_Element_Buffer is new Ada.Containers.Vectors (Natural, Ada.Streams.Stream_Element, Ada.Streams."=");

   type Byte_Buffer (Size : Ada.Containers.Count_Type) is new Ada.Finalization.Limited_Controlled with record
      Count        : Ada.Containers.Count_Type;
      Buffer       : Stream_Element_Buffer.Vector;
   end record;

   overriding
   procedure Initialize (The_Byte_Buffer : in out Byte_Buffer);

   procedure Read (The_Byte_Buffer : in out Byte_Buffer;
                   Item            : out Ada.Streams.Stream_Element_Array;
                   Last            : out Ada.Streams.Stream_Element_Offset);

   procedure Write (The_Byte_Buffer : in out Byte_Buffer;
                    Item            : in Ada.Streams.Stream_Element_Array);

end Google.Protobuf.Buffered_Byte_Stream;
