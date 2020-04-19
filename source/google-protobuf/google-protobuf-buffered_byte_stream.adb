with Ada.Unchecked_Deallocation;
package body Google.Protobuf.Buffered_Byte_Stream is
----------
-- Read --
----------

   overriding procedure Read
     (Stream : in out Byte_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
   begin
      Read (Stream.Byte_Stream_Buffer, Item, Last);
   end Read;

   -----------
   -- Reset --
   -----------

   procedure Reset (Stream : in Byte_Stream_Access)
   is
   begin
      Stream.Byte_Stream_Buffer.Count := 0;
      Stream.Byte_Stream_Buffer.Buffer.Clear;
      Stream.Byte_Stream_Buffer.Buffer.Reserve_Capacity (Stream.Byte_Stream_Buffer.Size);
   end Reset;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out Byte_Stream;
      Item   : in Ada.Streams.Stream_Element_Array)
   is
   begin
      Write (Stream.Byte_Stream_Buffer, Item);
   end Write;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (The_Byte_Buffer : in out Byte_Buffer)
   is
   begin
      The_Byte_Buffer.Count := Ada.Containers.Count_Type'First;
      The_Byte_Buffer.Buffer.Reserve_Capacity (Capacity => The_Byte_Buffer.Size);
   end Initialize;

   ----------
   -- Read --
   ----------

   procedure Read
     (The_Byte_Buffer : in out Byte_Buffer;
      Item            : out Ada.Streams.Stream_Element_Array;
      Last            : out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Containers.Count_Type;
      use type Ada.Streams.Stream_Element_Offset;
      Read_Count : Ada.Containers.Count_Type := Ada.Containers.Count_Type (Item'Last - Item'First + 1);
   begin
      if The_Byte_Buffer.Count = 0 then
         Last := Item'First - 1;
         return;
      end if;

      if Read_Count > The_Byte_Buffer.Count then
         Read_Count := The_Byte_Buffer.Count;
      end if;

      declare
         Item_Index   : Ada.Streams.Stream_Element_Offset := Item'First;
         Buffer_Index : Natural := The_Byte_Buffer.Buffer.First_Index;
      begin
         while Read_Count > 0 loop
            Item (Item_Index) := The_Byte_Buffer.Buffer.Element (Index => The_Byte_Buffer.Buffer.First_Index);
            The_Byte_Buffer.Buffer.Delete_First;
            Buffer_Index := Buffer_Index + 1;
            The_Byte_Buffer.Count := The_Byte_Buffer.Count - 1;
            Item_Index := Item_Index + 1;
            Read_Count := Read_Count - 1;
         end loop;
         Last := Item_Index - 1;
      end;

   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (The_Byte_Buffer : in out Byte_Buffer;
      Item            : in Ada.Streams.Stream_Element_Array)
   is
      use type Ada.Containers.Count_Type;
      use type Ada.Streams.Stream_Element_Offset;
      Item_Size : constant Ada.Streams.Stream_Element_Offset := Item'Last - Item'First + 1;
   begin
      for Index in Item'First .. Item'Last loop
         The_Byte_Buffer.Buffer.Append (Item (Index));
      end loop;

      The_Byte_Buffer.Count := The_Byte_Buffer.Count + Ada.Containers.Count_Type (Item_Size);
   end Write;

   -----------------------------
   -- To_Stream_Element_Array --
   -----------------------------

   function To_Stream_Element_Array (Stream : in Byte_Stream_Access) return Ada.Streams.Stream_Element_Array
   is
      use type Ada.Streams.Stream_Element_Offset;

      Index              : Ada.Streams.Stream_Element_Offset := 1;
      Length             : constant Ada.Streams.Stream_Element_Offset := Ada.Streams.Stream_Element_Offset (Stream.Byte_Stream_Buffer.Count);
      Buffer_Cursor      : Stream_Element_Buffer.Cursor := Stream_Element_Buffer.First (Container => Stream.Byte_Stream_Buffer.Buffer);
      Byte_Array         : Ada.Streams.Stream_Element_Array (1 .. Length);
   begin
      while Stream_Element_Buffer.Has_Element (Buffer_Cursor) loop
         Byte_Array (Index) := Stream_Element_Buffer.Element (Buffer_Cursor);
         Stream_Element_Buffer.Next (Buffer_Cursor);
         Index := Index + 1;
      end loop;

      return Byte_Array;
   end To_Stream_Element_Array;

   -----------------
   -- Load_Buffer --
   -----------------

   procedure Empty_And_Load_Buffer (Stream : in Byte_Stream_Access; Byte_Array : in Ada.Streams.Stream_Element_Array)
   is
      use type Ada.Containers.Count_Type;
   begin
      Reset (Stream);
      for I in Byte_Array'Range loop
         Stream.Byte_Stream_Buffer.Buffer.Append (Byte_Array (I));
         Stream.Byte_Stream_Buffer.Count := Stream.Byte_Stream_Buffer.Count + 1;
      end loop;
   end;

   ----------
   -- Free --
   ----------

   procedure Free (Stream : in out Byte_Stream_Access) is

      -----------------
      -- Free_Stream --
      -----------------

      procedure Free_Stream is
        new Ada.Unchecked_Deallocation (Object => Byte_Stream,
                                        Name   => Byte_Stream_Access);
   begin
      Free_Stream (Stream);
   end Free;



end Google.Protobuf.Buffered_Byte_Stream;
