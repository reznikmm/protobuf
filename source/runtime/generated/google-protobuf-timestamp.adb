with Ada.Unchecked_Deallocation;
with PB_Support.IO;
with PB_Support.Internal;

package body Google.Protobuf.Timestamp is

   function Length (Self : Timestamp_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   function Get
    (Self  : Timestamp_Vector;
     Index : Positive)
      return Timestamp is
   begin
      return Self.Data (Index);
   end Get;

   procedure Clear (Self : in out Timestamp_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Timestamp_Array, Timestamp_Array_Access);

   procedure Append (Self : in out Timestamp_Vector; V    : Timestamp) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Timestamp'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Timestamp_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Timestamp_Array'
             (Self.Data.all & Timestamp_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Timestamp_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Timestamp_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Timestamp_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   procedure Read_Timestamp
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Timestamp) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.Seconds);
            when 2 =>
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.Nanos);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Timestamp;

   procedure Write_Timestamp
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Timestamp) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Timestamp (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Varint (1, V.Seconds);
         WS.Write_Varint (2, V.Nanos);
         if WS.End_Message then
            Write_Timestamp (WS'Access, V);
         end if;
      end;
   end Write_Timestamp;

end Google.Protobuf.Timestamp;