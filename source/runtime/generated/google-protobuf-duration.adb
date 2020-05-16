with Ada.Unchecked_Deallocation;
with PB_Support.IO;
with PB_Support.Internal;

package body Google.Protobuf.Duration is

   function Length (Self : Duration_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   function Get (Self  : Duration_Vector; Index : Positive) return Duration is
   begin
      return Self.Data (Index);
   end Get;

   procedure Clear (Self : in out Duration_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Duration_Array, Duration_Array_Access);

   procedure Append (Self : in out Duration_Vector; V    : Duration) is
      Init_Length : constant Positive := Positive'Max (1, 256 / Duration'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Duration_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Duration_Array'
             (Self.Data.all & Duration_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Duration_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Duration_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Duration_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   procedure Read_Duration
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Duration) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Integer_64 (Stream, Key.Encoding, V.Seconds);
            when 2 =>
               PB_Support.IO.Read_Integer_32 (Stream, Key.Encoding, V.Nanos);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Duration;

   procedure Write_Duration
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Duration) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Duration (WS'Access, V);
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
            Write_Duration (WS'Access, V);
         end if;
      end;
   end Write_Duration;

end Google.Protobuf.Duration;