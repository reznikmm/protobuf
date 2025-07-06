with Ada.Unchecked_Deallocation;
with Proto_Support.IO;
with Proto_Support.Internal;

package body Google.Protobuf.Duration is

   function Length (Self : Duration_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Duration_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Duration_Array, Duration_Array_Access);

   procedure Append (Self : in out Duration_Vector; V    : Duration) is
      Init_Length : constant Positive := Positive'Max (1, 256 / Duration'Size);
      Aux_Data    : Duration_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Duration_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Duration_Array'
             (Self.Data.all & Duration_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
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

   not overriding function Get_Duration_Variable_Reference
    (Self  : aliased in out Duration_Vector;
     Index : Positive)
      return Duration_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Duration_Variable_Reference;

   not overriding function Get_Duration_Constant_Reference
    (Self  : aliased Duration_Vector;
     Index : Positive)
      return Duration_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Duration_Constant_Reference;

   procedure Read_Duration
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Duration) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               Proto_Support.IO.Read_Varint (Stream, Key.Encoding, V.Seconds);
            when 2 =>
               Proto_Support.IO.Read_Varint (Stream, Key.Encoding, V.Nanos);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Duration;

   procedure Write_Duration
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Duration) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Duration (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Varint_Option (1, V.Seconds, 0);
         WS.Write_Varint_Option (2, V.Nanos, 0);
         if WS.End_Message then
            Write_Duration (WS'Access, V);
         end if;
      end;
   end Write_Duration;

end Google.Protobuf.Duration;