with Ada.Unchecked_Deallocation;
with PB_Support.IO;
with PB_Support.Internal;

package body Google.Protobuf.Empty is

   function Length (Self : Empty_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Empty_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Empty_Array, Empty_Array_Access);

   procedure Append (Self : in out Empty_Vector; V    : Empty) is
      Init_Length : constant Positive := Positive'Max (1, 256 / Empty'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Empty_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Empty_Array'
             (Self.Data.all & Empty_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Empty_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Empty_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Empty_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Empty_Variable_Reference
    (Self  : aliased in out Empty_Vector;
     Index : Positive)
      return Empty_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Empty_Variable_Reference;

   not overriding function Get_Empty_Constant_Reference
    (Self  : aliased Empty_Vector;
     Index : Positive)
      return Empty_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Empty_Constant_Reference;

   procedure Read_Empty
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Empty) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Empty;

   procedure Write_Empty
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Empty) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Empty (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if WS.End_Message then
            Write_Empty (WS'Access, V);
         end if;
      end;
   end Write_Empty;

end Google.Protobuf.Empty;