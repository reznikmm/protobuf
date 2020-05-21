with Ada.Unchecked_Deallocation;
with PB_Support.IO;
with PB_Support.Internal;

package body Google.Protobuf.Any is

   function Length (Self : Any_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Any_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Any_Array, Any_Array_Access);

   procedure Append (Self : in out Any_Vector; V    : Any) is
      Init_Length : constant Positive := Positive'Max (1, 256 / Any'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Any_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Any_Array'(Self.Data.all & Any_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Any_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Any_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Any_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Any_Variable_Reference
    (Self  : aliased in out Any_Vector;
     Index : Positive)
      return Any_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Any_Variable_Reference;

   not overriding function Get_Any_Constant_Reference
    (Self  : aliased Any_Vector;
     Index : Positive)
      return Any_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Any_Constant_Reference;

   procedure Read_Any
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Any) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read (Stream, Key.Encoding, V.Type_Url);
            when 2 =>
               PB_Support.IO.Read (Stream, Key.Encoding, V.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Any;

   procedure Write_Any
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Any) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Any (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Option (1, V.Type_Url);
         WS.Write_Option (2, V.Value);
         if WS.End_Message then
            Write_Any (WS'Access, V);
         end if;
      end;
   end Write_Any;

end Google.Protobuf.Any;