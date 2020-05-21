with Ada.Unchecked_Deallocation;
with PB_Support.IO;
with PB_Support.Internal;

package body Google.Protobuf.Field_Mask is

   function Length (Self : Field_Mask_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Field_Mask_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Field_Mask_Array, Field_Mask_Array_Access);

   procedure Append (Self : in out Field_Mask_Vector; V    : Field_Mask) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Field_Mask'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Field_Mask_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Field_Mask_Array'
             (Self.Data.all & Field_Mask_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Field_Mask_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Field_Mask_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Field_Mask_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Field_Mask_Variable_Reference
    (Self  : aliased in out Field_Mask_Vector;
     Index : Positive)
      return Field_Mask_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Field_Mask_Variable_Reference;

   not overriding function Get_Field_Mask_Constant_Reference
    (Self  : aliased Field_Mask_Vector;
     Index : Positive)
      return Field_Mask_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Field_Mask_Constant_Reference;

   procedure Read_Field_Mask
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Field_Mask) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Vector (Stream, Key.Encoding, V.Paths);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Field_Mask;

   procedure Write_Field_Mask
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Field_Mask) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Field_Mask (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write (1, V.Paths);
         if WS.End_Message then
            Write_Field_Mask (WS'Access, V);
         end if;
      end;
   end Write_Field_Mask;

end Google.Protobuf.Field_Mask;