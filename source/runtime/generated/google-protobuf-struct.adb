with Ada.Unchecked_Deallocation;
with PB_Support.IO;
with PB_Support.Internal;

package body Google.Protobuf.Struct is

   package List_Value_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Struct.List_Value,
        Google.Protobuf.Struct.List_Value_Vector,
        Google.Protobuf.Struct.Append);

   type Integer_Null_Value is  range 0 .. 0
     with Size => Google.Protobuf.Struct.Null_Value'Size;

   package Null_Value_IO is
     new PB_Support.IO.Enum_IO
       (Google.Protobuf.Struct.Null_Value, Integer_Null_Value,
        Google.Protobuf.Struct.Null_Value_Vectors);

   package Struct_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Struct.Struct, Google.Protobuf.Struct.Struct_Vector,
        Google.Protobuf.Struct.Append);

   package Fields_Entry_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Struct.Fields_Entry,
        Google.Protobuf.Struct.Fields_Entry_Vector,
        Google.Protobuf.Struct.Append);

   package Value_IO is
     new PB_Support.IO.Message_IO
       (Google.Protobuf.Struct.Value, Google.Protobuf.Struct.Value_Vector,
        Google.Protobuf.Struct.Append);

   function Length (Self : Struct_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Struct_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Struct_Array, Struct_Array_Access);

   procedure Append (Self : in out Struct_Vector; V    : Struct) is
      Init_Length : constant Positive := Positive'Max (1, 256 / Struct'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Struct_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Struct_Array'
             (Self.Data.all & Struct_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Struct_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Struct_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Struct_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Struct_Variable_Reference
    (Self  : aliased in out Struct_Vector;
     Index : Positive)
      return Struct_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Struct_Variable_Reference;

   not overriding function Get_Struct_Constant_Reference
    (Self  : aliased Struct_Vector;
     Index : Positive)
      return Struct_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Struct_Constant_Reference;

   procedure Read_Struct
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Struct) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               Fields_Entry_IO.Read_Vector (Stream, Key.Encoding, V.Fields);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Struct;

   procedure Write_Struct
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Struct) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Struct (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         for J in 1 .. V.Fields.Length loop
            WS.Write_Key ((1, PB_Support.Length_Delimited));
            Google.Protobuf.Struct.Fields_Entry'Write (Stream, V.Fields (J));
         end loop;
         if WS.End_Message then
            Write_Struct (WS'Access, V);
         end if;
      end;
   end Write_Struct;

   function Length (Self : Fields_Entry_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Fields_Entry_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Fields_Entry_Array, Fields_Entry_Array_Access);

   procedure Append (Self : in out Fields_Entry_Vector; V    : Fields_Entry) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Fields_Entry'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Fields_Entry_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Fields_Entry_Array'
             (Self.Data.all & Fields_Entry_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Fields_Entry_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Fields_Entry_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Fields_Entry_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Fields_Entry_Variable_Reference
    (Self  : aliased in out Fields_Entry_Vector;
     Index : Positive)
      return Fields_Entry_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Fields_Entry_Variable_Reference;

   not overriding function Get_Fields_Entry_Constant_Reference
    (Self  : aliased Fields_Entry_Vector;
     Index : Positive)
      return Fields_Entry_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Fields_Entry_Constant_Reference;

   procedure Read_Fields_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Fields_Entry) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read (Stream, Key.Encoding, V.Key);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Value_IO.Read (Stream, Key.Encoding, V.Value.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Fields_Entry;

   procedure Write_Fields_Entry
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Fields_Entry) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Fields_Entry (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Option (1, V.Key);
         if V.Value.Is_Set then
            WS.Write_Key ((2, PB_Support.Length_Delimited));
            Google.Protobuf.Struct.Value'Write (Stream, V.Value.Value);
         end if;
         if WS.End_Message then
            Write_Fields_Entry (WS'Access, V);
         end if;
      end;
   end Write_Fields_Entry;

   function Length (Self : Value_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Value_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Value_Array, Value_Array_Access);

   procedure Append (Self : in out Value_Vector; V    : Value) is
      Init_Length : constant Positive := Positive'Max (1, 256 / Value'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Value_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Value_Array'
             (Self.Data.all & Value_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Value_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Value_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Value_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Value_Variable_Reference
    (Self  : aliased in out Value_Vector;
     Index : Positive)
      return Value_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Value_Variable_Reference;

   not overriding function Get_Value_Constant_Reference
    (Self  : aliased Value_Vector;
     Index : Positive)
      return Value_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Value_Constant_Reference;

   procedure Read_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Value) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if V.Variant.Kind /= Null_Value_Kind then
                  V.Variant := (Null_Value_Kind, others => <>);
               end if;
               Null_Value_IO.Read (Stream, Key.Encoding, V.Variant.Null_Value);
            when 2 =>
               if V.Variant.Kind /= Number_Value_Kind then
                  V.Variant := (Number_Value_Kind, others => <>);
               end if;
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Variant.Number_Value);
            when 3 =>
               if V.Variant.Kind /= String_Value_Kind then
                  V.Variant := (String_Value_Kind, others => <>);
               end if;
               PB_Support.IO.Read
                 (Stream, Key.Encoding, V.Variant.String_Value);
            when 4 =>
               if V.Variant.Kind /= Bool_Value_Kind then
                  V.Variant := (Bool_Value_Kind, others => <>);
               end if;
               PB_Support.IO.Read (Stream, Key.Encoding, V.Variant.Bool_Value);
            when 5 =>
               if V.Variant.Kind /= Struct_Value_Kind then
                  V.Variant := (Struct_Value_Kind, others => <>);
               end if;
               Struct_IO.Read (Stream, Key.Encoding, V.Variant.Struct_Value);
            when 6 =>
               if V.Variant.Kind /= List_Value_Kind then
                  V.Variant := (List_Value_Kind, others => <>);
               end if;
               List_Value_IO.Read (Stream, Key.Encoding, V.Variant.List_Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Value;

   procedure Write_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Value) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Value (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         case V.Variant.Kind is
            when Null_Value_Kind =>
               Null_Value_IO.Write (WS, 1, V.Variant.Null_Value);
            when Number_Value_Kind =>
               WS.Write (2, V.Variant.Number_Value);
            when String_Value_Kind =>
               WS.Write (3, V.Variant.String_Value);
            when Bool_Value_Kind =>
               WS.Write (4, V.Variant.Bool_Value);
            when Struct_Value_Kind =>
               WS.Write_Key ((5, PB_Support.Length_Delimited));
               Google.Protobuf.Struct.Struct'Write
                 (Stream, V.Variant.Struct_Value);
            when List_Value_Kind =>
               WS.Write_Key ((6, PB_Support.Length_Delimited));
               Google.Protobuf.Struct.List_Value'Write
                 (Stream, V.Variant.List_Value);
            when Kind_Not_Set =>
               null;
         end case;
         if WS.End_Message then
            Write_Value (WS'Access, V);
         end if;
      end;
   end Write_Value;

   function Length (Self : List_Value_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out List_Value_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (List_Value_Array, List_Value_Array_Access);

   procedure Append (Self : in out List_Value_Vector; V    : List_Value) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / List_Value'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new List_Value_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new List_Value_Array'
             (Self.Data.all & List_Value_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out List_Value_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new List_Value_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out List_Value_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_List_Value_Variable_Reference
    (Self  : aliased in out List_Value_Vector;
     Index : Positive)
      return List_Value_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_List_Value_Variable_Reference;

   not overriding function Get_List_Value_Constant_Reference
    (Self  : aliased List_Value_Vector;
     Index : Positive)
      return List_Value_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_List_Value_Constant_Reference;

   procedure Read_List_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out List_Value) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               Value_IO.Read_Vector (Stream, Key.Encoding, V.Values);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_List_Value;

   procedure Write_List_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : List_Value) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_List_Value (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         for J in 1 .. V.Values.Length loop
            WS.Write_Key ((1, PB_Support.Length_Delimited));
            Google.Protobuf.Struct.Value'Write (Stream, V.Values (J));
         end loop;
         if WS.End_Message then
            Write_List_Value (WS'Access, V);
         end if;
      end;
   end Write_List_Value;

end Google.Protobuf.Struct;