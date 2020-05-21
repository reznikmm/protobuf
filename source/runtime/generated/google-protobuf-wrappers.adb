with Ada.Unchecked_Deallocation;
with PB_Support.IO;
with PB_Support.Internal;

package body Google.Protobuf.Wrappers is

   function Length (Self : Double_Value_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Double_Value_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Double_Value_Array, Double_Value_Array_Access);

   procedure Append (Self : in out Double_Value_Vector; V    : Double_Value) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Double_Value'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Double_Value_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Double_Value_Array'
             (Self.Data.all & Double_Value_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Double_Value_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Double_Value_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Double_Value_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Double_Value_Variable_Reference
    (Self  : aliased in out Double_Value_Vector;
     Index : Positive)
      return Double_Value_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Double_Value_Variable_Reference;

   not overriding function Get_Double_Value_Constant_Reference
    (Self  : aliased Double_Value_Vector;
     Index : Positive)
      return Double_Value_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Double_Value_Constant_Reference;

   procedure Read_Double_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Double_Value) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read (Stream, Key.Encoding, V.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Double_Value;

   procedure Write_Double_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Double_Value) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Double_Value (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Option (1, V.Value, 0.0);
         if WS.End_Message then
            Write_Double_Value (WS'Access, V);
         end if;
      end;
   end Write_Double_Value;

   function Length (Self : Float_Value_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Float_Value_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Float_Value_Array, Float_Value_Array_Access);

   procedure Append (Self : in out Float_Value_Vector; V    : Float_Value) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Float_Value'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Float_Value_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Float_Value_Array'
             (Self.Data.all & Float_Value_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Float_Value_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Float_Value_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Float_Value_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Float_Value_Variable_Reference
    (Self  : aliased in out Float_Value_Vector;
     Index : Positive)
      return Float_Value_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Float_Value_Variable_Reference;

   not overriding function Get_Float_Value_Constant_Reference
    (Self  : aliased Float_Value_Vector;
     Index : Positive)
      return Float_Value_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Float_Value_Constant_Reference;

   procedure Read_Float_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Float_Value) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read (Stream, Key.Encoding, V.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Float_Value;

   procedure Write_Float_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Float_Value) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Float_Value (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Option (1, V.Value, 0.0);
         if WS.End_Message then
            Write_Float_Value (WS'Access, V);
         end if;
      end;
   end Write_Float_Value;

   function Length (Self : Int_64Value_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Int_64Value_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Int_64Value_Array, Int_64Value_Array_Access);

   procedure Append (Self : in out Int_64Value_Vector; V    : Int_64Value) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Int_64Value'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Int_64Value_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Int_64Value_Array'
             (Self.Data.all & Int_64Value_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Int_64Value_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Int_64Value_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Int_64Value_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Int_64Value_Variable_Reference
    (Self  : aliased in out Int_64Value_Vector;
     Index : Positive)
      return Int_64Value_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Int_64Value_Variable_Reference;

   not overriding function Get_Int_64Value_Constant_Reference
    (Self  : aliased Int_64Value_Vector;
     Index : Positive)
      return Int_64Value_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Int_64Value_Constant_Reference;

   procedure Read_Int_64Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Int_64Value) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Int_64Value;

   procedure Write_Int_64Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Int_64Value) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Int_64Value (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Varint_Option (1, V.Value, 0);
         if WS.End_Message then
            Write_Int_64Value (WS'Access, V);
         end if;
      end;
   end Write_Int_64Value;

   function Length (Self : UInt_64Value_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out UInt_64Value_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (UInt_64Value_Array, UInt_64Value_Array_Access);

   procedure Append (Self : in out UInt_64Value_Vector; V    : UInt_64Value) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / UInt_64Value'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new UInt_64Value_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new UInt_64Value_Array'
             (Self.Data.all & UInt_64Value_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out UInt_64Value_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new UInt_64Value_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out UInt_64Value_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_UInt_64Value_Variable_Reference
    (Self  : aliased in out UInt_64Value_Vector;
     Index : Positive)
      return UInt_64Value_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_UInt_64Value_Variable_Reference;

   not overriding function Get_UInt_64Value_Constant_Reference
    (Self  : aliased UInt_64Value_Vector;
     Index : Positive)
      return UInt_64Value_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_UInt_64Value_Constant_Reference;

   procedure Read_UInt_64Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out UInt_64Value) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_UInt_64Value;

   procedure Write_UInt_64Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : UInt_64Value) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_UInt_64Value (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Varint_Option (1, V.Value, 0);
         if WS.End_Message then
            Write_UInt_64Value (WS'Access, V);
         end if;
      end;
   end Write_UInt_64Value;

   function Length (Self : Int_32Value_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Int_32Value_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Int_32Value_Array, Int_32Value_Array_Access);

   procedure Append (Self : in out Int_32Value_Vector; V    : Int_32Value) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Int_32Value'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Int_32Value_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Int_32Value_Array'
             (Self.Data.all & Int_32Value_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Int_32Value_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Int_32Value_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Int_32Value_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Int_32Value_Variable_Reference
    (Self  : aliased in out Int_32Value_Vector;
     Index : Positive)
      return Int_32Value_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Int_32Value_Variable_Reference;

   not overriding function Get_Int_32Value_Constant_Reference
    (Self  : aliased Int_32Value_Vector;
     Index : Positive)
      return Int_32Value_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Int_32Value_Constant_Reference;

   procedure Read_Int_32Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Int_32Value) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Int_32Value;

   procedure Write_Int_32Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Int_32Value) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Int_32Value (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Varint_Option (1, V.Value, 0);
         if WS.End_Message then
            Write_Int_32Value (WS'Access, V);
         end if;
      end;
   end Write_Int_32Value;

   function Length (Self : UInt_32Value_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out UInt_32Value_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (UInt_32Value_Array, UInt_32Value_Array_Access);

   procedure Append (Self : in out UInt_32Value_Vector; V    : UInt_32Value) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / UInt_32Value'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new UInt_32Value_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new UInt_32Value_Array'
             (Self.Data.all & UInt_32Value_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out UInt_32Value_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new UInt_32Value_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out UInt_32Value_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_UInt_32Value_Variable_Reference
    (Self  : aliased in out UInt_32Value_Vector;
     Index : Positive)
      return UInt_32Value_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_UInt_32Value_Variable_Reference;

   not overriding function Get_UInt_32Value_Constant_Reference
    (Self  : aliased UInt_32Value_Vector;
     Index : Positive)
      return UInt_32Value_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_UInt_32Value_Constant_Reference;

   procedure Read_UInt_32Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out UInt_32Value) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read_Varint (Stream, Key.Encoding, V.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_UInt_32Value;

   procedure Write_UInt_32Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : UInt_32Value) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_UInt_32Value (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Varint_Option (1, V.Value, 0);
         if WS.End_Message then
            Write_UInt_32Value (WS'Access, V);
         end if;
      end;
   end Write_UInt_32Value;

   function Length (Self : Bool_Value_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Bool_Value_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Bool_Value_Array, Bool_Value_Array_Access);

   procedure Append (Self : in out Bool_Value_Vector; V    : Bool_Value) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Bool_Value'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Bool_Value_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Bool_Value_Array'
             (Self.Data.all & Bool_Value_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Bool_Value_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Bool_Value_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Bool_Value_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Bool_Value_Variable_Reference
    (Self  : aliased in out Bool_Value_Vector;
     Index : Positive)
      return Bool_Value_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Bool_Value_Variable_Reference;

   not overriding function Get_Bool_Value_Constant_Reference
    (Self  : aliased Bool_Value_Vector;
     Index : Positive)
      return Bool_Value_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Bool_Value_Constant_Reference;

   procedure Read_Bool_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Bool_Value) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read (Stream, Key.Encoding, V.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Bool_Value;

   procedure Write_Bool_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Bool_Value) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Bool_Value (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Option (1, V.Value, False);
         if WS.End_Message then
            Write_Bool_Value (WS'Access, V);
         end if;
      end;
   end Write_Bool_Value;

   function Length (Self : String_Value_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out String_Value_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (String_Value_Array, String_Value_Array_Access);

   procedure Append (Self : in out String_Value_Vector; V    : String_Value) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / String_Value'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new String_Value_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new String_Value_Array'
             (Self.Data.all & String_Value_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out String_Value_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new String_Value_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out String_Value_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_String_Value_Variable_Reference
    (Self  : aliased in out String_Value_Vector;
     Index : Positive)
      return String_Value_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_String_Value_Variable_Reference;

   not overriding function Get_String_Value_Constant_Reference
    (Self  : aliased String_Value_Vector;
     Index : Positive)
      return String_Value_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_String_Value_Constant_Reference;

   procedure Read_String_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out String_Value) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read (Stream, Key.Encoding, V.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_String_Value;

   procedure Write_String_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : String_Value) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_String_Value (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Option (1, V.Value);
         if WS.End_Message then
            Write_String_Value (WS'Access, V);
         end if;
      end;
   end Write_String_Value;

   function Length (Self : Bytes_Value_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Bytes_Value_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Bytes_Value_Array, Bytes_Value_Array_Access);

   procedure Append (Self : in out Bytes_Value_Vector; V    : Bytes_Value) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Bytes_Value'Size);
   begin
      if Self.Length = 0 then
         Self.Data :=  new Bytes_Value_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Bytes_Value_Array'
             (Self.Data.all & Bytes_Value_Array'(1 .. Self.Length => <>));
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Bytes_Value_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Bytes_Value_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Bytes_Value_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Bytes_Value_Variable_Reference
    (Self  : aliased in out Bytes_Value_Vector;
     Index : Positive)
      return Bytes_Value_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Bytes_Value_Variable_Reference;

   not overriding function Get_Bytes_Value_Constant_Reference
    (Self  : aliased Bytes_Value_Vector;
     Index : Positive)
      return Bytes_Value_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Bytes_Value_Constant_Reference;

   procedure Read_Bytes_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Bytes_Value) is
      Key : aliased PB_Support.IO.Key;
   begin
      while PB_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               PB_Support.IO.Read (Stream, Key.Encoding, V.Value);
            when others =>
               PB_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Bytes_Value;

   procedure Write_Bytes_Value
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Bytes_Value) is
   begin
      if Stream.all not in PB_Support.Internal.Stream then
         declare
            WS : aliased PB_Support.Internal.Stream (Stream);
         begin
            Write_Bytes_Value (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : PB_Support.Internal.Stream renames
           PB_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Option (1, V.Value);
         if WS.End_Message then
            Write_Bytes_Value (WS'Access, V);
         end if;
      end;
   end Write_Bytes_Value;

end Google.Protobuf.Wrappers;