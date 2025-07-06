with Ada.Unchecked_Deallocation;
with Proto_Support.IO;
with Proto_Support.Internal;

package body Google.Protobuf.Descriptor is

   package Google_Protobuf_Descriptor_Descriptor_Proto_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Descriptor_Proto,
        Google.Protobuf.Descriptor.Descriptor_Proto_Vector,
        Google.Protobuf.Descriptor.Append);

   package Google_Protobuf_Descriptor_Extension_Range_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Extension_Range,
        Google.Protobuf.Descriptor.Extension_Range_Vector,
        Google.Protobuf.Descriptor.Append);

   package Google_Protobuf_Descriptor_Reserved_Range_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Reserved_Range,
        Google.Protobuf.Descriptor.Reserved_Range_Vector,
        Google.Protobuf.Descriptor.Append);

   type Integer_Google_Protobuf_Descriptor_Edition is  range 0 .. 2147483647
     with Size => Google.Protobuf.Descriptor.Edition'Size;

   package Google_Protobuf_Descriptor_Edition_IO is
     new Proto_Support.IO.Enum_IO
       (Google.Protobuf.Descriptor.Edition,
        Integer_Google_Protobuf_Descriptor_Edition,
        Google.Protobuf.Descriptor.Edition_Vectors);

   package Google_Protobuf_Descriptor_Enum_Descriptor_Proto_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Enum_Descriptor_Proto,
        Google.Protobuf.Descriptor.Enum_Descriptor_Proto_Vector,
        Google.Protobuf.Descriptor.Append);

   package Google_Protobuf_Descriptor_Enum_Reserved_Range_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Enum_Reserved_Range,
        Google.Protobuf.Descriptor.Enum_Reserved_Range_Vector,
        Google.Protobuf.Descriptor.Append);

   package Google_Protobuf_Descriptor_Enum_Options_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Enum_Options,
        Google.Protobuf.Descriptor.Enum_Options_Vector,
        Google.Protobuf.Descriptor.Append);

   package Google_Protobuf_Descriptor_Enum_Value_Descriptor_Proto_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Enum_Value_Descriptor_Proto,
        Google.Protobuf.Descriptor.Enum_Value_Descriptor_Proto_Vector,
        Google.Protobuf.Descriptor.Append);

   package Google_Protobuf_Descriptor_Enum_Value_Options_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Enum_Value_Options,
        Google.Protobuf.Descriptor.Enum_Value_Options_Vector,
        Google.Protobuf.Descriptor.Append);

   package Google_Protobuf_Descriptor_Extension_Range_Options_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Extension_Range_Options,
        Google.Protobuf.Descriptor.Extension_Range_Options_Vector,
        Google.Protobuf.Descriptor.Append);

   package Google_Protobuf_Descriptor_Declaration_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Declaration,
        Google.Protobuf.Descriptor.Declaration_Vector,
        Google.Protobuf.Descriptor.Append);

   type Integer_Google_Protobuf_Descriptor_Verification_State is  range 0 .. 1
     with Size => Google.Protobuf.Descriptor.Verification_State'Size;

   package Google_Protobuf_Descriptor_Verification_State_IO is
     new Proto_Support.IO.Enum_IO
       (Google.Protobuf.Descriptor.Verification_State,
        Integer_Google_Protobuf_Descriptor_Verification_State,
        Google.Protobuf.Descriptor.Verification_State_Vectors);

   package Google_Protobuf_Descriptor_Feature_Set_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Feature_Set,
        Google.Protobuf.Descriptor.Feature_Set_Vector,
        Google.Protobuf.Descriptor.Append);

   type Integer_Google_Protobuf_Descriptor_Enforce_Naming_Style is
      range 0 .. 2
     with Size => Google.Protobuf.Descriptor.Enforce_Naming_Style'Size;

   package Google_Protobuf_Descriptor_Enforce_Naming_Style_IO is
     new Proto_Support.IO.Enum_IO
       (Google.Protobuf.Descriptor.Enforce_Naming_Style,
        Integer_Google_Protobuf_Descriptor_Enforce_Naming_Style,
        Google.Protobuf.Descriptor.Enforce_Naming_Style_Vectors);

   type Integer_Google_Protobuf_Descriptor_Enum_Type is  range 0 .. 2
     with Size => Google.Protobuf.Descriptor.Enum_Type'Size;

   package Google_Protobuf_Descriptor_Enum_Type_IO is
     new Proto_Support.IO.Enum_IO
       (Google.Protobuf.Descriptor.Enum_Type,
        Integer_Google_Protobuf_Descriptor_Enum_Type,
        Google.Protobuf.Descriptor.Enum_Type_Vectors);

   type Integer_Google_Protobuf_Descriptor_Field_Presence is  range 0 .. 3
     with Size => Google.Protobuf.Descriptor.Field_Presence'Size;

   package Google_Protobuf_Descriptor_Field_Presence_IO is
     new Proto_Support.IO.Enum_IO
       (Google.Protobuf.Descriptor.Field_Presence,
        Integer_Google_Protobuf_Descriptor_Field_Presence,
        Google.Protobuf.Descriptor.Field_Presence_Vectors);

   type Integer_Google_Protobuf_Descriptor_Json_Format is  range 0 .. 2
     with Size => Google.Protobuf.Descriptor.Json_Format'Size;

   package Google_Protobuf_Descriptor_Json_Format_IO is
     new Proto_Support.IO.Enum_IO
       (Google.Protobuf.Descriptor.Json_Format,
        Integer_Google_Protobuf_Descriptor_Json_Format,
        Google.Protobuf.Descriptor.Json_Format_Vectors);

   type Integer_Google_Protobuf_Descriptor_Message_Encoding is  range 0 .. 2
     with Size => Google.Protobuf.Descriptor.Message_Encoding'Size;

   package Google_Protobuf_Descriptor_Message_Encoding_IO is
     new Proto_Support.IO.Enum_IO
       (Google.Protobuf.Descriptor.Message_Encoding,
        Integer_Google_Protobuf_Descriptor_Message_Encoding,
        Google.Protobuf.Descriptor.Message_Encoding_Vectors);

   type Integer_Google_Protobuf_Descriptor_Repeated_Field_Encoding is
      range 0 .. 2
     with Size => Google.Protobuf.Descriptor.Repeated_Field_Encoding'Size;

   package Google_Protobuf_Descriptor_Repeated_Field_Encoding_IO is
     new Proto_Support.IO.Enum_IO
       (Google.Protobuf.Descriptor.Repeated_Field_Encoding,
        Integer_Google_Protobuf_Descriptor_Repeated_Field_Encoding,
        Google.Protobuf.Descriptor.Repeated_Field_Encoding_Vectors);

   type Integer_Google_Protobuf_Descriptor_Utf_8Validation is  range 0 .. 3
     with Size => Google.Protobuf.Descriptor.Utf_8Validation'Size;

   package Google_Protobuf_Descriptor_Utf_8Validation_IO is
     new Proto_Support.IO.Enum_IO
       (Google.Protobuf.Descriptor.Utf_8Validation,
        Integer_Google_Protobuf_Descriptor_Utf_8Validation,
        Google.Protobuf.Descriptor.Utf_8Validation_Vectors);

   type Integer_Google_Protobuf_Descriptor_Default_Symbol_Visibility is
      range 0 .. 4
     with Size => Google.Protobuf.Descriptor.Default_Symbol_Visibility'Size;

   package Google_Protobuf_Descriptor_Default_Symbol_Visibility_IO is
     new Proto_Support.IO.Enum_IO
       (Google.Protobuf.Descriptor.Default_Symbol_Visibility,
        Integer_Google_Protobuf_Descriptor_Default_Symbol_Visibility,
        Google.Protobuf.Descriptor.Default_Symbol_Visibility_Vectors);

   package Google_Protobuf_Descriptor_Feature_Set_Edition_Default_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Feature_Set_Edition_Default,
        Google.Protobuf.Descriptor.Feature_Set_Edition_Default_Vector,
        Google.Protobuf.Descriptor.Append);

   package Google_Protobuf_Descriptor_Field_Descriptor_Proto_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Field_Descriptor_Proto,
        Google.Protobuf.Descriptor.Field_Descriptor_Proto_Vector,
        Google.Protobuf.Descriptor.Append);

   type Integer_Google_Protobuf_Descriptor_Label is  range 1 .. 3
     with Size => Google.Protobuf.Descriptor.Label'Size;

   package Google_Protobuf_Descriptor_Label_IO is
     new Proto_Support.IO.Enum_IO
       (Google.Protobuf.Descriptor.Label,
        Integer_Google_Protobuf_Descriptor_Label,
        Google.Protobuf.Descriptor.Label_Vectors);

   type Integer_Google_Protobuf_Descriptor_Proto_Type is  range 1 .. 18
     with Size => Google.Protobuf.Descriptor.Proto_Type'Size;

   package Google_Protobuf_Descriptor_Proto_Type_IO is
     new Proto_Support.IO.Enum_IO
       (Google.Protobuf.Descriptor.Proto_Type,
        Integer_Google_Protobuf_Descriptor_Proto_Type,
        Google.Protobuf.Descriptor.Proto_Type_Vectors);

   package Google_Protobuf_Descriptor_Field_Options_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Field_Options,
        Google.Protobuf.Descriptor.Field_Options_Vector,
        Google.Protobuf.Descriptor.Append);

   type Integer_Google_Protobuf_Descriptor_CType is  range 0 .. 2
     with Size => Google.Protobuf.Descriptor.CType'Size;

   package Google_Protobuf_Descriptor_CType_IO is
     new Proto_Support.IO.Enum_IO
       (Google.Protobuf.Descriptor.CType,
        Integer_Google_Protobuf_Descriptor_CType,
        Google.Protobuf.Descriptor.CType_Vectors);

   package Google_Protobuf_Descriptor_Edition_Default_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Edition_Default,
        Google.Protobuf.Descriptor.Edition_Default_Vector,
        Google.Protobuf.Descriptor.Append);

   package Google_Protobuf_Descriptor_Feature_Support_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Feature_Support,
        Google.Protobuf.Descriptor.Feature_Support_Vector,
        Google.Protobuf.Descriptor.Append);

   type Integer_Google_Protobuf_Descriptor_JSType is  range 0 .. 2
     with Size => Google.Protobuf.Descriptor.JSType'Size;

   package Google_Protobuf_Descriptor_JSType_IO is
     new Proto_Support.IO.Enum_IO
       (Google.Protobuf.Descriptor.JSType,
        Integer_Google_Protobuf_Descriptor_JSType,
        Google.Protobuf.Descriptor.JSType_Vectors);

   type Integer_Google_Protobuf_Descriptor_Option_Retention is  range 0 .. 2
     with Size => Google.Protobuf.Descriptor.Option_Retention'Size;

   package Google_Protobuf_Descriptor_Option_Retention_IO is
     new Proto_Support.IO.Enum_IO
       (Google.Protobuf.Descriptor.Option_Retention,
        Integer_Google_Protobuf_Descriptor_Option_Retention,
        Google.Protobuf.Descriptor.Option_Retention_Vectors);

   type Integer_Google_Protobuf_Descriptor_Option_Target_Type is  range 0 .. 9
     with Size => Google.Protobuf.Descriptor.Option_Target_Type'Size;

   package Google_Protobuf_Descriptor_Option_Target_Type_IO is
     new Proto_Support.IO.Enum_IO
       (Google.Protobuf.Descriptor.Option_Target_Type,
        Integer_Google_Protobuf_Descriptor_Option_Target_Type,
        Google.Protobuf.Descriptor.Option_Target_Type_Vectors);

   package Google_Protobuf_Descriptor_File_Descriptor_Proto_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.File_Descriptor_Proto,
        Google.Protobuf.Descriptor.File_Descriptor_Proto_Vector,
        Google.Protobuf.Descriptor.Append);

   package Google_Protobuf_Descriptor_File_Options_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.File_Options,
        Google.Protobuf.Descriptor.File_Options_Vector,
        Google.Protobuf.Descriptor.Append);

   type Integer_Google_Protobuf_Descriptor_Optimize_Mode is  range 1 .. 3
     with Size => Google.Protobuf.Descriptor.Optimize_Mode'Size;

   package Google_Protobuf_Descriptor_Optimize_Mode_IO is
     new Proto_Support.IO.Enum_IO
       (Google.Protobuf.Descriptor.Optimize_Mode,
        Integer_Google_Protobuf_Descriptor_Optimize_Mode,
        Google.Protobuf.Descriptor.Optimize_Mode_Vectors);

   package Google_Protobuf_Descriptor_Annotation_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Annotation,
        Google.Protobuf.Descriptor.Annotation_Vector,
        Google.Protobuf.Descriptor.Append);

   type Integer_Google_Protobuf_Descriptor_Semantic is  range 0 .. 2
     with Size => Google.Protobuf.Descriptor.Semantic'Size;

   package Google_Protobuf_Descriptor_Semantic_IO is
     new Proto_Support.IO.Enum_IO
       (Google.Protobuf.Descriptor.Semantic,
        Integer_Google_Protobuf_Descriptor_Semantic,
        Google.Protobuf.Descriptor.Semantic_Vectors);

   package Google_Protobuf_Descriptor_Message_Options_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Message_Options,
        Google.Protobuf.Descriptor.Message_Options_Vector,
        Google.Protobuf.Descriptor.Append);

   package Google_Protobuf_Descriptor_Method_Descriptor_Proto_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Method_Descriptor_Proto,
        Google.Protobuf.Descriptor.Method_Descriptor_Proto_Vector,
        Google.Protobuf.Descriptor.Append);

   package Google_Protobuf_Descriptor_Method_Options_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Method_Options,
        Google.Protobuf.Descriptor.Method_Options_Vector,
        Google.Protobuf.Descriptor.Append);

   type Integer_Google_Protobuf_Descriptor_Idempotency_Level is  range 0 .. 2
     with Size => Google.Protobuf.Descriptor.Idempotency_Level'Size;

   package Google_Protobuf_Descriptor_Idempotency_Level_IO is
     new Proto_Support.IO.Enum_IO
       (Google.Protobuf.Descriptor.Idempotency_Level,
        Integer_Google_Protobuf_Descriptor_Idempotency_Level,
        Google.Protobuf.Descriptor.Idempotency_Level_Vectors);

   package Google_Protobuf_Descriptor_Oneof_Descriptor_Proto_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Oneof_Descriptor_Proto,
        Google.Protobuf.Descriptor.Oneof_Descriptor_Proto_Vector,
        Google.Protobuf.Descriptor.Append);

   package Google_Protobuf_Descriptor_Oneof_Options_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Oneof_Options,
        Google.Protobuf.Descriptor.Oneof_Options_Vector,
        Google.Protobuf.Descriptor.Append);

   package Google_Protobuf_Descriptor_Service_Descriptor_Proto_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Service_Descriptor_Proto,
        Google.Protobuf.Descriptor.Service_Descriptor_Proto_Vector,
        Google.Protobuf.Descriptor.Append);

   package Google_Protobuf_Descriptor_Service_Options_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Service_Options,
        Google.Protobuf.Descriptor.Service_Options_Vector,
        Google.Protobuf.Descriptor.Append);

   package Google_Protobuf_Descriptor_Source_Code_Info_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Source_Code_Info,
        Google.Protobuf.Descriptor.Source_Code_Info_Vector,
        Google.Protobuf.Descriptor.Append);

   package Google_Protobuf_Descriptor_Location_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Location,
        Google.Protobuf.Descriptor.Location_Vector,
        Google.Protobuf.Descriptor.Append);

   type Integer_Google_Protobuf_Descriptor_Symbol_Visibility is  range 0 .. 2
     with Size => Google.Protobuf.Descriptor.Symbol_Visibility'Size;

   package Google_Protobuf_Descriptor_Symbol_Visibility_IO is
     new Proto_Support.IO.Enum_IO
       (Google.Protobuf.Descriptor.Symbol_Visibility,
        Integer_Google_Protobuf_Descriptor_Symbol_Visibility,
        Google.Protobuf.Descriptor.Symbol_Visibility_Vectors);

   package Google_Protobuf_Descriptor_Uninterpreted_Option_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Uninterpreted_Option,
        Google.Protobuf.Descriptor.Uninterpreted_Option_Vector,
        Google.Protobuf.Descriptor.Append);

   package Google_Protobuf_Descriptor_Name_Part_IO is
     new Proto_Support.IO.Message_IO
       (Google.Protobuf.Descriptor.Name_Part,
        Google.Protobuf.Descriptor.Name_Part_Vector,
        Google.Protobuf.Descriptor.Append);

   function Length (Self : File_Descriptor_Set_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out File_Descriptor_Set_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (File_Descriptor_Set_Array, File_Descriptor_Set_Array_Access);

   procedure Append
    (Self : in out File_Descriptor_Set_Vector;
     V    : File_Descriptor_Set) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / File_Descriptor_Set'Size);
      Aux_Data    : File_Descriptor_Set_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new File_Descriptor_Set_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new File_Descriptor_Set_Array'
             (Self.Data.all
                & File_Descriptor_Set_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out File_Descriptor_Set_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new File_Descriptor_Set_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out File_Descriptor_Set_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_File_Descriptor_Set_Variable_Reference
    (Self  : aliased in out File_Descriptor_Set_Vector;
     Index : Positive)
      return File_Descriptor_Set_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_File_Descriptor_Set_Variable_Reference;

   not overriding function Get_File_Descriptor_Set_Constant_Reference
    (Self  : aliased File_Descriptor_Set_Vector;
     Index : Positive)
      return File_Descriptor_Set_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_File_Descriptor_Set_Constant_Reference;

   procedure Read_File_Descriptor_Set
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out File_Descriptor_Set) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               Google_Protobuf_Descriptor_File_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, V.File);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_File_Descriptor_Set;

   procedure Write_File_Descriptor_Set
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : File_Descriptor_Set) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_File_Descriptor_Set (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         for J in 1 .. V.File.Length loop
            WS.Write_Key ((1, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.File_Descriptor_Proto'Write
              (Stream, V.File (J));
         end loop;
         if WS.End_Message then
            Write_File_Descriptor_Set (WS'Access, V);
         end if;
      end;
   end Write_File_Descriptor_Set;

   function Length (Self : File_Descriptor_Proto_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out File_Descriptor_Proto_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (File_Descriptor_Proto_Array, File_Descriptor_Proto_Array_Access);

   procedure Append
    (Self : in out File_Descriptor_Proto_Vector;
     V    : File_Descriptor_Proto) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / File_Descriptor_Proto'Size);
      Aux_Data    : File_Descriptor_Proto_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new File_Descriptor_Proto_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new File_Descriptor_Proto_Array'
             (Self.Data.all
                & File_Descriptor_Proto_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out File_Descriptor_Proto_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new File_Descriptor_Proto_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out File_Descriptor_Proto_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_File_Descriptor_Proto_Variable_Reference
    (Self  : aliased in out File_Descriptor_Proto_Vector;
     Index : Positive)
      return File_Descriptor_Proto_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_File_Descriptor_Proto_Variable_Reference;

   not overriding function Get_File_Descriptor_Proto_Constant_Reference
    (Self  : aliased File_Descriptor_Proto_Vector;
     Index : Positive)
      return File_Descriptor_Proto_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_File_Descriptor_Proto_Constant_Reference;

   procedure Read_File_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out File_Descriptor_Proto) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Name.Is_Set then
                  V.Name := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Name.Value);
            when 2 =>
               if  not V.Proto_Package.Is_Set then
                  V.Proto_Package := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Proto_Package.Value);
            when 3 =>
               Proto_Support.IO.Read_Vector (Stream, Key.Encoding, V.Dependency);
            when 10 =>
               Proto_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Public_Dependency);
            when 11 =>
               Proto_Support.IO.Read_Varint_Vector
                 (Stream, Key.Encoding, V.Weak_Dependency);
            when 15 =>
               Proto_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Option_Dependency);
            when 4 =>
               Google_Protobuf_Descriptor_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, V.Message_Type);
            when 5 =>
               Google_Protobuf_Descriptor_Enum_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, V.Enum_Type);
            when 6 =>
               Google_Protobuf_Descriptor_Service_Descriptor_Proto_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Service);
            when 7 =>
               Google_Protobuf_Descriptor_Field_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, V.Extension);
            when 8 =>
               if  not V.Options.Is_Set then
                  V.Options := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_File_Options_IO.Read
                 (Stream, Key.Encoding, V.Options.Value);
            when 9 =>
               if  not V.Source_Code_Info.Is_Set then
                  V.Source_Code_Info := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Source_Code_Info_IO.Read
                 (Stream, Key.Encoding, V.Source_Code_Info.Value);
            when 12 =>
               if  not V.Syntax.Is_Set then
                  V.Syntax := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Syntax.Value);
            when 14 =>
               if  not V.Edition.Is_Set then
                  V.Edition := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Edition_IO.Read
                 (Stream, Key.Encoding, V.Edition.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_File_Descriptor_Proto;

   procedure Write_File_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : File_Descriptor_Proto) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_File_Descriptor_Proto (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Name.Is_Set then
            WS.Write (1, V.Name.Value);
         end if;
         if V.Proto_Package.Is_Set then
            WS.Write (2, V.Proto_Package.Value);
         end if;
         WS.Write (3, V.Dependency);
         WS.Write_Varint (10, V.Public_Dependency);
         WS.Write_Varint (11, V.Weak_Dependency);
         WS.Write (15, V.Option_Dependency);
         for J in 1 .. V.Message_Type.Length loop
            WS.Write_Key ((4, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Descriptor_Proto'Write
              (Stream, V.Message_Type (J));
         end loop;
         for J in 1 .. V.Enum_Type.Length loop
            WS.Write_Key ((5, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Enum_Descriptor_Proto'Write
              (Stream, V.Enum_Type (J));
         end loop;
         for J in 1 .. V.Service.Length loop
            WS.Write_Key ((6, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Service_Descriptor_Proto'Write
              (Stream, V.Service (J));
         end loop;
         for J in 1 .. V.Extension.Length loop
            WS.Write_Key ((7, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Field_Descriptor_Proto'Write
              (Stream, V.Extension (J));
         end loop;
         if V.Options.Is_Set then
            WS.Write_Key ((8, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.File_Options'Write
              (Stream, V.Options.Value);
         end if;
         if V.Source_Code_Info.Is_Set then
            WS.Write_Key ((9, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Source_Code_Info'Write
              (Stream, V.Source_Code_Info.Value);
         end if;
         if V.Syntax.Is_Set then
            WS.Write (12, V.Syntax.Value);
         end if;
         if V.Edition.Is_Set then
            Google_Protobuf_Descriptor_Edition_IO.Write
              (WS, 14, V.Edition.Value);
         end if;
         if WS.End_Message then
            Write_File_Descriptor_Proto (WS'Access, V);
         end if;
      end;
   end Write_File_Descriptor_Proto;

   function Length (Self : Descriptor_Proto_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Descriptor_Proto_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Descriptor_Proto_Array, Descriptor_Proto_Array_Access);

   procedure Append
    (Self : in out Descriptor_Proto_Vector;
     V    : Descriptor_Proto) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Descriptor_Proto'Size);
      Aux_Data    : Descriptor_Proto_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Descriptor_Proto_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Descriptor_Proto_Array'
             (Self.Data.all & Descriptor_Proto_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Descriptor_Proto_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Descriptor_Proto_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Descriptor_Proto_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Descriptor_Proto_Variable_Reference
    (Self  : aliased in out Descriptor_Proto_Vector;
     Index : Positive)
      return Descriptor_Proto_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Descriptor_Proto_Variable_Reference;

   not overriding function Get_Descriptor_Proto_Constant_Reference
    (Self  : aliased Descriptor_Proto_Vector;
     Index : Positive)
      return Descriptor_Proto_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Descriptor_Proto_Constant_Reference;

   procedure Read_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Descriptor_Proto) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Name.Is_Set then
                  V.Name := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Name.Value);
            when 2 =>
               Google_Protobuf_Descriptor_Field_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, V.Field);
            when 6 =>
               Google_Protobuf_Descriptor_Field_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, V.Extension);
            when 3 =>
               Google_Protobuf_Descriptor_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, V.Nested_Type);
            when 4 =>
               Google_Protobuf_Descriptor_Enum_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, V.Enum_Type);
            when 5 =>
               Google_Protobuf_Descriptor_Extension_Range_IO.Read_Vector
                 (Stream, Key.Encoding, V.Extension_Range);
            when 8 =>
               Google_Protobuf_Descriptor_Oneof_Descriptor_Proto_IO.Read_Vector
                 (Stream, Key.Encoding, V.Oneof_Decl);
            when 7 =>
               if  not V.Options.Is_Set then
                  V.Options := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Message_Options_IO.Read
                 (Stream, Key.Encoding, V.Options.Value);
            when 9 =>
               Google_Protobuf_Descriptor_Reserved_Range_IO.Read_Vector
                 (Stream, Key.Encoding, V.Reserved_Range);
            when 10 =>
               Proto_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Reserved_Name);
            when 11 =>
               if  not V.Visibility.Is_Set then
                  V.Visibility := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Symbol_Visibility_IO.Read
                 (Stream, Key.Encoding, V.Visibility.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Descriptor_Proto;

   procedure Write_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Descriptor_Proto) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Descriptor_Proto (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Name.Is_Set then
            WS.Write (1, V.Name.Value);
         end if;
         for J in 1 .. V.Field.Length loop
            WS.Write_Key ((2, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Field_Descriptor_Proto'Write
              (Stream, V.Field (J));
         end loop;
         for J in 1 .. V.Extension.Length loop
            WS.Write_Key ((6, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Field_Descriptor_Proto'Write
              (Stream, V.Extension (J));
         end loop;
         for J in 1 .. V.Nested_Type.Length loop
            WS.Write_Key ((3, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Descriptor_Proto'Write
              (Stream, V.Nested_Type (J));
         end loop;
         for J in 1 .. V.Enum_Type.Length loop
            WS.Write_Key ((4, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Enum_Descriptor_Proto'Write
              (Stream, V.Enum_Type (J));
         end loop;
         for J in 1 .. V.Extension_Range.Length loop
            WS.Write_Key ((5, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Extension_Range'Write
              (Stream, V.Extension_Range (J));
         end loop;
         for J in 1 .. V.Oneof_Decl.Length loop
            WS.Write_Key ((8, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Oneof_Descriptor_Proto'Write
              (Stream, V.Oneof_Decl (J));
         end loop;
         if V.Options.Is_Set then
            WS.Write_Key ((7, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Message_Options'Write
              (Stream, V.Options.Value);
         end if;
         for J in 1 .. V.Reserved_Range.Length loop
            WS.Write_Key ((9, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Reserved_Range'Write
              (Stream, V.Reserved_Range (J));
         end loop;
         WS.Write (10, V.Reserved_Name);
         if V.Visibility.Is_Set then
            Google_Protobuf_Descriptor_Symbol_Visibility_IO.Write
              (WS, 11, V.Visibility.Value);
         end if;
         if WS.End_Message then
            Write_Descriptor_Proto (WS'Access, V);
         end if;
      end;
   end Write_Descriptor_Proto;

   function Length (Self : Extension_Range_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Extension_Range_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Extension_Range_Array, Extension_Range_Array_Access);

   procedure Append
    (Self : in out Extension_Range_Vector;
     V    : Extension_Range) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Extension_Range'Size);
      Aux_Data    : Extension_Range_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Extension_Range_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Extension_Range_Array'
             (Self.Data.all & Extension_Range_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Extension_Range_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Extension_Range_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Extension_Range_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Extension_Range_Variable_Reference
    (Self  : aliased in out Extension_Range_Vector;
     Index : Positive)
      return Extension_Range_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Extension_Range_Variable_Reference;

   not overriding function Get_Extension_Range_Constant_Reference
    (Self  : aliased Extension_Range_Vector;
     Index : Positive)
      return Extension_Range_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Extension_Range_Constant_Reference;

   procedure Read_Extension_Range
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Extension_Range) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Start.Is_Set then
                  V.Start := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint (Stream, Key.Encoding, V.Start.Value);
            when 2 =>
               if  not V.Proto_End.Is_Set then
                  V.Proto_End := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Proto_End.Value);
            when 3 =>
               if  not V.Options.Is_Set then
                  V.Options := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Extension_Range_Options_IO.Read
                 (Stream, Key.Encoding, V.Options.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Extension_Range;

   procedure Write_Extension_Range
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Extension_Range) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Extension_Range (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Start.Is_Set then
            WS.Write_Varint (1, V.Start.Value);
         end if;
         if V.Proto_End.Is_Set then
            WS.Write_Varint (2, V.Proto_End.Value);
         end if;
         if V.Options.Is_Set then
            WS.Write_Key ((3, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Extension_Range_Options'Write
              (Stream, V.Options.Value);
         end if;
         if WS.End_Message then
            Write_Extension_Range (WS'Access, V);
         end if;
      end;
   end Write_Extension_Range;

   function Length (Self : Reserved_Range_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Reserved_Range_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Reserved_Range_Array, Reserved_Range_Array_Access);

   procedure Append
    (Self : in out Reserved_Range_Vector;
     V    : Reserved_Range) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Reserved_Range'Size);
      Aux_Data    : Reserved_Range_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Reserved_Range_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Reserved_Range_Array'
             (Self.Data.all & Reserved_Range_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Reserved_Range_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Reserved_Range_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Reserved_Range_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Reserved_Range_Variable_Reference
    (Self  : aliased in out Reserved_Range_Vector;
     Index : Positive)
      return Reserved_Range_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Reserved_Range_Variable_Reference;

   not overriding function Get_Reserved_Range_Constant_Reference
    (Self  : aliased Reserved_Range_Vector;
     Index : Positive)
      return Reserved_Range_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Reserved_Range_Constant_Reference;

   procedure Read_Reserved_Range
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Reserved_Range) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Start.Is_Set then
                  V.Start := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint (Stream, Key.Encoding, V.Start.Value);
            when 2 =>
               if  not V.Proto_End.Is_Set then
                  V.Proto_End := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Proto_End.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Reserved_Range;

   procedure Write_Reserved_Range
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Reserved_Range) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Reserved_Range (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Start.Is_Set then
            WS.Write_Varint (1, V.Start.Value);
         end if;
         if V.Proto_End.Is_Set then
            WS.Write_Varint (2, V.Proto_End.Value);
         end if;
         if WS.End_Message then
            Write_Reserved_Range (WS'Access, V);
         end if;
      end;
   end Write_Reserved_Range;

   function Length (Self : Extension_Range_Options_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Extension_Range_Options_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Extension_Range_Options_Array, Extension_Range_Options_Array_Access);

   procedure Append
    (Self : in out Extension_Range_Options_Vector;
     V    : Extension_Range_Options) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Extension_Range_Options'Size);
      Aux_Data    : Extension_Range_Options_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Extension_Range_Options_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Extension_Range_Options_Array'
             (Self.Data.all
                & Extension_Range_Options_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Extension_Range_Options_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Extension_Range_Options_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Extension_Range_Options_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Extension_Range_Options_Variable_Reference
    (Self  : aliased in out Extension_Range_Options_Vector;
     Index : Positive)
      return Extension_Range_Options_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Extension_Range_Options_Variable_Reference;

   not overriding function Get_Extension_Range_Options_Constant_Reference
    (Self  : aliased Extension_Range_Options_Vector;
     Index : Positive)
      return Extension_Range_Options_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Extension_Range_Options_Constant_Reference;

   procedure Read_Extension_Range_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Extension_Range_Options) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 999 =>
               Google_Protobuf_Descriptor_Uninterpreted_Option_IO.Read_Vector
                 (Stream, Key.Encoding, V.Uninterpreted_Option);
            when 2 =>
               Google_Protobuf_Descriptor_Declaration_IO.Read_Vector
                 (Stream, Key.Encoding, V.Declaration);
            when 50 =>
               if  not V.Features.Is_Set then
                  V.Features := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Feature_Set_IO.Read
                 (Stream, Key.Encoding, V.Features.Value);
            when 3 =>
               if  not V.Verification.Is_Set then
                  V.Verification := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Verification_State_IO.Read
                 (Stream, Key.Encoding, V.Verification.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Extension_Range_Options;

   procedure Write_Extension_Range_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Extension_Range_Options) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Extension_Range_Options (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         for J in 1 .. V.Uninterpreted_Option.Length loop
            WS.Write_Key ((999, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Uninterpreted_Option'Write
              (Stream, V.Uninterpreted_Option (J));
         end loop;
         for J in 1 .. V.Declaration.Length loop
            WS.Write_Key ((2, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Declaration'Write
              (Stream, V.Declaration (J));
         end loop;
         if V.Features.Is_Set then
            WS.Write_Key ((50, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Feature_Set'Write
              (Stream, V.Features.Value);
         end if;
         if V.Verification.Is_Set then
            Google_Protobuf_Descriptor_Verification_State_IO.Write
              (WS, 3, V.Verification.Value);
         end if;
         if WS.End_Message then
            Write_Extension_Range_Options (WS'Access, V);
         end if;
      end;
   end Write_Extension_Range_Options;

   function Length (Self : Declaration_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Declaration_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Declaration_Array, Declaration_Array_Access);

   procedure Append (Self : in out Declaration_Vector; V    : Declaration) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Declaration'Size);
      Aux_Data    : Declaration_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Declaration_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Declaration_Array'
             (Self.Data.all & Declaration_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Declaration_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Declaration_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Declaration_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Declaration_Variable_Reference
    (Self  : aliased in out Declaration_Vector;
     Index : Positive)
      return Declaration_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Declaration_Variable_Reference;

   not overriding function Get_Declaration_Constant_Reference
    (Self  : aliased Declaration_Vector;
     Index : Positive)
      return Declaration_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Declaration_Constant_Reference;

   procedure Read_Declaration
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Declaration) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Number.Is_Set then
                  V.Number := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Number.Value);
            when 2 =>
               if  not V.Full_Name.Is_Set then
                  V.Full_Name := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Full_Name.Value);
            when 3 =>
               if  not V.Proto_Type.Is_Set then
                  V.Proto_Type := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Proto_Type.Value);
            when 5 =>
               if  not V.Reserved.Is_Set then
                  V.Reserved := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Reserved.Value);
            when 6 =>
               if  not V.Repeated.Is_Set then
                  V.Repeated := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Repeated.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Declaration;

   procedure Write_Declaration
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Declaration) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Declaration (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Number.Is_Set then
            WS.Write_Varint (1, V.Number.Value);
         end if;
         if V.Full_Name.Is_Set then
            WS.Write (2, V.Full_Name.Value);
         end if;
         if V.Proto_Type.Is_Set then
            WS.Write (3, V.Proto_Type.Value);
         end if;
         if V.Reserved.Is_Set then
            WS.Write (5, V.Reserved.Value);
         end if;
         if V.Repeated.Is_Set then
            WS.Write (6, V.Repeated.Value);
         end if;
         if WS.End_Message then
            Write_Declaration (WS'Access, V);
         end if;
      end;
   end Write_Declaration;

   function Length (Self : Field_Descriptor_Proto_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Field_Descriptor_Proto_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Field_Descriptor_Proto_Array, Field_Descriptor_Proto_Array_Access);

   procedure Append
    (Self : in out Field_Descriptor_Proto_Vector;
     V    : Field_Descriptor_Proto) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Field_Descriptor_Proto'Size);
      Aux_Data    : Field_Descriptor_Proto_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Field_Descriptor_Proto_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Field_Descriptor_Proto_Array'
             (Self.Data.all
                & Field_Descriptor_Proto_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Field_Descriptor_Proto_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Field_Descriptor_Proto_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Field_Descriptor_Proto_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Field_Descriptor_Proto_Variable_Reference
    (Self  : aliased in out Field_Descriptor_Proto_Vector;
     Index : Positive)
      return Field_Descriptor_Proto_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Field_Descriptor_Proto_Variable_Reference;

   not overriding function Get_Field_Descriptor_Proto_Constant_Reference
    (Self  : aliased Field_Descriptor_Proto_Vector;
     Index : Positive)
      return Field_Descriptor_Proto_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Field_Descriptor_Proto_Constant_Reference;

   procedure Read_Field_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Field_Descriptor_Proto) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Name.Is_Set then
                  V.Name := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Name.Value);
            when 3 =>
               if  not V.Number.Is_Set then
                  V.Number := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Number.Value);
            when 4 =>
               if  not V.Label.Is_Set then
                  V.Label := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Label_IO.Read
                 (Stream, Key.Encoding, V.Label.Value);
            when 5 =>
               if  not V.Proto_Type.Is_Set then
                  V.Proto_Type := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Proto_Type_IO.Read
                 (Stream, Key.Encoding, V.Proto_Type.Value);
            when 6 =>
               if  not V.Type_Name.Is_Set then
                  V.Type_Name := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Type_Name.Value);
            when 2 =>
               if  not V.Extendee.Is_Set then
                  V.Extendee := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Extendee.Value);
            when 7 =>
               if  not V.Default_Value.Is_Set then
                  V.Default_Value := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Default_Value.Value);
            when 9 =>
               if  not V.Oneof_Index.Is_Set then
                  V.Oneof_Index := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Oneof_Index.Value);
            when 10 =>
               if  not V.Json_Name.Is_Set then
                  V.Json_Name := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Json_Name.Value);
            when 8 =>
               if  not V.Options.Is_Set then
                  V.Options := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Field_Options_IO.Read
                 (Stream, Key.Encoding, V.Options.Value);
            when 17 =>
               if  not V.Proto_3_Optional.Is_Set then
                  V.Proto_3_Optional := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Proto_3_Optional.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Field_Descriptor_Proto;

   procedure Write_Field_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Field_Descriptor_Proto) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Field_Descriptor_Proto (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Name.Is_Set then
            WS.Write (1, V.Name.Value);
         end if;
         if V.Number.Is_Set then
            WS.Write_Varint (3, V.Number.Value);
         end if;
         if V.Label.Is_Set then
            Google_Protobuf_Descriptor_Label_IO.Write (WS, 4, V.Label.Value);
         end if;
         if V.Proto_Type.Is_Set then
            Google_Protobuf_Descriptor_Proto_Type_IO.Write
              (WS, 5, V.Proto_Type.Value);
         end if;
         if V.Type_Name.Is_Set then
            WS.Write (6, V.Type_Name.Value);
         end if;
         if V.Extendee.Is_Set then
            WS.Write (2, V.Extendee.Value);
         end if;
         if V.Default_Value.Is_Set then
            WS.Write (7, V.Default_Value.Value);
         end if;
         if V.Oneof_Index.Is_Set then
            WS.Write_Varint (9, V.Oneof_Index.Value);
         end if;
         if V.Json_Name.Is_Set then
            WS.Write (10, V.Json_Name.Value);
         end if;
         if V.Options.Is_Set then
            WS.Write_Key ((8, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Field_Options'Write
              (Stream, V.Options.Value);
         end if;
         if V.Proto_3_Optional.Is_Set then
            WS.Write (17, V.Proto_3_Optional.Value);
         end if;
         if WS.End_Message then
            Write_Field_Descriptor_Proto (WS'Access, V);
         end if;
      end;
   end Write_Field_Descriptor_Proto;

   function Length (Self : Oneof_Descriptor_Proto_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Oneof_Descriptor_Proto_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Oneof_Descriptor_Proto_Array, Oneof_Descriptor_Proto_Array_Access);

   procedure Append
    (Self : in out Oneof_Descriptor_Proto_Vector;
     V    : Oneof_Descriptor_Proto) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Oneof_Descriptor_Proto'Size);
      Aux_Data    : Oneof_Descriptor_Proto_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Oneof_Descriptor_Proto_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Oneof_Descriptor_Proto_Array'
             (Self.Data.all
                & Oneof_Descriptor_Proto_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Oneof_Descriptor_Proto_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Oneof_Descriptor_Proto_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Oneof_Descriptor_Proto_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Oneof_Descriptor_Proto_Variable_Reference
    (Self  : aliased in out Oneof_Descriptor_Proto_Vector;
     Index : Positive)
      return Oneof_Descriptor_Proto_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Oneof_Descriptor_Proto_Variable_Reference;

   not overriding function Get_Oneof_Descriptor_Proto_Constant_Reference
    (Self  : aliased Oneof_Descriptor_Proto_Vector;
     Index : Positive)
      return Oneof_Descriptor_Proto_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Oneof_Descriptor_Proto_Constant_Reference;

   procedure Read_Oneof_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Oneof_Descriptor_Proto) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Name.Is_Set then
                  V.Name := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Name.Value);
            when 2 =>
               if  not V.Options.Is_Set then
                  V.Options := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Oneof_Options_IO.Read
                 (Stream, Key.Encoding, V.Options.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Oneof_Descriptor_Proto;

   procedure Write_Oneof_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Oneof_Descriptor_Proto) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Oneof_Descriptor_Proto (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Name.Is_Set then
            WS.Write (1, V.Name.Value);
         end if;
         if V.Options.Is_Set then
            WS.Write_Key ((2, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Oneof_Options'Write
              (Stream, V.Options.Value);
         end if;
         if WS.End_Message then
            Write_Oneof_Descriptor_Proto (WS'Access, V);
         end if;
      end;
   end Write_Oneof_Descriptor_Proto;

   function Length (Self : Enum_Descriptor_Proto_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Enum_Descriptor_Proto_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Enum_Descriptor_Proto_Array, Enum_Descriptor_Proto_Array_Access);

   procedure Append
    (Self : in out Enum_Descriptor_Proto_Vector;
     V    : Enum_Descriptor_Proto) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Enum_Descriptor_Proto'Size);
      Aux_Data    : Enum_Descriptor_Proto_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Enum_Descriptor_Proto_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Enum_Descriptor_Proto_Array'
             (Self.Data.all
                & Enum_Descriptor_Proto_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Enum_Descriptor_Proto_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Enum_Descriptor_Proto_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Enum_Descriptor_Proto_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Enum_Descriptor_Proto_Variable_Reference
    (Self  : aliased in out Enum_Descriptor_Proto_Vector;
     Index : Positive)
      return Enum_Descriptor_Proto_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Enum_Descriptor_Proto_Variable_Reference;

   not overriding function Get_Enum_Descriptor_Proto_Constant_Reference
    (Self  : aliased Enum_Descriptor_Proto_Vector;
     Index : Positive)
      return Enum_Descriptor_Proto_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Enum_Descriptor_Proto_Constant_Reference;

   procedure Read_Enum_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Enum_Descriptor_Proto) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Name.Is_Set then
                  V.Name := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Name.Value);
            when 2 =>
               Google_Protobuf_Descriptor_Enum_Value_Descriptor_Proto_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Value);
            when 3 =>
               if  not V.Options.Is_Set then
                  V.Options := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Enum_Options_IO.Read
                 (Stream, Key.Encoding, V.Options.Value);
            when 4 =>
               Google_Protobuf_Descriptor_Enum_Reserved_Range_IO.Read_Vector
                 (Stream, Key.Encoding, V.Reserved_Range);
            when 5 =>
               Proto_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Reserved_Name);
            when 6 =>
               if  not V.Visibility.Is_Set then
                  V.Visibility := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Symbol_Visibility_IO.Read
                 (Stream, Key.Encoding, V.Visibility.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Enum_Descriptor_Proto;

   procedure Write_Enum_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Enum_Descriptor_Proto) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Enum_Descriptor_Proto (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Name.Is_Set then
            WS.Write (1, V.Name.Value);
         end if;
         for J in 1 .. V.Value.Length loop
            WS.Write_Key ((2, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Enum_Value_Descriptor_Proto'Write
              (Stream, V.Value (J));
         end loop;
         if V.Options.Is_Set then
            WS.Write_Key ((3, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Enum_Options'Write
              (Stream, V.Options.Value);
         end if;
         for J in 1 .. V.Reserved_Range.Length loop
            WS.Write_Key ((4, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Enum_Reserved_Range'Write
              (Stream, V.Reserved_Range (J));
         end loop;
         WS.Write (5, V.Reserved_Name);
         if V.Visibility.Is_Set then
            Google_Protobuf_Descriptor_Symbol_Visibility_IO.Write
              (WS, 6, V.Visibility.Value);
         end if;
         if WS.End_Message then
            Write_Enum_Descriptor_Proto (WS'Access, V);
         end if;
      end;
   end Write_Enum_Descriptor_Proto;

   function Length (Self : Enum_Reserved_Range_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Enum_Reserved_Range_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Enum_Reserved_Range_Array, Enum_Reserved_Range_Array_Access);

   procedure Append
    (Self : in out Enum_Reserved_Range_Vector;
     V    : Enum_Reserved_Range) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Enum_Reserved_Range'Size);
      Aux_Data    : Enum_Reserved_Range_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Enum_Reserved_Range_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Enum_Reserved_Range_Array'
             (Self.Data.all
                & Enum_Reserved_Range_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Enum_Reserved_Range_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Enum_Reserved_Range_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Enum_Reserved_Range_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Enum_Reserved_Range_Variable_Reference
    (Self  : aliased in out Enum_Reserved_Range_Vector;
     Index : Positive)
      return Enum_Reserved_Range_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Enum_Reserved_Range_Variable_Reference;

   not overriding function Get_Enum_Reserved_Range_Constant_Reference
    (Self  : aliased Enum_Reserved_Range_Vector;
     Index : Positive)
      return Enum_Reserved_Range_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Enum_Reserved_Range_Constant_Reference;

   procedure Read_Enum_Reserved_Range
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Enum_Reserved_Range) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Start.Is_Set then
                  V.Start := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint (Stream, Key.Encoding, V.Start.Value);
            when 2 =>
               if  not V.Proto_End.Is_Set then
                  V.Proto_End := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Proto_End.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Enum_Reserved_Range;

   procedure Write_Enum_Reserved_Range
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Enum_Reserved_Range) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Enum_Reserved_Range (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Start.Is_Set then
            WS.Write_Varint (1, V.Start.Value);
         end if;
         if V.Proto_End.Is_Set then
            WS.Write_Varint (2, V.Proto_End.Value);
         end if;
         if WS.End_Message then
            Write_Enum_Reserved_Range (WS'Access, V);
         end if;
      end;
   end Write_Enum_Reserved_Range;

   function Length
    (Self : Enum_Value_Descriptor_Proto_Vector)
      return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Enum_Value_Descriptor_Proto_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Enum_Value_Descriptor_Proto_Array,
      Enum_Value_Descriptor_Proto_Array_Access);

   procedure Append
    (Self : in out Enum_Value_Descriptor_Proto_Vector;
     V    : Enum_Value_Descriptor_Proto) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Enum_Value_Descriptor_Proto'Size);
      Aux_Data    : Enum_Value_Descriptor_Proto_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=
            new Enum_Value_Descriptor_Proto_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Enum_Value_Descriptor_Proto_Array'
             (Self.Data.all
                & Enum_Value_Descriptor_Proto_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Enum_Value_Descriptor_Proto_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Enum_Value_Descriptor_Proto_Array'
             (Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Enum_Value_Descriptor_Proto_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Enum_Value_Descriptor_Proto_Variable_Reference
    (Self  : aliased in out Enum_Value_Descriptor_Proto_Vector;
     Index : Positive)
      return Enum_Value_Descriptor_Proto_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Enum_Value_Descriptor_Proto_Variable_Reference;

   not overriding function Get_Enum_Value_Descriptor_Proto_Constant_Reference
    (Self  : aliased Enum_Value_Descriptor_Proto_Vector;
     Index : Positive)
      return Enum_Value_Descriptor_Proto_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Enum_Value_Descriptor_Proto_Constant_Reference;

   procedure Read_Enum_Value_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Enum_Value_Descriptor_Proto) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Name.Is_Set then
                  V.Name := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Name.Value);
            when 2 =>
               if  not V.Number.Is_Set then
                  V.Number := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Number.Value);
            when 3 =>
               if  not V.Options.Is_Set then
                  V.Options := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Enum_Value_Options_IO.Read
                 (Stream, Key.Encoding, V.Options.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Enum_Value_Descriptor_Proto;

   procedure Write_Enum_Value_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Enum_Value_Descriptor_Proto) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Enum_Value_Descriptor_Proto (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Name.Is_Set then
            WS.Write (1, V.Name.Value);
         end if;
         if V.Number.Is_Set then
            WS.Write_Varint (2, V.Number.Value);
         end if;
         if V.Options.Is_Set then
            WS.Write_Key ((3, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Enum_Value_Options'Write
              (Stream, V.Options.Value);
         end if;
         if WS.End_Message then
            Write_Enum_Value_Descriptor_Proto (WS'Access, V);
         end if;
      end;
   end Write_Enum_Value_Descriptor_Proto;

   function Length (Self : Service_Descriptor_Proto_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Service_Descriptor_Proto_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Service_Descriptor_Proto_Array, Service_Descriptor_Proto_Array_Access);

   procedure Append
    (Self : in out Service_Descriptor_Proto_Vector;
     V    : Service_Descriptor_Proto) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Service_Descriptor_Proto'Size);
      Aux_Data    : Service_Descriptor_Proto_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Service_Descriptor_Proto_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Service_Descriptor_Proto_Array'
             (Self.Data.all
                & Service_Descriptor_Proto_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Service_Descriptor_Proto_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Service_Descriptor_Proto_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Service_Descriptor_Proto_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Service_Descriptor_Proto_Variable_Reference
    (Self  : aliased in out Service_Descriptor_Proto_Vector;
     Index : Positive)
      return Service_Descriptor_Proto_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Service_Descriptor_Proto_Variable_Reference;

   not overriding function Get_Service_Descriptor_Proto_Constant_Reference
    (Self  : aliased Service_Descriptor_Proto_Vector;
     Index : Positive)
      return Service_Descriptor_Proto_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Service_Descriptor_Proto_Constant_Reference;

   procedure Read_Service_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Service_Descriptor_Proto) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Name.Is_Set then
                  V.Name := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Name.Value);
            when 2 =>
               Google_Protobuf_Descriptor_Method_Descriptor_Proto_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Method);
            when 3 =>
               if  not V.Options.Is_Set then
                  V.Options := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Service_Options_IO.Read
                 (Stream, Key.Encoding, V.Options.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Service_Descriptor_Proto;

   procedure Write_Service_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Service_Descriptor_Proto) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Service_Descriptor_Proto (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Name.Is_Set then
            WS.Write (1, V.Name.Value);
         end if;
         for J in 1 .. V.Method.Length loop
            WS.Write_Key ((2, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Method_Descriptor_Proto'Write
              (Stream, V.Method (J));
         end loop;
         if V.Options.Is_Set then
            WS.Write_Key ((3, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Service_Options'Write
              (Stream, V.Options.Value);
         end if;
         if WS.End_Message then
            Write_Service_Descriptor_Proto (WS'Access, V);
         end if;
      end;
   end Write_Service_Descriptor_Proto;

   function Length (Self : Method_Descriptor_Proto_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Method_Descriptor_Proto_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Method_Descriptor_Proto_Array, Method_Descriptor_Proto_Array_Access);

   procedure Append
    (Self : in out Method_Descriptor_Proto_Vector;
     V    : Method_Descriptor_Proto) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Method_Descriptor_Proto'Size);
      Aux_Data    : Method_Descriptor_Proto_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Method_Descriptor_Proto_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Method_Descriptor_Proto_Array'
             (Self.Data.all
                & Method_Descriptor_Proto_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Method_Descriptor_Proto_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Method_Descriptor_Proto_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Method_Descriptor_Proto_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Method_Descriptor_Proto_Variable_Reference
    (Self  : aliased in out Method_Descriptor_Proto_Vector;
     Index : Positive)
      return Method_Descriptor_Proto_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Method_Descriptor_Proto_Variable_Reference;

   not overriding function Get_Method_Descriptor_Proto_Constant_Reference
    (Self  : aliased Method_Descriptor_Proto_Vector;
     Index : Positive)
      return Method_Descriptor_Proto_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Method_Descriptor_Proto_Constant_Reference;

   procedure Read_Method_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Method_Descriptor_Proto) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Name.Is_Set then
                  V.Name := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Name.Value);
            when 2 =>
               if  not V.Input_Type.Is_Set then
                  V.Input_Type := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Input_Type.Value);
            when 3 =>
               if  not V.Output_Type.Is_Set then
                  V.Output_Type := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Output_Type.Value);
            when 4 =>
               if  not V.Options.Is_Set then
                  V.Options := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Method_Options_IO.Read
                 (Stream, Key.Encoding, V.Options.Value);
            when 5 =>
               if  not V.Client_Streaming.Is_Set then
                  V.Client_Streaming := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Client_Streaming.Value);
            when 6 =>
               if  not V.Server_Streaming.Is_Set then
                  V.Server_Streaming := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Server_Streaming.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Method_Descriptor_Proto;

   procedure Write_Method_Descriptor_Proto
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Method_Descriptor_Proto) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Method_Descriptor_Proto (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Name.Is_Set then
            WS.Write (1, V.Name.Value);
         end if;
         if V.Input_Type.Is_Set then
            WS.Write (2, V.Input_Type.Value);
         end if;
         if V.Output_Type.Is_Set then
            WS.Write (3, V.Output_Type.Value);
         end if;
         if V.Options.Is_Set then
            WS.Write_Key ((4, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Method_Options'Write
              (Stream, V.Options.Value);
         end if;
         if V.Client_Streaming.Is_Set then
            WS.Write (5, V.Client_Streaming.Value);
         end if;
         if V.Server_Streaming.Is_Set then
            WS.Write (6, V.Server_Streaming.Value);
         end if;
         if WS.End_Message then
            Write_Method_Descriptor_Proto (WS'Access, V);
         end if;
      end;
   end Write_Method_Descriptor_Proto;

   function Length (Self : File_Options_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out File_Options_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (File_Options_Array, File_Options_Array_Access);

   procedure Append (Self : in out File_Options_Vector; V    : File_Options) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / File_Options'Size);
      Aux_Data    : File_Options_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new File_Options_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new File_Options_Array'
             (Self.Data.all & File_Options_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out File_Options_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new File_Options_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out File_Options_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_File_Options_Variable_Reference
    (Self  : aliased in out File_Options_Vector;
     Index : Positive)
      return File_Options_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_File_Options_Variable_Reference;

   not overriding function Get_File_Options_Constant_Reference
    (Self  : aliased File_Options_Vector;
     Index : Positive)
      return File_Options_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_File_Options_Constant_Reference;

   procedure Read_File_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out File_Options) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Java_Package.Is_Set then
                  V.Java_Package := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Java_Package.Value);
            when 8 =>
               if  not V.Java_Outer_Classname.Is_Set then
                  V.Java_Outer_Classname := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Java_Outer_Classname.Value);
            when 10 =>
               if  not V.Java_Multiple_Files.Is_Set then
                  V.Java_Multiple_Files := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Java_Multiple_Files.Value);
            when 20 =>
               if  not V.Java_Generate_Equals_And_Hash.Is_Set then
                  V.Java_Generate_Equals_And_Hash := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Java_Generate_Equals_And_Hash.Value);
            when 27 =>
               if  not V.Java_String_Check_Utf_8.Is_Set then
                  V.Java_String_Check_Utf_8 := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Java_String_Check_Utf_8.Value);
            when 9 =>
               if  not V.Optimize_For.Is_Set then
                  V.Optimize_For := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Optimize_Mode_IO.Read
                 (Stream, Key.Encoding, V.Optimize_For.Value);
            when 11 =>
               if  not V.Go_Package.Is_Set then
                  V.Go_Package := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Go_Package.Value);
            when 16 =>
               if  not V.Cc_Generic_Services.Is_Set then
                  V.Cc_Generic_Services := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Cc_Generic_Services.Value);
            when 17 =>
               if  not V.Java_Generic_Services.Is_Set then
                  V.Java_Generic_Services := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Java_Generic_Services.Value);
            when 18 =>
               if  not V.Py_Generic_Services.Is_Set then
                  V.Py_Generic_Services := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Py_Generic_Services.Value);
            when 23 =>
               if  not V.Deprecated.Is_Set then
                  V.Deprecated := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Deprecated.Value);
            when 31 =>
               if  not V.Cc_Enable_Arenas.Is_Set then
                  V.Cc_Enable_Arenas := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Cc_Enable_Arenas.Value);
            when 36 =>
               if  not V.Objc_Class_Prefix.Is_Set then
                  V.Objc_Class_Prefix := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Objc_Class_Prefix.Value);
            when 37 =>
               if  not V.Csharp_Namespace.Is_Set then
                  V.Csharp_Namespace := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Csharp_Namespace.Value);
            when 39 =>
               if  not V.Swift_Prefix.Is_Set then
                  V.Swift_Prefix := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Swift_Prefix.Value);
            when 40 =>
               if  not V.Php_Class_Prefix.Is_Set then
                  V.Php_Class_Prefix := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Php_Class_Prefix.Value);
            when 41 =>
               if  not V.Php_Namespace.Is_Set then
                  V.Php_Namespace := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Php_Namespace.Value);
            when 44 =>
               if  not V.Php_Metadata_Namespace.Is_Set then
                  V.Php_Metadata_Namespace := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Php_Metadata_Namespace.Value);
            when 45 =>
               if  not V.Ruby_Package.Is_Set then
                  V.Ruby_Package := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Ruby_Package.Value);
            when 50 =>
               if  not V.Features.Is_Set then
                  V.Features := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Feature_Set_IO.Read
                 (Stream, Key.Encoding, V.Features.Value);
            when 999 =>
               Google_Protobuf_Descriptor_Uninterpreted_Option_IO.Read_Vector
                 (Stream, Key.Encoding, V.Uninterpreted_Option);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_File_Options;

   procedure Write_File_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : File_Options) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_File_Options (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Java_Package.Is_Set then
            WS.Write (1, V.Java_Package.Value);
         end if;
         if V.Java_Outer_Classname.Is_Set then
            WS.Write (8, V.Java_Outer_Classname.Value);
         end if;
         if V.Java_Multiple_Files.Is_Set then
            WS.Write (10, V.Java_Multiple_Files.Value);
         end if;
         if V.Java_Generate_Equals_And_Hash.Is_Set then
            WS.Write (20, V.Java_Generate_Equals_And_Hash.Value);
         end if;
         if V.Java_String_Check_Utf_8.Is_Set then
            WS.Write (27, V.Java_String_Check_Utf_8.Value);
         end if;
         if V.Optimize_For.Is_Set then
            Google_Protobuf_Descriptor_Optimize_Mode_IO.Write
              (WS, 9, V.Optimize_For.Value);
         end if;
         if V.Go_Package.Is_Set then
            WS.Write (11, V.Go_Package.Value);
         end if;
         if V.Cc_Generic_Services.Is_Set then
            WS.Write (16, V.Cc_Generic_Services.Value);
         end if;
         if V.Java_Generic_Services.Is_Set then
            WS.Write (17, V.Java_Generic_Services.Value);
         end if;
         if V.Py_Generic_Services.Is_Set then
            WS.Write (18, V.Py_Generic_Services.Value);
         end if;
         if V.Deprecated.Is_Set then
            WS.Write (23, V.Deprecated.Value);
         end if;
         if V.Cc_Enable_Arenas.Is_Set then
            WS.Write (31, V.Cc_Enable_Arenas.Value);
         end if;
         if V.Objc_Class_Prefix.Is_Set then
            WS.Write (36, V.Objc_Class_Prefix.Value);
         end if;
         if V.Csharp_Namespace.Is_Set then
            WS.Write (37, V.Csharp_Namespace.Value);
         end if;
         if V.Swift_Prefix.Is_Set then
            WS.Write (39, V.Swift_Prefix.Value);
         end if;
         if V.Php_Class_Prefix.Is_Set then
            WS.Write (40, V.Php_Class_Prefix.Value);
         end if;
         if V.Php_Namespace.Is_Set then
            WS.Write (41, V.Php_Namespace.Value);
         end if;
         if V.Php_Metadata_Namespace.Is_Set then
            WS.Write (44, V.Php_Metadata_Namespace.Value);
         end if;
         if V.Ruby_Package.Is_Set then
            WS.Write (45, V.Ruby_Package.Value);
         end if;
         if V.Features.Is_Set then
            WS.Write_Key ((50, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Feature_Set'Write
              (Stream, V.Features.Value);
         end if;
         for J in 1 .. V.Uninterpreted_Option.Length loop
            WS.Write_Key ((999, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Uninterpreted_Option'Write
              (Stream, V.Uninterpreted_Option (J));
         end loop;
         if WS.End_Message then
            Write_File_Options (WS'Access, V);
         end if;
      end;
   end Write_File_Options;

   function Length (Self : Message_Options_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Message_Options_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Message_Options_Array, Message_Options_Array_Access);

   procedure Append
    (Self : in out Message_Options_Vector;
     V    : Message_Options) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Message_Options'Size);
      Aux_Data    : Message_Options_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Message_Options_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Message_Options_Array'
             (Self.Data.all & Message_Options_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Message_Options_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Message_Options_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Message_Options_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Message_Options_Variable_Reference
    (Self  : aliased in out Message_Options_Vector;
     Index : Positive)
      return Message_Options_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Message_Options_Variable_Reference;

   not overriding function Get_Message_Options_Constant_Reference
    (Self  : aliased Message_Options_Vector;
     Index : Positive)
      return Message_Options_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Message_Options_Constant_Reference;

   procedure Read_Message_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Message_Options) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Message_Set_Wire_Format.Is_Set then
                  V.Message_Set_Wire_Format := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Message_Set_Wire_Format.Value);
            when 2 =>
               if  not V.No_Standard_Descriptor_Accessor.Is_Set then
                  V.No_Standard_Descriptor_Accessor := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding,
                  V.No_Standard_Descriptor_Accessor.Value);
            when 3 =>
               if  not V.Deprecated.Is_Set then
                  V.Deprecated := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Deprecated.Value);
            when 7 =>
               if  not V.Map_Entry.Is_Set then
                  V.Map_Entry := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Map_Entry.Value);
            when 11 =>
               if  not V.Deprecated_Legacy_Json_Field_Conflicts.Is_Set then
                  V.Deprecated_Legacy_Json_Field_Conflicts :=
                    (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding,
                  V.Deprecated_Legacy_Json_Field_Conflicts.Value);
            when 12 =>
               if  not V.Features.Is_Set then
                  V.Features := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Feature_Set_IO.Read
                 (Stream, Key.Encoding, V.Features.Value);
            when 999 =>
               Google_Protobuf_Descriptor_Uninterpreted_Option_IO.Read_Vector
                 (Stream, Key.Encoding, V.Uninterpreted_Option);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Message_Options;

   procedure Write_Message_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Message_Options) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Message_Options (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Message_Set_Wire_Format.Is_Set then
            WS.Write (1, V.Message_Set_Wire_Format.Value);
         end if;
         if V.No_Standard_Descriptor_Accessor.Is_Set then
            WS.Write (2, V.No_Standard_Descriptor_Accessor.Value);
         end if;
         if V.Deprecated.Is_Set then
            WS.Write (3, V.Deprecated.Value);
         end if;
         if V.Map_Entry.Is_Set then
            WS.Write (7, V.Map_Entry.Value);
         end if;
         if V.Deprecated_Legacy_Json_Field_Conflicts.Is_Set then
            WS.Write (11, V.Deprecated_Legacy_Json_Field_Conflicts.Value);
         end if;
         if V.Features.Is_Set then
            WS.Write_Key ((12, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Feature_Set'Write
              (Stream, V.Features.Value);
         end if;
         for J in 1 .. V.Uninterpreted_Option.Length loop
            WS.Write_Key ((999, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Uninterpreted_Option'Write
              (Stream, V.Uninterpreted_Option (J));
         end loop;
         if WS.End_Message then
            Write_Message_Options (WS'Access, V);
         end if;
      end;
   end Write_Message_Options;

   function Length (Self : Field_Options_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Field_Options_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Field_Options_Array, Field_Options_Array_Access);

   procedure Append
    (Self : in out Field_Options_Vector;
     V    : Field_Options) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Field_Options'Size);
      Aux_Data    : Field_Options_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Field_Options_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Field_Options_Array'
             (Self.Data.all & Field_Options_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Field_Options_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Field_Options_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Field_Options_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Field_Options_Variable_Reference
    (Self  : aliased in out Field_Options_Vector;
     Index : Positive)
      return Field_Options_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Field_Options_Variable_Reference;

   not overriding function Get_Field_Options_Constant_Reference
    (Self  : aliased Field_Options_Vector;
     Index : Positive)
      return Field_Options_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Field_Options_Constant_Reference;

   procedure Read_Field_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Field_Options) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Ctype.Is_Set then
                  V.Ctype := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_CType_IO.Read
                 (Stream, Key.Encoding, V.Ctype.Value);
            when 2 =>
               if  not V.Packed.Is_Set then
                  V.Packed := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Packed.Value);
            when 6 =>
               if  not V.Jstype.Is_Set then
                  V.Jstype := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_JSType_IO.Read
                 (Stream, Key.Encoding, V.Jstype.Value);
            when 5 =>
               if  not V.Lazy.Is_Set then
                  V.Lazy := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Lazy.Value);
            when 15 =>
               if  not V.Unverified_Lazy.Is_Set then
                  V.Unverified_Lazy := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Unverified_Lazy.Value);
            when 3 =>
               if  not V.Deprecated.Is_Set then
                  V.Deprecated := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Deprecated.Value);
            when 10 =>
               if  not V.Weak.Is_Set then
                  V.Weak := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Weak.Value);
            when 16 =>
               if  not V.Debug_Redact.Is_Set then
                  V.Debug_Redact := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Debug_Redact.Value);
            when 17 =>
               if  not V.Retention.Is_Set then
                  V.Retention := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Option_Retention_IO.Read
                 (Stream, Key.Encoding, V.Retention.Value);
            when 19 =>
               Google_Protobuf_Descriptor_Option_Target_Type_IO.Read_Vector
                 (Stream, Key.Encoding, V.Targets);
            when 20 =>
               Google_Protobuf_Descriptor_Edition_Default_IO.Read_Vector
                 (Stream, Key.Encoding, V.Edition_Defaults);
            when 21 =>
               if  not V.Features.Is_Set then
                  V.Features := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Feature_Set_IO.Read
                 (Stream, Key.Encoding, V.Features.Value);
            when 22 =>
               if  not V.Feature_Support.Is_Set then
                  V.Feature_Support := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Feature_Support_IO.Read
                 (Stream, Key.Encoding, V.Feature_Support.Value);
            when 999 =>
               Google_Protobuf_Descriptor_Uninterpreted_Option_IO.Read_Vector
                 (Stream, Key.Encoding, V.Uninterpreted_Option);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Field_Options;

   procedure Write_Field_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Field_Options) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Field_Options (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Ctype.Is_Set then
            Google_Protobuf_Descriptor_CType_IO.Write (WS, 1, V.Ctype.Value);
         end if;
         if V.Packed.Is_Set then
            WS.Write (2, V.Packed.Value);
         end if;
         if V.Jstype.Is_Set then
            Google_Protobuf_Descriptor_JSType_IO.Write (WS, 6, V.Jstype.Value);
         end if;
         if V.Lazy.Is_Set then
            WS.Write (5, V.Lazy.Value);
         end if;
         if V.Unverified_Lazy.Is_Set then
            WS.Write (15, V.Unverified_Lazy.Value);
         end if;
         if V.Deprecated.Is_Set then
            WS.Write (3, V.Deprecated.Value);
         end if;
         if V.Weak.Is_Set then
            WS.Write (10, V.Weak.Value);
         end if;
         if V.Debug_Redact.Is_Set then
            WS.Write (16, V.Debug_Redact.Value);
         end if;
         if V.Retention.Is_Set then
            Google_Protobuf_Descriptor_Option_Retention_IO.Write
              (WS, 17, V.Retention.Value);
         end if;
         Google_Protobuf_Descriptor_Option_Target_Type_IO.Write
           (WS, 19, V.Targets);
         for J in 1 .. V.Edition_Defaults.Length loop
            WS.Write_Key ((20, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Edition_Default'Write
              (Stream, V.Edition_Defaults (J));
         end loop;
         if V.Features.Is_Set then
            WS.Write_Key ((21, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Feature_Set'Write
              (Stream, V.Features.Value);
         end if;
         if V.Feature_Support.Is_Set then
            WS.Write_Key ((22, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Feature_Support'Write
              (Stream, V.Feature_Support.Value);
         end if;
         for J in 1 .. V.Uninterpreted_Option.Length loop
            WS.Write_Key ((999, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Uninterpreted_Option'Write
              (Stream, V.Uninterpreted_Option (J));
         end loop;
         if WS.End_Message then
            Write_Field_Options (WS'Access, V);
         end if;
      end;
   end Write_Field_Options;

   function Length (Self : Edition_Default_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Edition_Default_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Edition_Default_Array, Edition_Default_Array_Access);

   procedure Append
    (Self : in out Edition_Default_Vector;
     V    : Edition_Default) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Edition_Default'Size);
      Aux_Data    : Edition_Default_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Edition_Default_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Edition_Default_Array'
             (Self.Data.all & Edition_Default_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Edition_Default_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Edition_Default_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Edition_Default_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Edition_Default_Variable_Reference
    (Self  : aliased in out Edition_Default_Vector;
     Index : Positive)
      return Edition_Default_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Edition_Default_Variable_Reference;

   not overriding function Get_Edition_Default_Constant_Reference
    (Self  : aliased Edition_Default_Vector;
     Index : Positive)
      return Edition_Default_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Edition_Default_Constant_Reference;

   procedure Read_Edition_Default
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Edition_Default) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 3 =>
               if  not V.Edition.Is_Set then
                  V.Edition := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Edition_IO.Read
                 (Stream, Key.Encoding, V.Edition.Value);
            when 2 =>
               if  not V.Value.Is_Set then
                  V.Value := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Value.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Edition_Default;

   procedure Write_Edition_Default
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Edition_Default) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Edition_Default (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Edition.Is_Set then
            Google_Protobuf_Descriptor_Edition_IO.Write
              (WS, 3, V.Edition.Value);
         end if;
         if V.Value.Is_Set then
            WS.Write (2, V.Value.Value);
         end if;
         if WS.End_Message then
            Write_Edition_Default (WS'Access, V);
         end if;
      end;
   end Write_Edition_Default;

   function Length (Self : Feature_Support_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Feature_Support_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Feature_Support_Array, Feature_Support_Array_Access);

   procedure Append
    (Self : in out Feature_Support_Vector;
     V    : Feature_Support) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Feature_Support'Size);
      Aux_Data    : Feature_Support_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Feature_Support_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Feature_Support_Array'
             (Self.Data.all & Feature_Support_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Feature_Support_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Feature_Support_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Feature_Support_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Feature_Support_Variable_Reference
    (Self  : aliased in out Feature_Support_Vector;
     Index : Positive)
      return Feature_Support_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Feature_Support_Variable_Reference;

   not overriding function Get_Feature_Support_Constant_Reference
    (Self  : aliased Feature_Support_Vector;
     Index : Positive)
      return Feature_Support_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Feature_Support_Constant_Reference;

   procedure Read_Feature_Support
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Feature_Support) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Edition_Introduced.Is_Set then
                  V.Edition_Introduced := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Edition_IO.Read
                 (Stream, Key.Encoding, V.Edition_Introduced.Value);
            when 2 =>
               if  not V.Edition_Deprecated.Is_Set then
                  V.Edition_Deprecated := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Edition_IO.Read
                 (Stream, Key.Encoding, V.Edition_Deprecated.Value);
            when 3 =>
               if  not V.Deprecation_Warning.Is_Set then
                  V.Deprecation_Warning := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Deprecation_Warning.Value);
            when 4 =>
               if  not V.Edition_Removed.Is_Set then
                  V.Edition_Removed := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Edition_IO.Read
                 (Stream, Key.Encoding, V.Edition_Removed.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Feature_Support;

   procedure Write_Feature_Support
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Feature_Support) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Feature_Support (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Edition_Introduced.Is_Set then
            Google_Protobuf_Descriptor_Edition_IO.Write
              (WS, 1, V.Edition_Introduced.Value);
         end if;
         if V.Edition_Deprecated.Is_Set then
            Google_Protobuf_Descriptor_Edition_IO.Write
              (WS, 2, V.Edition_Deprecated.Value);
         end if;
         if V.Deprecation_Warning.Is_Set then
            WS.Write (3, V.Deprecation_Warning.Value);
         end if;
         if V.Edition_Removed.Is_Set then
            Google_Protobuf_Descriptor_Edition_IO.Write
              (WS, 4, V.Edition_Removed.Value);
         end if;
         if WS.End_Message then
            Write_Feature_Support (WS'Access, V);
         end if;
      end;
   end Write_Feature_Support;

   function Length (Self : Oneof_Options_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Oneof_Options_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Oneof_Options_Array, Oneof_Options_Array_Access);

   procedure Append
    (Self : in out Oneof_Options_Vector;
     V    : Oneof_Options) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Oneof_Options'Size);
      Aux_Data    : Oneof_Options_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Oneof_Options_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Oneof_Options_Array'
             (Self.Data.all & Oneof_Options_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Oneof_Options_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Oneof_Options_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Oneof_Options_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Oneof_Options_Variable_Reference
    (Self  : aliased in out Oneof_Options_Vector;
     Index : Positive)
      return Oneof_Options_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Oneof_Options_Variable_Reference;

   not overriding function Get_Oneof_Options_Constant_Reference
    (Self  : aliased Oneof_Options_Vector;
     Index : Positive)
      return Oneof_Options_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Oneof_Options_Constant_Reference;

   procedure Read_Oneof_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Oneof_Options) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Features.Is_Set then
                  V.Features := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Feature_Set_IO.Read
                 (Stream, Key.Encoding, V.Features.Value);
            when 999 =>
               Google_Protobuf_Descriptor_Uninterpreted_Option_IO.Read_Vector
                 (Stream, Key.Encoding, V.Uninterpreted_Option);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Oneof_Options;

   procedure Write_Oneof_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Oneof_Options) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Oneof_Options (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Features.Is_Set then
            WS.Write_Key ((1, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Feature_Set'Write
              (Stream, V.Features.Value);
         end if;
         for J in 1 .. V.Uninterpreted_Option.Length loop
            WS.Write_Key ((999, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Uninterpreted_Option'Write
              (Stream, V.Uninterpreted_Option (J));
         end loop;
         if WS.End_Message then
            Write_Oneof_Options (WS'Access, V);
         end if;
      end;
   end Write_Oneof_Options;

   function Length (Self : Enum_Options_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Enum_Options_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Enum_Options_Array, Enum_Options_Array_Access);

   procedure Append (Self : in out Enum_Options_Vector; V    : Enum_Options) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Enum_Options'Size);
      Aux_Data    : Enum_Options_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Enum_Options_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Enum_Options_Array'
             (Self.Data.all & Enum_Options_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Enum_Options_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Enum_Options_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Enum_Options_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Enum_Options_Variable_Reference
    (Self  : aliased in out Enum_Options_Vector;
     Index : Positive)
      return Enum_Options_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Enum_Options_Variable_Reference;

   not overriding function Get_Enum_Options_Constant_Reference
    (Self  : aliased Enum_Options_Vector;
     Index : Positive)
      return Enum_Options_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Enum_Options_Constant_Reference;

   procedure Read_Enum_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Enum_Options) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 2 =>
               if  not V.Allow_Alias.Is_Set then
                  V.Allow_Alias := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Allow_Alias.Value);
            when 3 =>
               if  not V.Deprecated.Is_Set then
                  V.Deprecated := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Deprecated.Value);
            when 6 =>
               if  not V.Deprecated_Legacy_Json_Field_Conflicts.Is_Set then
                  V.Deprecated_Legacy_Json_Field_Conflicts :=
                    (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding,
                  V.Deprecated_Legacy_Json_Field_Conflicts.Value);
            when 7 =>
               if  not V.Features.Is_Set then
                  V.Features := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Feature_Set_IO.Read
                 (Stream, Key.Encoding, V.Features.Value);
            when 999 =>
               Google_Protobuf_Descriptor_Uninterpreted_Option_IO.Read_Vector
                 (Stream, Key.Encoding, V.Uninterpreted_Option);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Enum_Options;

   procedure Write_Enum_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Enum_Options) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Enum_Options (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Allow_Alias.Is_Set then
            WS.Write (2, V.Allow_Alias.Value);
         end if;
         if V.Deprecated.Is_Set then
            WS.Write (3, V.Deprecated.Value);
         end if;
         if V.Deprecated_Legacy_Json_Field_Conflicts.Is_Set then
            WS.Write (6, V.Deprecated_Legacy_Json_Field_Conflicts.Value);
         end if;
         if V.Features.Is_Set then
            WS.Write_Key ((7, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Feature_Set'Write
              (Stream, V.Features.Value);
         end if;
         for J in 1 .. V.Uninterpreted_Option.Length loop
            WS.Write_Key ((999, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Uninterpreted_Option'Write
              (Stream, V.Uninterpreted_Option (J));
         end loop;
         if WS.End_Message then
            Write_Enum_Options (WS'Access, V);
         end if;
      end;
   end Write_Enum_Options;

   function Length (Self : Enum_Value_Options_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Enum_Value_Options_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Enum_Value_Options_Array, Enum_Value_Options_Array_Access);

   procedure Append
    (Self : in out Enum_Value_Options_Vector;
     V    : Enum_Value_Options) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Enum_Value_Options'Size);
      Aux_Data    : Enum_Value_Options_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Enum_Value_Options_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Enum_Value_Options_Array'
             (Self.Data.all
                & Enum_Value_Options_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Enum_Value_Options_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Enum_Value_Options_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Enum_Value_Options_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Enum_Value_Options_Variable_Reference
    (Self  : aliased in out Enum_Value_Options_Vector;
     Index : Positive)
      return Enum_Value_Options_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Enum_Value_Options_Variable_Reference;

   not overriding function Get_Enum_Value_Options_Constant_Reference
    (Self  : aliased Enum_Value_Options_Vector;
     Index : Positive)
      return Enum_Value_Options_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Enum_Value_Options_Constant_Reference;

   procedure Read_Enum_Value_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Enum_Value_Options) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Deprecated.Is_Set then
                  V.Deprecated := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Deprecated.Value);
            when 2 =>
               if  not V.Features.Is_Set then
                  V.Features := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Feature_Set_IO.Read
                 (Stream, Key.Encoding, V.Features.Value);
            when 3 =>
               if  not V.Debug_Redact.Is_Set then
                  V.Debug_Redact := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Debug_Redact.Value);
            when 4 =>
               if  not V.Feature_Support.Is_Set then
                  V.Feature_Support := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Feature_Support_IO.Read
                 (Stream, Key.Encoding, V.Feature_Support.Value);
            when 999 =>
               Google_Protobuf_Descriptor_Uninterpreted_Option_IO.Read_Vector
                 (Stream, Key.Encoding, V.Uninterpreted_Option);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Enum_Value_Options;

   procedure Write_Enum_Value_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Enum_Value_Options) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Enum_Value_Options (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Deprecated.Is_Set then
            WS.Write (1, V.Deprecated.Value);
         end if;
         if V.Features.Is_Set then
            WS.Write_Key ((2, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Feature_Set'Write
              (Stream, V.Features.Value);
         end if;
         if V.Debug_Redact.Is_Set then
            WS.Write (3, V.Debug_Redact.Value);
         end if;
         if V.Feature_Support.Is_Set then
            WS.Write_Key ((4, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Feature_Support'Write
              (Stream, V.Feature_Support.Value);
         end if;
         for J in 1 .. V.Uninterpreted_Option.Length loop
            WS.Write_Key ((999, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Uninterpreted_Option'Write
              (Stream, V.Uninterpreted_Option (J));
         end loop;
         if WS.End_Message then
            Write_Enum_Value_Options (WS'Access, V);
         end if;
      end;
   end Write_Enum_Value_Options;

   function Length (Self : Service_Options_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Service_Options_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Service_Options_Array, Service_Options_Array_Access);

   procedure Append
    (Self : in out Service_Options_Vector;
     V    : Service_Options) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Service_Options'Size);
      Aux_Data    : Service_Options_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Service_Options_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Service_Options_Array'
             (Self.Data.all & Service_Options_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Service_Options_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Service_Options_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Service_Options_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Service_Options_Variable_Reference
    (Self  : aliased in out Service_Options_Vector;
     Index : Positive)
      return Service_Options_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Service_Options_Variable_Reference;

   not overriding function Get_Service_Options_Constant_Reference
    (Self  : aliased Service_Options_Vector;
     Index : Positive)
      return Service_Options_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Service_Options_Constant_Reference;

   procedure Read_Service_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Service_Options) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 34 =>
               if  not V.Features.Is_Set then
                  V.Features := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Feature_Set_IO.Read
                 (Stream, Key.Encoding, V.Features.Value);
            when 33 =>
               if  not V.Deprecated.Is_Set then
                  V.Deprecated := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Deprecated.Value);
            when 999 =>
               Google_Protobuf_Descriptor_Uninterpreted_Option_IO.Read_Vector
                 (Stream, Key.Encoding, V.Uninterpreted_Option);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Service_Options;

   procedure Write_Service_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Service_Options) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Service_Options (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Features.Is_Set then
            WS.Write_Key ((34, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Feature_Set'Write
              (Stream, V.Features.Value);
         end if;
         if V.Deprecated.Is_Set then
            WS.Write (33, V.Deprecated.Value);
         end if;
         for J in 1 .. V.Uninterpreted_Option.Length loop
            WS.Write_Key ((999, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Uninterpreted_Option'Write
              (Stream, V.Uninterpreted_Option (J));
         end loop;
         if WS.End_Message then
            Write_Service_Options (WS'Access, V);
         end if;
      end;
   end Write_Service_Options;

   function Length (Self : Method_Options_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Method_Options_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Method_Options_Array, Method_Options_Array_Access);

   procedure Append
    (Self : in out Method_Options_Vector;
     V    : Method_Options) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Method_Options'Size);
      Aux_Data    : Method_Options_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Method_Options_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Method_Options_Array'
             (Self.Data.all & Method_Options_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Method_Options_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Method_Options_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Method_Options_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Method_Options_Variable_Reference
    (Self  : aliased in out Method_Options_Vector;
     Index : Positive)
      return Method_Options_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Method_Options_Variable_Reference;

   not overriding function Get_Method_Options_Constant_Reference
    (Self  : aliased Method_Options_Vector;
     Index : Positive)
      return Method_Options_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Method_Options_Constant_Reference;

   procedure Read_Method_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Method_Options) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 33 =>
               if  not V.Deprecated.Is_Set then
                  V.Deprecated := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Deprecated.Value);
            when 34 =>
               if  not V.Idempotency_Level.Is_Set then
                  V.Idempotency_Level := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Idempotency_Level_IO.Read
                 (Stream, Key.Encoding, V.Idempotency_Level.Value);
            when 35 =>
               if  not V.Features.Is_Set then
                  V.Features := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Feature_Set_IO.Read
                 (Stream, Key.Encoding, V.Features.Value);
            when 999 =>
               Google_Protobuf_Descriptor_Uninterpreted_Option_IO.Read_Vector
                 (Stream, Key.Encoding, V.Uninterpreted_Option);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Method_Options;

   procedure Write_Method_Options
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Method_Options) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Method_Options (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Deprecated.Is_Set then
            WS.Write (33, V.Deprecated.Value);
         end if;
         if V.Idempotency_Level.Is_Set then
            Google_Protobuf_Descriptor_Idempotency_Level_IO.Write
              (WS, 34, V.Idempotency_Level.Value);
         end if;
         if V.Features.Is_Set then
            WS.Write_Key ((35, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Feature_Set'Write
              (Stream, V.Features.Value);
         end if;
         for J in 1 .. V.Uninterpreted_Option.Length loop
            WS.Write_Key ((999, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Uninterpreted_Option'Write
              (Stream, V.Uninterpreted_Option (J));
         end loop;
         if WS.End_Message then
            Write_Method_Options (WS'Access, V);
         end if;
      end;
   end Write_Method_Options;

   function Length (Self : Uninterpreted_Option_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Uninterpreted_Option_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Uninterpreted_Option_Array, Uninterpreted_Option_Array_Access);

   procedure Append
    (Self : in out Uninterpreted_Option_Vector;
     V    : Uninterpreted_Option) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Uninterpreted_Option'Size);
      Aux_Data    : Uninterpreted_Option_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Uninterpreted_Option_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Uninterpreted_Option_Array'
             (Self.Data.all
                & Uninterpreted_Option_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Uninterpreted_Option_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Uninterpreted_Option_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Uninterpreted_Option_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Uninterpreted_Option_Variable_Reference
    (Self  : aliased in out Uninterpreted_Option_Vector;
     Index : Positive)
      return Uninterpreted_Option_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Uninterpreted_Option_Variable_Reference;

   not overriding function Get_Uninterpreted_Option_Constant_Reference
    (Self  : aliased Uninterpreted_Option_Vector;
     Index : Positive)
      return Uninterpreted_Option_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Uninterpreted_Option_Constant_Reference;

   procedure Read_Uninterpreted_Option
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Uninterpreted_Option) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 2 =>
               Google_Protobuf_Descriptor_Name_Part_IO.Read_Vector
                 (Stream, Key.Encoding, V.Name);
            when 3 =>
               if  not V.Identifier_Value.Is_Set then
                  V.Identifier_Value := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Identifier_Value.Value);
            when 4 =>
               if  not V.Positive_Int_Value.Is_Set then
                  V.Positive_Int_Value := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Positive_Int_Value.Value);
            when 5 =>
               if  not V.Negative_Int_Value.Is_Set then
                  V.Negative_Int_Value := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Negative_Int_Value.Value);
            when 6 =>
               if  not V.Double_Value.Is_Set then
                  V.Double_Value := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Double_Value.Value);
            when 7 =>
               if  not V.String_Value.Is_Set then
                  V.String_Value := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.String_Value.Value);
            when 8 =>
               if  not V.Aggregate_Value.Is_Set then
                  V.Aggregate_Value := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Aggregate_Value.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Uninterpreted_Option;

   procedure Write_Uninterpreted_Option
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Uninterpreted_Option) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Uninterpreted_Option (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         for J in 1 .. V.Name.Length loop
            WS.Write_Key ((2, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Name_Part'Write (Stream, V.Name (J));
         end loop;
         if V.Identifier_Value.Is_Set then
            WS.Write (3, V.Identifier_Value.Value);
         end if;
         if V.Positive_Int_Value.Is_Set then
            WS.Write_Varint (4, V.Positive_Int_Value.Value);
         end if;
         if V.Negative_Int_Value.Is_Set then
            WS.Write_Varint (5, V.Negative_Int_Value.Value);
         end if;
         if V.Double_Value.Is_Set then
            WS.Write (6, V.Double_Value.Value);
         end if;
         if V.String_Value.Is_Set then
            WS.Write (7, V.String_Value.Value);
         end if;
         if V.Aggregate_Value.Is_Set then
            WS.Write (8, V.Aggregate_Value.Value);
         end if;
         if WS.End_Message then
            Write_Uninterpreted_Option (WS'Access, V);
         end if;
      end;
   end Write_Uninterpreted_Option;

   function Length (Self : Name_Part_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Name_Part_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Name_Part_Array, Name_Part_Array_Access);

   procedure Append (Self : in out Name_Part_Vector; V    : Name_Part) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Name_Part'Size);
      Aux_Data    : Name_Part_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Name_Part_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Name_Part_Array'
             (Self.Data.all & Name_Part_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Name_Part_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Name_Part_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Name_Part_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Name_Part_Variable_Reference
    (Self  : aliased in out Name_Part_Vector;
     Index : Positive)
      return Name_Part_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Name_Part_Variable_Reference;

   not overriding function Get_Name_Part_Constant_Reference
    (Self  : aliased Name_Part_Vector;
     Index : Positive)
      return Name_Part_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Name_Part_Constant_Reference;

   procedure Read_Name_Part
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Name_Part) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Name_Part);
            when 2 =>
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Is_Extension);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Name_Part;

   procedure Write_Name_Part
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Name_Part) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Name_Part (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write (1, V.Name_Part);
         WS.Write (2, V.Is_Extension);
         if WS.End_Message then
            Write_Name_Part (WS'Access, V);
         end if;
      end;
   end Write_Name_Part;

   function Length (Self : Feature_Set_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Feature_Set_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Feature_Set_Array, Feature_Set_Array_Access);

   procedure Append (Self : in out Feature_Set_Vector; V    : Feature_Set) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Feature_Set'Size);
      Aux_Data    : Feature_Set_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Feature_Set_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Feature_Set_Array'
             (Self.Data.all & Feature_Set_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Feature_Set_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Feature_Set_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Feature_Set_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Feature_Set_Variable_Reference
    (Self  : aliased in out Feature_Set_Vector;
     Index : Positive)
      return Feature_Set_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Feature_Set_Variable_Reference;

   not overriding function Get_Feature_Set_Constant_Reference
    (Self  : aliased Feature_Set_Vector;
     Index : Positive)
      return Feature_Set_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Feature_Set_Constant_Reference;

   procedure Read_Feature_Set
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Feature_Set) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               if  not V.Field_Presence.Is_Set then
                  V.Field_Presence := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Field_Presence_IO.Read
                 (Stream, Key.Encoding, V.Field_Presence.Value);
            when 2 =>
               if  not V.Enum_Type.Is_Set then
                  V.Enum_Type := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Enum_Type_IO.Read
                 (Stream, Key.Encoding, V.Enum_Type.Value);
            when 3 =>
               if  not V.Repeated_Field_Encoding.Is_Set then
                  V.Repeated_Field_Encoding := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Repeated_Field_Encoding_IO.Read
                 (Stream, Key.Encoding, V.Repeated_Field_Encoding.Value);
            when 4 =>
               if  not V.Utf_8_Validation.Is_Set then
                  V.Utf_8_Validation := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Utf_8Validation_IO.Read
                 (Stream, Key.Encoding, V.Utf_8_Validation.Value);
            when 5 =>
               if  not V.Message_Encoding.Is_Set then
                  V.Message_Encoding := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Message_Encoding_IO.Read
                 (Stream, Key.Encoding, V.Message_Encoding.Value);
            when 6 =>
               if  not V.Json_Format.Is_Set then
                  V.Json_Format := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Json_Format_IO.Read
                 (Stream, Key.Encoding, V.Json_Format.Value);
            when 7 =>
               if  not V.Enforce_Naming_Style.Is_Set then
                  V.Enforce_Naming_Style := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Enforce_Naming_Style_IO.Read
                 (Stream, Key.Encoding, V.Enforce_Naming_Style.Value);
            when 8 =>
               if  not V.Default_Symbol_Visibility.Is_Set then
                  V.Default_Symbol_Visibility := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Default_Symbol_Visibility_IO.Read
                 (Stream, Key.Encoding, V.Default_Symbol_Visibility.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Feature_Set;

   procedure Write_Feature_Set
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Feature_Set) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Feature_Set (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Field_Presence.Is_Set then
            Google_Protobuf_Descriptor_Field_Presence_IO.Write
              (WS, 1, V.Field_Presence.Value);
         end if;
         if V.Enum_Type.Is_Set then
            Google_Protobuf_Descriptor_Enum_Type_IO.Write
              (WS, 2, V.Enum_Type.Value);
         end if;
         if V.Repeated_Field_Encoding.Is_Set then
            Google_Protobuf_Descriptor_Repeated_Field_Encoding_IO.Write
              (WS, 3, V.Repeated_Field_Encoding.Value);
         end if;
         if V.Utf_8_Validation.Is_Set then
            Google_Protobuf_Descriptor_Utf_8Validation_IO.Write
              (WS, 4, V.Utf_8_Validation.Value);
         end if;
         if V.Message_Encoding.Is_Set then
            Google_Protobuf_Descriptor_Message_Encoding_IO.Write
              (WS, 5, V.Message_Encoding.Value);
         end if;
         if V.Json_Format.Is_Set then
            Google_Protobuf_Descriptor_Json_Format_IO.Write
              (WS, 6, V.Json_Format.Value);
         end if;
         if V.Enforce_Naming_Style.Is_Set then
            Google_Protobuf_Descriptor_Enforce_Naming_Style_IO.Write
              (WS, 7, V.Enforce_Naming_Style.Value);
         end if;
         if V.Default_Symbol_Visibility.Is_Set then
            Google_Protobuf_Descriptor_Default_Symbol_Visibility_IO.Write
              (WS, 8, V.Default_Symbol_Visibility.Value);
         end if;
         if WS.End_Message then
            Write_Feature_Set (WS'Access, V);
         end if;
      end;
   end Write_Feature_Set;

   function Length (Self : Visibility_Feature_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Visibility_Feature_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Visibility_Feature_Array, Visibility_Feature_Array_Access);

   procedure Append
    (Self : in out Visibility_Feature_Vector;
     V    : Visibility_Feature) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Visibility_Feature'Size);
      Aux_Data    : Visibility_Feature_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Visibility_Feature_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Visibility_Feature_Array'
             (Self.Data.all
                & Visibility_Feature_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Visibility_Feature_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Visibility_Feature_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Visibility_Feature_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Visibility_Feature_Variable_Reference
    (Self  : aliased in out Visibility_Feature_Vector;
     Index : Positive)
      return Visibility_Feature_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Visibility_Feature_Variable_Reference;

   not overriding function Get_Visibility_Feature_Constant_Reference
    (Self  : aliased Visibility_Feature_Vector;
     Index : Positive)
      return Visibility_Feature_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Visibility_Feature_Constant_Reference;

   procedure Read_Visibility_Feature
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Visibility_Feature) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Visibility_Feature;

   procedure Write_Visibility_Feature
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Visibility_Feature) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Visibility_Feature (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if WS.End_Message then
            Write_Visibility_Feature (WS'Access, V);
         end if;
      end;
   end Write_Visibility_Feature;

   function Length (Self : Feature_Set_Defaults_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Feature_Set_Defaults_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Feature_Set_Defaults_Array, Feature_Set_Defaults_Array_Access);

   procedure Append
    (Self : in out Feature_Set_Defaults_Vector;
     V    : Feature_Set_Defaults) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Feature_Set_Defaults'Size);
      Aux_Data    : Feature_Set_Defaults_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Feature_Set_Defaults_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Feature_Set_Defaults_Array'
             (Self.Data.all
                & Feature_Set_Defaults_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Feature_Set_Defaults_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Feature_Set_Defaults_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Feature_Set_Defaults_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Feature_Set_Defaults_Variable_Reference
    (Self  : aliased in out Feature_Set_Defaults_Vector;
     Index : Positive)
      return Feature_Set_Defaults_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Feature_Set_Defaults_Variable_Reference;

   not overriding function Get_Feature_Set_Defaults_Constant_Reference
    (Self  : aliased Feature_Set_Defaults_Vector;
     Index : Positive)
      return Feature_Set_Defaults_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Feature_Set_Defaults_Constant_Reference;

   procedure Read_Feature_Set_Defaults
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Feature_Set_Defaults) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               Google_Protobuf_Descriptor_Feature_Set_Edition_Default_IO
                 .Read_Vector
                 (Stream, Key.Encoding, V.Defaults);
            when 4 =>
               if  not V.Minimum_Edition.Is_Set then
                  V.Minimum_Edition := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Edition_IO.Read
                 (Stream, Key.Encoding, V.Minimum_Edition.Value);
            when 5 =>
               if  not V.Maximum_Edition.Is_Set then
                  V.Maximum_Edition := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Edition_IO.Read
                 (Stream, Key.Encoding, V.Maximum_Edition.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Feature_Set_Defaults;

   procedure Write_Feature_Set_Defaults
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Feature_Set_Defaults) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Feature_Set_Defaults (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         for J in 1 .. V.Defaults.Length loop
            WS.Write_Key ((1, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Feature_Set_Edition_Default'Write
              (Stream, V.Defaults (J));
         end loop;
         if V.Minimum_Edition.Is_Set then
            Google_Protobuf_Descriptor_Edition_IO.Write
              (WS, 4, V.Minimum_Edition.Value);
         end if;
         if V.Maximum_Edition.Is_Set then
            Google_Protobuf_Descriptor_Edition_IO.Write
              (WS, 5, V.Maximum_Edition.Value);
         end if;
         if WS.End_Message then
            Write_Feature_Set_Defaults (WS'Access, V);
         end if;
      end;
   end Write_Feature_Set_Defaults;

   function Length
    (Self : Feature_Set_Edition_Default_Vector)
      return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Feature_Set_Edition_Default_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Feature_Set_Edition_Default_Array,
      Feature_Set_Edition_Default_Array_Access);

   procedure Append
    (Self : in out Feature_Set_Edition_Default_Vector;
     V    : Feature_Set_Edition_Default) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Feature_Set_Edition_Default'Size);
      Aux_Data    : Feature_Set_Edition_Default_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=
            new Feature_Set_Edition_Default_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Feature_Set_Edition_Default_Array'
             (Self.Data.all
                & Feature_Set_Edition_Default_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust
    (Self : in out Feature_Set_Edition_Default_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Feature_Set_Edition_Default_Array'
             (Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
    (Self : in out Feature_Set_Edition_Default_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Feature_Set_Edition_Default_Variable_Reference
    (Self  : aliased in out Feature_Set_Edition_Default_Vector;
     Index : Positive)
      return Feature_Set_Edition_Default_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Feature_Set_Edition_Default_Variable_Reference;

   not overriding function Get_Feature_Set_Edition_Default_Constant_Reference
    (Self  : aliased Feature_Set_Edition_Default_Vector;
     Index : Positive)
      return Feature_Set_Edition_Default_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Feature_Set_Edition_Default_Constant_Reference;

   procedure Read_Feature_Set_Edition_Default
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Feature_Set_Edition_Default) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 3 =>
               if  not V.Edition.Is_Set then
                  V.Edition := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Edition_IO.Read
                 (Stream, Key.Encoding, V.Edition.Value);
            when 4 =>
               if  not V.Overridable_Features.Is_Set then
                  V.Overridable_Features := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Feature_Set_IO.Read
                 (Stream, Key.Encoding, V.Overridable_Features.Value);
            when 5 =>
               if  not V.Fixed_Features.Is_Set then
                  V.Fixed_Features := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Feature_Set_IO.Read
                 (Stream, Key.Encoding, V.Fixed_Features.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Feature_Set_Edition_Default;

   procedure Write_Feature_Set_Edition_Default
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Feature_Set_Edition_Default) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Feature_Set_Edition_Default (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         if V.Edition.Is_Set then
            Google_Protobuf_Descriptor_Edition_IO.Write
              (WS, 3, V.Edition.Value);
         end if;
         if V.Overridable_Features.Is_Set then
            WS.Write_Key ((4, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Feature_Set'Write
              (Stream, V.Overridable_Features.Value);
         end if;
         if V.Fixed_Features.Is_Set then
            WS.Write_Key ((5, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Feature_Set'Write
              (Stream, V.Fixed_Features.Value);
         end if;
         if WS.End_Message then
            Write_Feature_Set_Edition_Default (WS'Access, V);
         end if;
      end;
   end Write_Feature_Set_Edition_Default;

   function Length (Self : Source_Code_Info_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Source_Code_Info_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Source_Code_Info_Array, Source_Code_Info_Array_Access);

   procedure Append
    (Self : in out Source_Code_Info_Vector;
     V    : Source_Code_Info) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Source_Code_Info'Size);
      Aux_Data    : Source_Code_Info_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Source_Code_Info_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Source_Code_Info_Array'
             (Self.Data.all & Source_Code_Info_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Source_Code_Info_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Source_Code_Info_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Source_Code_Info_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Source_Code_Info_Variable_Reference
    (Self  : aliased in out Source_Code_Info_Vector;
     Index : Positive)
      return Source_Code_Info_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Source_Code_Info_Variable_Reference;

   not overriding function Get_Source_Code_Info_Constant_Reference
    (Self  : aliased Source_Code_Info_Vector;
     Index : Positive)
      return Source_Code_Info_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Source_Code_Info_Constant_Reference;

   procedure Read_Source_Code_Info
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Source_Code_Info) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               Google_Protobuf_Descriptor_Location_IO.Read_Vector
                 (Stream, Key.Encoding, V.Location);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Source_Code_Info;

   procedure Write_Source_Code_Info
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Source_Code_Info) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Source_Code_Info (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         for J in 1 .. V.Location.Length loop
            WS.Write_Key ((1, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Location'Write (Stream, V.Location (J));
         end loop;
         if WS.End_Message then
            Write_Source_Code_Info (WS'Access, V);
         end if;
      end;
   end Write_Source_Code_Info;

   function Length (Self : Location_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Location_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Location_Array, Location_Array_Access);

   procedure Append (Self : in out Location_Vector; V    : Location) is
      Init_Length : constant Positive := Positive'Max (1, 256 / Location'Size);
      Aux_Data    : Location_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Location_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Location_Array'
             (Self.Data.all & Location_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Location_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Location_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Location_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Location_Variable_Reference
    (Self  : aliased in out Location_Vector;
     Index : Positive)
      return Location_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Location_Variable_Reference;

   not overriding function Get_Location_Constant_Reference
    (Self  : aliased Location_Vector;
     Index : Positive)
      return Location_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Location_Constant_Reference;

   procedure Read_Location
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Location) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               Proto_Support.IO.Read_Varint_Vector (Stream, Key.Encoding, V.Path);
            when 2 =>
               Proto_Support.IO.Read_Varint_Vector (Stream, Key.Encoding, V.Span);
            when 3 =>
               if  not V.Leading_Comments.Is_Set then
                  V.Leading_Comments := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Leading_Comments.Value);
            when 4 =>
               if  not V.Trailing_Comments.Is_Set then
                  V.Trailing_Comments := (True, others => <>);
               end if;
               Proto_Support.IO.Read
                 (Stream, Key.Encoding, V.Trailing_Comments.Value);
            when 6 =>
               Proto_Support.IO.Read_Vector
                 (Stream, Key.Encoding, V.Leading_Detached_Comments);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Location;

   procedure Write_Location
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Location) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Location (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Varint_Packed (1, V.Path);
         WS.Write_Varint_Packed (2, V.Span);
         if V.Leading_Comments.Is_Set then
            WS.Write (3, V.Leading_Comments.Value);
         end if;
         if V.Trailing_Comments.Is_Set then
            WS.Write (4, V.Trailing_Comments.Value);
         end if;
         WS.Write (6, V.Leading_Detached_Comments);
         if WS.End_Message then
            Write_Location (WS'Access, V);
         end if;
      end;
   end Write_Location;

   function Length (Self : Generated_Code_Info_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Generated_Code_Info_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Generated_Code_Info_Array, Generated_Code_Info_Array_Access);

   procedure Append
    (Self : in out Generated_Code_Info_Vector;
     V    : Generated_Code_Info) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Generated_Code_Info'Size);
      Aux_Data    : Generated_Code_Info_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Generated_Code_Info_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Generated_Code_Info_Array'
             (Self.Data.all
                & Generated_Code_Info_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Generated_Code_Info_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new Generated_Code_Info_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Generated_Code_Info_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Generated_Code_Info_Variable_Reference
    (Self  : aliased in out Generated_Code_Info_Vector;
     Index : Positive)
      return Generated_Code_Info_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Generated_Code_Info_Variable_Reference;

   not overriding function Get_Generated_Code_Info_Constant_Reference
    (Self  : aliased Generated_Code_Info_Vector;
     Index : Positive)
      return Generated_Code_Info_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Generated_Code_Info_Constant_Reference;

   procedure Read_Generated_Code_Info
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Generated_Code_Info) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               Google_Protobuf_Descriptor_Annotation_IO.Read_Vector
                 (Stream, Key.Encoding, V.Annotation);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Generated_Code_Info;

   procedure Write_Generated_Code_Info
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Generated_Code_Info) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Generated_Code_Info (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         for J in 1 .. V.Annotation.Length loop
            WS.Write_Key ((1, Proto_Support.Length_Delimited));
            Google.Protobuf.Descriptor.Annotation'Write
              (Stream, V.Annotation (J));
         end loop;
         if WS.End_Message then
            Write_Generated_Code_Info (WS'Access, V);
         end if;
      end;
   end Write_Generated_Code_Info;

   function Length (Self : Annotation_Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   procedure Clear (Self : in out Annotation_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Free is new Ada.Unchecked_Deallocation
     (Annotation_Array, Annotation_Array_Access);

   procedure Append (Self : in out Annotation_Vector; V    : Annotation) is
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Annotation'Size);
      Aux_Data    : Annotation_Array_Access;
   begin
      if Self.Length = 0 then
         Self.Data :=  new Annotation_Array (1 .. Init_Length);

      elsif Self.Length = Self.Data'Last then
         Aux_Data := Self.Data;
         Self.Data :=
           new Annotation_Array'
             (Self.Data.all & Annotation_Array'(1 .. Self.Length => <>));
         Free (Aux_Data);
      end if;
      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := V;
   end Append;

   overriding procedure Adjust (Self : in out Annotation_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Annotation_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Annotation_Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   not overriding function Get_Annotation_Variable_Reference
    (Self  : aliased in out Annotation_Vector;
     Index : Positive)
      return Annotation_Variable_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Annotation_Variable_Reference;

   not overriding function Get_Annotation_Constant_Reference
    (Self  : aliased Annotation_Vector;
     Index : Positive)
      return Annotation_Constant_Reference is
   begin
      return (Element => Self.Data (Index)'Access);
   end Get_Annotation_Constant_Reference;

   procedure Read_Annotation
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : out Annotation) is
      Key : aliased Proto_Support.IO.Key;
   begin
      while Proto_Support.IO.Read_Key (Stream, Key'Access) loop
         case Key.Field is
            when 1 =>
               Proto_Support.IO.Read_Varint_Vector (Stream, Key.Encoding, V.Path);
            when 2 =>
               if  not V.Source_File.Is_Set then
                  V.Source_File := (True, others => <>);
               end if;
               Proto_Support.IO.Read (Stream, Key.Encoding, V.Source_File.Value);
            when 3 =>
               if  not V.Proto_Begin.Is_Set then
                  V.Proto_Begin := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Proto_Begin.Value);
            when 4 =>
               if  not V.Proto_End.Is_Set then
                  V.Proto_End := (True, others => <>);
               end if;
               Proto_Support.IO.Read_Varint
                 (Stream, Key.Encoding, V.Proto_End.Value);
            when 5 =>
               if  not V.Semantic.Is_Set then
                  V.Semantic := (True, others => <>);
               end if;
               Google_Protobuf_Descriptor_Semantic_IO.Read
                 (Stream, Key.Encoding, V.Semantic.Value);
            when others =>
               Proto_Support.IO.Unknown_Field (Stream, Key.Encoding);
         end case;
      end loop;
   end Read_Annotation;

   procedure Write_Annotation
    (Stream : access Ada.Streams.Root_Stream_Type'Class;
     V      : Annotation) is
   begin
      if Stream.all not in Proto_Support.Internal.Stream then
         declare
            WS : aliased Proto_Support.Internal.Stream (Stream);
         begin
            Write_Annotation (WS'Access, V);
            return;
         end;
      end if;
      declare
         WS : Proto_Support.Internal.Stream renames
           Proto_Support.Internal.Stream (Stream.all);
      begin
         WS.Start_Message;
         WS.Write_Varint_Packed (1, V.Path);
         if V.Source_File.Is_Set then
            WS.Write (2, V.Source_File.Value);
         end if;
         if V.Proto_Begin.Is_Set then
            WS.Write_Varint (3, V.Proto_Begin.Value);
         end if;
         if V.Proto_End.Is_Set then
            WS.Write_Varint (4, V.Proto_End.Value);
         end if;
         if V.Semantic.Is_Set then
            Google_Protobuf_Descriptor_Semantic_IO.Write
              (WS, 5, V.Semantic.Value);
         end if;
         if WS.End_Message then
            Write_Annotation (WS'Access, V);
         end if;
      end;
   end Write_Annotation;

end Google.Protobuf.Descriptor;