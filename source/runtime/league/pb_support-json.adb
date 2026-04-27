with League.Holders;
with League.JSON.Documents;

package body PB_Support.JSON is

   function "+" (Text : String)
      return League.Strings.Universal_String
            renames League.Strings.From_UTF_8_String;

   procedure Push_Value
     (Self  : in out JSON_Writer;
      Value : League.JSON.Values.JSON_Value);

   ----------------
   -- Push_Value --
   ----------------

   procedure Push_Value
     (Self  : in out JSON_Writer;
      Value : League.JSON.Values.JSON_Value)
   is
   begin
      if Self.Stack.Is_Empty then
         if Self.Has_Root then
            raise JSON_Format_Error with "Multiple top-level JSON values";
         end if;

         Self.Root_Value := Value;
         Self.Has_Root := True;
         return;
      end if;

      declare
         Parent : Container := Self.Stack.Last_Element;
      begin
         case Parent.Kind is
            when Object_Container =>
               if not Self.Has_Pending_Key then
                  raise JSON_Format_Error with "JSON object key is missing";
               end if;

               Parent.Object_Value.Insert (Self.Pending_Key, Value);
               Self.Has_Pending_Key := False;

            when Array_Container =>
               Parent.Array_Value.Append (Value);
         end case;

         Self.Stack.Replace_Element (Self.Stack.Last_Index, Parent);
      end;
   end Push_Value;

   ------------------
   -- Start_Object --
   ------------------

   procedure Start_Object (Self : in out JSON_Writer) is
   begin
      Self.Stack.Append
        (New_Item =>
           (Kind            => Object_Container,
            Object_Value    => League.JSON.Objects.Empty_JSON_Object,
            Pending_Key     => League.Strings.Empty_Universal_String,
            Has_Pending_Key => False));
   end Start_Object;

   ----------------
   -- End_Object --
   ----------------

   procedure End_Object (Self : in out JSON_Writer) is
   begin
      if Self.Stack.Is_Empty or else
         Self.Stack.Last_Element.Kind /= Object_Container
      then
         raise JSON_Format_Error with "Mismatched JSON object end";
      end if;

      declare
         Value : constant League.JSON.Values.JSON_Value :=
           Self.Stack.Last_Element.Object_Value.To_JSON_Value;
      begin
         Self.Stack.Delete_Last;
         Push_Value (Self, Value);
      end;
   end End_Object;

   -----------------
   -- Start_Array --
   -----------------

   procedure Start_Array (Self : in out JSON_Writer) is
   begin
      Self.Stack.Append
        ((Kind => Array_Container,
          Array_Value => League.JSON.Arrays.Empty_JSON_Array));
   end Start_Array;

   ---------------
   -- End_Array --
   ---------------

   procedure End_Array (Self : in out JSON_Writer) is
   begin
      if Self.Stack.Is_Empty or else
         Self.Stack.Last_Element.Kind /= Array_Container
      then
         raise JSON_Format_Error with "Mismatched JSON array end";
      end if;

      declare
         Value : constant League.JSON.Values.JSON_Value :=
           Self.Stack.Last_Element.Array_Value.To_JSON_Value;
      begin
         Self.Stack.Delete_Last;
         Push_Value (Self, Value);
      end;
   end End_Array;

   ---------------
   -- Write_Key --
   ---------------

   procedure Write_Key (Self : in out JSON_Writer; Name : String) is
   begin
      if Self.Stack.Is_Empty or else
         Self.Stack.Last_Element.Kind /= Object_Container
      then
         raise JSON_Format_Error with "JSON key outside object";
      end if;

      Self.Pending_Key := +Name;
      Self.Has_Pending_Key := True;
   end Write_Key;

   function To_Universal_String
     (Self : JSON_Writer) return League.Strings.Universal_String is
      Doc : League.JSON.Documents.JSON_Document;
   begin
      if not Self.Stack.Is_Empty then
         raise JSON_Format_Error with "JSON document is not closed";
      end if;

      if not Self.Has_Root then
         return League.Strings.Empty_Universal_String;
      end if;

      if Self.Root_Value.Is_Object then
         Doc.Set_Object (Self.Root_Value.To_Object);
      elsif Self.Root_Value.Is_Array then
         Doc.Set_Array (Self.Root_Value.To_Array);
      else
         raise JSON_Format_Error
           with "Top-level JSON value must be object or array";
      end if;

      return Doc.To_JSON;
   end To_Universal_String;

   ------------------
   -- Write_String --
   ------------------

   procedure Write_String (Self : in out JSON_Writer; Value : String) is
   begin
      Push_Value (Self, League.JSON.Values.To_JSON_Value (+Value));
   end Write_String;

   -------------------
   -- Write_Integer --
   -------------------

   procedure Write_Integer
      (Self  : in out JSON_Writer;
       Value : Long_Long_Integer) is
   begin
      Push_Value
        (Self,
         League.JSON.Values.To_JSON_Value
           (League.Holders.Universal_Integer (Value)));
   end Write_Integer;

   -----------------
   -- Write_Float --
   -----------------

   procedure Write_Float (Self : in out JSON_Writer; Value : Long_Float) is
   begin
      Push_Value
        (Self,
         League.JSON.Values.To_JSON_Value
           (League.Holders.Universal_Float (Value)));
   end Write_Float;

   -------------------
   -- Write_Boolean --
   -------------------

   procedure Write_Boolean (Self : in out JSON_Writer; Value : Boolean) is
   begin
      Push_Value (Self, League.JSON.Values.To_JSON_Value (Value));
   end Write_Boolean;

   ----------------
   -- Write_Null --
   ----------------

   procedure Write_Null (Self : in out JSON_Writer) is
   begin
      Push_Value (Self, League.JSON.Values.Null_JSON_Value);
   end Write_Null;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : JSON_Writer) return String is
   begin
      return To_Universal_String (Self).To_UTF_8_String;
   end To_String;

end PB_Support.JSON;
