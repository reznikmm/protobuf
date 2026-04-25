package body PB_Support.JSON is

   use Ada.Strings.Unbounded;

   ------------------
   -- Start_Object --
   ------------------

   procedure Start_Object (Self : in out JSON_Writer) is
   begin
      if Self.Needs_Comma then
         Append (Self.Text, ",");
      end if;
      Append (Self.Text, "{");
      Self.Needs_Comma := False;
   end Start_Object;

   ----------------
   -- End_Object --
   ----------------

   procedure End_Object (Self : in out JSON_Writer) is
   begin
      Append (Self.Text, "}");
      Self.Needs_Comma := True;
   end End_Object;

   -----------------
   -- Start_Array --
   -----------------

   procedure Start_Array (Self : in out JSON_Writer) is
   begin
      if Self.Needs_Comma then
         Append (Self.Text, ",");
      end if;
      Append (Self.Text, "[");
      Self.Needs_Comma := False;
   end Start_Array;

   ---------------
   -- End_Array --
   ---------------

   procedure End_Array (Self : in out JSON_Writer) is
   begin
      Append (Self.Text, "]");
      Self.Needs_Comma := True;
   end End_Array;

   ---------------
   -- Write_Key --
   ---------------

   procedure Write_Key (Self : in out JSON_Writer; Name : String) is
   begin
      if Self.Needs_Comma then
         Append (Self.Text, ",");
      end if;
      Append (Self.Text, """");
      Append (Self.Text, Name);
      Append (Self.Text, """:");
      Self.Needs_Comma := False;
   end Write_Key;

   ------------------
   -- Write_String --
   ------------------

   procedure Write_String (Self : in out JSON_Writer; Value : String) is
   begin
      if Self.Needs_Comma then
         Append (Self.Text, ",");
      end if;
      Append (Self.Text, """");
      --  TODO: Handle escaping of specific characters
      Append (Self.Text, Value);
      Append (Self.Text, """");
      Self.Needs_Comma := True;
   end Write_String;

   -------------------
   -- Write_Integer --
   -------------------

   procedure Write_Integer
      (Self  : in out JSON_Writer;
       Value : Long_Long_Integer)
   is
      S : constant String := Long_Long_Integer'Image (Value);
   begin
      if Self.Needs_Comma then
         Append (Self.Text, ",");
      end if;
      if S (S'First) = ' ' then
         Append (Self.Text, S (S'First + 1 .. S'Last));
      else
         Append (Self.Text, S);
      end if;
      Self.Needs_Comma := True;
   end Write_Integer;

   -----------------
   -- Write_Float --
   -----------------

   procedure Write_Float (Self : in out JSON_Writer; Value : Long_Float) is
      S : constant String := Long_Float'Image (Value);
   begin
      if Self.Needs_Comma then
         Append (Self.Text, ",");
      end if;
      --  TODO: Ensure valid float parsing/printing mapping in JSON 
      --  (No trailing zeros required, no exponential if possible)
      if S (S'First) = ' ' then
         Append (Self.Text, S (S'First + 1 .. S'Last));
      else
         Append (Self.Text, S);
      end if;
      Self.Needs_Comma := True;
   end Write_Float;

   -------------------
   -- Write_Boolean --
   -------------------

   procedure Write_Boolean (Self : in out JSON_Writer; Value : Boolean) is
   begin
      if Self.Needs_Comma then
         Append (Self.Text, ",");
      end if;
      if Value then
         Append (Self.Text, "true");
      else
         Append (Self.Text, "false");
      end if;
      Self.Needs_Comma := True;
   end Write_Boolean;

   ----------------
   -- Write_Null --
   ----------------

   procedure Write_Null (Self : in out JSON_Writer) is
   begin
      if Self.Needs_Comma then
         Append (Self.Text, ",");
      end if;
      Append (Self.Text, "null");
      Self.Needs_Comma := True;
   end Write_Null;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : JSON_Writer) return String is
   begin
      return To_String (Self.Text);
   end To_String;

end PB_Support.JSON;