--  SPDX-FileCopyrightText: 2020-2025 Max Reznik <reznikmm@gmail.com>
--
--  Copyright (c) 2020-2025 Max Reznik
--

package body Proto_Support.Stdio_Streams is

   -----------
   -- Flush --
   -----------

   procedure Flush (Self : in out Stdio_Stream'Class) is
      Ignore : Interfaces.C_Streams.int;
   begin
      Ignore := Interfaces.C_Streams.fflush (Self.stdout);
   end Flush;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : in out Stdio_Stream'Class;
      stdin  : Interfaces.C_Streams.FILEs := Interfaces.C_Streams.stdin;
      stdout : Interfaces.C_Streams.FILEs := Interfaces.C_Streams.stdout) is
   begin
      Interfaces.C_Streams.set_mode
        (Interfaces.C_Streams.fileno (stdin), Interfaces.C_Streams.None);

      Interfaces.C_Streams.set_mode
        (Interfaces.C_Streams.fileno (stdout), Interfaces.C_Streams.None);

      Self.stdin := stdin;
      Self.stdout := stdout;
   end Initialize;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self : in out Stdio_Stream;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Count;

      Size : constant Interfaces.C_Streams.size_t :=
        Interfaces.C_Streams.fread
          (buffer => Item'Address,
           size   => 1,
           count  => Item'Length,
           stream => Self.stdin);
   begin
      Last := Item'First + Ada.Streams.Stream_Element_Count (Size) - 1;
   end Read;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : in out Stdio_Stream;
      Item : Ada.Streams.Stream_Element_Array)
   is
      use type Interfaces.C_Streams.size_t;

      Size : constant Interfaces.C_Streams.size_t :=
        Interfaces.C_Streams.fwrite
          (buffer => Item'Address,
           size   => 1,
           count  => Item'Length,
           stream => Self.stdout);
   begin
      pragma Assert (Size = Item'Length);
   end Write;

end Proto_Support.Stdio_Streams;
