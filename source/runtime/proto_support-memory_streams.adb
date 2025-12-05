--  SPDX-FileCopyrightText: 2020-2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--

package body Proto_Support.Memory_Streams is

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Memory_Stream'Class) is
   begin
      Self.Data.Clear;
      Self.Read := 0;
   end Clear;

   ----------
   -- Data --
   ----------

   function Data (Self : Memory_Stream'Class)
     return League.Stream_Element_Vectors.Stream_Element_Vector is
   begin
      return Self.Data;
   end Data;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self : in out Memory_Stream;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Count : constant Ada.Streams.Stream_Element_Offset :=
        Ada.Streams.Stream_Element_Offset'Min
          (Item'Length, Self.Data.Length - Self.Read);
   begin
      for J in 1 .. Count loop
         Item (Item'First + J - 1) := Self.Data.Element (Self.Read + J);
      end loop;

      Last := Item'First + Count - 1;
      Self.Read := Self.Read + Count;
   end Read;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : in out Memory_Stream;
      Item : Ada.Streams.Stream_Element_Array) is
   begin
      Self.Data.Append (Item);
   end Write;

   -------------
   -- Written --
   -------------

   function Written
     (Self : Memory_Stream'Class) return Ada.Streams.Stream_Element_Count is
   begin
      return Self.Data.Length;
   end Written;

end Proto_Support.Memory_Streams;
