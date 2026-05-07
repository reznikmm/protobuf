--  SPDX-License-Identifier: MIT

package body PB_Support.Memory_Streams is

   procedure Clear (Self : in out Memory_Stream'Class) is
   begin
      Self.Data.Clear;
      Self.Read := 0;
   end Clear;

   function Written
     (Self : Memory_Stream'Class) return Ada.Streams.Stream_Element_Count is
   begin
      return Ada.Streams.Stream_Element_Count (Self.Data.Length);
   end Written;

   function Data (Self : Memory_Stream'Class)
     return PB_Support.Basics.Stream_Element_Vector is
   begin
      return Self.Data;
   end Data;

   overriding procedure Read
     (Self : in out Memory_Stream;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Count;
      Available : constant Ada.Streams.Stream_Element_Count :=
        Ada.Streams.Stream_Element_Count (Self.Data.Length) - Self.Read;
      Count     : constant Ada.Streams.Stream_Element_Count :=
        Ada.Streams.Stream_Element_Count'Min (Item'Length, Available);
   begin
      for J in 1 .. Count loop
         Item (Item'First + J - 1) := Self.Data.Element
           (Positive (Self.Read + J));
      end loop;
      Self.Read := Self.Read + Count;
      Last := Item'First + Count - 1;
   end Read;

   overriding procedure Write
     (Self : in out Memory_Stream;
      Item : Ada.Streams.Stream_Element_Array)
   is
   begin
      for J in Item'Range loop
         Self.Data.Append (Item (J));
      end loop;
   end Write;

end PB_Support.Memory_Streams;
