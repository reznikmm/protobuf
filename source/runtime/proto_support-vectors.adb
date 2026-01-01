--  SPDX-FileCopyrightText: 2020-2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--

with Ada.Unchecked_Deallocation;

package body Proto_Support.Vectors is

   procedure Free is new Ada.Unchecked_Deallocation
     (Element_Array, Element_Array_Access);

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Element_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   ------------
   -- Append --
   ------------

   procedure Append (Self : in out Vector; Value : Element_Type) is
      Old : Element_Array_Access := Self.Data;
      Init_Length : constant Positive :=
        Positive'Max (1, 256 / Natural'Max (1, Element_Type'Size));
   begin
      if Self.Length = 0 then
         Self.Data := new Element_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data := new Element_Array'
           (Old.all & (1 .. Self.Length => <>));
         Free (Old);
      end if;

      Self.Length := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Vector) is
   begin
      Self.Length := 0;
   end Clear;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Vector) is
   begin
      if Self.Data /= null then
         Free (Self.Data);
      end if;
   end Finalize;

   ---------
   -- Get --
   ---------

   function Get (Self : Vector; Index : Positive) return Element_Type is
   begin
      return Self.Data (Index);
   end Get;

   ------------
   -- Length --
   ------------

   function Length (Self : Vector) return Natural is
   begin
      return Self.Length;
   end Length;

end Proto_Support.Vectors;
