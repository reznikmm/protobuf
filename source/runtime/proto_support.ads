--  SPDX-FileCopyrightText: 2020-2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--

package Proto_Support is

   pragma Pure;

   type Field_Number is range 1 .. 2 ** 29 - 1;

   type Wire_Type is
     (Var_Int,
      Fixed_64,
      Length_Delimited,
      Start_Group,
      End_Group,
      Fixed_32);

   type Key is record
      Field    : Field_Number;
      Encoding : Wire_Type;
   end record;

end Proto_Support;
