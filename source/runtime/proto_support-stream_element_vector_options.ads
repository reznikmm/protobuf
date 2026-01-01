--  SPDX-FileCopyrightText: 2020-2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--

with Proto_Support.Options;
with Proto_Support.Stream_Element_Vectors;

package Proto_Support.Stream_Element_Vector_Options is
  new Proto_Support.Options (Proto_Support.Stream_Element_Vectors.Vector);
pragma Preelaborate (Proto_Support.Stream_Element_Vector_Options);
