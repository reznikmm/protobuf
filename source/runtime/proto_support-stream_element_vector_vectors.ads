--  SPDX-FileCopyrightText: 2020-2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--

with Proto_Support.Vectors;
with League.Stream_Element_Vectors;

package Proto_Support.Stream_Element_Vector_Vectors is
  new Proto_Support.Vectors (League.Stream_Element_Vectors.Stream_Element_Vector);
pragma Preelaborate (Proto_Support.Stream_Element_Vector_Vectors);
