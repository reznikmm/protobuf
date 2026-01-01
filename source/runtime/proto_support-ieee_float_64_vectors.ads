--  SPDX-FileCopyrightText: 2020-2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--

with Interfaces;
with Proto_Support.Vectors;

package Proto_Support.IEEE_Float_64_Vectors is
  new Proto_Support.Vectors (Interfaces.IEEE_Float_64);

pragma Preelaborate (Proto_Support.IEEE_Float_64_Vectors);
