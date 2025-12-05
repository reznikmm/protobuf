--  SPDX-FileCopyrightText: 2020-2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--

with Proto_Support.Vectors;
with Interfaces;

package Proto_Support.Integer_64_Vectors is
  new Proto_Support.Vectors (Interfaces.Integer_64);
pragma Preelaborate (Proto_Support.Integer_64_Vectors);
