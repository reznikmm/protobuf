--  SPDX-FileCopyrightText: 2020-2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--

with Interfaces;
with Proto_Support.Options;

package Proto_Support.IEEE_Float_64_Options is
  new Proto_Support.Options (Interfaces.IEEE_Float_64);
pragma Pure (Proto_Support.IEEE_Float_64_Options);
