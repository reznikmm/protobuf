--  SPDX-FileCopyrightText: 2020-2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--

with Proto_Support.Options;
with Interfaces;

package Proto_Support.Integer_32_Options is
  new Proto_Support.Options (Interfaces.Integer_32);
pragma Pure (Proto_Support.Integer_32_Options);
