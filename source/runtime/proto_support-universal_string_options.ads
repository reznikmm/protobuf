--  SPDX-FileCopyrightText: 2020-2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--

with Proto_Support.Options;
with League.Strings;

package Proto_Support.Universal_String_Options is
  new Proto_Support.Options (League.Strings.Universal_String);
pragma Preelaborate (Proto_Support.Universal_String_Options);
