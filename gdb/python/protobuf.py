"""
Pretty-printers for vectors in Protobuf. It depends on https://github.com/AdaCore/gnat-gdb-scripts
Source this file in ~/.gdbinit to activate the pretty-printers.
"""

from typing import Iterable, Optional, Tuple, Union

import gdb

from gnatdbg.generics import Match
from gnatdbg.printers import PrettyPrinter
from gnatdbg.utils import pretty_typename
from gnatdbg.printers import GDBPrettyPrinters


class PbVectorPrinter(PrettyPrinter):
    """Pretty-print Protobuf vector values."""

    name = "Vector"

    type_pattern = Match.TypeName(
        suffix="vector",
        pattern=Match.Struct(
            Match.Field("_parent", Match.Struct()),
            Match.Field("data", Match.Typedef()),
            Match.Field("length", Match.Integer()),
        ),
    )

    def display_hint(self) -> str:
        return "array"

    @property
    def array_bounds(self) -> Tuple[int, int]:
        last_index = int(self.value["length"])
        if last_index < 1:
            return (1, 0)
        return (1, last_index)

    @property
    def length(self) -> int:
        first, last = self.array_bounds
        if first <= last:
            return last - first + 1
        else:
            return 0

    @property
    def array_elements(self) -> Optional[gdb.Value]:
        first, last = self.array_bounds
        if first <= last:
            base_value = self.value["data"]["P_ARRAY"].dereference()
            return base_value.cast(base_value.type.target().array(first, last))
        else:
            return None

    def element(self, index: Union[int, gdb.Value]) -> gdb.Value:
        idx = int(index)
        first, last = self.array_bounds
        if first <= idx and idx <= last:
            elts = self.array_elements
            assert elts is not None
            return elts[idx]
        else:
            raise gdb.MemoryError(
                "Out of bound vector access ({} not in {} ..  {})".format(
                    index, first, last
                )
            )

    def children(self) -> Iterable[Tuple[str, gdb.Value]]:
        elements = self.array_elements
        if elements:
            first_index, last_index = elements.type.range()
            for i in range(first_index, last_index + 1):
                elt = elements[i]
                elt.fetch_lazy()
                yield ("[{}]".format(i), elt)

    def to_string(self) -> str:
        return "{} of length {}".format(
            pretty_typename(self.value.type), self.length
        )

_printers = GDBPrettyPrinters("protobuf")
_printers.append(PbVectorPrinter)

gdb.printing.register_pretty_printer(gdb.current_objfile(), _printers)
