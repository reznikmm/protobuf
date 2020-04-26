# protobuf

[![reuse compliant](https://img.shields.io/badge/reuse-compliant-green.svg)](https://reuse.software/)

> Naive Google Protocol Buffers implementation in Ada

Not implemented features:
* [oneof](https://developers.google.com/protocol-buffers/docs/proto3#oneof)
* packed strings and bytes fields
* [ZigZag](https://developers.google.com/protocol-buffers/docs/encoding#signed-integers) types
* messages with cycle dependencies
* ...

## Install

Run
```
make all install PREFIX=/path/to/install
```

### Dependencies
It depends on
* [Matreshka](https://forge.ada-ru.org/matreshka) library.
* [ada_pretty](https://github.com/reznikmm/ada-pretty/tree/master) - pretty printing library.
* `protoc` - protobuf compiler

## Usage
Launch `protoc` with `--ada_out=<dir>` option having `protoc-gen-ada` in
the `PATH`.

Example:

```
PATH=.:$PATH protoc --ada_out=/tmp/ /usr/include/google/protobuf/descriptor.proto
```


## Maintainer

[@MaximReznik](https://github.com/reznikmm).

## Contribute

Feel free to dive in!
[Open an issue](https://github.com/reznikmm/protobuf/issues/new)
or submit PRs.

## License

[MIT](LICENSE) © Maxim Reznik

