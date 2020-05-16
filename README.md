# protobuf

[![Build Status](https://github.com/reznikmm/protobuf/workflows/Build/badge.svg)](https://github.com/reznikmm/protobuf/actions)
[![Download](https://api.bintray.com/packages/reznikmm/matreshka/protobuf/images/download.svg) ](https://bintray.com/reznikmm/matreshka/protobuf/_latestVersion)

> Naive Google Protocol Buffers implementation in Ada

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
PATH=.objs/compiler/:$PATH protoc --ada_out=/tmp/ /usr/include/google/protobuf/descriptor.proto
```

See more details in [Ada Generated Code](docs/generated_code.md) document.

## Maintainer

[@MaximReznik](https://github.com/reznikmm).

## Contribute

Feel free to dive in!
[Open an issue](https://github.com/reznikmm/protobuf/issues/new)
or submit PRs.

## License

[MIT](LICENSE) © Maxim Reznik

