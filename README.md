Protocol Buffers in Ada _(protobuf)_
====================================

[![reuse compliant](https://img.shields.io/badge/reuse-compliant-green.svg)](https://reuse.software/)

> Naive Google Protocol Buffers implementation

## Install

Run
```
make all install PREFIX=/path/to/install
```

### Dependencies
It depends on
* [Matreshka](https://forge.ada-ru.org/matreshka) library.
* [Ada_Pretty](https://github.com/reznikmm/ada-pretty) library.

## Usage
Add `protoc-gen-ada` to PATH and run:
```
protoc --ada_out=<DIR> <YOUR.proto>
```

## Maintainer

[@MaximReznik](https://github.com/reznikmm).

## Contribute

Feel free to dive in!
[Open an issue](https://github.com/reznikmm/protobuf/issues/new) or submit PRs.

## License

[MIT](LICENSE) © Maxim Reznik

