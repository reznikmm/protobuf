# protobuf

[![Build Status](https://github.com/reznikmm/protobuf/workflows/Build/badge.svg)](https://github.com/reznikmm/protobuf/actions)
[![Copr build status](https://copr.fedorainfracloud.org/coprs/reznik/ada/package/ada-protobuf/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/reznik/ada)

> A Google Protocol Buffers implementation in Ada

## Install

The preferred installation method is compiling from sources. Just checkout repository and run:
```
make all install PREFIX=/path/to/install
```

There is also [a precompiled RPM for Fedora Linux](https://copr.fedorainfracloud.org/coprs/reznik/ada/).

### Dependencies
It depends on
* [matreshka](https://forge.ada-ru.org/matreshka) - the Matreshka framework.
* [ada_pretty](https://github.com/reznikmm/ada-pretty) - an Ada pretty printing library.
* [protoc](https://github.com/protocolbuffers/protobuf) - the protobuf compiler.

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

## Support

If you find this open source project useful, you may provide me a small support
on [Patreon](https://www.patreon.com/ada_ru).

## License

[MIT](LICENSE) © Maxim Reznik
