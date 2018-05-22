# Package build scripts for MongooseIM

Build MongooseIM packages in a local container.
Contents:

```sh
.
├── README.md       # This file
├── build*          # Script to build build containers
├── env/            # Environment variable definitions
├── files/          # Files provisioned to the build containers
├── packages/       # Shiny new packages go here
├── platforms/      # Dockerfile definitions for your Linux distro flavor
├── publish*        # Script to publish ready packages
└── run*            # Script to run build containers, i.e. build packages
```


## Building a build container

Run:

```sh
./build PLATFORM
```

Look into `platforms/` for valid values of `PLATFORM`.

For example, specify `centos7` if you want to build an `.rpm` or `debian_stretch` to get a `.deb`
package for the relevant version of the system.

Adding support for a new platform is basically writing a Dockerfile
which will preinstall all the build dependencies for the project.
The build container image is reused for consecutive builds,
so make sure to do as much as possible when building the build container
to effectively shorten package build times later.


## Building a package

Run:

```sh
$ ./run PLATFORM <VERSION>
```

Look into `platforms/` for valid values of `PLATFORM` - it is the same thing
as when building a build container.

If all goes well, the package will land in `packages/`
subdirectory - mounted as a container volume at `/packages`.
The container instance is removed once the build finishes.

Repository to build MongooseIM from can be overridden by exporting
the `MONGOOSEIM_REPO` environment variable.
The default is the official MongooseIM repository: https://github.com/esl/mongooseim.git

In the rare case of changing the package build scripts,
but not released code itself, it's also possible to specify package
revision:

```sh
$ ./run PLATFORM <VERSION> <PACKAGE-REVISION>
```

The default `PACKAGE-REVISION` is 1, so if you have to specify it,
you'll most likely use 2, 3... and so on.

A resulting package will, for example, be called:

```
mongooseim-2.1.0-1~centos7.x86_64.rpm
```

For version 2.1.0, revision 1 and platform `centos7`.


### Tagging versions

`TAGGED-PUBLIC-VERSION` is the reference which will be built and packaged.
However, the package name will contain content of the `VERSION` file from the
repository's top level directory.
Make sure to keep the tagged versions and `VERSION` in sync - otherwise,
it will become impossible (or hard at least) to track what code
version went into which package.

`VERSION` is also used as version of the Erlang release
which will be installed from the package.
The operations people will be grateful for staying sane if you make sure
that the package name/version, release version and git tag all match.

Git hooks can be a great reminder here!

```sh
#!/usr/bin/env sh

# Make sure VERSION file and git tag define the same MongooseIM version.
#
# Put in:
#
#   <MONGOOSEIM_ROOT>/.git/hooks/pre-commit
#
# or
#
#   <MONGOOSEIM_ROOT>/.git/hooks/post-commit
#
# depending on whether you want to be reminded before or after
# entering the commit message.
# When this script fails committing will also fail.

set -e

VERSION=$(cat VERSION)
GIT_TAG=$(git describe --tags)

if [ x"$VERSION" = x"$GIT_TAG" ]; then
    :
else
    echo "Versions do not match!"
    echo "VERSION file: " $VERSION
    echo "git tag:      " $GIT_TAG
    echo
    echo "Set IGNORE_VERSION_CHECK to commit anyway."
    echo
    if [ x"$IGNORE_VERSION_CHECK" != x"" ]; then
        exit 0
    else
        exit 1
    fi
fi

exit 0
```


## Publishing the package

Once you've built the package and it's available in `packages/`:

```
./publish TAGGED-PUBLIC-VERSION [PACKAGE-REVISION]
```

As indicated by square brackets `PACKAGE-REVISION` is optional.

Check or modify `env/publish` for the location where the package is published.
This will vary depending on your project.
