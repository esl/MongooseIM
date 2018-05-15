# Package build scripts for MongooseIM

Build MongooseIM packages locally in a container.
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


## Building the build container

Run:

```sh
./build centos6
```

Specify `centos7` if that's the platform you want the RPM for.

To build the container you need to have read access to
https://github.com/esl/package-buildscripts.

Available platforms are listed under `platforms/`.
Adding support for a new platform is basically writing a Dockerfile
which will preinstall all the build dependencies for the project.
The build container image is reused for consecutive builds,
so make sure to do as much as possible when building the build container
to effectively shorten package build times later.


## Running the build container and building a package

Run:

```sh
$ ./run centos6
[root@centos6 /]# ./build TAGGED-PUBLIC-VERSION
```

Again, you can specify `centos7` or any other platform.
If all goes well, the package will land in `packages/`
subdirectory - mounted as a container volume at `/packages`.
The container instance is discarded once you exit the shell.

Repository to build MongooseIM from can be overridden by exporting
`MONGOOSEIM_REPO` inside the container, but before running `build`.

In the rare case of changing the package build scripts,
but not released code itself, it's also possible to specify package
revision:

```sh
[root@centos6 /]# ./build TAGGED-PUBLIC-VERSION PACKAGE-REVISION
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

TODO: add git hook example


## Publishing the package

Once you've built the package and it's available in `packages/`:

```
./publish TAGGED-PUBLIC-VERSION [PACKAGE-REVISION]
```

As indicated by square brackets `PACKAGE-REVISION` is optional.

Check or modify `env/publish` for the location where the package is published.
This will vary depending on your project.

## Building a Debian build container

In `platforms/debian_stretch` there is Dockerfile and buildscripts for image
with all dependencies required to build Debian packages.
The container image is already uploaded to DockerHub repo
as `erlangsolutions/mongooseim.debian_stretch.builder`,
so there is no reason to build it.
However it might be rebuilt and pushed with:

```
cd platforms/debian_stretch
docker build -t erlangsolutions/mongooseim.debian_stretch.builder .
docker push erlangsolutions/mongooseim.debian_stretch.builder
```

To start container out of this image, one needs just to run:

```
cd tools/pkg
./run debian_stretch
```
