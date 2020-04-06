# Package build scripts for MongooseIM

`build.sh` script builds the MongooseIM package for different operating systems
with usage of Docker. `build.sh` bases on the source code passed as a docker
context, commands contained in a dockerfile and building scripts. The process of
building the package runs during docker image building. Copying of the ready
package takes place after running the docker image containing it.

Source code version used for building a package is set by checking out the desired
git reference in the project.

Contents:

```sh
.
├── README.md            # This file
├── build.sh*            # Build package script
├── Dockerfile_{deb,rpm} # Instructions for building different platforms packages
├── scripts/             # Files and scripts used to build packages
├── packages/            # Preferable directory for the built packages
├── env/                 # Environment variable definitions
├── publish.sh*          # Script to publish ready packages
```


## Building a package

To build a package run:

```sh
  ./build.sh \
    --platform $PLATFORM \
    --version $VERSION \
    --revision $REVISION \
    --erlang_version $ERLANG_VERSION \
    --dockerfile_path "$DOCKERFILE_PATH" \
    --context_path $CONTEXT_PATH \
    --built_packages_directory "$BUILT_PACKAGES_DIRECTORY"
```

Where:

* `$PLATFORM` - an OS and an OS version name separated by "_" (e.g. centos_7,
debian_stretch),
* `$VERSION` - a version of MongooseIM (for most cases version from the `VERSION`
file will be suitable),
* `$REVISION` - a revision of a package (should be increased each time a package
is built for the same source code but with the usage of changed build scripts),
* `$ERLANG_VERSION` - a version of the esl-erlang package which should be used
while compiling MongooseIM (please remember about concerning minimal erlang version
specified in the `rebar.config` file and the esl-erlang package revision - e.g. 22.2.5-2),
* `DOCKERFILE_PATH` - a dockerfile path which should be used to build a package
for given platform (e.g. path of `Dockerfile_rpm` for `centos_7`),
* `CONTEXT_PATH` - a root directory of the MongooseIM project (during building
whole source code is copied to a building docker image container and the `_build`
directory is erased),
* `BUILT_PACKAGES_DIRECTORY` - a directory in which ready package will be placed.

If all goes well, the package will land in the`$built_packages_directory`
subdirectory - mounted as a container bind mount at `/built_packages` in the
container. The container instance is removed once the build finishes.

A resulting package will be called:

```
mongooseim_3.6.0-1~centos~7_amd64.rpm
```
For passed `version`: "3.6.0", `revision`: "1" and `platform`: "centos_7".

## Sample configuration

Below variables can be used to build a default package for Debian Stretch:

```
PROJECT_ROOT=$(git rev-parse --show-toplevel)
PLATFORM="debian_stretch"
VERSION=$(cat "${PROJECT_ROOT}/VERSION")
REVISION="1"
ERLANG_VERSION="22.2.5-1"
DOCKERFILE_PATH="$PROJECT_ROOT/tools/pkg/Dockerfile_deb"
CONTEXT_PATH=$PROJECT_ROOT
BUILT_PACKAGES_DIRECTORY="$PROJECT_ROOT/tools/pkg/packages"
```

## Setting package version and revision

A package version consists of a MongooseIM `version` and a `revision` of a package. 
Their default and recommended values were showed above. It is possible though
that a package will be built for non tagged version of the source code.

Setting `version` and `revision` is up to creator of a package but following is
recommended:

* always set: `VERSION=$(cat "${PROJECT_ROOT}/VERSION")`,
* while building a package for tagged source code use: `REVISION="1"`,
* while building a package for non tagged source code try to indicate what source
code was used (e.g. by adding a commit hash to the revision: 
`REVISION="1.$(git rev-parse --short HEAD)"`).

Make sure to keep the `"${PROJECT_ROOT}/VERSION"` and passed `$VERSION`
in sync - otherwise, it will become impossible (or hard at least) to track
what code version went into which package.

Passed `$VERSION` is also used as a version of the Erlang release which will
be installed from the package. The operations people will be grateful for staying
sane if you make sure that the package name/version/revision, and source code
version from which package was built all match.

## Publishing the package

Once you've built the package and it's available in `packages/`:

```
./publish VERSION [PACKAGE-REVISION]
```

As indicated by square brackets `PACKAGE-REVISION` is optional.

Check or modify `env/publish` for the location where the package is published.
This will vary depending on your project.
