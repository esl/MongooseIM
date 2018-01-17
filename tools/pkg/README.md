# Package build scripts for MongooseIM

Build MongooseIM packages locally in containerised environments.
Contents:

```sh
.
├── README.md       # This file
├── build*          # Script to build build containers
├── env/            # Environment variable definitions
├── files/          # Files provisioned to the build containers
├── packages/       # Shiny new packages go here
├── platforms/      # Dockerfile definitions for your Linux distro flavor
├── publish         # Script to publish ready packages
└── run             # Script to run build containers, i.e. build packages
```


## Building the build container

Run:

```sh
./build centos6
```

Specify `centos7` if that's the platform you want the RPM for.
If you get one of the errors shown below:

```sh
./build: files/esl-sign-private.key not available
./build: files/necromancer.gpg.key not available
```

Contact [Bartosz Szafran][email:bartek]
or [Radek Szymczyszyn][email:radek] to get the keys which will allow
the build container to sign packages.

To build the container you need to have read access to
https://github.com/esl/package-buildscripts.

[email:bartek]: mailto:bartosz.szafran@erlang-solutions.com
[email:radek]: mailto:radoslaw.szymczyszyn@erlang-solutions.com


## Running the build container and building a package

Run:

```sh
$ ./run centos6
[root@centos6 /]# ./build TAGGED-PUBLIC-VERSION
```

Again, you can specify `centos7` in this case, too.
You will be asked for verifying GitHub's SSH key fingerprint
and at the end of the process for entering the signing key password.
The latter is empty, just press enter.
If all goes well, the package will land in `packages`
subdirectory - mounted as a container volume.


## Publishing the package

Once you've built the package and it's available in `packages/`:

```
./publish TAGGED-PUBLIC-VERSION
```

See/modify `env/publish` for the location where the package is published.
