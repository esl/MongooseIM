# Installation

There are multiple ways in which you can get MongooseIM:

* install MongooseIM binaries from a [package](#packages) Erlang Solutions delivers,
* get the [Docker image](#docker),
* use the [Helm chart](#helm).

Alternatively, check out our tutorial on [How to build MongooseIM from source code](../tutorials/How-to-build.md) for an introduction to compiling, building and testing MongooseIM.

## Packages

Go to the [latest release](https://github.com/esl/MongooseIM/releases/latest) page and select the package compatible with your system version.
The following sections describe the installation process for different operating systems.


=== "Ubuntu and Debian"

    Once the deb file is downloaded, open a terminal window and navigate to the directory containing the package. Use the following command to unpack and install MongooseIM:

    ```bash
    sudo dpkg -i mongooseim_[version here].deb
    ```

=== "CentOS compatible: AlmaLinux / Rocky Linux"

    Once the RPM file is downloaded, open a terminal window and navigate to the directory containing the package. Use the following command to unpack and install MongooseIM:

    ```bash
    sudo rpm -i mongooseim_[version here].rpm
    ```

!!! info
    Packages for older systems (Ubuntu Focal, Debian Bullseye, Debian Buster, AlmaLinux 8, Rocky Linux 8)
    include a bundled OpenSSL 3.x, as this version is required for MongooseIM to function correctly,
    but is not available in the official repositories for these distributions.
    If your system already has OpenSSL 3.x installed, the package will detect and use the system version instead of the bundled one.

## Docker

In order to install MongooseIM using Docker, simply run the following command:

```bash
docker pull erlangsolutions/mongooseim
```

This will download the latest release.
You can use tags to download an exact version.

We build Docker images for every release marked with a git tag, as well as for every Pull Request.
You can see all of them on [DockerHub](https://hub.docker.com/r/erlangsolutions/mongooseim).
In order to learn more about how the images are built, please visit the [source code repository](https://github.com/esl/mongooseim-docker).

The `mongooseimctl` command is available in `/usr/lib/mongooseim/bin/mongooseimctl` in the container.

## Helm

You can easily install MongooseIM to a Kubernetes cluster with the help of our [Helm chart](https://artifacthub.io/packages/helm/mongoose/mongooseim), defined in the [source code repository](https://github.com/esl/MongooseHelm).
After you have a Kubernetes cluster set up, simply run:

```bash
helm repo add mongoose https://esl.github.io/MongooseHelm/
```

to add our chart repository, and then:

```bash
helm install my-mongooseim mongoose/mongooseim
```

to install the chart.
You can use any name instead of `my-mongooseim`, or generate a random name.

## Source

Please see the tutorial [How to build MongooseIM from source code](../tutorials/How-to-build.md).
