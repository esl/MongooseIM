## Installation

There are multiple ways in which you can get MongooseIM:

* install MongooseIM binaries from a package Erlang Solutions delivers.
* get the [Docker image](https://hub.docker.com/r/mongooseim/mongooseim).
* use the [Helm chart](https://artifacthub.io/packages/helm/mongoose/mongooseim).

Alternatively, check out our tutorial on [How to build MongooseIM from source code](../user-guide/How-to-build.md) for an introduction to compiling, building and testing MongooseIM.

In this guide, we will use the packaged version.
Go to the [downloads](https://www.erlang-solutions.com/resources/download.html) section of the Erlang Solutions website, and choose the version of MongooseIM you want. The following sections describe the installation process for different operating systems.

### Ubuntu and Debian

Once the deb file is downloaded, open a terminal window and navigate to the directory containing the package. Use the following command to unpack and install MongooseIM:

```bash
sudo dpkg -i mongooseim_[version here].deb
```

### CentOS

An ODBC (RDBMS) driver must be installed on your machine to unpack and install from RPM packages. Enter the following command in a terminal window to install the latest unixODBC driver:
```bash
sudo yum install unixODBC
```
Once the RPM file is downloaded, open a terminal window and navigate to the directory containing the package. Use the following command to unpack and install MongooseIM:
```bash
sudo rpm -i mongooseim_[version here].rpm
```
