# How to build and run MongooseIM docker image

The instruction below will guide you through the process of building and running the MongooseIM docker image.

## Requirements

To follow this guide you need to have [docker](https://www.docker.com/) installed and the MongooseIM GitHub repository cloned locally.

## Building docker image

To build a MongooseIM image, navigate to the main repo directory (referenced as `$REPO` in this guide) and execute:

```bash
./tools/build-docker-from-remote.sh
```

which will build a MongooseIM docker image based on the current local commit if it is available on the remote.

Alternatively, it is possible to build a docker image based on any commit available on remote (commit hash referenced as `$COMMIT_HASH`), by executing:

```bash
./tools/build-docker-from-remote.sh $COMMIT_HASH
```

## Running docker image

Full tutorial on running a docker image is available on [mongooseim-docker](https://github.com/esl/mongooseim-docker) GitHub. Here only simple and one-node configuration will be presented. In order to run it execute:

```bash
docker run -dt -h first-node --name first-node -e JOIN_CLUSTER=false mongooseim
```

which starts a single MongooseIM node named `first-node`.
