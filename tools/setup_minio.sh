#!/bin/bash

# cd to repo
cd "$(dirname "$0")/../"
source tools/common-vars.sh

source tools/db-versions.sh

## see https://github.com/minio/minio/issues/4769 for more examples
  
minio_docker_name="mongooseim-minio"
minio_access_key="AKIAIAOAONIULXQGMOUA"
minio_secret_key="CG5fGqG0/n6NCPJ10FylpdgRnuV52j8IZvU7BSj8"
minio_bucket="mybucket"

$DOCKER rm -v -f "${minio_docker_name}" || echo "Skip removing previous container"

IMAGE="minio/minio:$MINIO_VERSION"
MC_IMAGE="minio/mc:$MINIO_MC_VERSION"

$DOCKER run -d -p 9000:9000 \
    --name "${minio_docker_name}" \
    -e "MINIO_ACCESS_KEY=${minio_access_key}" \
    -e "MINIO_SECRET_KEY=${minio_secret_key}" \
    $IMAGE server /data

tools/wait_for_service.sh "${minio_docker_name}" 9000

MINIO_IP=$(docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' $minio_docker_name)

mc_cmd="$(cat <<-EOF
  mc config host add myminio http://${MINIO_IP}:9000 "${minio_access_key}" "${minio_secret_key}" &&
  mc mb "myminio/${minio_bucket}" &&
  mc policy set download "myminio/${minio_bucket}"
  exit 0
EOF
)"

# The config script in ${mc_cmd} needs to be run in `minio/mc` container
# because `minio/server` container doesn't have the `mc` command.
$DOCKER run --rm --entrypoint sh $MC_IMAGE -c "${mc_cmd}"
