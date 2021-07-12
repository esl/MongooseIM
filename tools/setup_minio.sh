#!/bin/bash

# cd to repo
cd "$(dirname "$0")/../"

## see https://github.com/minio/minio/issues/4769 for more examples

minio_docker_name="mongooseim-minio"
minio_access_key="AKIAIAOAONIULXQGMOUA"
minio_secret_key="CG5fGqG0/n6NCPJ10FylpdgRnuV52j8IZvU7BSj8"
minio_bucket="mybucket"

docker rm -v -f "${minio_docker_name}" || echo "Skip removing previous container"

docker run -d -p 9000:9000 \
    --name "${minio_docker_name}" \
    -e "MINIO_ACCESS_KEY=${minio_access_key}" \
    -e "MINIO_SECRET_KEY=${minio_secret_key}" \
    minio/minio server /data

# Pulling while waiting
docker pull minio/mc &

tools/wait_for_service.sh "${minio_docker_name}" 9000

mc_cmd="$(cat <<-EOF
  mc config host add myminio http://minio:9000 "${minio_access_key}" "${minio_secret_key}" &&
  mc mb "myminio/${minio_bucket}" &&
  mc policy set download "myminio/${minio_bucket}"
  exit 0
EOF
)"

# The config script in ${mc_cmd} needs to be run in `minio/mc` container
# because `minio/server` container doesn't have the `mc` command.
docker run --rm --entrypoint sh \
    --link "${minio_docker_name}:minio" \
    minio/mc -c "${mc_cmd}"
