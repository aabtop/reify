#!/bin/bash

THIS_SCRIPT_LOCATION="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

HOST_SRC_DIR_SHELL=${THIS_SCRIPT_LOCATION}
HOST_OUT_DIR_SHELL=${THIS_SCRIPT_LOCATION}/docker_out
DOCKERFILE_DIRECTORY=${THIS_SCRIPT_LOCATION}

# Make sure the host output directory exists.
mkdir -p $HOST_OUT_DIR_SHELL

RUN_COMMAND="/src/src/build_in_docker.sh"

# Enable the user to override some default settings via command line parameters.
while getopts r: option
do
case "${option}"
in
# The 'r' flag lets a user specify a custom command to run the container with.
r) RUN_COMMAND=${OPTARG};;
esac
done

# This logic exists to ensure that we can run Docker fine from not just standard
# Linux environments but also WSL and MSYS.
if [ -x "$(command -v docker.exe)" ] && [ -x "$(command -v wslpath)" ]; then
    # Presumably running from WSL with Docker Desktop for Windows.
    DOCKER_COMMAND=docker.exe
    HOST_SRC_DIR=$(wslpath -aw $HOST_SRC_DIR_SHELL)
    HOST_OUT_DIR=$(wslpath -aw $HOST_OUT_DIR_SHELL)
    DOCKERFILE_DIRECTORY=$(wslpath -aw $DOCKERFILE_DIRECTORY)
elif [ -x "$(command -v docker)" ]; then
    DOCKER_COMMAND=docker
    HOST_SRC_DIR=${HOST_SRC_DIR_SHELL}
    HOST_OUT_DIR=${HOST_OUT_DIR_SHELL}
    DOCKERFILE_DIRECTORY=${DOCKERFILE_DIRECTORY}
else
    echo "Cannot find docker in path."
    exit 1
fi

#If we're running on MSYS we need to prepend "winpty" to the docker command.
if [ "$(expr substr $(uname -s) 1 5)" == "MINGW" ]; then
    DOCKER_COMMAND="winpty ${DOCKER_COMMAND}"
fi

# Make sure the Docker container image containing the build environment is
# up to date and then run the actual build command inside the container.
${DOCKER_COMMAND} build -t reify-build-env ${DOCKERFILE_DIRECTORY} && \
${DOCKER_COMMAND} run \
    --rm \
    --name reify-build-env-instance \
    -it \
    --mount type=bind,source=${HOST_SRC_DIR},target=/src,readonly \
    --mount type=bind,source=${HOST_OUT_DIR},target=/out \
    --mount type=volume,source=nix,target=/nix \
    reify-build-env \
    ${RUN_COMMAND}
