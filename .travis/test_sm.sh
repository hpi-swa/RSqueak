#!/bin/bash

set -e

IMAGE=${IMAGE:-}
IMAGE_URL=${IMAGE_URL:-"https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/bundle/RSqueak.tar.gz"}
IMAGE_DIR=${IMAGE_DIR:-}
IMAGE_PATH=${IMAGE_PATH:-"./RSqueak.app/Contents/Resources/RSqueak.image"}
IMAGE_EXTRACT=${IMAGE_EXTRACT:-"./RSqueak.app/Contents/Resources/"}
RESULT_CMD=${RESULT_CMD:-}
RESULT_CMD_URL=${RESULT_CMD_URL:-"https://raw.githubusercontent.com/hpi-swa/smalltalkCI/master/lib/junit_xml_prettfier.py"}

realpath() {
    [[ $1 = /* ]] && echo "$1" || echo "$PWD/${1#./}"
}

FILETREE_PATH=$(realpath repository)

read -d '' CODE <<EOF || true
    FileStream startUp: true.

    Gofer new
        repository: 'filetree://${FILETREE_PATH}';
        package: 'SQPyte-Core';
        package: 'SQPyte-Tests';
        load.

    SCISqueakTestReport runClasses: {SQLiteTests. SQPyteTests} named: 'test'.
EOF

if [ -z "${IMAGE}" ]; then
    IMAGE_DIR=${IMAGE_DIR:-$(mktemp -d --suffix -sqpyte-test)}
    TAR="${IMAGE_DIR}/rsqueak.tar.gz"

    mkdir -p "${IMAGE_DIR}"
    pushd "${IMAGE_DIR}"

    if [ ! -f "${TAR}" ]; then
        echo "==== Download image archive..."
        wget --quiet "${IMAGE_URL}" -O "${TAR}"
    fi

    echo "==== Extract archive..."
    tar -zxf "${TAR}" "${IMAGE_EXTRACT}"
    IMAGE=$(realpath "${IMAGE_PATH}")

    popd
else
    IMAGE_DIR=$(dirname "${IMAGE}")
fi

if [ ! -f "${IMAGE}" ]; then
    echo "Image file not found. That's bad."
    exit 1
fi

echo "==== Run tests..."
./rsqueak -r "${CODE}" "${IMAGE}"

if [ -z "${RESULT_CMD}" ]; then
    RESULT_CMD="/tmp/print_test_results.py"

    if [ ! -f "${RESULT_CMD}" ]; then
        wget --quiet "${RESULT_CMD_URL}" -O "${RESULT_CMD}"
        chmod +x "${RESULT_CMD}"
    fi
fi

if [ ! -z "${RESULT_CMD}" ]; then
    "${RESULT_CMD}" "${IMAGE_DIR}"
fi
