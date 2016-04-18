#!/usr/bin/env bash

set -o errexit

readonly TEMPLATE_DIR="${TRAVIS_BUILD_DIR}/template"
readonly RESOURCES_DIR="${TEMPLATE_DIR}/RSqueak.app/Contents/Resources"
readonly IMAGE_TARGET="${RESOURCES_DIR}/RSqueak.image"
readonly CHANGES_TARGET="${RESOURCES_DIR}/RSqueak.image"
readonly TARGET_FILE="${TRAVIS_BUILD_DIR}/RSqueak.tar.gz"
readonly TARGET_URL="https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/\
rsqueak/bundle/"

cp "${SMALLTALK_CI_IMAGE}" "${IMAGE_TARGET}"
cp "${SMALLTALK_CI_CHANGES}" "${CHANGES_TARGET}"
cp "${SMALLTALK_CI_BUILD}/"*.sources "${RESOURCES_DIR}/"

pushd "${TEMPLATE_DIR}" > /dev/null
tar czvf "${TARGET_FILE}" "./RSqueak.app"
popd > /dev/null

curl -T "${TARGET_FILE}" -u "${DEPLOY_CREDENTIALS}" "${TARGET_URL}"