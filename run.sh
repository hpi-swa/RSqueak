#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset

readonly BUNDLE_URL="https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/bundle/RSqueak.tar.gz"

# Download bundle
curl -f -s -L --retry 3 -o "rsqueak.tar.gz" "${BUNDLE_URL}"
tar xzf "rsqueak.tar.gz"

RSIMAGE="RSqueak.app/Contents/Resources/RSqueak.image"

case "$(uname -s)" in
  "Linux")
    RSVM="RSqueak.app/Contents/Linux/RSqueak"
    ;;
  "Darwin")
    RSVM="RSqueak.app/Contents/MacOS/RSqueak"
    ;;
  *)
    echo "OS not supported" && exit 1
    ;;
esac

# echo the selected categories for prosperity
cat "${SMALLTALK_CONFIG}" | tail -n +5 | head -n -3

status=0
"${SMALLTALK_CI_HOME}/run.sh" --debug --vm "${RSVM}" --image "${RSIMAGE}" "${SMALLTALK_CONFIG}" || status=$?

if ! [[ "${SMALLTALK_CI_BUILD}/build_status.txt" ]]; then
  echo "Test execution failed, maybe the VM crashed?"
  exit "${status}"
fi
