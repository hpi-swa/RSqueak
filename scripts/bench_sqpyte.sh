#!/bin/bash

# Required Squeak packages: KMeans, Obsidian-Elo, ORM, SQPyte-Core

if [ "$#" -ne 2 ]; then
  echo "Please provide a RSqueak binary and an image!"
  exit
fi

RSQUEAK=$1
IMAGE=$2
ARGS="--silent"

MAX=10000
STEPS=2000
MATCHES=2000

echo "#### Running Elo Benchmarks..."
while [ ${MATCHES} -le ${MAX} ]; do
  "${RSQUEAK}" ${ARGS} -r "SQLElo testPreparedElo: ${MATCHES} usingSQPyte: False" "${IMAGE}"
  echo "for ${MATCHES} matches with RFFI+SQLite"
  echo "======================================================================="
  "${RSQUEAK}" ${ARGS} -r "SQLElo testPreparedElo: ${MATCHES} usingSQPyte: True" "${IMAGE}"
  echo "for ${MATCHES} matches with RFFI+SQPyte"
  echo "======================================================================="
  "${RSQUEAK}" ${ARGS} -r "|b| DBObject Mode: 0. b := (EloBenchmark new) setUp: ${MATCHES}. ^ [b benchElo] timeToRun" "${IMAGE}"
  echo "for ${MATCHES} matches without database"
  echo "======================================================================="
  "${RSQUEAK}" ${ARGS} -r "|b| DBObject Mode: 1. b := (EloBenchmark new) setUp: ${MATCHES}. ^ [b benchElo] timeToRun" "${IMAGE}"
  echo "for ${MATCHES} matches with DBObject+SQLite"
  echo "======================================================================="
  "${RSQUEAK}" ${ARGS} -r "|b| DBObject Mode: 2. b := (EloBenchmark new) setUp: ${MATCHES}. ^ [b benchElo] timeToRun" "${IMAGE}"
  echo "for ${MATCHES} matches with DBObject+SQPyte"
  echo "======================================================================="

  "${RSQUEAK}" ${ARGS} -r "|b| DBObject Mode: 0. b := (EloBenchmark new) setUp: ${MATCHES}. ^ [b benchEloWithAllInstances] timeToRun." "${IMAGE}"
  echo "for ${MATCHES} matches without database and with Object>>allInstancesDo:"
  echo "======================================================================="
  "${RSQUEAK}" ${ARGS} -r "|b| DBObject Mode: 1. b := (EloBenchmark new) setUp: ${MATCHES}. ^ [b benchEloWithAllInstances] timeToRun." "${IMAGE}"
  echo "for ${MATCHES} matches with RFFI+SQLite and DBObject>>allInstancesDo:"
  echo "======================================================================="
  "${RSQUEAK}" ${ARGS} -r "|b| DBObject Mode: 2. b := (EloBenchmark new) setUp: ${MATCHES}. ^ [b benchEloWithAllInstances] timeToRun." "${IMAGE}"
  echo "for ${MATCHES} matches with RFFI+SQPyte and DBObject>>allInstancesDo:"
  echo "======================================================================="

  MATCHES=$((${MATCHES}+${STEPS}))
done

echo ""

echo "#### Running KMeans Benchmarks..."
"${RSQUEAK}" ${ARGS} -r "|b| DBObject Mode: 0. b := KMeansBenchmark new. b setup. ^ [b run] timeToRun" "${IMAGE}"
echo "KMeansBenchmark without database"
echo "======================================================================="
"${RSQUEAK}" ${ARGS} -r "|b| DBObject Mode: 1. b := KMeansBenchmark new. b setup. ^ [b run] timeToRun" "${IMAGE}"
echo "KMeansBenchmark with DBObject+SQLite"
echo "======================================================================="
"${RSQUEAK}" ${ARGS} -r "|b| DBObject Mode: 2. b := KMeansBenchmark new. b setup. ^ [b run] timeToRun" "${IMAGE}"
echo "KMeansBenchmark with DBObject+SQPyte"
echo "======================================================================="
