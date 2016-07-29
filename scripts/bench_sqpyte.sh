#!/bin/bash

# Requires SQPyte package to be present in Squeak image (see /repository)

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
  "${RSQUEAK}" ${ARGS} -r "|b| DBObject Mode: 0. b := (EloBenchmark new) setUp: ${MATCHES}. ^ [b benchEloWithAllInstances] timeToRun." "${IMAGE}"
  echo "for ${MATCHES} matches without database and with Object>>allInstancesDo:"
  echo "======================================================================="
  "${RSQUEAK}" ${ARGS} -r "|b| DBObject Mode: 1. b := (EloBenchmark new) setUp: ${MATCHES}. ^ [b benchElo] timeToRun" "${IMAGE}"
  echo "for ${MATCHES} matches with DBObject+SQLite"
  echo "======================================================================="
  "${RSQUEAK}" ${ARGS} -r "|b| DBObject Mode: 1. b := (EloBenchmark new) setUp: ${MATCHES}. ^ [b benchEloWithAllInstances] timeToRun." "${IMAGE}"
  echo "for ${MATCHES} matches with RFFI+SQLite and DBObject>>allInstancesDo:"
  echo "======================================================================="
  "${RSQUEAK}" ${ARGS} -r "|b| DBObject Mode: 2. b := (EloBenchmark new) setUp: ${MATCHES}. ^ [b benchElo] timeToRun" "${IMAGE}"
  echo "for ${MATCHES} matches with DBObject+SQPyte"
  echo "======================================================================="
  "${RSQUEAK}" ${ARGS} -r "|b| DBObject Mode: 2. b := (EloBenchmark new) setUp: ${MATCHES}. ^ [b benchEloWithAllInstances] timeToRun." "${IMAGE}"
  echo "for ${MATCHES} matches with RFFI+SQPyte and DBObject>>allInstancesDo:"
  echo "======================================================================="

  MATCHES=$((${MATCHES}+${STEPS}))
done

echo ""

POINTS=1000
POINTS_STEP=1000
POINTS_MAX=5000
CLUSTERS=2
CLUSTERS_STEP=10
CLUSTERS_MAX=20

echo "#### Running KMeans Benchmarks..."
while [ ${CLUSTERS} -le ${CLUSTERS_MAX} ]; do
  while [ ${POINTS} -le ${POINTS_MAX} ]; do
    
    "${RSQUEAK}" ${ARGS} -r "|b| DBObject Mode: 0. b := KMeansBenchmark new. b setupNrPoints: ${POINTS} withSeed: 42. ^ [b runWithNrClusters: ${CLUSTERS}] timeToRun" "${IMAGE}"
    echo "KMeansBenchmark (${POINTS} Points, ${CLUSTERS} Clusters) without database"
    echo "======================================================================="
    "${RSQUEAK}" ${ARGS} -r "|b| DBObject Mode: 1. b := KMeansBenchmark new. b setupNrPoints: ${POINTS} withSeed: 42. ^ [b runWithNrClusters: ${CLUSTERS}] timeToRun" "${IMAGE}"
    echo "KMeansBenchmark (${POINTS} Points, ${CLUSTERS} Clusters) with DBObject+SQLite"
    echo "======================================================================="
    "${RSQUEAK}" ${ARGS} -r "|b| DBObject Mode: 2. b := KMeansBenchmark new. b setupNrPoints: ${POINTS} withSeed: 42. ^ [b runWithNrClusters: ${CLUSTERS}] timeToRun" "${IMAGE}"
    echo "KMeansBenchmark (${POINTS} Points, ${CLUSTERS} Clusters) with DBObject+SQPyte"
    echo "======================================================================="
    
    POINTS=$((${POINTS}+${POINTS_STEP}))
  done
  CLUSTERS=$((${CLUSTERS}+${CLUSTERS_STEPS}))
done
