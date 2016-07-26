#!/bin/bash

# Required Squeak packages: KMeans, Obsidian-Elo, ORM, SQPyte-Core

if [ "$#" -ne 1 ]; then
  echo "Please provide an image!"
  exit
fi

IMAGE=$1
MAX=10000
STEPS=2000
MATCHES=2000

echo "#### Running Elo Benchmarks..."
while [ ${MATCHES} -le ${MAX} ]; do
  ./rsqueak --silent -r "SQLElo testPreparedElo: ${MATCHES} usingSQPyte: False" "${IMAGE}"
  echo "for ${MATCHES} matches with RFFI+SQLite"
  echo "======================================================================="
  ./rsqueak --silent -r "SQLElo testPreparedElo: ${MATCHES} usingSQPyte: True" "${IMAGE}"
  echo "for ${MATCHES} matches with RFFI+SQPyte"
  echo "======================================================================="
  ./rsqueak --silent -r "|b| DBObject Mode: 0. b := (EloBenchmark new) setUp: ${MATCHES}. ^ [b benchElo] timeToRun" "${IMAGE}"
  echo "for ${MATCHES} matches without database"
  echo "======================================================================="
  ./rsqueak --silent -r "|b| DBObject Mode: 1. b := (EloBenchmark new) setUp: ${MATCHES}. ^ [b benchElo] timeToRun" "${IMAGE}"
  echo "for ${MATCHES} matches with DBObject+SQLite"
  echo "======================================================================="
  ./rsqueak --silent -r "|b| DBObject Mode: 2. b := (EloBenchmark new) setUp: ${MATCHES}. ^ [b benchElo] timeToRun" "${IMAGE}"
  echo "for ${MATCHES} matches with DBObject+SQPyte"
  echo "======================================================================="

  ./rsqueak -r "|b| DBObject Mode: 0. b := (EloBenchmark new) setUp: ${MATCHES}. ^ [b benchEloWithAllInstances] timeToRun." "${IMAGE}"
  echo "for ${MATCHES} matches without database and with Object>>allInstancesDo:"
  echo "======================================================================="
  ./rsqueak -r "|b| DBObject Mode: 1. b := (EloBenchmark new) setUp: ${MATCHES}. ^ [b benchEloWithAllInstances] timeToRun." "${IMAGE}"
  echo "for ${MATCHES} matches with RFFI+SQLite and DBObject>>allInstancesDo:"
  echo "======================================================================="
  ./rsqueak -r "|b| DBObject Mode: 2. b := (EloBenchmark new) setUp: ${MATCHES}. ^ [b benchEloWithAllInstances] timeToRun." "${IMAGE}"
  echo "for ${MATCHES} matches with RFFI+SQPyte and DBObject>>allInstancesDo:"
  echo "======================================================================="

  MATCHES=$((${MATCHES}+${STEPS}))
done

echo ""

echo "#### Running KMeans Benchmarks..."
./rsqueak --silent -r "|b| DBObject Mode: 0. b := KMeansBenchmark new. b setup. ^ [b run] timeToRun" "${IMAGE}"
echo "KMeansBenchmark without database"
echo "======================================================================="
./rsqueak --silent -r "|b| DBObject Mode: 1. b := KMeansBenchmark new. b setup. ^ [b run] timeToRun" "${IMAGE}"
echo "KMeansBenchmark with DBObject+SQLite"
echo "======================================================================="
./rsqueak --silent -r "|b| DBObject Mode: 2. b := KMeansBenchmark new. b setup. ^ [b run] timeToRun" "${IMAGE}"
echo "KMeansBenchmark with DBObject+SQPyte"
echo "======================================================================="
