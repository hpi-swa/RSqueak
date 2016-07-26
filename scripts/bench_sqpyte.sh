#!/bin/bash

# Required Squeak packages: Obsidian-Elo, ORM, SQPyte-Core

if [ "$#" -ne 1 ]; then
  echo "Please provide an image!"
  exit
fi

IMAGE=$1
MAX=10000
STEPS=2000
MATCHES=2000

while [ ${MATCHES} -le ${MAX} ]; do
  ./rsqueak --silent -r "SQLElo testPreparedElo: ${MATCHES} usingSQPyte: False" "${IMAGE}"
  echo "for ${MATCHES} matches with RFFI+SQLite"
  echo "======================================================================="
  ./rsqueak --silent -r "SQLElo testPreparedElo: ${MATCHES} usingSQPyte: True" "${IMAGE}"
  echo "for ${MATCHES} matches with RFFI+SQPyte"
  echo "======================================================================="
  ./rsqueak --silent -r "|obj| DBObject Mode: 0. obj := (EloBenchmark new) setUp: ${MATCHES}. ^ [obj benchElo] timeToRun" "${IMAGE}"
  echo "for ${MATCHES} matches without database"
  echo "======================================================================="
  ./rsqueak --silent -r "|obj| DBObject Mode: 1. obj := (EloBenchmark new) setUp: ${MATCHES}. ^ [obj benchElo] timeToRun" "${IMAGE}"
  echo "for ${MATCHES} matches with DBObject+SQLite"
  echo "======================================================================="
  ./rsqueak --silent -r "|obj| DBObject Mode: 2. obj := (EloBenchmark new) setUp: ${MATCHES}. ^ [obj benchElo] timeToRun" "${IMAGE}"
  echo "for ${MATCHES} matches with DBObject+SQPyte"
  echo "======================================================================="
  MATCHES=$((${MATCHES}+${STEPS}))
done
