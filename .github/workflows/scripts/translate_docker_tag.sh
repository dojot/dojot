#!/bin/sh
set -e

BRANCH=$(echo ${GITHUB_REF} | sed -e "s/refs\/heads\///g" | sed -e "s/\//-/g")

if [ "${BRANCH}" = "master" ]; then
  TAG="latest"
elif [ $(echo "${GITHUB_REF}" | sed -e "s/refs\/tags\///g") != "${GITHUB_REF}" ]; then
  #is Git Tag
  TAG=$(echo ${GITHUB_REF} | sed -e "s/refs\/tags\///g")
elif [ $(echo "${GITHUB_REF}" | sed -e "s/refs\/pull\///g") != "${GITHUB_REF}" ]; then
  #isPullRequest
  TAG="${GITHUB_SHA}"
else
  TAG="${BRANCH}"
fi;

#tag for version
echo $TAG