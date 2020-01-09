#!/bin/sh

mkdir -p ./build-docs

cp $PATH_APIB ./build-docs/api.apib

cd ./build-docs

mkdir -p $FOLDER_TO_GH_PAGES

docker run --volume $(pwd):/temp:Z dojot/aglio -i /temp/api.apib -o - > ./$FOLDER_TO_GH_PAGES/apiary_${VERSION_NAME}.html

rm -rf api.apib

git add $FOLDER_TO_GH_PAGES/apiary_${VERSION_NAME}.html
