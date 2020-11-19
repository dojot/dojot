#!/bin/sh

mkdir -p ./swagger-docs

cp $PATH_SWAGGER_YML ./swagger-docs/swagger_${VERSION_NAME}.yml

cd ./swagger-docs

git add swagger_${VERSION_NAME}.yml

if [ -f "swagger_${VERSION_NAME}.yml" ]; then
	echo "Doc API create "
else
	echo "Error, couldn't create file for doc"
fi
