#!/bin/bash
cd src/bank-application-frontend
yarn set version stable
yarn --version
set -e

yarn config set cacheFolder $(pwd)/.yarn-cache
yarn config set httpTimeout 600000

yarn install
yarn build package

./updateWebUI.sh

cd ../..
mvn clean package
