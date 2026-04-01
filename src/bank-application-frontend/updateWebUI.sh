#!/bin/bash
rm -rf ../webui/WebContent/static

mkdir -p ../webui/WebContent/static
mkdir -p ../webui/WebContent/static/css
mkdir -p ../webui/WebContent/static/js
mkdir -p ../webui/WebContent/static/media

cp -r build/* ../webui/WebContent/
cp -r build/static/css/* ../webui/WebContent/static/css/
cp -r build/static/js/* ../webui/WebContent/static/js/
cp -r build/static/media/* ../webui/WebContent/static/media/
