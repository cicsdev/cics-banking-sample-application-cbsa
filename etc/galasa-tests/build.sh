#!/usr/bin/env bash
rm -rf ~/.m2
./gradlew --stop
./gradlew clean build publishToMavenLocal
