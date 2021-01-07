#!/usr/bin/env bash

mvn clean package
mv ./target/ComputorV-1-jar-with-dependencies.jar ./ComputorV1.jar
chmod u+x ComputorV1.jar