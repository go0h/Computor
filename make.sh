#!/usr/bin/env bash

mvn clean package
mv ./target/ComputorV-2-jar-with-dependencies.jar ./ComputorV2.jar
chmod u+x ComputorV2.jar