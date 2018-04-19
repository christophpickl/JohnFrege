#!/usr/bin/env bash

PROJECT_DIR=/Users/cpickl/Workspace/_private/JohnFrege

echo "Compiling Frege code ..."

java -Xss1m \
    -classpath ${PROJECT_DIR}/frege3.24.405.jar \
    frege.compiler.Main -inline -make \
    -d ${PROJECT_DIR}/HelloFrege/target/frege \
    -sp ${PROJECT_DIR}/HelloFrege/src/main/frege \
    ${PROJECT_DIR}/HelloFrege/

echo "Done."
