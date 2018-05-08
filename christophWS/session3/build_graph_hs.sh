#!/bin/bash

echo "Compiling HS ..."
ghc graph.hs

if [ $? -ne 0 ]; then
  exit 1
fi

echo "Running ..."
./graph
