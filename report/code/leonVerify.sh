#!/bin/sh
echo Executing Leon...
leon --timeout=3 --solvers=smt-z3 \
     --functions="connectedComponent" \
     src/main/scala/go/*.scala

