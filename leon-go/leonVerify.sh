#!/bin/sh
echo Executing Leon...

leon --debug=timers --timeout=10 \
     --solvers=smt-z3 \
     --functions="connectedComponent" \
     src/main/scala/go/collection/*.scala \
     src/main/scala/go/core/*.scala \
     src/main/scala/go/util/{conversions,Range,Logic}.scala
