#!/bin/sh
echo Executing Leon...

     # --functions="connectedComponent" \
leon --debug=timers --timeout=3 \
     --solvers=smt-z3 \
     src/main/scala/go/collection/*.scala \
     src/main/scala/go/core/*.scala \
     src/main/scala/go/util/{conversions,Range,Logic}.scala
