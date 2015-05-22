#!/bin/sh
echo Executing Leon...

# conversions
# leon src/main/scala/go/util/conversions.scala src/main/scala/go/core/{Point,PlacedCell,Cell,PlayerType}.scala

# Collections
# leon --debug=timers --timeout=3 src/main/scala/go/collection/*.scala src/main/scala/go/core/{Point,PlacedCell,Cell}.scala

# Point
# leon src/main/scala/go/core/{Point,PlacedCell,Cell}.scala

# Board
leon --debug=timers --timeout=3 \
                                --solvers=smt-z3 \
                                --functions="connectedComponent" \
                                src/main/scala/go/collection/*.scala \
                                src/main/scala/go/core/*.scala \
                                src/main/scala/go/util/{conversions,Range,Logic}.scala
