#!/bin/sh
echo Executing Leon...

# conversions
# leon src/main/scala/go/util/conversions.scala src/main/scala/go/core/{Point,PlacedCell,Cell,PlayerType}.scala

# Collections
leon src/main/scala/go/collection/GoMap.scala src/main/scala/go/core/{Point,PlacedCell,Cell}.scala

# Point
# leon src/main/scala/go/core/{Point,PlacedCell,Cell}.scala
