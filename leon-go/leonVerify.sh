#!/bin/sh
echo Executing Leon...
leon src/main/scala/go/core/*.scala src/main/scala/go/collection/*.scala src/main/scala/go/util/{conversions,Range}.scala
