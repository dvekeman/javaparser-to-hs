#!/bin/bash

rm lib/javaparser-to-hs.jar 2>/dev/null
rm lib/checkparses.jar 2>/dev/null

javac -sourcepath java/src -cp lib/slf4j-simple-1.7.21.jar:lib/slf4j-api-1.7.21.jar:lib/javaparser-1.0.8.jar:lib/commons-lang3-3.1.jar -d java/classes java/src/*.java

jar cmf java/META-INF/javaparsertohs/MANIFEST.MF lib/javaparser-to-hs.jar -C java/classes . -C java/resources/ .
jar cmf java/META-INF/checkparses/MANIFEST.MF lib/checkparses.jar -C java/classes . -C java/resources .
