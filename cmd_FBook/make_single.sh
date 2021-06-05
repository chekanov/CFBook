#!/bin/sh
#
MYDIR=`pwd`
rm -f ./fbook.f
cat ../src/FBook/*.f  > ./main.tmp
cat ../src/FBook/README.txt ./main.tmp > ./fbook.f
rm -f ./main.tmp
echo "fbook.f was created"
 
