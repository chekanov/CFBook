#!/bin/bash
 
MYDIR=`pwd`
FILE=example
g++ -o  $FILE'.exe'   $FILE'.cpp'  -L../../lib -lcbook -I../../inc
