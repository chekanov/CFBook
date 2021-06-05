#!/bin/bash
 
MYDIR=`pwd`
FILE=example
gfortran  -o  $FILE'.exe'   $FILE'.f' stat.f  -L../../lib -lfbook -I../../inc
