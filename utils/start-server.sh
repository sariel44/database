#!/bin/sh

set -e

if [ ! -d utils ]; then 
  echo "Run this script as from the root of the project" 1>&2
  exit 1
fi 

. utils/lib.sh

web-exe


