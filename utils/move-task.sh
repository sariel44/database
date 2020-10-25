#!/bin/sh

set -e

if [ ! -d utils ]; then 
  echo "Run this script as from the root of the project" 1>&2
  exit 1
fi 

. utils/lib.sh

database="tasks"
name="$1"
state="$2"

if [ "$database"x = "x" ]; then 
  echo "Need a database" 1>&2
fi 

if [ "$name"x = "x" ]; then
  echo "Need a name" 1>&2
  exit 1
fi 

if [ "$name"x = "x" ]; then
  echo "Need a state" 1>&2
  exit 1
fi 


case $state in
  open)
    database-exec task-open "$database" "$name"
    ;;
  busy)
    database-exec task-busy "$database" "$name"
    ;;
  "done")
    database-exec task-done "$database" "$name"
    ;;
  *)
    echo "Wrong task state $state" 1>&2
    exit 1
    ;;
esac
