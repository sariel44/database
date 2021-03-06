#!/bin/sh

set -e

if [ ! -d utils ]; then 
  echo "Run this script as from the root of the project" 1>&2
  exit 1
fi 

. utils/lib.sh
database="tasks"
name="$1"
shift

if [ "$name"x = "x" ]; then
  echo "Need a name" 1>&2
  exit 1
fi 

if [ "$database"x = "x" ]; then 
  echo "Need a database" 1>&2
fi 

cat_template "$database" "$name"

edit "$name"

write_tags "$name" $@

edit "$name.tags" 

database-exe task-save "$database" "$name" "${name}.tags" 

hide_and_add "$database" "$name"

cleanup "$name"


