#!/bin/sh

set -e

if [ ! -d utils ]; then 
  echo "Run this script as from the root of the project" 1>&2
  exit 1
fi

message=$1

if [ "$message"x = "x" ]; then 
  echo "Message must be given" 1>&2
  exit 1
fi  

git-secret hide
find . -iname "*.secret" | while read -r secret; do 
  echo "Adding $secret to list" 
  git add "$secret"
done

git commit -m "$1" 
