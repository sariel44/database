#!/bin/sh

set -e

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
