#!/bin/sh

database="$1"
name="$2"

if [ "$name"x = "x" ]; then
  echo "Need a name" 1>&2
  exit 1
fi 

if [ "$database"x = "x" ]; then 
  echo "Need a database" 1>&2
fi 

echo "<Add your data here>" > "$name" 

nvim "$name" < `tty` > `tty`

echo '["<exampletag"]' > "${name}.tags"

nvim "$name.tags" < `tty` > `tty`

database-exe save "$database" "$name" "${name}.tags" 

echo "Hiding change"
git-secret add "$database/$name"
git-secret hide
echo "Staging change"
git add "$database/${name}.secret"






