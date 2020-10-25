#!/bin/sh

# Make sure we never overwrite data accidentally
cat_template(){
  cat "templates/$1" >> "$2"
}

edit(){
  $EDITOR "$1" <  `tty` > `tty`
}

write_tags(){
  name=$1
  shift
  printf "[" >> "${name}.tags"
  printf '"%s",' $@ | sed -E 's/,$//' >> "${name}.tags" 
  printf "]" >> "${name}.tags"
}

cleanup(){
  name=$1 
  rm -v $name
  rm -v "${name}."*
}

hide_and_add(){
  database=$1
  name=$2
  echo "Hiding change in $database/$name" 1>&2
  git-secret add "$database/$name"
  git-secret hide
  echo "Staging change"
  git add "$database/${name}.secret" 1>&2
}
