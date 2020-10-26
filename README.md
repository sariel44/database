# database

Simple cli-driven document database on top of git. You can fuzzy search through the files.  

It allows encryption through git-secret. I wanted something I could search through the standard core-utils and which naturally allows encryption and versioning. 

Git and git-secret[1] together with this simple tool matches perfectly.  

I also implemented a simple task system

# Installation 

   make install 

# Usage 

First create a gpg key 

    gpg --generate-key

    git-secret tell <your email address>

    ./utils/create.sh database name tag1 tag2 tag3
    ./utils/edit.sh database name

    database-exec search database <keyword>
    database-exec show database name

    
    ./utils/create-task.sh name tag1 tag2 tag3
    ./utils/edit-task.sh name 
    ./utils/move-task name (open|busy|done)

    database-exec search tasks <keyword>

    ./utils/reveal.sh
    ... do something with the data ...
    ./utils/cleanup.sh

# Todo 

* Extract metadata from record (Format ^key: value$)
* Make script to load everything automatically in elastic search
* Build javascript client lib for just reading records 
* Build global index
* Allow cross references (with checks!)
* Add a slow chat widget (perhaps through ncurses)

# Refs

[1] https://git-secret.io

# What more
Interested in the data, don't hesitate to contact me. I am here to help, but only to the right people ^_^ It is easy to add another GPG key to give access to.
