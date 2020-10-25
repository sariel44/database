# database

Simple cli-driven document database on top of git. You can fuzzy search through the files.  

It allows encryption through git-secret. I wanted something I could search through the standard core-utils and which naturally allows encryption and versioning. 

Git and git-secret[1] together with this simple tool matches perfectly.  

I also implemented a simple task system

# Installation 

    bash install.sh
    stack install 

# Usage 

First create a gpg key 

    gpg --generate-key

    git-secret tell <your email address>

    ./utils/create.sh database name tag1 tag2 tag3
    ./utils/edit.sh database name
    database-exec search <searchword>
    
    ./utils/create-task.sh name tag1 tag2 tag3

    database-exec task-open taskname tagfile 
    database-exec task-busy taskname 
    database-exec task-done taskname 
    database-exec task-search <searchword>


    database-exec search <keyword>

# Todo 

* Build javascript client lib for just reading records 
* Build edit script for task
* Build global index
* Allow cross references

# Refs

[1] https://git-secret.io

Interested in the data, don't hesitate to contact me. I am here to help, but only to the right people ^_^ It is easy to add another GPG key to give access to.
