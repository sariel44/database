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

    database-exec get database <recordname>

    database-exec show database <recordname>

    database-exec search <searchword>
    
    database-exec save database <recordname> <tagfile>
    git-secret add database/recordname
    git-secret hide 
  
    database-exec task-create taskname tagfile 
    database-exec task-open taskname tagfile 
    database-exec task-busy taskname 
    database-exec task-done taskname 
    database-exec task-search <searchword>


    database-exec search <keyword>

# Todo 

* Build global index
* Allow cross references

# Refs

[1] https://git-secret.io

Interested in the data, don't hesitate to contact me. I am here to help, but only to the right people ^_^
