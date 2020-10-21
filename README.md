# database

Simple cli-driven document database on top of git. You can fuzzy search through the files.  

It allows encryption through git-secret. I wanted something I could search through the standard core-utils and which naturally allows encryption and versioning. 

Git and git-secret together with this simple tool matches perfectly. 

# Installation 

    bash install.sh
    stack install 

# Usage 

First create a gpg key 

    gpg --generate-key

    git-secret tell <your email address>

    database-exec get database <recordname>
    
    database-exec save database <recordname> <tagfile>
    git-secret add database/recordname
    git-secret hide 


    database-exec search <keyword>

# Todo 

Add random keyvalue pairs as metadata
Implement foreign keys 
