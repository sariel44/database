git-secret hide
find . -iname "*.secret" | while read -r secret; do 
  git add $secret
done
