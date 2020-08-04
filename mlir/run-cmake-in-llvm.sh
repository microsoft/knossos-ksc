# Translate from ps1 script so there's only one place to update
PS=$(echo $0 | sed s/sh$/ps1/)
tr '`' '\\' < $PS 
