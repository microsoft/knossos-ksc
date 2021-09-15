#!/bin/bash
cmd=$1
tmpfile=$2

usage() {
  echo "usage: find-new-files [mark|diff] TMPFILE" 
  exit 1
}

test $# = 2 || test "$cmd" = test || usage

list () {
  find . -type f
}
if test $cmd = mark 
then
  echo "find-new-files: saving list to $tmpfile"
  list > $tmpfile
elif test $cmd = diff
then
  echo "find-new-files: begin diff from $tmpfile"
  list | diff -U 0 $tmpfile - | grep -v ^@@ | sed -e '1,2d' -e 's/^/find-new-files:  /'
  echo "find-new-files: end diff from $tmpfile"
elif test $cmd = test
then
  me=`readlink -f $0`
  echo "Testing: $me"
  tmpdir=`mktemp -d`
  tmpfile=`mktemp`
  cd $tmpdir
  
  touch a b c d e f g

  sh $me mark $tmpfile 

  rm a c
  touch d1 e1

  sh $me diff $tmpfile

  rm ? ??
  cd ..
  rm $tmpfile
  rmdir $tmpdir
else
  usage
fi
