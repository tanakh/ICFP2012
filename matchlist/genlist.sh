#!/bin/sh
dir=$1
if [ x$dir == 'x' ]; then
  dir='.'
fi

find $dir -name '*.map' -type f > maps
find $dir -name 'll-ai*' -type f -perm +111 > ailist
