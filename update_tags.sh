#!/bin/bash

# this script requires Universal Ctags to be installed

TMPFILE_SCALA="$(mktemp)"
find app  -type f -name \*.scala > $TMPFILE_SCALA
find test -type f -name \*.scala >> $TMPFILE_SCALA

echo "TMPFILE_SCALA is: $TMPFILE_SCALA"

exctags --output-format=etags -eL $TMPFILE_SCALA -f TAGS

# - having problems setting up gnu global to use universal ctags
# - commenting out for now
#gtags -v --gtagslabel new-ctags -f $TMPFILE
