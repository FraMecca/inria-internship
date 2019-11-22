#!/bin/bash -e
for f in $(ls *.ml); do 
    ocamlc -drawlambda $f 2>&1| tee $(basename $f .ml).lambda
done
rm *.cm*
rm a.out
