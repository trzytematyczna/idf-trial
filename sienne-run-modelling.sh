#!/bin/bash

for i in {1..14}
do
    echo "Number: $i"
    Rscript --vanilla --verbose 1.1sienne-modelling.r $i
done
