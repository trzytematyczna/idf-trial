#!/bin/bash

for i in {5..54}
do
    echo "Number: $i"
    Rscript --vanilla --verbose 7predict-topic-testdata.R $i
done
