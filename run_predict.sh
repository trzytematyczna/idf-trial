#!/bin/bash

for i in {1..1}
do
    echo "Number: $i"
    Rscript --vanilla --verbose 7predict-topic-testdata.R $i
done
