#!/bin/bash

for i in {1..54}
do
    echo "Number: $i"
    Rscript --vanilla --verbose khiva-predict-topic.R $i
done
