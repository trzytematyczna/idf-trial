#!/bin/bash

for i in {1..6}
do
echo "Number: $i"
Rscript --vanilla --verbose gaps.R $i
done
