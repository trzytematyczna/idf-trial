#!/bin/bash

for i in {1..1}
do
  echo "Number: $i"
  Rscript --vanilla --verbose 10.1-assigned-days.R $i
done