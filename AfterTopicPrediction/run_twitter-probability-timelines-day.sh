#!/bin/bash

for i in {1..6}
do
  echo "Number: $i"
  Rscript --vanilla --verbose twitter-probability-timelines-aggregated.R $i
done
