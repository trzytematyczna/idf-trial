#!/bin/bash

for i in {1..14}
do
echo "Number: $i"
Rscript --vanilla --verbose timeline-topics.R $i
done
