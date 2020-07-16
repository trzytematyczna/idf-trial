#!/bin/sh

for file in /home/mra/Documents/UMPC/idf-trial/results/twitter-trained/predict-results/*
do
  if [ -f "$file" ]
  then
    
   printf $file 
    sed '1 s/.*/id,t_1,t_2,t_3,t_4,t_5,t_6,t_7,t_8,t_9/' $file > temp
    cp temp $file
    rm temp 
  fi
done
