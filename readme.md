#0. raw data --> conversion  to ascii 

 iconv -c -f utf-8 -t ascii texts-preformat.csv -o texts-ascii.csv

1template-modelling.r -> LDA run
2template-topics.r -> cluster words, dendrograms
3template-plots.r -> avg probabilities, avg prob per topic per month -- for twitter only sample data 
# split data into batches of 2M cat texts-ascii.csv | parallel --header : --pipe -N2000000 'cat >file_{#}.csv'
4twitter-data-sampling.r -> sampling from data files split into 2M/file of data to train model (2M) for twitter --> it includes cleaning the sample from weather bots
5topic-proximity-probability.r -> plots showing topic similarity between models with different cluster number e.g. model with k=5 vs model with k=7 - how the five topics are included in the seven topics
# split data into batches of 500K -> cat texts-ascii.csv | parallel --header : --pipe -N500000 'cat >file_{#}.csv'
6twitter-clean-testdata.r -> cleans files (divided into 54 files x 500K liners) for prediction
7predict-topic-testdata.r -> data prediction of 54 files using trained model from 1template-modeling.r --> THE RESULTED 54 FILEs WITH PREDICTIONS DO NOT CONTAIN "ID" FOR THE FIRST COLUMN
8sum-assignments-plots.r -> uses trained data from 7predict (only id + topic probability value), counts topic probability means for all topics only for tweets trained for all files (500k) 
##add id to all 54 files
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
##
9assign-join -> join redictions with info tweets (add_ids-to-predict-results-54.sh), create 6 files with preditions + info for tweets
8.1-sum-assignments-plots-retweets.r -> uses joined data (predictions + info about tweets, devided into 6 files), counts topics probability means for all topics using tweets and RT info (!) 
10assigned-plots.r probability timeline plots divided by month/week using tweets only/retweets only/rt+tweets
10.1-assigned-days.r probability timeline plots divided by DAYS using tweets only/retweets only/rt+tweets
assign-days-guardian.r probability timeline plots divided by DAYS for guardian
12peakfuns.r -> taking tweets by max number of retweets etc.
