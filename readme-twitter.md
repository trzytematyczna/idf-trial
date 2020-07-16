TopicModeling
1. raw data --> conversion  to ascii iconv -c -f utf-8 -t ascii texts-preformat.csv -o texts-ascii.csv
2.  split data into batches of 2M cat texts-ascii.csv | parallel --header : --pipe -N2000000 'cat >file_{#}.csv'
3. twitter-sample-train-data.r -> sampling from data files split into 2M/file of data to train model (2M) for twitter --> it includes cleaning the sample from weather bots
4. topic-modeling.r -> LDA run /or sienne-topic-modeling and sienne-run-topic-modeling for using remote computer
5. topic-modeling-plots.r -> cluster words, dendrograms
6. k-selection-topics-proximity.r -> plots showing topic similarity between models with different cluster number e.g. model with k=5 vs model with k=7 - how the five topics are included in the seven topics

TopicPredicion
1. split data into batches of 500K -> cat texts-ascii.csv | parallel --header : --pipe -N500000 'cat >file_{#}.csv'
2. twitter-clean-test-data.r -> cleans files (divided into 54 files x 500K liners) for prediction
3. prediction-test-data.r -> data prediction of 54 files using trained model from 1template-modeling.r --> THE RESULTED 54 FILEs WITH PREDICTIONS DO NOT CONTAIN "ID" FOR THE FIRST COLUMN --> or khiva-prediction-test-data + khiva-run-prediction-test-data for remote
4. add-ids-to-predict-results-54.sh
5. twitter-join-predictions-with-tweetinfo.r -> join redictions with info tweets (after add_ids-to-predict-results-54.sh), create 6 files with preditions + info for tweets
  
AfterPrediction
1. (on raw predictions from TopicPrediction/prediction-test-data) twitter-all-data-tweets-topics-avgs.r -> uses trained data from 7predict (only id + topic probability value), counts topic probability means for all topics only for tweets trained for all files (500k) 
2. (on data after TopicPrediciton/twitter-join-predictions-with-tweetinfo) twitter-all-data-rt-topics-avgs.r -> uses joined data (predictions + info about tweets, devided into 6 files), counts topics probability means for all topics using tweets and RT info (!) 3. twitter-probability-timelines-aggregated.r probability timeline plots divided by month/week using tweets only/retweets only/rt+tweets
4. twitter-probability-timelines-day.r probability timeline plots divided by DAYS using tweets only/retweets only/rt+tweets

TrendsAnalysis
twitter-trends
twitter-trends-selected
twitter-trends-selected-rt
twitter-peaks-info.r -> taking tweets by max number of retweets etc.
