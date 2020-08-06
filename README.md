# idf-trial

0. raw data --> conversion  to ascii iconv -c -f utf-8 -t ascii texts-preformat.csv -o texts-ascii.csv

TopicModeling
  1. split data into batches of 2M cat texts-ascii.csv | parallel --header : --pipe -N2000000 'cat >file_{#}.csv'
  1.1. twitter-sample-train-data (oldname: 4twitter-data-sampling.r) -> sampling from data files split into 2M/file of data to train model (2M) for twitter - it includes cleaning the sample from weather bots
  2. topic-modeling (oldname: 1template-modelling.r) -> LDA run 
  --> sienne-topic-modeling + sienne-run-topic-modeling.sh
  3. topic-modeling-plots (oldname: 2template-topics.r) -> cluster words, dendrograms
  3.1 k-selection-topics-proximity (oldname: 5topic-proximity-probability.r) -> plots showing topic similarity between models with different cluster number e.g. model with k=5 vs model with k=7 - how the five topics are included in the seven topics
  
TopicPrediction

  1. # split data into batches of 500K -> cat texts-ascii.csv | parallel --header : --pipe -N500000 'cat >file_{#}.csv'
  2. twitter-clean-test-data (oldname: 6twitter-clean-testdata.r) -> cleans files (divided into 54 files x 500K liners) for prediction
  3. prediction-test-data (oldname: 7predict-topic-testdata.r) -> data prediction of 54 files using trained model from 1template-modeling.r --> THE RESULTED 54 FILEs WITH PREDICTIONS DO NOT CONTAIN "ID" FOR THE FIRST COLUMN
      -->khiva-prediction-test-data + khiva-run-prediction-test-data
  4. add id to all 54 files:  add-ids-to-predict-results-x54-before-twitter-join-predictions.sh
  5. twitter-join-predictions-with-tweetinfo (oldname: 9assign-join) -> join redictions with info tweets (add_ids-to-predict-results-54.sh), create 6 files with preditions + info for tweets
6. sort the 6 files:
      - join 6 files to one awk '(NR == 1) || (FNR > 1)' assign-*.csv > assign-all.csv
      - sort the joined file by date { head -n1 assign-all.csv; for f in assign-all.csv; do sort -t, -k 2 assign-all.csv; done; }  > sorted-assign-all.csv
      - get rid of end header (why is it here ?) head -n -1 sorted-assign-all.csv > temp.txt ; mv temp.txt sorted-assign-all.csv
      - divide the sorted joined file into 6 ones cat sorted-assign-all.csv | parallel --header : --pipe -N4106762 'cat >sorted-assign-{#}.csv'

Gaps:
1. run gaps run-gaps.sh using gaps.r on SORTED 6 files (topicpredicton pt 6)
2. join 6 files to one awk '(NR == 1) || (FNR > 1)' gaps-*.csv > gaps-all.txt

AfterPrediction:

  1. (on raw predictions from TopicPrediction/prediction-test-data) twitter-all-data-tweets-topics-avgs (oldname: 8sum-assignments-plots.r) -> uses trained data from 7predict (only id + topic probability value), counts topic probability means for all topics only for tweets trained for all files (500k) 
  2. (on data after TopicPrediciton/twitter-join-predictions-with-tweetinfo) twitter-all-data-rt-topics-avgs (oldname: 8.1-sum-assignments-plots-retweets.r) -> uses joined data (predictions + info about tweets, devided into 6 files), counts topics probability means for all topics using tweets and RT info (!)     
  3. twitter-probability-timelines-aggregated (oldname: 10assigned-plots.r) probability timeline plots divided by month/week using tweets only/retweets only/rt+tweets
  4. twitter-probability-timelines-day (oldname: 10.1-assigned-days.r) probability timeline plots divided by DAYS using tweets only/retweets only/rt+tweets
  5. guardian-probability-timelines (oldname: assign-days-guardian.r) probability timeline plots divided by DAYS for guardian

TrendsAnalysis
    - twitter-peaks-info (oldname: 12peakfuns.r) -> taking tweets by max number of retweets etc.


moved to scripts:
  3template-plots.r -> avg probabilities, avg prob per topic per month -- for twitter only sample data 
 11sentiment.r