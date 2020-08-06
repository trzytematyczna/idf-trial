# script name:
# timeline.R

#* @apiTitle Timeline plots
#* @apiDescription This API is able to provide plots showing the distribution of probabilities of topics from topic modeling component in time (using aggregation parameter)

library(dplyr)
library(ggplot2)
library(stringi)

load("./results/guardian-articles/guardian-prediction-data.RData")
  
#* Return names of topics from static file  
#* @param labfilename Path to file
#* @get /topics.labs.fun 
topics.labs.fun<-function(labfilename){
  topic.labels<-read_csv(labfilename)
  topic.labels<-topic.labels%>% 
    group_by(topic) %>%
    summarise(s=paste(word, collapse = ", "))
  
  topic.labels<- topic.labels%>%
    group_by(topic) %>%
    summarize(label=paste(s,topic, collapse = ","))
}
  
#* Return plot  
####* @param data  Data containing only id, topics, probability of topics, and date
#* @param granularity Granularity of aggregation for plot {day,week,month}
#* @get /plot
#* @png
plot<-function(granularity){
  topic.labels<-topics.labs.fun("./results/guardian-articles/k-10-topic-words.csv")

  global.means <- data %>% 
    group_by(topic) %>%
    summarize(gtp=mean(probability))
  global.means <- global.means %>% mutate(topic=factor(topic,levels = 1:10,labels=topic.labels$label))
  
  data.arranged<-data%>%arrange(date,topic)
  
  if(stri_cmp_eq(tolower(granularity),"day")){
    xlabel<-'Day'
  
    grouped.data <- data.arranged%>%
    group_by(date,topic) %>%
    summarise(sum_probability=mean(probability))
  
    grouped.data <- grouped.data %>%  
      mutate(topic=factor(topic,levels = 1:10,  labels=topic.labels$label))
    
    g<-ggplot(grouped.data, aes(x=date,y=sum_probability))+
      geom_bar(stat="identity",position="stack")+
      ylim(0.0, (max(grouped.data$sum_probability)))+
      geom_hline(data=global.means, aes(yintercept = gtp), lty="dashed",color="#CC6666")+
      # geom_hline(data=global.means.rt, aes(yintercept = gtp), lty="dashed",color=col[3])+
      theme(axis.text.x = element_text(angle = 90))+
      ggtitle("Probability of Guardian articles")+
      xlab(xlabel)+
      ylab("Probability")+
      facet_wrap(.~topic, ncol=2)
    print(g)
    # ggsave(paste0(res_dir,"guardian-probs-daily-all-years.pdf"), device = "pdf", limitsize = FALSE, plot=last_plot(), scale = 2)
  }
  else if(stri_cmp_eq(tolower(granularity),"week") | stri_cmp_eq(tolower(granularity),"month")){
    if(stri_cmp_eq(tolower(granularity),"week")){
      divide.by <- '%y-%V'
      xlabel<-'Week'
    } else{
      divide.by <- '%y-%m'
      xlabel<-'Month'
    }
    divided.arranged<-data.arranged%>%
      mutate(div=format(date, divide.by))
    
    grouped.data <- divided.arranged%>%
      group_by(div,topic) %>%
      summarise(sum_probability=mean(probability))
    
    grouped.data <- grouped.data %>%  
      mutate(topic=factor(topic,levels = 1:10,  labels=topic.labels$label))
    
    g<-ggplot(grouped.data, aes(x=date,y=sum_probability))+
      geom_bar(stat="identity",position="stack")+
      ylim(0.0, (max(grouped.data$sum_probability)))+
      geom_hline(data=global.means, aes(yintercept = gtp), lty="dashed",color="#CC6666")+
      # geom_hline(data=global.means.rt, aes(yintercept = gtp), lty="dashed",color=col[3])+
      theme(axis.text.x = element_text(angle = 90))+
      ggtitle("Probability of Guardian articles")+
      xlab(xlabel)+
      ylab("Probability")+
      facet_wrap(.~topic, ncol=2)
    print(g)
    
  }
  else{
    print("wrong granularity")
  }
  
}
