library(topicmodels)
library(ggplot2)
library(textmineR)
library(reshape2)
library(dplyr)
library(tidyr)
library(data.table)





getDataByOrganization <- function(organization,data){
  organization<-tolower(organization)
  things<-data[data$text %like% organization, ]
  things
}

getCommentsBelow <- function(organization, adata, cdata){
  organization<-tolower(organization)
  arts<-getDataByOrganization(organization, adata)
  commentsBelowOrgArticles <- cdata[cdata$article_id %in% arts$id,]
  commentsBelowOrgArticles
}
getArticlesCommented <- function(organization, adata, cdata){
  organization<-tolower(organization)
  coms<-getDataByOrganization(organization, cdata)
  articlesCommentedOrgComments <- adata[adata$id %in% coms$article_id,]
  articlesCommentedOrgComments
}

countCommentsBelow <- function(organization, adata, cdata){
  organization<-tolower(organization)
  arts<-getDataByOrganization(organization, adata)
  count_commentsBelowOrgArticles <- nrow(cdata[cdata$article_id %in% arts$id,]) 
  count_commentsBelowOrgArticles
}
countArticlesCommented <- function(organization, adata, cdata){
  organization<-tolower(organization)
  coms<-getDataByOrganization(organization, cdata)
  count_articlesCommentedOrgComments <- nrow(adata[adata$id %in% coms$article_id,]) 
  count_articlesCommentedOrgComments
}

statsOrganizationsList<-function(organizations, adata,cdata){
  organizations<-tolower(organizations)
  df<-data.frame()
  for (i in 1:length(organizations)){
    ndf<-data.frame(organization_name=organizations[i],
                    art_nb=nrow(adata[adata$text %like% organizations[i], ]),
                    comm_nb =nrow(cdata[cdata$text %like% organizations[i], ]),
                    comm_below_art = countCommentsBelow(organizations[i],adata,cdata),
                    art_commented = countArticlesCommented(organizations[i],adata,cdata)
    )
    df<-rbind(df,ndf)
  }
  df<-df%>%arrange(desc(art_nb))
  df
}

statsOrganizations<-function(organizations, adata,cdata, which.names=0){
  
  organizations$Organization_name<-tolower(organizations$Organization_name)
  df<-data.frame()
  
  if(which.names==1){ #long only
    
  } else if(which.names==2){ #short only
    
  } else{ #both
     
    
    for(short in unique(organizations$Name)){

      ta<-data.frame()
      tc<-data.frame()
      # comm_below_art <- 0 
      # art_commented <- 0
      
      orgs <- organizations[organizations$Name == short,]$Organization_name
      if(nrow(orgs)>1){
        
      }
      for(i in organizations[organizations$Name == short,]$Organization_name){
        ta<-rbind(ta,select(adata[adata$text %like% paste0(" ",i, " "), ], id))
        tc<-rbind(tc,select(cdata[cdata$text %like% paste0(" ",i, " "), ],id))
        # comm_below_art <- comm_below_art + countCommentsBelow(i,adata,cdata)
        # art_commented <- art_commented + countArticlesCommented(i,adata,cdata)
      }
      
      ndf<-data.frame(organization_name=short,
                      art_nb=nrow(unique(ta)),
                      comm_nb =nrow(unique(tc))
                      # comm_below_art = comm_below_art,
                      # art_commented = art_commented
      )
      df<-rbind(df,ndf)
    }
  }
  
  df<-df%>%arrange(desc(art_nb))
  df
}



plotTimeline<-function(data, dates_name, title, filename, isWeekly = FALSE){
  
  if(isWeekly == TRUE){
    grouped <- data %>% 
      count(format(!!sym(dates_name),'%y-%V'))  #date_published = articles, date = comments
    colnames(grouped)<-c("date","nb")
    xlab_name <-  "Year and week"
    xbreaks <- grouped$date[seq(from = 1, to = nrow(grouped), by = 4)]
  }else{
    grouped <- data %>% 
      count(format(!!sym(dates_name),'%y-%m'))  #date_published = articles, date = comments
    colnames(grouped)<-c("date","nb")
    xlab_name <- "Year and month"
    xbreaks <-grouped$date
  }
  
  p <- ggplot(grouped, aes(x = date, y = nb)) + 
    geom_col() +
    theme(axis.text.x = element_text(angle = 90)) + 
    xlab(xlab_name) +
    ylab("#") +
    scale_x_discrete(breaks=xbreaks)+
    ggtitle(title)
  ggsave(filename)
  
}




checkOrganization<-function(organizations, data){
  organizations<-tolower(organizations)
  df<-data.frame(organization_name=character(0), art_nb=numeric(0), stringsAsFactors=FALSE)
  for (i in 1:length(organizations)){
    ndf<-data.frame(organization_name=organizations[i],
                    art_nb=nrow(data[data$text %like% organizations[i], ]))
    df<-rbind(df,ndf)
  }
  colnames(df)<-c("organization_name","art_nb")
  df
}


