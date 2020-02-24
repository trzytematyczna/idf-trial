library(plyr)
library (rjson)
library(data.table)
library(dplyr)

# data <- fromJSON (file = "./data/sample_guardian-article-wcomments.json")

smart_rbind <- function(df, row_values) {
    # Create prototype row (empty values, but names from the df)
    list_proto <- rep(list(NA), ncol(df))
    names(list_proto) <- colnames(df)
    # Create the comparison closure
    comparison_fun <- function(key, new_list) {
        function(key) {
            if (key %in% names(new_list) & length(new_list[[key]]) > 0){
                new_list[key]
            } else {
                list_proto[key]
            }
        }
    }
    # Update the incoming values
    updated_list <- lapply(names(list_proto), comparison_fun(key, row_values))
    rbind.data.frame(df, updated_list, stringsAsFactors=FALSE)
}


readGuardianJson<- function(data){
    # data <- fromJSON (fileName)
    articles <- data.frame(matrix(ncol = 15, nrow = 0), stringsAsFactors=FALSE)
    colnames(articles)<-c("id", "type", "url", "authors", "authors_nb", "section", "tags", "tags_nb", 
                          "date_published", "date_modified", "share_count", "comment_nb", "title", 
                          "description", "text")
    comments <- data.frame ()
    
    for (article in data) {
        df.selected_article<-data.frame()
        ## FORMAT FIELDS
        article$id <- article[["_id"]][[1]]
        print(article$id)        
        article$authors_nb <- length (article$authors)
        article$authors <- paste (article$authors, collapse = ", ")
        article$tags_nb <- length (article$article_tags)
        article$tags <- paste (article$article_tags, collapse = ", ")
        article$section <- article$article_section
        
        
        # article$date_published <- as.Date (as.POSIXct (article$date_published[[1]]/1000, origin = "1970-01-01"))
        # article$date_modified <- as.Date (as.POSIXct (article$date_modified[[1]]/1000, origin = "1970-01-01"))
        
        article$share_count <- as.numeric (article$share_count)
        article$comment_nb <- length (article$comments)
        article$title <- article$og_fields[['og:title']]
        article$type <- article$og_fields[['og:type']]
        
        ## SELECT FIELDS
        selected_article <- article[c("id", "type", "url", "authors", "authors_nb", "section", "tags", "tags_nb", 
                                      "date_published", "date_modified", "share_count", "comment_nb", "title", 
                                      "description", "text")] 
        #[c("id", "url", "authors", "topic_seed_url", "share_count","article_section", "article_tags", "date_published", "date_modified", "title", "keyword_urls","description", "text", "headline","discussion_section_open")]
        selected_article<- lapply(selected_article, function(x){
            if(is.vector(x)){paste(unlist(x),collapse=" ,")}
            else x
        })
        
        df.selected_article<-as.data.frame(selected_article[!is.na(names(selected_article))], stringsAsFactors=FALSE)
        
        for(key in c("id", "type", "url", "authors", "authors_nb", "section", "tags", "tags_nb", 
                     "date_published", "date_modified", "share_count", "comment_nb", "title", 
                     "description", "text")){
            if(!(key %in% colnames(df.selected_article))){
                df.selected_article <-df.selected_article %>% mutate(!!key := NA)
                # colnames(df.selected_article)[ncol(df.selected_article)] <- key
            }
        }
        
        df.selected_article%>% select(id,type,url,authors,authors_nb, section,tags, tags_nb, date_published, date_modified, share_count, 
                                      comment_nb, title, description, text)
        if(nrow(df.selected_article)==1){
            articles<-rbind.fill(articles, df.selected_article)
        }
        
        # articles <- smart_rbind (articles, as.data.frame(selected_article, stringsAsFactors=FALSE))
        
        ## LOOP THROUGH COMMENTS
        if (! is.null (article$comments)) {
            for (comment in article$comments) {
                
                ## FORMAT FIELS
                comment$article_id <- article$id
                comment$date <- as.Date (as.POSIXct (comment$time_stamp[[1]]/1000, origin = "1970-01-01"))
                comment$recommendation_count <- as.numeric (comment$recommendation_count)
                if (length (comment$recommendation_count) == 0) { comment$recommendation_count <- 0 }
                if (is.null (comment$in_reply_to)) { comment$in_reply_to <- "NULL" }
                
                ## SELECT FIELDS
                selected_comment <- comment [c("id", "article_id", "schema_org_type", "author", "author_id", "date", "recommendation_count", "in_reply_to", "text")]
                comments <- rbind (comments, as.data.frame (selected_comment))
            }
        }
    }
    result<-list(articles, comments)
    # write.csv (articles, "articles.csv", row.names = FALSE)
    # write.csv (comments, "comments.csv", row.names = FALSE)
}

files<-list.files("./data/guardian-data/")
    # files<-files[1:1]
res_articles<-data.frame()
res_comments<-data.frame()
for(gfile in files){
    print(gfile)
    data <- fromJSON (file = paste("./data/guardian-data/",gfile,sep=""))
    res<-readGuardianJson(data)
    res_articles<-rbind(res_articles,res[[1]])
    res_comments<-rbind(res_comments,res[[2]])
    write.csv2(res_articles,"./data/full_articles_guardian.csv", row.names = FALSE, append = TRUE)
    write.csv2(res_comments,"./data/full_comments_guardian.csv", row.names = FALSE, append = TRUE)
}
