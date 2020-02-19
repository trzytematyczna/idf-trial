library (rjson)

# data <- fromJSON (file = "./data/sample_guardian-article-wcomments.json")

readGuardianJson<- function(data){
    # data <- fromJSON (fileName)
    articles <- data.frame ()
    comments <- data.frame ()
    
    for (article in data) {
        article <- data[[2]]
        ## FORMAT FIELDS
        article$id <- article[["_id"]][[1]]
        
        article$authors_nb <- length (article$authors)
        article$authors <- paste (article$authors, collapse = ", ")
        article$tags_nb <- length (article$article_tags)
        article$tags <- paste (article$article_tags, collapse = ", ")
        article$section <- article$article_section
        
        article$date_published <- as.Date (as.POSIXct (article$date_published[[1]]/1000, origin = "1970-01-01"))
        article$date_modified <- as.Date (as.POSIXct (article$date_modified[[1]]/1000, origin = "1970-01-01"))
        ##article$date_comments_parsed <- as.Date (as.POSIXct (article$date_comments_parsed[[1]]/1000, origin = "1970-01-01"))
    
        article$share_count <- as.numeric (article$share_count)
        article$comment_nb <- length (article$comments)
        article$title <- article$og_fields[['og:title']]
        article$type <- article$og_fields[['og:type']]
    
        ## SELECT FIELDS
        selected_article <- article [c("id", "type", "url", "authors", "authors_nb", "section", "tags", "tags_nb", "date_published", "date_modified", "share_count", "comment_nb", "title", "description", "text")]
        articles <- rbind (articles, as.data.frame (selected_article))
    
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
files<-files[1:2]
res_articles<-data.frame()
res_comments<-data.frame()
for(gfile in files){
    print(gfile)
    data <- fromJSON (file = paste("./data/guardian-data/",gfile,sep=""))
    res<-readGuardianJson(data)
    res_articles<-rbind(res_articles,res[[1]])
    res_comments<-rbind(res_comments,res[[2]])
}
write.csv2(res_articles,"articles_.csv", row.names = FALSE)
write.csv2(res_comments,"comments_.csv", row.names = FALSE)
