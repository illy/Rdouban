user_book_labelcloud<-function(x,width=700,height=800,file=NULL){
  library(wordcloud)
  num<-x$book
  do<-x$book_do_labels
  wish<-x$book_wish_labels
  collect<-x$book_collect_labels
  
  all<-rbind(do,wish,collect)  
  all<-tapply(all[,2],all[,1],sum)
  all<-data.frame(label_name=names(all),label_freq=all[])
  all<-all[order(all[,2],decreasing=T),]
  
  color1<-length(unique(do$label_freq))
  color2<-length(unique(wish$label_freq))
  color3<-length(unique(collect$label_freq))
  color4<-length(unique(all$label_freq))
  
  filename=paste0(file,'labels.png')
  png(filename,width=width,height=height)  
  op<-par(mfrow=c(2,2),pty='s')
  wordcloud(words=do$label_name,freq=do$label_freq,
            min.freq=1,colors=rainbow(color1))
  title(main=paste0('reading book: ',num['do']))
  wordcloud(words=wish$label_name,freq=wish$label_freq,
            min.freq=1,colors=rainbow(color2))
  title(main=paste0('wish read book: ',num['wish']))
  
  wordcloud(words=collect$label_name,freq=collect$label_freq,
            min.freq=1,colors=rainbow(color3))
  title(main=paste0('had read book: ',num['collect']))
  wordcloud(words=all$label_name,freq=all$label_freq,
            min.freq=1,colors=rainbow(color4))
  title(main=paste0('All book labels : ',nrow(all)))
  par(par)
  dev.off()
  
  cat("picture's path and name are :",paste0(getwd(),'/',filename))
}