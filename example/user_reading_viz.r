qxde<-douban_user_statuses(nickname='qxde01',getNote=FALSE,getReview=FALSE,
                           getList=TRUE,verbose=TRUE)
library(wordcloud)
library(ggplot2)


collect<-qxde$book_collect_df
month<-substr(collect$read_date,1,7)
amount<-table(month)
time_df<-data.frame(month=names(amount),amount=amount[],year=substr(names(amount),1,4))

##阅读书籍数量树状图 
png('bar.png',width=900,height=600)
ggplot(time_df, aes(x = month, fill=year,colour=amount)) +
  geom_bar(stat="identity", ymin=0, aes(y=amount, ymax=amount),position="dodge") +
  geom_text(aes(x=month, y=amount, ymax=amount,
                label=amount,hjust=ifelse(sign(amount)>0, 1, 0)),
            vjust=-0.5,size=6,colour=amount,
            position = position_dodge(width=1))+
 labs(x='年-月份', y='读书数量(本)',title =paste0('总共阅读 ',sum(amount),' 本书'))
dev.off()

#######
book_info<-collect$book_info
price<-lapply(as.list(book_info),function(x){y<-unlist(strsplit(x,'/'));y[length(y)] })
price<-as.numeric(gsub('元','',price))
price[price>1500]<-NA
p_df<-data.frame(price=price,year=substr(collect$read_date,1,4))
py<-tapply(p_df$price,p_df$year,sum,na.rm = T)
price_df<-data.frame(money=py,year=factor(names(py)))
price_df<-price_df[price_df$money>0,]
##所读书籍定价饼图
png('pie.png',width=600,height=600)
pie <- ggplot(price_df, aes(x = "",y=money, fill = year)) + 
  geom_bar(stat="identity",width = 1) + 
  geom_text(aes(y = money/2 + c(0, cumsum(money)[-length(money)]),
                label =paste0(money,'￥')),size=10)
pie + coord_polar(theta = "y")+
  labs(x='',y='',title = paste0('总价值(>=)',sum(price,na.rm=T),'元'))
dev.off()
###所读书籍的标签
library(wordcloud)
collect_labels<-qxde$book_collect_labels
png('cloud.png',width=600,height=600)
par(bg='gray90')
wordcloud(words=collect_labels$label_name,
          freq=collect_labels$label_freq,
          min.freq=1,scale=c(5,1.4),
          random.order=F,
          ordered.colors=F,
          colors=rainbow(length(table(collect_labels$label_freq))))
title(main=paste0('所读书籍的标签'))
dev.off()
######
###缩略图拼接
####################################################
get_image<-function(urls,w=100,h=120){
  ##批量读取图片
  library(EBImage)
  n=length(urls)
  images<-vector('list')
  for(i in 1:n){
    tmp<-readImage(urls[i])
    images[[i]]<-resize(tmp,w=w,h=h) 
  }
  images
}
img_url<-collect$img_url
images<-get_image(img_url,w=100,h=120)
##图像合并
img<-combine(images)

png('im.png',width=600,height=840)
display(img,method="raster",all=T)
dev.off()
#save(images,file='images.rda')

###############
###########################
## 书籍之间的关系图

graph.df<-function(x){
  #### 将数据转化为关系形式
  as.df<-function(x){
    x<-c(as.character(x[1]),as.character(x[2]))
    x[1]<-gsub(' ','',x[1])
    left=x[1]
    right<-unlist(strsplit(x[2],' '))
    right<-right[nchar(right)>0]
    right<-right[right!=left]
    left<-rep(left,length(right))
    cbind(left,right)
  }
  tag_list<-apply(x,1,as.df)
  n=length(tag_list)
  tag<-c()
  for(i in 1:n){
    tag<-rbind(tag,tag_list[[i]])
  }
  tag
}
book_tag<-data.frame(tilte=collect$book_tilte,
                     tag=collect$book_tag,stringsAsFactors=F)
book_tag$tilte<-unlist(lapply(book_tag$tilte,function(x) 
  unlist(strsplit(x,':'))[1]))
book_tag$tag<-gsub('标签:','',book_tag$tag)
g0<-graph.df(book_tag)

library("igraph")

g<-graph.data.frame(g0,directed=F)
name=V(g)$name
title<-gsub(' ','',unique(book_tag$tilte))
col=rep(2,length(name))
col[match(name,title)]<-4

wc=walktrap.community(g)
#fc=fastgreedy.community(g)
#sc=spinglass.community(g)
##随机漫步社区发现
png('wc.png',width=800,height=800)
par(mar=rep(2,4))
plot(wc,g,vertex.label=name,vertex.size=col,edge.arrow.size=0.5)
dev.off()
### 力学导向的fruchterman.reingold关系图
png('fr.png',width=800,height=800)
plot(g, layout=layout.fruchterman.reingold, vertex.size=col,
     vertex.label.dist=0, vertex.color=col+1, 
     edge.arrow.size=0.5,vertex.label=name,
     vertex.label.color=col+2)
dev.off()
