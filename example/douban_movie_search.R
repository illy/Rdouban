###电影搜索，只能获取前20项

douban_movie_search<-function(keyword,...){
  strurl<-paste0('http://www.douban.com/search?cat=1002&q=',keyword)
  pagetree<-htmlParse(getURL(strurl))
  
  urlsnode<-getNodeSet(pagetree, '//div[@class="result"]//a')
  urls<-unique(sapply(urlsnode,function(x) xmlGetAttr(x, "href")))
  urls<-urls[grep('movie',urls)]
  urlid<-gsub('[^0-9]','',urls)
  n=length(urls)
  df<-c()
  for(i in 1:n){
    cat('getting ',urls[i],'...\n')
    info<-get_movie_info(movieid=urlid[i])
    title<-info$movie_title
    base_info=info$movie_base_info
    base_info<-gsub('导演|编剧|主演|类型|制片国家/地区|语言|首播日期| ','',base_info)
    base_info<-unlist(strsplit(base_info,':'))
    base_info<-base_info[nchar(base_info)>0]
    director<-base_info[1]
    writer<-base_info[2]
    actor<-base_info[3]
    type<-base_info[4]
    location<-base_info[5]
    lang<-base_info[6]
    #score=info[[2]][1]
    #vote=info[[2]][2]
    movie_intro=info$movie_intro
    
    df0<-c(title=title,
      director=director,
      writer=writer,
      actor=actor,
      type=type,
      location=location,
      lang=lang,
      #score=score,
      #vote=vote,
      intro=movie_intro)
    df<-rbind(df,df0)
  }
  row.names(df)<-NULL
  df 
}

df<-douban_movie_search(keyword='成龙')
## 格式化演员数据
as.graph<-function(x){
  gg<-edge<-c()
  for(i in 1:20){
    title<-df[i,1]
    edge0<-data.frame(name=title,type='title',stringsAsFactors=F)
    director<-unlist(strsplit(df[i,2],'/'))
    n1=length(director)
    g1<-data.frame(left=rep(title,n1),right=director)
    edge1<-data.frame(name=director,type=rep('director',n1))
   
    writer<-unlist(strsplit(df[i,3],'/'))
    n2=length(writer)
    g2<-data.frame(left=rep(title,n2),right=writer,stringsAsFactors=F)
    edge2<-data.frame(name=writer,type=rep('writer',n2))
    
    actor<-unlist(strsplit(df[i,'actor'],'/'))
    n3=length(actor)
    g3=data.frame(left=rep(title,n3),right=actor)
    edge3=data.frame(name=actor,type=rep('actor',n3),stringsAsFactors=F)
    
    gg0<-rbind(g1,g2,g3)
    edge00<-rbind(edge0,edge1,edge2,edge3)
    gg<-rbind(gg,gg0)
    edge<-rbind(edge,edge00)  
  }
  row.names(gg)<-NULL
  row.names(edge)<-NULL
  list(gg=unique(gg),
       edge=unique(edge))
}

###网络关系图
g<-as.graph(df)
gg<-g$gg
edge<-g$edge
library(igraph)

gg=graph.data.frame(gg)
label=V(gg)$name

vs=rep(4,227)
t1=edge[edge[,2]=='title',]
vs[label %in% t1[,1]]=6

png('cheng.png',width=800,height=800)
par(bg='black',mar=rep(0,4))
plot(gg,layout=layout.fruchterman.reingold,
     vertex.label=V(gg)$name,vertex.size=vs,
     vertex.color=vs,vertex.label.color=vs-1,
     vertex.label.cex=1.3)
dev.off()
wc=walktrap.community(gg)

png('wc.png',width=900,height=900)
par(bg='gray10',mar=rep(0,4))
plot(wc,gg,vertex.label=V(gg)$name,vertex.size=vs,
     vertex.label.color=vs+6,vertex.label.cex=1.2)
dev.off()