\encoding{UTF-8}
\name{get_movie_discussions}
\alias{get_movie_discussions}
\title{��ȡ�������ĳ����Ӱ���ۻ���
}
\description{��ȡ�������ĳ����Ӱ���ۻ���.
}
\usage{
get_movie_discussions(movieid,n=100,verbose=TRUE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{movieid}{�����Ӱ����Ӿ��ID,��������ɵ��ַ���,��ͨ��ÿ����Ӱ����Ӿ�Ķ�����ҳ��ѯ.}
  \item{n}{��ȡ���������,Ĭ��n=100.}
  \item{verbose}{��\code{verbose=True}ʱ,��ʾ������ִ�й�����Ϣ,\code{verbose=False}ʱ����ʾ,Ĭ��Ϊ\code{True}.}
}
}

\value{һ��data.frame,��7��:
  \item{title }{�������}
  \item{dicussion }{��������}
  \item{nickname }{��������������ǳ�}
  \item{time }{���ⷢ����ʱ��}
  \item{useful }{����Ϊ���õĴ���}
  \item{unuseful}{����Ϊ���õĴ���}
  \item{discussion_url }{������ַ}
  
}

\note{��ʵ�ʻ�����С��n,���ȡ����ʵ����,��ʵ��������n,���ȡ����������\code{ceiling(n/20)*20}.}

\author{
qxde01<\email{qxde01@@gmail.com}>
}


\seealso{
\code{\link{get_movie_comments,get_movie_reviews}}
}
\examples{
## http://movie.douban.com/subject/5308265/discussion/
\dontrun{get_movie_disussions(movieid=5308265,n=100)}
}

\keyword{disussion}