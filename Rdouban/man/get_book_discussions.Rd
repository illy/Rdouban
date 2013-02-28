\encoding{UTF-8}

\name{get_book_discussions}
\alias{get_book_discussions}
\title{��ȡ����ͼ��Ļ�����������
}
\description{��ȡ����ͼ�黰����������.
}
\usage{
get_book_discussions(bookid,n=100,verbose=TRUE,...)
}

\arguments{
  \item{bookid}{����ͼ���ID,��������ɵ��ַ���,��ͨ������ר���Ķ�����ҳ��ѯ.}
  \item{n}{��ȡ���������,Ĭ��n=100.}
  \item{verbose}{��\code{verbose=TRUE}ʱ,��ʾ������ִ�й�����Ϣ,\code{verbose=FALSE}ʱ����ʾ,Ĭ��Ϊ\code{TRUE}.}
}

\value{һ��data.frame,��7��:
  \item{title }{�������}
  \item{dicussion }{��������}
  \item{nickname }{��������������ǳ�}
  \item{time }{���ⷢ����ʱ��}
  \item{useful }{����Ϊ���õĴ���}
  \item{unuseful}{������Ϊ���õĴ���}
  \item{discussion_url }{������ַ}
  
}

\note{��ʵ�ʻ�����С��n,���ȡ����ʵ����,��ʵ��������n,���ȡ����������\code{ceiling(n/20)*20}.}

\author{
qxde01<\email{qxde01@@gmail.com}>
}


\seealso{
\code{\link{get_book_comments,get_book_reviews}}
}
\examples{
## http://book.douban.com/subject/1291204/discussion/
\dontrun{get_book_discussions(bookid=1291204,n=100)}
}

\keyword{discussion}