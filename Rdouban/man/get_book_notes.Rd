\name{get_book_notes}
\alias{get_book_notes}

\title{
��ȡ����ͼ��Ķ���ʼ�.
}
\description{
��ȡ����ͼ��Ķ���ʼ�.
}
\usage{
get_book_notes(bookid,n=50, ...)
}
\arguments{
  \item{bookid}{�����鼮��id��,һ������,��20326626}
  \item{n}{��ȡ����ʼǵ�ƪ����Ĭ��n=50}
}

\value{
  һ���б�(list), ����:
  \item{book_title}{�鼮����} 
  \item{notes_amount}{�ʼ���������}
  \item{notes_info}{һ��data.frame,����8��:
    \code{titles}������������,    
    \code{notes}����ʼ�����,
    \code{time}����ʼǷ���ʱ��,
    \code{authors}�����ǳ�,     
    \code{rating}��������,     
    \code{author_url}������ҳ, 
    \code{notes_url}�ñʼǵ���ҳ��
    \code{notes_comment_url}�Ըñʼǵ�������ҳ}
}
 
\author{
  <\email{qxde01@@gmail.com}>
}


\seealso{
\code{\link{get_book_comments}}
}
\examples{

\dontrun{get_book_notes(bookid='2120492',n=50)}
}

\keyword{ book}
\keyword{ douban }
\keyword{ note}