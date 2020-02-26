\name{KwWlx}
\alias{KwWlx}
\title{KwWlx}
\usage{
     KwWlx(data = data_wt, i= 4)
}
\arguments{
\item{data}{输入数据框，第一列为样本编号，第二列为分组，注意分组标签必须设定为group，第三列以后就是测定或者收集的指标了}

\item{i}{代表您想要进行统计的列，比如：第三列：i = 3}

}
\value{

}

\description{
调用wilcox.test进行非参数检验
}
\examples{
#example1
#使用案例
data(data_wt)
res = KwWlx(data = data_wt, i= 4)
res[[1]]


}

