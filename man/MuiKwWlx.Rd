\name{MuiKwWlx}
\alias{MuiKwWlx}
\title{MuiKwWlx}
\usage{
    MuiKwWlx(data = data_wt,num = c(4:6))
}
\arguments{
\item{data}{输入数据框，第一列为样本编号，第二列为分组，注意分组标签必须设定为group，第三列以后就是测定或者收集的指标了}

\item{num}{代表您想要进行统计的列,这里可以输入多个列，只需要指定列号即可：例如：num = c(4:6)}


}
\value{

}

\description{
批量多重比较，输出结果简单容易，并且配套全套后续分析流程，完美解决差异统计。
}
\examples{
#example1
# #使用案例
result = MuiKwWlx(data = data_wt,num = c(4:6))
result


}

