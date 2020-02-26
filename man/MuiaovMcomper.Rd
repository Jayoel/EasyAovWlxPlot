\name{MuiaovMcomper}
\alias{MuiaovMcomper}
\title{MuiaovMcomper}
\usage{
    MuiaovMcomper(data = data_wt,num = c(4:6),method_Mc = "Tukey")
}
\arguments{
\item{data}{输入数据框，第一列为样本编号，第二列为分组，注意分组标签必须设定为group，第三列以后就是测定或者收集的指标了}

\item{num}{代表您想要进行统计的列,这里可以输入多个列，只需要指定列号即可：例如：num = c(4:6)}

\item{method_Mc}{选择需要使用的多重比较方法，这里又多种方法可供选择：method_Mc == "LSD";method_Mc == "SNK";method_Mc == "Duncan";method_Mc == "scheffe"}

}
\value{

}

\description{
快速批量方差分析和多重比较，输出结果简单容易，并且配套全套后续分析流程，完美解决差异统计。
}
\examples{
#example1
result = MuiaovMcomper(data = data_wt,num = c(4:6),method_Mc = "Tukey")
result
}

