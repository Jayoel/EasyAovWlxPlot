\name{aovMcomper}
\alias{aovMcomper}
\title{aovMcomper}
\usage{
    aovMcomper (data = data_wt, i= 5,method_Mc = "Tukey")
}
\arguments{
\item{data}{输入数据框，第一列为样本编号，第二列为分组，注意分组标签必须设定为group，第三列以后就是测定或者收集的指标了}

\item{i}{代表您想要进行统计的列，比如：第三列：i = 3}

\item{method_Mc}{选择需要使用的多重比较方法，这里又多种方法可供选择：method_Mc == "LSD";method_Mc == "SNK";method_Mc == "Duncan";method_Mc == "scheffe"}

}
\value{

}

\description{
由于快速方差分析和剁成比较，输出结果简单容易都，并且配套全套后续分析流程，完美解决差异统计。
}
\examples{
#example1
result= aovMcomper (data = data_wt, i= 5,method_Mc = "Tukey")
# 提取多重比较结果
result[[1]]
#example2
result= aovMcomper (data = data_wt, i= 4,method_Mc = "LSD")
# 提取多重比较结果
result[[1]]

#example3
result= aovMcomper (data = data_wt, i= 3,method_Mc = "SNK")
# 提取多重比较结果
result[[1]]

}

