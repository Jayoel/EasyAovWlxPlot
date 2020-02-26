\name{NorNorCVTest}
\alias{NorNorCVTest}
\title{NorNorCVTest}
\usage{
    NorNorCVTest(data = data_wt, i= 4,method_cv = "leveneTest")
}
\arguments{
\item{data}{输入数据框，第一列为样本编号，第二列为分组，注意分组标签必须设定为group，第三列以后就是测定或者收集的指标了}

\item{i}{代表您想要进行统计的列，比如：第三列：i = 3}

\item{method_cv}{d代表选择方差齐性的方法，有两种可供选择：method_cv == "bartlett.test" ;method_cv == "leveneTest"}

}
\value{

}

\description{
快速正态检验和方差齐性分析
}
\examples{
#example1
##使用案例
NorCV = NorNorCVTest(data = data_wt, i= 4,method_cv = "leveneTest")
NorCV[[1]]

}

