\name{MuiNorCV}
\alias{MuiNorCV}
\title{MuiNorCV}
\usage{
    MuiNorCV(data = data_wt,num = c(4,5,6),method_cv = "leveneTest")
}
\arguments{
\item{data}{输入数据框，第一列为样本编号，第二列为分组，注意分组标签必须设定为group，第三列以后就是妮妮测定或者收集的指标了}

\item{num}{代表您想要进行统计的列,这里可以输入多个列，只需要指定列号即可：例如：num = c(4:6)}

\item{method_cv}{d代表选择方差齐性的方法，有两种可供选择：method_cv == "bartlett.test" ;method_cv == "leveneTest"}


}
\value{

}

\description{
批量快速正态检验和方差齐性分析
}
\examples{
#example1
# 使用案例
norCv = MuiNorCV(data = data_wt,num = c(4,5,6),method_cv = "leveneTest")
norCv

}

