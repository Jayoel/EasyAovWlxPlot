\name{MuiStat}
\alias{MuiStat}
\title{MuiStat}
\usage{
    MuiStat(data = data_wt,num = c(4,5,6),method_cv = "leveneTest",method_Mc = "Tukey",sig_show  = "abc",ncol = 2,plot = "box",plottype = "mui")
}
\arguments{
\item{data}{输入数据框，第一列为样本编号，第二列为分组，注意分组标签必须设定为group，第三列以后就是测定或者收集的指标了}

\item{num}{代表您想要进行统计的列,这里可以输入多个列，只需要指定列号即可：例如：num = c(4:6)}

\item{method_cv}{d代表选择方差齐性的方法，有两种可供选择：method_cv == "bartlett.test" ;method_cv == "leveneTest"}

\item{method_Mc}{选择需要使用的多重比较方法，这里又多种方法可供选择：method_Mc == "LSD";method_Mc == "SNK";method_Mc == "Duncan";method_Mc == "scheffe"}

\item{plot}{可以选择需要的出图类型，柱状图和箱线图}

\item{sig_show}{代表差异展示方式；sig_show ="abc"是使用字母表示;sig_show ="line"是使用连线和星号表示；如果是NA，那么就不显示显著性结果}

\item{ncol}{代表分面展示每一行放几张图}

\item{plottype}{输出图形是分面展示plottype =mui，还是单张展示:plottype == "single"}

}
\value{

}

\description{
实现了多个指标批量整体运行；这个函数可以将我们的目标列做正态检验和方差齐性，然后根据结果选择方差检验或者多重比较方法，最后选择自己需要的出图方式和显著性标记方式展示。
}
\examples{
#example1
#使用案例
result = MuiStat(data = data_wt,num = c(4,5,6),method_cv = "leveneTest",method_Mc = "Tukey",sig_show  = "abc",ncol = 2,plot = "box",plottype = "mui")
result[[1]]

}

