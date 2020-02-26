\name{SingleStat}
\alias{SingleStat}
\title{SingleStat}
\usage{
    SingleStat(data = data_wt,plot = "bar",method_Mc = "Tukey",i= 4,sig_show ="abc")
}
\arguments{
\item{data}{输入数据框，第一列为样本编号，第二列为分组，注意分组标签必须设定为group，第三列以后就是测定或者收集的指标了}

\item{i}{代表您想要进行统计的列，比如：第三列：i = 3}

\item{method_Mc}{选择需要使用的多重比较方法，这里又多种方法可供选择：method_Mc == "LSD";method_Mc == "SNK";method_Mc == "Duncan";method_Mc == "scheffe"}
\item{plot}{可以选择需要的出图类型，柱状图和箱线图}


}
\value{

}

\description{
这个函数可以将我们的目标列做正态检验和方差齐性，然后根据结果选择方差检验或者多重比较方法，最后选择自己需要的出图方式和显著性标记方式展示。
}
\examples{
#example1
# #输出结果第一个为图片，第二个是统计结果，第三个是统计方法
result = SingleStat(data = data_wt,plot = "bar",method_Mc = "Tukey",i= 4,sig_show ="abc")
# #导出图片
p = result[[1]]
p

}

