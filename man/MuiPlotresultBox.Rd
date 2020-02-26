\name{MuiPlotresultBox}
\alias{MuiPlotresultBox}
\title{MuiPlotresultBox}
\usage{
   MuiPlotresultBox(data = data_wt,num = c(4:6),result = result,sig_show ="abc")
}
\arguments{
\item{data}{输入数据框，第一列为样本编号，第二列为分组，注意分组标签必须设定为group，第三列以后就是测定或者收集的指标了}

\item{num}{代表您想要进行统计的列,这里可以输入多个列，只需要指定列号即可：例如：num = c(4:6)}

\item{sig_show}{代表差异展示方式；sig_show ="abc"是使用字母表示;sig_show ="line"是使用连线和星号表示；如果是NA，那么就不显示显著性结果}

\item{result}{代表显著性差异分析结果，是一个数据框，每一列是显著性标记字母,MuiKwWlx}

}
\value{

}

\description{
批量出图，自动保存。将方差分析或者非参数检验结果进行可视化箱线图展示，使用ggplot出图，同时标注显著性，有两种标记方式供大家选择
}
\examples{
#example1
#使用案例
result = MuiKwWlx(data = data_wt,num = c(4:6))
result
# #直接出图到文件夹中
MuiPlotresultBox(data = data_wt,num = c(4:6),result = result,sig_show ="abc")

}

