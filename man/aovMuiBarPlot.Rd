\name{aovMuiBarPlot}
\alias{aovMuiBarPlot}
\title{aovMuiBarPlot}
\usage{
    aovMuiBarPlot(data = data_wt, i= 3,sig_show ="abc",result = result[[1]])
}
\arguments{
\item{data}{输入数据框，第一列为样本编号，第二列为分组，注意分组标签必须设定为group，第三列以后就是测定或者收集的指标了}

\item{i}{代表您想要进行统计的列，比如：第三列：i = 3}

\item{sig_show}{代表差异展示方式；sig_show ="abc"是使用字母表示;sig_show ="line"是使用连线和星号表示；如果是NA，那么就不显示显著性结果}
\item{result}{代表显著性差异分析结果，是一个数据框，第一列是显著性差异字母，第二列是分组group}



}
\value{

}

\description{
将方差分析或者非参数检验结果进行可视化柱状图展示，使用ggplot出图，同时标注显著性，有两种标记方式供大家选择
}
\examples{
#example1
data(data_wt)
###----使用方差检验结果和多重比较结果做展示：  柱状图展示
PlotresultBar = aovMuiBarPlot(data = data_wt, i= 3,sig_show ="abc",result = result[[1]])
PlotresultBar[[1]]
# 提取多重比较结果
result[[1]]
#example2
data(data_wt)
###----使用方差检验结果和多重比较结果做展示：  柱状图展示
PlotresultBar = aovMuiBarPlot(data = data_wt, i= 4,sig_show ="line",result = result[[1]])
PlotresultBar[[1]]


#example3
data(data_wt)
###----使用方差检验结果和多重比较结果做展示：  柱状图展示
PlotresultBar = aovMuiBarPlot(data = data_wt, i= 5,sig_show =NA,result = result[[1]])
PlotresultBar[[1]]

}

