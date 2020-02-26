# \item{data}{输入数据框，第一列为样本编号，第二列为分组，注意分组标签必须设定为group，第三列以后就是测定或者收集的指标了}
#
# \item{num}{代表您想要进行统计的列,这里可以输入多个列，只需要指定列号即可：例如：num = c(4:6)}
#
# \item{method_cv}{d代表选择方差齐性的方法，有两种可供选择：method_cv == "bartlett.test" ;method_cv == "leveneTest"}
#
# \item{method_Mc}{选择需要使用的多重比较方法，这里又多种方法可供选择：method_Mc == "LSD";method_Mc == "SNK";method_Mc == "Duncan";method_Mc == "scheffe"}
#
# \item{plot}{可以选择需要的出图类型，柱状图和箱线图}
#
# \item{sig_show}{代表差异展示方式；sig_show ="abc"是使用字母表示;sig_show ="line"是使用连线和星号表示；如果是NA，那么就不显示显著性结果}
#
# \item{ncol}{代表分面展示每一行放几张图}
#
# \item{plottype}{输出图形是分面展示plottype =mui，还是单张展示:plottype == "single"}



# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#使用案例
# result = MuiStat(data = data_wt,num = c(4,5,6),method_cv = "leveneTest",method_Mc = "Tukey",sig_show  = "abc",ncol = 2,plot = "boxbar",plottype = "mui")
# result[[1]]


# ###-----------------------------------走一个流程--正态检验---方差齐性---是否方差检验如果多个变量实现这个自动化过程，应该怎么办？
#
# num = c(4,5,6)
# data = data_wt
# method_cv = "leveneTest"
# # i = 1
# sig_show  = "abc"
# ncol = 2
# plot = "bar"
# plottype = "mui"


MuiStat = function(data = data_wt,num = c(4,5,6),method_cv = "leveneTest",method_Mc = "Tukey",sig_show  = "abc",ncol = 2,plot = "bar",plottype = "mui"){
  Mytheme <- theme_bw()+

    # scale_fill_manual(values = mi, guide = guide_legend(title = NULL))+
    theme(

      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),

      plot.title = element_text(vjust = -8.5,hjust = 0.1),
      axis.title.y =element_text(size = 20,face = "bold",colour = "black"),
      axis.title.x =element_text(size = 24,face = "bold",colour = "black"),
      axis.text = element_text(size = 20,face = "bold"),
      axis.text.x = element_text(colour = "black",size = 14),
      axis.text.y = element_text(colour = "black",size = 14),
      legend.text = element_text(size = 15,face = "bold"),
      legend.position = "none"#是否删除图例

    )

  data_wt = data
  norCv = MuiNorCV(data = data_wt,num = num,method_cv = "leveneTest")
  norCv

  # 将符合正态分布和方差齐性的分为一组，将两者有一个不符合的分为一组；
  AA = c()
  BB = c()
  for (i in 1:length(num)) {

    if (norCv[,"cor"][i] == TRUE & norCv[,"CV"][i] == TRUE) {
      num[i]
      AA = c(AA,num[i])
      AA

    }

    if (norCv[,"cor"][i] == FALSE | norCv[,"CV"][i] == FALSE) {
      num[i]
      BB = c(BB,num[i])
      BB

    }

  }

  # AA
  # BB



  resultAA = MuiaovMcomper(data = data_wt,num = AA,method_Mc = method_Mc )
  resultAA

  resultBB = MuiKwWlx(data = data_wt,num = BB)
  resultBB

  resultall = cbind(resultAA,resultBB)
  resultall



  num = c(AA,BB)
  if (plottype == "single") {
    if (plot == "bar") {
      plot = MuiPlotresultBar(data = data_wt,num = num,result = resultall,sig_show = sig_show )
      p = "Folder"
    }

    if (plot == "box") {
      plot = MuiPlotresultBox(data = data_wt,num = num,result = resultall,sig_show = sig_show)
      p = "Folder"
    }

    if (plot == "boxbar") {
      plot = MuiPlotresultBox(data = data_wt,num = num,result = resultall,sig_show = sig_show)
      p = "Folder"
    }


  }



  if (plottype == "mui") {
    if (plot == "bar") {
      result1 = FacetMuiPlotresultBar(data = data_wt,num = num,result = resultall,sig_show =sig_show,ncol = ncol )
      p = result1[[1]]
    }

    if (plot == "box") {
      result1 = FacetMuiPlotresultBox(data = data_wt,num = num,result = resultall,sig_show =sig_show,ncol =ncol )
      p = result1[[1]]

    }
    if (plot == "boxbar") {
      result1 = FacetMuiPlotReBoxBar(data = data_wt,num = num,result = resultall,sig_show =sig_show,ncol =ncol )
      p = result1[[1]]

    }



  }

  return(list(p, aov = AA,wlx = BB,table =resultall ))


}
