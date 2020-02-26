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


# # #使用案例
# # #输出结果第一个为图片，第二个是统计结果，第三个是统计方法
# result = SingleStat(data = data_wt,plot = "bar",method_Mc = "Tukey",i= 4,sig_show ="abc")
# # #导出图片
# p = result[[1]]
# p


###--------------------------------------走一个流程--正态检验---方差齐性---是否方差检验--是否非参数检验--s
# # 判断是否符合正态分布函数 TRUE为符合，F为不符合
# p1 >= 0.05
# #判断是否方差齐性
# p2 >.05
# data = data_wt
# plot = "bar"
# method_Mc = "Tukey"
# i= 4
# sig_show ="abc"
SingleStat = function(data = data_wt,plot = "bar",method_Mc = "Tukey",i= 4,sig_show ="abc"){

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

  NorCV = NorNorCVTest(data = data_wt, i= i,method_cv = "leveneTest")
  #zhen
  a = NorCV[[1]]
  p1 = length(a$p.value[-dim(a)[1]][!a$p.value[-dim(a)[1]] >= 0.05]) == 0
  p2 = NorCV[[2]]


  if (p1!= 0& p2 >.05) {
    result= aovMcomper (data = data_wt, i= i,method_Mc = "Tukey")
    A = print("aov")
    A
  } else if (p1  != 0| p2 <.05){
    result = KwWlx(data = data_wt, i= i)
    A = print("wlx")
    A
  }

  if (plot == "bar") {
    PlotresultBar = aovMuiBarPlot(data = data_wt, i= i,sig_show =sig_show,result = result[[1]])
    p = PlotresultBar[[1]]

  } else if (plot == "box"){
    PlotresultBox = aovMuiBoxP(data = data_wt, i= i,sig_show =sig_show,result = result[[1]])
    p = PlotresultBox[[1]]

  }else if (plot == "boxbar"){
    PlotresultBox = aovMuiBoxBarP(data = data_wt, i= i,sig_show =sig_show,result = result[[1]])
    p = PlotresultBox[[1]]

  }

  return(list(p,table = result[[1]],method =A))

}

