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


# #使用案例
# res = KwWlx(data = data_wt, i= 4)
# res[[1]]


# data_wt = read.csv("./data_wt.csv")
# head(data_wt)
# data = data_wt
# i= 3
# save(data_wt, file = "./data/data_wt.rda")
# write.csv(data_wt,"./data_wt.csv")
#--------------------------------------------非参数检验------------------------------
KwWlx = function(data = data_wt, i= 3){
  data_wt = data

  #构造待分析的子数据框
  ss <- data_wt[i]
  colnames(ss) <- c("count")
  ss$group = data_wt$group
  ss


  library(ggpubr)
  library("multcompView")
  # 多组kw检验
  krusk=compare_means(count ~ group, data=ss, method = "kruskal.test")
  # krusk = kruskal.test(count ~ group, data=ss)

  sumkrusk=as.data.frame(krusk)
  sumkrusk
  #多组比较小于0.05，表明多组之间具有差异，可以进行两两非参数检验，并标记字母
  #但是这里没有做，个人认为挺难的，还

  #

  krusk=compare_means(count ~ group, data=ss, method = "wilcox.test")

  xx=as.data.frame(krusk)
  xx$group1
  wilcox_levels = paste(xx$group1,xx$group2,sep = "-")
  wilcox_levels = xx$p


  names(wilcox_levels) =  paste(xx$group1,xx$group2,sep = "-")

  wilcox.labels <- data.frame(multcompLetters(wilcox_levels, threshold = 0.05)['Letters'])
  colnames(wilcox.labels) = "groups"
  aa = wilcox.labels
  aa$group = row.names(aa)
  # wentao = merge(aa,went, by="row.names",all=F)
  # wentao
  #
  # # colnames(went) = c("mean" ,"SD")
  # aa = mutate(wentao, ymin = mean - SD, ymax =  mean + SD)
  # aa$group = levels(ss$group)
  # row.names(aa) = aa$Row.names
  aa
  # FileName <- paste(plotname ,name_i,"_wilcox.test_YES_bar", ".csv", sep = "_")
  # write.csv(aa,FileName,quote = F)

  return(list(aa,wilcox = krusk,kruskal = sumkrusk))

}
