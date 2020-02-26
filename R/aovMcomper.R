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

# result= aovMcomper (data = data_wt, i= 5,method_Mc = "Tukey")
# # 提取多重比较结果
# result[[1]]

# 根据数据做方差分析和多重比较，指定多重比较发方法；
aovMcomper = function( data = data_wt, i= 3,method_Mc = "Tukey"){

  data_wt = data

  #构造待分析的子数据框
  ss <- data_wt[i]
  colnames(ss) <- c("count")
  ss$group = data_wt$group

  #进行方差检验 下面wtx3为提取的p值
  model<-aov(count ~ group, data= ss)#方差分析
  wtx1 = summary(model)
  wtx2 = wtx1[[1]]
  wtx3 = wtx2[5]#

  if (method_Mc == "Tukey") {
    library(multcomp)

    litter.mc <- glht(model, linfct = mcp(group = 'Tukey'))
    # summary(litter.mc)
    insx = cld(litter.mc)
    aa <- insx$mcletters$monospacedLetters
    aa = as.data.frame(aa)
    colnames(aa) = c("groups")
    head(aa)
    aa$group = row.names(aa)

  }


  if (method_Mc == "LSD") {
    out <- LSD.test(model,"group", p.adj="none")#进行多重比较，不矫正P值
    aa = out$group#结果显示：标记字母法
    aa$group = row.names(aa)
    aa
  }
  #【SNK法】（Student-Newman-Keuls）程序运行结果与LSD.test类似。
  if (method_Mc == "SNK") {
    library("agricolae")
    out <- SNK.test(model,"group")
    aa = out$groups# 查看每个组的label

    aa$group = row.names(aa)
    stat = aa
    aa
  }

  #【Duncan法】(新复极差法)（SSR）
  if (method_Mc == "Duncan") {
    # library("agricolae")
    out <-duncan.test (model,"group")
    aa = out$groups# 查看每个组的label

    aa$group = row.names(aa)
    stat = aa
    aa
  }

  #Scheffe检验特点：各组样本数相等或不等均可以，但是以各组样本数不相等使用较多；
  if (method_Mc == "scheffe") {
    # library("agricolae")
    out <-scheffe.test (model,"group")
    aa = out$groups# 查看每个组的label

    aa$group = row.names(aa)
    stat = aa
    aa
  }

  return(list(Muicomper = aa,model))
}
