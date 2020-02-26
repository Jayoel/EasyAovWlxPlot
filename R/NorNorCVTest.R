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





# ##使用案例
# NorCV = NorNorCVTest(data = data_wt, i= 4,method_cv = "leveneTest")
# #zhen
# NorCV[[1]]










# 依赖关系检测与安装
p_list = c("ggplot2","tidyverse","agricolae","reshape2","ggpubr","ggsignif","vegan")
for(p in p_list){
  if (!requireNamespace(p)){
    install.packages(p)}
  library(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)}

##----监测数据正态性和方差齐性：-------
#正态检验：全部组别都需要正态分布，看p值或者后面的标签，是否全部都是正态分布； p2 >=.05:方差齐性
NorNorCVTest = function(data = data_wt, i= 4,method_cv = "leveneTest"){
  data_wt = data
  #构造待分析的子数据框
  ss <- data_wt[i]
  colnames(ss) <- c("count")
  ss$group = data_wt$group


  shapiro.test.multi <- function(   #定义函数名
    data,   #定义函数第一个参数
    value,  #第2参数
    group)  #第3参数
  {       #开始计算

    require(magrittr)   #按需要加载管道函数包

    table(data[,group]) %>%   #提取分组信息，此处即为统计group中值出现的次数，达到了去重的目的
      data.frame(.) -> a1   #将提取信息从table格式转为数据库data.frame并存为a1，这样才能提取其中一列转为向量

    a2 <- as.vector(a1[,1])  #将a1数据的第一列转为向量，这一列即为不重复的分组信息

    data = data.frame(group = data[,group],  #对数据集进行关键变量的提取，提取分组变量为data数据集的group变量
                      value = data[,value])  #提取计算值为data数据集的value

    test.result <- data.frame(No=0,        #行号
                              Name=0,      #分组名
                              W=0,         #W值
                              p.value=0,   #p值
                              norm.test=0) #检测结果

    for (i in (1:length(a2))){     #定义for循环计算，从1到a2的长度值这样一个区间，a2的长度即为分组的数量
      subset(data,                 #指定取数据集    换行写代码使层次更清晰
             group == a2[i],       #定义去子集的条件，“==”为判断
             select = value) %>%   #定义需要取集的变量/列，“=”为定义
        .[,1] %>%                  #  "."定义计算结果放置的位置
        shapiro.test(.) -> t.r     #进行正态检验分布并存储为t.r

      test.result[i,1] = i              #存储组序号
      test.result[i,2] = a2[i]          #存储分组名
      test.result[i,3] = t.r$statistic  #存储W统计量
      test.result[i,4] = t.r$p.value    #存储计算的p值

      if      #if判断
      (t.r$p.value > 0.05)           #判断条件
        test.result[i,5] = "Norm"    #通过判断后的命令
      else
        test.result[i,5] = "Other_situation"  #未通过判断执行的命令
    } #结束循环计算

    test.result[nrow( test.result)+1,1] = "Test Method:"  #给数据框加上检验正态分布方法信息，在最后一行之后加上一行，在第1列放入次数据
    test.result[nrow( test.result),2] = "Shapiro-Wilk"    #同上行，存在第二列

    test.result  #显示用于存储计算结果的数据框
  }              #脚本结束

  a = shapiro.test.multi(data_wt[c(2,i)],value = colnames(data_wt[c(2,i)][2]),group = "group")
  # p2 >=.05:方差齐性
  if (method_cv == "leveneTest" ) {
    library("car")
    xc <- leveneTest(count~group,data=ss)
    (p2 <- xc[[3]][1])

  }


  if (method_cv == "bartlett.test" ) {

    xc <- bartlett.test(count~group,data=ss)
    (p2 <- xc[[3]])
    p2 <- round(p2,3)


  }

  return(list(a,p2))

  # a$norm.test[-dim(a)[1]] == "Norm"



}
