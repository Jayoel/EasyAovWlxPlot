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


# # 使用案例
# norCv = MuiNorCV(data = data_wt,num = c(4,5,6),method_cv = "leveneTest")
# norCv


## ---------------------第一部分----------------多组数据正态分布和方差齐性分析
# num = c(4,6)
# N =4
MuiNorCV = function(data = data_wt,num = c(4:6),method_cv = "leveneTest"){
  data_wt = data
  s1 = rep("A",length(num))
  s2 = rep("A",length(num))
  s0 = rep("A",length(num))
  i = 1
  for (N in num) {

    resul = NorNorCVTest(data = data_wt, i= N ,method_cv = "leveneTest")
    a = resul[[1]]
    b = resul[[2]]
    name = colnames(data_wt[N])
    a1 = length(a$p.value[-dim(a)[1]][!a$p.value[-dim(a)[1]] >= 0.05]) == 0
    b1 = b >=.05
    s1[i] = a1
    s2[i] = b1
    s0[i] = name
    i = i+1
  }

  result = cbind(s0,s1,s2)
  colnames(result) = c("DI","cor","CV")
  return(result)

}
