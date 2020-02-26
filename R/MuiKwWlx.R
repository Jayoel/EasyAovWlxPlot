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
# result = MuiKwWlx(data = data_wt,num = c(4:6))
# result

####-------------多组数据做非参数检验
MuiKwWlx = function(data = data_wt,num = c(4:6)){
  N = num[1]


  data_wt = data
  result = KwWlx(data = data_wt, i= N)
  aa = result[[1]]
  name = colnames(data_wt[N])

  colnames(aa)[1] = name
  aa$group = NULL
  A = aa

  for (N in num[-1]) {
    result = KwWlx(data = data_wt, i= N)
    aa = result[[1]]
    name = colnames(data_wt[N])

    colnames(aa)[1] = name
    aa$group = NULL

    A=  cbind(A,aa)

  }

  return(A)

}
