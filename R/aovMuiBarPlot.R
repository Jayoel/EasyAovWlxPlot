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


# # sig_show ="line"
# # data = data_wt
# # i= 3
# # #使用案例
# PlotresultBar = aovMuiBarPlot(data = data_wt, i= 3,sig_show ="abc",result = result[[1]])
# PlotresultBar[[1]]


###----使用方差检验结果和多重比较结果做展示：  柱状图展示
aovMuiBarPlot = function(data = data_wt, i= 3,sig_show ="line",result = result){
  library(ggplot2)
  result = result
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
  name_i = colnames(data_wt[i])
  #求取均值和方差
  wen1 = as.data.frame(tapply(as.vector(as.matrix(data_wt[i])),data_wt$group,mean,na.rm=TRUE))
  wen2 = as.data.frame(tapply(as.vector(as.matrix(data_wt[i])),data_wt$group,sd,na.rm=TRUE))
  went = cbind(wen1,wen2)

  colnames(went) = c("mean" ,"SD")
  went

  aa = result
  wentao = merge(aa,went, by="row.names",all=F)
  wentao
  # wentao$Row.names = NULL
  # FileName <- paste(plotname ,name_i,method_Mc,"_aov_bar", ".csv", sep = "_")
  # write.csv(wentao,FileName,quote = F)
  library(tidyverse)
  # colnames(wentao) = c(colnames(wentao[1:4]),"mean" ,"SD")
  #使用的tidyverse函数，对数据框添加两列，目的为柱状图添加bar
  aa = mutate(wentao, ymin = mean - SD, ymax =  mean + SD)
  a = max(aa$ymax)*1.2##用于设置y轴最大值

  ### 出图柱状图
  p = ggplot(aa , aes(x = group, y = mean,colour= group)) +
    geom_bar(aes(colour= group,fill = group),stat = "identity", width = 0.4,position = "dodge") +

    geom_errorbar(aes(ymin=ymin,
                      ymax=ymax),
                  colour="black",width=0.1,size = 1)+
    # geom_hline(aes(yintercept=mean(as.vector(as.matrix(data_wt[i])))), colour="black", linetype=2) +
    # geom_vline(aes(xintercept=0), colour="black", linetype="dashed") +
    scale_y_continuous(expand = c(0,0),limits = c(0,a))+#
    labs(
      # x=paste(name_i,"of all group", sep = "_"),
      y=name_i
      # title = paste("Normality test",p1,"Homogeneity of variance",p2,sep = ":")
    )
  p
  if (sig_show == "line") {
    zuhe = combn(aa$group,2)
    xxxx <- tapply(zuhe,rep(1:ncol(zuhe),each=nrow(zuhe)),function(i)i)
    xxxx
    sig_lis = rep("a",dim(zuhe)[2])
    for (i in 1:dim(zuhe)[2]) {
      library(tidyverse)
      library("ggsignif")

      if (filter(aa, group == xxxx[[i]][1])$groups == filter(aa, group == xxxx[[i]][2])$groups) {
        sig_lis[i] = "no_sig"
      }

      if (filter(aa, group == xxxx[[i]][1])$groups != filter(aa, group == xxxx[[i]][2])$groups) {
        sig_lis[i] = "*"
      }

    }

    p = p +
      geom_signif(comparisons = xxxx, annotations=sig_lis,
                  y_position = (seq(from=1, to=max(aa$mean)/4,length.out=dim(zuhe)[2]) + max(aa$mean)), tip_length = rep(0.03,dim(zuhe)[2]),color = "black")
    p
  }


  if (sig_show == "abc") {


    p = p + geom_text(aes(label = groups,y=ymax, x = group,vjust = -0.3,size = 6))
    p
  }
  #as.vector(as.matrix(data_wt[i]))为进行差异分析的一组数据
  p=p+Mytheme
  p

  if (length(unique(data_wt$group))>3){	p=p+theme(axis.text.x=element_text(angle=45,vjust=1, hjust=1))}

  return(list(p,wentao))
}
