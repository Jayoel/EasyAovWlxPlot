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
# library(EasyAovWlxPlot)
# library(ggplot2)
# result= aovMcomper (data = data_wt, i=3,method_Mc = "Tukey")
# # 提取多重比较结果
# result[[1]]
#
# PlotresultBox = aovMuiBoxP(data = data_wt, i= 3,sig_show ="abc",result = result[[1]])
# p = PlotresultBox[[1]]
# p
# data_wt
#
# data = data_wt
# i= 3
# sig_show ="abc"
# result = result[[1]]
#

###----使用方差检验结果和多重比较结果做展示：  箱线图展示
aovMuiBoxP = function(data = data_wt, i= 3,sig_show ="line",result = result){
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
  aa = result
  print(i)
  name_i = colnames(data_wt[i])
  name_i
  data_box = data_wt[c(1,2,i)]
  colnames(data_box) = c("ID" , "group","dd" )
  library(tidyverse)
  library(ggplot2)
  library(XML)
  library(plyr)
  data_box$stat=aa[as.character(data_box$group),]$groups


  max=max(data_box[,c("dd")])
  min=min(data_box[,c("dd")])
  x = data_box[,c("group","dd")]

  y = x %>% group_by(group) %>% summarise_(Max=paste('max(',"dd",')',sep=""))
  y=as.data.frame(y)
  y
  rownames(y)=y$group
  data_box$y=y[as.character(data_box$group),]$Max + (max-min)*0.05

  # mi=c("#1B9E77" ,"#D95F02", "#7570B3","#E7298A")
  # FileName <- paste(plotname ,name_i,method_Mc,"aov_box", ".csv", sep = "_")
  # write.csv(data_box,FileName,quote = F)

  head(data_box)
  p = ggplot(data_box, aes(x=group, y=data_box[["dd"]], color=group)) +
    geom_boxplot(alpha=1, outlier.size=0, size=0.7, width=0.5, fill="transparent") +
    labs(
      y=name_i)+
    geom_jitter( position=position_jitter(0.17), size=1, alpha=0.7)+theme(legend.position="none")+
    geom_hline(aes(yintercept=mean(data_box$dd)), colour="black", linetype=2) +
    geom_vline(aes(xintercept=0), colour="black", linetype="dashed")
  p
  if (sig_show == "abc") {
    p = p +
      geom_text(data=data_box, aes(x=group, y=y, color=group, label= stat))

    p
  }
  wtq = levels(data_wt$group)
  lis = combn(levels(data_wt$group), 2)
  x <-lis
  my_comparisons <- tapply(x,rep(1:ncol(x),each=nrow(x)),function(i)i)

  # if (sig_show == "line") {
  #
  #
  #
  #   p = p +
  #     stat_compare_means()+
  #     stat_compare_means(comparisons=my_comparisons,label = "p.signif",hide.ns = F) # Add pairwise
  #
  #   p
  # }

  if (sig_show == "line") {
    zuhe = combn(aa$group,2)
    xxxx <- tapply(zuhe,rep(1:ncol(zuhe),each=nrow(zuhe)),function(i)i)
    xxxx
    sig_lis = rep("a",dim(zuhe)[2])
    for (i in 1:dim(zuhe)[2]) {
      library(tidyverse)

      if (filter(aa, group == xxxx[[i]][1])$groups == filter(aa, group == xxxx[[i]][2])$groups) {
        sig_lis[i] = "no_sig"
      }

      if (filter(aa, group == xxxx[[i]][1])$groups != filter(aa, group == xxxx[[i]][2])$groups) {
        sig_lis[i] = "*"
      }

    }



    p = p +
      geom_signif(comparisons = xxxx, annotations=sig_lis,
                  y_position = (seq(from=1, to=max(data_box$dd)/4,length.out=dim(zuhe)[2]) + max(data_box$dd)), tip_length = rep(0.03,dim(zuhe)[2]),color = "black")
    p
  }

  p=p+Mytheme
  p

  return(list(p,data_box))
}
