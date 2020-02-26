# \item{data}{输入数据框，第一列为样本编号，第二列为分组，注意分组标签必须设定为group，第三列以后就是测定或者收集的指标了}
#
# \item{num}{代表您想要进行统计的列,这里可以输入多个列，只需要指定列号即可：例如：num = c(4:6)}
#
# \item{sig_show}{代表差异展示方式；sig_show ="abc"是使用字母表示;sig_show ="line"是使用连线和星号表示；如果是NA，那么就不显示显著性结果}
#
# \item{result}{代表显著性差异分析结果，是一个数据框，每一列是显著性标记字母,MuiKwWlx}
# \item{ncol}{代表分面展示每一行放几张图}


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
data(data_wt)


# # #使用案例
# result = MuiKwWlx(data = data_wt,num = c(4:6))
# result
# result1 = FacetMuiPlotresultBar(data = data_wt,num = c(4:6),result = result,sig_show ="abc",ncol = 2 )
# result1[[1]]

##-----------现在我们需要做多组分面图形--第二种， 柱状图
# num = c(4:6)
# N =4
FacetMuiPlotresultBar = function(data = data_wt,num = c(4:6),result = result,sig_show ="abc",ncol = 3 ){
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
  N = num[1]

  name = colnames(data_wt[N])

  as = result[match( name,colnames(result))]
  colnames(as) = "groups"
  as$group = row.names(as)

  PlotresultBar = aovMuiBarPlot(data = data_wt, i= N,sig_show =sig_show,result = as)

  p = PlotresultBar[[2]]
  p
  name = colnames(data_wt[N])
  p$name = name
  A = p

  for (N in num[-1]) {

    name = colnames(data_wt[N])

    as = result[match( name,colnames(result))]
    colnames(as) = "groups"
    as$group = row.names(as)

    PlotresultBox = aovMuiBarPlot(data = data_wt, i= N,sig_show =sig_show,result = as)

    p = PlotresultBox[[2]]
    p
    name = colnames(data_wt[N])
    p$name = name

    A = rbind(A,p)
  }
  head(A)

  # a = max(A$SD)*1.2
  p<-ggplot(A, aes(x=group , y=mean ))+
    geom_bar(aes(colour= group,fill = group),stat = "identity", width = 0.4,position = "dodge") +

    geom_errorbar(aes(ymin=mean - SD,
                      ymax=mean+SD),
                  colour="black",width=0.1,size = 1)+

    scale_y_continuous(expand = c(0,0))+#,limits = c(0,a)
    labs(
      # x=paste(name_i,"of all group", sep = "_"),
      # y=name_i
      # title = paste("Normality test",p1,"Homogeneity of variance",p2,sep = ":")
    ) +
    theme_classic()+
    geom_text(data=A, aes(x=group , y=mean+SD ,label=groups))+
    guides(color=guide_legend(title = NULL),shape=guide_legend(title = NULL))+facet_wrap(.~name,scales="free_y",ncol  = ncol)
  p

  p=p+Mytheme
  p

  if (length(unique(data_wt$group))>3){	p=p+theme(axis.text.x=element_text(angle=45,vjust=1, hjust=1))}
  p


  # path = "./Muibar_Facet/"
  # dir.create(path)
  # FileName <- paste(path,name,"Facet_bar", ".pdf", sep = "_")
  # ggsave(FileName, p, width = 8, height = 8)

  return(list(p,table = A))
}



