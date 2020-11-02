##################################################
## Project: 数学建模实验_14（聚类分析）
## Script purpose: 《数学实验与建模》实验报告，具体见word文档
## Date: 2020-06
## Author: Minghao Du
## email: 0108170318@csu.edu.cn / 695948191@qq.com
##################################################

#####Packages#####
library(openxlsx)
library(ggplot2)
library(dplyr)
library(dendextend)

#####Load the data#####
xlsxFile <- '/Users/Minghao/课程/2020_数模/实验/data.xlsx'
data <- read.xlsx(xlsxFile, rowNames=TRUE ,rows=(1:14), cols=c(1:11)) 

#####Start analysus#####
par(family='STKaiti') # 为了让系统画图显示汉字

## 使用欧式距离(euclidean)
# transform the data to distance matrix
dis_euc <- dist(data, method='euclidean')
# clustering analysis
ca_euc <- hclust(dis_euc, method = 'average')  #使用平均距离'average'
# Convert the "hclust" object into a "dendrogram" object
den_euc <- as.dendrogram(ca_euc)
# Caculate the cophenet correlation coefficient
cor_euc <- cor(dis_euc, cophenetic(ca_euc))
# Use standard colours for clusters
clusters_euc <- cutree(den_euc, 3)[order.dendrogram(den_euc)]
den_euc %>% set("branches_k_color", k = 3, value = unique(clusters_euc) + 1) %>% plot(main='Cophenetic correlation = 0.7794')
# Add a coloured bar 
colored_bars(clusters_euc + 1,
             rowLabels = paste(3, "clusters"))



## 使用最长距离(Furthest neighbor),重心聚类(centroid)
# transform the data to distance matrix
dis_max <- dist(data, method='maximum')
# clustering analysis
ca_max <- hclust(dis_max, method = 'average')  #使用平均距离'average'
# Convert the "hclust" object into a "dendrogram" object
den_max <- as.dendrogram(ca_max)
# Caculate the cophenet correlation coefficient
cor_max <- cor(dis_max, cophenetic(ca_max))
# Use standard colours for clusters
clusters_max <- cutree(den_max, 3)[order.dendrogram(den_max)]
den_max %>% set("branches_k_color", k = 3, value = unique(clusters_max) + 1) %>% plot(main='Cophenetic correlation = 0.7795')
# Add a coloured bar 
colored_bars(clusters_max + 1,
             rowLabels = paste(3, "clusters"))


## Compate two clustering analysis results
den_em <- dendlist(den_euc, den_max)
tanglegram(
  untangle(den_em),
  sort = TRUE,
  common_subtrees_color_branches = TRUE,
  main_left = "Eulidean",
  main_right = "Furthest neighbor" )











