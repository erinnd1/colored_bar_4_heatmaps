# colored_bar_4_heatmaps

###extract the colors in TSNE plot#####
p<-TSNEPlot(meoh, pt.size = 2, do.label = T, label.size = 12, no.legend=T)
aa<-p$data
aa<-aa[[1]]
ab<-meoh@ident
ab<-data.frame(ab)
ab$color<-aa$colour
##the second ab is the name of the col, not the object itself##
c1<-subset(ab, ab=="PT (S3)")
c2<-subset(ab, ab==1)
c3<-subset(ab, ab==2)
c4<-subset(ab, ab==3)
c5<-subset(ab, ab==4)
new1<-rbind(c1, c2)
new1<-rbind(new1, c3)
new1<-rbind(new1, c4)
new1<-rbind(new1, c5)

###Extract the data matrix from DoHeatmap and plot it out using heatmap.2 with adjusted parameters#####
pt_heatmap<-DoHeatmap(meoh, genes.use = top10$gene, order.by.ident = TRUE, slim.col.label = TRUE, remove.key = TRUE, do.return = T)
minmax=function(data,min,max) {
  data2=data
  data2[data2>max]=max
  data2[data2<min]=min
  return(data2)
}
data.use=minmax(pt_heatmap,min=-2.5,max=2.5)
heatmap.2(data.use,dendrogram="none",trace="none",Rowv=FALSE,Colv = FALSE,  ColSideColors=new1$color, labCol=FALSE,col = c("lemonchiffon1","lemonchiffon1","lemonchiffon1","pink", "purple"))


##gives you a centered-ish heatmap w/o key; no color coding##
heatmap.2(data.use,dendrogram="none",trace="none",Rowv=FALSE,Colv = FALSE, key = FALSE, lmat=rbind(c(2),c(3),c(1),c(4)), lhei = c(.25,.25,3,.25), lwid = 1, labCol=FALSE,margins = c(2,3), col = c("lemonchiffon1","lemonchiffon1","lemonchiffon1","pink", "purple"))

##gives you a top centered ish heatmap with bar but no gene names##
heatmap.2(data.use,dendrogram="none",trace="none",Rowv=FALSE,Colv = FALSE, key = FALSE, lmat=rbind(c(2),c(3),c(1),c(4),c(5)), lhei = c(4,.25,.25,.25,3), lwid = 0.0001, labCol=FALSE,  ColSideColors=new1$color, margins = c(.25,.25), col = c("lemonchiffon1","lemonchiffon1","lemonchiffon1","pink", "purple"))

##closest so far##
heatmap.2(data.use,dendrogram="none",trace="none",Rowv=FALSE,Colv = FALSE, key = FALSE, lmat=rbind(c(2),c(3),c(1),c(4),c(5)), lhei = c(4,.25,.25,.25,3), lwid = 1, labCol=FALSE,  ColSideColors=new1$color, margins = c(.05,4), col = c("lemonchiffon1","lemonchiffon1","lemonchiffon1","pink", "purple"))
