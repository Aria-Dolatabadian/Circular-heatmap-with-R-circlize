library (circlize)
library(grid)
library(ComplexHeatmap)

dataURL <- "https://raw.githubusercontent.com/Aria-Dolatabadian/Circular-heatmap-with-R-circlize/main/data.txt"
expData.df<- read.table(dataURL,header=TRUE,sep="\t",stringsAsFactors=FALSE)
expData.mat <- as.matrix(expData.df[-c(1)])
rownames(expData.mat) <- expData.df$Mol
useT.mat <- t(expData.mat)
mat_list = useT.mat #changed to fit 1 heatmap
dend_list = as.dendrogram(hclust(dist(t(mat_list)))) #changed to calculate for 1 matrix
col_fun = colorRamp2(breaks= c(-13.288,-5.265,-6.674,-2.544,4.694,5.000), 
colors = c("blue4","lightblue","yellow","orange","orangered", "red")) 
#removed transparency so easier to save in EPS, very optional
#excluded 'factor' as there is only 1 heatmap

circos.par("start.degree" = 90,cell.padding = c(0, 0, 0, 0), gap.degree = 15) 
circos.initialize("a", xlim =c(0,292)) #changed to 1 heatmap setting
#adding new track for column label
circos.track(ylim = c(0, 1), bg.border = NA, track.height = 0.05, 
panel.fun = function(x, y) {
    for(i in seq_len(ncol(useT.mat))) {
        circos.text(i-0.5, 0, colnames(useT.mat)[order.dendrogram(dend_list)][i], adj = c(0, 0.5), 
            facing = "clockwise", niceFacing = TRUE,
         cex = 0.5)                
    }
})

circos.track(ylim = c(0, 2), bg.border = NA, panel.fun = function(x, y) {
m = mat_list 
dend = dend_list
#changed variable for 1 heatmap setting
m2 = m[, order.dendrogram(dend)]
col_mat = col_fun(m2)
nr = nrow(m2)
nc = ncol(m2)
for(i in 1:nr) {
    circos.rect(1:nc - 1, rep(nr - i, nc), 
        1:nc, rep(nr - i + 1, nc), 
        border = col_mat[i, ], col = col_mat[i, ])
}
#adding row label
circos.text(rep(1, 2), 1:2, 
        rownames(useT.mat), 
        facing = "downward", adj = c(1.45, 1.1), cex = 0.7) 

})
#Dendrogram
max_height = attr(dend_list, "height") #changed for 1 dendrogram setting
circos.track(ylim = c(0, max_height), bg.border = NA, track.height = 0.3, 
panel.fun = function(x, y) {
    dend = dend_list
    circos.dendrogram(dend, max_height = max_height)
})
circos.clear()

#adding legend key
library(ComplexHeatmap)
lgd_links = Legend(at=c(-15,-5,0,5),col_fun = col_fun, 
title_position = "topleft", title = "Value", direction = "horizontal")
draw(lgd_links, x = unit(1, "npc") - unit(2, "mm"), y = unit(4, "mm"), 
just = c("right", "bottom"))
