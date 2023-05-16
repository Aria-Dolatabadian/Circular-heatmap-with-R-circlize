library(circlize)

m <- read.csv("random_data.csv")

circos.heatmap(m, col = colorRamp2(c(-2, 0, 2), c("blue", "white", "red")))
