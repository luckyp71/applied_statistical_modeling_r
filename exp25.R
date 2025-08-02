# Principal Component Analysis (PCA)

require(datasets)
require(dplyr)
require(ggplot2)
require(plotly)

log_x <- log(iris[, 1:4])
head(log_x)

ir_species <- iris[, 'species']
head(ir_species)

# apply PCA - scale. = TRUE is highly advisable, the default is FALSE
ir_pca <- prcomp(log.x,
       center=T,
       scale. = T)

ir_pca

# check which PC explains maximum variability in data which PC to retain
plot(ir_pca, type='l')

summary(ir_pca)
