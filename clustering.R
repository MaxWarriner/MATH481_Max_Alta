data('USArrests')
df <- scale(mtcars)
library(factoextra)

set.seed(123)
km.res <- kmeans(df, 4, nstart = 100)
km.res

aggregate(mtcars, by = list(cluster = km.res$cluster), mean)

fviz_nbclust(df, kmeans, method = "wss")

factoextra::fviz_cluster(km.res, data = df, )

library(cluster)
library(fpc)

pam_result <- pamk(df)



