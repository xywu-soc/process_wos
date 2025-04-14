#'Perform GRDPG
#'Moved from previous B1
#'
#'Xingyun Wu
#'
#'10/26/2021

library(igraph)
library(grdpg)
library(mclust)

# set working directory
setwd('/Users/xywu/Documents/ra/citation_sasha/')

# load data
load('data_abc/network/graph1980to1981.rda')

# get the final A matrix to perform GRDPG
# if the largest component
A <- as_adj(gr_largest, type='both')
# if the whole network
#A <- as_adj(gr, type='both')

"## get refs in the largest component
# get the indices of nodes in the largest component
largest_comp <- which(gr_comp$membership == lcomp)
# extrace largest-component refs from all unique refs in bpt
refsA <- refs_uniq[largest_comp]
refsA <- as.data.frame(refsA)
refsA2 <- refsA %>%
  separate(refsA, c('year', 'journal', 'page'), ',')
Ajournal <- refsA2$journal"


########
# GRDPG
########

# set model parameters: grdpg with G=1:30, work=200
Kmax = 50
seed = 1234

# set G = 1 to see the runtime of ASE
ptm <- proc.time()
set.seed(seed)
results <- GRDPGwithoutCovariates(A, G = 1, work=200)
print(proc.time() - ptm)
#'user  system elapsed 
#'343.043  21.477 377.117 

# run Mclust separately
ptm <- proc.time()
set.seed(seed)
model <- Mclust(results$Xhat, G = 1:Kmax)
print(proc.time() - ptm)

# save output
save(results, model, file = 'data_abc/grdpg/grdpg1980to1981.rda')


## ignore the rest at this moment

#==========================================
# Attempt to do paralleling in Mclust part
#==========================================
# Not really speeds up in my attempt though

library(doParallel)
registerDoParallel(cores = 2)

# using 'foreach'
ptm <- proc.time()
df_res <- foreach (K = 1:Kmax, .combine='rbind') %dopar% {
  # Cluster based on estimated latent position
  set.seed(seed)
  model <- Mclust(results$Xhat, G = K)
  # output
  df <- tibble(Khat = K, modelName = model$modelName, bic = model$bic,
               block = max(model$classification))
  save(df, file="data_v12/mclust1978_allKhat.RData")
  df
}
print(proc.time() - ptm)

df_res %>%
  select(Khat = Khat, model = model)

dfAll <- as.data.frame(matrix(NA, nrow = Kmax, ncol = ))
colnames(dfAll) <- c('khat', 'modelName', 'BIC', 'clusters')
for(i in 1:Kmax){
  t <- df_res$model[i]
  dfAll[i,] <- c(df_res$Khat[i], t$modelName, t$bic, max(t$classification))
}

