#'One-Mode Projection
#'
#'Based on B1_v6.R
#'Change working directory from local to Dropbox
#'
#'Xingyun Wu
#'7/25/2022

library(igraph)


# set working directory
# setwd('/Users/xywu/Documents/ra/citation_sasha/')
setwd('~/Dropbox/HIVAIDS_Networks/')
setwd('C:/Users/xwu70/Dropbox/HIVAIDS_Networks')


######################
# One-mode projection
######################

# load data
load('data_abc/network/bpt1980to1981_76.rda')

## one-mode projection
# method 1: matrix operation
ptm <- proc.time()
refNet <- t(bpt) %*% bpt
print(proc.time() - ptm)
save(refNet, file = "E:/Restricted/citation/wu/network/onemode1980to1981_76.rda")

# implementation on several networks in a loop
for(y in 1965:1987){
  print(y)
  load(paste('data/network/bpt', as.character(y), 'to', 
             as.character(y+1), '_75.rda', sep = ''))
  print(dim(bpt))
  ptm <- proc.time()
  refNet <- t(bpt) %*% bpt
  print(proc.time() - ptm)
  save(refNet, file = paste('data/network/onemode', as.character(y),
                            'to', as.character(y+1), '_75.rda', sep = ''))
}

#'method 2 (alternative to method 1): nested looping
#'if built-in matrix multiplication doesn't work due to memory constraint, 
#'use nested looping instead
ptm <- proc.time()
refNet <- matrix(NA, nrow = ncol(bpt), ncol = ncol(bpt))
for(i in 1:ncol(bpt)){
  #print(i)
  for(j in 1:ncol(bpt)){
    ti <- bpt[, i]
    tj <- bpt[, j]
    refNet[i, j] <- as.numeric(ti %*% tj)
  }
}
print(proc.time() - ptm)
save(refNet, file = "E:/Restricted/citation/wu/network/onemode1980to1981_76.rda")


