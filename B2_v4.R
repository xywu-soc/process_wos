#'Get network statistics
#'Add normalized betweenness in addition to raw scale
#'
#'Based on B2_v3.R
#'
#'Updates:
#'  1.Add the option to obtain network statistics of weighted network instead of unweighted network
#'    a) Archive the previous network stats of unweighted network 
#'      in Dropbox > data > network > unweighted
#'    b) Add the network stats of weighted network in Dropbox > data > network > weighted
#'  2.Add the option for adding network stats of largest component instead of doing it by default
#'
#'Xingyun Wu
#'
#'Initial: 8/8/2023

library(igraph)
library(CINNA)
library(dplyr)
library(tidyr)

options(scipen=999)

# setwd('/Users/xywu/Documents/ra/citation_sasha/')
setwd('~/Dropbox/HIVAIDS_Networks/')
setwd('C:/Users/xwu70/Dropbox/HIVAIDS_Networks')


############
# Functions
############

get_network_stats <- function(nw_graph, id_vec = NULL, nw_comp = NULL){
  # generate an empty data frame
  if(is.null(id_vec)){
    nw_stats <- as.data.frame(as.numeric(V(nw_graph)))
  }
  else{
    nw_stats <- as.data.frame(id_vec)
  }
  # mark the column as rid
  colnames(nw_stats) <- 'rid'
  
  # component distribution
  if(!is.null(nw_comp)){
    # number of components
    cat('Number of components: ', nw_comp$no, '\n')
    # component and its distribution
    print('Component distribution: ')
    print(summary(nw_comp$csize))
    cat('The number of isolated nodes: ', 
        length(which(nw_comp$csize == 1)), '\n')
    nw_stats$component <- nw_comp$membership
  }
  
  # degree centrality
  print('Degree...')
  nw_stats$degree_raw <- degree(nw_graph, normalized = F)
  nw_stats$degree_norm <- degree(nw_graph, normalized = T)
  
  # betweenness centrality
  print('Betweenness centrality...')
  nw_stats$betweenness_raw <- betweenness(nw_graph, directed = F, normalized = F)
  nw_stats$betweenness_norm <- betweenness(nw_graph, directed = F, normalized = T)
  
  # eigenvector centrality
  print('Eigenvector centrality...')
  nw_stats$eigenvector_centrality <- eigen_centrality(nw_graph)$vector
  # print out eigenvalue
  cat('Eigenvalue of eigenvector centrality: ', 
      eigen_centrality(nw_graph)$value, '\n')
  
  # # closeness centrality
  # print('Closeness centrality...')
  # nw_stats$closeness <- closeness(nw_graph)
  # 
  # # harmonic centrality
  # print('Harmonic centrality...')
  # nw_stats$harnomic_centrality <- harmonic_centrality(nw_graph)
  # 
  # # transitivity of the whole network
  # print('Transitivity...')
  # cat('Global transitivity: ', transitivity(nw_graph), '\n')
  # # local transitivity
  # nw_stats$local_transitivity <- transitivity(nw_graph, type = 'local')
  
  # print out summary statistics
  print('Summary of network stats:')
  print(summary(nw_stats))
  
  # output
  return(nw_stats)
}


main <- function(startYear, endYear, unweighted = F, rmMaxBc = F, getLargestComp = F){
  # load the weighted network from one-mode projection
  print('Load the weighted one-mode network...')
  load(paste('data/network/onemode', as.character(startYear), 'to',
             as.character(endYear), '_75.rda', sep = ''))
  
  
  # turn diagonal to 0 to disable self-loop
  diag(refNet) <- 0
  dim(refNet) # 16394 16394
  
  # get unweighted network with diagonal turned to 0
  if(unweighted){
    print('Turn it into unweighted network...')
    refNet[refNet > 1] <- 1
  }
  else{
    print('Keep it as weighted network...')
  }
  
  # generate igraph object
  gr <- graph_from_adjacency_matrix(refNet, mode = 'undirected', diag = F, weighted = T)
  
  # obtain edge list
  if(unweighted){
    elist <- get.edgelist(gr, names = F)
  }
  else{
    elist <- cbind(get.edgelist(gr, names = F), E(gr)$weight)
  }
  colnames(elist) <- c('v1', 'v2', 'wt')
  # output to csv for ORC computation
  write.csv(elist, row.names = F,
            file = paste('data/network/', ifelse(unweighted, 'unweighted', 'weighted'), 
                         '/edges', as.character(startYear), 'to', 
                         as.character(endYear), '.csv', sep = ''))
  
  
  print('Get network components...')
  # components
  gr_comp <- components(gr)
  # get the largest component
  lcomp <- which(gr_comp$csize == max(gr_comp$csize))
  gr_largest <- delete.vertices(gr, V(gr)[which(gr_comp$membership != lcomp)])
  
  # save the graph objects before next step
  save(gr, gr_largest, gr_comp, 
       file = paste('data/network/', ifelse(unweighted, 'unweighted', 'weighted'), 
                    '/graph', as.character(startYear), 
                    'to', as.character(endYear), '.rda', sep = ''))
  
  # get network statistics
  print('Get network stats for the whole network...')
  
  # the whole network
  statsWhole <- get_network_stats(gr, id_vec = NULL, nw_comp = gr_comp)
  # output
  write.csv(statsWhole, row.names = F,
            file = paste('data/network/', ifelse(unweighted, 'unweighted', 'weighted'), 
                         '/stats', as.character(startYear), 
                         'to', as.character(endYear), 'whole.csv', sep = ''))
  
  # the largest component
  if(getLargestComp){
    print('Get network stats for the largest component...')
    idLargest <- which(gr_comp$membership == lcomp)
    statsLargest <- get_network_stats(gr_largest, id_vec = idLargest,
                                      nw_comp = NULL)
    # output
    write.csv(statsLargest, row.names = F,
              file = paste('data/network/', ifelse(unweighted, 'unweighted', 'weighted'), 
                           '/stats', as.character(startYear), 
                           'to', as.character(endYear), 'largest.csv', sep = ''))
  }
  
  # the counterfactual network without maximum betweenness
  if(rmMaxBc){
    print('Get network stats after the removal of max. betweenness...')
    # get the row & column index of the reference with maximum betweenness
    maxBc <- which(statsWhole$betweenness == max(statsWhole$betweenness))
    cat('The node id with largest betweenness centrality: ', maxBc, '\n')
    # remove the reference from the network
    refNet2 <- refNet[-maxBc, -maxBc]
    # generate a new graph for the counterfactual
    gr2 <- graph_from_adjacency_matrix(refNet2, mode='undirected', diag=F)
    gr_comp2 <- components(gr2)
    # get stats
    idRmMax <- as.numeric(rownames(refNet2))
    statsRmMax <- get_network_stats(gr2, id_vec = idRmMax, nw_comp = gr_comp2)
    # output
    write.csv(statsRmMax, row.names = F,
              file = paste('data/network/', ifelse(unweighted, 'unweighted', 'weighted'), 
                           '/stats', as.character(startYear), 
                           'to', as.character(endYear), 'rmMaxBc.csv', sep = ''))
  }
  
  # return the calculated network stats
  statsFinal <- list()
  statsFinal$whole <- statsWhole
  if(exists('statsLargest')){
    statsFinal$largest <- statsLargest
  }
  if(exists('statsRmMax')){
    statsFinal$rmMax <- statsRmMax
  }
  return(statsFinal)
}


############
# Implement
############

stats1980to1981 <- main(startYear = 1980, endYear = 1981, unweighted = F,
                        rmMaxBc = F, getLargestComp = F)

stats1975to1979 <- main(startYear = 1975, endYear = 1979, unweighted = F,
                        rmMaxBc = F, getLargestComp = F)

# implementation on several networks in a loop
for(y in 1965:1987){
  cat('\n', as.character(y), 'to', as.character(y + 1), '\n')
  tmp <- main(startYear = y, endYear = (y + 1), unweighted = F,
              rmMaxBc = F, getLargestComp = F)
}
