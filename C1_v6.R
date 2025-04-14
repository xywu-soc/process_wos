#'Covariate construction for co-citation network post-analysis
#'
#'Updates based on C1_v5.R
#'1. Use stats from weighted networks instead of unweighted networks
#'2. Changes in field-journal classification:
#'  A) Modify the grouping of top journals: non-field journals
#'  B) Retain the coding for MMWR in field, which was coded into NA in v5
#'  C) Code the 'AIDS' references into GM before the establishment of 'AIDS' journal
#'3. Use 'tier' than 'rank' throughout. Previously use 'tier' for earlier version of
#'   tier-journal classification and 'rank' for an adjusted classification.
#'
#'Xingyun Wu
#'
#'Initial update: 8/29/2023
#'Latest update: 8/29/2023

library(stringr)
library(readxl)
# library(xlsx)
library(tidyr)
library(dplyr)

options(scipen=999)


setwd('~/Dropbox/HIVAIDS_Networks/')
setwd('C:/Users/xwu70/Dropbox/HIVAIDS_Networks')


####################
# Update Covariates
####################
# added in v5, only run once

#======================
# Field Classification
#======================
# added in v5, only run once

# code Science and Nature as non-field journals
load('data/journal_data/fieldsAll.rda')
fieldsAll$field <- ifelse(fieldsAll$journal %in% c('SCIENCE', 'NATURE', 'P NATL ACAD SCI USA',
                                                   'MMWR-MORBID MORTAL W'), 
                          'Non-Field Journals', fieldsAll$field)

# summary table
fieldSum <- as.data.frame(fieldsAll %>% group_by(field) %>% 
  summarise(n_journal = n(), p = round(n_journal / dim(fieldsAll)[1], 3)))

# output
save(fieldsAll, fieldSum, file = 'data/journal_data/fieldsAll_v3.rda')


###################
# Nodal covariates
###################

#===========
# functions
#===========

#' auxiliary function to construct nodal covariates
get_cov <- function(startYear, endYear, addStats = T){

  # load the reference list first
  load(paste('data/network/bpt', as.character(startYear), 'to', 
             as.character(endYear), '_75.rda', sep = ''))
  # general lists
  load('data/journal_data/updatedRank.rda')
  # journal-fields
  load('data/journal_data/fieldsAll_v3.rda') # modified on 8/29/2023 to use v3
  
  # separate the concatenated reference info into different columns
  refCov <- separate(uniqRefs, 'final', c('year', 'journal', 'page'),
                     sep=",", extra="drop")
  # re-order the columns
  refCov <- refCov[, c('rid', 'year', 'journal', 'page')]
  # removed redundant code to combine 'CANCER-AM CANCER SOC' into 'CANCER' on 1/30/2023
  
  # add mutually exclusive covariates for top9-top49-top75, simplified in v5
  refCov$tier_id <- ifelse(refCov$journal %in% top9, 1,
                           ifelse(refCov$journal %in% top49, 2,
                                  ifelse(refCov$journal %in% topAIDS, 3, -1)))
  if(-1 %in% refCov$tier_id){
    stop('Extra journal detected. Inspect before run.')
  }
  refCov$tier <- ifelse(refCov$tier_id == 1, 'top09',
                        ifelse(refCov$tier_id == 2, 'top49', 'topAIDS'))
  
  # merge field into reference data by journal
  refCov$field <- NA
  for(i in 1:dim(fieldsAll)[1]){
    tid <- which(refCov$journal == fieldsAll$journal[i])
    refCov[tid, 'field'] <- fieldsAll$field[i]
  }
  
  # optional: merge with nodal network statistics from B2
  if(addStats){
    netStat <- read.csv(paste('data/network/weighted/stats', # use weighted network status & csv, modified on 8/29/2023
                              as.character(startYear), 'to', 
                              as.character(endYear), 'whole.csv', sep = ''))
    refCov <- left_join(x = refCov, y = netStat, by = 'rid')
  }
  
  # # output the covariates
  # save(refCov, file = paste('data/cov/cov', as.character(startYear), 
  #                           '-', as.character(endYear), '.rda', sep = ''))

  # return
  return(refCov)
}


#' modified based on inspect_cov from v2 on 7/18/2022 
#' to update existing cov files rather than creating redundant csv files
main <- function(startYear, endYear, addStats = TRUE, orderBC = FALSE, 
                 topNum = NULL, print_sum = FALSE){
  
  # print netowrk name
  print(paste('Network', as.character(startYear), 
              '-', as.character(endYear)))
  
  refDf <- get_cov(startYear, endYear, addStats = addStats)
  
  # print distribution of betweenness centrality
  if(print_sum){
    print('Distribution of the raw betweenness:')
    print(summary(refDf$betweenness_raw))
    print('Distribution of normalzied betweenness:')
    print(summary(refDf$betweenness_norm))
    cat('The number of nodes with betweenness = 0 is', 
        length(which(refDf$betweenness_raw == 0)),
        ", and it's", 
        round(length(which(refDf$betweenness_raw == 0))/dim(refDf)[1], 3),
        'to the total number of nodes\n')
    cat('Distribution of the log of betweenness (+ 0.5 on all nodes before taking log):\n') 
    cat(summary(log(refDf$betweenness_raw + 0.5)), '\n')
  }
  
  # order rows by betweenness centrality (descending order)
  if(orderBC){
    refDf <- refDf[order(refDf$betweenness_raw, decreasing = T),]
    cat('\nOrdered by betweenness\n\n')
  }
  
  # extract the top references on betweenness if specified
  if(!is.null(topNum)){
      refDf <- refDf[1:topNum,]
      suffix <- paste('_top', as.character(topNum), sep = '')
  }
  else{
    suffix <- '' # changed from '_ordered_' to '' on 7/18/2022
    # to update existing file to avoid redundant intermediate data files
  }

  # round centrality measures
  refDf$betweenness_raw <- refDf$betweenness_raw
  refDf$betweenness_norm <- refDf$betweenness_norm
  refDf$degree_norm <- refDf$degree_norm
  refDf$eigenvector_centrality <- refDf$eigenvector_centrality
  # if local transitivity calculated
  if('closeness' %in% colnames(refDf)){
    refDf$closeness <- refDf$closeness
    refDf$harnomic_centrality <- refDf$harnomic_centrality
    refDf$local_transitivity <- refDf$local_transitivity
  }
  
  # rename row numbers as sequence numbers
  rownames(refDf) <- seq(1, dim(refDf)[1], 1)
  
  # output, changed from csv to rda and update existing files if available
  save(refDf, file = paste('data/cov/ref_cov', as.character(startYear), '-', 
                           as.character(endYear), suffix, '.rda', sep = ''))
  
  # return
  return(refDf)
}


#================
# implementation
#================

# # implement get_cov individually
# cov1979to1980 <- get_cov(1979, 1980, addStats = T)
# # implementation on several networks in loop
# for(y in 1965:1987){
#   cat(as.character(y), 'to', as.character(y+1), '\n')
#   tmp <- get_cov(y, y+1, addStats = T)
# }

# implementation on 1979to1980 network only
t1979to1980 <- main(1979, 1980, addStats = T)

# biyearly networks
for(y in 1965:1987){
  refDf <- main(y, y+1)
  print(dim(refDf))
}
print('Finished. Data saved.')

