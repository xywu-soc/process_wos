#' Get bipartite network and its projection into one-mode
#' 
#' Based on A2_v6.R
#' Change working directory from local to Dropbox
#' 
#' Xingyun Wu
#' 7/25/2022

library(dplyr)

# set working directory
# setwd('/Users/xywu/Documents/ra/citation_sasha/')
setwd('~/Dropbox/HIVAIDS_Networks/')
setwd('C:/Users/xwu70/Dropbox/HIVAIDS_Networks')


#########################
## Get bipartite network
#########################

get_bpt <- function(year_start, year_end, jnls = NULL){
  
  # read and combine data from multiple years
  print('Reading data...')
  # initialize
  i <- 0 # year id
  cnt <- 0 # cumulative number publications from combined years
  # summary table
  smry <- as.data.frame(matrix(NA, nrow = length(year_start:year_end), ncol = 7))
  colnames(smry) <- c('year', 'm1', 'n1', 'm2', 'n2', 'm3', 'n3')
  smry$year <- seq(year_start, year_end, 1)
  
  # concatenate pub-ref from selected years
  for(year in year_start:year_end){
    i <- i + 1
    print(year)
    load(paste('data/refs1/refs', '.rda', sep = as.character(year)))
    smry[i, 'm1'] <- length(unique(refs$mid))
    smry[i, 'n1'] <- dim(refs)[1]
    # assign a new mid for combined years
    cmbd_id <- as.data.frame(as.integer(unique(refs$mid)))
    colnames(cmbd_id) <- 'mid'
    cmbd_id$cid <- seq((cnt + 1), 
                       (cnt + length(unique(refs$mid))), 1)
    refs$mid <- as.integer(refs$mid)
    refs <- left_join(x = refs, y = cmbd_id, by = c('mid'))
    refs$pub_year <- year
    # keep track of the number of publications
    cnt <- cnt + length(unique(refs$mid))
    # combine rows
    if(i == 1){
      df <- refs
    }
    else{
      df <- rbind(df, refs)
    }
  }
  print('Finished reading data')
  
  # ensure unique publication-references
  df <- df[!duplicated(df),]
  # concatenate 'year', 'journal', and 'page' into 'final' column
  df$final <- paste(df$year, df$journal, df$page, sep = ',')
  
  # get m2 and n2 of each selected year, unique publications and references
  for(year in year_start:year_end){
    smry[smry$year == year, 'm2'] <- length(unique(df[df$pub_year == year, 'mid']))
    smry[smry$year == year, 'n2'] <- length(unique(df[df$pub_year == year, 'final']))
  }
  
  # if journal list is given, select valid references from those journals only
  if(!is.null(jnls)){
    print(paste('Journals specified. Keep references from top',
                as.character(length(jnls)), '.', sep = ''))
    df <- df[df$journal %in% jnls,]
  }
  else{
    print('Journals not specified, default all')
  }
  
  # get m3 and n3 of each selected year after selection of journals
  for(year in year_start:year_end){
    smry[smry$year == year, 'm3'] <- length(unique(df[df$pub_year == year, 'mid']))
    smry[smry$year == year, 'n3'] <- length(unique(df[df$pub_year == year, 'final']))
  }
  
  print('Getting pub-ref bipartite network...')
  
  # get unique references throughout
  uniqRefs <- unique(df$final)
  # add a sequence id for all references
  uniqRefs <- as.data.frame(uniqRefs)
  colnames(uniqRefs) <- 'final'
  uniqRefs$final <- as.character(uniqRefs$final)
  uniqRefs$rid <- seq(1, dim(uniqRefs)[1], 1)
  # merge the reference id into df
  df <- left_join(x = df, y = uniqRefs, by = c('final'))
  
  # construct bipartite network
  pubs_in_bpt <- unique(df$cid)
  bpt <- as.data.frame(matrix(0, nrow = length(pubs_in_bpt), ncol = dim(uniqRefs)[1]))
  colnames(bpt) <- uniqRefs$rid
  rownames(bpt) <- pubs_in_bpt
  
  # iterate through all publications one by one
  for(i in 1:dim(df)[1]){
    row_id <- which(pubs_in_bpt == df[i, 'cid'])
    col_id <- which(uniqRefs$rid == df[i, 'rid'])
    bpt[row_id, col_id] <- 1
  }
  
  # save the bpt and the indices of publications 
  #(for potential publication covariates)
  bpt <- as.matrix(bpt)
  pubRef <- df[, c('pub_year', 'mid', 'cid', 'rid')]
  # output
  save(bpt, pubRef, pubs_in_bpt, uniqRefs, smry, 
       file = paste('data/network/bpt', as.character(year_start), 
                    'to', as.character(year_end), '_', #isaids,
                    as.character(length(jnls)), '.rda', sep = ''))
  write.csv(smry, file = paste('data/summary/A2_', as.character(year_start), 'to',
                               as.character(year_end), '_', #isaids,
                               as.character(length(jnls)),
                               '.csv', sep = ''), row.names = F)
  
  # output: bpt instead of smry in A2_v6.R
  return(bpt)
}


#======
# Test
#======

# Note: Data saving is included within the function. Change directory there.

# define journal list for reference restriction
# general lists
load('data/journal_data/updatedRank.rda')
# the complete journal vector for 75 selected journals
topAll <- c(top9, top49, topAIDS)
rm(top9, top49, topAIDS)

# run for a single year
bpt1987 <- get_bpt(1987, 1987, jnls = topAll)

# run for a pair of years: restrict to selected 76
bpt1983to1984 <- get_bpt(1983, 1984, topAll)
dim(bpt1983to1984) # 4158 21886

# run to get all biyearly networks between 1965 and 1988
for(y in 1965:1987){
  print(y)
  tbpt <- get_bpt(y, y+1, topAll)
  print(dim(tbpt))
}
