#' Script to inspect covariate distribution of references 
#' ranked by betweenness
#' 
#' Version history:
#'   Renamed from inspect_cov_v5.R to C3_v1.R to mark steps clearly
#'   C3_v2: 
#'     Change background and grid colors of figures
#'     Renamed 'rank' as 'tier'
#'   C3_v3:
#'     Add degree into inspection of covariate distribution
#'     Add option for raw vs. normalized centrality, default normalized
#'   C3_v4:
#'     Get degree of refs at specified percentiles of betweenness only
#'     Revise get_distribution() to allow specified quantiles
#'     Review plotting func accordingly for specified quantiles for efficiency
#'     Remove MMWR from its previously assigned field
#' 
#' Xingyun Wu
#' 2/1/2023

library(dplyr)
library(stringr)
library(ggplot2)
library(directlabels)

options(scipen=999)
setwd('C:/Users/xwu70/Dropbox/HIVAIDS_Networks')
setwd('~/Dropbox/HIVAIDS_Networks/')
# setwd('~/Documents/ra/citation_sasha/')


###########
# Function
###########

#' auxiliary function  to pre-process data for a summary table 
#'    for Function get_thres (simplified in v4)
get_distribution <- function(data, thres, targetVar, normalized, probs){
  # select columns: target grouping variable + centrality
  centrality <- ifelse(normalized, paste('betweenness', 'norm', sep = '_'),
                       paste('betweenness', 'raw', sep = '_'))
  dc <- ifelse(normalized, paste('degree', 'norm', sep = '_'),
               paste('degree', 'raw', sep = '_'))
  data <- data[, c(targetVar, centrality, dc)] # add degree column in v5
  colnames(data) <- c('group', centrality, dc)
  
  # get raw summary
  rv <- as.data.frame(data[1:thres,] %>%
                        group_by(group) %>%
                        summarise(n = n()))

  # add mean, std, and specified percentiles
  rv[, c('mean', 'sd', paste('p', probs, sep = ''), paste('dc', probs, sep = ''))] <- NA
  for(i in 1:dim(rv)[1]){
    # slice df by group
    tdf <- data[data$group == rv[i, 'group'],]
    # get mean and standard deviation
    rv[i, 'mean'] <- round(mean(tdf[, centrality]), 5)
    rv[i, 'sd'] <- round(sd(tdf[, centrality]), 5)
    # get betweenness at specified percentile
    rv[i, paste('p', probs, sep = '')] <- round(quantile(tdf[, centrality], probs = probs), 5)
    # get degree of ref(s) at specified percentile
    tdf <- tdf[order(tdf$betweenness_norm, decreasing = F),]
    for(j in 1:length(probs)){
      if((dim(tdf)[1]) == 1){
        trow <- 1
      }
      else{
        if(probs[j] == 0.5){
          trow <- c(floor(dim(tdf)[1] * probs[j]), ceiling(dim(tdf)[1] * probs[j]))
          rv[i, paste('dc', probs[j], sep = '')] <- round(mean(tdf[trow, dc]), 5)
        }
        else{
          trow <- round(dim(tdf)[1] * probs[j], 0)
          rv[i, paste('dc', probs[j], sep = '')] <- round(tdf[trow, dc], 5)
        }
      }
    }
  }

  # if targetVar is rank, enforce 3 categories rather than whatever from data,
  # in order to include topAIDS journals
  if(targetVar == 'rank'){
    if(dim(rv)[1] == 2){
      rv[3,] <- c('topAIDS', 0, rep(NA, dim(rv)[2] - 2))
    }
  }

  # output
  return(rv)
}


#' auxiliary function to get the appropriate threshold 
#' to have smallest group's n >= 30
#'     for Function inspect_top
get_thres <- function(data, thres, targetVar, normalized, probs, smallestN){
  print('Looking for an appropriate threshold...')
  
  # initial summary
  print('  Initiate...')
  rv <- get_distribution(data, thres = thres, targetVar = targetVar,
                         normalized = normalized, probs = probs)
  
  # adjust the threshold so that the smallest field's n >= 30, update summary table accordingly
  print('  Update thresthold...')
  while(min(rv$n) < smallestN){
    thres <- thres + 1
    # continue condition
    if(thres <= dim(data)[1]){
      # update summary
      rv <- get_distribution(data, thres, targetVar = targetVar, 
                             normalized = normalized, probs = probs)
    }
    # break condition
    else{
      print('Largest n reached for threshold. Use all references.')
      break
    }
  }
  print(paste('Threshold found. Looking at top', as.character(thres)))
  
  # return
  return(rv)
}


## major function to standardize the summary process
inspect_top <- function(data, targetVar = 'rank', normalized, probs, 
                        thres, smallestN = 30, outputTable, filename = NULL){
  # create a flag whether a threshold is specified
  findThres <- ifelse(isFALSE(thres), TRUE, FALSE)
  
  ## initiate data and threshold
  # if by rank
  if(targetVar == 'rank'){
    init_thres <- 90
  }
  # if by field
  else{
    init_thres <- 1000
  }

  ## if we need the program to find a threshold to have smallest n >= 30
  # if threshold not given, use the initial threshold
  if(findThres){
    rv <- get_thres(data, thres = init_thres, targetVar = targetVar, 
                    normalized = normalized, probs = probs, smallestN = smallestN)
  }
  # if a threshold is given
  else{
    rv <- get_distribution(data, thres, targetVar = targetVar, 
                           normalized = normalized, probs = probs)
  }
  
  # ensure rv is a data frame
  rv <- as.data.frame(rv)

  # output as csv when required or not specified
  if(outputTable){
    write.csv(rv, file = paste('~/Documents/ra/citation_sasha/data_abc/cov/field_top', 
                               as.character(thres), '_', filename, 
                               '.csv', sep = ''), row.names = F)
  }

  # return
  return(rv)
}


#' auxiliary function to summarize across networks
#'     for Function 'main'
organize_summary <- function(dataLst, targetVar = targetVar, normalized, probs, 
                             thres = FALSE, outputTable = FALSE){
  
  # get summary data
  smryLst <- list() # by network by group
  tbAll <- as.data.frame(matrix(NA, nrow = length(dataLst), ncol = 1 + length(probs) * 2)) # by network
  colnames(tbAll) <- c('network', paste('p', probs, sep = ''), paste('dc', probs, sep = ''))
  tbAll$network <- names(dataLst)
  for(i in 1:length(dataLst)){
    print(names(dataLst)[i])
    tempTop <- inspect_top(dataLst[[i]], targetVar = targetVar, normalized, probs, 
                           thres = thres, outputTable = outputTable, filename = names(dataLst)[i])
    if(i == 1){
      thres <- sum(tempTop$n)
    }
    smryLst[[i]] <- tempTop
    tbAll[i, paste('p', probs, sep = '')] <- quantile(dataLst[[i]][1:thres, 'betweenness_norm'], 
                                                      probs = probs)
    tbAll[i, paste('dc', probs, sep = '')] <- quantile(dataLst[[i]][1:thres, 'degree_norm'],
                                                       probs = probs)
  }
  names(smryLst) <- names(dataLst)
  
  # list of all unique ranks/fields: to organize multiple summary tables into one
  if(targetVar == 'rank'){
    gpLst <- c('top09', 'top49', 'topAIDS')
  }
  else{
    gpLst <- c()
    for(i in 1:length(smryLst)){
      gpLst <- c(gpLst, as.character(smryLst[[i]]$group))
    }
    gpLst <- unique(gpLst)
  }
  
  # get unified summary table
  print('Combine info into one summary table...')
  pctVec <- c('mean', 'sd', paste('p', probs, sep = ''), paste('dc', probs, sep = ''))
  tb <- as.data.frame(matrix(NA, nrow = length(smryLst) * length(gpLst), 
                             ncol = (4 + length(pctVec))))
  colnames(tb) <- c('network', 'group', 'abbr', 'n', pctVec)
  tb$network <- rep(names(smryLst), each = length(gpLst))
  tb$group <- rep(gpLst, length(smryLst))
  
  # get abbreviation for each group for plot
  if(targetVar == 'rank'){
    tb$abbr <- ifelse(tb$group == 'top09', 'T1',
                      ifelse(tb$group == 'top49', 'T2', 'T3'))
  }
  else{
    tb$abbr <- gsub('[^[:upper:]]', '', tb$group)
  }
  
  # organize data
  print('Filling info...')
  tempNet <- '' # network name
  j <- 0 # initiate network index
  for(i in 1:dim(tb)[1]){
    if(tempNet != tb[i, 'network']){
      tempNet <- tb[i, 'network']
      j <- j + 1
    }
    if(tb[i, 'group'] %in% smryLst[[j]]$group){
      tempRow <- which(smryLst[[j]]$group == tb[i, 'group'])
      tb[i, 'n'] <- smryLst[[j]][tempRow, 'n']
      tb[i, pctVec] <- smryLst[[j]][tempRow, pctVec]
    }
  }
  print('Ready to plot.')
  
  # output
  rv <- list()
  rv$thres <- thres
  rv$smryLst <- smryLst
  rv$tb <- tb
  rv$all <- tbAll
  return(rv)
}


#' auxiliary function to summarize across networks and plot
#'     for Function 'main'
get_plot <- function(tb, startYear, endYear, thres, pct, targetVar, 
                     normalized, trendAll){
  # covariate's name for plot
  covName <- ifelse(targetVar == 'rank', 'Tier', str_to_sentence(targetVar))
  # select specified column of betweenness
  tcol <- ifelse(is.character(pct), pct, paste('p', pct, sep = ''))
  print(tcol)
  tcos <- ifelse(is.character(pct), 'sd', paste('dc', pct, sep = ''))
  # standardize column names after selection
  tb <- tb[, c('network', 'group', 'abbr', 'n', tcol, tcos)]
  colnames(tb) <- c('network', 'group', 'abbr', 'n', 'pct', 'dc')
  # # slice the table for all (not by group) as well
  trendAll <- trendAll[, c('network', tcol, tcos)]
  colnames(trendAll) <- c('network', 'pct', 'dc')

  # attach abbreviation to group for labeling
  tb$group <- paste(tb$group, ' (', tb$abbr, ')', sep = '')
  
  # plot 1: plot betweenness
  p1 <- ggplot(data = tb, aes(x = network, y = pct, group = group)) +
    geom_line(aes(color = group)) +
    geom_point(aes(color = group)) +
    scale_x_discrete(expand = expansion(add = 3)) +
    geom_dl(aes(label = abbr, color = group),
            method = list(dl.trans(x = x - .2), 'first.points')) +
    geom_dl(aes(label = abbr, color = group),
            method = list(dl.trans(x = x + .2), 'last.points')) +
    ggtitle(paste('Trend at', pct * 100, 'th Percentile of Betweenness by', covName, 'of Journals'),
            subtitle = paste(startYear, 'to', endYear,
                             ': Top', as.character(thres))) +
    xlab('Network') +
    ylab(paste(ifelse(normalized, 'Normalized', 'Raw'), 'betweenness')) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.position="bottom",
          panel.background = element_rect(fill = 'white', colour = 'gray'),
          panel.grid = element_line(colour="lightgray"))
  p1 <- p1 +
    geom_line(data = trendAll, colour = 'black', linetype = 2,
              aes( x = network, y = pct, group = 1))
  # print(p1)
  
  # plot2: degree of ref(s) at specified percentile of betweenness
  p2 <- ggplot(data = tb, aes(x = network, y = dc, group = group)) +
    geom_line(aes(color = group)) +
    geom_point(aes(color = group)) +
    scale_x_discrete(expand = expansion(add = 3)) +
    geom_dl(aes(label = abbr, color = group),
            method = list(dl.trans(x = x - .2), 'first.points')) +
    geom_dl(aes(label = abbr, color = group),
            method = list(dl.trans(x = x + .2), 'last.points')) +
    ggtitle(paste('Degree at', pct * 100, 'th Percentile of Betweenness by', 
                  covName, 'of Journals'),
            subtitle = paste(startYear, 'to', endYear,
                             ': Top', as.character(thres))) +
    xlab('Network') +
    ylab(paste(ifelse(normalized, 'Normalized', 'Raw'), 'Degree')) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.position="bottom",
          panel.background = element_rect(fill = 'white', colour = 'gray'),
          panel.grid = element_line(colour="lightgray"))
  # print(p2)
  p2 <- p2 +
    geom_line(data = trendAll, colour = 'black', linetype = 2,
              aes( x = network, y = dc, group = 1))
  
  # return
  rv <- list()
  rv$bc <- p1
  rv$dc <- p2
  return(rv)
}


## wrapper function to fun inspect_top for multiple networks
main <- function(dataLst, thres = FALSE, normalized = T, 
                 probs = c(0.9, 0.95, 0.97, 0.98, 0.99), 
                 outputTable = FALSE, byRank = TRUE, byField = TRUE){
  # initiate
  rv <- list()
  initThres = thres
  
  # get start year and end year for plots' subtitle
  startYear <- str_extract(names(dataLst)[1], '[:digit:]{4}')
  endYear <- paste('19', 
                   str_trim(str_extract_all(names(dataLst)[length(dataLst)],
                                                  '[:digit:]{2}')[[1]][3]), 
                   sep = '')
  
  ## summary by rank
  if(byRank){
    print('Summary by rank of journals:')
    rankSmry <- organize_summary(dataLst, targetVar = 'rank', normalized = normalized, 
                                 probs = probs, thres = thres, outputTable = outputTable)
    # attach to return list
    thres <- rankSmry$thres
    rv$byRank <- rankSmry$smryLst
    rv$summaryRank <- rankSmry$tb
    rv$summaryAll <- rankSmry$all
    
    # plot
    plt1 <- list()
    plt2 <- list()
    for(k in 1:length(probs)){ # plotting for percentiles, not mean with std
      pTmp <- get_plot(rankSmry$tb, startYear = startYear, endYear = endYear,
                       thres = thres, pct = probs[k], targetVar = 'rank',
                       normalized = normalized, trendAll = rankSmry$all)
      plt1[[k]] <- pTmp$bc
      plt2[[k]] <- pTmp$dc
    }
    names(plt1) <- paste('Pct', as.character(probs * 100), sep = '')
    names(plt2) <- paste('Pct', as.character(probs * 100), sep = '')
    # attach to return list
    rv$bcRank <- plt1
    rv$dcRank <- plt2
  }
  
  ## summary by field
  if(byField){
    print('Summary by fields of journals:')
    thres <- initThres
    # remove MMWR from EPHP field (added on 2/7/2023)
    for(i in 1:length(dataLst)){
      dataLst[[i]] <- dataLst[[i]][!is.na(dataLst[[i]]$field),]
    }
    # get summary data
    fieldSmry <- organize_summary(dataLst, targetVar = 'field', normalized = normalized, 
                                  probs = probs, thres = thres, outputTable = outputTable)
    # attach to return list
    thres <- fieldSmry$thres
    rv$byField <- fieldSmry$smryLst
    rv$summaryField <- fieldSmry$tb
    
    # plot
    print('Plotting...')
    plt3 <- list() # plot all fields (betweenness)
    plt4 <- list() # plot all fields (degree)
    plt5 <- list() # plot selected fields only (betweenness)
    plt6 <- list() # plot selected fields only (degree)
    for(k in 1:length(probs)){
      pTmp <- get_plot(fieldSmry$tb, startYear = startYear, endYear = endYear, 
                       thres = thres, pct = probs[k], targetVar = 'field', 
                       normalized = normalized, trendAll = fieldSmry$all)
      plt3[[k]] <- pTmp$bc
      plt4[[k]] <- pTmp$dc
      pTmp <- get_plot(fieldSmry$tb[fieldSmry$tb$abbr %in% c('BBS', 'EPHP', 'GM', 'GS', 'G', 'I', 'V', 'VMZ',
                                                                  'N', 'S', 'PNAS'),], 
                       startYear = startYear, endYear = endYear, thres = thres, 
                       pct = probs[k], targetVar = 'field', normalized = normalized,
                       trendAll = fieldSmry$all)
      plt5[[k]] <- pTmp$bc
      plt6[[k]] <- pTmp$dc
    }
    names(plt3) <- paste('Pct', as.character(probs * 100), sep = '')
    names(plt4) <- paste('Pct', as.character(probs * 100), sep = '')
    names(plt5) <- paste('Pct', as.character(probs * 100), sep = '')
    names(plt6) <- paste('Pct', as.character(probs * 100), sep = '')
    # attach to return list
    rv$bcField <- plt3
    rv$dcField <- plt4
    rv$bcMainField <- plt5
    rv$dcMainField <- plt6
  }
  
  # output whole return object
  if(outputTable){
    save(rv, file = paste('~/Documents/ra/citation_sasha/data_abc/summary/summary_top', 
                          as.character(startYear), 'to', as.character(endYear),
                          '.rda', sep = ''))
  }
  
  # return
  return(rv)
}


#################
# Implementation
#################

## load biyearly networks between 1979 and 1986
# read data
all_cov <- list()
cnt <- 0
for(y in 1965:1987){
  cnt <- cnt + 1
  load(paste('data/cov/cov', as.character(y), 'to', 
             as.character(y+1), '.rda', sep = ''))
  # reorder
  refDf <- refDf[order(refDf$betweenness_raw, decreasing = T),]
  all_cov[[cnt]] <- refDf
  names(all_cov)[cnt] <- paste(as.character(y), '-', 
                               substr(as.character(y+1), 3, 4), sep = '')
}
rm(refDf)


## get summary by field
t1965to1988 <- main(all_cov, outputTable = F, byRank = T, byField = T, 
                    probs = c(0.5, 0.60, 0.70, 0.75, 0.80, 0.85, 
                              0.90, 0.95, 0.97, 0.98, 0.99))
t1965to1988$dcRank$Pct98
t1965to1988$bcMainField$Pct50
t1965to1988$bcMainField$Pct60
t1965to1988$bcMainField$Pct70
t1965to1988$bcMainField$Pct75
t1965to1988$bcMainField$Pct80
t1965to1988$bcMainField$Pct85
t1965to1988$dcMainField$Pct98


# output figures to png files
for(i in 1:length(t1965to1988$bcRank)){
  print(i)
  # by rank
  print(t1965to1988$bcRank[[i]])
  ggsave(filename = paste('results/shiny/betweenness_norm/rank1965to1988_',
                          names(t1965to1988$bcRank)[i], '_bc.png', sep = ''),
         width = 12, height = 10)
  print(t1965to1988$dcRank[[i]])
  ggsave(filename = paste('results/shiny/betweenness_norm/rank1965to1988_',
                          names(t1965to1988$dcRank)[i], '_dc.png', sep = ''),
         width = 12, height = 10)
  # by field: all fields
  print(t1965to1988$bcField[[i]])
  ggsave(filename = paste('results/shiny/betweenness_norm/field1965to1988_',
                          names(t1965to1988$bcField)[i], '_bc.png', sep = ''),
         width = 12, height = 10)
  print(t1965to1988$dcField[[i]])
  ggsave(filename = paste('results/shiny/betweenness_norm/field1965to1988_',
                          names(t1965to1988$dcField)[i], '_dc.png', sep = ''),
         width = 12, height = 10)
  # by field: selected fields
  print(t1965to1988$bcMainField[[i]])
  ggsave(filename = paste('results/shiny/betweenness_norm/field1965to1988_main_',
                          names(t1965to1988$bcMainField)[i], '_bc.png', sep = ''),
         width = 12, height = 10)
  print(t1965to1988$dcMainField[[i]])
  ggsave(filename = paste('results/shiny/betweenness_norm/field1965to1988_main_',
                          names(t1965to1988$dcMainField)[i], '_dc.png', sep = ''),
         width = 12, height = 10)
}

