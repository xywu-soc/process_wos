#'Script to process basic univariate, bi-variate, and tri-variate distribution
#'
#'Version history:
#'  C4_v1: Moved from summarize_cov_v4.R
#'    Univariate: line 165 to 187
#'    Bivariate: line 190 to 243
#'    Trivariate: line 246 to 633
#'  C4_v2: 
#'    Changed background and grid colors of figures
#'    Renamed 'rank' as 'tier'
#'
#'Xingyun Wu
#'1/1/2023

library(xlsx)
library(stringr)
library(ggplot2)
library(ggpubr)
library(ggtext)
library(directlabels)

options(scipen=999)

# set working directory: HPC PC vs. Xingyun's own mac
setwd('C:/Users/xwu70/Dropbox/HIVAIDS_Networks')
setwd('~/Dropbox/HIVAIDS_Networks/')

# source get_trivariate()
source('scripts/get_trivariate.R')


#######
# Data
#######

## biyearly networks between 1979 and 1986
# read data
all_cov <- list()
cnt <- 0
for(y in 1965:1987){
  cnt <- cnt + 1
  load(file = paste('data/cov/cov', as.character(y), 'to', 
                    as.character(y+1), '.rda', sep = ''))
  all_cov[[cnt]] <- refDf[, c(1:15)]
  names(all_cov)[cnt] <- paste(as.character(y), '-', 
                               substr(as.character(y+1), 3, 4), sep = '')
}

## processed summary table for rank, field, and journal in C2
rankDf <- read.csv('data/cov/rank_id.csv')
fieldDf <- read.csv('data/cov/field_id_v2.csv') # updated to use v2
jnlDf <- read.csv('results/shiny/journal_id.csv')


#############
# Univariate
#############
# betweenness only

## summary across years: betweenness
bcSum <- as.data.frame(matrix(NA, nrow = length(all_cov), ncol = 17))
colnames(bcSum) <- c('network', 'n', 'min', '1stQu', 'median',
                     'mean', '3rdQu', 'max', '90thPct', '95thPct',
                     '96thPct', '97thPct', '98thPct', '98.5thPct',
                     '99thPct', '99.5thPct', '99.9thPct')
for(i in 1:length(all_cov)){
  print(i)
  bcSum[i, 'network'] <- names(all_cov)[i]
  bcSum[i, 'n'] <- dim(all_cov[[i]])[1]
  bcSum[i, c(3:8)] <- as.vector(summary(all_cov[[i]]$betweenness_norm))
  bcSum[i, c(9:17)] <- quantile(all_cov[[i]]$betweenness_norm, 
                                probs = c(0.9, 0.95, 0.96, 0.97, 0.98, 
                                          0.985, 0.990, 0.995, 0.999))
}
bcSum <- bcSum[, c(1:7, 9:17, 8)]
bcSum[, c(3:17)] <- round(bcSum[, c(3:17)], 5)
write.csv(bcSum, file = '~/Dropbox/HIVAIDS_Networks/data/summary/distribution1965to1988_bc_v2.csv',
          row.names = F) # updated to save as v2


############
# Bivariate
############

## summary across years: reference year
# 97th percentile
rySum97 <- as.data.frame(matrix(NA, nrow = length(all_cov), ncol = 6))
colnames(rySum97) <- c('network(pyear)', 'n', 'ryear1950to1974',
                       'ryear1975to1981', 'ryear1982to1988', 'bc_whole_97th')
for(i in 1:dim(rySum97)[1]){
  print(i)
  rySum97[i, 'network(pyear)'] <- names(all_cov)[i]
  rySum97[i, 'n'] <- dim(all_cov[[i]])[1]
  rySum97[i, 'ryear1950to1974'] <- quantile(all_cov[[i]][all_cov[[i]]$year < 1975,
                                                         'betweenness_norm'], 
                                            probs = 0.97)
  rySum97[i, 'ryear1975to1981'] <- quantile(all_cov[[i]][all_cov[[i]]$year >= 1975
                                                         & all_cov[[i]]$year < 1982,
                                                         'betweenness_norm'], 
                                            probs = 0.97)
  rySum97[i, 'ryear1982to1988'] <- quantile(all_cov[[i]][all_cov[[i]]$year >= 1982,
                                                         'betweenness_norm'], 
                                            probs = 0.97)
  rySum97[i, 'bc_whole_97th'] <- quantile(all_cov[[i]]$betweenness_norm, probs = 0.97)
}
rySum97[, c(3:6)] <- round(rySum97[, c(3:6)], 0)
# 98th percentile
rySum98 <- as.data.frame(matrix(NA, nrow = length(all_cov), ncol = 6))
colnames(rySum98) <- c('network(pyear)', 'n', 'ryear1950to1974',
                       'ryear1975to1981', 'ryear1982to1988')
for(i in 1:dim(rySum98)[1]){
  print(i)
  rySum98[i, 'network(pyear)'] <- names(all_cov)[i]
  rySum98[i, 'n'] <- dim(all_cov[[i]])[1]
  rySum98[i, 'ryear1950to1974'] <- quantile(all_cov[[i]][all_cov[[i]]$year < 1975,
                                                         'betweenness'], 
                                            probs = 0.98)
  rySum98[i, 'ryear1975to1981'] <- quantile(all_cov[[i]][all_cov[[i]]$year >= 1975
                                                         & all_cov[[i]]$year < 1982,
                                                         'betweenness'], 
                                            probs = 0.98)
  rySum98[i, 'ryear1982to1988'] <- quantile(all_cov[[i]][all_cov[[i]]$year >= 1982,
                                                         'betweenness'], 
                                            probs = 0.98)
  rySum98[i, 'bc_whole_98th'] <- quantile(all_cov[[i]]$betweenness, probs = 0.98)
}
rySum98[, c(3:6)] <- round(rySum98[, c(3:6)], 0)
# output
wb <- createWorkbook(type = 'xlsx')
tsheet <- createSheet(wb, sheetName = '97th_percentile')
addDataFrame(x = rySum97, sheet = tsheet, row.names = F)
tsheet <- createSheet(wb, sheetName = '98th_percentile')
addDataFrame(x = rySum98, sheet = tsheet, row.names = F)
saveWorkbook(wb, file = '~/Dropbox/HIVAIDS_Networks/results/table/trivariate_years.xlsx')


#############
# Trivariate
#############
# betweenness by network, rank/field, ryear

#=====================
# auxiliary functions
#=====================

## auxiliary function to plot trivariate trends of betweenness
plot_trivariate <- function(tb, main_field = FALSE){
  # get variable name: rank or field
  targetVar <- gsub('_id', '', colnames(tb)[2])
  typeVar <- gsub('[[:digit:]].*', '', colnames(tb)[8])
  tsubTitle <- ifelse(typeVar == 'bc_ryear', 'reference year (ryear)',
                      'tier of journal')
  pct <- str_extract(colnames(tb)[8], '[[:digit:]].*')
  colnames(tb)[8] <- str_replace(colnames(tb)[8], '[[:digit:]].*', '')
  
  # plot
  # by rank-ryear
  if(targetVar == 'rank'){
    targetVar <- 'tier' # added in v2
    # add abbreviation
    tb$abbr <- ifelse(tb$group == 'top09', 'T1',
                      ifelse(tb$group == 'top49', 'T2', 'T3'))
    # expansion of plotting area on the right-hand side
    rhs_expansion <- 0.1
  }
  # by field-ryear or by field-rank
  else{
    # keep main fields only if specified
    if(main_field){
      tb <- tb[tb$field_id %in% c(1:5, 7, 13),]
    }
    # add abbreviation
    tb$abbr <- gsub('[^[:upper:]]', '', tb$group)
    # expansion of plotting area on the right-hand side
    rhs_expansion <- 0.2
  }
  # attach abbreviation to group names
  tb$group <- paste(tb$group, ' (', tb$abbr, ')', sep = '')
  
  # plot 1: tri-variate figure
  plt1 <- ggplot(data = tb, aes(x = network, y = bc_tri, group = group)) +
    geom_line(aes(colour = group)) +
    geom_point(aes(colour = group)) +
    scale_x_discrete(expand = expansion(mult = c(0.05, rhs_expansion))) +
    geom_dl(aes(label = abbr, colour = group),
            method = list(dl.trans(x = x + .2), 'last.points')) +
    ggtitle(paste('Trend at', paste(pct, 'th', sep = ''), 
                  'Percentile of Betweenness by', 
                  str_to_sentence(targetVar), 'of Journals'),
            subtitle = paste('1965 to 1988, by ', tsubTitle, '. Dashed line ',
                             'represents betweenness at the same percentile ', 
                             'by network-ryear.', sep = '')) +
    scale_fill_discrete(breaks=c('top09 (T1)', 'top49 (T2)', 
                                 'topAIDS (T3)')) +
    xlab('Network') +
    ylab('Betweenness Centrality') +
    theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.2, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.position="bottom",
          panel.background = element_rect(fill = 'white', colour = 'gray'),
          panel.grid = element_line(colour="lightgray"))
  # by type var
  if(typeVar == 'bc_ryear'){
    plt1 <- plt1 +
      # add network bc at the same percentile
      geom_line(linetype = 2, #colour = 'azure4',
                aes(x = network, y = bc_ryear, group = group)) +
      # add facet setting
      facet_wrap(. ~ factor(ryear, levels = c('before1975', '1975to1981', 
                                              '1982to1988')))
  }
  else if(typeVar == 'bc_rank'){
    plt1 <- plt1 +
      # add network bc at the same percentile
      geom_line(linetype = 2, #colour = 'azure4',
                aes(x = network, y = bc_rank, group = group)) +
      # add facet setting
      facet_wrap(. ~ factor(rank, levels = c('T1', 'T2', 'T3')))
  }
  
  # plot 2: bc at the same percentile of the whole network
  plt2 <- ggplot(data = tb, aes(x = network, y = bc_all, group = 1))+
    geom_point()+
    geom_line(linetype = 3) +
    ggtitle(paste('Trend at', paste(pct, 'th', sep = ''), 
                  'Percentile of Betweenness \nof the Whole Networks'),
            subtitle = '1965 to 1988') +
    xlab('Network') +
    ylab('Betweenness Centrality') +
    theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.2, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 12),
          panel.background = element_rect(fill = 'white', colour = 'gray'),
          panel.grid = element_line(colour="lightgray"))
  
  # combine plt1 and plt2
  plt3 <- ggarrange(plt1, plt2, ncol = 2, widths = c(7, 3))
  print(plt3)
  
  # return
  rv <- list()
  rv$table <- tb
  rv$trivar <- plt1
  rv$whole <- plt2
  rv$combined <- plt3
  return(rv)
}


#====================
# bc by rank + ryear
#====================

# initiate
rLst <- list()
cnt <- 0
wb <- createWorkbook(type = 'xlsx')

# process data and plot
for(p in c(97, 97.5, 98, 98.5)){
  cat('\n\n', p)
  cnt <- cnt + 1
  # process data
  tSmry <- get_trivariate(all_cov, targetVar = 'rank', idVar = rankDf, pct = p)
  rLst[[cnt]] <- tSmry
  # add processed data into xlsx file
  tsheet <- createSheet(wb, sheetName = paste(as.character(p),
                                              'th_percentile', sep = ''))
  addDataFrame(x = tSmry, sheet = tsheet, row.names = F)
  # plot all fields
  tPlot <- plot_trivariate(tSmry)
  ggsave(filename = paste('results/shiny/rank', as.character(p),
                          'th_ryear.png', sep = ''),
         width = 22, height = 8)
}

# mark percentiles of corresponding tables in list
names(rLst) <- c(97, 97.5, 98, 98.5)
# save to xlsx
saveWorkbook(wb, file = 'results/table/trivariate_rank_ryear.xlsx')


#=====================
# bc by field + ryear
#=====================

# initiate
fLst <- list()
cnt <- 0
wb <- createWorkbook(type = 'xlsx')

# process data and plot
for(p in c(97, 97.5, 98, 98.5)){
  cat('\n\n', p)
  cnt <- cnt + 1
  # process data
  tSmry <- get_trivariate(all_cov, targetVar = 'field', 
                          idVar = fieldDf, pct = p)
  fLst[[cnt]] <- tSmry
  # add processed data into xlsx file
  tsheet <- createSheet(wb, sheetName = paste(as.character(p),
                                              'th_percentile', sep = ''))
  addDataFrame(x = tSmry, sheet = tsheet, row.names = F)
  # plot all fields
  tPlot <- plot_trivariate(tSmry, main_field = F)
  ggsave(filename = paste('results/shiny/field', as.character(p),
                          'th_ryear_all.png', sep = ''),
         width = 22, height = 8)
  # plot main fields
  tPlot <- plot_trivariate(tSmry, main_field = T)
  ggsave(filename = paste('results/shiny/field', as.character(p),
                          'th_ryear_main.png', sep = ''),
         width = 22, height = 8)
}

# mark percentiles of corresponding tables in list
names(fLst) <- c(97, 97.5, 98, 98.5)
# save to xlsx
saveWorkbook(wb, file = 'results/table/trivariate_field_ryear.xlsx')


#====================
# bc by rank + field
#====================

# initiate
bLst <- list()
cnt <- 0
wb <- createWorkbook(type = 'xlsx')

# process data and plot
for(p in c(97, 97.5, 98, 98.5)){
  cat('\n\n', p)
  cnt <- cnt + 1
  # process data
  tSmry <- get_trivariate(all_cov, targetVar = 'both', 
                          idVar = fieldDf, pct = p)
  bLst <- tSmry
  # add processed data into xlsx
  tsheet <- createSheet(wb, sheetName = paste(as.character(p),
                                              'th_percentile', sep = ''))
  addDataFrame(x = tSmry, sheet = tsheet, row.names = F)
  # plot all fields
  tPlot <- plot_trivariate(tSmry, main_field = F)
  ggsave(filename = paste('results/shiny/field', as.character(p),
                          'th_rank_all.png', sep = ''),
         width = 22, height = 8)
  # plot main fields
  tPlot <- plot_trivariate(tSmry, main_field = T)
  ggsave(filename = paste('results/shiny/field', as.character(p),
                          'th_rank_main.png', sep = ''),
         width = 22, height = 8)
}

# mark percentiles of corresponding tables in list
names(fLst) <- c(97, 97.5, 98, 98.5)
# save to xlsx
saveWorkbook(wb, file = 'results/table/trivariate_field_rank.xlsx')

