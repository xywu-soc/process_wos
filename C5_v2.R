#'Script to inspect by-journal distribution of betweenness within a specified rank
#'
#'Version history:
#'  C5_v1: Moved from summarize_cov_v4.R, line 706 to 1081
#'  C5_v2: 
#'    Changed background and grid colors of figures
#'    Renamed 'rank' as 'tier'
#'
#'Xingyun Wu
#'1/1/2023

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


######################
# Auxiliary functions
######################

## auxiliary function to plot raw counts and proportions by journal of specified rank
plot_journal_cnt <- function(dt, idVar, tRank = 3, outputFigure = F){
  
  # raw count and proportion from all refs from journals in specified rank
  # over all references
  jnlCnt <- as.data.frame(matrix(NA, nrow = length(dt), ncol = 4))
  colnames(jnlCnt) <- c('network', 'n', 'n_t', 'prop_t')
  jnlCnt$network <- names(dt)
  for(i in 1:length(dt)){
    jnlCnt[i, 'n'] <- dim(dt[[i]])[1]
    jnlCnt[i, 'n_t'] <- dim(dt[[i]][dt[[i]]$rank_id == tRank,])[1]
    jnlCnt[i, 'prop_t'] <- jnlCnt[i, 'n_t'] / jnlCnt[i, 'n']
  }
  
  ## distribution of counts and proportions
  # plot: raw count
  plt1 <- ggplot(data = jnlCnt, aes(x = network, y = n_t, group = 1)) +
    geom_point() +
    geom_line() +
    ggtitle(paste('Raw Count of References from T', as.character(tRank), 
                  ' Journals Across Time', sep = ''),
            subtitle = '1965 to 1988') +
    xlab('Network') +
    ylab('Count') +
    theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 12),
          panel.background = element_rect(fill = 'white', colour = 'gray'),
          panel.grid = element_line(colour="lightgray"))
  # plot: proportion
  plt2 <- ggplot(data = jnlCnt, aes(x = network, y = prop_t, group = 1)) +
    geom_point() +
    geom_line() +
    ggtitle(paste('Proportion of References from T', as.character(tRank),
                  ' Journals Across Time', sep = ''),
            subtitle = '1965 to 1988') +
    xlab('Network') +
    ylab('Proportion') +
    theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 12),
          panel.background = element_rect(fill = 'white', colour = 'gray'),
          panel.grid = element_line(colour="lightgray"))
  plt3 <- ggarrange(plt1, plt2)
  if(outputFigure){
    ggsave(filename = paste('results/shiny/t', as.character(tRank),
                            '_refs.png', sep = ''), width = 15, height = 8)
  }
  
  ## proportion table 
  jnlVec <- idVar[idVar$rank_id == tRank, 'journal']
  jnlInd <- as.data.frame(matrix(NA, nrow = 1, ncol = 5))
  colnames(jnlInd) <- c('network', 'journal', 'n', 'prop_t', 'prop_all')
  cnt <- 0
  for(i in 1:length(dt)){
    tmpT <- dim(dt[[i]][dt[[i]]$rank_id == tRank,])[1]
    tmpAll <- dim(dt[[i]])[1]
    for(j in 1:length(jnlVec)){
      cnt <- cnt + 1
      jnlInd[cnt, 'network'] <- names(dt)[i]
      jnlInd[cnt, 'journal'] <- jnlVec[j]
      jnlInd[cnt, 'n'] <- dim(dt[[i]][dt[[i]]$journal == jnlVec[j],])[1]
      jnlInd[cnt, 'prop_t'] <- jnlInd[cnt, 'n'] / tmpT
      jnlInd[cnt, 'prop_all'] <- jnlInd[cnt, 'n'] / tmpAll
    }
  }
  jnlInd[, c(4, 5)] <- round(jnlInd[, c(4, 5)], 3)
  
  # add abbreviation
  if(tRank == 1){
    jnlInd$abbr <- c('AJPH', 'BMJ', 'JAMA', 'L', 'MMWR', 'N', 'NEJM', 'PNAS', 'S')
  }
  else{
    jnlInd$abbr <- gsub('[^[:upper:]]', '', str_to_title(jnlInd$journal))
  }
  # put journal names and abbreviations together
  jnlInd$journal <- paste(jnlInd$journal, ' (', jnlInd$abbr, ')', sep = '')
  
  # plot: raw counts by journal
  plt4 <- ggplot(data = jnlInd, aes(x = network, y = n, group = journal)) +
    geom_point(aes(colour = journal)) +
    geom_line(aes(colour = journal)) +
    scale_x_discrete(expand = expansion(add = 2)) +
    geom_dl(aes(label = abbr, colour = journal),
            method = list(dl.trans(x = x - .2), 'last.points')) +
    ggtitle(paste('Raw Count of References from T', as.character(tRank), 
                  ' Journals Across Time', sep = ''),
            subtitle = '1965 to 1988') +
    xlab('Network') +
    ylab('Count') +
    theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.position="bottom",
          panel.background = element_rect(fill = 'white', colour = 'gray'),
          panel.grid = element_line(colour="lightgray"))
  if(outputFigure){
    ggsave(filename = paste('results/shiny/t', as.character(tRank), 
                            '_journal_count.png', sep = ''), 
           width = 15, height = 10)
  }
  
  # plot: proportion to T3 refs
  plt5 <- ggplot(data = jnlInd, aes(x = network, y = prop_t, group = journal)) +
    geom_point(aes(colour = journal)) +
    geom_line(aes(colour = journal)) +
    scale_x_discrete(expand = expansion(add = 2)) +
    geom_dl(aes(label = abbr, colour = journal),
            method = list(dl.trans(x = x + .2), 'last.points')) +
    geom_dl(aes(label = abbr, colour = journal),
            method = list(dl.trans(x = x - .2), 'first.points')) +
    ggtitle(paste('Proportion of References from Each T', as.character(tRank),
                  ' Journal to All T', as.character(tRank), 
                  ' Journals Across Time', sep = ''),
            subtitle = '1965 to 1988') +
    xlab('Network') +
    ylab(paste('Proportion to references from T', as.character(tRank), sep = '')) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.position="bottom",
          panel.background = element_rect(fill = 'white', colour = 'gray'),
          panel.grid = element_line(colour="lightgray"))
  if(outputFigure){
    ggsave(filename = paste('results/shiny/t', as.character(tRank), 
                            '_journal_prop_t', as.character(tRank),
                            '.png', sep = ''), 
           width = 15, height = 10)
  }
  
  # plot: proportion to all refs
  plt6 <- ggplot(data = jnlInd, aes(x = network, y = prop_all, group = journal)) +
    geom_point(aes(colour = journal)) +
    geom_line(aes(colour = journal)) +
    scale_x_discrete(expand = expansion(add = 2)) +
    geom_dl(aes(label = abbr, colour = journal),
            method = list(dl.trans(x = x + .2), 'last.points')) +
    geom_dl(aes(label = abbr, colour = journal),
            method = list(dl.trans(x = x - .2), 'first.points')) +
    ggtitle(paste('Proportion of References from Each T', as.character(tRank), 
                  ' Journal to All T', as.character(tRank), 
                  ' Journals Across Time', sep = ''),
            subtitle = '1965 to 1988') +
    xlab('Network') +
    ylab('Proportion to all references') +
    theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.position="bottom",
          panel.background = element_rect(fill = 'white', colour = 'gray'),
          panel.grid = element_line(colour="lightgray"))
  if(outputFigure){
    ggsave(filename = paste('results/shiny/t', as.character(tRank), 
                            '_journal_prop_all.png', sep = ''), 
           width = 15, height = 10)
  }
  
  # output
  rv <- list()
  rv$count_total <- plt1
  rv$prop_total <- plt2
  rv$count <- plt4
  rv$prop_t <- plt5
  rv$prop_all <- plt6
  return(rv)
}


## auxiliary function to plot betweenness at specified percentile
## taking advantage of get_trivariate function created in C4
plot_journal_bc <- function(dt, idVar, pct, tRank = 3){
  
  # journal vector based on choice of rank
  jnlVec <- idVar[idVar$rank_id == tRank, 'journal']
  
  # utilize get_trivariate() to get betweenness at the specified percentile
  tb <- get_trivariate(dt, 'journal', pct = pct, tRank = tRank,
                       idVar = idVar[idVar$journal %in% jnlVec,])
  tb <- tb[, c('network', 'journal_id', 'group', 'bc_group', 'bc_all')]
  tb <- tb[!duplicated(tb),]
  
  # add abbreviation
  if(tRank == 1){
    tb$abbr <- c('AJPH', 'BMJ', 'JAMA', 'L', 'MMWR', 'N', 'NEJM', 'PNAS', 'S')
  }
  else{
    tb$abbr <- gsub('[^[:upper:]]', '', str_to_title(tb$group))
  }
  # put the journal names and the abbreviations together
  tb$group <- paste(tb$group, ' (', tb$abbr, ')', sep = '')
  
  # plot
  plt <- ggplot(data = tb, aes(x = network, y = bc_group, group = group)) +
    geom_point(aes(colour = group)) +
    geom_line(aes(colour = group)) +
    geom_line(aes(x = network, y = bc_all, group = 1), linetype = 2) +
    scale_x_discrete(expand = expansion(add = 2)) +
    geom_dl(aes(label = abbr, colour = group),
            method = list(dl.trans(x = x + .2), 'last.points')) +
    ggtitle(paste('Trend at ', pct, 'th Percentile of Betweenness by T',
                  as.character(tRank), ' Journals', sep = ''),
            subtitle = '1965 to 1988\nDashed line represents betweenness at the same percentile of all T3 references') +
    xlab('Network') +
    ylab('Betweenness') +
    theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.position="bottom",
          panel.background = element_rect(fill = 'white', colour = 'gray'),
          panel.grid = element_line(colour="lightgray"))
  print(plt)
  
  # output
  rv <- list()
  rv$table <- tb
  rv$plot <- plt
  return(rv)
}


#######
# Data
#######

## read data
all_cov <- list()
cnt <- 0
for(y in 1965:1987){
  cnt <- cnt + 1
  load(file = paste('data/cov/cov', as.character(y), 'to', 
                    as.character(y+1), '.rda', sep = ''))
  all_cov[[cnt]] <- refDf[, c(1:13)]
  names(all_cov)[cnt] <- paste(as.character(y), '-', 
                               substr(as.character(y+1), 3, 4), sep = '')
}
# remove redundant objects
rm(refDf, cnt, y)

## read summary tables for rank, field, and journal from C2
jnlDf <- read.csv('results/shiny/journal_id.csv')


##########
# Inspect
##########

#=======
# count
#=======

# T1 journals
t1 <- plot_journal_cnt(all_cov, idVar = jnlDf, tRank = 1, outputFigure = T)

# T3 journals
t3 <- plot_journal_cnt(all_cov, idVar = jnlDf, tRank = 3, outputFigure = T)


#=============
# betweenness
#=============

## T1 journals
# 97th percentile
jplot97 <- plot_journal_bc(all_cov, idVar = jnlDf, pct = 97, tRank = 1)
ggsave(filename = 'results/shiny/t1_journal_bc97.png', width = 15, height = 10)
# 97.5th percentile
jplot97.5 <- plot_journal_bc(all_cov, idVar = jnlDf, pct = 97.5, tRank = 1)
ggsave(filename = 'results/shiny/t1_journal_bc97.5.png', width = 15, height = 10)
# 98th percentile
jplot98 <- plot_journal_bc(all_cov, idVar = jnlDf, pct = 98, tRank = 1)
ggsave(filename = 'results/shiny/t1_journal_bc98.png', width = 15, height = 10)
# 98.5th percentile
jplot98.5 <- plot_journal_bc(all_cov, idVar = jnlDf, pct = 98.5, tRank = 1)
ggsave(filename = 'results/shiny/t1_journal_bc98.5.png', width = 15, height = 10)

# put figures together to use in paper
t1p <- ggarrange(plotlist = list(t1$count_total, t1$prop_total,
                                 t1$prop_t, jplot98$plot), 
                 nrow = 2, ncol = 2, common.legend = TRUE, legend="bottom")
t1p + bgcolor('white') + border('white')
ggsave(filename = 'results/shiny/t1_smry_bc98.jpeg', width = 20, height = 15)


## T3 journals
# 97th percentile
jplot97 <- plot_journal_bc(all_cov, idVar = jnlDf, pct = 97, tRank = 3)
ggsave(filename = 'results/shiny/t3_journal_bc97.png', width = 15, height = 10)
# 97.5th percentile
jplot97.5 <- plot_journal_bc(all_cov, idVar = jnlDf, pct = 97.5, tRank = 3)
ggsave(filename = 'results/shiny/t3_journal_bc97.5.png', width = 15, height = 10)
# 98th percentile
jplot98 <- plot_journal_bc(all_cov, idVar = jnlDf, pct = 98, tRank = 3)
ggsave(filename = 'results/shiny/t3_journal_bc98.png', width = 15, height = 10)
# 98.5th percentile
jplot98.5 <- plot_journal_bc(all_cov, idVar = jnlDf, pct = 98.5, tRank = 3)
ggsave(filename = 'results/shiny/t3_journal_bc98.5.png', width = 15, height = 10)

# put figures together to use in paper
t3p <- ggarrange(plotlist = list(t3$count_total, t3$prop_total,
                                 t3$prop_t, jplot98$plot), 
                 nrow = 2, ncol = 2, common.legend = TRUE, legend="bottom")
t3p + bgcolor("white") + border('white')
ggsave(filename = 'results/shiny/t3_smry_bc98.jpeg', width = 20, height = 15)


