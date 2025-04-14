#'Script to get summary tables for component distribution and summary table processing
#'for subsequent steps
#'
#'Updates based on v2:
#'1.Add degree into covariate files
#'2.Add option for raw vs. normalized degree/betweenness
#'3.Remove MMWR from its previously assigned field
#'
#'Xingyun Wu
#'2/1/2023 => 2/7/2023

library(dplyr)
library(ggplot2)

options(scipen=999)

setwd('~/Dropbox/HIVAIDS_Networks/')
setwd('C:/Users/xwu70/Dropbox/HIVAIDS_Networks')


#######
# Data
#######

# read data
all_cov <- list()
cnt <- 0
for(y in 1965:1987){
  cnt <- cnt + 1
  load(file = paste('data/cov/cov', as.character(y), 'to', 
                    as.character(y+1), '.rda', sep = ''))
  all_cov[[cnt]] <- refDf
  names(all_cov)[cnt] <- paste(as.character(y), '-', 
                               substr(as.character(y+1), 3, 4), sep = '')
}

# remove redundant objects
rm(refDf, cnt, y)


#########################
# Component distribution
#########################

#====================================
# summary for component distribution
#====================================

# load network stats
all_nw <- list()
for(i in 1:length(all_cov)){
  print(i )
  y <- 1964 + i
  all_nw[[i]] <- read.csv(paste('data/network/stats', as.character(y), 'to',
                                as.character(y + 1), 'whole.csv', sep = ''))
}
names(all_nw) <- names(all_cov)

# get component distribution across years
compSum <- as.data.frame(matrix(NA, nrow = length(all_nw), ncol = 9))
colnames(compSum) <- c('network', 'n', 'numComp', 'nLgst', 'pLgst', 
                       'nBc0All', 'nBc0Lgst', 'pBc0All', 'pBc0Lgst')
for(i in 1:dim(compSum)[1]){
  compSum[i, 'network'] <- names(all_nw)[i]
  compSum[i, 'n'] <- dim(all_nw[[i]])[1]
  compSum[i, 'numComp'] <- max(all_nw[[i]]$component)
  compSum[i, 'nLgst'] <- length(which(all_nw[[i]]$component == 1))
  compSum[i, 'pLgst'] <- compSum[i, 'nLgst'] / compSum[i, 'n']
  compSum[i, 'nBc0All'] <- length(which(all_nw[[i]]$betweenness == 0))
  compSum[i, 'nBc0Lgst'] <- dim(all_nw[[i]][all_nw[[i]]$betweenness == 0 & all_nw[[i]]$component == 1,])[1]
  compSum[i, 'pBc0All'] <- compSum[i, 'nBc0All'] / compSum[i, 'n']
  compSum[i, 'pBc0Lgst'] <- compSum[i, 'nBc0Lgst'] / compSum[i, 'nLgst']
}

# # output table
# write.csv(compSum, row.names = F,
#           file = 'results/table/component_distribution.csv')
rm(all_nw)


#======
# plot
#======

compSum <- read.csv('results/table/component_distribution.csv')

## number of components
p2 <- ggplot(data = compSum, aes(x = network, y = numComp, group = 1)) +
  geom_line() +
  geom_point() +
  # scale_x_discrete(expand = expansion(add = 3)) +
  ggtitle('Component Distribution Across Time',
          subtitle = '1965 to 1988') +
  xlab('Network') +
  ylab('Number of Components') +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12))
p2
ggsave(filename = 'results/shiny/comp1965to1988.png',
       width = 10, height = 10)


## proportion of n in largest components to the whole network
p3 <- ggplot(data = compSum, aes(x = network, y = pLgst, group = 1)) +
  geom_line() +
  geom_point() +
  # scale_x_discrete(expand = expansion(add = 3)) +
  ggtitle('Component Distribution Across Time',
          subtitle = '1965 to 1988') +
  xlab('Network') +
  ylab('Proportion of Nodes in Largest Component') +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12))
p3
ggsave(filename = 'results/shiny/pLgstComp1965to1988.png',
       width = 10, height = 10)
# proportion of bc = 0


####################################
# Summary for descriptive analysis
###################################

## get a data frame of field and assign field id to replace texts
rankDf <- all_cov[[1]] %>%
  group_by(rank_id, rank) %>%
  summarise()
rankDf <- as.data.frame(rankDf)
write.csv(rankDf, file = 'data/cov/rank_id.csv', row.names = F)


## get a data frame of field and assign field id to replace texts
fieldVec <- c()
for(i in 1:length(all_cov)){
  fieldVec <- c(fieldVec, as.character(all_cov[[i]]$field))
}
fieldVec <- sort(unique(fieldVec))
fieldDf <- as.data.frame(fieldVec)
colnames(fieldDf) <- 'field'
fieldDf$field <- as.character(fieldDf$field)
fieldDf$field_id <- seq(1, dim(fieldDf)[1], 1)
# further process fieldDf for quantile regression in Stata
# collapse fields as discussed
fieldDf$collapsed <- ifelse(fieldDf$field_id == 10, 7,
                            ifelse(fieldDf$field_id %in% c(11, 13), 8, 
                                   fieldDf$field_id))
fieldDf$new_id <- ifelse(fieldDf$collapsed == 12, 10, 
                         ifelse(fieldDf$collapsed == 14, 11, 
                         ifelse(fieldDf$collapsed == 15, 12,
                         ifelse(fieldDf$collapsed == 16, 13, fieldDf$collapsed))))
fieldDf$new_name <- fieldDf$field
fieldDf[c(7, 10), 'new_name'] <- 'Internal Medicine_Oncology'
fieldDf[c(8, 11, 13), 'new_name'] <- 'Pathology and Diagnosis_Radiology_MCTTM'
fieldDf$new_abbr <- gsub('[^[:upper:]]', '', fieldDf$new_name)
fieldDf$new_abbr[c(7, 10)] <- c('IM_O', 'IM_O')
fieldDf$new_abbr[c(8, 11, 13)] <- c('PD_R_MCTTM', 'PD_R_MCTTM', 'PD_R_MCTTM')
fieldDf$new_abbr[c(9, 14)] <- fieldDf$field[c(9, 14)]
# clean redundant column(s) and column names
fieldDf$collapsed <- NULL 
# output into v2 on 2/1/2023
write.csv(fieldDf, file = 'data/cov/field_id_v2.csv', row.names = F)

## get a data frame of journal names and assign id to replace texts in modeling
allData <- bind_rows(all_cov, .id = "network")
jnlDf <- allData %>%
  group_by(journal, rank_id, rank, field) %>%
  summarise(num_cited = n())
jnlDf <- as.data.frame(jnlDf)
# attach journal id
jnlDf$journal_id <- seq(1, dim(jnlDf)[1], 1)
# remove redundant data from memory
rm(allData)
# output
write.csv(jnlDf, file = 'results/shiny/journal_id.csv', row.names = F)

# modified on 2/7/2023: take out MMWR from EPHP
jnlDf <- read.csv('results/shiny/journal_id.csv')
jnlDf[jnlDf$journal == 'MMWR-MORBID MORTAL W', 'field'] <- NA
write.csv(jnlDf, file = 'results/shiny/journal_id_v2.csv', row.names = F)


################################
# Summary for quantile modeling
################################

# load fieldDf and jnlDf
fieldDf <- read.csv('data/cov/field_id_v2.csv')
jnlDf <- read.csv('results/shiny/journal_id_v2.csv')

# iterate through
for(i in 1:length(all_cov)){
  print(names(all_cov)[i])
  # remove double check for mutually exclusive rank categories in accordance with simplification in C1
  tdf <- all_cov[[i]]
  colnames(tdf)[2] <- 'ryear'
  # # replace MMWR's field as NA
  # tdf[tdf$journal == 'MMWR-MORBID MORTAL W', 'field'] <- NA
  # binary variable for whether largest component
  tfreq <- tdf %>% group_by(component) %>% summarise(n = n())
  tfreq <- as.data.frame(tfreq)
  tmax <- tfreq[which(tfreq$n == max(tfreq$n)), 'component']
  tdf$lgst_comp <- ifelse(tdf$component == tmax, 1, 0)
  # merge field id after collapsing smaller fields
  tdf <- left_join(x = tdf, y = fieldDf[, c('field', 'field_id', 'new_id')], 
                   by = 'field')
  # merge journal id
  tdf <- left_join(x = tdf, y = jnlDf[, c('journal', 'journal_id')], 
                   by = 'journal')
  # output to csv
  write.csv(tdf, file = paste('data/cov/bc', names(all_cov)[i], '.csv', sep = ''),
            row.names = FALSE)
}
rm(tdf)

