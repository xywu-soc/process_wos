#' R script to clean the 9 + (55 - 6) + 17 = 75 journals' variants
#' 
#' Based on A1_v6.R
#' Changed working directory from local to Dropbox
#' Include the newly cleaned T3 journal variants into cleaning process
#' Use the updated journal ranks to replace the previous top9-top64minus9 system
#' 
#' Xingyun Wu
#' 7/25/2022

library(stringr)

# setwd('~/Documents/ra/citation_sasha')
setwd('~/Dropbox/HIVAIDS_Networks/')
setwd('C:/Users/xwu70/Dropbox/HIVAIDS_Networks')

# load processed journal variants
load('data/journal_data/updatedRank.rda')
load('data/journal_data/journal_variants_202207.rda')
# subset the selected journals only
fnl <- fnl[intersect(names(fnl), c(top9, top49, topAIDS))]


########
# Clean
########

#' Function 1: clean journal variants of a specified year
#' Auxiliary function to clean journal variants by year
clean_variants <- function(vrts, year){
  # load data from A0.R
  load(paste('data/refs0/refs', '.rda', sep = as.character(year)))
  
  # clean variants
  for(i in 1:dim(refs)[1]){
    for(j in 1:length(vrts)){
      if(refs[i, 'journal'] %in% vrts[[j]]){
        refs[i, 'journal'] <- names(vrts[j])
      }
    }
  }
  
  # output
  return(refs)
}


#' Function 2: clean within-mid duplicates
#' Auxiliary function to clean journal variants by year
clean_duplicates <- function(df, year){
  # concatenate the multiple fields
  df$final <- paste(df$mid, df$year, df$journal, 
                      df$page, sep = ',')
  
  # save the bag for within-mid-duplicated references
  tdup <- df[duplicated(df$final), 'final']
  bag2 <- df[df$final %in% tdup,]
  write.csv(bag2, file = paste('data/refs1/duplicated', '.csv', 
                               sep = as.character(year)), row.names = F)
  
  # save the bag for non-within-mid-duplicated references
  refs <- df[!(df$final %in% tdup),]
  save(refs, file = paste('data/refs1/refs', '.rda', 
                          sep = as.character(year)))
  
  # output
  rv <- list()
  rv$refs <- refs
  rv$bag2 <- bag2
  return(rv)
}


#' Function 3:
#' Run for journal variants cleaning and get a summary table 
#' for n1_top9 and n1_top_others from n1
main <- function(vrts, yrs = c(1965:1990)){
  i <- 0
  
  # read the summary table from A0
  smry <- as.data.frame(matrix(NA, nrow = length(yrs), ncol = 11))
  colnames(smry) <- c('year', 'm1', 'n1', 'n1_T1', 'n1_T2', 'n1_T3',
                      'm2', 'n2', 'n2_T1', 'n2_T2', 'n2_T3')
  smry$year <- yrs
    
  for(y in yrs){
    i <- i + 1
    print(y)
    
    # update journal variants
    refs <- clean_variants(vrts, y) # call function 1
    # dimension prior to the removal of duplicates
    smry[i, 'm1'] <- length(unique(refs$mid))
    smry[i, 'n1'] <- dim(refs)[1]
    # count
    refs$T1 <- ifelse(refs$journal %in% top9, 1, 0)
    refs$T2 <- ifelse(refs$journal %in% top49, 1, 0)
    refs$T3 <- ifelse(refs$journal %in% topAIDS, 1, 0)
    # update summary table
    smry[i, 'n1_T1'] <- length(which(refs$T1 == 1))
    smry[i, 'n1_T2'] <- length(which(refs$T2 == 1))
    smry[i, 'n1_T3'] <- length(which(refs$T3 == 1))
    # remove newly added columns
    refs[, c('T1', 'T2', 'T3')] <- NULL
    
    # clean within-mid duplicates
    trv <- clean_duplicates(refs, y) # call function 2
    refs <- trv$refs
    bag2 <- trv$bag2
    # print the counts of within-mid duplicates
    cat('Number of within-mid duplicated references =', dim(bag2)[1], 
        '\nNumber of publications having within-mid duplicated references =',
        length(unique(bag2$mid)), '\n')
    # dimension of remaining refs
    smry[i, 'm2'] <- length(unique(refs$mid))
    smry[i, 'n2'] <- dim(refs)[1]
    
    # count n2 in top 9 and top 64
    refs$T1 <- ifelse(refs$journal %in% top9, 1, 0)
    refs$T2 <- ifelse(refs$journal %in% top49, 1, 0)
    refs$T3 <- ifelse(refs$journal %in% topAIDS, 1, 0)
    # update summary table
    smry[i, 'n2_T1'] <- length(which(refs$T1 == 1))
    smry[i, 'n2_T2'] <- length(which(refs$T2 == 1))
    smry[i, 'n2_T3'] <- length(which(refs$T3 == 1))
  }
  
  # output
  write.csv(smry, file = paste('data/summary/A1', '_', as.character(yrs[1]), 
                               'to', as.character(yrs[length(yrs)]), '.csv', sep = ''),
            row.names = F)
  
  return(smry)
}


######
# Run
######

# all years
a1_all <- main(fnl)

# multiple years
a1_1980to1985 <- main(fnl, c(1980:1985))
load('data_abc/refs1/refs1980.rda')

# single year
a1_1980 <- main(fnl, c(1980))

