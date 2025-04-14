#' Clean Raw Data from WOS
#' Put the within-mid duplicated references in a bag and remove
#'   
#' Xingyun Wu
#' 8/31/2021 ==> 9/4/2021

library(stringr)

################
## Initialize ##
################

## Set working directory
setwd('/Users/xywu/Documents/ra/citation_sasha/')
# set initiate=T if it's the first time running from the raw data
initiate=F
if(initiate==T){
  # load data
  load('/Users/xywu/Dropbox/HIVAIDS_Networks/globalenvironment-02-05-18.RData')
  # keep only the raw dataframe in the working space
  lst <- ls()
  lst <- lst[lst!='AidsFrame']
  rm(list=lst)
  save(AidsFrame, file='AidsFrame.rda')
}

## Load data
load('AidsFrame.rda')


## check if duplicated rows (publications) in AidsFrame (raw data)
dim(AidsFrame) #73826    61
dim(AidsFrame[!duplicated(AidsFrame),]) # 73826    61
# no duplicated rows (publications)


###############
## Functions ##
###############

#' Function 1
#' Auxiliary function to extract reference information from raw data
#' and split the reference column of each publication into a vector
#' of references in string format
separate_ref <- function(data){
  print('Get references from raw WOS data...')
  # construct a list to hold the references for each observation
  ref_col <- list()
  
  # iterate through every publication
  for(i in 1:nrow(data)){
    temp_ref <- c()
    temp_lst <- trimws(strsplit(as.character(data[i, 'CR']), ';')[[1]])
    ref_col[[i]] <- temp_lst
  }
  
  # return
  return(ref_col)
}


#' Function 2:
#' Auxiliary function to remove redundant comma or space from
#' journal titles
clean_object <- function(raw_object){
  temp_object <- trimws(raw_object)
  # if the whole title is any of the following, it's not a valid reference anyhow
  if(temp_object %in% c('', 'WORK', 'DATA', 'RESULTS', 'REPORT', 'MANUAL')){
    final_object <- -1
  }
  else{
    temp_object <- gsub(',', '', temp_object)
    temp_object <- trimws(str_replace_all(temp_object, '\\.[:space:]*', ' '))
    temp_object <- str_replace_all(temp_object, '[:space:]+', ' ')
    final_object <- temp_object
  }
  return(final_object)
}


#' Function 3:
#' Auxiliary function to process journal info of one reference:
#' journal title and page
parse_jnl <- function(info){
  
  # flag to proceed
  flag <- T
  
  while(flag){
    # deal with 'in press', 'to be published', 'unpublished', and 'cited indirectly'
    # check if the reference is unpublished/in press/cited indirectly
    isunpub <- str_match(info, 'UNPUB')
    istobepub <- str_match(info, 'TO BE PUBLISHED')
    isinpress <- str_match(info, 'IN PRESS')
    isindir <- str_match(info, 'CITED INDIRECTLY')
    if(!is.na(isunpub[1])){
      break
    }
    else if(!is.na(isindir[1])){
      break
    } 
    else if(!is.na(istobepub[1])){
      info = gsub(pattern = 'TO BE PUBLISHED', replacement = '', x = info)
    }
    else if(!is.na(isinpress[1])){
      info <- gsub(pattern = 'IN PRESS', replacement = '', x = info)
    }
    
    # parse information piece by piece
    info <- gsub('^\\,[:space:]*\\,*', '', info)
    info <- trimws(str_split(info, ',')[[1]])
    for(i in 1:length(info)){
      # check if page or volume info presents
      tempp <- str_match(info[i], '(.*)(P|PAGE|P.)([:space:]*)([:digit:]+)')
      # if page
      if(!is.na(tempp[1]) & tempp[2] == ''){
        temp_page <- tempp[5]
      }
      # if neither page nor volume, parse the first element as journal
      else{
        temp_journal <- clean_object(info[1]) # call Function 2
      }
    }
    
    # turn flag into F to end the trunk
    flag <- F
  }
  
  # mark -1 if journal title or page not found
  if(!exists('temp_journal')){
    temp_journal <- -1
  }
  if(!exists('temp_page')){
    temp_page <- -1
  }
  
  # output
  rv <- c(temp_journal, temp_page)
  return(rv)
}


#' Function 4:
#' Auxiliary function to parse information of a reference from a whole string
parse_ref <- function(ref){
  # if NA, return -1 immediately
  if(is.na(ref)){
    return(rep(-1, 3))
  }
  
  # parse text
  temp_parsed <- str_match(ref, '(.*)(,[:space:]*)(1[:digit:]{3})(,[:space:]*)(.*)')
  
  # if standard pattern
  if(!is.na(temp_parsed[1])){
    temp_year <- temp_parsed[4]
    temp_jnl <- parse_jnl(temp_parsed[6]) # call Function 3
  }
  
  # if pattern not standard
  else{
    
    # check if the whole string is only a 1-3 character or 1-3-digit number
    if(nchar(ref) <= 3){
      return(rep(-1, 3))
    }
    
    # check if starting from year
    temp_parsed <- str_match(ref, '(.*)(1[:digit:]{3})(,[:space:]*)(.*)')
    if((!is.na(temp_parsed[1])) & (temp_parsed[2] == '')){
      temp_year <- temp_parsed[3]
      temp_jnl <- parse_jnl(temp_parsed[5]) # call Function 3
    }
  }
  
  # if an element of output not exists, assign -1
  if(!exists('temp_year')){
    temp_year <- -1
  }
  if(!exists('temp_jnl')){
    temp_jnl <- c(-1, -1) # journal, page
  }
  
  # output: 3 entries in total because temp_jnl contains journal and page)
  final_ref <- c(temp_year, temp_jnl)
  return(final_ref)
}


#' Function 5:
#' Auxiliary function to parse publication-references of a specified year
get_ref <- function(df, year){
  rcnt <- 0
  pcnt <- 0 # newly added publication id
  all_df <- as.data.frame(matrix(NA, nrow = 1, ncol = 5))
  colnames(all_df) <- c('mid', 'original', 'year', 
                        'journal', 'page')
  
  temp_df <- df[df$PY == year & df$DT == 'ARTICLE' & !is.na(df$CR),]
  raw_refs <- separate_ref(temp_df) # call Function 1
  
  print('Parse references...')
  # iterate through all publications
  for(i in 1:length(raw_refs)){
    pcnt <- pcnt + 1
    # iterate through all references
    for(j in 1:length(raw_refs[[i]])){
      rcnt <- rcnt + 1
      temp_ref <- parse_ref(raw_refs[[i]][j]) # call Function 4
      all_df[rcnt,] <- c(pcnt, raw_refs[[i]][j], temp_ref[1], 
                         temp_ref[2], temp_ref[3])
    }
  }
  
  # get valid references
  refs <- all_df[all_df$year != -1 & all_df$journal != -1 &
                   all_df$page != -1,]
  
  # turn the parsed character year into numeric year
  refs$year <- as.integer(refs$year)

  # save parsed refs
  save(refs, file=paste('data_abc/refs0/refs', '.rda', sep=as.character(year)))
  
  # get dimensions before and after validness: m0, n0, m1, n1
  m0 <- length(unique(all_df$mid))
  n0 <- dim(all_df)[1]
  m1 <- length(unique(refs$mid))
  n1 <- dim(refs)[1]
  
  # output
  rv <- c(m0, n0, m1, n1)
  return(rv)
}


#' Function 6: main function of A0.R
#' The main (wrapper) function of get_ref and making the summary table
#' This is the only function we need to manually implement
main <- function(dt, yrs = c(1965:1990)){
  
  # create an empty summary table
  smry <- as.data.frame(matrix(NA, nrow = length(yrs), ncol = 5))
  colnames(smry) <- c('year', 'm0', 'n0', 'm1', 'n1')
  smry$year <- yrs
  
  # iterate through the specified years
  for(i in 1:length(yrs)){
    y <- yrs[i]
    print(y)
    
    # get references of year y
    temp <- get_ref(dt, y)
    
    # summary table
    smry[i, c(2:5)] <- temp
    cat('Year ', y, ': m = ', smry[i, 'm0'], ', n = ', smry[i, 'n0'],
        ', m1 = ', smry[i, 'm1'], ', n1 = ', smry[i, 'n1'], '\n')
  }
  
  # output
  write.csv(smry, file = paste('data_abc/summary/A0', '_', as.character(yrs[1]), 
                               'to', as.character(yrs[length(yrs)]), 
                               '.csv', sep = ''), row.names = F)
  return(smry)
}


#########
## Run ##
#########

# multiple years
a0_1978to1979 <- main(AidsFrame, c(1978:1979))
a0_1980to1985 <- main(AidsFrame, c(1980:1985))

# single year
t1980 <- main(AidsFrame, c(1980))

# all years: no need to specify years because that's the default
all_years <- main(AidsFrame)

