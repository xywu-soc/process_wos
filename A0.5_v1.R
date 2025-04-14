#' R script to clean journal name variants after step1_clean_data.R
#' 
#' Based on local_dir > code_v12 > step1_raw_clean_journal_v12_1.R
#' Add variants cleaning for T3 journals
#' Replace the 7/7/2021 version of journal_variants.rda copied from 'data_v12' folder
#' 
#' Xingyun Wu
#' 
#' 7/24 to 7/25, 2022

library(readxl)
library(stringr)

# setwd('~/Documents/ra/citation_sasha')
setwd('~/Dropbox/HIVAIDS_Networks/')


#######################################################
# All unique journals in data, with potential variants
#######################################################
#' based on previous code, moved to upfront 
#' modified directory to data_abc/refs0 
#' modified object name of df
#' modified column names to identify unique refs

## read all cleaned data
lst <- list()
i <- 0
for(y in 1965:1990){
  print(y)
  i <- i + 1 
  load(paste('data/refs0/refs', '.rda', sep = as.character(y)))
  lst[[i]] <- refs
}
rm(refs)

## get all unique journal names across years
jAll <- c()
for(i in 1:26){
  jTemp <- unique(lst[[i]]$journal)
  jAll <- c(jAll, jTemp)
}
jUniq <- unique(jAll)
length(jUniq) # 25,916 unique journal names
# remove redundant objects to release memory
rm(lst)
rm(jAll)
rm(jTemp)
rm(i)
rm(y)


##############
# T3 journals
##############

load('~/Dropbox/HIVAIDS_Networks/data/journal_data/updatedRank.rda')
rm(top9)
rm(top49)
print(topAIDS)

#' 1. "J AM VET MED ASSOC": JOURNAL OF THE AMERICAN VETERINARY MEDICAL ASSOCIATION
l1 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], 'J[:alpha:]*[:space:]+AM*[:space:]+V[:alpha:]*[:space:]+M[:alpha:]*[:space:]+A.*')
  if(!is.na(t0[1])){
    l1 <- c(l1, jUniq[i])
  }
}
l1
#'[1] "J AM VET MED ASSOC" "J AM VET MED ASS"   "J AM VET M A"  
#'keep all of them

#' 2. "VET IMMUNOL IMMUNOP": VETERINARY IMMUNOLOGY AND IMMUNOPATHOLOGY
l2 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], '^VE[:alpha:]*[:space:]+I[:alpha:]*[:space:]+I.*')
  if(!is.na(t0[1])){
    l2 <- c(l2, jUniq[i])
  }
}
l2
#'[1] "VET IMMUNOL IMMUNOP"  "VET IMMUNOL IMMUNOPA" "VET IMMUNOLO IMMUNOP"
#'keep all of them

#' 3. "VET REC": VETERINARY RECORD
l3 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], 'VE[:alpha:]*[:space:]+REC.*')
  if(!is.na(t0[1])){
    l3 <- c(l3, jUniq[i])
  }
}
l3
#'[1] "VETERINARY RECORD"  "VET REC"  "VET RECORD"   "VET RECORD S"                            
#'[5] "IN PRACTICE (SUPPL TO VETERINARY RECORD)"
#'keep 1-4
l3 <- l3[1:4]

#' 4. "AM J VET RES": AMERICAN JOURNAL OF VETERINARY RESEARCH
l4 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], 'A[:alpha:]*[:space:]+J[:alpha:]*[:space:]+V[:alpha:]*[:space:]+R.*')
  if(!is.na(t0[1])){
    l4 <- c(l4, jUniq[i])
  }
}
l4
#'[1] "AM J VET RES"          "JAPANESE JOUR VET RES" "AMER J VET RES"
#'[4] "AMER JOUR VET RES"     "JAPAN JOUR VET RES"    "S AFR J VET RES"
#'[7] "AM J VET RE"           "AM J VETERINARY RES"   "AM J VETER RES"
#'[10] "AM J VET RES CHICAGO"  "AM J VET RES OCT"      "CAN J VET RES"
#'keep selected, tentatively not including 10
l4 <- l4[c(1, 3, 4, 7:9, 11)]

#' 5. "ADV IMMUNOL": ADVANCES IN IMMUNOLOGY
l5 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], 'AD[:alpha:]*[:space:]+IM.*')
  if(!is.na(t0[1])){
    l5 <- c(l5, jUniq[i])
  }
}
l5
#'[1] "ADV IMMUNOL"           "ADVANCES IMMUNOLO ED"  "ADVANCE IMMUNOL"
#'[4] "ADVAN IMMUNOL"         "RAD IMMUNE MECHANISM"  "ADV IMMUNOLOGY"
#'[7] "CONCEPTUAL ADV IMMUN"  "LYMPHADENOPATHIE IMM"  "ADV IMMUNOPATHOLOGY"
#'[10] "ADV IMMUNOPHARMACOLO"  "ADV IMMUNOPHARMACOL"   "ADV IMMUNOLOGY BLOOD"
#'[13] "ADV IMMUNOHISTOCHEMI"  "PARADOXES IMMUNOLOGY"  "MED RADIONUCLIDE IMA"
#'[16] "ADV IMMUNOLOGY IMMUN"  "ADV IMMUN CANCER THER"
#'keep selected
l5 <- l5[c(1, 3, 4, 6)]

#' 6. "FASEB J": FASEB JOURNAL (Federation of American Societies for Experimental Biology)
l6 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], 'FA[:alpha:]*[:space:]+J.*')
  if(!is.na(t0[1])){
    l6 <- c(l6, jUniq[i])
  }
}
l6
#'[1] "FASEB J"       "FASEB JOURNAL"

#' 7. "ARCH VIROL": ARCHIVES OF VIROLOGY
l7 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], 'ARC[:alpha:]*[:space:]+VI.*')
  if(!is.na(t0[1])){
    l7 <- c(l7, jUniq[i])
  }
}
l7
#'[1] "ARCH VIRUSFORSCH" "ARCH VIROL"       "ARCH VIR"         "ARCH VIROLOGY" 
l7 <- l7[c(2:4)]

#' 8. "TISSUE ANTIGENS": TISSUE ANTIGENS
l8 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], 'TIS[:alpha:]*[:space:]+AN.*')
  if(!is.na(t0[1])){
    l8 <- c(l8, jUniq[i])
  }
}
l8
#'[1] "SEMINARS IN ARTHRITIS AND RHEUMATISM" "TISSUE ANTIGENS"
#'[3] "INTRO STATISTICAL AN"                 "STATISTICAL ANAL"
#'[5] "BLOOD TISSUE ANTIGEN"                 "ARTHRITIS AND RHEUMATISM"
#'[7] "BIOSTATISTICAL ANAL"                  "HERPETISCHE ANGENERK"
#'[9] "TISSUE ANTIGENS S"                    "STATISTICAL ANAL COM"
#'[11] "STATISTICAL ANAL DEC"                 "STATISTICAL ANAL FAI"
#'[13] "STATISTICAL ANAL PSY" 
l8 <- l8[c(2, 9)]

#' 9. "J NEUROSURG": JOURNAL OF NEUROSURGERY
l9 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], 'J[:alpha:]*[:space:]+O*F*[:space:]*NEUROS.*')
  if(!is.na(t0[1])){
    l9 <- c(l9, jUniq[i])
  }
}
l9
#'[1] "J NEUROSURG"     "J NEUROSCI RES"  "J NEUROSCI"      "J NEUROSCIENCE"
#'[5] "INT J NEUROSCI"  "J NEUROSCI METH" "J NEUROSCI NURS"
l9 <- l9[1]

#' 10. "J CLIN IMMUNOL": JOURNAL OF CLINICAL IMMUNOLOGY
l10 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], 'J[:alpha:]*[:space:]+C[:alpha:]*[:space:]+IM.*')
  if(!is.na(t0[1])){
    l10 <- c(l10, jUniq[i])
  }
}
l10
#'[1] "J CLIN IMMUNOL"       "J CLIN IMMUNOL S"     "J CLIN IMMUNOL IMMUN"
#'[4] "J CLIN IMMUNOLOGY"    "J CLIN IMMUNOLOGY S"  "J CLIN IMMUNOASSAY"
#'[7] "JPN J CLIN IMMUNOL"  
l10 <- l10[c(1, 2, 4, 5)]

#' 11. "NAT IMMUN CELL GROW": NATURAL IMMUNITY AND CELL GROWTH REGULATION
l11 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], 'N[:alpha:]*[:space:]+I[:alpha:]*[:space:]+C[:alpha:]*[:space:]+G.*')
  if(!is.na(t0[1])){
    l11 <- c(l11, jUniq[i])
  }
}
l11
#'[1] "NAT IMMUN CELL GROW"  "NAT IMMUNOL CELL GRO"


#' 12. "ANTIBIOT CHEMOTHER (1971)" ==> need to check whether the highly cited is a book or a journal but getting a typo to have the year included
# load("/Users/xywu/Documents/ra/citation_sasha/data_abc/refs0/refs1988.rda")
# View(refs[refs$journal == 'ANTIBIOT CHEMOTHER (1971)',])
# If journal: ANTIBIOTICS AND CHEMOTHERAPY
# If book: authored by Garrod, L.P.; O'grady, F.
l12 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], '^ANT[:alpha:]*[:space:]+CHE.*')
  if(!is.na(t0[1])){
    l12 <- c(l12, jUniq[i])
  }
}
l12
#'[1] "ANTIBIOT CHEMOTHER"        "ANTIBIOTIC CHEMOTHER"      "ANTIMICROB CHEMOTHER"
#'[4] "ANTIBIOT CHEMOTHER (1971)" "ANTIVIRAL CHEMOTHERA"      "ANTIFUNGAL CHEMOTHERAPY"
#'[7] "ANTIHERPESVIRUS CHEM"      "ANTIBIOTICS CHEMOTHE"      "ANTIBIOT CHEMOTHER A"
l12 <- l12[c(1, 2, 4, 8)]

#' 13. "ANTICANCER RES": ANTICANCER RESEARCH
l13 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], 'ANTIC[:alpha:]*[:space:]+R.*')
  if(!is.na(t0[1])){
    l13 <- c(l13, jUniq[i])
  }
}
l13
#'[1] "ANTICANCER RES"    "ATLANTIC REPORTER" 
l13 <- l13[1]

#' 14. "EUR J CANCER CLIN ON": EUROPEAN JOURNAL OF CANCER & CLINICAL ONCOLOGY
l14 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], 'E[:alpha:]*[:space:]+J[:alpha:]*[:space:]+O*F*[:space:]*C[:alpha:]*[:space:]+[:punct:]*C[:alpha:]*[:space:]+O.*')
  if(!is.na(t0[1])){
    l14 <- c(l14, jUniq[i])
  }
}
l14
#'[1] "EUR J CANCER CLIN ON"

#' 15. "IMMUNOBIOLOGY": IMMUNOBIOLOGY
l15 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], '^IMMUNOB.*')
  if(!is.na(t0[1])){
    l15 <- c(l15, jUniq[i])
  }
}
l15
#'[1] "IMMUNOBIOLOGY"  "IMMUNOBIOLOGY TRANSP"  "IMMUNOBIOLOGY MACROP"
#'[4] "IMMUNOBIOLOGY ONCOGE"  "IMMUNOBIOLOGY PROTEI"  "IMMUNOBIOLOGY IMMUNO"
#'[7] "IMMUNOBIOLOGIA"  "IMMUNOBIOLOGY NEISSE"  "IMMUNOBIOLOGICAL ASP"
#'[10] "IMMUNOBIOLOGY MAJOR"  "IMMUNOBIOLOGY BONE M"  "IMMUNOBIOLOGY EOSINO"
#'[13] "IMMUNOBIOLOGY OF THE EOSINOPHIL (PROC 1ST INTERNAT SYMP"
#'[14] "IMMUNOBIOL"  "IMMUNOBIOLOGY HERPES"  "IMMUNOBIOLOGY TROPHO"
#'[17] "IMMUNOBIOLOGY TRANSF"  "IMMUNOBIOLOGY NATURA"  "IMMUNOBIOLOGY HSV IN"
#'[20] "IMMUNOBIOLOGY COMPLE"  "IMMUNOBIOLOGY S"  "IMMUNOBIOLOGY PATHOG"
#'[23] "IMMUNOBIOLOGY S5"  "IMMUNOBIOLOGY HLA"  
l15 <- l15[c(1, 14, 21, 23)]
l15

#' 16. "IMMUNOBIOLOGY NATURA": not found in WOS
l16 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], '^IMMUNOB[:alpha:]*[:space:]+N.*')
  if(!is.na(t0[1])){
    l16 <- c(l16, jUniq[i])
  }
}
l16
#'[1] "IMMUNOBIOLOGY NEISSE" "IMMUNOBIOLOGY NATURA"
l16 <- l16[2]

#' 17. "IMMUNOCYTOCHEMISTRY"
l17 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], '^IMMUNOC[:alpha:]*$')
  if(!is.na(t0[1])){
    l17 <- c(l17, jUniq[i])
  }
}
l17
#'[1] "IMMUNOCHEMISTRY"     "IMMUNOCYTOCHEMISTRY" "IMMUNOCHEM"    
l17 <- l17[2]


## wrap the vectors into a list
t3l <- list(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, 
            l11, l12, l13, l14, l15, l16, l17)
names(t3l) <- topAIDS
names(t3l)[12] <- 'ANTIBIOT CHEMOTHER'

## update the object for journal variants
rm(t0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16, l17)
# load the original local > data_abc > top_journals.rda used in A1_v6.R
# renamed as journal_variants_202109.rda
load('data/journal_data/journal_variants_202109.rda')
fnl <- c(fnl, t3l)
# add variant for CANCER journal that was found in C1_v4.R
fnl$CANCER
fnl$CANCER[length(fnl$CANCER) + 1] <- 'CANCER-AM CANCER SOC'
# double check to make sure whether any further variants can be found
c0 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], 'CANCER-A[:alpha:]*[:space:]+C[:alpha:]*[:space:]+S.*')
  if(!is.na(t0[1])){
    c0 <- c(c0, jUniq[i])
  }
}
c0 # "CANCER-AM CANCER SOC" ==> no further journal variant to add
# not removing the excluded 6 T2 journals here to avoid error
save(fnl, file = 'data/journal_data/journal_variants_202207.rda')


###################################
# Archived code for previous top64
###################################

## read all cleaned data
lst <- list()
i <- 0
for(y in 1965:1990){
  print(y)
  i <- i + 1 
  load(paste('data_v12/refs2/refs', '.rda', sep = as.character(y)))
  lst[[i]] <- refs$df
}

## get all unique journal names across years
jAll <- c()
for(i in 1:26){
  tdf <- lst[[i]]
  jTemp <- unique(tdf[tdf$author != -1 & tdf$year != -1 & tdf$journal != -1, 'journal'])
  jAll <- c(jAll, jTemp)
}
jUniq <- unique(jAll)
length(jUniq) # 33494 unique journal names

## read the list of top cited journals in 1990
top62 <- as.data.frame(read_excel(path = '~/Dropbox/HIVAIDS_Networks/weekly_reports/1990_Journal_codes(1-12-20).xlsx',
                    sheet = '13 Fields', ))[1:62,]
colnames(top62)[22] <- 'assigned'


#==========
# Virology
#==========

# extract journal names in virology field
vjnls <- as.vector(top62[!is.na(top62$Virology), 'journal'])
vjnls # 5 journals in this field: "J VIROL"     "VIROLOGY"    "EMBO J"      "J GEN VIROL" "J MED VIROL"

# "J VIROL": JOURNAL OF VIROLOGY
l1 <- c()
for(i in 1:length(jUniq)){
  #t0 <- str_match(jUniq[i], 'J[:alpha:]*[:space:]+VIROL.*')
  t0 <- str_match(jUniq[i], 'J[:alpha:]*[:space:]+O*F*[:space:]*VIROL.*')
  if(!is.na(t0[1])){
    l1 <- c(l1, jUniq[i])
  }
}
l1
#' [1] "J VIROL"              "J VIROLOGY"           "J VIROL METHODS"      "J VIROL METH"         "J VIROLOGICAL METHOD"
#' [6] "MED J VIROLOGY"       "CHINESE J VIROLOGY"   "CHINESE J VIROL"      "J VIROL JUN"          "J VIROL MET"         
#' [11] "J VIROLOGIC"          "J VIROLOGICAL"        "CHIN J VIROLOGY"      "J VIROL MAY"          "OJ VIROL"
#v1 <- c("J VIROL", "J VIROLOGY", "J VIROL JUN", "J VIROLOGIC", "J VIROL MAY", "OJ VIROL")
# updated
#'[1] "J VIROL"              "J VIROLOGY"           "JOURNAL OF VIROLOGY"
#'[4] "J VIROL METHODS"      "J VIROL METH"         "J VIROLOGICAL METHOD"
#'[7] "MED J VIROLOGY"       "CHINESE J VIROLOGY"   "CHINESE J VIROL"
#'[10] "J VIROL JUN"          "J VIROL MET"          "J VIROLOGIC"
#'[13] "J VIROLOGICAL"        "CHIN J VIROLOGY"      "J VIROL MAY"
#'[16] "OJ VIROL"  
vl[[1]] <- c("J VIROL", "J VIROLOGY", "JOURNAL OF VIROLOGY", 
             "J VIROL JUN", "J VIROLOGIC", "J VIROL MAY", "OJ VIROL")

# "VIROLOGY": VIROLOGY
l2 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], '(.*)(VIRO.*)')
  if(!is.na(t0[1])){
    if(t0[2] == ''){
      l2 <- c(l2, jUniq[i])
    }
  }
}
#' [1] "VIROLOGY"             "VIROLOGIE KLINIK"     "VIROLOGY MONOGRAPHS"  "VIROLOGICAL TECHNIQU" "VIROL MONOGR"        
#' [6] "VIROLOGY EPIDEMIOLOG" "VIROL"                "VIROLOGY MONOGR"      "VIROL ABSTR"          "VIROLOGICAL PROCEDUR"
#' [11] "VIROLOGY PROCEDURES"  "VIROLOGIE"            "VIROLOGISCHE ARBEITS" "VIROLOGY MONOGRAPH"   "VIROLOGY AGR"        
#' [16] "VIROL MONOGRAPHS"     "VIROL IMMUNOLOGY IMM" "VIROLOGY P NATN ACAD" "VIROLOGIE MED"        "VIROLOGY PRACTICAL A"
#' [21] "VIROLOGY TISSUE CULT" "VIROL PASTEUR I E"    "VIROLOGY FIELDS"      "VIROLOGY HLTH CARE"   "VIROLOGY 1990"       
#' [26] "VIROL SINICA"         "VIROLOGY IMMUNOLOGY"  "VIROLOG"  
v2 <- c("VIROLOGY", "VIROL", "VIROLOGY 1990", "VIROLOG") # Virologie is another journal

# "EMBO J": EMBO JOURNAL
l3 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], 'EMBO[:alpha:]*[:space:]*J.*')
  if(!is.na(t0[1])){
    l3 <- c(l3, jUniq[i])
  }
}
l3
#'[1] "EMBO J"
v3 <- l3

# "J GEN VIROL": JOURNAL OF GENERAL VIROLOGY
l4 <- c()
for(i in 1:length(jUniq)){
  #t0 <- str_match(jUniq[i], 'J[:alpha:]*[:space:]+G[:alpha:]*[:space:]+V[:alpha:]*')
  t0 <- str_match(jUniq[i], 'J[:alpha:]*[:space:]+O*F*[:space:]*G[:alpha:]*[:space:]+V[:alpha:]*')
  if(!is.na(t0[1])){
    l4 <- c(l4, jUniq[i])
  }
}
l4
#'[1] "J GEN VIROLOGY" "J GEN VIROL" "J GENERAL VIROLOGY" 
#'[4] "J GEN VIROLJ" "J GENN VIROL" "J GENERAL V"  
#v4 <- l4
#'[1] "J GEN VIROLOGY"  "JOURNAL OF GENERAL VIROLOGY"  "J GEN VIROL" 
#'[4] "J GENERAL VIROLOGY"  "J GEN VIROLJ"  "J GENN VIROL"
#'[7] "J GENERAL V"  
vl[[4]] <- l4

# "J MED VIROL": JOURNAL OF MEDICAL VIROLOGY
l5 <- c()
for(i in 1:length(jUniq)){
  #t0 <- str_match(jUniq[i], 'J[:alpha:]*[:space:]+M[:alpha:]*[:space:]+VIR[:alpha:]*')
  t0 <- str_match(jUniq[i], 'J[:alpha:]*[:space:]+O*F*[:space:]*M[:alpha:]*[:space:]+VIR[:alpha:]*')
  if(!is.na(t0[1])){
    l5 <- c(l5, jUniq[i])
  }
}
l5
#'[1] "J MED VIROL"    "J MED VIROLOGY" "J MED VIR" 
#'If using 'J[:alpha:]*[:space:]+M[:alpha:]*[:space:]+V[:alpha:]*':
#'"J MED VIROL"     "J MED VIROLOGY"  "J MED VET MYCOL" "J MED VIR"       "J MAL VASCUL"  
#v5 <- c("J MED VIROL", "J MED VIROLOGY", "J MED VIR")
#'[1] "J MED VIROL" "J MED VIROLOGY" "JOURNAL OF MEDICAL VIROLOGY" "J MED VIR"  
vl[[5]] <- l5

# vector of variants for all 5 journals
# comment out in the 2nd round
"vl <- list(v1, v2, v3, v4, v5)
names(vl) <- vjnls
vl
save(vl, file = 'data_v12/other/journal_variants.rda')"
# 2nd round saving
save(vl, il, bl, im, ol, file = 'data_v12/other/journal_variants.rda')

#============
# Immunology
#============

# extract journal names in immunology field
ijnls <- as.vector(top62[!is.na(top62$Immunology), 'journal'])
ijnls
#' [1] "J IMMUNOL"            "EUR J IMMUNOL"        "CLIN EXP IMMUNOL"     "IMMUNOL REV"          "CELL IMMUNOL"        
#' [6] "IMMUNOLOGY"           "J IMMUNOL METHODS"    "IMMUNOL TODAY"        "ANNU REV IMMUNOL"     "CLIN IMMUNOL IMMUNOP"

# "J IMMUNOL": JOURNAL OF IMMUNOLOGY
l1 <- c()
for(i in 1:length(jUniq)){
  #t0 <- str_match(jUniq[i], '(.*)(J[:alpha:]*[:space:]+IM[:alpha:])')
  t0 <- str_match(jUniq[i], '(.*)(J[:alpha:]*[:space:]+O*F*[:space:]*IM[:alpha:])')
  if(!is.na(t0[1])){
    if(t0[2] == ''){
      l1 <- c(l1, jUniq[i])
    }
  }
}
l1
#' [1] "J IMMUNOL"            "J IMMUN"              "JOUR IMMUNOL"         "J IMMUNOLOGY"         "J IMMUNOCHEM"        
#' [6] "J IMMUNOL METHODS"    "J IMMUNOLOGICAL METH" "J IMMUNOGENET"        "J IMMUNOLOGY O"       "J IMMUNOL METH"      
#' [11] "J IMMUNOL METHOD"     "J IMMUNOPHARMACOL"    "J IMMUNOGENETICS"     "J IMMUNOASSAY"        "J IMM"               
#' [16] "J IMMUNOL DEC"        "J IMM METHODS"        "J IMMUNOLOGICAL"      "J IMMUNOL IMMUNOPATH" "J IMMUNO"            
#' [21] "J IMMUNOL LETT"       "J IMMUNOL M"          "J IMMUNOLOG"          "J IMMUNOLMETHODS"     "J IMMUNOL REV"       
#' [26] "J IMMUNOL S"          "J IMMUNOL S2"         "J IMMUOL"             "J IMMUNOGEN" 
#i1 <- c("J IMMUNOL", "J IMMUN", "JOUR IMMUNOL", "J IMMUNOLOGY", "J IMM", 
#        "J IMMUNOL DEC", "J IMMUNO", "J IMMUNOLOG", "J IMMUOL")
#'[1] "J IMMUNOL" "J IMMUN" "JOUR IMMUNOL" "JOURNAL OF IMMUNOLOGY"
#'[5] "J IMMUNOLOGY" "J IMMUNOCHEM" "JOURNAL OF IMMUNOLOGICAL METHODS" 
#'[8] "J IMMUNOL METHODS" "J IMMUNOLOGICAL METH" "J IMMUNOGENET"
#'[11] "J IMMUNOLOGY O" "J IMMUNOL METH" "J IMMUNOL METHOD" "J IMMUNOPHARMACOL"
#'[15] "J IMMUNOGENETICS" "J IMMUNOASSAY"  "J IMM" "J IMMUNOL DEC"
#'[19] "J IMM METHODS" "J IMMUNOLOGICAL"  "J IMMUNOL IMMUNOPATH" 
#'[22] "JOURNAL OF IMMUNOASSAY" "J IMMUNO" "J IMMUNOL LETT"
#'[25] "J IMMUNOL M" "J IMMUNOLOG" "J IMMUNOLMETHODS" "J IMMUNOL REV"
#'[29] "J IMMUNOL S" "J IMMUNOL S2" "J IMMUOL"
il[[1]] <- c("J IMMUNOL", "J IMMUN", "JOUR IMMUNOL", "J IMMUNOLOGY", "J IMM", 
        "J IMMUNOL DEC", "J IMMUNO", "J IMMUNOLOG", "J IMMUOL", 
        "JOURNAL OF IMMUNOLOGY")

# "EUR J IMMUNOL": EUROPEAN JOURNAL OF IMMUNOLOGY
l2 <- c()
for(i in 1:length(jUniq)){
  #t0 <- str_match(jUniq[i], 'EU[:alpha:]*[:space:]+J[:alpha:]*[:space:]+IM[:alpha:]')
  t0 <- str_match(jUniq[i], 'EU[:alpha:]*[:space:]+J[:alpha:]*[:space:]+O*F*[:space:]*IM[:alpha:]')
  if(!is.na(t0[1])){
    l2 <- c(l2, jUniq[i])
  }
}
l2
#'[1] "EUR J IMMUNOL"        "EUROP J IMMUNOL"      "EUR J IMMUN"
#'[4] "EUROPEAN J IMMUNOLOG" "EUR J IMMUNOLOGY"     "EUR J IMMMUNOL"
#i2 <- l2
#'[1] "EUROPEAN JOURNAL OF IMMUNOLOGY" "EUR J IMMUNOL"  "EUROP J IMMUNOL"
#'[4] "EUR J IMMUN" "EUROPEAN J IMMUNOLOG" "EUR J IMMUNOLOGY"
#'[7] "EUR J IMMMUNOL" 
il[[2]] <- l2

# "CLIN EXP IMMUNOL": CLINICAL AND EXPERIMENTAL IMMUNOLOGY
l3 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], 'CL.*EXP[:alpha:]*[:space:]+IM[:alpha:]')
  if(!is.na(t0[1])){
    l3 <- c(l3, jUniq[i])
  }
}
l3
#'[1] "CLINICAL AND EXPERIMENTAL IMMUNOLOGY" "CLIN EXP IMMUNOL"                    
#'[3] "CLIN EXP IMMUN"                       "CLIN EXPT IMMUNOLOGY"                
#'[5] "CLINICAL EXPTL IMMUN"                 "J CLIN EXP IMMUNOL"                  
#'[7] "J CLIN EXPT IMMUNOLO"                 "AFR J CLIN EXP IMMUN"                
#'[9] "CLIN EXP IMM"   
i3 <- c("CLINICAL AND EXPERIMENTAL IMMUNOLOGY", "CLIN EXP IMMUNOL", "CLIN EXP IMMUN",
        "CLIN EXPT IMMUNOLOGY", "CLINICAL EXPTL IMMUN", "CLIN EXP IMM" )

# "IMMUNOL REV": IMMUNOLOGICAL REVIEWS
l4 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], '(.*)(IM[:alpha:]*[:space:]+REV[:alpha:]*)')
  if(!is.na(t0[1])){
    if(t0[2] == ''){
      l4 <- c(l4, jUniq[i])
    }
  }
}
# IM[:alpha:]*[:space:]+R[:alpha:].*
# (.*)(IM[:alpha:]*[:space:]+R[:alpha:].*)
l4
#'[1] "IMMUNOL REV"       "IMMUNOLOGY REV"    "IMMUNOLOGICAL REV" "IMMUN REV"        
#'[5] "IMM REV"           "IMMUNODEFIC REV" 
i4 <- l4[1:5]

# "CELL IMMUNOL": CELLULAR IMMUNOLOGY
l5 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], '(.*)(CEL[:alpha:]*[:space:]+IM[:alpha:]*)')
  if(!is.na(t0[1])){
    if(t0[2] == ''){
      l5 <- c(l5, jUniq[i])
    }
  }
}
l5
#'[1] "CELL IMMUNOL"         "CELLULAR IMMUNOLOGY"  "CELL IMMUN"           "CELLULAR IMMUN"      
#'[5] "CELLULAR IMMUNOL"     "CELL IMMUNOL IMMUNOP" "CELLS IMMUNOGLOBULIN" "CELLULAR IMMUNITY VI"
#'[9] "CELL IMMUNO"          "CELLULAR IMMUNE EVEN" "CELLULAR IMMUNOTHERA" "CEL IMMUNOL"         
#'[13] "CELLULAR IM"          "CELLULAR IMMUNITY IM"
i5 <- c(l5[1:5], "CELL IMMUNO", "CEL IMMUNOL", "CELLULAR IM")

# "IMMUNOLOGY": IMMUNOLOGY
l6 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], '(.*)(IMMUNOL[:alpha:]*)(.*)')
  if(!is.na(t0[1])){
    if(t0[2] == '' & t0[4] == ''){
      l6 <- c(l6, jUniq[i])
    }
  }
}
l6
#'[1] "IMMUNOLOGY"           "IMMUNOL"              "IMMUNOLOGIE"         
#'[4] "IMMUNOLOGIYA"         "IMMUNOLYUMINESTSENTS" "IMMUNOLOGIA"         
#'[7] "IMMUNOLOGIJA"         "IMMUNOLOGIKA" 
i6 <- c("IMMUNOLOGY", "IMMUNOL")

# "J IMMUNOL METHODS": JOURNAL OF IMMUNOLOGICAL METHODS
l7 <- c()
for(i in 1:length(jUniq)){
  #t0 <- str_match(jUniq[i], 'J[:alpha:]*[:space:]+IM[:alpha:]*[:space:]+M[:alpha:]*')
  t0 <- str_match(jUniq[i], 'J[:alpha:]*[:space:]+O*F*[:space:]*IM[:alpha:]*[:space:]+M[:alpha:]*')
  if(!is.na(t0[1])){
    l7 <- c(l7, jUniq[i])
  }
}
l7
#'[1] "J IMMUNOL METHODS"    "J IMMUNOLOGICAL METH" "J IMMUNOL METH"      
#'[4] "J IMMUNOL METHOD"     "J IMM METHODS"        "J IMMUNOL M"
#i7 <- l7
#'[1] "JOURNAL OF IMMUNOLOGICAL METHODS" "J IMMUNOL METHODS"
#'[3] "J IMMUNOLOGICAL METH"             "J IMMUNOL METH"
#'[5] "J IMMUNOL METHOD"                 "J IMM METHODS"
#'[7] "J IMMUNOL M" 
il[[7]] <- l7

# "IMMUNOL TODAY": IMMUNOLOGY TODAY
l8 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], '(.*)(IM[:alpha:]*[:space:]+TOD[:alpha:]*)')
  if(!is.na(t0[1])){
    if(t0[2] == ''){
      l8 <- c(l8, jUniq[i])
    }
  }
}
l8
#'[1] "IMMUNOL TODAY"     "IMMUNOLOGY TODAY"  "IMMUNOL TODAY SEP" "IMMUNOL TOD"      
#'[5] "IMMUNOL TODAY MAY"
i8 <- l8

# "ANNU REV IMMUNOL": ANNUAL REVIEW OF IMMUNOLOGY
l9 <- c()
for(i in 1:length(jUniq)){
  #t0 <- str_match(jUniq[i], '(.*)(A[:alpha:]*[:space:]+REV[:alpha:]*[:space:]+I[:alpha:]*)')
  t0 <- str_match(jUniq[i], '(.*)(A[:alpha:]*[:space:]+REV[:alpha:]*[:space:]+O*F*[:space:]*I[:alpha:]*)')
  if(!is.na(t0[1])){
    if(t0[2] == ''){
      l9 <- c(l9, jUniq[i])
    }
  }
}
l9
#'[1] "ANNU REV IMMUNOL" "ANN REV IMMUNOL"  "A REV IMMUN"      "ANN REV IMM"
i9 <- l9
# not changed adding O*F*[:space:]*

# "CLIN IMMUNOL IMMUNOP": CLINICAL IMMUNOLOGY AND IMMUNOPATHOLOGY
l10 <- c()
for(i in 1:length(jUniq)){
  #t0 <- str_match(jUniq[i], 'CL[:alpha:]*[:space:]+I[:alpha:]*[:space:]+I[:alpha:]*')
  t0 <- str_match(jUniq[i], 'CL[:alpha:]*[:space:]+I[:alpha:]*[:space:]+&*A*N*D*[:space:]*I[:alpha:]*')
  if(!is.na(t0[1])){
    l10 <- c(l10, jUniq[i])
  }
}
l10
#'[1] "CLIN IMMUNOL IMMUNOP" "CLIN IMMUN IMMUNOPAT" "CLIN IMMUNOL IMMUNPA"
#'[4] "CLIN IMMUNOLOGY IMMU" "CLIN IMMUNOL IMMOPAT" "CLIN IMMUNOL I"      
#'[7] "J CLIN IMMUNOL IMMUN" # J CLIN IMMUNOL ==> JOURNAL OF CLINICAL IMMUNOLOGY?
#i10 <- l10[1:6]
#'[1] "CLINICAL IMMUNOLOGY AND IMMUNOPATHOLOGY" "CLIN IMMUNOL IMMUNOP"
#'[3] "CLIN IMMUN IMMUNOPAT"                    "CLIN IMMUNOL IMMUNPA"
#'[5] "CLIN IMMUNOLOGY IMMU"                    "CLIN IMMUNOL IMMOPAT"
#'[7] "CLIN IMMUNOL I"                          "J CLIN IMMUNOL IMMUN"
il[[10]] <- l10[1:7]

# wrap variants into list
"il <- list(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10)
names(il) <- ijnls
il
save(vl, il, file = 'data_v12/other/journal_variants.rda')"
save(vl, il, bl, im, ol, file = 'data_v12/other/journal_variants.rda')

#==========================
# Basic Biological Science 
#==========================

bjnls <- as.vector(top62[!is.na(top62$`Basic Biological Science`), 'journal'])
bjnls # 10 journals

# 1. "J EXP MED": JOURNAL OF EXPERIMENTAL MEDICINE
l1 <- c()
for(i in 1:length(jUniq)){
  #t0 <- str_match(jUniq[i], '(.*)(J[:alpha:]*[:space:]+EX[:alpha:]*[:space:]+M[:alpha:]*)')
  t0 <- str_match(jUniq[i], '(.*)(J[:alpha:]*[:space:]+O*F*[:space:]*EX[:alpha:]*[:space:]+M[:alpha:]*)')
  if(!is.na(t0[1])){
    if(t0[2] == ''){
      l1 <- c(l1, jUniq[i])
    }
  }
}
l1
#' [1] "J EXP MED"            "JOUR EXP MED"         "JOUR EXPTL MED"
#' [4] "J EXPER MED"          "J EXPTL MED"          "J EXPTL MOL PATHOL"  
#' [7] "J EXPERIMENTAL MEDIC" "J EXP MED 2"          "J EXP MED TOHOKU"  
#' [10] "J EXPL MED"           "J EXPT MED"           "J EXP MED S"  
#' [13] "J EXP MEDICINE"       "J EXPT MEDICINE" 
#b1 <- c(l1[c(1:5, 7, 8, 10, 11, 13, 14)])
#'[1] "J EXP MED"                        "JOUR EXP MED"
#'[3] "JOUR EXPTL MED"                   "J EXPER MED"
#'[5] "J EXPTL MED"                      "J EXPTL MOL PATHOL"
#'[7] "JOURNAL OF EXPERIMENTAL MEDICINE" "J EXPERIMENTAL MEDIC"
#'[9] "J EXP MED 2"                      "J EXP MED TOHOKU"
#'[11] "J EXPL MED"                       "J EXPT MED"
#'[13] "J EXP MED S"                      "J EXP MEDICINE"
#'[15] "J EXPT MEDICINE" 
bl[[1]] <- l1[c(1:5, 7:9, 11:15)]

# 2. "J BIOL CHEM": JOURNAL OF BIOLOGICAL CHEMISTRY
l2 <- c()
for(i in 1:length(jUniq)){
  #t0 <- str_match(jUniq[i], 'J[:alpha:]*[:space:]+B[:alpha:]*[:space:]+C[:alpha:]*')
  t0 <- str_match(jUniq[i], 'J[:alpha:]*[:space:]+O*F*[:space:]*B[:alpha:]*[:space:]+C[:alpha:]*')
  if(!is.na(t0[1])){
    l2 <- c(l2, jUniq[i])
  }
}
l2
#' [1] "J BIOL CHEM"                "JOUR BIOL CHEM"
#' [3] "CAN J BIOCHEM CELL B"       "EUROP J BIOL CLIN RE" 
#' [5] "J BIOLOGICAL CHEM"          "INT J BIOMED COMPUT"   
#' [7] "J BALTIMORE COLL DENT SURG" "J BIO CHEM"  
#' [9] "J BURN CARE REHABIL"        "J BIOACT COMPAT POL"  
#' [11] "J BIOACT COMPAT POLY"    
#b2 <- l2[c(1, 2, 5, 8)]
#'[1] "J BIOL CHEM"                     "JOURNAL OF BIOLOGICAL CHEMISTRY"
#'[3] "JOUR BIOL CHEM"                  "CAN J BIOCHEM CELL B"
#'[5] "EUROP J BIOL CLIN RE"            "J BIOLOGICAL CHEM"
#'[7] "INT J BIOMED COMPUT"             "J BALTIMORE COLL DENT SURG"
#'[9] "J BIO CHEM"                      "J BURN CARE REHABIL"
#'[11] "J BIOACT COMPAT POL"             "J BIOACT COMPAT POLY"
bl[[2]] <- l2[c(1:3, 6, 9)]

# 3. "MOL CELL BIOL": MOLECULAR AND CELLULAR BIOLOGY
l3 <- c()
for(i in 1:length(jUniq)){
  "t0 <- str_match(jUniq[i], 'MOL[:alpha:]*[:space:]+C[:alpha:]*[:space:]+B[:alpha:]*')
  if(!is.na(t0[1])){
    l3 <- c(l3, jUniq[i])
  }"
  t <- str_match(jUniq[i], '(.*)(MOL[:alpha:]*[:space:]+&*A*N*D*[:space:]*C[:alpha:]*[:space:]+B[:alpha:]*)')
  if(!is.na(t[1])){
    if(t[2] == ''){
      l3 <- c(l3, jUniq[i])
    }
  }
}
l3
#'[1] "MOLECULAR CELLULAR B" "MOL CELLULAR BASIS A" "MOL CELL BIOCHEM"    
#'[4] "ICNUCLA S MOL CELL B" "MOL CELL BIOCH"       "MOL CELLULAR BASIS L"
#'[7] "MOL CELL BIOL"        "DNA-J MOLEC CELL BIO" "UCLL S MOL CELL BIOL"
#'[10] "UCLA S MOLEC CELL BI" "J MOL CELL BIOL"      "UCLA SYMP MOL CELL B"
#'[13] "UCLA S MOL CELLU BIO" "UCLA S MOL CELL BIOL" "MOL CELLULAR BIOL LY"
#'[16] "MOL CELLUL BIOL"      "MOL CELLULAR BIOL"    "MOL CELLULAR BIOL EY"
#'[19] "MOL CELL BI"          "UCLA SYP MOL CELL BI" "MOL CELLULAR BIOL WO"
#'[22] "MOL CELLULAR BIOLD"   "MOL CELL BIL"         "MOL CONTROL BLOOD CE"
#'[25] "UCLA S MOL CELLUAR B"
#'Don't know whether "MOLECULAR CELLULAR B" or "MOL CELL BI" is 
#'biology, biochem, or biomechanics. Not included to make sure no confounders.
'b3 <- c("MOL CELL BIOL", "MOL CELLULAR BIOL LY", "MOL CELLUL BIOL", 
        "MOL CELLULAR BIOL", "MOL CELLULAR BIOL EY", "MOL CELLULAR BIOL WO",
        "MOL CELLULAR BIOLD", "MOL CONTROL BLOOD CE")'
#'[1] "MOLECULAR CELLULAR B"           "MOL CELLULAR BASIS A"
#'[3] "MOL CELL BIOCHEM"               "MOL CELL BIOCH"
#'[5] "MOL CELLULAR BASIS L"           "MOL CELL BIOL"
#'[7] "MOL CELLULAR BIOL LY"           "MOL CELLUL BIOL"
#'[9] "MOL CELLULAR BIOL"              "MOL CELLULAR BIOL EY"
#'[11] "MOL CELL BI"                    "MOL CELLULAR BIOL WO" 
#'[13] "MOL CELLULAR BIOLD"             "MOL CELL BIL"
#'[15] "MOL CONTROL BLOOD CE"           "MOLECULAR AND CELLULAR BIOLOGY"
bl[[3]] <- l3[c(1, 6, 8, 9, 11, 13, 14, 16)]

# 4. "J CLIN MICROBIOL": JOURNAL OF CLINICAL MICROBIOLOGY
l4 <- c()
for(i in 1:length(jUniq)){
  "t0 <- str_match(jUniq[i], 'J[:alpha:]*[:space:]+CL[:alpha:]*[:space:]+M[:alpha:]*')
  if(!is.na(t0[1])){
    l4 <- c(l4, jUniq[i])
  }"
  t <- str_match(jUniq[i], '(.*)(J[:alpha:]*[:space:]+O*F*[:space:]*CL[:alpha:]*[:space:]+M[:alpha:]*)')
  if(!is.na(t[1])){
    if(t[2] == ''){
      l4 <- c(l4, jUniq[i])
    }
  }
}
l4
#'[1] "JAPAN J CLIN MED"     "JAP J CLIN MED S"     "J CLIN MICROBIOL"    
#'[4] "J CLIN MICROBIOL AUG" "JAP J CLIN MED"       "JPN J CLIN MED"      
#'[7] "EUR J CLIN MICROBIOL" "J CLIN MICROBIOLOGIC" "J CLIN MICRIBIOL"    
#'[10] "EUROP J CLIN MICROBI" "J CLIN MICRO"         "EUROP J CLIN M"      
#'[13] "J CLIN MICROBIOLOGY"  "J CLIN MICROSC"       "J CLIN MICROB"       
#'[16] "J CLIN MIKROBIOL"     "J CLIN MICR"   
#b4 <- l4[c(3, 4, 8, 9, 11, 13, 15, 16, 17)]
#'[1] "J CLIN MICROBIOL"                 "J CLIN MICROBIOL AUG"
#'[3] "J CLIN MICROBIOLOGIC"             "J CLIN MICRIBIOL"
#'[5] "J CLIN MICRO"                     "J CLIN MICROBIOLOGY"
#'[7] "J CLIN MICROSC"                   "J CLIN MICROB"
#'[9] "J CLIN MIKROBIOL"                 "J CLIN MICR"
#'[11] "JOURNAL OF CLINICAL MICROBIOLOGY"
bl[[4]] <- l4

# 5. "INFECT IMMUN": INFECTION AND IMMUNITY
l5 <- c()
for(i in 1:length(jUniq)){
  "t0 <- str_match(jUniq[i], 'INF[:alpha:]*[:space:]+IM[:alpha:]*')
  if(!is.na(t0[1])){
    l5 <- c(l5, jUniq[i])
  }"
  t <- str_match(jUniq[i], '(.*)(INF[:alpha:]*[:space:]+&*A*N*D*[:space:]*IM[:alpha:]*)')
  if(!is.na(t[1])){
    if(t[2] == ''){
      l5 <- c(l5, jUniq[i])
    }
  }
}
l5
#'[1] "INFECT IMMUN"         "J INFECT IMMUN"       "INFECT IMMUNITY"     
#'[4] "INFEC IMMUN"          "INFECTION IMMUNITY"   "INFEC IMMUNITY"      
#'[7] "INFECT IMMUNOL"       "INFEKTION IMMUNANTWO" "INF IMMUNOL"         
#'[10] "INFECT IMMUM"         "INFECTIONS IMMUNOCOM" "2ND P S INF IMM HOST"
#'[13] "INF IMMUN"            "2ND INT S INF IMM HO" "INFECTION IMMUNOLOGY"
#'[16] "PATIENT INFEKTION IM" "INFECTION IMMUNITY B" "INFECT IMMUN DEC"    
#'[19] "INFECT IMMU"          "INF IMM"              "PATIENT INFECTION IM"
#'[22] "4TH INT S INF IMM HO" "3RD INT S INF IMM HO" "INFEKTIONEN IMPFUNGE"
#'[25] "EINFUHRUNG IMMUNHAMA" "INFECT IMMUNITY BLOO"
#'Does "Infection and Immunology" exist?
#b5 <- l5[c(1, 3, 4:6, 13, 18, 19, 20)]
#'[1] "INFECT IMMUN"           "INFECTION AND IMMUNITY" "INFECT IMMUNITY"
#'[4] "INFEC IMMUN"            "INFECTION IMMUNITY"     "INFEC IMMUNITY"
#'[7] "INFECT IMMUNOL"         "INFECTION & IMMUNITY"   "INFEKTION IMMUNANTWO"
#'[10] "INF IMMUNOL"            "INFECT IMMUM"           "INFECTIONS IMMUNOCOM"
#'[13] "INF IMMUN"              "INFECTION IMMUNOLOGY"   "INFECTION IMMUNITY B"
#'[16] "INFECT IMMUN DEC"       "INFECT IMMU"            "INF IMM"
#'[19] "INFEKTIONEN IMPFUNGE"   "INFECT IMMUNITY BLOO" 
bl[[5]] <- l5[c(1:6, 8, 11, 13, 16:18)]

# 6. "ANTIMICROB AGENTS CH": ANTIMICROBIAL AGENTS AND CHEMOTHERAPY
l6 <- c()
for(i in 1:length(jUniq)){
  #t0 <- str_match(jUniq[i], '(.*)(AN[:alpha:]*[:space:]+A[:alpha:]*[:space:]+C[:alpha:]*)')
  t0 <- str_match(jUniq[i], '(.*)(AN[:alpha:]*[:space:]+A[:alpha:]*[:space:]+&*A*N*D*[:space:]*C[:alpha:]*)')
  if(!is.na(t0[1])){
    if(t0[2] == ''){
      l6 <- c(l6, jUniq[i])
    }
  }
}
l6
#'[1] "ANTIBIOT AND CHE MOTHER"                "ANTIBIOT AND CHEMOTHER" 
#'[3] "ANTIMICROB AGENTS CH"                   "ANTIMICROB AGENTS CHEMOTHER (BETHESDA)"
#'[5] "ANTIMICROG AGENTS CH"                   "ANTIMICROB AGENTS CHEMOTHER" 
#'[7] "ANTIMICROB AG CHEMOT"                   "ANTIMICROB AGENTS CHEMOTHERAP"
#'[9] "ANESTH ANALG CLEVE"                     "ANTIMICROB AGENT CHE"
#b6 <- l6[c(1:8, 10)]
#'[1] "ANTIBIOT AND CHE MOTHER"        "ANTIBIOT AND CHEMOTHER"
#'[3] "ANTIMICROB AGENTS CH"           "ANTIMICROB AGENTS CHEMOTHER (BETHESDA)"
#'[5] "ANTIMICROG AGENTS CH"           "ANTIMICROBIAL AGENTS AND CHEMOTHERAPY"
#'[7] "ANTIMICROB AGENTS CHEMOTHER"    "ANTIMICROB AG CHEMOT"
#'[9] "ANTIMICROB AGENTS CHEMOTHERAP"  "ANESTH ANALG CLEVE"
#'[11] "ANTIMICROB AGENT CHE"          "ANN AM ACAD POLIT SS" 
bl[[6]] <- l6[c(3:9, 11)]

# 7. "J MOL BIOL": JOURNAL OF MOLECULAR BIOLOGY
l7 <- c()
for(i in 1:length(jUniq)){
  #t0 <- str_match(jUniq[i], 'J[:alpha:]*[:space:]+MO[:alpha:]*[:space:]+B[:alpha:]*')
  t0 <- str_match(jUniq[i], 'J[:alpha:]*[:space:]+O*F*[:space:]*MO[:alpha:]*[:space:]+B[:alpha:]*')
  if(!is.na(t0[1])){
    l7 <- c(l7, jUniq[i])
  }
}
l7
#b7 <- l7
#'[1] "J MOL BIOL"                   "JOUR MOLECULAR BIOL"
#'[3] "JOURNAL OF MOLECULAR BIOLOGY" "J MOLEC BIOL"
#'[5] "J MOLECULAR BIOLOGY"          "J MOL BIOLOGY"
#'[7] "J MOL BOL"
bl[[7]] <- l7

# 8. "ANAL BIOCHEM": ANALYTICAL BIOCHEMISTRY
l8 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], '(.*)(ANA[:alpha:]*[:space:]+BI[:alpha:]*)')
  if(!is.na(t0[1])){
    l8 <- c(l8, jUniq[i])
  }
}
l8
#'[1] "ANAL BIOCHEM"  "REG GANADIENNE BIOL"  "REV CANADIENNE BIOL" 
#'[4] "BOLLETTINO SOCIETA ITALIANA BIOLOGIA SPERIMENTALE"  "ANAL BIOCH"  
#'[6] "ANALYT BIOCHEM"  "ANAL BINARY DATA"  "ANAL BIOCH A"
b8 <- l8[c(1, 5, 6)]

# 9. "BIOCHEMISTRY-US": BIOCHEMISTRY
l9 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], 'BIOC.*US[:alpha:]*')
  if(!is.na(t0[1])){
    l9 <- c(l9, jUniq[i])
  }
}
l9
#'[1] "BIOCHEMISTRY-USSR+"   "BIOCHEMISTRY-US"      "BIOCHEMISTRY-USSR"   
#'[4] "BIOCHEMISTRY VIRUSES" "BIOCH VIRUSES"        "BIOCHEMISTRY USSR"
b9 <- l9[2]

# 10. "BIOCHEM BIOPH RES CO": BIOCHEMICAL AND BIOPHYSICAL RESEARCH COMMUNICATIONS
l10 <- c()
for(i in 1:length(jUniq)){
  #t0 <- str_match(jUniq[i], 'B[:alpha:]*[:space:]+B[:alpha:]*[:space:]+R[:alpha:]*[:space:]+C[:alpha:]*')
  t0 <- str_match(jUniq[i], 'B[:alpha:]*[:space:]+&*A*N*D*[:space:]*B[:alpha:]*[:space:]+R[:alpha:]*[:space:]+C[:alpha:]*')
  if(!is.na(t0[1])){
    l10 <- c(l10, jUniq[i])
  }
}
l10
#'[1] "BIOCHEM BIOPH RES CO"  "BIOCHEM BIOPHYS RES COMMUN"  "BIOCH BIOPHYS RES CO"  
#b10 <- l10
#'[1] "BIOCHEM BIOPH RES CO"
#'[2] "BIOCHEM BIOPHYS RES COMMUN"
#'[3] "BIOCHEMICAL AND BIOPHYSICAL RESEARCH COMMUNICATIONS"
#'[4] "BIOCH BIOPHYS RES CO" 
bl[[10]] <- l10

# wrap the variants into lists
"bl <- list(b1, b2, b3, b4, b5)
names(bl) <- bjnls[1:5]
bl[[6]] <- b6
bl[[7]] <- b7
bl[[8]] <- b8
bl[[9]] <- b9
bl[[10]] <- b10
names(bl) <- bjnls
save(vl, il, bl, file = 'data_v12/other/journal_variants.rda')"
save(vl, il, bl, im, ol, file = 'data_v12/other/journal_variants.rda')

#===================
# Internal Medicine
#===================

i2jnls <- as.vector(top62[!is.na(top62$`Internal Medicine`), 'journal'])
i2jnls

# 1."AM REV RESPIR DIS": AMERICAN REVIEW OF RESPIRATORY DISEASE
l1 <- c()
for(i in 1:length(jUniq)){
  #t0 <- str_match(jUniq[i], 'A[:alpha:]*[:space:]+R[:alpha:]*[:space:]+R[:alpha:]*[:space:]+D[:alpha:]*')
  t0 <- str_match(jUniq[i], 'A[:alpha:]*[:space:]+R[:alpha:]*[:space:]+O*F*[:space:]*R[:alpha:]*[:space:]+D[:alpha:]*')
  if(!is.na(t0[1])){
    l1 <- c(l1, jUniq[i])
  }
}
l1
#'[1] "AMER REV RESP DIS"        "AM REV RESPIR DIS"        "AM REV RESP DIS" 
#'[4] "AMER REV RESP DIS S"      "AM REV RESP DIS S"        "AMER REV RESP D S167"
#'[7] "AM REV RESPIRAT DISE"     "AM REV RESP DIS 2"        "AM REV RESP DIS S342"
#'[10] "AM REV RESP DIS S85"      "AM REV RESPIRATORY D"     "AM REV RESPIR D S164"
#'[13] "AM REV RESPIR DIS S"      "AM REV REP DIS S1312"     "AM REV RESPIR DIS S1"
#'[16] "AMER REV RESPIRATORY DIS" "AM REV RESPIR DIS A"      "AM REV RES DIS" 
#'[19] "AM REV RESPIR D S189"     "AM REV RESPIR DI S81"     "AM REV RESPIR D S195"
#'[22] "AM REV RESPIR DIS SA"     "AM REO RESP DIS S"  
#i1 <- l1
#'[1] "AMER REV RESP DIS"                      "AM REV RESPIR DIS"
#'[3] "AM REV RESP DIS"                        "AMER REV RESP DIS S"
#'[5] "AM REV RESP DIS S"                      "AMER REV RESP D S167"
#'[7] "AM REV RESPIRAT DISE"                   "AM REV RESP DIS 2"
#'[9] "AM REV RESP DIS S342"                   "AM REV RESP DIS S85"
#'[11] "AM REV RESPIRATORY D"                   "AMERICAN REVIEW OF RESPIRATORY DISEASE"
#'[13] "AM REV RESPIR D S164"                   "AM REV RESPIR DIS S"
#'[15] "AM REV REP DIS S1312"                   "AM REV RESPIR DIS S1"
#'[17] "AMER REV RESPIRATORY DIS"               "AM REV RESPIR DIS A"
#'[19] "AM REV RES DIS"                         "AM REV RESPIR D S189"
#'[21] "AM REV RESPIR DI S81"                   "AM REV RESPIR D S195"
#'[23] "AM REV RESPIR DIS SA"                   "AM REO RESP DIS S"
im[[1]] <- l1

# 2."BLOOD": BLOOD
l2 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], '(.*)(BLOOD)')
  if(!is.na(t0[1])){
    if(t0[2] == ''){
      l2 <- c(l2, jUniq[i])
    }
  }
}
l2
#'[1] "BLOOD"                "BLOOD-J HEMATOL"      "BLOOD GROUPS TRANSFU"
#'[4] "BLOOD JOUR HEMATOL"   "BLOOD COAGULATION HE" "BLOOD TRANSFUSION CL"
#'[7] "BLOOD DISEASES INFAN" "BLOOD S2"             "BLOOD SUPPLY BONE" 
#'[10] "BLOOD TISSUE ANTIGEN" "BLOOD CELLS"          "BLOOD GROUPS MAN"
#'[13] "BLOOD CELLS TISSUE"   "BLOOD TODD SANFORDS"  "BLOOD S1"
#'[16] "BLOOD S"              "BLOOD VESSELS"        "BLOOD BRAIN BARRIER"
#'[19] "BLOOD ITS DISORDERS"  "BLOOD CAOGULATION HA" "BLOOD LIPIDS LIPOPRO"
#'[22] "BLOOD LEUCOCYTES FUN" "BLOOD A"              "BLOOD FEV"
#'[25] "BLOOD S5"             "BLOOD S1A"            "BLOOD T IMMUNOHAEMAT"
#'[28] "BLOOD FLOW MEASUREME" "BLOOD REVIEWS"        "BLOOD TXB HAEMATOLOG"
#'[31] "BLOOD REV"            "BLOOD TRANSFUSION HT" "BLOOD TRANSFUSION PR"
i2 <- c("BLOOD", "BLOOD S2", "BLOOD S1", "BLOOD S", "BLOOD S5", "BLOOD S1A") 
# not sure about "BLOOD S*"

# 3."ANN NEUROL": ANNALS OF NEUROLOGY
l3 <- c()
for(i in 1:length(jUniq)){
  #t0 <- str_match(jUniq[i], '(.*)(AN[:alpha:]*[:space:]+NEU[:alpha:]*)')
  t0 <- str_match(jUniq[i], '(.*)(AN[:alpha:]*[:space:]+O*F*[:space:]*NEU[:alpha:]*)')
  if(!is.na(t0[1])){
    if(t0[2] == ''){
      l3 <- c(l3, jUniq[i])
    }
  }
}
l3
#'[1] "ANN NEUROL"          "ANN NEUROL S"        "ANN NEUROLOGY" 
#'[4] "ANN NEUROLOGY S"     "ANN NEUROL BOSTON S"
# round 2: no change
#'[1] "ANN NEUROL"          "ANN NEUROL S"        "ANN NEUROLOGY"
#'[4] "ANN NEUROLOGY S"     "ANN NEUROL BOSTON S"

# 4."CHEST": CHEST
l4 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], '(.*)(CHEST)')
  if(!is.na(t0[1])){
    if(t0[2] == ''){
      l4 <- c(l4, jUniq[i])
    }
  }
}
l4
#'[1] "CHEST"                "CHEST DISEASES"       "CHEST S1"
#'[4] "CHEST ROENTGENOLOGY"  "CHEST RADIOLOGY PATT" "CHEST S"
#'[7] "CHEST S3"             "CHEST GALLIUM"        "CHEST RADIOLOGY PLAI"
#'[10] "CHEST S2"
i4 <- l4[c(1, 3, 6, 7, 10)]

# 5."TRANSPLANTATION": TRANSPLANTATION
l5 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], '(.*)(TRANSPLANTATION)')
  if(!is.na(t0[1])){
    if(t0[2] == ''){
      l5 <- c(l5, jUniq[i])
    }
  }
}
l5
#'[1] "TRANSPLANTATION"             "TRANSPLANTATION ANTI" 
#'[3] "TRANSPLANTATION BULL"        "TRANSPLANTATION S" 
#'[5] "TRANSPLANTATION TISS"        "TRANSPLANTATION PROCEEDINGS"
#'[7] "TRANSPLANTATIONS TUM"        "TRANSPLANTATION (BALTIMORE)"
#'[9] "TRANSPLANTATION REV"         "TRANSPLANTATION URBA"
#'[11] "TRANSPLANTATION PROC"        "TRANSPLANTATION IMMU"
#'[13] "TRANSPLANTATION TEST"        "TRANSPLANTATION P"
#'[15] "TRANSPLANTATION CLIN"        "TRANSPLANTATION TODA" 
#'[17] "TRANSPLANTATION HDB"         "TRANSPLANTATION BALT"
#'[19] "TRANSPLANTATION S2"          "TRANSPLANTATION LIVE" 
i5 <- l5[c(1, 19)]
# add 'Transplantation S' in round 2
im[[5]][length(im[[5]])+1] <- "TRANSPLANTATION S" 

# 6."ARCH INTERN MED": ARCHIVES OF INTERNAL MEDICINE
l6 <- c()
for(i in 1:length(jUniq)){
  #t0 <- str_match(jUniq[i], '(.*)(AR[:alpha:]*[:space:]+IN[:alpha:]*[:space:]+M[:alpha:]*)')
  t0 <- str_match(jUniq[i], '(.*)(AR[:alpha:]*[:space:]+O*F*[:space:]*IN[:alpha:]*[:space:]+M[:alpha:]*)')
  if(!is.na(t0[1])){
    if(t0[2] == ''){
      l6 <- c(l6, jUniq[i])
    }
  }
}
l6
#'[1] "ARCH INTERN MED"       "ARCH INTERN MED CHIC"  "ARCH INT MED"
#'[4] "ARCH INTERN MED CHI"   "ARCH INTERNAL MED"     "ARCH INT MED CHICAGO" 
#'[7] "ARCH INV MED MEX S1"   "ARCH INVEST MED"       "ARCH INVEST MED (MEX)"
#'[10] "ARCH INT MDE CHICAGO"  "ARC INT MED"           "ARCH INT ME"   
#i6 <- l6[c(1, 3, 5, 8, 11, 12)]
#'[1] "ARCH INTERN MED"               "ARCH INTERN MED CHIC"
#'[3] "ARCH INT MED"                  "ARCH INTERN MED CHI"
#'[5] "ARCH INTERNAL MED"             "ARCH INT MED CHICAGO"
#'[7] "ARCHIVES OF INTERNAL MEDICINE" "ARCH INV MED MEX S1"
#'[9] "ARCH INVEST MED"               "ARCH INVEST MED (MEX)"
#'[11] "ARCH INT MDE CHICAGO"          "ARC INT MED"
#'[13] "ARCH INT ME"
im[[6]] <- l6[c(1, 3, 5, 7, 12, 13)]

# 7."GASTROENTEROLOGY": GASTROENTEROLOGY
l7 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], '(.*)(GASTRO.*)')
  if(!is.na(t0[1])){
    if(t0[2] == ''){
      l7 <- c(l7, jUniq[i])
    }
  }
}
l7
#'[1] "GASTROENTEROLOGY" "GASTROENTEROL" "GASTROENTEROL NUTRI" "GASTROENTEROL JPN"
#'[5] "GASTROENTEROLOGIA" "GASTROINTESTINAL PRO" "GASTROINTESTINAL ANG" "GASTROINTEST ENDOSC"
#'[9] "GASTROEN CLIN BIOL" "GASTROINTESTINAL DIS" "GASTROENTEROL ENDOSC" "GASTROINTESTINAL PAT"
#'[13] "GASTROENTEROL CLIN B" "GASTROINTEST RADIOL" "GASTROENTEROLOGIA JAPONICA"
#'[16] "GASTROINTESTINAL RAD" "GASTROINTESTINAL TRA" "GASTROENTEROL CLIN BIOL"
#'[19] "GASTROENT CLIN BIOL" "GASTROENTEROLOGIA BA" "GASTROINTESTINAL ENDOSCOPY"
#'[22] "GASTROENTEROLOGY LIV" "GASTROENTEROLOGIE ST" "GASTROENTEROL INT"
#'[25] "GASTROENTEROLOGIA CL" "GASTROENTEROL CLIN N" "GASTROINTEST CANCER" 
#'[28] "GASTROENTEROL INT S1" "GASTRODUODENAL PATHO" "GASTROENTEROL HEPATO"
i7 <- l7[c(1, 2)]

# 8. "NEUROLOGY": NEUROLOGY
l8 <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], '(.*)(NEUROLOGY.*)')
  if(!is.na(t0[1])){
    if(t0[2] == ''){
      l8 <- c(l8, jUniq[i])
    }
  }
}
l8
#'[1] "NEUROLOGY"            "NEUROLOGY LONDON"     "NEUROLOGY MINNEAP" 
#'[4] "NEUROLOGY MINNEAPOLI" "NEUROLOGY S2"         "NEUROLOGY 2 ED"
#'[7] "NEUROLOGY MINNEAP 2"  "NEUROLOGY OCULAR MUS" "NEUROLOGY S1"
#'[10] "NEUROLOGY 2"          "NEUROLOGY NEUROSURGE" "NEUROLOGY NY"
#'[13] "NEUROLOGY S"          "NEUROLOGY NEUROBIOLO" "NEUROLOGY CLIN"
#'[16] "NEUROLOGY NEWBORN"    "NEUROLOGY EYE MOVEME"
i8 <- l8[c(1, 5, 6, 9, 10, 13)]

# save results
"im <- list(i1, i2, i3, i4, i5, i6, i7, i8)
names(im) <- i2jnls
save(vl, il, bl, im, file = 'data_v12/other/journal_variants.rda')"
save(vl, il, bl, im, ol, file = 'data_v12/other/journal_variants.rda')

#========
# Others
#========

top9 <- c('NATURE', 'SCIENCE', 'P NATL ACAD SCI USA', 'NEW ENGL J MED', 'JAMA-J AM MED ASSOC', 
          'BMJ-BRIT MED J', 'AM J PUBLIC HEALTH', 'LANCET', 'MMWR-MORBID MORTAL W')
ojnls <- setdiff(top62$journal, 
                 c(names(vl), names(il), names(bl), names(im), top9))
ojnls <- sort(ojnls)
ojnls

ol <- list()

# 1."AIDS": AIDS
l <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], '(.*)(AIDS)')
  if(!is.na(t0[1])){
    if(t0[2] == ''){
      l <- c(l, jUniq[i])
    }
  }
}
l
ol[[1]] <- c("AIDS")

# 2."AIDS RES HUM RETROV": AIDS RESEARCH + AIDS RESEARCH AND HUMAN RETROVIRUSES
l <- c()
for(i in 1:length(jUniq)){
  #t0 <- str_match(jUniq[i], '(.*)(AIDS R[:alpha:]*[:space:]*H*[:alpha:]*[:space:]*R*[:alpha:]*)')
  t0 <- str_match(jUniq[i], '(.*)(AIDS[:space:]+R.*)')
  if(!is.na(t0[1])){
    if(t0[2] == ''){
      l <- c(l, jUniq[i])
    }
  }
}
l
#'[1] "AIDS RES"  "AIDS RESEARCH"  "AIDS RES HUM RETROV"  "AIDS RES S1"
#'[5] "AIDS RES HU"  "AIDS RELATED ISSUES"  "AIDS RES HUM RETROVIRUSES" 
#'[8] "AIDS RES HUMAN RETRO"  "AIDS RETROVIRUS"  "AIDS RS"
#'[11] "AIDS RES HEM RETROVI"  "AIDS RES MARY A S147"
#'[13] "AIDS RES HUM RETR"  "AIDS REC"  "AIDS REFERENCES MODE" 
ol[[2]] <- l[c(1:5, 7, 8, 10, 11:14)]
#' same results for l, but add [9] since there's no AIDS RETROVIRUS in WOS and
#' it should be identical as AIDS RS
ol[[2]][length(ol[[2]]) + 1] <- "AIDS RETROVIRUS"

# 3."AM J CLIN PATHOL": AMERICAN JOURNAL OF CLINICAL PATHOLOGY
l <- c()
for(i in 1:length(jUniq)){
  #t0 <- str_match(jUniq[i], 'AM[:alpha:]*[:space:]+J[:alpha:]*[:space:]+CL[:alpha:]*[:space:]+P')
  t0 <- str_match(jUniq[i], 'AM[:alpha:]*[:space:]+J[:alpha:]*[:space:]+O*F*[:space:]*CL[:alpha:]*[:space:]+P')
  if(!is.na(t0[1])){
    l <- c(l, jUniq[i])
  }
}
l
# round 1
#'[1] "AM J CLIN PATHOL"               "AMER J CLIN PATH" 
#'[3] "AMER JOUR CLIN PATH TECH SUPPL" "AMER JOUR CLIN PATH" 
#'[5] "AMER J CLIN PATH S"             "AM J CLIN PATH" 
#'[7] "AMER J CLIN PATH S22"           "AM J CLINICAL PATHOL" 
#'[9] "AMER J CLIN PATHOL"             "AM J CLIN PATHOLOGY" 
#'[11] "AM J CLIN PATHOL S"             "AM J CLIN P" 
#'[13] "AM J CLLIN PATHOL"              "AM J CLIAN PATHOL"
# round 2
#'[1] "AM J CLIN PATHOL"                       "AMER J CLIN PATH"
#'[3] "AMER JOUR CLIN PATH TECH SUPPL"         "AMER JOUR CLIN PATH"
#'[5] "AMER J CLIN PATH S"                     "AM J CLIN PATH"
#'[7] "AMER J CLIN PATH S22"                   "AM J CLINICAL PATHOL"
#'[9] "AMER J CLIN PATHOL"                     "AM J CLIN PATHOLOGY"
#'[11] "AMERICAN JOURNAL OF CLINICAL PATHOLOGY" "AM J CLIN PATHOL S"
#'[13] "AM J CLIN P"                            "AM J CLLIN PATHOL"
#'[15] "AM J CLIAN PATHOL" 
ol[[3]] <- l

# 4."AM J MED": AMERICAN JOURNAL OF MEDICINE
l <- c()
for(i in 1:length(jUniq)){
  #t0 <- str_match(jUniq[i], 'AM[:alpha:]*[:space:]+J[:alpha:]*[:space:]+MED')
  t0 <- str_match(jUniq[i], 'AM[:alpha:]*[:space:]+J[:alpha:]*[:space:]+O*F*[:space:]*MED')
  if(!is.na(t0[1])){
    l <- c(l, jUniq[i])
  }
}
l
#'[1] "AM J MED"              "AM J MED SCI"          "AMER J MED" 
#'[4] "AMER J MED SCI"        "AMER J MED TECHN"      "AMER J MED TECHNOL" 
#'[7] "AM J MED TECHNOL"      "AM J MED TECHN"        "AM J MEDICINE" 
#'[10] "AM J MEDICAL TECHNOL"  "AM J MED TECHNOLOGY"   "AM J MED TECH JUL" 
#'[13] "AMER JOUR MED TECHNOL" "AM J MED ACYCLOVIR S"  "AM J MED S"
#'[16] "AM J MED GENET"        "AM J MED 0728"         "AM J MED S1B"
#'[19] "AM JL MED"             "AM J MED S2A"          "AM J MED GENET SUPPL"
#'[22] "AM J MED S5C"          "AM J MED HYG"          "AM J MED ELECTRON"
# do not include 'AM J MED S' here because that exists as another journal
#ol[[4]] <- l[c(1, 3, 9, 17, 19)]
#'[1] "AM J MED"                     "AM J MED SCI"
#'[3] "AMER J MED"                   "AMER J MED SCI"
#'[5] "AMER J MED TECHN"             "AMER J MED TECHNOL"
#'[7] "AM J MED TECHNOL"             "AM J MED TECHN"
#'[9] "AM J MEDICINE"                "AM J MEDICAL TECHNOL"
#'[11] "AM J MED TECHNOLOGY"          "AM J MED TECH JUL"
#'[13] "AMERICAN JOURNAL OF MEDICINE" "AMER JOUR MED TECHNOL"
#'[15] "AM J MED ACYCLOVIR S"         "AM J MED S"
#'[17] "AM J MED GENET"               "AM J MED 0728"
#'[19] "AM J MED S1B"                 "AM JL MED"
#'[21] "AM J MED S2A"                 "AM J MED GENET SUPPL"
#'[23] "AM J MED S5C"                 "AM J MED HYG"
#'[25] "AM J MED ELECTRON" 
ol[[4]] <- l[c(1, 3, 9, 13, 18, 20)]

# 5."AM J PATHOL": AMERICAN JOURNAL OF PATHOLOGY
l <- c()
for(i in 1:length(jUniq)){
  #t0 <- str_match(jUniq[i], 'AM[:alpha:]*[:space:]+J[:alpha:]*[:space:]+PAT')
  t0 <- str_match(jUniq[i], 'AM[:alpha:]*[:space:]+J[:alpha:]*[:space:]+O*F*[:space:]*PAT')
  if(!is.na(t0[1])){
    l <- c(l, jUniq[i])
  }
}
l
#'[1] "AM J PATHOL"          "AMER J PATH"          "AMER JOUR PATHOL" 
#'[4] "AM J PATH"            "AM J PATHOLOGY"       "AMER J PATHOL" 
#'[7] "AMER JOUR PATH"       "THYMECTOMY AM J PATH"
#ol[[5]] <- l[1:7]
#'[1] "AM J PATHOL"                   "AMER J PATH"
#'[3] "AMER JOUR PATHOL"              "AM J PATH"
#'[5] "AM J PATHOLOGY"                "AMER J PATHOL"
#'[7] "AMERICAN JOURNAL OF PATHOLOGY" "AMER JOUR PATH"
#'[9] "THYMECTOMY AM J PATH" 
ol[[5]] <- l[1:8]

# 6."AM J TROP MED HYG": AMERICAN JOURNAL OF TROPICAL MEDICINE AND HYGIENE
l <- c()
for(i in 1:length(jUniq)){
  "t0 <- str_match(jUniq[i], 'A[:alpha:]*[:space:]+J[:alpha:]*[:space:]+T[:alpha:]*[:space:]+M.*H')
  if(!is.na(t0[1])){
    l <- c(l, jUniq[i])
  }"
  t <- str_match(jUniq[i], '(.*)(A[:alpha:]*[:space:]+J.*T[:alpha:]*[:space:]+M.*H)')
  if(!is.na(t[1])){
    if(t[2] == ''){
      l <- c(l, jUniq[i])
    }
  }
}
l
#'[1] "AM J TROP MED HYG"  "AMER J TROP MED HYG" 
#'[3] "AMER JOUR TROP MED AND HYG"  "SE ASIAN J TROP MED PUB HLTH"
#'[5] "SOUTHEAST ASIAN J TROP MED PUB HLTH" 
#'[6] "SOUTHEAST ASIAN J TROP MED PUBLIC HEALTH"
#'[7] "AM J TROPICAL MED HY"  "AM J TROP MED HYG S" 
#'[9] "AM J TROP MED HY S79"  "AM J TROP MED H S104" 
#'[11] "JAP J TROP MED HYG"
ol[[6]] <- l[c(1:3, 7:10)]
# final result of round 2: no change

# 7."ANN INTERN MED": ANNALS OF INTERNAL MEDICINE
# This should belong to the Internal Medicine field?
l <- c()
for(i in 1:length(jUniq)){
  t0 <- str_match(jUniq[i], '(.*)(AN[:alpha:]*[:space:]+IN[:alpha:]*[:space:]+M[:alpha:]*)')
  t0 <- str_match(jUniq[i], '(.*)(AN[:alpha:]*[:space:]+O*F*[:space:]*IN[:alpha:]*[:space:]+M[:alpha:]*)')
  if(!is.na(t0[1])){
    if(t0[2] == ''){
      l <- c(l, jUniq[i])
    }
  }
}
l
#'[1] "ANN INTERN MED"       "ANN INT MED"          "ANN INTER MED" 
#'[4] "ANN INTERNAL MED"     "AN INST MED TROP"     "ANNALS INTERNAL MEDI"
#'[7] "ANN INTERN MED CHICA" "ANN INTERXN MED"      "ANN INTERN MED 2" 
#'[10] "ANN INTERN MED 1"     "AN INTERN MED"        "ANN INTERNAL M" 
#ol[[7]] <- l[c(1:4, 6, 8:12)]
#'[1] "ANN INTERN MED"              "ANN INT MED"
#'[3] "ANN INTER MED"               "ANN INTERNAL MED"
#'[5] "AN INST MED TROP"            "ANNALS INTERNAL MEDI"
#'[7] "ANN INTERN MED CHICA"        "ANNALS OF INTERNAL MEDICINE"
#'[9] "ANN INTERXN MED"             "ANN INTERN MED 2"
#'[11] "ANN INTERN MED 1"            "AN INTERN MED"
#'[13] "ANN INTERNAL M" 
ol[[7]] <- l[c(1:4, 6, 8:13)]

# 8."ARCH PATHOL LAB MED": ARCHIVES OF PATHOLOGY & LABORATORY MEDICINE
l <- c()
for(i in 1:length(jUniq)){
  #t0 <- str_match(jUniq[i], 'A[:alpha:]*[:space:]+P[:alpha:]*[:space:]+L[:alpha:]*[:space:]+M')
  t <- str_match(jUniq[i], '(.*)(A[:alpha:]*[:space:]+O*F*[:space:]*P.*L[:alpha:]*[:space:]+M)')
  if(!is.na(t[1])){
    if(t[2] == ''){
      l <- c(l, jUniq[i])
    }
  }
}
l
#'"ARCH PATHOL LAB MED" "ARCH PATH LAB MED"   "ARCH PATHO LAB MED" 
ol[[8]] <- l
# final results of round 2 no change

# 9."CANCER": CANCER
l <- c()
for(i in 1:length(jUniq)){
  t <- str_match(jUniq[i], '(.*)(CAN[:alpha:]*[:space:]*[:digit:]*)(.*)')
  if(!is.na(t[1])){
    if(t[2] == '' & t[4] == ''){
      l <- c(l, jUniq[i])
    }
  }
}
l
#'"CANCER"      "CANCER 1"    "CANDIDIASIS"
ol[[9]] <- l[1:2]

# 10."CANCER RES": CANCER RESEARCH
l <- c()
for(i in 1:length(jUniq)){
  t <- str_match(jUniq[i], '(.*)(CAN[:alpha:]*[:space:]+R[:alpha:]*)')
  if(!is.na(t[1])){
    if(t[2] == ''){
      l <- c(l, jUniq[i])
    }
  }
}
l
#'[1] "CANCER RES"            "CANC RES"              "CANCER RES S" 
#'[4] "CANCER RADIOTHERAPY"   "CANCER RESEARCH"       "CANCERCHEMOTHERAP REP"
#'[7] "CAN REV SOCIOL ANTHR"  "CANCER RES 1"          "CANCER RES CLIN ONCO"
#'[10] "CAN REV SOC ANTHROP"   "CANCER RES CELL BIOL"  "CAN RES"
#'[13] "CANCER REVIEWS"        "CANCER RES REP"        "CANCER RES REPORTS"
#'[16] "CANCER REV"            "CANCER RES S45"
ol[[10]] <- l[c(1:3, 5, 8, 12, 17)]

# 11."CELL": CELL
l <- c()
for(i in 1:length(jUniq)){
  t <- str_match(jUniq[i], '(.*)(CEL[:alpha:]*[:space:]*[:digit:]*)(.*)')
  if(!is.na(t[1])){
    if(t[2] == '' & t[4] == ''){
      l <- c(l, jUniq[i])
    }
  }
}
l
#'"CELL"  "CELLULARPATHOLOGIC"  "CELLL"   
ol[[11]] <- l[c(1, 3)]

# 12."GENE": GENE
l <- c()
for(i in 1:length(jUniq)){
  t <- str_match(jUniq[i], '(.*)(GEN[:alpha:]*[:space:]*[:digit:]*)(.*)')
  if(!is.na(t[1])){
    if(t[2] == '' & t[4] == ''){
      l <- c(l, jUniq[i])
    }
  }
}
l
#'[1] "GEN"         "GENETICS"    "GENEESKUNDE" "GENETICA"    "GENETIKA"    "GENE"       
#'[7] "GENET"       "GENOMICS"    "GENES"       "GENUS"       "GENITOURINA" "GENITOURIN" 
ol[[12]] <- l[c(1, 6)]

# 13."HUM PATHOL": HUMAN PATHOLOGY
l <- c()
for(i in 1:length(jUniq)){
  t <- str_match(jUniq[i], 'HU[:alpha:]*[:space:]+PA')
  if(!is.na(t[1])){
    l <- c(l, jUniq[i])
  }
}
l
#'[1] "GENERAL HUMAN PATHOL" "HUMAN PATHOLOGY"      "HUM PATHOL" 
#'[4] "HUMAN PATH"           "HUM PATH"             "HUMAN PATHOL"
#'[7] "15E C INT RHUM PAR"   "HUMAN PANCREATIC POL" "ATLAS HUMAN PARASITO"
#'[10] "2ND INT C HUM PAP SQ"
ol[[13]] <- l[2:6]

# 14."INT J CANCER": INTERNATIONAL JOURNAL OF CANCER
l <- c()
for(i in 1:length(jUniq)){
  #t <- str_match(jUniq[i], 'IN[:alpha:]*[:space:]+J[:alpha:]*[:space:]+CA')
  t <- str_match(jUniq[i], 'IN[:alpha:]*[:space:]+J[:alpha:]*[:space:]+O*F*[:space:]*CA')
  if(!is.na(t[1])){
    l <- c(l, jUniq[i])
  }
}
l
#'[1] "INT J CANCER"     "INDIAN J CANCER"    "INT J CANC"         "IND J CANCER"
#'[5] "INT J CANCER NOV"   "INT J CANCE"        "INT J CAN"          "INT J CARDIOL"
#'[9] "CHIN J CANCER"      "CHINESE J CANCER"   "INT J CANCER SUPPL" "INT J CAN CER" 
#ol[[14]] <- l[c(1, 3, 4, 6, 7, 12)]
#'[1] "INT J CANCER"                    "INDIAN J CANCER"
#'[3] "INTERNATIONAL JOURNAL OF CANCER" "INDIAN JOURNAL OF CANCER"
#'[5] "INT J CANC"                      "IND J CANCER"
#'[7] "INT J CANCER NOV"                "INT J CANCE"
#'[9] "INT J CAN"                       "INT J CARDIOL"
#'[11] "CHIN J CANCER"                   "CHINESE J CANCER"
#'[13] "INT J CANCER SUPPL"              "INT J CAN CER" 
ol[[14]] <- l[c(1, 3, 5:9, 13, 14)]

#'15."J ACQ IMMUN DEF SYND": JOURNAL OF ACQUIRED IMMUNE DEFICIENCY SYNDROMES 
#'+ JOURNAL OF ACQUIRED IMMUNE DEFICIENCY SYNDROMES AND HUMAN RETROVIROLOGY
l <- c()
for(i in 1:length(jUniq)){
  #t <- str_match(jUniq[i], 'J[:alpha:]*[:space:]+A[:alpha:]*[:space:]+I[:alpha:]*[:space:]+D[:alpha:]*[:space:]+S')
  t <- str_match(jUniq[i], 'J.*A[:alpha:]*[:space:]+I[:alpha:]*[:space:]+D[:alpha:]*[:space:]+S')
  if(!is.na(t[1])){
    l <- c(l, jUniq[i])
  }
}
l
# round 1
#' "J ACQ IMMUN DEF SYND"
# round 2
#'[1] "J ACQ IMMUN DEF SYND"
#'[2] "JOURNAL OF ACQUIRED IMMUNE DEFICIENCY SYNDROMES"
ol[[15]] <- l

# 16."J CLIN INVEST": JOURNAL OF CLINICAL INVESTIGATION
l <- c()
for(i in 1:length(jUniq)){
  t <- str_match(jUniq[i], '(.*)(J[:alpha:]*[:space:]+.*CL[:alpha:]*[:space:]+IN)')
  if(!is.na(t[1])){
    if(t[2] == ''){
      l <- c(l, jUniq[i])
    }
  }
}
l
#'[1] "J CLIN INVEST"                     "JOUR CLIN INVEST"
#'[3] "JOURNAL OF CLINICAL INVESTIGATION" "J SCAND CLIN INV S21"
#'[5] "J CLINICAL INVESTIGA"              "J CLIN INVESTIGATION"
#'[7] "J CLIN INVE" 
ol[[16]] <- l[c(1:3, 5:7)]

# 17."J INFECT DIS": JOURNAL OF INFECTIOUS DISEASES
l <- c()
for(i in 1:length(jUniq)){
  t <- str_match(jUniq[i], '(.*)(J[:alpha:]*[:space:]+.*IN[:alpha:]*[:space:]+DI)')
  if(!is.na(t[1])){
    if(t[2] == ''){
      l <- c(l, jUniq[i])
    }
  }
}
l
#'[1] "J INFECT DIS"  "JOUR INFECT DIS"  "J INFECT DIS S115"  "J INF DIS"
#'[5] "JOURNAL OF INFECTIOUS DISEASES" "J CLIN DIS"  "J INFECT DIS S"
#'[8] "J INFECT DISEASES"  "J INFECTIOUS DISEASE"   "J INFEC DIS" 
#'[11] "J INFEC DISEASES"  "J INFECT DIS SA"  "J INFECT DIS S774"
#'[14] "J INFECTIOUS DISEA S"  "J INVEST DIS"  "J INFECT DIS SUP"
#'[17] "J INFECT DIS S52"  "J PEDIATR INFECT DIS"  "J SPEECH HEARING DIS"
#'[20] "J INFECTIVE DISEASES"  "J INFECT DIS A"  "J INFECT DI"
#'[23] "J INFECT DIS S5"  "J INFECT DIS S6"
ol[[17]] <- l[c(1:5, 7:11, 13:17, 20, 22:24)]

# 18."J NATL CANCER I": JOURNAL OF THE NATIONAL CANCER INSTITUTE + JNCI-JOURNAL OF THE NATIONAL CANCER INSTITUTE
l <- c()
for(i in 1:length(jUniq)){
  t <- str_match(jUniq[i], '(JNCI)|(J[:alpha:]*[:space:]+.*NA[:alpha:]*[:space:]+C[:alpha:]*[:space:]+I)')
  if(!is.na(t[1])){
    l <- c(l, jUniq[i])
  }
}
l
#'[1] "JNCI-J NATL CANCER I"  "J NATL CANCER I"  "J NATN CANCER I"
#'[4] "J NAT CANCER I"  "JOUR NATL CANCER INST"  "J NAT CANCER INST"
#'[7] "J NATIONAL CANCER I"  "JOURNAL OF THE NATIONAL CANCER INSTITUTE"
#'[9] "J NAT CANC I"  "J NAT CANCER I MONOG"  "J NATL CANCER INST"
#'[12] "JNCI MAY"  "J NATL CANCER I MONO"  "J NATL CANC I MONOGR"
#'[15] "JNCI J NATL CANCER I"  "J NAT CANCER I WASH"  "JNCI"
#'[18] "J NATL CAN I"  "J NATL CANC I" 
ol[[18]] <- l[c(1:9, 11, 12, 15, 17:19)]

# 19."LAB INVEST": LABORATORY INVESTIGATION
l <- c()
for(i in 1:length(jUniq)){
  t <- str_match(jUniq[i], '(.*)(LA[:alpha:]*[:space:]+I)')
  if(!is.na(t[1])){
    if(t[2] == ''){
      l <- c(l, jUniq[i])
    }
  }
}
l
#'[1] "LAB INVEST"  "LATENT INFECTION PRE"  "LABOR INVEST"
#'[4] "LAV IST ANAT ISTOL PATOL UNTV STUDI PERUGIA"  "LABORATORY INVESTIGATION"
#'[6] "LAV IST ANAT ISTOL PATOL UNIV STUDI PERUGIA"  "LABORATORY INVESTIGA"
#'[8] "LAB INVEST ABST"  "LABORATORIO INMUNOLO"  "LAB INVEST A" 
#'[11] "LAB INV"  "LASERS IN SURGERY AND MEDICINE"
ol[[19]] <- l[c(1, 3, 5, 7, 11)]

# 20."NUCLEIC ACIDS RES": NUCLEIC ACIDS RESEARCH
l <- c()
for(i in 1:length(jUniq)){
  t <- str_match(jUniq[i], '(.*)(N[:alpha:]*[:space:]+A[:alpha:]*[:space:]+R)')
  if(!is.na(t[1])){
    if(t[2] == ''){
      l <- c(l, jUniq[i])
    }
  }
}
l
#'[1] "NUCLEIC ACIDS RES"      "NUCL ACID RES"          "NUCLEIC ACID RES"
#'[4] "NUCL ACIDS RES"         "NUCLEIC ACIDS RES S"    "NUC ACID RES"
#'[7] "NUCL ACIDS RES SPECI"   "NUCL ACIDS RES S SER"   "NUC ACIDS RES"
#'[10] "NOVEL ADP RIBOSYLATI"   "NUCLEIC ACIDS RESEARCH"
ol[[20]] <- l[c(1:9, 11)]

# 21."RADIOLOGY": RADIOLOGY
l <- c()
for(i in 1:length(jUniq)){
  t <- str_match(jUniq[i], '(.*)(RADIO[:alpha:]*[:space:]*[:digit:]*)(.*)')
  if(!is.na(t[1])){
    if(t[2] == '' & t[4] == ''){
      l <- c(l, jUniq[i])
    }
  }
}
l
#'[1] "RADIOLOGY"            "RADIOBIOLOGIYA"       "RADIOLOGE"
#'[4] "RADIOL"               "RADIOPHARMACEUTICALS" "RADIOGRAPHY"
#'[7] "RADIOBIOLOGIJA"       "RADIOGRAPHICS"
ol[[21]] <- l[c(1, 3, 4)]

# 22."REV INFECT DIS": REVIEWS OF INFECTIOUS DISEASES
l <- c()
for(i in 1:length(jUniq)){
  t <- str_match(jUniq[i], '(.*)(RE[:alpha:]*.*[:space:]+I[:alpha:]*[:space:]+D)')
  if(!is.na(t[1])){
    if(t[2] == ''){
      l <- c(l, jUniq[i])
    }
  }
}
l
#'[1] "REVISTA DO INSTITUTO DE MEDICINA TROPICAL DE SAO PAULO"
#'[2] "REVISTA IBERICA DE PARASITOLOGIA"  "REV INFECT DIS"
#'[4] "REV INFECTIOUS DISEA"  "REV INF DIS"  "REV J INFECT DIS"
#'[7] "REV INFECT DIS S"  "REV INFECTIOUS DIS S"
#'[9] "REVIEWS OF INFECTIOUS DISEASES"  "REV INFECT DIS S3"
#'[11] "REV INFECT DIS S1"
#'[12] "REVUE SCIENTIFIQUE ET TECHNIQUE OFFICE INTERNATIONAL DES EPIZOOTIES"
#'[13] "REV INFECT DIS S5"  "REV INFECT DIS S4"  "REV INFECT DIS SS1" 
#'[16] "REV INFECTIOUS DI S1"  "REV INFECT DIS S7"  "REV INFECT DIS S2"
#'[19] "REPORT IN DEPTH SURV"
ol[[22]] <- l[c(3:5, 7:11, 13:18)]

# save output
names(ol) <- ojnls
save(vl, il, bl, im, ol, file = 'data_v12/other/journal_variants.rda')
