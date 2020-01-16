# EMILY CHISHOLM
# JANUARY 15TH 2020

# Nutrient Project
# Supervisor: Catherine Johnson
# Collaborators: Zhi Ping Mei, Benoit Casault


# Goal: To remove erroneous data points from data frame used to perform nutrient
# analysis by Dan Reed, project being continued by ZPM. Data frame contains
# historical nutrient, temperature and slainity data from BIO.

# Process: Idea is to compare data frame from ZPM to Emily's staging tables
# containing QC'd BioChem Reboot data by sample ID/ event ID / cruise number.
# Pull quality flags from staging tables into data frame to allow removal of
# erroneous data points.


library(RODBC)

library(plyr)

library(dplyr)

library(stringr)
#load biochem login data

load('C:/login/biochemlogin.RData')

#connect to biochem

# Create a database connection.
# con = odbcConnect( dsn="PTRAN", uid=biochem$user, pwd=biochem$password, believeNRows=F)

# if(con != -1){
#  cat('Connection successful! \n')
# }

# load in data files


load('N_ts_PO4_gt_4.RData')

load('N_ts_sal_gt_40.RData')

load('N_ts_temp_gt_30.RData')


# explore data files


#combine into one data frame

data <- join_all(list(N_ts1, N_ts2, N_ts3), type = 'full')


unique(data$TS_ID)


# do cruise ID's match what I have wuality flags for? are there any missing

# HUD2011004 - AZMP
# HUD2012042 - AZMP
# BCD2013667 - FIXED STATION, NOT COMPLETED
# HUD2013013 - AZMP, OUT OF RANGE
# 186476 - ?
# NA - ?
# PAR2000002 - AZMP
# 186477 - ?


rel_cruises <- c('HUD2011004', 'HUD2012042', 'PAR2000002')

val <- list()
for (i in 1:length(rel_cruises)){
val[[i]] <- stringr::str_match(data$TS_ID, pattern = rel_cruises[i])

names(val) <- rel_cruises
}

length(which(!is.na(val$HUD2011004)))

length(which(!is.na(val$HUD2012042)))

length(which(!is.na(val$PAR2000002)))

# total 13 / 43 samples can be accounted for

data[which(!is.na(val$HUD2011004)),]

data[which(!is.na(val$HUD2012042)),]

data[which(!is.na(val$PAR2000002)),]


# find BCD files for relevant cruises

bcd <- list()
for (i in 1:length(rel_cruises)){
  
  bcd_fn <- paste0(rel_cruises[i], '_BCD_flagged.csv')
  
  full_fn <- list.files('E:/BioChem QC/azmp_data/', pattern = bcd_fn, recursive = TRUE, full.names = TRUE)
  
  bcd[[i]] <- read.csv(full_fn)
  
}
names(bcd) <- rel_cruises
     
# save RData for working at home
# save(bcd, file = 'BCD_data.RData')

# define relevant data types in BCD
# caution may need more depending on bcd file
rel_dattypes <- c("SiO4_Tech_F", "NO2NO3_Tech_F", "PO4_Tech_F", "Temp_CTD_1968", "Salinity_CTD")

#match samples
dat_cr <- vector()
dat_cr$HUD2011004 <- data[which(!is.na(val$HUD2011004)),]
dat_cr$HUD2012042 <- data[which(!is.na(val$HUD2012042)),]
dat_cr$PAR2000002 <- data[which(!is.na(val$PAR2000002)),]

HUD11004_event <- unique(dat_cr$HUD2011004$event_ID)
HUD12042_event <- unique(dat_cr$HUD2012042$event_ID)
PAR00002_event <- unique(dat_cr$PAR2000002$event_ID)

HUD11004_dat <- bcd$HUD2011004 %>%
  dplyr::filter(., bcd$HUD2011004$EVENT_COLLECTOR_EVENT_ID == HUD11004_event )

HUD11004_dat <- HUD11004_dat %>%
  dplyr::filter(., DIS_HEADER_START_DEPTH %in% unique(dat_cr$HUD2011004$depth) ) %>%
  dplyr::filter(., DATA_TYPE_METHOD %in% rel_dattypes)


# merge data QC flags with ZPM data frame

# Replace invalid values with NA

HUD11004_dat_inv <- HUD11004_dat %>%
  dplyr::filter(., DIS_DETAIL_DATA_QC_CODE == 4)

temp_sal <- c("Salinity_CTD", "Temp_CTD_1968")


for(i in 1:length(HUD11004_dat_inv)){
  
  dat_line <- HUD11004_dat_inv[i,]
  
  if(dat_line$DATA_TYPE_METHOD == 'Salinity_CTD'){
    
    N_ts$salinity[N_ts$mission == dat_line$MISSION_DESCRIPTOR &&
                    N_ts$event_ID == dat_line$EVENT_COLLECTOR_EVENT_ID &&
                    N_ts$depth == dat_line$DIS_HEADER_START_DEPTH] <- NA
    
  }
  
  if(dat_line$DATA_TYPE_METHOD == 'Temp_CTD_1968'){
    
    N_ts$temperature[N_ts$mission == dat_line$MISSION_DESCRIPTOR &&
                    N_ts$event_ID == dat_line$EVENT_COLLECTOR_EVENT_ID &&
                    N_ts$depth == dat_line$DIS_HEADER_START_DEPTH] <- NA
    
  }
  
  if(!(dat_line$DATA_TYPE_METHOD %in% temp_sal)){
  
    dat_line <- dat_line %>%
      dplyr::mutate(.,
                    species = ifelse(
                      DATA_TYPE_METHOD == "NO2NO3_Tech_F",
                      'nitrate',
                      ifelse(
                        DATA_TYPE_METHOD == "SiO4_Tech_F",
                        "silicate",
                        ifelse(DATA_TYPE_METHOD == "PO4_Tech_F", "phosphate", NA)
                      )
                    ))
    
    N_ts <- N_ts %>%
      dplyr::mutate(
        .,
        QC_code = case_when(
            mission == dat_line$MISSION_DESCRIPTOR &
            event_ID == dat_line$EVENT_COLLECTOR_EVENT_ID &
            depth == dat_line$DIS_HEADER_START_DEPTH &
            species == dat_line$species 
              ~ dat_line$DIS_DETAIL_DATA_QC_CODE
        )
      )
    
    
    # check 
    
    test <- N_ts %>%
      dplyr::filter(
        .,
        
          mission == dat_line$MISSION_DESCRIPTOR &
            event_ID == dat_line$EVENT_COLLECTOR_EVENT_ID &
            depth == dat_line$DIS_HEADER_START_DEPTH &
            species == dat_line$species 
          
        
      )
    
    
    
  }
  
  
}
