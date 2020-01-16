# E. Chisholm
# January 15th 2020

library(plyr)
library(dplyr)
library(stringr)
library(sp)
# run preliminary rnage check quality control on ZPM nutrient data

load("N_TS.RData")

# initalize QC column

N_ts <- N_ts %>%
  tibble::add_column(., "QC_code" = 0)
# from HL_02

#grep(unique(N_ts$station), pattern = 'HL', value = TRUE)

#find all possible HL02 aliases

#hl2 <- c("HL_02", "HL_02A", "HL_02.6", "SET1=HL2", "HL2", "HL2A", "HL2 set 107", "HL2 Set0", "HL2 - AZMP     S3/3", "HL2 - ZMP STATION2", "HL2 Set 103", "HL2.6")

# hl2_dat <- N_ts %>%
#  dplyr::filter(., station %in% hl2 )


# load in climatology


Petrie <- R.matlab::readMat('Petrie.mat')

P <- as.data.frame(Petrie)

remove(Petrie)

BIO_Q <- R.matlab::readMat('B_stage3_Q_BIO.mat')

nuts <- BIO_Q$test36

nutdf <- data.frame(nuts[[3]])

remove(nuts, BIO_Q)


# check for cruise in staging tables

# Get BCD

# find cruise names

TS_ids <- unique(N_ts$TS_ID)

split_ids <- str_split(TS_ids, pattern = '_' )

cruise_names <- list()
for(i in 1:length(split_ids)){
  cruise_names[[i]] <- split_ids[[i]][2]
}

cruise_names <- unlist(cruise_names)

# check valid cruise names

azmp_path <- 'E:/BioChem QC/azmp_data/'
azmp_cruises <- list.files(azmp_path)

gf_path <- 'E:/BioChem QC/groundfish_data/'
gf_cruises <- list.files(gf_path)


cruise_names_azmp <- unique(cruise_names[cruise_names %in% azmp_cruises])

cruise_names_gf <- unique(cruise_names[cruise_names %in% gf_cruises])

all_cruises <- c(cruise_names_azmp, cruise_names_gf)

# relevant data types for analysis in BCDs
rel_dattypes <- c("SiO4_Tech_F", "NO2NO3_Tech_F", "PO4_Tech_F", "Temp_CTD_1968", "Salinity_CTD")
temp_sal <- c("Salinity_CTD", "Temp_CTD_1968", "Salinity_Sal_PSS")

sumtab_all <- data.frame(mission=character(), method=character(),f0=numeric(), f1=numeric(), f2=numeric(),
                         f3=numeric(), f4=numeric(), f7=numeric(), stringsAsFactors=FALSE)

sumtab_clim <- data.frame(mission=character(), method=character(),f0=numeric(), f1=numeric(), f2=numeric(),
                          f3=numeric(), f4=numeric(), f7=numeric(), stringsAsFactors=FALSE)

sink('BCD_flagging_record.txt')
# compare to BCDs --------------------------------------------------------------------------------------
for(i in 1:length(all_cruises)){
  
  if (all_cruises[i] %in% cruise_names_azmp){
    cat('Comparing, ', all_cruises[i], ', AZMP cruise...  \n')
    program <- 'azmp'
    basepath <- azmp_path
  }else{
    cat('Comparing, ', all_cruises[i], ', Groundfish cruise... \n')
    program <- 'gf'
    basepath <- gf_path
  }
  
  # load BCD file
  
  cruise <- all_cruises[i]
  
  bcd_fn <- list.files(basepath, pattern = paste0(cruise, '_BCD_flagged.csv'), recursive = TRUE, full.names = TRUE)
  
  cat('Loading BCD file, ', bcd_fn, ' ... \n')
  
  bcd <- read.csv(bcd_fn)
  
  # find invalid samples
  
  # temperature and salinity
  
  bcd_inv <- bcd %>%
    dplyr::filter(., DIS_DETAIL_DATA_QC_CODE %in% c(2, 3, 4, 5, 6, 7)) %>%
    dplyr::filter(., DATA_TYPE_METHOD %in% rel_dattypes)%>%
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
  
  cat('Found ', length(bcd_inv$MISSION_DESCRIPTOR), 'invalid records in ', cruise, '. \n')
  
  cat(length(bcd_inv$DIS_DATA_NUM[bcd_inv$DATA_TYPE_METHOD %in% temp_sal]), ' temperature and salinity records to be removed. \n')
  
  cat(length(bcd_inv$DIS_DATA_NUM[!(bcd_inv$DATA_TYPE_METHOD %in% temp_sal)]), ' nutrient records to be flagged. \n')
  
  if (length(bcd_inv$DIS_DATA_NUM) > 0){
  
  # total records to be flagged and removed, print as table
  
  flag_table <- table(bcd_inv$DATA_TYPE_METHOD, as.numeric(bcd_inv$DIS_DETAIL_DATA_QC_CODE))
  
  write.csv(paste0(cruise, '_BCD_flags.csv'), x = flag_table)
  
  
  cat("\n","\n")
  cat(paste("-> Flags sucessfully transfered to BCD file for mission", cruise,"."))
  cat("\n","\n")
  cat("-> Summary of the flags for each parameter:")
  cat("\n","\n")
  print(flag_table)
  
  
  # NEW ADDITION: convert sumtab to dataframe and merge them all together
  
  # convert to dataframe
  b=as.data.frame.matrix(flag_table)
  
  # add f to the name
  names(b)=paste0("f",names(b))
  
  # add column with method and mission
  b$method=row.names(b)
  b$cruise=cruise
  
  # bind them all together
  sumtab_all=rbind.fill(sumtab_all,b)
  
  
  
  for (ii in  1:length(bcd_inv$DIS_DATA_NUM)){
   
      dat_line <- bcd_inv[ii,]
      
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
        
        
        N_ts <- N_ts %>%
          dplyr::mutate(
            .,
            QC_code = case_when(
              mission == dat_line$MISSION_DESCRIPTOR &
                event_ID == dat_line$EVENT_COLLECTOR_EVENT_ID &
                depth == dat_line$DIS_HEADER_START_DEPTH &
                species == dat_line$species 
              ~ as.numeric(dat_line$DIS_DETAIL_DATA_QC_CODE),
              TRUE ~ QC_code
            )
          )
        
        
      }
      
      
  }
  }
  cat('BCD comparison complete for ', cruise, '.\n')
  }
  
# change NA to 0 in summary table for the flags

sumtab_all[is.na(sumtab_all)]=0

# aggregate results for all cruises, each method separately
all_flags=aggregate(.~method,sumtab_all[,2:8],sum)

write.csv(all_flags,"all_flags_summary.csv", row.names=F)

sink()


# perform range check ------------------------------------------------------------------------------------
 sink('range_check_flag_record.txt')

cat('Test 1: Global range check ... \n')

# define global ranges for each parameter
sal_range <- c(0, 45)
temp_range <- c(-100, 100)
nut_range <- c(0, 100)


# check global value

# salinity

sal_range_check <- N_ts %>%
  dplyr::filter(., salinity < sal_range[1] | salinity > sal_range[2])


cat('\n')
cat('\n')



cat(length(sal_range_check$cast_ID), 'salinity points outside of globally possible range! \n')


if (length(sal_range_check$cast_ID) > 0){
N_ts <- N_ts %>%
  dplyr::mutate(
    .,
    salinity = case_when(
      salinity > sal_range[1] | salinity < sal_range[2]
       ~ salinity
    )
  )


cat('Gloablly impossible salinity points set to NA values! \n')
}


# temperature

temp_range_check <- N_ts %>%
  dplyr::filter(., temperature < temp_range[1] | temperature > temp_range[2])


cat('\n')
cat('\n')



cat(length(temp_range_check$cast_ID), 'temperature points outside of globally possible range! \n')

if (length(temp_range_check$cast_ID) > 0 ){
N_ts <- N_ts %>%
  dplyr::mutate(
    .,
    temperature = case_when(
      temperature < temp_range[1] | temperature > temp_range[2]
      ~ NA,
      TRUE ~ .$QC_code
    )
  )


cat('Gloablly impossible temperature points set to NA values! \n')

}

# nutrients

nut_range_check <- N_ts %>%
  dplyr::filter(., concentration < nut_range[1] | concentration > nut_range[2])


cat('\n')
cat('\n')



cat(length(nut_range_check$cast_ID), 'nutrient points outside of globally possible range! \n')

if (length(nut_range_check$cast_ID) > 0 ){
N_ts <- N_ts %>%
  dplyr::mutate(
    .,
    QC_code = case_when(
      N_ts$concentration < nut_range[1] | N_ts$concentration > nut_range[2]
      ~ 4,
      TRUE ~ .$QC_code
    )
  )


cat('Gloablly impossible nutrient points given QC code of 4 (erroneous)! \n')

}


 sink()
# check climatology values ---------------------------------------------------------------------------------

 sink('climatology_flag_record.txt')
cat('Comparing values to Scotian Shelf climatolgoy (by mission) ... \n')


for (i in 1:length(unique(N_ts$mission))){
# refine to just cruise data
nts <- N_ts[N_ts$mission == unique(N_ts$mission)[i],]

cat('Comparing data to climatology limits for ', unique(N_ts$mission)[i], '\n')

for (ii in 1:length(nts$cast_ID)){
  
# find which box each geographical point is within
  
  box_check <- list()
for(j in 1:length(P)){
  box_check[[j]] <- sp::point.in.polygon(nts$longitude[[ii]], nts$latitude[[ii]], P[[j]]$lon, P[[j]]$lat)
}
  box_check <- unlist(box_check)

  climatology_box <- which(box_check == 1)
  
  if (length(climatology_box) == 0){
    cat('Data point outside of known climatology! Not able to check valid range. \n')
  }else{
  nts_line <- nts[ii, ]
  # compare temp values to max and min in box
  
  # compare salinity value to max and min inside box
  
 

  ts_dat <-  nts_line %>%
    dplyr::filter(.,
                    nts_line$temperature < min(P[[climatology_box]]$temp.min) | 
                    nts_line$temperature > max(P[[climatology_box]]$temp.max) |
                    nts_line$salinity < min(P[[climatology_box]]$psal.min) |
                    nts_line$salinity > max(P[[climatology_box]]$psal.max)
  
                    )
  
  # print ts_dat
  #if ts dat is out of range
  
  if(length(ts_dat$cast_ID) > 1){
    cat(ts_dat)
    cat('\n')
    cat('\n')
    
    cat('Temperature or Salinity value found out of climatology range! Flagged with 5 or 6. \n')
   
    # will leave temperature and salinity unless outside of bounds in which case will create Na values
    nts_line <- nts_line %>%
      dplyr::mutate(.,
                    QC_code =
                      case_when(
                        QC_code == 0 &
                        temperature < min(P[[climatology_box]]$temp.min, na.rm = TRUE) | temperature > max(P[[climatology_box]]$temp.max, na.rm = TRUE) 
                        ~ 5, 
                        TRUE ~ as.numeric(QC_code)
                      )) %>%
      dplyr::mutate(., QC_code = 
                      case_when(
                        QC_code == 0 &
                        salinity < min(P[[climatology_box]]$psal.min, na.rm = TRUE) | salinity > max(P[[climatology_box]]$psal.max, na.rm = TRUE)
                        ~ 6,
                        TRUE ~ as.numeric(QC_code)
                      ))
    
    
  }
  

  
# compare nutrient values ot max and min inside box
  
  
  
  # find which box each geographical point is within
  
  box_check_nuts <- list()
  for(j in 1:length(nutdf)){
    box_check_nuts[[j]] <- sp::point.in.polygon(nts$longitude[[ii]], nts$latitude[[ii]], nutdf[[j]]$lon, nutdf[[j]]$lat)
  }
  box_check_nuts <- unlist(box_check_nuts)
  
  climatology_box_nuts <- which(box_check_nuts == 1)
  
  
  nitr_check <- nts_line %>%
    dplyr::filter(.,
                
                      species == 'nitrate' &
                        concentration < min(nutdf[[climatology_box_nuts]]$ntrz.min) | concentration > max(nutdf[[climatology_box_nuts]]$ntrz.max)
    )
  
  phos_check <- nts_line %>%
    dplyr::filter(.,
  
                      species == 'phosphate' &
                        concentration < min(nutdf[[climatology_box_nuts]]$phos.min) | concentration > max(nutdf[[climatology_box_nuts]]$phos.max)
    )
  
  slca_check <- nts_line %>%
    dplyr::filter(.,
                      species == 'silicate' &
                        concentration < min(nutdf[[climatology_box_nuts]]$slca.min) | concentration > max(nutdf[[climatology_box_nuts]]$slca.max)
    )
                     
  if(length(nitr_check$cast_ID) >1){
    cat('Nitrate value found outside of climatology range! Flagged as 2 \n')
    cat(nitr_check)
    cat('\n')
    cat('\n')
  }
  
  if(length(phos_check$cast_ID) >1){
    cat('Phosphate value found outside of climatology range! Flagged as 2 \n')
    cat(phos_check)
    cat('\n')
    cat('\n')
  }
  
  if(length(slca_check$cast_ID) >1){
    cat('Silicate value found outside of climatology range! Flagged as 2 \n')
    cat(slca_check)
    cat('\n')
    cat('\n')
  }
  
  nts_line <- nts_line %>%
    dplyr::mutate(.,
                  QC_code = 
                    case_when(
                      QC_code %in% c(0, 5, 6) &
                      species == 'nitrate' &
                        concentration < min(nutdf[[climatology_box_nuts]]$ntrz.min, na.rm = TRUE) |
                        concentration > max(nutdf[[climatology_box_nuts]]$ntrz.max, na.rm = TRUE)
                        ~ 2,
                      QC_code %in% c(0, 5, 6) &
                      species == 'phosphate' &
                        concentration < min(nutdf[[climatology_box_nuts]]$phos.min, na.rm = TRUE) |
                        concentration > max(nutdf[[climatology_box_nuts]]$phos.max, na.rm = TRUE)
                        ~ 2,
                      QC_code %in% c(0, 5, 6) &
                      species == 'silicate' &
                        concentration < min(nutdf[[climatology_box_nuts]]$slca.min, na.rm = TRUE) |
                        concentration > max(nutdf[[climatology_box_nuts]]$slca.max, na.rm = TRUE)
                        ~ 2,
                      TRUE ~ QC_code
                      )
    )
  # replace QC line
  nts[ii, ] <- nts_line
 
} # end loop if point is outside climatology
  

}

# summarize flags

flag_tab <- table(nts$species, nts$QC_code)

write.csv(flag_tab, file = paste0(unique(nts$mission), '_clim_flags.csv'))

print(flag_tab)

# NEW ADDITION: convert sumtab to dataframe and merge them all together

# convert to dataframe
b=as.data.frame.matrix(flag_tab)

# add f to the name
names(b)=paste0("f",names(b))

# add column with method and mission
b$method=row.names(b)
b$mission=unique(nts$mission)

# bind them all together
sumtab_clim=rbind.fill(sumtab_clim,b)


# replace QC'd cruise data
N_ts[N_ts$mission == unique(N_ts$mission)[i],] <- nts


}



# change NA to 0 in summary table for the flags

sumtab_clim[is.na(sumtab_clim)]=0

# aggregate results for all cruises, each method separately
clim_flags=aggregate(.~method,sumtab_clim[,2:8],sum)

write.csv(clim_flags,"climatology_flags_summary.csv", row.names=F)

sink()


cat(' -> All flags completed for all missions!')
cat('\n')
cat('\n')

cat(' -> saving new data frame!')

N_ts_qc <- N_ts
save(N_ts_qc, file = 'N_ts_qc.RData')
