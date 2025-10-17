### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# By Marcus Westerberg
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
the_wd <- "S:\\SCREESCO\\Studier\\Year 0 - 2023\\"
setwd(the_wd)

data_output <- paste0(the_wd,"Data\\")

# packages
require(tidyverse)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# Define adverse events in list by group and then search for them
ae_list <- list("Septicemia"=c("A40","A41"),
                "IschemicHeartDisease"=paste0("I",20:25),
                "PulmonaryEmbolism"=c("I26"),
                "AcuteEndocarditis"=c("I33"),
                "CardiacArrest"=c("I46"),
                "CerebralInfarction"=c("I63"),
                "PeripheralArteryEmbolism"=c("I74"),
                "VenousThromboembolism"=c("I81","I82"),
                "UnspecifiedGastrointestinalBleeding"=c("K922"),
                "SplenicInjury"=c("S360"),
                "ColonicInjury"=c("S365"),
                "RectalInjury"=c("S366"),
                "BleedingIatrogenic"=c("T810"),
                "Perforation"=c("T812"),
                "AllergicReactionOfDrugs"=c("T886"))

ae_all <- as.list(unique(unlist(ae_list)))
names(ae_all) <- unlist(ae_list)

ae_list$Cardiovascular <- unlist(ae_list[2:8])
ae_list$Gastrointestinal <- unlist(ae_list[c(9:14)])
ae_list$Other <- unlist(ae_list[c(1,15)])
ae_list$ThromboembolicDisease <-  unlist(ae_list[c(3,6,7,8)])
ae_list <- append(ae_list,ae_all)

ae_list$AnyAE  <- unique(unlist(ae_list))
ae_list$AnyAECG  <- unique(unlist(ae_list[c("Cardiovascular","Gastrointestinal")]))



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Read/load data
source(".\\Script\\DataManagement_LoadData.R",encoding = "UTF-8")





# # # # # # # # # # # # # # # # Exclusion of two individuals # # # # # # # # # # # # # # # # # # # # 
table(df_randomisering$rand_arm,df_randomisering$rand_year %in% 1954:1956)
table(df_randomisering$rand_arm)

# table(df_randomisering$nr,df_randomisering$rand_arm)
D <- df_randomisering %>% 
  filter(nr == 1) 
dim(D)
table(D$rand_arm)
D$fodelsedatum <- as.Date(D$fodelsedatum,origin="1970-01-01")
# # # # # # # # # # # # # # # # Exclusion of two individuals # # # # # # # # # # # # # # # # # # # # 


### ### ### 
# Birth country
length(unique(BirthCountry$LopNr))

D <- D %>%
  left_join(BirthCountry %>% select(LopNr,BornSwed),
            by="LopNr")

### ### ### 



###
# Cause of death and migrations
D <- D %>%
  left_join(CoD,by="LopNr")

D <- D %>%
  left_join(migr %>% select(LopNr,migr_date),by="LopNr")

D$ldof <- as.Date("2020-12-31")
D <- D %>%
  rowwise() %>%
  mutate(LoF=min(DeathDate,migr_date,ldof,na.rm=TRUE),
         Censor=as.numeric(DeathDate %in% LoF)) %>%
  ungroup()
summary(D$LoF)
table(D$Censor)

D$timefu <- as.numeric(D$LoF) - as.numeric(D$rand_date)


###
# Prior CRC
a <- CanReg %>%
  arrange(LopNr,DIADAT) %>%
  filter(CRC) %>%
  group_by(LopNr) %>%
  slice(1) %>%
  ungroup() %>%
  select(LopNr,DIADAT)


prior_crc <- a %>% 
  arrange(LopNr,DIADAT) %>%
  group_by(LopNr) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(D %>% select(LopNr,rand_date),by="LopNr") %>%
  filter(rand_date>DIADAT)

D$prior_crc <- D$LopNr %in% prior_crc$LopNr

# # # # # # # # # # # # # # # # Exclusion of individuals # # # # # # # # # # # # # # # # # # # # 

# First exclude those that died 
table(as.numeric(D$LoF - D$rand_date) <0 ,D$rand_arm)
table(D$prior_crc ,D$rand_arm)

table(as.numeric(D$LoF - D$rand_date) <0 ,D$prior_crc )

table(D$rand_arm)

D <- D %>%
  filter(LoF>=rand_date)

table(D$prior_crc ,D$rand_arm)

table(D$rand_arm )
table(D$rand_arm,D$Invited)
table(D$prior_crc ,D$rand_arm)


D <- D %>%
  filter(!prior_crc)

table(D$rand_arm )

table(D$rand_arm,D$Invited)

# # # # # # # # # # # # # # # # Exclusion of individuals # # # # # # # # # # # # # # # # # # # # 



### ### ### 
# PAR
# Define adverse events in PAR
for(j in 1:length(ae_list)){
  PAR[[paste0("AE_",names(ae_list)[j]) ]] <- FALSE
  PAR[[paste0("AE_hdia_",names(ae_list)[j]) ]] <- FALSE
  for(i in 1:length(ae_list[[j]])){
    tempcode <- ae_list[[j]][i]
    nc <- nchar(tempcode)
    PAR[[paste0("AE_",names(ae_list)[j]) ]] <- PAR[[paste0("AE_",names(ae_list)[j]) ]] | substr(PAR$dia,1,nc) %in% tempcode 
    
    PAR[[paste0("AE_hdia_",names(ae_list)[j]) ]] <- PAR[[paste0("AE_hdia_",names(ae_list)[j]) ]] | substr(PAR$hdia,1,nc) %in% tempcode
  }
  print(table(PAR[[paste0("AE_",names(ae_list)[j]) ]] ) )
}


PAR <- merge(PAR,
             D %>% select(LopNr,rand_date,LoF),
             by="LopNr",
             all.y=FALSE,
             all.x=FALSE)

PAR <- PAR %>%
  filter(AE_AnyAE)


extract_ae_from_par <- function(PAR,
                                D){
  
  for(j in 1:length(ae_list)){
    PAR[[paste0("AE_before_10_",names(ae_list)[j]) ]] <-   PAR[[paste0("AE_",names(ae_list)[j]) ]] & PAR$indatum < PAR$rand_date & PAR$indatum>= (PAR$rand_date-floor(365.25*10))
    PAR[[paste0("AE_hdia_before_10_",names(ae_list)[j]) ]] <-   PAR[[paste0("AE_hdia_",names(ae_list)[j]) ]] & PAR$indatum < PAR$rand_date & PAR$indatum >= (PAR$rand_date-floor(365.25*10))
    
    PAR[[paste0("AE_before_5_",names(ae_list)[j]) ]] <-   PAR[[paste0("AE_",names(ae_list)[j]) ]] & PAR$indatum<PAR$rand_date & PAR$indatum>= (PAR$rand_date-floor(365.25*5))
    PAR[[paste0("AE_hdia_before_5_",names(ae_list)[j]) ]] <-   PAR[[paste0("AE_hdia_",names(ae_list)[j]) ]] & PAR$indatum < PAR$rand_date & PAR$indatum >= (PAR$rand_date-floor(365.25*5))
    
    PAR[[paste0("AE_before_1_",names(ae_list)[j]) ]] <-   PAR[[paste0("AE_",names(ae_list)[j]) ]] & PAR$indatum<PAR$rand_date & PAR$indatum>= (PAR$rand_date-floor(365.25*1))
    PAR[[paste0("AE_hdia_before_1_",names(ae_list)[j]) ]] <-   PAR[[paste0("AE_hdia_",names(ae_list)[j]) ]] & PAR$indatum < PAR$rand_date & PAR$indatum >= (PAR$rand_date-floor(365.25*1))
    
    
    PAR[[paste0("AE_cens_",names(ae_list)[j]) ]] <-   PAR[[paste0("AE_",names(ae_list)[j]) ]] & PAR$indatum >= PAR$rand_date & PAR$indatum <= PAR$LoF
    PAR[[paste0("AE_hdia_cens_",names(ae_list)[j]) ]] <-   PAR[[paste0("AE_hdia_",names(ae_list)[j]) ]] & PAR$indatum >= PAR$rand_date  & PAR$indatum<=PAR$LoF
    
    PAR[[paste0("AE_timefu_",names(ae_list)[j]) ]] <-   as.numeric(PAR[[paste0("AE_cens_",names(ae_list)[j]) ]]*as.numeric(PAR$indatum)  + (1-PAR[[paste0("AE_cens_",names(ae_list)[j]) ]])*as.numeric(PAR$LoF)) 
    PAR[[paste0("AE_hdia_timefu_",names(ae_list)[j]) ]] <-    as.numeric(PAR[[paste0("AE_hdia_cens_",names(ae_list)[j]) ]]*as.numeric(PAR$indatum)  + (1-PAR[[paste0("AE_hdia_cens_",names(ae_list)[j]) ]])*as.numeric(PAR$LoF)) 
  }
  PAR <- PAR %>%
    select(starts_with( c("LopNr","AE") ) & contains(c("LopNr","before","after","timefu","cens") ))
  
  
  cns_bool <- colnames(PAR)[grepl(colnames(PAR),pattern="cens|before|after")]
  cns_time <- colnames(PAR)[grepl(colnames(PAR),pattern="timefu")]
  
  PAR <- PAR %>%
    group_by(LopNr) 
  
  for(j in 1:length(cns_bool)){
    #print(j)
    temp_var <- cns_bool[j]
    
    PAR <- PAR %>% 
      mutate(!!temp_var:= any( !! as.name(temp_var) ) )
  }
  
  for(j in 1:length(cns_time)){
    #print(j)
    temp_var <- cns_time[j]
    
    PAR <- PAR %>% 
      mutate(!!temp_var:= min( !! as.name(temp_var) ) )
  }
  
  
  PAR <- PAR  %>%
    select( contains(c("LopNr","timefu","cens","before") ) ) %>%
    unique() %>%
    ungroup()
  
  print(length(unique(PAR$LopNr)))
  print(nrow(PAR))
  
  
  D <- D %>%
    left_join(PAR ,
              by="LopNr")
  
  cns_bool <- colnames(D)[grepl(colnames(D),pattern="cens")]
  for(j in 1:length(cns_bool)){
    
    temp_var <- cns_bool[j]
    
    D[[temp_var]][is.na(D[[temp_var]])] <- 0
  }
  
  cns_bool <- colnames(D)[grepl(colnames(D),pattern="before")]
  for(j in 1:length(cns_bool)){
    
    temp_var <- cns_bool[j]
    
    D[[temp_var]][is.na(D[[temp_var]])] <- FALSE
  }
  
  D <- D %>%
    group_by(LopNr)
  
  cns_time <- colnames(D)[grepl(colnames(D),pattern="timefu") & !colnames(D) %in% "timefu"]
  for(j in 1:length(cns_time)){
    #print(j)
    temp_var <- cns_time[j]
    
    D <- D %>% 
      mutate(!!temp_var:= min( !! as.name(temp_var) , LoF, na.rm = TRUE)  -  as.numeric(rand_date)) %>% 
      mutate(!!temp_var:= max( !! as.name(temp_var) , 0.5)  )
  }
  
  D <- D %>%
    ungroup()
  
  print(nrow(D))
  print(length(unique(D$LopNr)))

  return(D)
}


D_par <- extract_ae_from_par(PAR=PAR,
                             D=D)

D <- D_par

rm("D_par")

### ### ### 
# CanReg - CRC 
CRC_CanReg <- CanReg %>%
  arrange(LopNr,DIADAT) %>%
  filter(CRC) 

CRC_CanReg <- CRC_CanReg %>%
  select(LopNr,DIADAT,ICDO10,SNOMED3,SNOMEDO10,T,N,M)

CRC_CanReg <- merge(CRC_CanReg,
                D %>% select(LopNr,rand_date,LoF),
                by="LopNr",
                all.x=TRUE,
                all.y=FALSE)
conv2num <- function(x){
  x <- substr(x,2,2)
  
  if(x %in% 0:4){
    x <- as.numeric(x)
  } else if(x %in% "i"){
    x <- -1
  }else{
    x <- -2
  }
  return(x)
}


CRC_CanReg <- CRC_CanReg %>% 
  filter(DIADAT>=rand_date & DIADAT<=LoF) %>%
  
  rowwise() %>%
  mutate(T=conv2num(T)) %>%
  mutate(N=conv2num(N)) %>%
  mutate(M=conv2num(M)) %>%
  ungroup()

CRC_CanReg <- CRC_CanReg %>% 
  
 
  mutate(stage=case_when(T %in% 1:2 & !N %in% c(1:2,-2) & !M  %in% 1 ~ 1,
                         T %in% 3:4 & !N %in% c(1:2,-2) & !M  %in% 1 ~ 2,
                         (  N %in% c(1:2) ) & !M  %in% 1 ~ 3,
                         M  %in% 1 ~ 4) ) %>%
  
  mutate(stage_group=case_when(T %in% 0:4 & N %in% 0 & !M %in% 1 ~ "I-II",
                               N %in% 0 & M %in% 0 ~ "I-II",
                               T %in% c(0:1) & !N %in% 1:2 & M %in% 0 ~ "I-II",
                               N %in% c(1:2) | M %in% 1 ~ "III-IV"
  ) )
table(CRC_CanReg$stage,CRC_CanReg$stage_group,useNA="always")


###
# Process SCRCR:
# use clinical TNM when pathological is missing
# Consider pathological T0 as missing 
SCRCR$Fixed_t <- FALSE
SCRCR$Fixed_n <- FALSE
SCRCR$Fixed_m <- FALSE

for(j in 1:nrow(SCRCR)){
  
  # Check T 
  if(is.na(SCRCR$A3_t[j]) | SCRCR$A3_t[j] %in% c(0,5)){
    if(SCRCR$A1_utfallt[j] %in% c(1,3,4) ){
      SCRCR$A3_t[j] <- SCRCR$A1_utfallt[j]
      SCRCR$Fixed_t[j]<-TRUE
    }
  }
  
  # Check N 
  if(is.na(SCRCR$A3_n[j]) | SCRCR$A3_n[j] %in% c(5)){
    if(SCRCR$A1_utfalln[j] %in% 0:1 ){
      SCRCR$A3_n[j] <- SCRCR$A1_utfalln[j]
      SCRCR$Fixed_n[j]<-TRUE
    }
  }
  
  # Check M 
  if(is.na(SCRCR$A3_m[j]) | SCRCR$A3_m[j] %in% c(2)){
    if(SCRCR$A1_utfallm[j] %in% 0:1 ){
      SCRCR$A3_m[j] <- SCRCR$A1_utfallm[j]
      SCRCR$Fixed_m[j]<-TRUE
    }
  }
}
table(SCRCR$Fixed_t,SCRCR$A1_utfallt,useNA="always")
table(SCRCR$Fixed_n,SCRCR$A1_utfalln,useNA="always")
table(SCRCR$Fixed_m,SCRCR$A1_utfallm,useNA="always")


### 
# Add CRC from SCRCR
Can_SCRCR <- SCRCR %>% 
  rename("T"=A3_t,"N"=A3_n,"M"=A3_m,
         "adeno"=A3_adeno) %>%
  rename(DIADAT=A1_Diagnosdatum) %>%
  left_join(D %>% select(LopNr,rand_date,LoF),
            by="LopNr") %>%
  filter(DIADAT >= rand_date & DIADAT <= LoF)

Can_SCRCR$T[Can_SCRCR$T %in% 5] <- NA

Can_SCRCR$N[Can_SCRCR$N %in% 5] <- NA
Can_SCRCR$N[Can_SCRCR$N %in% 2] <- 1

Can_SCRCR$M[Can_SCRCR$M %in% 2] <- NA # 2=X


Can_SCRCR <- Can_SCRCR %>%

  rowwise() %>%
  mutate(T=ifelse(is.na(T),-2,T)) %>%
  mutate(N=ifelse(is.na(N),-2,N)) %>%
  mutate(M=ifelse(is.na(M),-2,M)) %>%
  ungroup() %>%
  
  mutate(stage=case_when(T %in% 1:2 & !N %in% c(1:2,-2) & !M  %in% 1 ~ 1,
                         T %in% 3:4 & !N %in% c(1:2,-2) & !M  %in% 1 ~ 2,
                         (  N %in% c(1:2) ) & !M  %in% 1 ~ 3,
                         M  %in% 1 ~ 4) ) %>%
  
  mutate(stage_group=case_when(T %in% 0:4 & N %in% 0 & !M %in% 1 ~ "I-II",
                               N %in% 0 & M %in% 0 ~ "I-II",
                               T %in% c(0:1) & !N %in% 1:2 & M %in% 0 ~ "I-II",
                               N %in% c(1:2) | M %in% 1 ~ "III-IV"
  ) )


Can_SCRCR$ICDO10 <- NA
Can_SCRCR$SNOMED3  <- NA
Can_SCRCR$SNOMEDO10  <- NA


Can_SCRCR <- Can_SCRCR %>%
  select(-starts_with("Fixed")) %>%
  select(-starts_with("A1"))

CRCs <- rbind(CRC_CanReg %>% 
                  mutate(source="canreg",
                         "adeno"=NA),
                Can_SCRCR %>% mutate(source="scrcr") %>% relocate(colnames(CRC_CanReg))) %>%
  arrange(LopNr,DIADAT,source)


CRCs <- rbind(CRCs,
              crc_dates_screesco %>% 
                mutate(stage_group=case_when(stage %in% 1:2 ~"I-II",
                                             stage %in% 3:4 ~"III-IV"),
                       T=NA,N=NA,M=NA,ICDO10=NA,SNOMED3=NA,SNOMEDO10=NA,adeno=NA) %>%
                rename(DIADAT=koloskopidatum)  %>%
                left_join(D %>% select(LopNr,rand_date,LoF),
                          by="LopNr") %>% 
                mutate(source="screesco") %>%
                relocate(colnames(CRCs)) ) %>% 
  arrange(LopNr,DIADAT,source) %>%
  unique() 
  

CRCs$screesco_stage <- NA
CRCs$screesco_stage_group <- NA
CRCs$screesco_crc_date <- NA
CRCs$screesco_stage[CRCs$source %in% "screesco"] <- CRCs$stage[CRCs$source %in% "screesco"] 
CRCs$screesco_stage_group[CRCs$source %in% "screesco"] <- CRCs$stage_group[CRCs$source %in% "screesco"] 
CRCs$screesco_crc_date[CRCs$source %in% "screesco"] <- CRCs$DIADAT[CRCs$source %in% "screesco"] 

select_nonmis <- function(x){
  nonmis <- !is.na(x)
  if(any(nonmis)){
    ret <- min(which(nonmis))
  } else{
    ret <- 1
  }
}


CRCs <- CRCs %>%
  group_by(LopNr) %>%
  mutate(screesco_stage=screesco_stage[select_nonmis(screesco_stage)],
         screesco_stage_group=screesco_stage_group[select_nonmis(screesco_stage_group)],
         screesco_crc_date=screesco_crc_date[select_nonmis(screesco_crc_date)])


CRCs <- CRCs %>%
  group_by(LopNr) %>% 
  mutate(nr=n(),
         timediff=as.numeric(DIADAT-min(DIADAT)))

CRCsu <- CRCs %>%
  select(LopNr, rand_date, LoF, screesco_crc_date, screesco_stage, screesco_stage_group) %>%
  unique() %>%
  mutate(date=NA,in_scrcr=FALSE, scrcr_date=NA, scrcr_t=NA, scrcr_n=NA, scrcr_m=NA, scrcr_stage=NA ,scrcr_stage_group=NA,scrcr_nr=NA,scrcr_adeno=NA,scrcr_anymis=FALSE,
         in_canreg=FALSE, canreg_date=NA, canreg_icd=NA, canreg_snomed3=NA,canreg_snomed10=NA, canreg_t=NA, canreg_n=NA, canreg_m=NA, canreg_stage=NA, canreg_stage_group=NA,canreg_nr=NA,canreg_anymis=FALSE)
CRCsu$screesco_crc_date <- as.Date(CRCsu$screesco_crc_date,origin="1970-01-01")

for(j in 1:nrow(CRCsu)){
  if(j %% 100 ==0){print(j)}
  
  temp_canreg <- CRCs %>% 
    filter(LopNr %in% CRCsu$LopNr[j]) %>%
    filter(source %in% c("canreg"))
  
  temp_scrcr <- CRCs %>% 
    filter(LopNr %in% CRCsu$LopNr[j]) %>%
    filter(source %in% c("scrcr"))
  
  ###
  # Step 1: Check for first date of CRC in Canreg and SCRCR
  first_date <- NA
  
  if(nrow(temp_canreg)>0){
    first_date <- min( c(first_date,temp_canreg$DIADAT),na.rm=TRUE )
  }
  if(nrow(temp_scrcr)>0){
    first_date <- min( c(first_date,temp_scrcr$DIADAT),na.rm=TRUE )
  }
  
  CRCsu$date[j] <-  first_date
  
  ###
  # Step 2: Select all CRCs within 90 days from first_date

  
  temp_canreg <- CRCs %>% 
    filter(LopNr %in% CRCsu$LopNr[j]) %>%
    filter(source %in% c("canreg")) %>%
    filter(DIADAT<= (first_date+90))
  
  temp_scrcr <- CRCs %>% 
    filter(LopNr %in% CRCsu$LopNr[j]) %>%
    filter(source %in% c("scrcr"))%>%
    filter(DIADAT<= (first_date+90))
  ###
  # Step 3: Use stage info from SCRCR if not missing 
  if(nrow(temp_scrcr)>0){
    in_scrcr <- TRUE
    CRCsu$in_scrcr[j] <- TRUE
    

      if(nrow(temp_scrcr)>1){ 
       # stop("Many rows SCRCR...")
        temp_scrcr <- temp_scrcr %>%
          mutate(nr=n())
        
        if(any(is.na(temp_scrcr$stage))){
          CRCsu$scrcr_anymis[j] <- TRUE
          
          if(all(is.na(temp_scrcr$stage))){
            temp_scrcr <- temp_scrcr %>%
              slice(1)
          } else{
            temp_scrcr <- temp_scrcr %>%
              filter(!is.na(stage)) %>%
              filter(stage %in% max(stage)) %>%
              slice(1)
          }
          
          
        } else{
          temp_scrcr <- temp_scrcr %>%
            filter(stage %in% max(stage)) 
          
          if(nrow(temp_scrcr)>1){
            temp_scrcr <- temp_scrcr %>%
              filter(M %in% max(M)) %>%
              filter(N %in% max(N)) %>%
              filter(T %in% max(T)) %>%
              slice(1)
          }
        }
      } else{
        temp_scrcr$nr <- nrow(temp_scrcr)
      }
      
      CRCsu$scrcr_date[j] <- temp_scrcr$DIADAT
      CRCsu$scrcr_t[j] <- temp_scrcr$T
      CRCsu$scrcr_n[j] <- temp_scrcr$N
      CRCsu$scrcr_m[j] <- temp_scrcr$M
      CRCsu$scrcr_stage[j]  <- temp_scrcr$stage
      CRCsu$scrcr_stage_group[j]  <- temp_scrcr$stage_group
      CRCsu$scrcr_adeno[j] <- temp_scrcr$adeno
      CRCsu$scrcr_nr[j]  <- temp_scrcr$nr

  } # end if in SCRCR
  
  
  ###
  # Step 4: If stage in SCRCR was missing then look in CanReg
  in_canreg <- FALSE
  if(nrow(temp_canreg)>0){
    in_canreg <- TRUE
    CRCsu$in_canreg[j] <- TRUE
  
    if(nrow(temp_canreg)>1){ 
      temp_canreg <- temp_canreg %>%
        mutate(nr=n())

      
      if(any(is.na(temp_canreg$stage))){
        CRCsu$canreg_anymis[j] <- TRUE
        
        if(all(is.na(temp_canreg$stage))){
          temp_canreg <- temp_canreg %>%
            slice(1)
        } else{
          temp_canreg <- temp_canreg %>%
            filter(!is.na(stage)) %>%
            filter(stage %in% max(stage)) %>%
            slice(1)
          
        }
   
      } else{
        temp_canreg <- temp_canreg %>%
          filter(stage %in% max(stage)) 
        
        if(nrow(temp_canreg)>1){
          temp_canreg <- temp_canreg %>%
            filter(M %in% max(M)) %>%
            filter(N %in% max(N)) %>%
            filter(T %in% max(T)) %>%
            slice(1)
          #stop("canreg")
        }
      }
    } else{
      temp_canreg$nr <- nrow(temp_canreg)
    }
    
    CRCsu$canreg_date[j] <- temp_canreg$DIADAT
    
    CRCsu$canreg_icd[j] <- temp_canreg$ICDO10
    CRCsu$canreg_snomed3[j] <- temp_canreg$SNOMED3
    CRCsu$canreg_snomed10[j] <- temp_canreg$SNOMEDO10
    
    CRCsu$canreg_t[j] <- temp_canreg$T
    CRCsu$canreg_n[j] <- temp_canreg$N
    CRCsu$canreg_m[j] <- temp_canreg$M
    CRCsu$canreg_stage[j]  <- temp_canreg$stage
    CRCsu$canreg_stage_group[j]  <- temp_canreg$stage_group
    CRCsu$canreg_nr[j]  <- temp_canreg$nr
   
  }
}

CRCsu$stage <- CRCsu$scrcr_stage
CRCsu$stage[is.na(CRCsu$stage)] <- CRCsu$canreg_stage[is.na(CRCsu$stage)] 
CRCsu$stage_group <- CRCsu$scrcr_stage_group
CRCsu$stage_group[is.na(CRCsu$stage_group)] <- CRCsu$canreg_stage_group[is.na(CRCsu$stage_group)] 

CRCsu$scrcr_date <- as.Date(CRCsu$scrcr_date,origin="1970-01-01")
CRCsu$canreg_date <- as.Date(CRCsu$canreg_date,origin="1970-01-01")
CRCsu$date <- as.Date(CRCsu$date,origin="1970-01-01")

CRCsu$date[is.na(CRCsu$date)]<-CRCsu$scrcr_date[is.na(CRCsu$date)]

CRCsu <- CRCsu %>%
  relocate(LopNr,rand_date,LoF,screesco_crc_date,screesco_stage,screesco_stage_group,date,stage,stage_group)

## ### ## ## ### ## ## ### ## ## ### ## ## ### ## ## ### ## ## ### ## ## ### ## ## ### ## 

CRCs_unique <- CRCsu %>%
  select(LopNr,rand_date,LoF,screesco_crc_date,screesco_stage,screesco_stage_group,date,stage,stage_group) %>%
  rename(reg_crc_date=date,reg_stage=stage,reg_stage_group=stage_group) %>%
  rowwise() %>%
  
  mutate(CRC_cens = !is.na(reg_crc_date) & reg_crc_date<=LoF,
         CRC_timefu = ifelse(CRC_cens, as.numeric(reg_crc_date),  as.numeric(LoF) ),
         
         CRC_cens_screesco = !is.na(screesco_crc_date) & screesco_crc_date<=LoF,
         CRC_timefu_screesco =  ifelse(CRC_cens_screesco, as.numeric(screesco_crc_date),  as.numeric(LoF) )) %>%
  
  ungroup() %>%
  
  select(-c(rand_date,LoF)) 

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Add CRCs to main analysis
D <- D %>%
  left_join(CRCs_unique,
            by="LopNr")

D$CRC_cens[is.na(D$CRC_cens)]<-FALSE
D$CRC_timefu[is.na(D$CRC_timefu)]<-as.numeric(D$LoF[is.na(D$CRC_timefu)])
D$CRC_timefu <- D$CRC_timefu - as.numeric(D$rand_date)

D$CRC_cens_screesco[is.na(D$CRC_cens_screesco)]<-FALSE
D$CRC_timefu_screesco[is.na(D$CRC_timefu_screesco)]<-as.numeric(D$LoF[is.na(D$CRC_timefu_screesco)])
D$CRC_timefu_screesco <- D$CRC_timefu_screesco - as.numeric(D$rand_date)

# Four individuals with end of follow-up at date of randomization - assume that these were followed for half a day.
D$timefu[D$timefu==0] <- 0.5
# five with end of follow-up at date of randomization - assume half a day of follow-up 
D$CRC_timefu[D$CRC_timefu==0]<-0.5
















### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Educational level
LISA_edu <- merge(LISA_edu,
                  D %>% select(LopNr,rand_date),
                  by="LopNr",
                  all.y=FALSE,all.x=FALSE) %>%
  mutate(rand_year=substr(rand_date,1,4))

LISA_edu$rand_year <- as.numeric(LISA_edu$rand_year)

LISA_edu<- LISA_edu %>%
  arrange(LopNr,year)

select_edu <- function(edu,year,rand_year){
  ret <- NA
  
  years_under <- year <= rand_year

  if(any(years_under)){
    ret <- edu[max(which(years_under)) ]
  } else if(any(year <= (rand_year + 2))){
    ret <- edu[max(which(year <= (rand_year + 2))) ]
  } 
  return(ret)
}

# Define educational level 
LISA_edu <- LISA_edu  %>%
  group_by(LopNr) %>%
  filter(year <= ( rand_year + 2)  ) %>%
  mutate(Edu=select_edu(edu=EducationalLevel,year=year,rand_year=rand_year) ) %>%
  slice(1) %>% 
  select(LopNr,Edu) %>%
  ungroup()

nrow(LISA_edu)
length(unique(LISA_edu$LopNr))

dim(LISA_edu)
colnames(LISA_edu)


D <- D %>%
  left_join(LISA_edu,
            by="LopNr")

nrow(D)
length(unique(D$LopNr))


D_col <- D_col %>%
  left_join(LISA_edu,
            by="LopNr")


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 













### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# DCI at randomization
nrow(DCI)
length(unique(DCI$lopnr))

D <- D %>% 
  left_join(DCI %>% select(lopnr,dci) %>% rename(LopNr=lopnr),by="LopNr")

D$dci[is.na(D$dci)]<-0 # by definition

# CCI at randomization
D <- D %>% 
  left_join(CCI %>% select(lopnr,CCIw) %>% rename(LopNr=lopnr,cci=CCIw),
            by="LopNr")

D$cci[is.na(D$cci)]<-0 # by definition

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###






### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Save
D$AE_or_death_cens <- D$Censor | D$AE_cens_AnyAE
D$AE_or_death_timefu <- pmin(D$timefu, D$AE_timefu_AnyAE)

D$AECG_or_death_cens <- D$Censor | D$AE_cens_AnyAECG
D$AECG_or_death_timefu <- pmin(D$timefu, D$AE_timefu_AnyAECG)


save(D,file=paste0(data_output,"D.Rdata"))
save(CRCs,file=paste0(data_output,"CRCs.Rdata")) # for quality control and code checking
save(CRCsu,file=paste0(data_output,"CRCsu.Rdata")) # for quality control and code checking
save(CRCs_unique,file=paste0(data_output,"CRCs_unique.Rdata"))  # for use in analyses

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###