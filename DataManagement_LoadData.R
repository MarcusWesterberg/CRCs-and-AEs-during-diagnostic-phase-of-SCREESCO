
### ### ### 
# S:\\SCREESCO\\Data\\SoS\\Screesco\\
df_root <- haven::read_sas("S:\\SCREESCO\\Data\\SoS\\Screesco\\sos_df_root.sas7bdat")

# Invited to both arms = 	har ett utskicksdatum
df_root$Invited <- !is.na(df_root$utskicksdatum)
### 

dismissed <- df_root %>%
  filter(!is.na(deltagandeavfardad) & ! deltagandeavfardad %in% "NA") %>%
  select(LopNr,deltagandeavfardad) %>% 
  unique()


### ### ### 
# S:\\SCREESCO\\Data\\SoS\\Screesco\\
df_randomisering <- haven::read_sas("S:\\SCREESCO\\Data\\SoS\\Screesco\\sos_df_randomisering.sas7bdat")
df_randomisering <- df_randomisering %>%
  select(-c(randomisering_stratum,randomisering_sekvens)) %>%
  #arrange(LopNr) %>%
  group_by(LopNr) %>%
  mutate(nr=n()) %>%
  ungroup()

df_randomisering$rand_date <- as.Date(df_randomisering$rand_date)

df_randomisering$Invited <- df_randomisering$LopNr %in% df_root$LopNr[df_root$Invited] | df_randomisering$rand_arm %in% "control"


# Add region label
df_randomisering$rand_lan_desc <- ""
df_randomisering$rand_lan_desc[df_randomisering$rand_lan %in% "3"] <- "Uppsala"
df_randomisering$rand_lan_desc[df_randomisering$rand_lan %in% "4"] <- "Södermanland"
df_randomisering$rand_lan_desc[df_randomisering$rand_lan %in% "5"] <- "Östergötland"
df_randomisering$rand_lan_desc[df_randomisering$rand_lan %in% "6"] <- "Jönköping"
df_randomisering$rand_lan_desc[df_randomisering$rand_lan %in% "7"] <- "Kronoberg"
df_randomisering$rand_lan_desc[df_randomisering$rand_lan %in% "8"] <- "Kalmar"
df_randomisering$rand_lan_desc[df_randomisering$rand_lan %in% "10"] <- "Blekinge"
df_randomisering$rand_lan_desc[df_randomisering$rand_lan %in% "12"] <- "Skåne"
df_randomisering$rand_lan_desc[df_randomisering$rand_lan %in% "13"] <- "Halland"
df_randomisering$rand_lan_desc[df_randomisering$rand_lan %in% "14"] <- "Västra Götaland"
df_randomisering$rand_lan_desc[df_randomisering$rand_lan %in% "17"] <- "Värmland"
df_randomisering$rand_lan_desc[df_randomisering$rand_lan %in% "18"] <- "Örebro"
df_randomisering$rand_lan_desc[df_randomisering$rand_lan %in% "19"] <- "Västmanland"
df_randomisering$rand_lan_desc[df_randomisering$rand_lan %in% "20"] <- "Dalarna"
df_randomisering$rand_lan_desc[df_randomisering$rand_lan %in% "21"] <- "Gävleborg"
df_randomisering$rand_lan_desc[df_randomisering$rand_lan %in% "23"] <- "Jämtland"
df_randomisering$rand_lan_desc[df_randomisering$rand_lan %in% "24"] <- "Västerbotten"
df_randomisering$rand_lan_desc[df_randomisering$rand_lan %in% "25"] <- "Norrbotten"

df_randomisering$rand_sjukvreg <- NA
df_randomisering$rand_sjukvreg[df_randomisering$rand_lan_desc %in% c("Norrbotten","Västerbotten","Jämtland") ] <- c("North") 
df_randomisering$rand_sjukvreg[df_randomisering$rand_lan_desc %in% c("Gävleborg","Dalarna","Örebro","Västmanland","Uppsala","Värmland","Södermanland") ] <- c("Central") 
df_randomisering$rand_sjukvreg[df_randomisering$rand_lan_desc %in% c("Kalmar","Östergötland","Jönköping") ] <- c("Southeast")
df_randomisering$rand_sjukvreg[df_randomisering$rand_lan_desc %in% c("Västra Götaland","Halland") ] <- c("West")
df_randomisering$rand_sjukvreg[df_randomisering$rand_lan_desc %in% c("Skåne","Blekinge","Kronoberg") ] <- c("South")
table(df_randomisering$rand_sjukvreg ,useNA="always")

df_randomisering$is_fit_control <- df_randomisering$rand_arm %in% "control" & df_randomisering$rand_year %in% 1954:1956







### ### ### 
# CoD 
# S:\\SCREESCO\\Data\\SoS\\Dödsorsaksregistret\\ut_r_dors_9703_2022.sas7bdat
CoD <- haven::read_sas("S:\\SCREESCO\\Data\\SoS\\Dödsorsaksregistret\\ut_r_dors_9703_2022.sas7bdat") %>%
  relocate(LopNr,DODSDAT,ICD,ULORSAK)

CoD$uncertain_month <- substr(CoD$DODSDAT,5,6) %in% "00"
CoD$uncertain_day <- substr(CoD$DODSDAT,7,8) %in% "00"

CoD$DODSDAT <- sapply(FUN=function(x){ 
  
  yr <- substr(x,1,4)
  month <- substr(x,5,6)
  day <- substr(x,7,8)
  
  if(month %in% "00"){month <- "06"}
  
  if(day %in% "00"){day <- "15"}
  
  return(paste0(yr,"-",month,"-",day))
},CoD$DODSDAT)

CoD$DODSDAT <- as.Date(CoD$DODSDAT)

all(CoD$LopNr %in% df_randomisering$LopNr)

nrow(CoD)
length(unique(CoD$LopNr))

#table(substr(CoD$DODSDAT,1,4))

CoD <- CoD %>% 
  filter(!substr(DODSDAT,1,4) %in% 2021:2023) %>%
  select(LopNr,DODSDAT) %>%
  rename(DeathDate=DODSDAT)


###
# Load migrations
migr <- haven::read_sas("S:\\SCREESCO\\Data\\SCB\\20250612\\af_lev_migrationer.sas7bdat")
migr$migr_date <- as.Date(paste0(substr(migr$Datum,1,4),"-",substr(migr$Datum,5,6),"-",substr(migr$Datum,7,8)))
migr <- migr %>%
  select(-Datum) %>%
  filter(migr_date >= as.Date("2014-01-01") & migr_date <= as.Date("2020-12-31") ) %>%
  filter(Posttyp %in% "Utv") %>%
  select(-Posttyp) 

migr <- migr %>%
  filter(LopNr %in% df_randomisering$LopNr) %>%
  left_join(df_randomisering %>% select(LopNr,rand_date),by="LopNr") %>%
  arrange(LopNr,migr_date) %>%
  filter(migr_date>=rand_date) %>%
  group_by(LopNr) %>%
  slice(1) %>%
  ungroup()




### ### ### 
# Colonoscopy in SCREESCO
df_sumkol <- haven::read_sas("S:\\SCREESCO\\Data\\SoS\\Screesco\\sos_sum_koloskopi.sas7bdat")
df_sumkol$koloskopidatum <- as.Date(df_sumkol$koloskopidatum)

crc_dates_screesco <- df_sumkol %>%
  filter(risk_stage %in% "cancer") %>%
  select(LopNr,koloskopidatum) %>%
  unique()  %>%
  filter(!LopNr %in% c(3534,260842))

newstage <- read.csv("S:\\SCREESCO\\Studier\\Year 0 - 2023\\Data\\CRC_new.csv",sep=";",header=TRUE)
colnames(newstage)[1]<-"LopNr"
newstage <- newstage[1:10,]
newstage$koloskopidatum <- as.Date(newstage$koloskopidatum,tryFormats = c("%m-%d-%Y"))
newstage <- newstage %>% filter(CRC == 1)

crc_dates_screesco <- rbind(crc_dates_screesco,newstage %>% select(LopNr,koloskopidatum))


for(j in 1:nrow(crc_dates_screesco)){
  print(paste0("j"=j))
  r_lpnr <- df_sumkol$LopNr %in% crc_dates_screesco$LopNr[j]
  cat("___")
  print(sum(r_lpnr))
  if(any(r_lpnr)){
    r_date <- (df_sumkol$koloskopidatum %in% crc_dates_screesco$koloskopidatum[j] & r_lpnr) 
    
    print(sum(r_date))
    if(any(r_date)){
      df_sumkol$risk_stage[r_date] <- "cancer"
    } else{
      print(df_sumkol$koloskopidatum[r_lpnr])
      print("Using the date in df_sumkol... it is the same round...")
      stopifnot(sum(r_lpnr)==1)
      df_sumkol$risk_stage[r_lpnr] <- "cancer"
    }
  }
}

df_sumkol$risk_stage[df_sumkol$LopNr %in% c(3534,260842)] <- ""


### ### ### 
# Koloskopi komplikationer

df_kol <-  haven::read_sas("S:\\SCREESCO\\Data\\SoS\\Screesco\\sos_df_koloskopi.sas7bdat")
col_dates_screesco <- df_kol %>% 
  select(LopNr,koloskopidatum,skopist) %>%
  unique() 

crc_komplikationer <- df_kol %>%
  filter(koloskopikomplikationer %in% "ja" ) %>%
  select(LopNr,koloskopidatum,komplikationstyp) %>%
  arrange(LopNr,koloskopidatum) %>%
  unique()

crc_komplikationer$koloskopidatum <- as.Date(crc_komplikationer$koloskopidatum)

length(unique(crc_komplikationer$LopNr))
dim(crc_komplikationer)


crc_komplikationer_allv <- df_kol %>%
  filter(koloskopikomplikationer %in% "ja" ) %>%
  filter(komplikationstyp %in% c("blodning_med_atgard","tarmperforation")) %>%

  select(LopNr,koloskopidatum,komplikationstyp,blodningsatgard,studiearm) %>%
  arrange(LopNr,koloskopidatum) %>%
  unique()

table(crc_komplikationer_allv$blodningsatgard,crc_komplikationer_allv$studiearm)

col_dates_screesco$koloskopidatum <- as.Date(col_dates_screesco$koloskopidatum,origin="1970-01-01")

skopists <- get(load("S:\\SCREESCO\\Studier\\Year 0 - 2023\\Indata\\skopists.Rdata"))
skopists <- skopists %>%
  select(skopist_id,ssk) %>%
  rename(skopist=skopist_id) %>%
  mutate(skopist=as.character(skopist))

col_dates_screesco <- col_dates_screesco %>%
  left_join(skopists,by="skopist")





### ### ### 
# Stage in SCREESCO
df_stage <- haven::read_sas("S:\\SCREESCO\\Data\\SoS\\Screesco\\sos_stage.sas7bdat")
df_misstage <- haven::read_sas("S:\\SCREESCO\\Data\\SoS\\Screesco\\sos_misstage.sas7bdat")
df_remisop <- haven::read_sas("S:\\SCREESCO\\Data\\SoS\\Screesco\\sos_remissop.sas7bdat")

oldstage <- read.csv("S:\\SCREESCO\\Data\\Screesco\\Cancer stage\\cancer_stages.csv",sep=";")
oldstage <- oldstage %>%
  filter(!crc %in% 0)



all(df_stage$LopNr %in% df_randomisering$LopNr)
all(df_misstage$LopNr %in% df_randomisering$LopNr)
all(df_remisop$LopNr %in% df_randomisering$LopNr)
all(newstage$LopNr %in% df_randomisering$LopNr)

df_misstage <- df_misstage %>% 
  select(LopNr,Stage,Kommentar) %>% 
  rename(stage=Stage) %>% 
  mutate(StagePAD="",Kommentar2="") %>%
  relocate(colnames(df_stage))

df_remisop <- df_remisop %>%
  filter(cancer %in% "TRUE" & !Stage %in% "NA") %>% 
  select(LopNr,Stage) %>%
  rename(stage=Stage) %>%
  mutate(StagePAD="",Kommentar="",Kommentar2="") 

newstage2 <- newstage %>%
  select(LopNr,Stage) %>%
  rename(stage=Stage) %>%
  mutate(StagePAD="",Kommentar="",Kommentar2="")

screesco_stages <- rbind(df_stage,
                         df_misstage,
                         df_remisop,
                         newstage2) %>%
  arrange(LopNr) %>%
  filter(!LopNr %in% c(3534,260842))
length(unique(screesco_stages$LopNr))

screesco_stages$stage[screesco_stages$stage %in% "saknas"] <- screesco_stages$StagePAD[screesco_stages$stage %in% "saknas"]

screesco_stages$stage[screesco_stages$stage %in% "I"]<-1
screesco_stages$stage[screesco_stages$stage %in% "II"]<-2
screesco_stages$stage[screesco_stages$stage %in% "III"]<-3
screesco_stages$stage[screesco_stages$stage %in% "IV"]<-4
screesco_stages$stage[screesco_stages$stage %in% ""]<- NA

all(df_stage$LopNr %in% df_randomisering$LopNr)

lpnr_out <- crc_dates_screesco$LopNr[!crc_dates_screesco$LopNr %in% screesco_stages$LopNr]

o1 <- oldstage %>% filter(substr(pnr,1,8) %in% gsub(df_randomisering$fodelsedatum[df_randomisering$LopNr %in% lpnr_out[1]],pattern="-",replacement="") )
o2 <- oldstage %>% filter(substr(pnr,1,8) %in% gsub(df_randomisering$fodelsedatum[df_randomisering$LopNr %in% lpnr_out[2]],pattern="-",replacement="") )
o3 <- oldstage %>% filter(substr(pnr,1,8) %in% gsub(df_randomisering$fodelsedatum[df_randomisering$LopNr %in% lpnr_out[3]],pattern="-",replacement="") )
stopifnot(o1$Stage==2)
stopifnot(o2$Stage==1)
stopifnot(o3$Stage==1)

screesco_stages <- rbind(screesco_stages,
                         data.frame(LopNr=lpnr_out,
                                    stage=c(o1$Stage,o2$Stage,o3$Stage),
                                    StagePAD="",
                                    Kommentar="",
                                    Kommentar2=""))


crc_dates_screesco <- crc_dates_screesco %>%
  left_join(screesco_stages %>% select(LopNr,stage),by="LopNr")

stopifnot(sum(is.na(crc_dates_screesco$stage))==1)
crc_dates_screesco$stage[is.na(crc_dates_screesco$stage)] <- 3 # Based on SCRCR - trumphs canreg taht says stage 4






### ### ### 
# PAR
# S:\\SCREESCO\\Data\\SoS\\Patientregistret\\PAR.Rdata
PAR <- get(load("S:\\SCREESCO\\Data\\SoS\\Patientregistret\\PAR.Rdata"))
PAR <- PAR %>% rename(LopNr=lopnr)
all(PAR$LopNr %in% df_randomisering$LopNr)

PAR <- PAR %>% 
  filter(substr(indatum,1,4) %in% 1989:2020)






### 
PAR_op <- get(load("S:\\SCREESCO\\Data\\SoS\\Patientregistret\\PAR_OP.Rdata"))
PAR_op <- PAR_op %>% rename(LopNr=lopnr)
all(PAR_op$LopNr %in% df_randomisering$LopNr)
PAR_op <- PAR_op %>% 
  filter(substr(indatum,1,4) %in% 1989:2020)


PAR_svd <- get(load("S:\\SCREESCO\\Data\\SoS\\Patientregistret\\PAR_SVD.Rdata"))
PAR_svd <- PAR_svd %>% rename(LopNr=lopnr)
all(PAR_svd$LopNr %in% df_randomisering$LopNr)
PAR_svd <- PAR_svd %>% 
  filter(substr(indatum,1,4) %in% 1989:2020)





### ### ### 
# CanReg 
# # S:\\SCREESCO\\Data\\SoS\\Cancerregistret\\ut_r_can_9703_2022.sas7bdat
CanReg <- haven::read_sas("S:\\SCREESCO\\Data\\SoS\\Cancerregistret\\ut_r_can_9703_2022.sas7bdat")

CanReg$DIADAT <- sapply(FUN=function(x){ 
  
  yr <- substr(x,1,4)
  month <- substr(x,5,6)
  day <- substr(x,7,8)
  
  
  return(paste0(yr,"-",month,"-",day))
},CanReg$DIADAT)

CanReg$DIADAT <- as.Date(CanReg$DIADAT)

CanReg <- CanReg %>%
  filter(!substr(DIADAT,1,4) %in% 2021:2023)

all(CanReg$LopNr %in% df_randomisering$LopNr)


### 
# Look for CRC combined with snomed
CanReg$ICD_CRC <- CanReg$ICDO10 %in% c("C180","C182","C183","C184","C185","C186","C187","C188","C189",
                                       "C199","C209")

# table(CanReg$ICD_CRC)
CanReg$SNOMED_CRC <- CanReg$SNOMED3 %in% c(81403, 82113, 82133, 82203, 82433, 82613, 82633, 84803, 84903) |
  CanReg$SNOMEDO10 %in% c(81403, 82113, 82203, 84803, 84903)

# table(CanReg$SNOMED_CRC)

CanReg$CRC <- CanReg$ICD_CRC & CanReg$SNOMED_CRC 


table(df_sumkol$LopNr[df_sumkol$risk_stage %in% "cancer"] %in% CanReg$LopNr[CanReg$CRC])
table(df_sumkol$LopNr[df_sumkol$risk_stage %in% "cancer"] %in% CanReg$LopNr[CanReg$ICD_CRC])

### ### ### 
# SCRCR

SCRCR_ROT <- haven::read_sas("S:\\SCREESCO\\Data\\SoS\\Kolorektalregistret\\sos_scrcr_id2184_rot.sas7bdat")


SCRCR <- SCRCR_ROT %>%
  select(LopNr,A1_Diagnosdatum,A3_adeno,A1_utfallt,A1_utfalln,A1_utfallm,  A3_t,A3_n,A3_m) %>%
  unique() %>%
  arrange(LopNr,A1_Diagnosdatum) %>%
  filter(!substr(A1_Diagnosdatum,1,4) %in% 2021:2023)



### ### ### 
# Country of birth
BirthCountry <- read.table("S:\\SCREESCO\\Data\\SCB\\20220907\\Forsberg_Lev_Fodelselandgrupp.txt",sep="\t",header=TRUE)
all(BirthCountry$LopNr %in% df_randomisering$LopNr)

BirthCountry$BornSwed <- BirthCountry$FodGrEg4 %in% "Sverige"



### ### ### 
# Educational level
LISA_list <- list()
for(j in 1:9){
  LISA_list[[j]]<- read.table(paste0("S:\\SCREESCO\\Data\\SCB\\20220907\\Forsberg_Lev_LISA_",2012+j,".txt"),sep="\t",header=TRUE)
  colnames(LISA_list[[j]]) <- c("LopNr","SenPNr","biobank" ,"randomisering_arm_txt","Edu", "KallKod"    )
  LISA_list[[j]]$year <- 2012+j
  #print(colnames(LISA_list[[j]]))
}
LISA_edu <- do.call(rbind,LISA_list)
all(LISA_edu$LopNr %in% df_randomisering$LopNr)


LISA_edu <- LISA_edu %>% 
  arrange(LopNr,year) %>% 
  mutate(EducationalLevel=case_when(Edu %in% 1:2 ~ 'low',
                                    Edu %in% 3:4 ~ 'intermediate',
                                    Edu > 4:7 ~ 'high'))
LISA_edu <- LISA_edu %>%
  filter(!is.na(EducationalLevel))


### ### ### 
# DCI at randomization
# S:\\SCREESCO\\Data\\DCI\\rand_DCI.Rdata
DCI <- get(load("S:\\SCREESCO\\Data\\DCI\\rand_DCI.Rdata"))
DCI_col <- get(load("S:\\SCREESCO\\Data\\DCI\\col_DCI.Rdata"))


# CCI at randomization
# S:\\SCREESCO\\Data\\CCI\\CCI_rand_CCI_lookback_10
CCI <- get(load("S:\\SCREESCO\\Data\\CCI\\CCI_rand_CCI_lookback_10.Rdata"))
CCI_col <- get(load("S:\\SCREESCO\\Data\\CCI\\CCI_col_CCI_lookback_10.Rdata"))


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###












