### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# By Marcus Westerberg
# Started 20240305
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###




### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Settings and packages

# wd
the_wd <- "S:\\SCREESCO\\Studier\\Year 0 - 2023\\"
setwd(the_wd)

data_path <- paste0(the_wd,"Data\\")

# packages
require(tidyverse)

## ### ### ### ### ### ### ### ###




### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Load data
load(file=paste0(data_path,"D.Rdata")) # D 
table(D$rand_arm)
dim(D)

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
# Colorectal cancers
D$crc_source <- "canreg"
D$crc_source[D$CRC_cens_screesco] <- "screesco"

D$CRC_cens_stage12 <- D$CRC_cens & D$reg_stage_group %in% "I-II"
D$CRC_cens_stage34 <- D$CRC_cens  & D$reg_stage_group %in% "III-IV"
D$CRC_cens_stagemis <- D$CRC_cens & is.na(D$reg_stage_group)

D$CRC_timefu_stage12 <- D$CRC_timefu
D$CRC_timefu_stage34 <- D$CRC_timefu
D$CRC_timefu_stagemis <- D$CRC_timefu

D$CRC_cens_outside <- D$CRC_cens & D$crc_source %in% "canreg"
D$CRC_cens_outside_stage12 <- D$CRC_cens & D$crc_source %in% "canreg" & D$reg_stage_group %in% "I-II"
D$CRC_cens_outside_stage34 <- D$CRC_cens & D$crc_source %in% "canreg" & D$reg_stage_group %in% "III-IV"
D$CRC_cens_outside_stagemis <- D$CRC_cens & D$crc_source %in% "canreg" & is.na(D$reg_stage_group)

D$CRC_timefu_outside <- D$CRC_timefu
D$CRC_timefu_outside_stage12 <- D$CRC_timefu
D$CRC_timefu_outside_stage34 <- D$CRC_timefu
D$CRC_timefu_outside_stagemis <- D$CRC_timefu


D$CRC_cens_inside <- D$CRC_cens & (!D$crc_source %in% "canreg" | D$rand_arm %in% "control")
D$CRC_cens_inside_stage12 <- D$CRC_cens_inside & D$reg_stage_group %in% "I-II"
D$CRC_cens_inside_stage34 <- D$CRC_cens_inside & D$reg_stage_group %in% "III-IV"
D$CRC_cens_inside_stagemis <- D$CRC_cens_inside & is.na(D$reg_stage_group)

D$CRC_timefu_inside <- D$CRC_timefu
D$CRC_timefu_inside_stage12 <- D$CRC_timefu
D$CRC_timefu_inside_stage34 <- D$CRC_timefu
D$CRC_timefu_inside_stagemis <- D$CRC_timefu



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Baseline characteristics

# Source
source(paste0(".\\Script\\Table_wrap.R"))

### ### ### ### ### ### ### ### ### ### ### #

source(paste0(".\\Script\\Baseline_characteristics.R"))

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Extract outcomes

results_list <- list()


# Source file that extracts all the incidence rates and rate ratios
source(".\\Script\\Extract_outcomes.R")

# load the results from above
load(file=".\\Resultat\\main_outcomes_ae_unajd.Rdata") # adverse e events 

load(file=".\\Resultat\\main_outcomes_crc_unajd.Rdata") # colorectal cancers

round2 <- function(y){
  
  inner <- function(x){
    if(is.na(x)){
      ret <-  NA
    } else if(x<1){
      ret <- format(round(x,dig=2),nsmall=2)
    } else{
      ret <- format(round(x,dig=1),nsmall=1)
    }
    return(ret)
  }
 return(sapply(FUN=inner,y))
}

digs <- 2
fix_ci <- function(lower,upper,digs=1){
  return( paste0("(", round2(lower),"-",round2(upper),")") )
}


round3 <- function(y){
  
  inner <- function(x){
    if(is.na(x)){
      ret <-  NA
    }  else{
      ret <- format(round(x,dig=2),nsmall=2)
    }
    return(ret)
  }
  return(sapply(FUN=inner,y))
}

fix_ci2 <- function(lower,upper,digs=1){
  return( paste0("(", round3(lower),"-",round3(upper),")") )
}

person_years_scale <- 100000

# Save tables 
extract_subtable <- function(data,
                             labels=c("all pcol vs control","all fitx2 vs control","effect mod pcol","effect mod fit"),
                             outcomes="Colonoscopy_cens",
                             effectmods_from=NULL,
                             effectmods_to=NULL,
                             person_years_scale=1000){

  
  temp <- data %>%
    filter(label %in% labels & outcome %in% outcomes) %>%
    group_by(outcome) %>%
    arrange(factor(label, levels = labels)) %>% 
    ungroup() %>%
    select(-c(info,subgroup_analysis)) %>%
    
    mutate(Proportion=round2(Proportion),
           PersonYears=round(PersonYears),
           Rate=round2(Rate*person_years_scale),
           irr=round(irr,dig=digs)) %>%
    
    mutate(Rate_CI_lower=fix_ci(Rate_CI_lower*person_years_scale,Rate_CI_upper*person_years_scale,digs=digs)) %>%
    rename(Rate_CI=Rate_CI_lower) %>%
    select(-Rate_CI_upper) %>%
    
    mutate(irr_ci_lower=fix_ci2(irr_ci_lower,irr_ci_upper,digs=digs)) %>%
    rename(irr_ci=irr_ci_lower) %>%
    select(-irr_ci_upper)
  
  temp$interaction_irr <- NA
  temp$interaction_irr_ci <- NA
  
  if(!is.null(effectmods_from) & !is.null(effectmods_to)){
    stopifnot(length(effectmods_from)==length(effectmods_to))
    
    for(h in 1:length(effectmods_from)){
      temp$interaction_irr[effectmods_to[h]] <- temp$irr[effectmods_from[h]]
      temp$interaction_irr_ci[effectmods_to[h]] <-  temp$irr_ci[effectmods_from[h]]
    }
    temp <- temp[-effectmods_from,]
  }
  
  return(temp)
}

extract_subtable_wrap <- function(data,
                                  labels=c("pcol vs control","fitx2 vs control"),
                                  sublables=c("all","men","women"),
                                  effectmods=c("effect mod pcol","effect mod fit"),
                                  effectmods_from=NULL,
                                  effectmods_to=NULL,
                                  outcomes,
                                  add_space=TRUE,
                                  subselect_rows=NULL,
                                  person_years_scale=1000){
  
  ret <- list()
  for(j in 1:length(sublables)){
    
    ret_temp <- list()
    for(k in 1:length(outcomes)){
      ret_temp[[k]]<-extract_subtable(data=data,
                                      labels=c(paste0(sublables[j]," ",labels),effectmods),
                                      outcomes=outcomes[k],
                                      effectmods_from=effectmods_from,
                                      effectmods_to=effectmods_to,
                                      person_years_scale=person_years_scale)
    }
    temp <- do.call(rbind,ret_temp)

    temp <- temp %>% 
      mutate(Type=sublables[j]) %>%
      relocate(label,exposure,Type) %>%
      select(-c(adjusted))
 
    temp <- apply(FUN=function(x) gsub(as.character(x),pattern=" ",replacement=""),temp,MARGIN=2)
    
    if(!is.null(subselect_rows)){
      temp <- temp[subselect_rows,]
    }
    
    if(add_space){ temp <- rbind(temp,rep("",ncol(temp)))}
    
    ret[[j]]<-temp
  }
  
  ret <- do.call(rbind,ret)
  
  return(ret)
}

all_tables <- list()




###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  
# Adverse events  
outc <- c("AECG_or_death_cens","Censor","AE_cens_AnyAECG","AE_cens_Cardiovascular","AE_cens_Gastrointestinal")
temp <- lapply(FUN=extract_subtable_wrap,
               main_outcomes_ae_unajd,
               labels=c("pcol vs control","fitx2 vs control"),
               sublables=c("all","men","women"),
               effectmods=c("effect mod pcol","effect mod fit"),
               effectmods_to=c(1,3),
               effectmods_from=c(5,6),
               add_space=TRUE,
               outcomes=outc,
               subselect_rows=NULL,
               person_years_scale=person_years_scale)

all_tables$ae <- temp
openxlsx::write.xlsx(temp,file=".\\Resultat\\ae.xlsx")


# Format to table 3
unit <- c(1:12,17:20,13:16)
temp2 <- temp$all[unit,]
temp2[,"irr"][(1:10)*2 ] <- ""
temp2[,"irr_ci"][(1:10)*2 ] <- ""

temp2 <- temp2[,-c(13:14)]
openxlsx::write.xlsx(temp2,file=".\\Resultat\\table_ae.xlsx")


# Format to supplementary table
unit <- c(22:23,43:44,
          24:25,45:46)
temp2 <- temp$all[c(unit,unit+4,unit+8,unit+16,unit+12),]

temp2[,"interaction_irr"][ -c(1,5,9,13,17,21,25,29,33,37) ]<-""
temp2[,"interaction_irr_ci"][-c(1,5,9,13,17,21,25,29,33,37)]<-""
openxlsx::write.xlsx(temp,file=".\\Resultat\\suppl_table_ae.xlsx")





all_tables$ae_raw <- main_outcomes_ae_unajd$all

temp <- lapply(FUN=extract_subtable_wrap,
               main_outcomes_ae_unajd,
               labels=c("pcol vs control","fitx2 vs control","pcol men vs women","fit men vs women"),
               sublables=c("all","men","women"),
               effectmods=c("effect mod pcol","effect mod fit"),
               effectmods_to=NULL,
               effectmods_from=NULL,
               add_space=TRUE,
               outcomes=outc,
               subselect_rows=NULL,
               person_years_scale=person_years_scale)
openxlsx::write.xlsx(temp$all,file=".\\Resultat\\ae.raw.xlsx")




###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  
# Colorectal cancers 
crcselects <- c(1,5,9,2,
                3,7,11,4)

crcselects2 <- c(crcselects,crcselects+12,crcselects+24,crcselects+36)

temp <- lapply(FUN=extract_subtable_wrap,
               main_outcomes_crc_unajd,
               labels=c("pcol vs control","fitx2 vs control"),
               sublables=c("all","men","women"),
               effectmods=c("effect mod pcol","effect mod fit"),
               effectmods_to=c(1,3),
               effectmods_from=c(5,6),
               add_space=TRUE,
               outcomes=c("CRC_cens","CRC_cens_inside","CRC_cens_outside",
                          "CRC_cens_stage12","CRC_cens_inside_stage12","CRC_cens_outside_stage12",
                          "CRC_cens_stage34","CRC_cens_inside_stage34","CRC_cens_outside_stage34",
                          "CRC_cens_stagemis","CRC_cens_inside_stagemis","CRC_cens_outside_stagemis"),
               subselect_rows=crcselects2,
               person_years_scale=person_years_scale)

all_tables$crc <- temp
openxlsx::write.xlsx(temp,file=".\\Resultat\\crc.xlsx")

###
# Save to ready table

temp2 <- data.frame(temp$all[1:24,])
temp2 <- temp2  %>% select(-c("interaction_irr","interaction_irr_ci"))
temp2[grepl(temp2$outcome,pattern="inside|outside"),c("PersonYears" , "Rate"   ,   "Rate_CI","irr"   ,   "irr_ci")] <- ""

openxlsx::write.xlsx(temp,file=".\\Resultat\\table_crc.xlsx")



all_tables$crc_raw <- main_outcomes_crc_unajd$all
temp <- main_outcomes_crc_unajd$all
temp <- as.data.frame(apply(FUN=as.character,temp,MARGIN = 2))
openxlsx::write.xlsx( main_outcomes_crc_unajd$all,file=".\\Resultat\\crc.raw.xlsx")


# AE by type
temp <- all_tables$ae[[1]]

temp <- data.frame(temp)

temp <- temp[13:24,]
temp$outcome[temp$outcome %in% "AE_cens_Cardiovascular"] <- "Cardiovascular"
temp$outcome[temp$outcome %in% "AE_cens_Gastrointestinal"] <- "Gastrointestinal"
temp$outcome[temp$outcome %in% "AE_cens_Other"] <- "Other"

temp <- temp %>% select(-c(label,Type)) %>% relocate(outcome,exposure)
openxlsx::write.xlsx(temp,file=".\\Resultat\\ae_by_type_suppl_main.xlsx")


# AE by type detailed

temp <- lapply(FUN=extract_subtable_wrap,
               main_outcomes_ae_unajd[1],
               labels=c("pcol vs control","fitx2 vs control"),
               sublables=c("all","men","women"),
               effectmods=c("effect mod pcol","effect mod fit"),
               effectmods_to=c(1,3),
               effectmods_from=c(5,6),
               add_space=TRUE,
               outcomes=unique(main_outcomes_ae_unajd[[1]]$outcome),
               subselect_rows=NULL,
               person_years_scale=person_years_scale)

temp <- temp$all

temp <- data.frame(temp)

# Format to supplementary table

temp2 <- temp[c(17:20,57:76,53:56,
                13:16,25:52),]

temp2[,"interaction_irr"][ 2*(1:(nrow(temp2)/2)) ]<-""
temp2[,"interaction_irr_ci"][2*(1:(nrow(temp2)/2))]<-""
temp2 <- temp2 %>% select(-c(interaction_irr,interaction_irr_ci))
openxlsx::write.xlsx(temp,file=".\\Resultat\\suppl_table_ae_detailed.xlsx")



temp$outcome<-gsub(temp$outcome,pattern="Censor",replacement="Death")


temp <- temp %>% select(-c(label))
openxlsx::write.xlsx(temp,file=".\\Resultat\\ae_by_type_detailed_suppl_main.xlsx")








### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# IRR Figure

# Source figure
source(".\\Script\\IRR_figure.R")


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###





### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Cumulatice incidence Figure

# Source figure
source(".\\Script\\cuminc.R")


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###





