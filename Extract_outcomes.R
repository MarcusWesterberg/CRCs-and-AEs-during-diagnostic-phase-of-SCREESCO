###
# Script to extract outcomes





##
# Helper function to extract the outcomes
extract_outcome <- function(data,
                            outcomes_cens=c("Colonoscopy_cens"),
                            outcomes_timefu=c("Colonoscopy_timefu"),
                            glm.family="poisson",
                            robust.se=FALSE){
  
  
  stopifnot(length(outcomes_cens) == length(outcomes_timefu))
  stopifnot(all(outcomes_cens %in% colnames(data)))
  stopifnot(all(outcomes_timefu %in% colnames(data)))
  
  any_control <- any(data$rand_arm %in% "control")
  
  pcol <- data$rand_arm %in% "dk"
  fit <- data$rand_arm %in% "fit"
  
  control <- data$rand_arm %in% "control"
  fit_control <- data$rand_arm %in% "control" & data$is_fit_control
  
  both_sexes  <- any(data$sex %in% "man" ) & any(data$sex %in% "woman")
  
  
  # extract raw incidence rates per person years per outcome
  
  extract_raw_rates <- function(timefu,
                                cens,
                                rate_scale=1){
    
    timefu <- timefu / 365.24
    
    cens <- as.numeric((cens))
    
    rate <- sum(cens)/sum(timefu)*rate_scale
    
    if(sum(cens)>4){
    
      if(robust.se){
        m <- glm(cens ~ offset(log(timefu)), family=glm.family)
        
        library("sandwich")
        rse <- sqrt(diag(vcovHC(m, type="HC0"))) 
        
        rate_ci <- exp( (m$coefficients + rse*c(qnorm(0.025),qnorm(0.975))) )*rate_scale
        
      } else{
        rate_ci <- exp( confint.default( glm(cens ~ offset(log(timefu)), family=glm.family) ) )*rate_scale
      }
   
    } else{
      rate_ci <- c(NA,NA)
    }
    
    
    ret <- data.frame("N"=length(timefu),
                      "n"=sum(cens),
                      "Proportion"=mean(cens)*100,
                      "PersonYears"=sum(timefu),
                      "Rate"=rate,
                      "Rate_CI_lower"=rate_ci[1],
                      "Rate_CI_upper"=rate_ci[2])
    return(ret)
  }
  
  extract_irr <- function(timefu,
                          cens,
                          exposure,
                          exposure_levels){

    
    cens <- as.numeric((cens))
    
    overdisp <- NA
    
    if(all(exposure) | all(!exposure) | sum(cens)<5){
      
      irr <- NA
      ci <- c(NA,NA)
      info <- paste0("N exposure: ",sum(exposure),", N not exposure: ",sum(!exposure),", N outcomes: ",sum(cens))
    } else{
    
      m <- glm(cens ~ offset(log(timefu/365.24)) + exposure, family=glm.family)
      if(glm.family %in% "poisson"){
        m$overdisp <- AER::dispersiontest(m,alternative = "two.sided")
      }
    
      if(robust.se){
        library("sandwich")
        rse <- sqrt(diag(vcovHC(m, type="HC0")))[2]
        
        ci <- exp( (m$coefficients[2] + rse*c(qnorm(0.025),qnorm(0.975))) )*rate_scale
      } else{
        ci <- exp( confint.default( m , parm = 2) )
      }
        
      info <- ""
      
      irr <- exp( coefficients(m)[2] )
    
      overdisp <- m$overdisp
    }   
    
    part_a <- rbind(extract_raw_rates(timefu[exposure],
                                      cens[exposure]) %>%
                      mutate(exposure=exposure_levels[1]),
                    
                    extract_raw_rates(timefu[!exposure],
                                      cens[!exposure]) %>%
                      mutate(exposure=exposure_levels[2]))
    rownames(part_a)<-NULL
    
    part_b <- data.frame("irr"=irr,
                         "irr_ci_lower"=ci[1],
                         "irr_ci_upper"=ci[2],
                         "adjusted"=FALSE)
    rownames(part_b)<-NULL
    
    ret <- cbind(part_a,
                 part_b) %>%
      mutate("info"=info)
    
    attr(ret,"overdisp") <- overdisp
    return(ret)
    
  }
  
  
  extract_effect_mod <- function(timefu,
                                 cens,
                                 exposure,
                                 exposure2){
    
    
    cens <- as.numeric((cens))
    overdisp <- NA
    
    if(all(exposure) | all(exposure2) | all(!exposure2) | all(!exposure) | sum(cens)<5){
      
      irr <- NA
      ci <- c(NA,NA)
      info <- paste0("N exposure: ",sum(exposure),
                     ", N not exposure: ",sum(!exposure),
                     ", N exposure2: ",sum(exposure2),
                     ", N not exposure2: ",sum(!exposure2),
                     ", N outcomes: ",sum(cens))
    } else{
   
      m <- glm(cens ~ offset(log(timefu/365.24)) + exposure*exposure2, family=glm.family)
      if(glm.family %in% "poisson"){
        m$overdisp <- AER::dispersiontest(m,alternative = "two.sided")
      }
      info <- ""
      
      
      if(robust.se){
        library("sandwich")
        rse <- sqrt(diag(vcovHC(m, type="HC0")))[4]
        
        ci <- exp( (m$coefficients[4] + rse*c(qnorm(0.025),qnorm(0.975))) )*rate_scale
      } else{
        ci <- exp( confint.default( m , parm = 4) )
      }
       
      irr <- exp( coefficients(m)[4] )

      overdisp <- m$overdisp
    }
    
    part_a <- rbind(extract_raw_rates(timefu[exposure],
                                      cens[exposure]) %>%
                      mutate(exposure="Interaction test"),
                    extract_raw_rates(timefu[!exposure],
                                      cens[!exposure]) %>%
                      mutate(exposure="Interaction test"))
    rownames(part_a)<-NULL
    
    part_b <- data.frame("irr"=irr,
                         "irr_ci_lower"=ci[1],
                         "irr_ci_upper"=ci[2],
                         "adjusted"=FALSE)
    rownames(part_b)<-NULL
    
    ret <- cbind(part_a,
                 part_b) %>%
      mutate("info"=info)
    
    attr(ret,"overdisp")<- overdisp
    
    return(ret)
    
  }
  
  ret <- list()
  overdisp_list <- list()
  for(j in 1:length(outcomes_cens)){
    
    print(outcomes_cens[j])
    print(Sys.time())
    cens <- data[[outcomes_cens[j]]]
    timefu <- data[[outcomes_timefu[j]]]
    
    stopifnot(all(timefu>0))
    
    ret_temp <- list()
    
    
    ### PCOL ### PCOL ### 
    
    # all - pcol vs control
    urval <- pcol | control
    ret_temp[["all_pcol_vs_control"]] <- extract_irr(timefu=timefu[urval],
                                                     cens=cens[urval],
                                                     exposure=pcol[urval],
                                                     exposure_levels=c("PCOL","Control")) %>% 
      mutate(label="all pcol vs control")
   
    
    # men
    urval <- (pcol | control) & data$sex %in% "man"
    ret_temp[["men_pcol_vs_control"]]<- extract_irr(timefu[urval],
                                                    cens[urval],
                                                    exposure=pcol[urval],
                                                    exposure_levels=c("PCOL","Control"))%>% 
      mutate(label="men pcol vs control")
    
    
    # women
    urval <- (pcol | control) & data$sex %in% "woman"
    ret_temp[["women_pcol_vs_control"]]<- extract_irr(timefu[urval],
                                                      cens[urval],
                                                      exposure=pcol[urval],
                                                      exposure_levels=c("PCOL","Control")) %>% 
      mutate(label="women pcol vs control")
    
    
    # men vs women in pcol
    urval <- pcol 
    ret_temp[["pcol_men_vs_women"]]<- extract_irr(timefu=timefu[urval],
                                                  cens=cens[urval],
                                                  exposure=data$sex[urval] %in% "man",
                                                  exposure_levels=c("Men","Women")) %>% 
      mutate(label="pcol men vs women")
    
    
    
    ### FIT ### FIT ### 
    
    # all - FITx2 vs control
    urval <- fit | fit_control
    ret_temp[["all_fit_vs_control"]]<- extract_irr(timefu[urval],
                                                   cens[urval],
                                                   exposure=fit[urval],
                                                   exposure_levels=c("Fitx2","FITx2 Control"))%>% 
      mutate(label="all fitx2 vs control")
    
    
    # men
    urval <- (fit | fit_control ) & data$sex %in% "man"
    ret_temp[["men_fit_vs_control"]]<- extract_irr(timefu[urval],
                                                   cens[urval],
                                                   exposure=fit[urval],
                                                   exposure_levels=c("Fitx2","FITx2 Control"))%>% 
      mutate(label="men fitx2 vs control")
    
    
    # women
    urval <- (fit | fit_control ) & data$sex %in% "woman"
    ret_temp[["women_fit_vs_control"]]<- extract_irr(timefu[urval],
                                                     cens[urval],
                                                     exposure=fit[urval],
                                                     exposure_levels=c("Fitx2","FITx2 Control")) %>% 
      mutate(label="women fitx2 vs control")
    
    # men vs women in fit
    urval <- fit 
    ret_temp[["fit_men_vs_women"]]<- extract_irr(timefu[urval],
                                                 cens[urval],
                                                 exposure=data$sex[urval] %in% "man",
                                                 exposure_levels=c("Men","Women"))%>% 
      mutate(label="fit men vs women")
    
    
    
    
    
    ### PCOL VS FIT ### PCOL vs FIT ### 
    # men vs women in fit
    urval <- pcol | fit 
    ret_temp[["all pcol vs fit"]]<- extract_irr(timefu=timefu[urval],
                                                cens=cens[urval],
                                                exposure=pcol[urval],
                                                exposure_levels=c("PCOL","FITx2"))%>% 
      mutate(label="all pcol vs fit")
    
    
    urval <- ( pcol | fit ) & data$sex %in% "man"
    ret_temp[["men pcol vs fit"]]<- extract_irr(timefu[urval],
                                                cens[urval],
                                                exposure=pcol[urval],
                                                exposure_levels=c("PCOL","FITx2"))%>% 
      mutate(label="men pcol vs fit")
    
    
    urval <- ( pcol | fit ) & data$sex %in% "woman"
    ret_temp[["women pcol vs fit"]]<- extract_irr(timefu[urval],
                                                  cens[urval],
                                                  exposure=pcol[urval],
                                                  exposure_levels=c("PCOL","FITx2"))%>% 
      mutate(label="women pcol vs fit")
    
    
    ### Effect Modification ### Effect Modification ### 
    
    if(both_sexes){
      urval <- ( pcol  | control ) 
      ret_temp[["effect mod pcol"]]<- extract_effect_mod(timefu=timefu[urval],
                                                         cens=cens[urval],
                                                         exposure=pcol[urval],
                                                         exposure2=data$sex[urval] %in% "man")[1,] %>% 
        mutate(label="effect mod pcol")
      ret_temp[["effect mod pcol"]][,c( "N",     "n", "Proportion", "PersonYears",    "Rate", "Rate_CI_lower", "Rate_CI_upper" )] <- NA
      
      
      urval <- ( fit | fit_control ) 
      ret_temp[["effect mod fit"]]<- extract_effect_mod(timefu[urval],
                                                        cens[urval],
                                                        exposure=fit[urval],
                                                        exposure2=data$sex[urval] %in% "man")[1,] %>% 
        mutate(label="effect mod fit")
      ret_temp[["effect mod fit"]][,c( "N",     "n", "Proportion", "PersonYears",    "Rate", "Rate_CI_lower", "Rate_CI_upper" )] <- NA
    }
    

    overdisp_list[[ outcomes_cens[j] ]] <- lapply(FUN=function(x) attr(x,"overdisp"), ret_temp)
    
    ret[[j]] <- do.call(rbind,ret_temp) %>%
      mutate(outcome=outcomes_cens[j])
  }
  
  ret <- do.call(rbind,ret) %>%
    relocate(label,outcome,exposure,adjusted,N,n,Proportion,PersonYears,Rate,Rate_CI_lower,Rate_CI_upper,irr,irr_ci_lower,irr_ci_upper)
  
  rownames(ret) <- NULL
 
  
  attr(ret,"overdisp") <- overdisp_list
  return(ret)
}



##
# end of helper function
## 



sub_group_analyses <- list("all"=function(x) x %>% filter(TRUE) )

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
# Adverse events
outcomes_cens <- c("AECG_or_death_cens",
                   "Censor",
                   "AE_cens_AnyAECG",
                   "AE_cens_Cardiovascular",
                   "AE_cens_Gastrointestinal")

outcomes_cens <- c(outcomes_cens,colnames(D)[substr(colnames(D),1,8) %in% "AE_cens_" & !colnames(D) %in% outcomes_cens])

outcomes_timefu <- c("AECG_or_death_timefu",
                     "timefu",
                     "AE_timefu_AnyAECG",
                     "AE_timefu_Cardiovascular",
                     "AE_timefu_Gastrointestinal")
outcomes_timefu <- c(outcomes_timefu,colnames(D)[substr(colnames(D),1,10) %in% "AE_timefu_" & !colnames(D) %in% outcomes_timefu])

stopifnot(length(outcomes_timefu)==length(outcomes_cens))

# Original
main_outcomes_ae_unajd_orig <- list()

for(j in 1:length(sub_group_analyses)){
  print(names(sub_group_analyses)[j])
  main_outcomes_ae_unajd_orig[[j]] <- extract_outcome(data=sub_group_analyses[[j]](D),
                                                 outcomes_cens=outcomes_cens,
                                                 outcomes_timefu=outcomes_timefu,
                                                 glm.family="poisson",
                                                 bootci=FALSE) %>%
    mutate(subgroup_analysis=names(sub_group_analyses)[j])
}
main_outcomes_ae_unajd <- main_outcomes_ae_unajd_orig
names(main_outcomes_ae_unajd)<-names(sub_group_analyses)

save(main_outcomes_ae_unajd,file=".\\Resultat\\main_outcomes_ae_unajd.Rdata")

# Robust
main_outcomes_ae_unajd_robust <- list()

for(j in 1:length(sub_group_analyses)){
  print(names(sub_group_analyses)[j])
  main_outcomes_ae_unajd_robust[[j]] <- extract_outcome(data=sub_group_analyses[[j]](D),
                                                 outcomes_cens=outcomes_cens,
                                                 outcomes_timefu=outcomes_timefu,
                                                 glm.family="poisson",
                                                 bootci=FALSE,
                                                 robust.se = TRUE) %>%
    mutate(subgroup_analysis=names(sub_group_analyses)[j])
}
names(main_outcomes_ae_unajd_robust)<- "all"

overdisp <- attr(main_outcomes_ae_unajd_orig$all,"overdisp")
overdisp_ae <- list()
for(j in 1:length(overdisp)){
  overdisp_ae[[names(overdisp)[j]]]<-unlist( lapply(FUN=function(y) ifelse(!is.na(y),y$estimate,NA) ,overdisp[[j]]) )
}

save(main_outcomes_ae_unajd_orig,file=".\\Resultat\\main_outcomes_ae_unajd_orig.Rdata")
save(main_outcomes_ae_unajd,file=".\\Resultat\\main_outcomes_ae_unajd.Rdata")
save(overdisp_ae,file=".\\Resultat\\overdisp_ae.Rdata")



main_outcomes_crc_unajd <- list()
for(j in 1:length(sub_group_analyses)){
  print(names(sub_group_analyses)[j])
  
  main_outcomes_crc_unajd[[j]] <- extract_outcome(data=sub_group_analyses[[j]](D),
                                                  outcomes_cens=c("CRC_cens","CRC_cens_inside","CRC_cens_outside",
                                                                  "CRC_cens_stage12","CRC_cens_inside_stage12","CRC_cens_outside_stage12",
                                                                  "CRC_cens_stage34","CRC_cens_inside_stage34","CRC_cens_outside_stage34",
                                                                  "CRC_cens_stagemis","CRC_cens_inside_stagemis","CRC_cens_outside_stagemis"),
                                                  outcomes_timefu=c("CRC_timefu","CRC_timefu_inside","CRC_timefu_outside",
                                                                    "CRC_timefu","CRC_timefu_inside","CRC_timefu_outside",
                                                                    "CRC_timefu","CRC_timefu_inside","CRC_timefu_outside",
                                                                    "CRC_timefu","CRC_timefu_inside","CRC_timefu_outside")) %>%
    mutate(subgroup_analysis=names(sub_group_analyses)[j])

}
names(main_outcomes_crc_unajd)<-names(sub_group_analyses)

save(main_outcomes_crc_unajd,file=".\\Resultat\\main_outcomes_crc_unajd.Rdata")

overdisp <- attr(main_outcomes_crc_unajd[[1]],"overdisp")
overdisp_crc <- list()
for(j in 1:length(overdisp)){
  overdisp_crc[[names(overdisp)[j]]]<-unlist( lapply(FUN=function(y) ifelse(!is.na(y),y$estimate,NA) ,overdisp[[j]]) )
}
save(overdisp_crc,file=".\\Resultat\\overdisp_crc.Rdata")



###
# End
###