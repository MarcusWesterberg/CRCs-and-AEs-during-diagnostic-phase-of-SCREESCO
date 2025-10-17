###
# Baseline characteristics 

table_1 <- list()

dig=1
dig_percent=1

C = cbind(D$rand_arm %in% "dk",
          D$rand_arm %in% "control",
          D$rand_arm %in% "fit",
          D$rand_arm %in% "control" & D$is_fit_control
          )

DD <- D 

cn <- c("PCOL arm",
        "Control arm",
        "FITx2 arm",
        "Control arm (FITx2)")

# N 

DD$N <- TRUE
table_1$N   <- table_wrap(X=DD,
                          varname="N",
                          variable_name="N",
                          C=C,
                          cn=cn,
                          continuous=FALSE,
                          cut_it=FALSE,
                          breaks=NULL,
                          labels=NULL,
                          levels=NULL,
                          add_info=FALSE,
                          only_true=TRUE,
                          missing=FALSE,
                          percent=TRUE,
                          dig=dig,
                          dig_percent=dig_percent,
                          add_lab = "")

table_1$N <- table_1$N[2,]
table_1$N[1] <- "N"



# Sex
table_1$sex <- table_wrap(X=DD,
                              varname="sex",
                              variable_name="Sex",
                              C=C,
                              cn=cn,
                              continuous=FALSE,
                              labels=c("Man","Woman"),
                              levels=c("man","woman"),
                              add_info=FALSE,
                              only_true=FALSE,
                              missing=FALSE,
                              percent=TRUE,
                              dig=dig,
                              dig_percent=dig_percent,
                              add_lab = "")


# Year of randomization
table_1$year <- table_wrap(X=DD %>% mutate(rand_y=as.numeric(substr(rand_date,1,4))),
                           varname="rand_y",
                           variable_name="Year of randomization",
                           C=C,
                           cn=cn,
                           continuous=FALSE,
                           labels=c(2014:2018),
                           levels=c(2014:2018),
                           add_info=FALSE,
                           only_true=FALSE,
                           missing=FALSE,
                           percent=TRUE,
                           dig=dig,
                           dig_percent=dig_percent,
                           add_lab = "")


# Health care region
table_1$reg <- table_wrap(X=DD,
                           varname="rand_sjukvreg",
                           variable_name="Health care region of residence",
                           C=C,
                           cn=cn,
                           continuous=FALSE,
                           labels=c("North","Central","Southeast","South","West"),
                           levels=c("North","Central","Southeast","South","West"),
                           add_info=FALSE,
                           only_true=FALSE,
                           missing=FALSE,
                           percent=TRUE,
                           dig=dig,
                           dig_percent=dig_percent,
                           add_lab = "")

# Education
table_1$edu <- table_wrap(X=DD,
                          varname="Edu",
                          variable_name="Educational level",
                          C=C,
                          cn=cn,
                          continuous=FALSE,
                          labels=c("Low","Intermediate","High"),
                          levels=c("low","intermediate","high"),
                          add_info=FALSE,
                          only_true=FALSE,
                          missing=TRUE,
                          percent=TRUE,
                          dig=dig,
                          dig_percent=dig_percent,
                          add_lab = "")

# origin
table_1$origin <- table_wrap(X=DD,
                          varname="BornSwed",
                          variable_name="Swedish origin",
                          C=C,
                          cn=cn,
                          continuous=FALSE,
                          labels=c("No","Yes"),
                          levels=c("FALSE","TRUE"),
                          add_info=FALSE,
                          only_true=FALSE,
                          missing=FALSE,
                          percent=TRUE,
                          dig=dig,
                          dig_percent=dig_percent,
                          add_lab = "")


# CCI
table_1$cci <- table_wrap(X=DD %>% mutate(cci=cut(cci,breaks=c(0,1,2,3,Inf),include.lowest = TRUE,right=FALSE,labels =c("0","1","2","3+") )),
                          varname="cci",
                          variable_name="Charlson Comorbidity Index",
                          C=C,
                          cn=cn,
                          continuous=FALSE,
                          labels=c("0","1","2","3+"),
                          levels=c("0","1","2","3+"),
                          add_info=FALSE,
                          only_true=FALSE,
                          missing=FALSE,
                          percent=TRUE,
                          dig=dig,
                          dig_percent=dig_percent,
                          add_lab = "")


dci_quartiles <- quantile(DD$dci,probs=c(0.25,0.5,0.75))
quantile(DD$dci[DD$rand_arm %in% "control"],probs=c(0.25,0.5,0.75))

# DCI 
table_1$dci <- table_wrap(X=DD %>% mutate(dci=cut(dci,breaks=c(-Inf,dci_quartiles,Inf),include.lowest = TRUE,right=FALSE,labels =c("<Q1","Q1-Q2","Q2-Q3",">=Q3") )),
                          varname="dci",
                          variable_name="Drug Comorbidity Index",
                          C=C,
                          cn=cn,
                          continuous=FALSE,
                          labels=NULL,
                          levels=NULL,
                          add_info=FALSE,
                          only_true=FALSE,
                          missing=FALSE,
                          percent=TRUE,
                          dig=dig,
                          dig_percent=dig_percent,
                          add_lab = "")


# History of Adverse events 
table_1$ae <- table_wrap(X=DD,
                          varname="AE_before_10_AnyAECG",
                          variable_name="History of cardiovascular or gastrointestinal event",
                          C=C,
                          cn=cn,
                          continuous=FALSE,
                          labels=c("No","Yes"),
                          levels=c("FALSE","TRUE"),
                          add_info=FALSE,
                          only_true=FALSE,
                          missing=FALSE,
                          percent=TRUE,
                          dig=dig,
                          dig_percent=dig_percent,
                          add_lab = "")


table_1$ae_cv <- table_wrap(X=DD,
                            varname="AE_before_10_Cardiovascular",
                            variable_name="History of cardiovascular or gastrointestinal event",
                            C=C,
                            cn=cn,
                            continuous=FALSE,
                            labels=c("No","Yes"),
                            levels=c("FALSE","TRUE"),
                            add_info=FALSE,
                            only_true=FALSE,
                            missing=FALSE,
                            percent=TRUE,
                            dig=dig,
                            dig_percent=dig_percent,
                            add_lab = "")[c(3),]
table_1$ae_cv[1] <- "History of cardiovascular event"

table_1$ae_ge <- table_wrap(X=DD,
                            varname="AE_before_10_Gastrointestinal",
                            variable_name="History of Gastrointestinal event",
                            C=C,
                            cn=cn,
                            continuous=FALSE,
                            labels=c("No","Yes"),
                            levels=c("FALSE","TRUE"),
                            add_info=FALSE,
                            only_true=FALSE,
                            missing=FALSE,
                            percent=TRUE,
                            dig=dig,
                            dig_percent=dig_percent,
                            add_lab = "")[c(3),]
table_1$ae_ge[1] <- "History of gastrointestinal event"

# table_1$ae_other <- table_wrap(X=DD,
#                                varname="AE_before_10_Other",
#                                variable_name="History of other adverse event",
#                                C=C,
#                                cn=cn,
#                                continuous=FALSE,
#                                labels=c("No","Yes"),
#                                levels=c("FALSE","TRUE"),
#                                add_info=FALSE,
#                                only_true=FALSE,
#                                missing=FALSE,
#                                percent=TRUE,
#                                dig=dig,
#                                dig_percent=dig_percent,
#                                add_lab = "")[c(3),]
# table_1$ae_other[1] <- "History of other adverse event"

table_1 <- do.call(rbind,table_1)
table_1 <- as.data.frame(table_1)
openxlsx::write.xlsx(table_1,file=".\\Resultat\\Table1.xlsx")





























# 
# ###
# # Baseline characteristics 
# 
# table_1 <- list()
# 
# dig=1
# dig_percent=1
# 
# C = cbind(D_col$rand_arm %in% "dk",
#           D_col$rand_arm %in% "fit")
# 
# DD <- D_col 
# 
# cn <- c("PCOL arm",
#         "FITx2 arm")
# 
# # N 
# 
# DD$N <- TRUE
# table_1$N   <- table_wrap(X=DD,
#                           varname="N",
#                           variable_name="N",
#                           C=C,
#                           cn=cn,
#                           continuous=FALSE,
#                           cut_it=FALSE,
#                           breaks=NULL,
#                           labels=NULL,
#                           levels=NULL,
#                           add_info=FALSE,
#                           only_true=TRUE,
#                           missing=FALSE,
#                           percent=TRUE,
#                           dig=dig,
#                           dig_percent=dig_percent,
#                           add_lab = "")
# 
# table_1$N <- table_1$N[2,]
# table_1$N[1] <- "N"
# 
# 
# 
# # Sex
# table_1$sex <- table_wrap(X=DD,
#                           varname="sex",
#                           variable_name="Sex",
#                           C=C,
#                           cn=cn,
#                           continuous=FALSE,
#                           labels=c("Man","Woman"),
#                           levels=c("man","woman"),
#                           add_info=FALSE,
#                           only_true=FALSE,
#                           missing=FALSE,
#                           percent=TRUE,
#                           dig=dig,
#                           dig_percent=dig_percent,
#                           add_lab = "")
# 
# # Age at colonoscopy
# table_1$age <- table_wrap(X=DD ,
#                           varname="col_age",
#                           variable_name="Age at colonoscopy",
#                           C=C,
#                           cn=cn,
#                           continuous=TRUE,
#                           cut_it=TRUE,
#                           breaks=c(0,62,Inf),
#                           labels=c("59-61","62-65"),
#                           levels=NULL,
#                           add_info=TRUE,
#                           only_true=FALSE,
#                           missing=FALSE,
#                           percent=TRUE,
#                           dig=dig,
#                           dig_percent=dig_percent,
#                           add_lab = "")
# 
# 
# 
# 
# # Year of randomization
# table_1$year <- table_wrap(X=DD %>% mutate(rand_y=as.numeric(substr(rand_date,1,4))),
#                            varname="rand_y",
#                            variable_name="Year of randomization",
#                            C=C,
#                            cn=cn,
#                            continuous=FALSE,
#                            labels=c(2014:2018),
#                            levels=c(2014:2018),
#                            add_info=FALSE,
#                            only_true=FALSE,
#                            missing=FALSE,
#                            percent=TRUE,
#                            dig=dig,
#                            dig_percent=dig_percent,
#                            add_lab = "")
# 
# 
# # Health care region
# table_1$reg <- table_wrap(X=DD,
#                           varname="rand_sjukvreg",
#                           variable_name="Health care region of residence",
#                           C=C,
#                           cn=cn,
#                           continuous=FALSE,
#                           labels=c("North","Central","Southeast","South","West"),
#                           levels=c("North","Central","Southeast","South","West"),
#                           add_info=FALSE,
#                           only_true=FALSE,
#                           missing=FALSE,
#                           percent=TRUE,
#                           dig=dig,
#                           dig_percent=dig_percent,
#                           add_lab = "")
# 
# # Education
# table_1$edu <- table_wrap(X=DD,
#                           varname="Edu",
#                           variable_name="Educational level",
#                           C=C,
#                           cn=cn,
#                           continuous=FALSE,
#                           labels=c("Low","Intermediate","High"),
#                           levels=c("low","intermediate","high"),
#                           add_info=FALSE,
#                           only_true=FALSE,
#                           missing=FALSE,
#                           percent=TRUE,
#                           dig=dig,
#                           dig_percent=dig_percent,
#                           add_lab = "")
# 
# # origin
# table_1$origin <- table_wrap(X=DD,
#                              varname="BornSwed",
#                              variable_name="Swedish origin",
#                              C=C,
#                              cn=cn,
#                              continuous=FALSE,
#                              labels=c("No","Yes"),
#                              levels=c("FALSE","TRUE"),
#                              add_info=FALSE,
#                              only_true=FALSE,
#                              missing=FALSE,
#                              percent=TRUE,
#                              dig=dig,
#                              dig_percent=dig_percent,
#                              add_lab = "")
# 
# 
# # CCI
# table_1$cci <- table_wrap(X=DD %>% mutate(cci=cut(cci,breaks=c(0,1,2,3,Inf),include.lowest = TRUE,right=FALSE,labels =c("0","1","2","3+") )),
#                           varname="cci",
#                           variable_name="Charlson Comorbidity Index",
#                           C=C,
#                           cn=cn,
#                           continuous=FALSE,
#                           labels=c("0","1","2","3+"),
#                           levels=c("0","1","2","3+"),
#                           add_info=FALSE,
#                           only_true=FALSE,
#                           missing=FALSE,
#                           percent=TRUE,
#                           dig=dig,
#                           dig_percent=dig_percent,
#                           add_lab = "")
# 
# 
# #dci_quartiles <- quantile(DD$dci,probs=c(0.25,0.5,0.75))
# 
# # DCI 
# table_1$dci <- table_wrap(X=DD %>% mutate(dci=cut(dci,breaks=c(-Inf,dci_quartiles,Inf),include.lowest = TRUE,right=FALSE,labels =c("<Q1","Q1-Q2","Q2-Q3",">=Q3") )),
#                           varname="dci",
#                           variable_name="Drug Comorbidity Index",
#                           C=C,
#                           cn=cn,
#                           continuous=FALSE,
#                           labels=NULL,
#                           levels=NULL,
#                           add_info=FALSE,
#                           only_true=FALSE,
#                           missing=FALSE,
#                           percent=TRUE,
#                           dig=dig,
#                           dig_percent=dig_percent,
#                           add_lab = "")
# 
# #table(DD$AE_before_10_PulmonaryEmbolism,useNA="always")
# #table(DD$AE_before_10_AnyAE,useNA="always")
# # History of Adverse events 
# table_1$ae <- table_wrap(X=DD,
#                          varname="AECG_before_10_AnyAE",
#                          variable_name="History of adverse event",
#                          C=C,
#                          cn=cn,
#                          continuous=FALSE,
#                          labels=c("No","Yes"),
#                          levels=c("FALSE","TRUE"),
#                          add_info=FALSE,
#                          only_true=FALSE,
#                          missing=FALSE,
#                          percent=TRUE,
#                          dig=dig,
#                          dig_percent=dig_percent,
#                          add_lab = "")
# 
# table_1$ae_cv <- table_wrap(X=DD,
#                          varname="AE_before_10_Cardiovascular",
#                          variable_name="History of cardiovascular event",
#                          C=C,
#                          cn=cn,
#                          continuous=FALSE,
#                          labels=c("No","Yes"),
#                          levels=c("FALSE","TRUE"),
#                          add_info=FALSE,
#                          only_true=FALSE,
#                          missing=FALSE,
#                          percent=TRUE,
#                          dig=dig,
#                          dig_percent=dig_percent,
#                          add_lab = "")[c(3),]
# table_1$ae_cv[1] <- "History of cardiovascular event"
# 
# table_1$ae_ge <- table_wrap(X=DD,
#                             varname="AE_before_10_Gastrointestinal",
#                             variable_name="History of Gastrointestinal event",
#                             C=C,
#                             cn=cn,
#                             continuous=FALSE,
#                             labels=c("No","Yes"),
#                             levels=c("FALSE","TRUE"),
#                             add_info=FALSE,
#                             only_true=FALSE,
#                             missing=FALSE,
#                             percent=TRUE,
#                             dig=dig,
#                             dig_percent=dig_percent,
#                             add_lab = "")[c(3),]
# table_1$ae_ge[1] <- "History of gastrointestinal event"
# # 
# # table_1$ae_other <- table_wrap(X=DD,
# #                             varname="AE_before_10_Other",
# #                             variable_name="History of other adverse event",
# #                             C=C,
# #                             cn=cn,
# #                             continuous=FALSE,
# #                             labels=c("No","Yes"),
# #                             levels=c("FALSE","TRUE"),
# #                             add_info=FALSE,
# #                             only_true=FALSE,
# #                             missing=FALSE,
# #                             percent=TRUE,
# #                             dig=dig,
# #                             dig_percent=dig_percent,
# #                             add_lab = "")[c(3),]
# # table_1$ae_other[1] <- "History of other adverse event"
# 
# table_1 <- do.call(rbind,table_1)
# table_1 <- as.data.frame(table_1)
# openxlsx::write.xlsx(table_1,file=".\\Resultat\\SecondaryAnalysis_Table1.xlsx")









###
# End
###