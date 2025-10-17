###
# Assess participation 
require(tidyverse)

load("S:/SCREESCO/Scripts/Clean_data/Output/list_dates.Rdata") # contains data on only those that participated in FIT

load("S:/SCREESCO/Scripts/Clean_data/Output/flows.Rdata") # contains all that were invited

load("S:/SCREESCO/Scripts/Clean_data/Output/randomisering.Rdata")

load("S:/SCREESCO/Scripts/Clean_data/Output/root.Rdata")

load("S:/SCREESCO/Scripts/Clean_data/Output/koloskopi_summary.Rdata")


###
# Randomized
table(df_randomisering$rand_arm)

###
# those that were invited to kol
kol <- df_flows %>%
  filter(rand_arm %in% "koloskopi") %>% 
  left_join(df_randomisering %>% select(persnr,rand_date),by="persnr") %>%
  left_join(sum_koloskopi %>% filter(studiearm %in% "koloskopi") %>% select(persnr,koloskopidatum),by="persnr" ) %>%
  mutate(time2inv= as.numeric(first_utskick - rand_date),
         time2kol= as.numeric(koloskopidatum - rand_date),
         time_inv2kol= as.numeric(koloskopidatum - first_utskick))

table(!is.na(kol$koloskopidatum))
table(!is.na(kol$first_utskick))

length(unique(list_dates$persnr))

r1 <- list_dates %>% 
  filter(date_type %in% "utskicksdatum" & round==1)
length(unique(r1$persnr))

###
# FIT
fit <- df_root %>% 
  filter(studiearm %in% "fit") %>%
  select(persnr,utskicksdatum,round) %>%
  unique() %>%
  left_join(df_randomisering %>% select(persnr,rand_date),by="persnr") 

length(unique(fit$persnr))
table(fit$round,useNA="always")
length(unique(fit$persnr[fit$round ==1]))

# FIT round 1 participated
fit1 <- fit %>%
  filter(round %in% 1 )
length(unique(fit1$persnr))

# those that participated in FIT round 1 (performed a FIT)
fit1_ad <- fit1 %>%
  left_join(list_dates %>% 
              filter(round %in% 1) %>%
              filter(date_type %in% "provtagdatum") %>%
              select(persnr,date) %>%
              group_by(persnr) %>%
              slice(1) %>%
              ungroup() %>%
              rename(fitdate_r1=date),by="persnr") %>%
  left_join(list_dates %>% 
              filter(round %in% 1) %>%
              filter(date_type %in% "koloskopidatum") %>%
              select(persnr,date) %>%
              group_by(persnr) %>%
              slice(1) %>%
              ungroup() %>%
              rename(koldate_r1=date),by="persnr") %>%
  filter(!is.na(fitdate_r1))

length(unique(fit1_ad$persnr))

table(df_flows$fit_positiv_runda1[df_flows$persnr %in% fit1_ad$persnr])
length(unique(   fit1_ad$persnr[ !is.na(fit1_ad$koldate_r1) ]))
table(df_flows$kol_utford_runda1[df_flows$persnr %in% fit1_ad$persnr])


###
# FIT Round 2 

# invited
fit2 <- fit %>%
  filter(round %in% 2 )

length(unique(fit2$persnr)) 


summary(fit2$time2inv /365 )
table(fit2$time2inv > 365*2,fit2$round,useNA="always")

length(unique(fit1$persnr)) 


# those that participated in FIT round 2
fit2_ad <- fit2 %>%
  left_join(list_dates %>% 
              filter(round %in% 2) %>%
              filter(date_type %in% "provtagdatum") %>%
              select(persnr,date) %>%
              group_by(persnr) %>%
              slice(1) %>%
              ungroup() %>%
              rename(fitdate_r2=date),by="persnr") %>%
  left_join(list_dates %>% 
              filter(round %in% 2) %>%
              filter(date_type %in% "koloskopidatum") %>%
              select(persnr,date) %>%
              group_by(persnr) %>%
              slice(1) %>%
              ungroup() %>%
              rename(koldate_r2=date),by="persnr") %>%
  filter(!is.na(fitdate_r2))

length(unique(fit2_ad$persnr))

table(df_flows$fit_positiv_runda2[df_flows$persnr %in% fit2_ad$persnr])
length(unique(   fit2_ad$persnr[ !is.na(fit2_ad$koldate_r2) ]))
table(df_flows$kol_utford_runda2[df_flows$persnr %in% fit2_ad$persnr])

length(unique(fit2_ad$persnr[fit2_ad$persnr %in% fit1_ad$persnr]))

