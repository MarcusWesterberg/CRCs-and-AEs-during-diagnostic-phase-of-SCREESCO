###
# IRR Figure


irr_f <- function(D,
                  years=1,
                  rate_scale=1){
  library("sandwich")
  
  timefu <- D$CRC_timefu / 365.24
  
  cens <- as.numeric((D$CRC_cens))
  
  early_cens <- timefu > years
  
  timefu[early_cens] <- years
  cens[early_cens]<-0
  
  exposure <- as.numeric(!D$rand_arm %in% "control")
  
  extract_rate <- function(cens,timefu,rate_scale=1){
    rate <- sum(cens)/sum(timefu)*rate_scale
    
    if(sum(cens)>4){
      m <- glm(cens ~ offset(log(timefu)), family=poisson)
      rate_ci <- exp( confint.default( m ) )*rate_scale
      rse <- sqrt(diag(vcovHC(m, type="HC0"))) 
      sandwich_ci <- exp( (m$coefficients + rse*c(qnorm(0.025),qnorm(0.975))) )*rate_scale
    } else{
      rate_ci <- c(NA,NA)
    }
    return(c(rate,rate_ci,sandwich_ci))
  }
  
  rate_exp <- extract_rate(cens[exposure==1],timefu[exposure==1],rate_scale=rate_scale)
  rate_contr <- extract_rate(cens[exposure==0],timefu[exposure==0],rate_scale=rate_scale)
  
  m <- glm(cens ~ offset(log(timefu)) + exposure, family=poisson)
  
  irr <- exp( coefficients(m)[2] )
  ci <- exp( confint.default( m , parm = 2) )
  
  
  rse <- sqrt(diag(vcovHC(m, type="HC0")))[2]
  sandwich_ci <- exp( (m$coefficients[2] + rse*c(qnorm(0.025),qnorm(0.975))) )
  
  return(data.frame("years"=years,"irr"=irr,"cil"=ci[1],"ciu"=ci[2],"sandwich_cil"=sandwich_ci[1],"sandwich_ciu"=sandwich_ci[2],
                    "rate_exp"=rate_exp[1],"rate_exp_cil"=rate_exp[2],"rate_exp_ciu"=rate_exp[3],"rate_exp_sandwich_cil"=rate_exp[4],"rate_exp_sandwich_ciu"=rate_exp[5],
                    "rate_contr"=rate_contr[1],"rate_contr_cil"=rate_contr[2],"rate_contr_ciu"=rate_contr[3],"rate_contr_sandwich_cil"=rate_contr[4],"rate_contr_sandwich_ciu"=rate_contr[5]))
}



plot_f <- function(irr,main,ylim,ny=6){
  temp <- irr[1,]
  temp$years <- 0
  temp$irr <- 1
  temp$cil <-1
  temp$ciu <- 1
  irr <- rbind(temp,irr)
  
  plot(x=irr$years,
       y=irr$irr,
       type="n",
       lwd=2,
       ylim=ylim,
       xlim=c(1,7),
       xlab="",
       ylab="",
       main=main,
       cex.axis=1.5,
       axes=FALSE)
  axis(side=1,at=1:7,line=-0.5, cex.axis=1.5)
  yat <- seq(ylim[1],ylim[2],length.out = ny)
  axis(side=2,at=yat,line=0, cex.axis=1.5)
  
  mtext(side=1,line=2,"Years since randomization",cex=1)
  mtext(side=2,line=2.5,"Rate ratio",cex=1)
  
  for(j in 1:7){
    lines(x=c(j,j),y=ylim,lty=2,col="lightgrey")
  }
  for(j in 1:length(yat)){
    lines(x=c(1,7),y=c(yat[j],yat[j]),lty=2,col="lightgrey")
  }
  
  lines(x=c(0,ceiling(max(irr$years))),y=c(1,1),col="grey")

  
  
  why <- irr$years >0
  irr <- irr[why,]
  
  ci_width <- 0.2
  
  lines(x=irr$years,y=irr$irr,col="black",lwd=1.5)
  points(irr$years,irr$irr,pch=20,col="black",cex=2)
  
  for(k in 1:nrow(irr)){
    lines(x=c(irr$years[k],irr$years[k]),
          y=c(irr$cil[k],irr$ciu[k]),
          col="black",
          lwd=1,xpd=NA)
    
    lines(x=c(irr$years[k]-ci_width,irr$years[k]+ci_width),
          y=c(irr$cil[k],irr$cil[k]),
          col="black",
          lwd=1,xpd=NA)
    
    lines(x=c(irr$years[k]-ci_width,irr$years[k]+ci_width),
          y=c(irr$ciu[k],irr$ciu[k]),
          col="black",
          lwd=1,xpd=NA)
  }
  
  
}

plot_rate <- function(rate,
                      main,
                      ylim,
                      add=FALSE,
                      col=1,
                      cicol="grey",
                      ny=5){
  temp <- rate[1,]
  temp$years <- 0
  temp$rate <- 0
  temp$cil <-0
  temp$ciu <- 0
  rate <- rbind(temp,rate)
  
  if(!add){
    plot(x=rate$years,
         y=rate$rate,
         type="n",
         lwd=2,
         ylim=ylim,
         xlim=c(1,7),
         xlab="",
         ylab="",
         main=main,
         axes=FALSE,
         cex.axis=1.5)
    axis(side=1,at=1:7,line=-0.5, cex.axis=1.5)
    yat <- seq(ylim[1],ylim[2],length.out = ny)
    axis(side=2,at=yat,line=0, cex.axis=1.5)
    
    mtext(side=1,line=2,"Years since randomization",cex=1)
    mtext(side=2,line=2.5,"Rate",cex=1)
    
    for(j in 1:7){
      lines(x=c(j,j),y=ylim,lty=2,col="lightgrey")
    }
    for(j in 1:length(yat)){
      lines(x=c(1,7),y=c(yat[j],yat[j]),lty=2,col="lightgrey")
    }
  } 
  
  why <- rate$years >0
  rate <- rate[why,]
  
  ci_width <- 0.2
  
  lines(x=rate$years,y=rate$rate,col=col,lwd=1.5)
  points(rate$years,rate$rate,pch=20,col=col,cex=2)
  
  for(k in 1:nrow(rate)){
    lines(x=c(rate$years[k],rate$years[k]),
          y=c(rate$cil[k],rate$ciu[k]),
          col=col,lwd=1,xpd=NA)
    lines(x=c(rate$years[k]-ci_width,rate$years[k]+ci_width),
          y=c(rate$cil[k],rate$cil[k]),
          col=col,lwd=1,xpd=NA)
    lines(x=c(rate$years[k]-ci_width,rate$years[k]+ci_width),
          y=c(rate$ciu[k],rate$ciu[k]),
          col=col,lwd=1,xpd=NA)
  }

  
}


extract_f_r <- function(Dtemp,
                        ylim=c(0,2.5),
                        ylim_rate=c(0,1),
                        rate_scale=1,
                        ny=c(5,6)){
  
  years <- c(seq(1,6,1),max(Dtemp$CRC_timefu)/ 365.24)
  
  irr_list_pcol <- list()
  irr_list_fit <- list()
  
  for(j in 1:length(years)){
    print(j)
    irr_list_pcol[[j]] <- irr_f(D=Dtemp %>% filter(rand_arm %in% c("dk","control")) ,
                                years=years[j],
                                rate_scale=rate_scale)
    
    irr_list_fit[[j]] <- irr_f(D=Dtemp %>% filter(rand_arm %in% c("fit") | is_fit_control ),
                               years=years[j],
                               rate_scale=rate_scale)
  }
  
  irr_pcol <- do.call(rbind,irr_list_pcol)
  irr_fit <- do.call(rbind,irr_list_fit)

  col_contr <- rgb(0,0,0.4,alpha=0.5)
  col_contr_ci <- rgb(0,0,0.9,alpha=0.25)
  
  col_exp <- rgb(0,0.25,0,alpha=0.5)
  col_exp_ci <- rgb(0,0.7,0,alpha=0.25)

  plot_rate(rate=irr_pcol %>% 
              select(years,rate_exp,rate_exp_cil,rate_exp_ciu) %>% 
              rename(rate=rate_exp,cil=rate_exp_cil,ciu=rate_exp_ciu),
            ylim=ylim_rate,
            main="",
            col=col_exp,
            cicol=col_exp_ci,
            ny=ny[1])
  
  plot_rate(rate=irr_pcol %>% 
              select(years,rate_contr,rate_contr_cil,rate_contr_ciu) %>% 
              rename(rate=rate_contr,cil=rate_contr_cil,ciu=rate_contr_ciu),
            ylim=ylim_rate,
            main="",
            add=TRUE,
            col=col_contr,
            cicol=col_contr_ci,
            ny=ny[1])
  
  plot_f(irr=irr_pcol,
         main=paste0(""),
         ylim=ylim,
         ny=ny[2])
  
  
  plot_rate(rate=irr_fit %>% 
              select(years,rate_exp,rate_exp_cil,rate_exp_ciu) %>% 
              rename(rate=rate_exp,cil=rate_exp_cil,ciu=rate_exp_ciu),
            ylim=ylim_rate,
            main="",
            col=col_exp,
            cicol=col_exp_ci,
            ny=ny[1])
  
  plot_rate(rate=irr_fit %>% 
              select(years,rate_contr,rate_contr_cil,rate_contr_ciu) %>% 
              rename(rate=rate_contr,cil=rate_contr_cil,ciu=rate_contr_ciu),
            ylim=ylim_rate,
            main="",
            add=TRUE,
            col=col_contr,
            cicol=col_contr_ci,
            ny=ny[1])
  
  plot_f(irr=irr_fit,
         main=paste0(""),
         ylim=ylim,
         ny=ny[2])
  
  irr_time_table <- list("PCOL"=irr_pcol,
                         "FIT"=irr_fit)
  return(irr_time_table)
}

width <- 9
height <- 3.3

svg(file=".\\Resultat\\IRR_figure_ae.svg",width=width,height=height)
# adverse events
par(mfrow=c(1,4),mar=c(4,4,3,1))
irr_time_table <- extract_f_r(Dtemp=D %>% mutate(CRC_timefu=AE_timefu_AnyAECG,CRC_cens=AE_cens_AnyAECG),
                              ylim=c(0.5,1.5),
                              ylim_rate=c(0,2500),
                              rate_scale=100000,
                              ny=c(6,5))
openxlsx::write.xlsx(irr_time_table,file=".\\Resultat\\IRR_timetable_aecg.xlsx")
dev.off()


svg(file=".\\Resultat\\IRR_figure_ae_cvd.svg",width=width,height=height)
# adverse events
par(mfrow=c(1,4),mar=c(4,4,3,1))
irr_time_table <- extract_f_r(Dtemp=D %>% mutate(CRC_timefu=AE_timefu_Cardiovascular,CRC_cens=AE_cens_Cardiovascular),
                              ylim=c(0.5,1.5),
                              ylim_rate=c(0,2500),
                              rate_scale=100000,
                              ny=c(6,5))
openxlsx::write.xlsx(irr_time_table,file=".\\Resultat\\IRR_timetable_aecg.xlsx")
dev.off()


svg(file=".\\Resultat\\IRR_figure_ae_gi.svg",width=width,height=height)
# adverse events
par(mfrow=c(1,4),mar=c(4,4,3,1))
irr_time_table <- extract_f_r(Dtemp=D %>% mutate(CRC_timefu=AE_timefu_Gastrointestinal,CRC_cens=AE_cens_Gastrointestinal),
                              ylim=c(0.5,1.5),
                              ylim_rate=c(0,500),
                              rate_scale=100000,
                              ny=c(6,5))
openxlsx::write.xlsx(irr_time_table,file=".\\Resultat\\IRR_timetable_aecg.xlsx")
dev.off()



svg(file=".\\Resultat\\IRR_figure_death.svg",width=width,height=height)

# Death
par(mfrow=c(1,4),mar=c(4,4,3,1))
irr_time_table <- extract_f_r(Dtemp=D %>% mutate(CRC_timefu=timefu,CRC_cens=Censor),
                              ylim=c(0.5,1.5),
                              ylim_rate=c(0,750),
                              rate_scale=100000,
                              ny=c(6,5))
openxlsx::write.xlsx(irr_time_table,file=".\\Resultat\\IRR_timetable_death.xlsx")
dev.off()




svg(file=".\\Resultat\\IRR_figure_crc.svg",width=width,height=height)
# CRC
par(mfrow=c(1,4),mar=c(4,4,3,1))
irr_time_table <- extract_f_r(Dtemp=D %>% mutate(CRC_timefu=CRC_timefu,CRC_cens=CRC_cens),
                              ylim=c(0.5,3),
                              ylim_rate=c(0,200),
                              rate_scale=100000,
                              ny=c(5,6))
openxlsx::write.xlsx(irr_time_table,file=".\\Resultat\\IRR_timetable_crc_all.xlsx")
dev.off()


svg(file=".\\Resultat\\IRR_figure_crc_stage12.svg",width=width,height=height)
# CRC
par(mfrow=c(1,4),mar=c(4,4,3,1))
irr_time_table <- extract_f_r(Dtemp=D %>% mutate(CRC_timefu=CRC_timefu_stage12,CRC_cens=CRC_cens_stage12 ),
                              ylim=c(0.5,3),
                              ylim_rate=c(0,100),
                              rate_scale=100000,
                              ny=c(5,6))
openxlsx::write.xlsx(irr_time_table,file=".\\Resultat\\IRR_timetable_crc_12.xlsx")
dev.off()



svg(file=".\\Resultat\\IRR_figure_crc_stage34.svg",width=width,height=height)
# CRC
par(mfrow=c(1,4),mar=c(4,4,3,1))
irr_time_table <- extract_f_r(Dtemp=D %>% mutate(CRC_timefu=CRC_timefu_stage34,CRC_cens=CRC_cens_stage34 ),
                              ylim=c(0.5,3),
                              ylim_rate=c(0,100),
                              rate_scale=100000,
                              ny=c(5,6))
openxlsx::write.xlsx(irr_time_table,file=".\\Resultat\\IRR_timetable_crc_34.xlsx")
dev.off()






###
# End
###