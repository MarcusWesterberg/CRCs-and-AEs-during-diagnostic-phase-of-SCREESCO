###
# Cumulative incidence CRC with stage and death

# Death
D$timefu_crc_death <- D$timefu 
D$cens_crc_death <- D$Censor*4

# CRC stage 1-2 before death 
urval <- D$CRC_timefu_stage12 < D$timefu_crc_death & D$CRC_cens_stage12 %in% 1
table(urval)
D$timefu_crc_death[urval] <- D$CRC_timefu_stage12[urval]
D$cens_crc_death[urval]  <- 1

# CRC stage 3-4 before death
urval <- D$CRC_timefu_stage34 < D$timefu_crc_death & D$CRC_cens_stage34 %in% 1
table(urval)
D$timefu_crc_death[urval] <- D$CRC_timefu_stage34[urval]
D$cens_crc_death[urval]  <- 2

# CRC stage missing but before death
urval <- D$CRC_timefu < D$timefu_crc_death & D$CRC_cens %in% 1
table(urval)
D$timefu_crc_death[urval] <- D$CRC_timefu[urval]
D$cens_crc_death[urval]  <- 3

table(D$cens_crc_death,D$rand_arm)


# Cumulative incidence CRC without stage and death

# Death
D$timefu_crc_death2 <- D$timefu 
D$cens_crc_death2 <- D$Censor*2

# CRC before death
urval <- D$CRC_timefu < D$timefu_crc_death2 & D$CRC_cens %in% 1
table(urval)
D$timefu_crc_death2[urval] <- D$CRC_timefu[urval]
D$cens_crc_death2[urval]  <- 1

table(D$cens_crc_death2,D$rand_arm)



compute_surv_curves <- function(data,
                                strata_variable=NULL,
                                time_points=NULL, 
                                max.time = 20){
  
  stopifnot(all(c("timefu","cens") %in% colnames(data)))
  
  if(!is.null(strata_variable)){
    strata_levels <- levels(data[[strata_variable]])
  } else{
    strata_levels <- NULL
  }
  print(strata_levels)
  
  
  if(is.null(time_points)){
    time_points <- sort(unique(c(0,data$timefu)))
  } else{
    time_points <- sort(unique(c(0,time_points)))
  }
  
  
  # helper function
  compute_curve <- function(data,
                            time_points,
                            max.time){
    require("cmprsk")
    cens_levels <- sort(unique(data$cens))
    stopifnot(length(cens_levels)>=2)
    stopifnot(cens_levels[1]==0 & cens_levels[2]==1 )
    
    data$cens[data$timefu>max.time]<-0
    data$timefu[data$timefu>max.time] <- max.time
    
    # competing risk
    surv_curves <- cuminc(ftime = data$timefu,
                          fstatus = data$cens)
    
    surv_curves <- timepoints(surv_curves,times=time_points)
    
    surv_curves$est <- t(surv_curves$est)
    surv_curves$var <- t(surv_curves$var)
    
    
    se = sqrt(surv_curves$var) 
    z = 1.96
    ci_upper <- surv_curves$est * exp(-z * se/(surv_curves$est * log(surv_curves$est)))
    ci_lower = surv_curves$est * exp(z * se/(surv_curves$est * log(surv_curves$est)))
    
    surv_curves$ci_lower = ci_lower
    surv_curves$ci_upper = ci_upper
    
    # numbers at risk 
    n_at_risk <- sapply(FUN=function(x) sum(data$timefu>=x), time_points )
    
    surv_curves$n_at_risk <- data.frame("time"=time_points,
                                        "n"=n_at_risk)
    
    return(surv_curves)
  }
  
  # stratified 
  if(!is.null(strata_levels)){
    ret <- list()
    
    for(j in 1:length(strata_levels)){
      
      ret[[j]]<- compute_curve(data=data[data[[strata_variable]] %in% strata_levels[j],],
                               time_points=time_points,
                               max.time = max.time)
      
    }
  } else{
    # not stratified
    ret <- list(compute_curve(data=data,
                              time_points=time_points,
                              max.time = max.time))
    
  }
  names(ret) <- strata_levels
  return(ret)
}



plot_surv_curves <- function(surv_curves,
                             strata_level_names=NULL,
                             x_label="Time since RP (years)",
                             y_label="Cumulative incidence (%)",
                             xlim=NULL,
                             ylim=c(0,100),
                             x_axis_at,
                             y_axis_at=c(0,25,50,75,100),
                             cols=NULL,
                             stacked=TRUE,
                             n_at_risk_times,
                             n_at_risk_y=-20,
                             n_at_risk_label_x=-2,
                             n_at_risk_label_y=-20,
                             axis.cex=1,
                             labels.cex=1,
                             natrisk.cex=1){
  
  n_strata <- length(surv_curves)
  if(n_strata==1 & is.null(strata_level_names)){
    strata_level_names <- ""
  }
  
  # inner function
  plot_surv_curve <- function(surv_curve,
                              main="",
                              x_label="Time since RP (years)",
                              y_label="Cumulative incidence (%)",
                              xlim=NULL,
                              ylim=c(0,100),
                              cols=NULL,
                              stacked=TRUE,
                              n_at_risk_times,
                              n_at_risk_y=-20,
                              n_at_risk_label_x=-2,
                              n_at_risk_label_y=-20,
                              axis.cex=1,
                              labels.cex=1,
                              natrisk.cex=1){
    
    if(is.null(xlim)){
      xlim <- c(0,max(x_axis_at))
    }
    
    # make the plot ready 
    plot(1,type="n",axes=FALSE,xlab="",ylab="",xlim=xlim,ylim=ylim)
    
    axis(side=1,at=x_axis_at,labels=x_axis_at,cex.axis=axis.cex)
    axis(side=2,at=y_axis_at,labels=paste0(y_axis_at),cex.axis=axis.cex)
    
    mtext(side=1,line=2,text=x_label,cex=labels.cex)
    mtext(side=2,line=2.25,text=y_label,cex=labels.cex)
    
    mtext(side=3,line=1,text=main)
    
    # add numbers at risk
    n_at_risk <- c()
    for(j in 1:length(n_at_risk_times)){
      loc <- max(which(surv_curve$n_at_risk$time <= n_at_risk_times[j]))
      n_at_risk[j] <- surv_curve$n_at_risk$n[loc]
    }
    
    text(x=n_at_risk_times,
         y=rep(n_at_risk_y,length(n_at_risk_times)),
         labels=n_at_risk,
         xpd=NA,
         cex=natrisk.cex)
    
    # label
    text(x=n_at_risk_label_x,
         y=n_at_risk_label_y,
         labels="N at risk",
         xpd=NA,
         cex=natrisk.cex)
    
    # plot curves
    time_points <- surv_curve$n_at_risk$time
    last_nonzero <- min(max(surv_curve$n_at_risk$time[surv_curve$n_at_risk$n>0]),max(surv_curve$n_at_risk$time),na.rm=TRUE)
    selected_times <- surv_curve$n_at_risk$time <= last_nonzero
    
    n_outcomes <- ncol(surv_curve$est)
    est <- surv_curve$est[selected_times,]
    est <- matrix(est,ncol=n_outcomes)
    time_points <- time_points[selected_times]
    
    
    
    if(is.null(cols)){
      cols <- 1:n_outcomes
    }
    
    if(!stacked){
      for(k in 1:n_outcomes){
        lines(x=time_points,
              y=est[,k]*100,
              col=cols[k])
      }
    } else{
      est <- t(apply(FUN=cumsum,est,MARGIN=1))
      for(k in n_outcomes:1){
        polygon(x=c(rev(time_points),time_points),
                y=c(rep(0,nrow(est)),est[,k]*100),
                col=cols[k],
                border=NA)
      }
    }
  }
  
  
  for(j in 1:n_strata){
    plot_surv_curve(surv_curve=surv_curves[[j]],
                    main=strata_level_names[j],
                    x_label=x_label,
                    y_label=y_label,
                    xlim=xlim,
                    ylim=ylim,
                    cols=cols,
                    stacked=stacked,
                    n_at_risk_times=n_at_risk_times,
                    n_at_risk_y=n_at_risk_y,
                    n_at_risk_label_x=n_at_risk_label_x,
                    n_at_risk_label_y=n_at_risk_label_y,
                    axis.cex=axis.cex,
                    labels.cex=labels.cex,
                    natrisk.cex=natrisk.cex) 
    
  }
  
}












D$rand_arm <- factor(D$rand_arm)
D$cens_crc_death <- factor(D$cens_crc_death)

###
# PCOL vs CONTROL

data <- D %>% 
  filter(rand_arm %in% c("dk","control")) %>% 
  mutate(timefu=timefu_crc_death/365.24,
         cens=cens_crc_death)
data$rand_arm <- factor(data$rand_arm,levels=c("dk","control"),labels=c("dk","control"))
table(data$cens,data$rand_arm)

surv_curves_pcol <- compute_surv_curves(data=data,
                                        strata_variable="rand_arm",
                                        time_points=NULL, 
                                        max.time = 7)

data <- D %>% 
  filter(rand_arm %in% c("dk","control")) %>% 
  mutate(timefu=timefu_crc_death2/365.24,
         cens=cens_crc_death2)
data$rand_arm <- factor(data$rand_arm,levels=c("dk","control"),labels=c("dk","control"))
table(data$cens,data$rand_arm)

surv_curves_pcol2 <- compute_surv_curves(data=data,
                                        strata_variable="rand_arm",
                                        time_points=NULL, 
                                        max.time = 7)

####
# FIT vs CONTROL
data <- D %>% 
  filter(rand_arm %in% c("fit") | is_fit_control ) %>% 
  mutate(timefu=timefu_crc_death/365.24,
         cens=cens_crc_death)
data$rand_arm <- factor(data$rand_arm,levels=c("fit","control"),labels=c("fit","control"))
table(data$cens,data$rand_arm)
surv_curves_fit <- compute_surv_curves(data=data,
                                       strata_variable="rand_arm",
                                       time_points=NULL, 
                                       max.time = 7)

data <- D %>% 
  filter(rand_arm %in% c("fit") | is_fit_control ) %>% 
  mutate(timefu=timefu_crc_death2/365.24,
         cens=cens_crc_death2)
data$rand_arm <- factor(data$rand_arm,levels=c("fit","control"),labels=c("fit","control"))
table(data$cens,data$rand_arm)
surv_curves_fit2 <- compute_surv_curves(data=data,
                                       strata_variable="rand_arm",
                                       time_points=NULL, 
                                       max.time = 7)









####
# Create plots 

create_blank <- function(){
  plot(1,type="n",axes=FALSE,xlab="",ylab="",xlim=c(0,7),ylim=c(0,1))
  
  axis(side=1,at=c(0:7),labels=0:7,cex.axis=1,line=-0.2,padj=-0.85)
  axis(side=2,at=c(c(0,2,4,6,8,10)/10),labels=paste0(c(0,2,4,6,8,10)/10,"%"),cex.axis=1,line=-0.2,padj=0.85)
  
  mtext(side=1,line=1,text="Years since randomization",cex=1)
  mtext(side=2,line=1.5,text="Cumulatice incidence proportion",cex=1)
}

draw_line <- function(d,outcome,col){
  x <- as.numeric(rownames(d$est))
  y <- d$est[,outcome]*100 # in percent
  lines(x,y,col=col,lwd=2)
}

add_n_at_risk <- function(d,n_at_risk_times,n_at_risk_y,col="black"){
  n_at_risk <- c()
  for(j in 1:length(n_at_risk_times)){
    loc <- max(which(d$n_at_risk$time <= n_at_risk_times[j]))
    n_at_risk[j] <- d$n_at_risk$n[loc]
  }
  
  text(x=n_at_risk_times,
       y=rep(n_at_risk_y,length(n_at_risk_times)),
       labels=n_at_risk,
       xpd=NA,
       cex=1,
       col=col)
}

col_contr <- rgb(0,0,0.4,alpha=0.5)
col_contr_ci <- rgb(0,0,0.9,alpha=0.25)
col_exp <- rgb(0,0.25,0,alpha=0.5)
col_exp_ci <- rgb(0,0.7,0,alpha=0.25)

n_at_risk_times <- c(0:6,6.9)

width <- 11
height <- 3.9

###
# CRC overall in PCOL vs CONTROL

svg(file=".\\Resultat\\cuminc_crc.svg",width=width,height=height)

par(mfrow=c(1,2),mar=c(4,2.5,1,0.1))
create_blank()
draw_line(d=surv_curves_pcol2$dk,outcome=1,col=col_exp)
add_n_at_risk(d=surv_curves_pcol2$dk,n_at_risk_times=n_at_risk_times,n_at_risk_y=-0.22,col=col_exp)

draw_line(d=surv_curves_pcol2$control,outcome=1,col=col_contr)
add_n_at_risk(d=surv_curves_pcol2$control,n_at_risk_times=n_at_risk_times,n_at_risk_y=-0.29,col=col_contr)


create_blank()
draw_line(d=surv_curves_fit2$fit,outcome=1,col=col_exp)
add_n_at_risk(d=surv_curves_fit2$fit,n_at_risk_times=n_at_risk_times,n_at_risk_y=-0.22,col=col_exp)

draw_line(d=surv_curves_fit2$control,outcome=1,col=col_contr)
add_n_at_risk(d=surv_curves_fit2$control,n_at_risk_times=n_at_risk_times,n_at_risk_y=-0.29,col=col_contr)

dev.off()




svg(file=".\\Resultat\\cuminc_crc_stage12.svg",width=width,height=height)

par(mfrow=c(1,2),mar=c(4,2.5,1,0.1))
create_blank()
draw_line(d=surv_curves_pcol$dk,outcome=1,col=col_exp)
add_n_at_risk(d=surv_curves_pcol$dk,n_at_risk_times=n_at_risk_times,n_at_risk_y=-0.22,col=col_exp)

draw_line(d=surv_curves_pcol$control,outcome=1,col=col_contr)
add_n_at_risk(d=surv_curves_pcol$control,n_at_risk_times=n_at_risk_times,n_at_risk_y=-0.29,col=col_contr)


create_blank()
draw_line(d=surv_curves_fit$fit,outcome=1,col=col_exp)
add_n_at_risk(d=surv_curves_fit$fit,n_at_risk_times=n_at_risk_times,n_at_risk_y=-0.22,col=col_exp)

draw_line(d=surv_curves_fit$control,outcome=1,col=col_contr)
add_n_at_risk(d=surv_curves_fit$control,n_at_risk_times=n_at_risk_times,n_at_risk_y=-0.29,col=col_contr)

dev.off()




svg(file=".\\Resultat\\cuminc_crc_stage34.svg",width=width,height=height)

par(mfrow=c(1,2),mar=c(4,2.5,1,0.1))
create_blank()
draw_line(d=surv_curves_pcol$dk,outcome=2,col=col_exp)
add_n_at_risk(d=surv_curves_pcol$dk,n_at_risk_times=n_at_risk_times,n_at_risk_y=-0.22,col=col_exp)

draw_line(d=surv_curves_pcol$control,outcome=2,col=col_contr)
add_n_at_risk(d=surv_curves_pcol$control,n_at_risk_times=n_at_risk_times,n_at_risk_y=-0.29,col=col_contr)


create_blank()
draw_line(d=surv_curves_fit$fit,outcome=2,col=col_exp)
add_n_at_risk(d=surv_curves_fit$fit,n_at_risk_times=n_at_risk_times,n_at_risk_y=-0.22,col=col_exp)

draw_line(d=surv_curves_fit$control,outcome=2,col=col_contr)
add_n_at_risk(d=surv_curves_fit$control,n_at_risk_times=n_at_risk_times,n_at_risk_y=-0.29,col=col_contr)

dev.off()


###
# End
###