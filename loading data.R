#Brown-Black widow web competition script




rm(list = ls())
#library(lme4)
#library(bbmle)
#library(dplyr)
#library(rptR)
#library(MASS)
#library(boot)
#library(beepr)
#library(lsmeans)
#library(piecewiseSEM)
#library(MuMIn)
#library(data.table)

#working directory already set, below code did not work-###
#setwd(Users/leapollack/Documents/gitHub/BWS_Disturbance)
bb<-read.csv(file="web_comp_data.csv",header=TRUE,na.strings=c(""))


#=================================Loading and formating the data=============================

#The dataset as it's formatted needs some work. There are currently two observations
#per trial (resident and intruder). Need to make it so that each trial is one observation:

go<-reshape(bb,
            idvar="filename",
            timevar="resident",
            direction="wide")

#subset out the relevant columns
bb<-subset(go,select=c(filename,focalID.1,blackwidow.0,trial.1,weight.1,
                       egg_present.1,mated.1,contest_ID.1,contest_black.1,
                       contest_weight.1,contest_mated.1,structural.1,
                       gumfooted.1,bodyshakes.1,abdomenpulse.1,contact.1,
                       time_to_contact.1,leg_touch.1,chelicerae_touch.1,
                       silk_spray.1,silk_attempt.1,kerfuffle.1,retreat.1,
                       time_to_retreat.1,left_web.1,
                       victory.1,vic_tie.1,bodyshakes.0,abdomenpulse.0,retreat.0,time_to_retreat.0,
                       left_web.0,time_to_leave_web.0
                       ))
#rename so the variable names are easy to work with..this chunk is ugly but it gets it done
bb$focalID<-bb$focalID.1
bb$trial<-bb$trial.1
bb$weight<-bb$weight.1
bb$egg_present<-bb$egg_present.1
bb$mated<-bb$mated.1
bb$contest_ID<-bb$contest_ID.1
bb$contest_black<-bb$contest_black.1
bb$contest_weight<-bb$contest_weight.1
bb$contest_mated<-bb$contest_mated.1
bb$structural<-bb$structural.1
bb$gumfooted<-bb$gumfooted.1
bb$bodyshakes<-bb$bodyshakes.1
bb$abdomenpulse<-bb$abdomenpulse.1
bb$contest_abdomenpulse<-bb$abdomenpulse.0
bb$contest_bodyshake<-bb$bodyshakes.0
bb$contact<-bb$contact.1
bb$time_to_contact<-bb$time_to_contact.1
bb$leg_touch<-bb$leg_touch.1
bb$chelicerae_touch<-bb$chelicerae_touch.1
bb$silk_spray<-bb$silk_spray.1
bb$silk_attempt<-bb$silk_attempt.1
bb$kerfuffle<-bb$kerfuffle.1
bb$retreat<-bb$retreat.1
bb$contest_retreat<-bb$retreat.0
bb$time_to_retreat<-bb$time_to_retreat.1
bb$contest_time_to_retreat<-bb$time_to_retreat.0
bb$left_web<-bb$left_web.1
bb$contest_left_web<-bb$left_web.0
bb$time_to_leave_web<-bb$time_to_leave_web.0
bb$victory<-bb$victory.1
bb$vic_tie<-bb$vic_tie.1
#Create new column called duration of trial
bb$time_to_leave_web[is.na(bb$time_to_leave_web)] <- 600
bb$trial_duration<-bb$time_to_leave_web
#Change IDs into factors, create variable for weight differences, and body shakes as binomial
bb$weight.diff <- bb$weight - bb$contest_weight
bb$contest_ID<-as.factor(bb$contest_ID)
bb$focalID<-as.factor(bb$focalID)
bb$vic_tie<-as.factor(bb$vic_tie)
bb$bs.binom <- ifelse(bb$bodyshakes==0,0,1)
bb$contest_bs.binom<-ifelse(bb$contest_bodyshake==0,0,1)
bb$contest_black<-as.factor(bb$contest_black)
#Make "NA" all victory values that are a tie (this creates a column of just winning and losing
#and a column for winning, losing, and tying)
bb$victory <- ifelse(bb$vic_tie==0,NA,bb$victory)
#There are trials that ended in a victory or loss, however we don't have the duration of the trial.
#Currently their listed as "600" which is inaccurate. So need to replace as NA.
#if duration equals 600 and victory is a 0 or 1, make NA
bb$trial_duration<-ifelse((bb$trial_duration==600)&(bb$vic_tie!=0),NA,bb$trial_duration)

#Create new column called duration of trial
bb$time_to_leave_web[is.na(bb$time_to_leave_web)] <- 600
bb$trial_duration<-bb$time_to_leave_web

widow_inv<-subset(bb,select=c(filename,focalID,contest_black,trial,weight,
                       egg_present,mated,contest_ID,bs.binom,contest_bs.binom,
                       contest_weight,contest_mated,structural,
                       gumfooted,bodyshakes,abdomenpulse,contact,
                       leg_touch,chelicerae_touch,weight.diff,
                       silk_spray,silk_attempt,kerfuffle,retreat,
                       time_to_retreat,left_web,
                       contest_abdomenpulse,contest_bodyshake,
                       time_to_contact,
                       contest_retreat, contest_time_to_retreat, 
                       contest_left_web,time_to_leave_web,
                       victory,vic_tie,trial_duration))


#===========================================================================================================

#graph outcomes (win-tie-lose) for resident as a function of number of structural lines

#view table
View(widow_inv)

#reorder 0,1,2 (tie, win, lose) to be 2,0,1 (lose, tie, win)
widow_inv$vic_tie <- factor(widow_inv$vic_tie , levels=c("2","0","1"))


#boxplot the outcomes
boxplot(structural~vic_tie, data=widow_inv, names=c("lose","tie","win"), horizontal=TRUE, main="Contest Outcome", xlab = "Number of structural lines", ylab = "Outcome")