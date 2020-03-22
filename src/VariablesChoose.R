
library(dplyr)
library(ggplot2)
library(data.table)
library(stringr)

load("/Users/greatyifan/Desktop/@Columbia/2019fall/data/HSLS_2016_v1_0_R_Datasets/hsls_16_student_v1_0.rdata")

dta <- hsls_16_student_v1_0


# candidate control variables
control <- c('X1SEX', 'X1RACE', 'X1MOMEMP', 
             'X1MOMOCC2', 'X1MOMRACE', 'X1DADEMP', 'X1MOMEDU', 
             'X1DADEDU', 'X1DADOCC2', 'X1DADRACE','X1PARPATTERN', 
             'X1HHNUMBER', 'X1LOCALE', 'X1REGION', 'P1ADHDMED') # control variable 
#' those not included but maybe useful later
#' 'X1SCHOOLENG', 'X1STU30OCC2', 'X1CONTROL', 'X1SCHOOLCLI', 'X1SCHOOLBEL', 
#' 'X1MTHID', 'X1SCIID', 'X1STDOB', 'X1DUALLANG'


# basic independent variables
iv <- c('S1HRACTIVITY', 'S1MOMTALKPRB', 'S1DADTALKPRB', 'S3CAREERINFLU',
        'P1ARTS', 'P1SPORTS', 'P1RELIGGRP',
        'P1CLUB', 'P1ACADEMIC', 'P1CAMPMS', 'P1CAMPOTH', 'P1NOOUTSCH')
#  'X1IEPFLAG'


# parenting-related independent variables
piv <- c('P1PTCONFER', 'P1PTOMTG', 'P1HWOFTEN','P1MUSEUM',
         'P1COMPUTER','P1FIXED','P1SCIFAIR', 'P1SCIPROJ', 'P1STEMDISC', 
         'P1LIBRARY', 'P1SHOW', 'P1NOACT', 'P1FUNDRAISE', 'P1HHTIME',
         'P1VOLUNTEER', 'P1SCHEVENT') # independent variable 
#' too little answer: 'P2DKHOWAPP', 'P2FORMSDIFF', 'P2DROPOUTOK',
#' 'P2BADGRADES', 'P2SCHWASTE','P2DISCEVENTS', 'P2DISCTROUBLE', 'P2DECIDECLG'
#' 'P2CONTACTSCH', 'P1SCHCHOICE'


# children-related independent variables
civ <- c('X1FAMINCOME', 'X1SESQ5') # competing independent variable
# 'X1SES' TOO MANY MISSING
dv <- 'X4INCOMECAT' #dependent variable  
# some variables left behind: @X3PROGLEVEL; @X3EARNPERHR1 


var <- c(control, iv, civ, piv, dv) # combine all of them together

table(dta$X4INCOMECAT) # inspect the main dependent variable: future income

dt <- dta[,var] 
dt <- as.data.table(dt)
dim(dt)

dt[dt <= 0] <- NA # recode -8, -9 which mean missing data as NA in the data table
summary(dt)

colnames(dt) <- tolower(str_remove(colnames(dt),'X1|S1|X4'))

table(dt$famincome)
table(dt$hractivity)
table(dt$incomecat)
dt$p1hhtime <- 6 - dt$p1hhtime
# dt[,parentInfluence := ifelse()]
dt$p1nooutsch <- 1-dt$p1nooutsch

# X4 income is recorded in 2013, the data from 2016 is suppressed!

# find na -----
findNA <- t(apply(dt,1,is.na))
li <- (apply(findNA,1,sum))
summary(li)
idx <- which(li > 12)
dt <- dt[-idx,] #get rid of those having more than 12 variable missing


