
#Place the datafile in the home repository 
setwd("~/Goncalves_et_al_MIOC_2024")
data<-read.csv("random_147devyear_GROUPS.csv")

#wilcoxon rank-sum power calc 

#https://rdrr.io/cran/rstatix/man/wilcox_effsize.html
#The effect size r is calculated as Z statistic divided by 
#square root of the sample size (N) (Z/√{N}). 
#The Z value is extracted from either coin::wilcoxsign_test() (case of one- or paired-samples test) 
#or coin::wilcox_test() (case of independent two-samples test).

library(rstatix)
library(samplesize)
library(dplyr)
library(devtools)
library(plyr)


#Usage
#n.wilcox.ord(power = 0.8, alpha = 0.05, t, p, q)
#Arguments
#power required Power
#alpha required two-sided Type-I-error level
#t sample size fraction n/N, where n is sample size of group B and N is the total sample size
#p vector of expected proportions of the categories in group A, should sum to 1
#q vector of expected proportions of the categories in group B, should be of equal
#length as p and should sum to 1

#DATA: n=142 controls; n=33 cimex; n=21 triatoma

# Controls vs T. infestans - by year of first evidence of development
n.wilcox.ord(power = 0.80, alpha = 0.05, 0.87,  # # n=163 houses [n=142 controls] t=142/163
             p = c(0.67, 0.25, 0.04, 0.04, 0, 0),#T. infestans
             q = c(0.15, 0.52, 0.08, 0.11, 0.06, 0.08))  # control
#total sample size required: n=62 (n=8 T. infestans and n=54 controls)

#wilcoxon:
Year_Developed<- c(2004,2010,2012,2017,2018,2020)
t_infestans_houses <- c(14, 5, 1, 1, 0, 0)
control_houses <- c(22, 74, 12, 15, 8, 11)

#displays the number of houses by year first developed, as measured by satellite image, for houses with T. infestans and controls
df0<-data.frame(Year_Developed, t_infestans_houses,control_houses )
df0

df1 <-data.frame(group=rep(c("t_infestans", "control"), each=6),
                            houses=c(t_infestans_houses,control_houses ))

group_by(df1, group)%>%
  dplyr::summarize(count =n (), median=median(houses), IQR=IQR(houses))

#runs the wilcoxon test to ascertain if T infestans infested houses are older than controls
test2 <- wilcox.test(houses~group, data=df1, exact=FALSE)
test2
#


# Sample Size: Controls vs Cimex sp. - by year of first evidence of development
n.wilcox.ord(power = 0.80, alpha = 0.05, 0.81,  # # n=175 houses [n=142 controls; n=33 Cimex sp.] t=142/175
             p = c(0.45, 0.43, 0.09, 0.03, 0, 0),#Cimex sp.
             q = c(0.15, 0.52, 0.08, 0.11, 0.06, 0.08))  # control

#total sample size required: n=90 (n=17 Cimex sp. and n=73 controls)

#Results
cimex_houses <- c(15, 14, 3, 1, 0, 0)
control_houses <- c(22, 74, 12, 15, 8, 11)

df3 <-data.frame(group=rep(c("cimex", "control"), each=6),
                 houses=c(cimex_houses,control_houses ))

group_by(df3, group)%>%
  dplyr::summarize(count =n (), median=median(houses), IQR=IQR(houses))

test4 <- wilcox.test(houses~group, data=df3, exact=FALSE)
test4

