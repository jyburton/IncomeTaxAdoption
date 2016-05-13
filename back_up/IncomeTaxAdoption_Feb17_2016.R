#######################################################
# Data Prep
#######################################################

####### GENERAL PREP

# Start Fresh
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption")

# Load/Transform the data
load("/Users/hectorbahamonde/RU/Dissertation/Data/dissertation.Rdata") # Load data

## Keep sample countries and variables
data = subset(dissertation, country == "Chile" | country ==  "Colombia" | country ==  "Ecuador" | country ==  "Guatemala" | country ==  "Nicaragua" | country ==  "Peru" | country ==  "Venezuela")
data = subset(data, select = c(country, year, democ, autoc, polity, polity2, urbpop, totpop, constmanufact, constagricult, exports, ppp, propagrmanu, realgdp, incometax, madisongdp, madisonpop, boix_democracy))
data = subset(data, constmanufact != "NA" & constagricult != "NA")

## Construct a the outcome variable (0,1) for when the income tax was imposed.
incometax.chile = data.frame(ifelse(data$year>=1924 & data$country == "Chile",1,0)) # Chile,  1924 (Mamalakis [1976, p. 20]
incometax.colombia  = data.frame(ifelse(data$year>=1935 & data$country == "Colombia",1,0)) # Colombia, Ley 78 Figueroa2008a, p. 9.
incometax.ecuador  = data.frame(ifelse(data$year>=1945 & data$country == "Ecuador",1,0)) # Ecuador Aguilera2013 p. 135
incometax.guatemala  = data.frame(ifelse(data$year>=1963 & data$country == "Guatemala",1,0)) # Guatemala: Decreto 1559, De2007 p 165
incometax.nicaragua  = data.frame(ifelse(data$year>=1974 & data$country == "Nicaragua",1,0)) # Ley No. 662 de 5 de Noviembre de 1974 (http://legislacion.asamblea.gob.ni/Normaweb.nsf/($All)/024063C3B373125E062570A10057EE73?OpenDocument)
incometax.peru  = data.frame(ifelse(data$year>=1934 & data$country == "Peru",1,0)) # Peru, Ley 7904 de 1934
incometax.venezuela  = data.frame(ifelse(data$year>=1943 & data$country == "Venezuela",1,0)) # Venezuela, Ley de Impuesto sobre la Renta, Publicada en la Gaceta Oficial número 20.851 del 17 de julio de 1.942, pero entra en vigencia el ano siguiente

## Sum across all the rows.
incometax.d = incometax.chile +  incometax.colombia +  incometax.ecuador +  incometax.guatemala +  incometax.nicaragua +  incometax.peru +  incometax.venezuela
colnames(incometax.d) = "incometax"


## generate the dataset
aux1.d = subset(data, select = c(country, year))
incometax.d = data.frame(aux1.d, incometax.d)

## merge with the dataset
data = merge(incometax.d, data, by=c("country", "year"))
colnames(data)[3] = "incometax.d"

## WHEN DEMOCRACY STARTED.
democracy.chile = data.frame(ifelse(data$year>= 1909 & data$country == "Chile",1,0)) 
democracy.colombia  = data.frame(ifelse(data$year>= 1937 & data$country == "Colombia",1,0)) 
democracy.ecuador  = data.frame(ifelse(data$year>= 1948 & data$country == "Ecuador",1,0)) 
democracy.guatemala  = data.frame(ifelse(data$year>= 1945 & data$country == "Guatemala",1,0)) 
democracy.nicaragua  = data.frame(ifelse(data$year>= 1984 & data$country == "Nicaragua",1,0)) 
democracy.peru  = data.frame(ifelse(data$year>= 1956 & data$country == "Peru",1,0)) 
democracy.venezuela  = data.frame(ifelse(data$year>= 1959 & data$country == "Venezuela",1,0))

## Sum across all the rows.
democracy.d = democracy.chile +  democracy.colombia +  democracy.ecuador +  democracy.guatemala +  democracy.nicaragua +  democracy.peru +  democracy.venezuela
colnames(democracy.d) = "democracy"


## generate the dataset
aux2.d = subset(data, select = c(country, year))
democracy.d = data.frame(aux2.d, democracy.d)

## merge with the dataset
data = merge(democracy.d, data, by=c("country", "year"))
colnames(data)[3] = "democracy.d"

## Saving Data
save(data, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/incometax_data.RData") # in paper's folder


##################################################
##              DATA PREP
# Cox Porportional Hazard Models
##################################################

# Start Fresh
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption")


# Load Data
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/incometax_data.RData") # Load data

# Declare Data for this model in particular
cox = data

# Exclude remanent "deaths" rows
## This function below creates a cumulative sum of the dummy var for income taxation
cox=within(cox, {
        incometax.d <- ave(incometax.d, country, FUN = cumsum)
}
)

## This below eliminates incometax.d <= 1
cox = subset(cox, incometax.d <= 1)

## Manual Log Transformation of Covariates
cox$ln.constmanufact = log(cox$constmanufact)
cox$ln.constagricult = log(cox$constagricult)
cox$ln.totpop = log(cox$totpop)

# Order and Transform the data
cox$ln.constmanufact = log(cox$constmanufact)
cox$ln.constagricult = log(cox$constagricult)
cox$ln.totpop = log(cox$totpop)

## Saving Data
save(cox, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/cox.RData") # in paper's folder

##################################################
##              DATA PREP
# Cox Porportional Hazard Models LAGGED MODELS
##################################################

# Start Fresh
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption")


## LAGGED DATA SET "cox.L"

# Load Data
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/cox.RData") # Load data

# install.packages("DataCombine")
library(data.table)
library(DataCombine)

cox.L <- setorder(cox, country, year)
cox.L <- subset(cox.L, select = - incometax.d)


constmanufact.L = slide(cox.L, Var = "constmanufact", GroupVar = "country", slideBy = 1)$constmanufact1
constagricult.L = slide(cox.L, Var = "constagricult", GroupVar = "country", slideBy = 1)$constagricult1

cox.L$constmanufact.L = constmanufact.L
cox.L$constagricult.L = constagricult.L

## Excluding NAs
cox.L = cox.L[!is.na(cox.L$constmanufact.L),]
cox.L = cox.L[!is.na(cox.L$constagricult.L),]

# The lag was eliminating the incometax.d=1's, so I do "year+1" to "update" the years
cox.L$year = cox.L$year+1

# Then I constructed the l.incometax.COUNTRY var again...
L.incometax.chile = data.frame(ifelse(cox.L$year==1924 & cox.L$country == "Chile",1,0)) # Chile,  1924 (Mamalakis [1976, p. 20]
L.incometax.colombia  = data.frame(ifelse(cox.L$year==1935 & cox.L$country == "Colombia",1,0)) # Colombia, Ley 78 Figueroa2008a, p. 9.
L.incometax.ecuador  = data.frame(ifelse(cox.L$year==1945 & cox.L$country == "Ecuador",1,0)) # Ecuador Aguilera2013 p. 135
L.incometax.guatemala  = data.frame(ifelse(cox.L$year==1963 & cox.L$country == "Guatemala",1,0)) # Guatemala: Decreto 1559, De2007 p 165
L.incometax.nicaragua  = data.frame(ifelse(cox.L$year==1974 & cox.L$country == "Nicaragua",1,0)) # Ley No. 662 de 5 de Noviembre de 1974 (http://legislacion.asamblea.gob.ni/Normaweb.nsf/($All)/024063C3B373125E062570A10057EE73?OpenDocument)
L.incometax.peru  = data.frame(ifelse(cox.L$year==1934 & cox.L$country == "Peru",1,0)) # Peru, Ley 7904 de 1934
L.incometax.venezuela  = data.frame(ifelse(cox.L$year==1943 & cox.L$country == "Venezuela",1,0)) # Venezuela, Ley de Impuesto sobre la Renta, Publicada en la Gaceta Oficial número 20.851 del 17 de julio de 1.942, pero entra en vigencia el ano siguiente

## Summed across all the rows.
L.incometax.d = L.incometax.chile +  L.incometax.colombia +  L.incometax.ecuador +  L.incometax.guatemala +  L.incometax.nicaragua +  L.incometax.peru +  L.incometax.venezuela
colnames(L.incometax.d) = "incometax"


## generated the dataset
L.aux1.d = subset(cox.L, select = c(country, year))
L.incometax.d = data.frame(L.aux1.d, L.incometax.d)

## merge with the dataset
L.cox = merge(L.incometax.d, cox.L, by=c("country", "year"))
colnames(L.cox)[3] = "incometax.d"


## Below is a test that the lagged dataset worked
### IT DOES
# test=data.frame(cox$country, cox.L$country, cox$year, cox.L$year, 
#cox$constmanufact, cox.L$constmanufact.L,
#cox$constagricult, cox.L$constagricult.L
#)

## Saving Data
save(L.cox, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/L_cox.RData") # in paper's folder


##################################################
##              DATA PREP
# Generalized Estimating Equation for Logit Regression GEE
##################################################

# Start Fresh
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption")


# Load Data
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/incometax_data.RData") # Load data

# Declare Data for this model in particular
logitgee  = data


# Order and Transform the data
logitgee <- logitgee[order(logitgee$country), ]
logitgee$ln.constmanufact = log(logitgee$constmanufact)
logitgee$ln.constagricult = log(logitgee$constagricult)
logitgee$ln.totpop = log(logitgee$totpop)

## Saving Data
save(logitgee, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/logitgee.RData") # in paper's folder


##################################################
##              MODELS
##
##################################################

# Start Fresh
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption")

# Load Datasets
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/cox.RData") # Cox
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/L_cox.RData") # Lagged Data for Cox
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/logitgee.RData") # Logit GEE

# Loading/Installing Library
# install.packages("survival") 
library(survival) # Cox Models
# install.packages("stargazer")
library(stargazer) # Tables
# install.packages("devtools")
# library(devtools)
# install_github("IQSS/Zelig")
library(Zelig)
### Feb. 15th, 2016, Zelig wasn't loading the (1) graph nor the (2) Rgraphviz, so I loaded them individually
### It seemed that these two packages were not available for my R version on CRAN. They were available for latest R from source, though
### source("https://bioconductor.org/biocLite.R")
### biocLite("graph")
### source("https://bioconductor.org/biocLite.R")
### biocLite("Rgraphviz")


# Robust std errors OR clustered variance -- they say it's enough to correct time dependence)
## Therneau2000 p. 171: Robust variance (via cluster) is good for when wants the model to leave out one **subject** (COUNTRY) at a time rather than one **observation** (COUNTRY-YEAR) at a time
cox1 = coxph(Surv(year, incometax.d) ~ log(constmanufact) + log(constagricult) + cluster(country), 
             data=cox
)

# I use this one for simulation (since it seems that the sim function doesn't take well natural logs)
cox3 = coxph(Surv(year, incometax.d) ~ constmanufact + constagricult + totpop + cluster(country), 
             data=cox
)


# Model with time-transformed variables
cox1.tt = coxph(Surv(year, incometax.d) ~ tt(constmanufact) + tt(constagricult) + cluster(country), 
                data=cox, x=T
)


# WORKING MODEL
cox2 = coxph(Surv(year, incometax.d) ~ ln.constmanufact + ln.constagricult + ln.totpop + cluster(country), 
             data=cox
)

# LAGGED MODEL
cox.L = coxph(Surv(year, incometax.d) ~ log(constmanufact.L) + log(constagricult.L) + log(totpop) + cluster(country), 
              data=L.cox)


## logit GEE
logitgee.1 = zelig(incometax.d ~ ln.constmanufact + ln.constagricult + ln.totpop, 
                   model = "logit.gee", 
                   id = "country", 
                   cite = FALSE,
                   data = logitgee)

# Table
stargazer(cox1, cox1.tt, cox2, cox.L, cox3, # logitgee.1, 
          type = "text", # change to "latex"/"text" when nedded
          #covariate.labels=c("Agr/Ind Proportion"),
          dep.var.labels=c("Income Taxation"),
          label = "results:1",
          notes = "Robust Std. Errors in Parentheses",
          title = "Structural Origins of Income Taxation",
          font.size = "scriptsize",
          table.placement = "h",
          notes.align = "c",
          style = "apsr")

### negative coefficients imply longer survival times.
### positive coefficients imply shorter survival times.
### yearly hazard of impose a tax system.


##################################################
##              MODELS
##                              Recurrent Events
##      Democracy and Taxation
##################################################

# Start Fresh
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption")

# Load Datasets
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/cox.RData") # Cox
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/L_cox.RData") # Lagged Data for Cox
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/logitgee.RData") # Logit GEE

# Loading/Installing Library
# install.packages("survival") 
library(survival) # Cox Models
# install.packages("stargazer")
library(stargazer) # Tables
# install.packages("devtools")
# library(devtools)
# install_github("IQSS/Zelig")
library(Zelig)
### Feb. 15th, 2016, Zelig wasn't loading the (1) graph nor the (2) Rgraphviz, so I loaded them individually
### It seemed that these two packages were not available for my R version on CRAN. They were available for latest R from source, though
### source("https://bioconductor.org/biocLite.R")
### biocLite("graph")
### source("https://bioconductor.org/biocLite.R")
### biocLite("Rgraphviz")


### From the clinical sciences, Cox Prop Haz Models have been estimating the recurrence of, say, infections.
### One important assumption, is that once a patient suffers from one, having a 2nd one, becomes more likely.
### That's exactly the type of similarity I'm looking for.
#### Andersen-Gill Models: (Therneau2000, 185-ss.): "multiple failure-time data": when a subject experiences one of the events, it still remains at risk for events of different types
##### This model is ideally suited to the situation of mutual independence of
##### the observations within a subject. This assumption is equivalent to each individual
##### counting process possessing independent increments, i.e., the num- bers of events 
##### in nonoverlapping time intervals are independent, given the covariates.

###### From Stata info: In these studies, failure times are correlated within cluster (subject or group), 
###### violating the independence of failure times assumption required in traditional survival analysis.
####### Further reference : http://www.stata.com/support/faqs/statistics/multiple-failure-time-data/

############
## DATA PREP
############

# Because there are no (exact) "ties" between DEMOCRACY and TAXATION, a simple
# cross summation between the two columns will suffice.
## Check this if the sample of countries is modified

# Load the data
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/cox.RData")
ag.data = cox

# Create a starting point variable for DEMOCRACY (similar to incometax.d).
ag.data.chile = data.frame(ifelse(cox$year== 1909 & cox$country == "Chile",1,0)) 
ag.data.colombia  = data.frame(ifelse(cox$year== 1937 & cox$country == "Colombia",1,0)) 
ag.data.ecuador  = data.frame(ifelse(cox$year== 1948 & cox$country == "Ecuador",1,0)) 
ag.data.guatemala  = data.frame(ifelse(cox$year== 1945 & cox$country == "Guatemala",1,0)) 
ag.data.nicaragua  = data.frame(ifelse(cox$year== 1984 & cox$country == "Nicaragua",1,0)) 
ag.data.peru  = data.frame(ifelse(cox$year== 1956 & cox$country == "Peru",1,0)) 
ag.data.venezuela  = data.frame(ifelse(cox$year== 1959 & cox$country == "Venezuela",1,0))

## Sum across all the rows.
ag.data.d = ag.data.chile +  ag.data.colombia +  ag.data.ecuador +  ag.data.guatemala +  ag.data.nicaragua +  ag.data.peru +  ag.data.venezuela
colnames(ag.data.d) = "democracy"

## generate the dataset
aux3.d = subset(cox, select = c(country, year))
democracy.d = data.frame(aux3.d, ag.data.d)

## merge with the dataset
ag.data = merge(democracy.d, ag.data, by=c("country", "year"))
colnames(data)[3] = "democracy.s"




ag.data$dem.tax = ag.data$incometax.d+ag.data$democracy.d

# Robust std errors OR clustered variance -- they say it's enough to correct time dependence)
cox1 = coxph(Surv(year, incometax.d) ~ log(constmanufact) + log(constagricult) + cluster(country), 
             data=cox
)

# I use this one for simulation (since it seems that the sim function doesn't take well natural logs)
cox3 = coxph(Surv(year, incometax.d) ~ constmanufact + constagricult + totpop + cluster(country), 
             data=cox
)


# Model with time-transformed variables
cox1.tt = coxph(Surv(year, incometax.d) ~ tt(constmanufact) + tt(constagricult) + cluster(country), 
                data=cox, x=T
)


# WORKING MODEL
cox2 = coxph(Surv(year, incometax.d) ~ ln.constmanufact + ln.constagricult + ln.totpop + cluster(country), 
             data=cox
)

# LAGGED MODEL
cox.L = coxph(Surv(year, incometax.d) ~ log(constmanufact.L) + log(constagricult.L) + log(totpop) + cluster(country), 
              data=L.cox)


## logit GEE
logitgee.1 = zelig(incometax.d ~ ln.constmanufact + ln.constagricult + ln.totpop, 
                   model = "logit.gee", 
                   id = "country", 
                   cite = FALSE,
                   data = logitgee)

# Table
stargazer(cox1, cox1.tt, cox2, cox.L, cox3, # logitgee.1, 
          type = "text", # change to "latex"/"text" when nedded
          #covariate.labels=c("Agr/Ind Proportion"),
          dep.var.labels=c("Income Taxation"),
          label = "results:1",
          notes = "Robust Std. Errors in Parentheses",
          title = "Structural Origins of Income Taxation",
          font.size = "scriptsize",
          table.placement = "h",
          notes.align = "c",
          style = "apsr")

### negative coefficients imply longer survival times.
### positive coefficients imply shorter survival times.
### yearly hazard of impose a tax system.



##################################################
##              POST ESTIMATION
##################################################



################
# COX PROP
################



# Preliminary Plots of the WORKING model
termplot(cox2, term=1, se=TRUE)
termplot(cox2, term=2, se=TRUE)


## Testing Prop. Assumption
test.assumption = cox.zph(cox2, transform = 'log')
print(test.assumption)
plot(test.assumption[1]) # covariate 1
plot(test.assumption[2]) # covariate 2
plot(test.assumption[3]) # covariate 3

### If the proportional hazards assumption is true, 
### beta(t) will be a horizontal line. 
### The printout gives a test for slope=0.
### A p-value less than 0.05 indicates a violation of the proportionality assumption.
### The last row contains the global test for all the interactions tested at once. 
### if the predictors satisfy the proportional hazard assumption then the shapes of the curves should be basically the same, and the separation between the curves should remain proportional across analysis time. 
### the proportional hazard assumption is precisely that the coefficient does not change over time


#### COX2 IT IS NOT VIOLATED.
#### COX1 IT IS VIOLATED
#### TIME DYNAMICS SHOULD BE INCORATED for COX1 i.e. log-time interactions
## Testing Prop. Assumption THIS PART OF THE CODE DOESNT HAVE TOO MUCH BACK UP PART FROM THE CODERS OF THESE FUNCIONS

#### LOGS
cox1.tt2 = coxph(Surv(year, incometax.d) ~ tt(constmanufact) + tt(constagricult) + cluster(country), 
                 data=cox, x=T, tt = c(function(x, t, ...) x * log(t+20), function(x, t, ...) x * log(t-20))
)

test.assumption = cox.zph(cox1.tt2, transform = 'log')
print(test.assumption)
plot(test.assumption[1]) # covariate 1
plot(test.assumption[2]) # covariate 2


#### SPLINES
##### this one here seems to work better compared to logs. Box-St. recommends SPLINES, so I will use them.
cox1.splines = coxph(Surv(year, incometax.d) ~ pspline(constmanufact, df=5) + pspline(constagricult, df=5) + cluster(country), 
                     data=cox)

termplot(cox1.splines, term=1, se=TRUE)
termplot(cox1.splines, term=2, se=TRUE)




cox3 = coxph(Surv(year, incometax.d) ~ constmanufact + constagricult + totpop + cluster(country), 
             data=cox
             )


sim1 <- coxsimLinear(cox3, 
                     b = "constmanufact", 
                     Xj = seq(1900-1973, by = 1)
                     )
# It says "All Xl set to 0" b/c it sets the reference category to 0.
# Hazard ratios are COMPARISONS, so the coefficients mean the effects of the Betas, relative to 0.
simGG(sim1, alpha = 0.05, type = "lines")


########################################################
#### Simulation: Relative Hazard
########################################################

# devtools::install_github('christophergandrud/simPH')
library(simPH)

sim3.m <- coxsimLinear(cox3, 
                     b = "constmanufact", 
                     qi = "Relative Hazard",
                     spin = T,
                     Xj = seq(min(cox$constmanufact), max(cox$constmanufact), by=150)
                     )

sim3.a <- coxsimLinear(cox3, 
                       b = "constagricult", 
                       qi = "Relative Hazard",
                       spin = T,
                       Xj = seq(min(cox$constagricult), max(cox$constagricult), by=250)
)

# It says "All Xl set to 0" b/c it sets the reference category to 0.
# Hazard ratios are COMPARISONS, so the coefficients mean the effects of the Betas, relative to 0.

# Plot
## IMPORTANT! A relative hazard for a unit at zero is always one, as it is a ratio of the hazards with itself. Gandrud2015 p. 10
simGG(sim3.m, xlab = "Industrial Output")
simGG(sim3.a, xlab = "Agricultural Output")

# Just in case I want to plot these using GGPLOT2, the QI are
## sim3.m$sims["QI"]
## sim3.a$sims["QI"]


########################################################
#### Simulation: Logit GEE
########################################################

library(Zelig)

# It's working but it's not a too informative simulation
x.out <- setx(logitgee.1)
set.seed(602)
s.out <- sim(logitgee.1, x = x.out)
plot(s.out)

# TEST

## THis here is the model
logitgee.1 = zelig(incometax.d ~ ln.constmanufact + ln.constagricult + ln.totpop, 
                   model = "logit.gee", 
                   id = "country", 
                   cite = FALSE,
                   data = logitgee)


# 1 X's IT IS STARTING MAKING SENSE...NOW I NEED THE PLOTS
## WORKING HERE
logitgee.1.x.L <- setx(logitgee.1, ln.constmanufact = summary(logitgee$ln.constmanufact)[2])
logitgee.1.x.H <- setx(logitgee.1, ln.constmanufact = summary(logitgee$ln.constmanufact)[5])
set.seed(602)
logitgee.1.s <- sim(logitgee.1, x = logitgee.1.x.L, x1= logitgee.1.x.H, num=2500)




## NOT RUN BELOW


#######################################################
# Conditional Logit CLOGIT = conditional likelihood function
#######################################################

# Load Data
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/incometax_data.RData") # Load data

# Declare Data for this model in particular
clogit = data


# Load Libraries
library(survival)

# Prepare Data
clogit$incometax.d2 = as.logical(clogit$incometax.d)
# clogit = na.omit(clogit)
## Manual Log Transformation of Covariates
clogit$ln.constmanufact = log(clogit$constmanufact)
clogit$ln.constagricult = log(clogit$constagricult)
clogit$ln.totpop = log(clogit$totpop)


# Models
clogit.1 = clogit(incometax.d2 ~  ln.constmanufact + strata(country), data = clogit)





