#######################################################
# Data Prep
#######################################################

####### GENERAL PREP

# Start Fresh
rm(list=ls())
cat("\014")
setwd("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption")

# Load/Transform the data
load("/Users/hectorbahamonde/RU/Dissertation/Data/dissertation.Rdata") # Load data

## Keep sample countries and variables
data = subset(dissertation, country == "Chile" | country ==  "Colombia" | country ==  "Ecuador" | country ==  "Guatemala" | country ==  "Nicaragua" | country ==  "Peru" | country ==  "Venezuela")
data = subset(data, select = c(country, year, democ, autoc, polity, polity2, urbpop, totpop, constmanufact, constagricult, exports, ppp, propagrmanu, realgdp, incometax, madisongdp, madisonpop, boix_democracy, madisonpercapgdp, customtax))
data = subset(data, constmanufact != "NA" & constagricult != "NA")

## Construct a the outcome variable (0,1) for when the income tax was imposed.
incometax.d.chile = data.frame(ifelse(data$year>=1924 & data$country == "Chile",1,0)) # Chile,  1924 (Mamalakis [1976, p. 20]
incometax.d.colombia  = data.frame(ifelse(data$year>=1935 & data$country == "Colombia",1,0)) # Colombia, Ley 78 Figueroa2008a, p. 9.
incometax.d.ecuador  = data.frame(ifelse(data$year>=1945 & data$country == "Ecuador",1,0)) # Ecuador Aguilera2013 p. 135
incometax.d.guatemala  = data.frame(ifelse(data$year>=1963 & data$country == "Guatemala",1,0)) # Guatemala: Decreto 1559, De2007 p 165
incometax.d.nicaragua  = data.frame(ifelse(data$year>=1974 & data$country == "Nicaragua",1,0)) # Ley No. 662 de 5 de Noviembre de 1974 (http://legislacion.asamblea.gob.ni/Normaweb.nsf/($All)/024063C3B373125E062570A10057EE73?OpenDocument)
incometax.d.peru  = data.frame(ifelse(data$year>=1934 & data$country == "Peru",1,0)) # Peru, Ley 7904 de 1934
incometax.d.venezuela  = data.frame(ifelse(data$year>=1943 & data$country == "Venezuela",1,0)) # Venezuela, Ley de Impuesto sobre la Renta, Publicada en la Gaceta Oficial número 20.851 del 17 de julio de 1.942, pero entra en vigencia el ano siguiente

## Sum across all the rows.
incometax.d = incometax.d.chile +  incometax.d.colombia +  incometax.d.ecuador +  incometax.d.guatemala +  incometax.d.nicaragua +  incometax.d.peru +  incometax.d.venezuela
colnames(incometax.d) = "incometax"


## generate the dataset
aux1.d = subset(data, select = c(country, year))
incometax.d = data.frame(aux1.d, incometax.d)

## merge with the dataset
data = merge(incometax.d, data, by=c("country", "year"))
colnames(data)[3] = "incometax.d"

## WHEN DEMOCRACY STARTED.
democracy.d.chile = data.frame(ifelse(data$year>= 1909 & data$country == "Chile",1,0)) 
democracy.d.colombia  = data.frame(ifelse(data$year>= 1937 & data$country == "Colombia",1,0)) 
democracy.d.ecuador  = data.frame(ifelse(data$year>= 1948 & data$country == "Ecuador",1,0)) 
democracy.d.guatemala  = data.frame(ifelse(data$year>= 1945 & data$country == "Guatemala",1,0)) 
democracy.d.nicaragua  = data.frame(ifelse(data$year>= 1984 & data$country == "Nicaragua",1,0)) 
democracy.d.peru  = data.frame(ifelse(data$year>= 1956 & data$country == "Peru",1,0)) 
democracy.d.venezuela  = data.frame(ifelse(data$year>= 1959 & data$country == "Venezuela",1,0))

## Sum across all the rows.
democracy.d = democracy.d.chile +  democracy.d.colombia +  democracy.d.ecuador +  democracy.d.guatemala +  democracy.d.nicaragua +  democracy.d.peru +  democracy.d.venezuela
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

## This below eliminates incometax.s <= 1
cox = subset(cox, incometax.d <= 1)

## Now, it renames the "incometax.d" column for "incometax.s"
colnames(cox)[4] = "incometax.s"


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
cox.L <- subset(cox.L, select = - incometax.s)


constmanufact.L = slide(cox.L, Var = "constmanufact", GroupVar = "country", slideBy = 1)$constmanufact1
constagricult.L = slide(cox.L, Var = "constagricult", GroupVar = "country", slideBy = 1)$constagricult1

cox.L$constmanufact.L = constmanufact.L
cox.L$constagricult.L = constagricult.L

## Excluding NAs
cox.L = cox.L[!is.na(cox.L$constmanufact.L),]
cox.L = cox.L[!is.na(cox.L$constagricult.L),]

# The lag was eliminating the incometax.s=1's, so I do "year+1" to "update" the years
cox.L$year = cox.L$year+1

# Then I constructed the l.incometax.s.COUNTRY var again...
L.incometax.s.chile = data.frame(ifelse(cox.L$year==1924 & cox.L$country == "Chile",1,0)) # Chile,  1924 (Mamalakis [1976, p. 20]
L.incometax.s.colombia  = data.frame(ifelse(cox.L$year==1935 & cox.L$country == "Colombia",1,0)) # Colombia, Ley 78 Figueroa2008a, p. 9.
L.incometax.s.ecuador  = data.frame(ifelse(cox.L$year==1945 & cox.L$country == "Ecuador",1,0)) # Ecuador Aguilera2013 p. 135
L.incometax.s.guatemala  = data.frame(ifelse(cox.L$year==1963 & cox.L$country == "Guatemala",1,0)) # Guatemala: Decreto 1559, De2007 p 165
L.incometax.s.nicaragua  = data.frame(ifelse(cox.L$year==1974 & cox.L$country == "Nicaragua",1,0)) # Ley No. 662 de 5 de Noviembre de 1974 (http://legislacion.asamblea.gob.ni/Normaweb.nsf/($All)/024063C3B373125E062570A10057EE73?OpenDocument)
L.incometax.s.peru  = data.frame(ifelse(cox.L$year==1934 & cox.L$country == "Peru",1,0)) # Peru, Ley 7904 de 1934
L.incometax.s.venezuela  = data.frame(ifelse(cox.L$year==1943 & cox.L$country == "Venezuela",1,0)) # Venezuela, Ley de Impuesto sobre la Renta, Publicada en la Gaceta Oficial número 20.851 del 17 de julio de 1.942, pero entra en vigencia el ano siguiente

## Summed across all the rows.
L.incometax.s = L.incometax.s.chile +  L.incometax.s.colombia +  L.incometax.s.ecuador +  L.incometax.s.guatemala +  L.incometax.s.nicaragua +  L.incometax.s.peru +  L.incometax.s.venezuela
colnames(L.incometax.s) = "incometax"


## generated the dataset
L.aux1.d = subset(cox.L, select = c(country, year))
L.incometax.s = data.frame(L.aux1.d, L.incometax.s)

## merge with the dataset
L.cox = merge(L.incometax.s, cox.L, by=c("country", "year"))
colnames(L.cox)[3] = "incometax.s"


## Below is a test that the lagged dataset worked
### IT DOES
# test=data.frame(cox$country, cox.L$country, cox$year, cox.L$year, 
#cox$constmanufact, cox.L$constmanufact.L,
#cox$constagricult, cox.L$constagricult.L
#)

# Transforming data
a= log(L.cox$constmanufact.L)

## Saving Data
save(L.cox, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/L_cox.RData") # in paper's folder


##################################################
##              DATA PREP
# Generalized Estimating Equation for Logit Regression GEE
##################################################


# GEE logit allows for dependence within clusters, such as in longitudinal
# GEE models measure population-averaged effects as opposed to cluster-specific effects


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
logitgee$ln.customtax = log(logitgee$customtax)


## Saving Data
save(logitgee, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/logitgee.RData") # in paper's folder


##################################################
##              DATA PREP
# Multiple Events Framework
##################################################


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



cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption")


# Load Data
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/incometax_data.RData") # Load data

ag.data = data

# Data Transformation
ag.data$ln.constmanufact = log(ag.data$constmanufact)
ag.data$ln.constagricult = log(ag.data$constagricult)
ag.data$ln.totpop = log(ag.data$totpop)

# Create a starting point variable for DEMOCRACY.
ag.data.chile = data.frame(ifelse(ag.data$year== 1909 & ag.data$country == "Chile",1,0)) 
ag.data.colombia  = data.frame(ifelse(ag.data$year== 1937 & ag.data$country == "Colombia",1,0)) 
ag.data.ecuador  = data.frame(ifelse(ag.data$year== 1948 & ag.data$country == "Ecuador",1,0)) 
ag.data.guatemala  = data.frame(ifelse(ag.data$year== 1945 & ag.data$country == "Guatemala",1,0)) 
ag.data.nicaragua  = data.frame(ifelse(ag.data$year== 1984 & ag.data$country == "Nicaragua",1,0)) 
ag.data.peru  = data.frame(ifelse(ag.data$year== 1956 & ag.data$country == "Peru",1,0)) 
ag.data.venezuela  = data.frame(ifelse(ag.data$year== 1959 & ag.data$country == "Venezuela",1,0))

## Sum across all the rows.
ag.data.d = ag.data.chile +  ag.data.colombia +  ag.data.ecuador +  ag.data.guatemala +  ag.data.nicaragua +  ag.data.peru +  ag.data.venezuela
colnames(ag.data.d) = "democracy"

## generate the dataset
aux3.d = subset(ag.data, select = c(country, year))
democracy.s = data.frame(aux3.d, ag.data.d)

## merge with the dataset
ag.data = merge(democracy.s, ag.data, by=c("country", "year"))
colnames(ag.data)[3] = "democracy.s"

# Create a starting point variable for INCOMETAX
incometax.s.chile = data.frame(ifelse(ag.data$year==1924 & ag.data$country == "Chile",1,0)) # Chile,  1924 (Mamalakis [1976, p. 20]
incometax.s.colombia  = data.frame(ifelse(ag.data$year==1935 & ag.data$country == "Colombia",1,0)) # Colombia, Ley 78 Figueroa2008a, p. 9.
incometax.s.ecuador  = data.frame(ifelse(ag.data$year==1945 & ag.data$country == "Ecuador",1,0)) # Ecuador Aguilera2013 p. 135
incometax.s.guatemala  = data.frame(ifelse(ag.data$year==1963 & ag.data$country == "Guatemala",1,0)) # Guatemala: Decreto 1559, De2007 p 165
incometax.s.nicaragua  = data.frame(ifelse(ag.data$year==1974 & ag.data$country == "Nicaragua",1,0)) # Ley No. 662 de 5 de Noviembre de 1974 (http://legislacion.asamblea.gob.ni/Normaweb.nsf/($All)/024063C3B373125E062570A10057EE73?OpenDocument)
incometax.s.peru  = data.frame(ifelse(ag.data$year==1934 & ag.data$country == "Peru",1,0)) # Peru, Ley 7904 de 1934
incometax.s.venezuela  = data.frame(ifelse(ag.data$year==1943 & ag.data$country == "Venezuela",1,0)) # Venezuela, Ley de Impuesto sobre la Renta, Publicada en la Gaceta Oficial número 20.851 del 17 de julio de 1.942, pero entra en vigencia el ano siguiente

## Sum across all the rows.
incometax.s = incometax.s.chile +  incometax.s.colombia +  incometax.s.ecuador +  incometax.s.guatemala +  incometax.s.nicaragua +  incometax.s.peru +  incometax.s.venezuela
colnames(incometax.s) = "incometax"


## generate the dataset
aux4.s = subset(data, select = c(country, year))
incometax.s = data.frame(aux4.s, incometax.s)

## merge with the dataset
ag.data = merge(incometax.s, ag.data, by=c("country", "year"))
colnames(ag.data)[3] = "incometax.s"

# Add up the two columns
## Because there are no (exact) "ties" between DEMOCRACY and TAXATION, a simple
## cross summation between the two columns will suffice.
### Check this if the sample of countries is modified
ag.data$dem.tax = ag.data$incometax.s+ag.data$democracy.s


# Now, generate a variable to exclude the periods that follow BOTH events (i.e. DEM and INCOMETAX)
##
ag.data=within(ag.data, {
        dem.cumsum <- ave(democracy.s, country, FUN = cumsum)
}
)
ag.data=within(ag.data, {
        tax.cumsum <- ave(incometax.s, country, FUN = cumsum)
}
)

ag.data<-ag.data[!(ag.data$dem.cumsum==1 & ag.data$tax.cumsum==1 & ag.data$incometax.s==0 & ag.data$democracy.s==0),]

## Saving Data
save(ag.data, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/ag_data.RData") # in paper's folder



##################################################
##              DATA PREP
#                               Lagged CLOGIT
##################################################

cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption")


# Load Data
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/incometax_data.RData") # Load data

L.clogit = data

# install.packages("DataCombine")
library(data.table)
library(DataCombine)

L.clogit <- setorder(L.clogit, country, year)
L.clogit <- subset(L.clogit, select = - incometax.d)


constmanufact.L = slide(L.clogit, Var = "constmanufact", GroupVar = "country", slideBy = 1)$constmanufact1
constagricult.L = slide(L.clogit, Var = "constagricult", GroupVar = "country", slideBy = 1)$constagricult1

L.clogit$constmanufact.L = constmanufact.L
L.clogit$constagricult.L = constagricult.L

## Excluding NAs
L.clogit = L.clogit[!is.na(L.clogit$constmanufact.L),]
L.clogit = L.clogit[!is.na(L.clogit$constagricult.L),]

# The lag was eliminating the incometax.s=1's, so I do "year+1" to "update" the years
L.clogit$year = L.clogit$year+1

# Then I constructed the l.incometax.d.COUNTRY var again...
L.incometax.d.chile = data.frame(ifelse(L.clogit$year>=1924 & L.clogit$country == "Chile",1,0)) # Chile,  1924 (Mamalakis [1976, p. 20]
L.incometax.d.colombia  = data.frame(ifelse(L.clogit$year>=1935 & L.clogit$country == "Colombia",1,0)) # Colombia, Ley 78 Figueroa2008a, p. 9.
L.incometax.d.ecuador  = data.frame(ifelse(L.clogit$year>=1945 & L.clogit$country == "Ecuador",1,0)) # Ecuador Aguilera2013 p. 135
L.incometax.d.guatemala  = data.frame(ifelse(L.clogit$year>=1963 & L.clogit$country == "Guatemala",1,0)) # Guatemala: Decreto 1559, De2007 p 165
L.incometax.d.nicaragua  = data.frame(ifelse(L.clogit$year>=1974 & L.clogit$country == "Nicaragua",1,0)) # Ley No. 662 de 5 de Noviembre de 1974 (http://legislacion.asamblea.gob.ni/Normaweb.nsf/($All)/024063C3B373125E062570A10057EE73?OpenDocument)
L.incometax.d.peru  = data.frame(ifelse(L.clogit$year>=1934 & L.clogit$country == "Peru",1,0)) # Peru, Ley 7904 de 1934
L.incometax.d.venezuela  = data.frame(ifelse(L.clogit$year>=1943 & L.clogit$country == "Venezuela",1,0)) # Venezuela, Ley de Impuesto sobre la Renta, Publicada en la Gaceta Oficial número 20.851 del 17 de julio de 1.942, pero entra en vigencia el ano siguiente

## Summed across all the rows.
L.incometax.d = L.incometax.d.chile +  L.incometax.d.colombia +  L.incometax.d.ecuador +  L.incometax.d.guatemala +  L.incometax.d.nicaragua +  L.incometax.d.peru +  L.incometax.d.venezuela
colnames(L.incometax.d) = "incometax.d"

# attach "L.incometax.d" to the "L.clogit" dataset
## generate the dataset
aux5 = subset(L.clogit, select = c(country, year))
incometax.d = data.frame(aux5, L.incometax.d)

## merge with the dataset
L.clogit = merge(incometax.d, L.clogit, by=c("country", "year"))

## Saving Data
save(L.clogit, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/l_clogit.RData") # in paper's folder

### TESTING LAG
#### test2= data.frame(L.clogit$country, L.clogit$year, L.clogit$constmanufact.L, L.clogit$constmanufact, L.clogit$constagricult.L, L.clogit$constagricult)
##### IT WORKS


##################################################
##              MODELS 1
## [results:1]
##################################################


##################################################
### THIS CHUNK GOES BEFORE THE MODELS
library(texreg) # install.packages("texreg")
library(methods)

## ---- texreg-extractor-geeglm ----
extract.geepack <- function(model) {
        s <- summary(model)
        names <- rownames(s$coef)
        co <- s$coef[, 1]
        se <- s$coef[, 2]
        pval <- s$coef[, 4]
        
        n <- nrow(model.frame(model))
        nclust <- length(s$geese$clusz)
        
        gof = c(n, nclust)
        gof.names = c("Num. obs.", "Num. clust.")
        
        tr <- createTexreg(
                coef.names = names,
                coef = co,
                se = se,
                pvalues = pval,
                gof.names = gof.names,
                gof = gof,
                gof.decimal = rep(FALSE, length(gof))
        )
        return(tr)
}

setMethod("extract", signature = className("geeglm", "geepack"),
          definition = extract.geepack)
##################################################

rm(list=ls())
cat("\014")

# Load Datasets
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/incometax_data.RData") # Load data
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/cox.RData") # Cox
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/L_cox.RData") # Lagged Data for Cox
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/logitgee.RData") # Logit GEE
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/ag_data.RData") # For Multiple Non Competing Hazard Ratios
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/l_clogit.RData") # Lagged CONSTANT AGR MANUFACT for clogit  (fixed effects)

# Loading/Installing Library
# DO NOT TOUCH

# Model with time-transformed variables
library(survival) # install.packages("survival") 
cox1.tt = coxph(Surv(year, incometax.s) ~ tt(constmanufact) + tt(constagricult) + cluster(country), 
                data=cox, x=T
)

# DO NOT TOUCH
# WORKING MODEL
library(survival) # install.packages("survival") 
cox2 = coxph(Surv(year, incometax.s) ~ log(constmanufact) + log(constagricult) + log(totpop) + cluster(country), 
             data=cox
)

# DO NOT TOUCH
# LAGGED MODEL
library(survival) # install.packages("survival") 
cox.L = coxph(Surv(year, incometax.s) ~ log(constmanufact.L) + log(constagricult.L) + log(totpop) + cluster(country), 
              data=L.cox)


## logit GEE
library(geepack) # install.packages("geepack")
logitgee.1 = geeglm(incometax.d ~ log(constmanufact) + log(constagricult) + log(totpop), 
                    family = binomial, 
                    id = country, 
                    corstr = "exchangeable",
                    data = logitgee)
logitgee.1 = extract(logitgee.1)


# DONT TOUCH
# Recurrent Events: Income Tax AND Democracy
# WORKING MODEL
library(survival) # install.packages("survival") 
cox2.ag = coxph(Surv(year, dem.tax) ~ log(constmanufact) + log(constagricult) + log(urbpop) + cluster(country), 
                data=ag.data
)


# DO NOT TOUCH
# WORKING MODEL for clogit
library(survival) # install.packages("survival")
clogit.1 = clogit(incometax.d ~  log(constmanufact) + log(constagricult) +strata(country), method= "efron", data = data)



# screenreg / texreg
texreg(
        list(cox1.tt, cox2, cox.L, clogit.1, cox2.ag, logitgee.1),
        caption = "Structural Origins of Income Taxation: Income Tax Law and Democratic Development",
        custom.coef.names = c(
                "Manufacture Output$_{tt}$",
                "Agricultural Output$_{tt}$",
                "Manufacture Output  (ln)",
                "Agricultural Output (ln)",
                "Total Population  (ln)",
                "Manufacture Output$_{t-1}$  (ln)",
                "Agricultural Output$_{t-1}$  (ln)",
                "Urban Population  (ln)",
                "(intercept)"
        ),
        custom.model.names = c(
                "Cox-PH: Time Transformed",
                "Cox-PH: Logged",
                "Cox-PH: Lagged",
                "Conditional Logit: FE",
                "Cox-PH: Andersen-Gill",
                "Logit GEE"),
        label = "results:1",
        custom.note = "%stars. Robust Standard Errors in All Models",
        fontsize = "scriptsize",
        center = TRUE,
        no.margin = TRUE, 
        float.pos = "h"
)
        


##################################################
##              POST ESTIMATION
##################################################



################
# COX PROP
## [fig:simpleplots]
## [fig:coxassump]
################



# Preliminary Plots of the WORKING model
par(mfrow=c(2,1)) 
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
#### COX1 IT IS VIOLATED (NOT USING THIS ONE ANYMORE)
#### TIME DYNAMICS SHOULD BE INCORATED for COX1 i.e. log-time interactions
## Testing Prop. Assumption THIS PART OF THE CODE DOESNT HAVE TOO MUCH BACK UP PART FROM THE CODERS OF THESE FUNCIONS

#### LOGS
cox1.tt2 = coxph(Surv(year, incometax.s) ~ tt(constmanufact) + tt(constagricult) + cluster(country), 
                 data=cox, x=T, tt = c(function(x, t, ...) x * log(t+20), function(x, t, ...) x * log(t-20))
)

test.assumption = cox.zph(cox1.tt2, transform = 'log')
print(test.assumption)
plot(test.assumption[1]) # covariate 1
plot(test.assumption[2]) # covariate 2


#### SPLINES
##### this one here seems to work better compared to logs. Box-St. recommends SPLINES, so I will use them.
cox1.splines = coxph(Surv(year, incometax.s) ~ pspline(constmanufact, df=5) + pspline(constagricult, df=5) + cluster(country), 
                     data=cox)

termplot(cox1.splines, term=1, se=TRUE)
termplot(cox1.splines, term=2, se=TRUE)


########################################################
#### Simulation: Relative Hazard
#### [simulation:1] [simulation:2]
########################################################
# load library
library(survival)
# I use this one for simulation (since it seems that the sim function doesn't take well natural logs)
cox3 = coxph(Surv(year, incometax.s) ~ constmanufact + constagricult + totpop + cluster(country), 
             data=cox
)

library(texreg) # install.packages("texreg")
texreg(cox3,
        caption = "Structural Origins of Income Taxation: Model Used to Compute Simulations",
        custom.coef.names = c(
                "Manufacture Output",
                "Agricultural Output",
                "Total Population"),
        custom.model.names = "Cox-PH",
        label = "results:2",
        custom.note = "%stars. Robust Standard Errors in All Models",
        fontsize = "scriptsize",
        float.pos = "h"
        )

# install.packages("devtools")
# library(devtools)
# devtools::install_github('christophergandrud/simPH')
library(simPH)

set.seed(602)
sim3.m <- coxsimLinear(cox3, 
                       b = "constmanufact", 
                       qi = "Relative Hazard",
                       nsim = 2000,
                       spin = T,
                       Xj = seq(min(cox$constmanufact), max(cox$constmanufact), by=250)
)
set.seed(602)
sim3.a <- coxsimLinear(cox3, 
                       b = "constagricult", 
                       qi = "Relative Hazard",
                       nsim = 2000,
                       spin = T,
                       Xj = seq(min(cox$constagricult), max(cox$constagricult), by=150)
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
#### predicted survivor functions
########################################################

# main model
cox3.p = coxph(Surv(year, incometax.s) ~ constmanufact + constagricult + totpop, 
             data=cox
             )

# predictions with std. errors
cox3.p.d= data.frame(predict(cox3.p, type="expected", se.fit=TRUE))
cox.predicted= cbind(cox3.p.d, cox)


quantile(cox$constmanufact, .15)

# test
summary(survfit(cox3.p), constmanufact=quantile(cox$constmanufact, .15))
summary(survfit(cox3.p), constmanufact=quantile(cox$constmanufact, .75))

str(summary(survfit(cox3.p), time = 2))

summary(survfit(cox3.p), time=20)$surv


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

# 1 X's IT IS STARTING MAKING SENSE...NOW I NEED THE PLOTS
## WORKING HERE
logitgee.1.x.L <- setx(logitgee.1, ln.constmanufact = summary(logitgee$ln.constmanufact)[2])
logitgee.1.x.H <- setx(logitgee.1, ln.constmanufact = summary(logitgee$ln.constmanufact)[5])
set.seed(602)
logitgee.1.s <- sim(logitgee.1, x = logitgee.1.x.L, x1= logitgee.1.x.H, num=2500)



########################################################
#### TAX IMPOSITION PLOT
########################################################

load("/Users/hectorbahamonde/RU/Dissertation/Data/dissertation.Rdata") 
# Load data
library(ggplot2) # install.packages("ggplot2")
library(gridExtra)  # install.packages("gridExtra")

# To force GGplots to share same legend.
grid_arrange_shared_legend <- function(...) {
  require(ggplot2)
  require(gridExtra)
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = grid::unit.c(unit(1, "npc") - lheight, lheight))
}

#### multiplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#### plots
chile.p = ggplot() + 
  geom_smooth(data=subset(dissertation, country=="Chile"), aes(x=year, y=log(constagricult), colour="Agricultural Output"), fill=NA, size=1) +
  geom_smooth(data=subset(dissertation, country=="Chile"), aes(x=year, y=log(constmanufact), colour="Industrial Output"), fill=NA, size=1) + 
  xlab("Year") +
  ylab("GDP Output (ln)") +
  labs(colour = "Legend") +
  scale_x_continuous(limits=c(1890,2010)) + 
  geom_vline(data=subset(dissertation, country=="Chile"), aes(xintercept = 1924, colour= "Income Tax Law"), linetype = "longdash") + # Income Tax Law
  geom_vline(aes(xintercept = 1909, colour= "Democracy"), linetype = "longdash") + # Democracy Boix
  theme_bw() + 
  theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=10), axis.title.x = element_text(size=10), legend.text=element_text(size=15), legend.title=element_text(size=0))  + 
  labs(title="Chile")

peru.p = ggplot() + 
  geom_smooth(data=subset(dissertation, country=="Peru"), aes(x=year, y=log(constagricult), colour="Agricultural Output"), fill=NA, size=1) +
  geom_smooth(data=subset(dissertation, country=="Peru"), aes(x=year, y=log(constmanufact), colour="Industrial Output"), fill=NA, size=1) + 
  xlab("Year") +
  ylab("GDP Output (ln)") +
  labs(colour = "Legend") +
  scale_x_continuous(limits=c(1890,2010)) + 
  geom_vline(data=subset(dissertation, country=="Peru"), aes(xintercept = 1934, colour= "Income Tax Law"), linetype = "longdash") + # Income Tax Law
  geom_vline(aes(xintercept = 1956, colour= "Democracy"), linetype = "longdash") + # Democracy Boix
  theme_bw() + 
  theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=10), axis.title.x = element_text(size=10), legend.text=element_text(size=15), legend.title=element_text(size=0))  + 
  labs(title="Peru") 

colombia.p = ggplot() + 
  geom_smooth(data=subset(dissertation, country=="Colombia"), aes(x=year, y=log(constagricult), colour="Agricultural Output"), fill=NA, size=1) +
  geom_smooth(data=subset(dissertation, country=="Colombia"), aes(x=year, y=log(constmanufact), colour="Industrial Output"), fill=NA, size=1) + 
  xlab("Year") +
  ylab("GDP Output (ln)") +
  labs(colour = "Legend") +
  scale_x_continuous(limits=c(1890,2010)) + 
  geom_vline(data=subset(dissertation, country=="Colombia"), aes(xintercept = 1935, colour= "Income Tax Law"), linetype = "longdash") + # Income Tax Law
  geom_vline(aes(xintercept = 1937, colour= "Democracy"), linetype = "longdash") + # Democracy Boix
  theme_bw() + 
  theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=10), axis.title.x = element_text(size=10), legend.text=element_text(size=15), legend.title=element_text(size=0))  + 
  labs(title="Colombia") 

ecuador.p= ggplot() + 
  geom_smooth(data=subset(dissertation, country=="Ecuador"), aes(x=year, y=log(constagricult), colour="Agricultural Output"), fill=NA, size=1) +
  geom_smooth(data=subset(dissertation, country=="Ecuador"), aes(x=year, y=log(constmanufact), colour="Industrial Output"), fill=NA, size=1) + 
  xlab("Year") +
  ylab("GDP Output (ln)") +
  labs(colour = "Legend") +
  scale_x_continuous(limits=c(1890,2010)) + 
  geom_vline(data=subset(dissertation, country=="Ecuador"), aes(xintercept = 1945, colour= "Income Tax Law"), linetype = "longdash") + # Income Tax Law
  geom_vline(aes(xintercept = 1948, colour= "Democracy"), linetype = "longdash") + # Democracy Boix
  theme_bw() + 
  theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=10), axis.title.x = element_text(size=10), legend.text=element_text(size=15), legend.title=element_text(size=0))  + 
  labs(title="Ecuador") 


venezuela.p= ggplot() + 
  geom_smooth(data=subset(dissertation, country=="Venezuela"), aes(x=year, y=log(constagricult), colour="Agricultural Output"), fill=NA, size=1) +
  geom_smooth(data=subset(dissertation, country=="Venezuela"), aes(x=year, y=log(constmanufact), colour="Industrial Output"), fill=NA, size=1) + 
  xlab("Year") +
  ylab("GDP Output (ln)") +
  labs(colour = "Legend") +
  scale_x_continuous(limits=c(1890,2010)) + 
  geom_vline(data=subset(dissertation, country=="Venezuela"), aes(xintercept = 1943, colour= "Income Tax Law"), linetype = "longdash") + # Income Tax Law
  geom_vline(aes(xintercept = 1959, colour= "Democracy"), linetype = "longdash") + # Democracy Boix
  theme_bw() + 
  theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=10), axis.title.x = element_text(size=10), legend.text=element_text(size=15), legend.title=element_text(size=0))  + 
  labs(title="Venezuela") 

nicaragua.p= ggplot() + 
  geom_smooth(data=subset(dissertation, country=="Nicaragua"), aes(x=year, y=log(constagricult), colour="Agricultural Output"), fill=NA, size=1) +
  geom_smooth(data=subset(dissertation, country=="Nicaragua"), aes(x=year, y=log(constmanufact), colour="Industrial Output"), fill=NA, size=1) + 
  xlab("Year") +
  ylab("GDP Output (ln)") +
  labs(colour = "Legend") +
  scale_x_continuous(limits=c(1890,2010)) + 
  geom_vline(data=subset(dissertation, country=="Nicaragua"), aes(xintercept = 1974, colour= "Income Tax Law"), linetype = "longdash") + # Income Tax Law
  geom_vline(aes(xintercept = 1984, colour= "Democracy"), linetype = "longdash") + # Democracy Boix
  theme_bw() + 
  theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=10), axis.title.x = element_text(size=10), legend.text=element_text(size=15), legend.title=element_text(size=0))  + 
  labs(title="Nicaragua") 

guatemala.p= ggplot() + 
  geom_smooth(data=subset(dissertation, country=="Guatemala"), aes(x=year, y=log(constagricult), colour="Agricultural Output"), fill=NA, size=1) +
  geom_smooth(data=subset(dissertation, country=="Guatemala"), aes(x=year, y=log(constmanufact), colour="Industrial Output"), fill=NA, size=1) + 
  xlab("Year") +
  ylab("GDP Output (ln)") +
  labs(colour = "Income Tax (ln)") +
  scale_x_continuous(limits=c(1890,2010)) + 
  geom_vline(data=subset(dissertation, country=="Guatemala"), aes(xintercept = 1963, colour= "Income Tax Law"), linetype = "longdash") + # Income Tax Law
  geom_vline(aes(xintercept = 1945, colour= "Democracy"), linetype = "longdash") + # Democracy Boix
  theme_bw() + 
  theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=10), axis.title.x = element_text(size=10), legend.text=element_text(size=15), legend.title=element_text(size=0))  + 
  labs(title="Guatemala") 

grid_arrange_shared_legend(chile.p, ecuador.p, nicaragua.p, venezuela.p, peru.p, colombia.p, guatemala.p)





