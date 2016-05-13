#### MARCH 2, 2016
#### MODERNIZATION THEORY SAYS THAT INCOME PER CAPITA PREDICTS DEMOCRACY. HERE I COIULD INCLUDE THE "madisonpercapgdp" VAR. 
#### HOWEVER, INDUSTRIAL AND AGRICULTURAL OUTPUTS NATURALLY TRANSLATE INTO HIGHER LEVELS OF INCOME PER CAPITA.
#### IN FACT, MANY OF THE MODELS DID NOT CONVERGE... PRESUMABLY B/C OF MULTICOLLINEARITY. 


# DO NOT TOUCH
# Model with time-transformed variables
cox1.tt = coxph(Surv(year, incometax.s) ~ tt(constmanufact) + tt(madisonpercapgdp) +  tt(constagricult) + cluster(country), 
                data=cox, x=T
)

# DO NOT TOUCH
# WORKING MODEL
cox2 = coxph(Surv(year, incometax.s) ~ log(constmanufact)+ log(constagricult) + log(madisonpercapgdp)  + cluster(country), 
             data=cox
)

# DO NOT TOUCH
# LAGGED MODEL
cox.L = coxph(Surv(year, incometax.s) ~ log(constmanufact.L) + log(constagricult.L) + log(madisonpercapgdp) + cluster(country), 
              data=L.cox)

logitgee$ln.madisonpercapgdp = log(logitgee$madisonpercapgdp)
## logit GEE
logitgee.1 = zelig(incometax.d ~ ln.constmanufact + ln.constagricult + ln.madisonpercapgdp, 
                   model = "logit.gee", 
                   id = "country", 
                   data = na.omit(logitgee))

# DONT TOUCH
# Recurrent Events: Income Tax AND Democracy
# WORKING MODEL
cox2.ag = coxph(Surv(year, dem.tax) ~ log(constmanufact) + log(constagricult)+ log(madisonpercapgdp) + log(urbpop) + cluster(country), 
                data=ag.data
)


# DO NOT TOUCH
# WORKING MODEL for clogit
clogit.1 = clogit(incometax.d ~  log(constmanufact) + log(constagricult)+ log(madisonpercapgdp) +strata(country), method= "efron", data = data)


# Table
stargazer(cox1.tt, cox2, cox.L, clogit.1, cox2.ag, #logitgee.1, 
          type = "text", # change to "latex"/"text" when nedded
          column.labels = c("Cox PH", "Cond. Logit", "Andersen-Gill"),
          column.separate = c(3, 1, 1),
          model.names = F,
          style = "apsr")
