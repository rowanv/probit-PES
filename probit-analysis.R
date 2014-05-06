library(foreign)
require(aod)
require(ggplot2)
require(plyr)

PSA<-read.dta(file="base\ ampliada\ 3.dta")

table(PSA$B1)
table(PSA$B2)

summary(PSA)

###############################
#########FUNCTIONS#############
###############################
#Rename variables
newvar.na <-function(var.new) #initializes a new variable with all values = NA
{
	var.new[1:length(PSA$F13)]<-rep(NA, length(PSA$F13))
	return(var.new)
}

##########RECODE VARIABLES############

#X1 gender
#X2 age
#X3 age squared
#X4 migration
#X5 principle 
#X6 punishment **
#X7 knowledge 
#X8 ethnicity
#X9 off farm income
#X10 farm income
#X11 area
#X12 trust 
#X13 educational index 
#X14 State Campeche
#X15 State Oaxaca 
#X16 State Quintana Roo 
#X17 Yucatan
#X18 Primary Income Agriculture
#X19 Primary Income Business
#X20 Primary Income Services
#X21 Primary Income Wood 
#X22 Primary Income Artisan
#X23 Primary Income Other

#Dependent Variables
PSA$vot.intent <- newvar.na(PSA$vot.intent)
PSA$vot.intent[PSA$C7 == "SI"] <- 1
PSA$vot.intent[PSA$C7 == "NO"] <- 0

PSA$actual.vote <- newvar.na(PSA$actual.vote)
PSA$actual.vote[PSA$C6 == "SI"] <- 1
PSA$actual.vote[PSA$C6 == "NO"] <- 0

PSA$choose.commun<-newvar.na(PSA$choose.commun) #C13
PSA$choose.commun[PSA$C13 == "A FAVOR"] <- 1
PSA$choose.commun[PSA$C13 == "EN CONTRA"] <- 0

PSA$leader.choose.commun<-newvar.na(PSA$leader.choose.commun)
PSA$leader.choose.commun[PSA$C14 == "SI"] <- 1 #C14
PSA$leader.choose.commun[PSA$C14 == "NO"] <- 0


#Independent Variables
#Variable 1: Gender
PSA$gender <- newvar.na(PSA$gender)
PSA$gender[PSA$B3=="FEMENINO"] <- 1
PSA$gender[PSA$B3=="MASCULINO"] <- 0

#Variable 2: Age
PSA$age <- newvar.na(PSA$age)
PSA$age<-PSA$B5A
#drop values less than 15, minimum age is 15
PSA$age[PSA$age<15]<-NA
table(PSA$age)

#Variable 3: Age Squared
PSA$age.squared <- newvar.na(PSA$age.squared)
PSA$age.squared <- PSA$age^2

#Variable 4: Migration
PSA$migration <- newvar.na(PSA$migration)
PSA$migration[PSA$B7 == "SI"] <- 1
PSA$migration[PSA$B7 == "NO"] <- 0

#Variable 5: Principle
#principles/permission cut down tree
PSA$principle <- newvar.na(PSA$principle)
PSA$principle[PSA$D12 == "SI"] <- 1
PSA$principle[PSA$D12 != "SI"] <- 0
#Add D13?

#Variable 6: Punishment
PSA$punishment <- newvar.na(PSA$punishment)
PSA$punishment[PSA$D14 == "SI"] <- 1
PSA$punishment[PSA$D14 != "SI"] <- 0

#Variable 7: Knowledge
#knowledge, received training about PES
PSA$knowledge <- newvar.na(PSA$knowledge)
PSA$knowledge[PSA$G5 == "SI"] <- 1
PSA$knowledge[PSA$G5 != "SI"] <- 0

#Variable 8: Ethnicity
PSA$ethnicity <- newvar.na(PSA$ethnicity)
PSA$ethnicity[PSA$B4 == "INDIGENA"] <- 1
PSA$ethnicity[PSA$B4 != "INDIGENA"] <- 0

#Variable 9: Off Farm Income
# 0 = no existence of off-farm income, 1 = existence of off-farm income
PSA$off.farm.income <- newvar.na(PSA$off.farm.income)
PSA$off.farm.income[PSA$F13 == "Ning\\xfan ingreso" ] <- 0
PSA$off.farm.income[PSA$F13 != "Ning\\xfan ingreso" ] <- 1

#Variable 10: Farm Income
#farm.income, 0 = no existence of farm income, 1 = existence farm income
PSA$farm.income <- newvar.na(PSA$farm.income)
PSA$farm.income[PSA$F11 == "Ning\\xfan ingreso" ] <- 0
PSA$farm.income[PSA$F11 != "Ning\\xfan ingreso" ] <- 1

#Variable 11: Area
PSA$area <- PSA$F1

#Variable 12: Trust
trust.numeric <- function(var.old, var.new)
{
	var.new[var.old=="1"]<-1
	var.new[var.old=="2"]<-2
	var.new[var.old=="3"]<-3
	var.new[var.old=="4"]<-4
	var.new[var.old=="5"]<-5
	var.new[var.old=="6"]<-6
	var.new[var.old=="7"]<-7
	return(var.new)
}


PSA$D1.r<-newvar.na(PSA$D1.r)
PSA$D1.r<-trust.numeric(PSA$D1, PSA$D1.r)

PSA$D2.r<-newvar.na(PSA$D2.r)
PSA$D2.r<-trust.numeric(PSA$D2, PSA$D2.r) 

PSA$D3.r<-newvar.na(PSA$D3.r)
PSA$D3.r<-trust.numeric(PSA$D3, PSA$D3.r)

PSA$D4.r<-newvar.na(PSA$D4.r)
PSA$D4.r<-trust.numeric(PSA$D4, PSA$D4.r)

PSA$D5.r<-newvar.na(PSA$D5.r)
PSA$D5.r<-trust.numeric(PSA$D5, PSA$D5.r)

PSA$D6.r<-newvar.na(PSA$D6.r)
PSA$D6.r<-trust.numeric(PSA$D6, PSA$D6.r)

PSA$D7.r<-newvar.na(PSA$D7.r)
PSA$D7.r <- trust.numeric(PSA$D7, PSA$D7.r)

PSA$D8.r<-newvar.na(PSA$D8.r)
PSA$D8.r <- trust.numeric(PSA$D8, PSA$D8.r)

PSA$D9.r<-newvar.na(PSA$D9.r)
PSA$D9.r <- trust.numeric(PSA$D9, PSA$D9.r)

PSA$trust<-newvar.na(PSA$trust)

PSA$trust<- PSA$D2.r + PSA$D3.r + PSA$D4.r + PSA$D5.r + PSA$D6.r + PSA$D7.r + PSA$D8.r + PSA$D9.r

PSA$trust<- PSA$trust/63
#trust is an index of 0 to 1 of all of the responses

#Variable 13: Educational Index
PSA$primary[PSA$PRIMARIA > 0] <- 1 #Any primary school, PRIMARIA is the number of years
PSA$secondary[PSA$SECUNDARIA > 0] <- 1 #Any secondary school, SECUNDARIA is number of years
PSA$bachill.re[PSA$BACHILLERATO>0] <- 1 #Any bachillerato, BACHILLERATO is number of years
#Have licenciatura, Licenciatura is text for area in which was obtained

#Education index
#Initializing variables for individual education measures
PSA$edu.index <- newvar.na(PSA$edu.index)
PSA$edu.none <- newvar.na(PSA$edu.none)
PSA$primary <- newvar.na(PSA$primary)
PSA$secondary <- newvar.na(PSA$secondary)
PSA$secondary.higher <- newvar.na(PSA$secondary.higher)
PSA$college <- newvar.na(PSA$college)
PSA$specialization <- newvar.na(PSA$specialization)
PSA$post.grad <- newvar.na(PSA$post.grad)

#transferring to new variables as numeric type variables
PSA$primary <- trust.numeric(PSA$PRIMARIA, PSA$primary)
PSA$secondary <- trust.numeric(PSA$SECUNDARIA, PSA$secondary)
PSA$secondary.higher <- trust.numeric(PSA$BACHILLERATO, PSA$secondary.higher)
PSA$college <- trust.numeric(PSA$LICENCIATURA, PSA$college)
PSA$specialization <- trust.numeric(PSA$ESPECIALIDAD, PSA$specialization)
PSA$post.grad <- trust.numeric(PSA$POSTGRADO, PSA$post.grad)

PSA$edu.none[PSA$B9A == "NINGUNO"] <- 1
PSA$college[PSA$B9A == "LICENCIATURA"] <- 1
PSA$specialization[PSA$B9A == "ESPECIALIDAD"] <- 1
PSA$post.grad[PSA$B9A == "POSGRADO"] <- 1



#don't have values for licenciatura, especialidad, posgrado
PSA$edu.index[PSA$edu.none == 1 ] <- 0
PSA$edu.index <- PSA$primary
PSA$edu.index[is.na(PSA$edu.index)] <- PSA$secondary + 6
PSA$edu.index[is.na(PSA$edu.index)] <- PSA$secondary.higher + 9 
PSA$edu.index[is.na(PSA$edu.index)] <- PSA$college + 12 + 4 #value is already 1
PSA$edu.index[is.na(PSA$edu.index)] <- PSA$specialization + 12 + 5
PSA$edu.index[is.na(PSA$edu.index)] <- PSA$post.grad + 12 + 7

PSA$edu.index <- PSA$edu.index/20

#Variables 14 through 17: States Campeche, Oaxaca, Quintana Roo, and Yucatan
PSA$State.Campeche <- newvar.na(PSA$State.Campeche)
PSA$State.Oaxaca <- newvar.na(PSA$State.Oaxaca)
PSA$State.Quintana.Roo <- newvar.na(PSA$State.Quintana.Roo)
PSA$State.Yucatan <- newvar.na(PSA$State.Yucatan)

PSA$State.Campeche[PSA$ESTADO=="CAMPECHE"] <- 1
PSA$State.Campeche[PSA$ESTADO!="CAMPECHE"] <- 0
PSA$State.Oaxaca[PSA$ESTADO=="OAXACA"] <- 1
PSA$State.Oaxaca[PSA$ESTADO!="OAXACA"] <- 0
PSA$State.Quintana.Roo[PSA$ESTADO=="QUINTANA ROO"] <- 1
PSA$State.Quintana.Roo[PSA$ESTADO!="QUINTANA ROO"] <- 0
PSA$State.Yucatan[PSA$ESTADO=="YUCAT\\xc1N"] <- 1
PSA$State.Yucatan[PSA$ESTADO!="YUCAT\\xc1N"] <- 0

#Variables 18 through 23: Primary Income, subdivided into Agriculture, Business, Services,
#Wood, Artisan and Other
PSA$pr.income.agriculture <- newvar.na(PSA$pr.income.agriculture)
PSA$pr.income.business <- newvar.na(PSA$pr.income.business)
PSA$pr.income.services <- newvar.na(PSA$pr.income.services)
PSA$pr.income.wood <- newvar.na(PSA$pr.income.wood)
PSA$pr.income.artisan <- newvar.na(PSA$pr.income.artisan)
PSA$pr.income.other <- newvar.na(PSA$pr.income.other)

PSA$pr.income.agriculture[PSA$F15 == "Agricultura"] <- 1
PSA$pr.income.agriculture[PSA$F15 != "Agricultura"] <- 0
PSA$pr.income.business[PSA$F15 == "Negocio propio"] <- 1
PSA$pr.income.business[PSA$F15 != "Negocio propio"] <- 0
PSA$pr.income.services[PSA$F15 == "Servicios"] <- 1
PSA$pr.income.services[PSA$F15 != "Servicios"] <- 0
PSA$pr.income.wood[PSA$F15 == "Venta de madera"] <- 1
PSA$pr.income.wood[PSA$F15 != "Venta de madera"] <- 0
PSA$pr.income.artisan[PSA$F15 == "Venta de artesan\\xedas"] <- 1
PSA$pr.income.artisan[PSA$F15 != "Venta de artesan\\xedas"] <- 0
PSA$pr.income.other[PSA$F15 == "Otra"] <- 1
PSA$pr.income.other[PSA$F15 != "Otra"] <- 0

#Variable 14: Deforestation, 0 = no deforestation, 1 = severe deforestation
PSA$deforestation <-newvar.na(PSA$deforestation)
PSA$deforestation[PSA$D15=="Bien conservado"] <- 0
PSA$deforestation[PSA$D15=="Algo deforestado"] <- (1/3)
PSA$deforestation[PSA$D15=="Moderadamente deforestado"] <- (2/3)
PSA$deforestation[PSA$D15=="Severamente deforestado"] <- 1

#Variables 15 through 19: Agricultural Income
#PSA$Ing_agri_480, PSA$ing_agri_481_720, PSA$ing_agri_721_960, PSA$ing_agri_961_1440, PSA$ing_agri_1441



#Additional dependent variables:
#Actual vote defined only for certain states
PSA$actual.vote.Campeche <- newvar.na(PSA$actual.vote.Campeche)
PSA$actual.vote.Oaxaca <- newvar.na(PSA$actual.vote.Oaxaca)
PSA$actual.vote.Quintana.Roo <- newvar.na(PSA$actual.vote.Quintana.Roo)
PSA$actual.vote.Campeche[PSA$State.Campeche==1] <- PSA$actual.vote
PSA$actual.vote.Oaxaca[PSA$State.Oaxaca==1] <- PSA$actual.vote
PSA$actual.vote.Quintana.Roo[PSA$State.Oaxaca==1] <- PSA$actual.vote


###################################
#########STATISTICS################
##################################

Y1 <- cbind(PSA$vot.intent) #voter intent, C7
Y2 <- cbind(PSA$actual.vote) #how did vote, C6
Y3 <- cbind(PSA$choose.commun) #How would they choose for community, C13
Y4 <- cbind(PSA$leader.choose.commun) #How would they choose for community if leader, C14

#Regressions for each state
Y5 <- cbind(PSA$actual.vote.Campeche) #C6 for Campeche
Y6 <- cbind(PSA$actual.vote.Oaxaca) #C6 for Oaxaca
Y7 <- cbind(PSA$actual.vote.Quintana.Roo) #C6 for Quintana Roo


#Regression for individual states


X<- cbind(PSA$gender, PSA$age, PSA$age.squared, PSA$migration,PSA$principle, PSA$punishment,
	PSA$knowledge, PSA$ethnicity, PSA$area, PSA$trust, PSA$edu.index, PSA$State.Campeche,
	PSA$State.Oaxaca, PSA$State.Quintana.Roo, PSA$pr.income.agriculture, PSA$pr.income.business,
	PSA$pr.income.services,PSA$pr.income.wood, PSA$pr.income.other, PSA$deforestation,
	 PSA$Ing_agri_480, PSA$ing_agri_481_720, PSA$ing_agri_721_960, PSA$ing_agri_961_1440,
	 PSA$ing_agri_1441) 
	 
X.Labels<-cbind("Gender", "Age", "Age Squared", "Migration", "Principle", "Punishment", "Knowledge",
	"Ethnicity", "Area", "Trust", "Education Index", "State Campeche", "State Oaxaca", 
	"State Quintana Roo", "Primary Income Agriculture", "Primary Income Business", 
	"Primary Income Services", "Primary Income Wood", "Primary Income Other", "Deforestation",
	"Agricultural Income 480", "Agricultural Income 481 720", "Agricultural Income 721 960",
	"Agricultural Income 961 1440", "Agricultural Income Over 1441")

	 #Removed because of insufficient observations: Off farm income, farm income, State Yucatan, 
	 #Primary Income Artisan

#X1 gender
#X2 age
#X3 age squared
#X4 migration
#X5 principle *
#X6 punishment *
#X7 knowledge ***
#X8 ethnicity
#X9 area
#X10 trust *
#X11 educational index *
#X12 State Campeche
#X13 State Oaxaca ***
#X14 State Quintana Roo ***
#X15 Primary Income Agriculture
#X16 Primary Income Business
#X17 Primary Income Services
#X18 Primary Income Wood (.)
#X19 Primary Income Other
#X20 Deforestation
#X21 Agricultural Income Under 480
#X22 Agricultural Income 481 - 720
#X23 Agricultural Income 721-960
#X24 Agricultural Income 961-1440
#X25 Agricultural Income 1441 and over
	 
summary(Y1)
summary(X)
table(Y1)


# Probit model coefficients
probit<- glm(Y1 ~ X, family=binomial (link="probit"))
summary(probit)


# Probit model average marginal effects
ProbitScalar <- mean(dnorm(predict(probit, type = "link")))
ProbitScalar * coef(probit)

# Probit model predicted probabilities
pprobit<- predict(probit, type="response")
summary(pprobit)


#Confidence Intervals - R

confint(probit)

# McFadden's Pseudo R-squared
probit0<-update(probit, formula= Y1 ~ 1)
McFadden<- 1-as.vector(logLik(probit)/logLik(probit0))
McFadden

#Chi Squared
#wald.test(b = coef(probit), Sigma = vcov(probit), Terms = 1:25)

#X1 gender
#X2 age
#X3 age squared
#X4 migration
#X5 principle 
#X6 punishment 
#X7 knowledge 
#X8 ethnicity
#X9 area (.)
#X10 trust 
#X11 educational index 
#X12 State Campeche
#X13 State Oaxaca 
#X14 State Quintana Roo 
#X15 Primary Income Agriculture
#X16 Primary Income Business
#X17 Primary Income Services
#X18 Primary Income Wood 
#X19 Primary Income Other
#X20 Deforestation
#X21 Agricultural Income Under 480
#X22 Agricultural Income 481 - 720
#X23 Agricultural Income 721-960
#X24 Agricultural Income 961-1440
#X25 Agricultural Income 1441 and over

####################
#ACTUAL VOTE#
###################

print("ACTUAL VOTE:")

#Descriptive statistics
summary(Y2)
summary(X)

table(Y2)
table(Y2)/sum(table(Y2))

# Probit model coefficients
probit<- glm(Y2 ~ X, family=binomial (link="probit"))
summary(probit)



# Probit model average marginal effects
ProbitScalar <- mean(dnorm(predict(probit, type = "link")))
ProbitScalar * coef(probit)

# Probit model predicted probabilities
pprobit<- predict(probit, type="response")
summary(pprobit)


#Confidence Intervals - R

confint(probit)

# McFadden's Pseudo R-squared
probit0<-update(probit, formula= Y2 ~ 1)
McFadden<- 1-as.vector(logLik(probit)/logLik(probit0))
McFadden

#Chi Squared
#wald.test(b = coef(probit), Sigma = vcov(probit), Terms = 1:25)

#X1 gender
#X2 age
#X3 age squared
#X4 migration
#X5 principle *
#X6 punishment *
#X7 knowledge ***
#X8 ethnicity
#X9 area
#X10 trust *
#X11 educational index *
#X12 State Campeche
#X13 State Oaxaca ***
#X14 State Quintana Roo ***
#X15 Primary Income Agriculture
#X16 Primary Income Business
#X17 Primary Income Services
#X18 Primary Income Wood (.)
#X19 Primary Income Other
#X20 Deforestation
#X21 Agricultural Income Under 480
#X22 Agricultural Income 481 - 720
#X23 Agricultural Income 721-960
#X24 Agricultural Income 961-1440
#X25 Agricultural Income 1441 and over



#########################
#ACTUAL VOTE -- Campeche#
#########################
print("ACTUAL VOTE: Campeche")

#Descriptive statistics
summary(Y5)
summary(X)

table(Y5)
table(Y5)/sum(table(Y5))

# Probit model coefficients
probit<- glm(Y5 ~ X, family=binomial (link="probit"))
summary(probit)



# Probit model average marginal effects
ProbitScalar <- mean(dnorm(predict(probit, type = "link")))
ProbitScalar * coef(probit)

# Probit model predicted probabilities
pprobit<- predict(probit, type="response")
summary(pprobit)


#Confidence Intervals - R

confint(probit)

# McFadden's Pseudo R-squared
probit0<-update(probit, formula= Y5 ~ 1)
McFadden<- 1-as.vector(logLik(probit)/logLik(probit0))
McFadden

#Chi Squared
#wald.test(b = coef(probit), Sigma = vcov(probit), Terms = 1:13)

#X1 gender
#X2 age
#X3 age squared
#X4 migration
#X5 principle *
#X6 punishment *
#X7 knowledge ***
#X8 ethnicity
#X9 area
#X10 trust *
#X11 educational index *
#X12 State Campeche
#X13 State Oaxaca ***
#X14 State Quintana Roo ***
#X15 Primary Income Agriculture
#X16 Primary Income Business
#X17 Primary Income Services
#X18 Primary Income Wood (.)
#X19 Primary Income Other
#X20 Deforestation
#X21 Agricultural Income Under 480
#X22 Agricultural Income 481 - 720
#X23 Agricultural Income 721-960
#X24 Agricultural Income 961-1440
#X25 Agricultural Income 1441 and over



#########################
#ACTUAL VOTE -- Oaxaca#
#########################

print("ACTUAL VOTE:")

#Descriptive statistics
summary(Y6)
summary(X)

table(Y6)
table(Y6)/sum(table(Y6))

# Probit model coefficients
probit<- glm(Y6 ~ X, family=binomial (link="probit"))
summary(probit)



# Probit model average marginal effects
ProbitScalar <- mean(dnorm(predict(probit, type = "link")))
ProbitScalar * coef(probit)

# Probit model predicted probabilities
pprobit<- predict(probit, type="response")
summary(pprobit)


#Confidence Intervals - R

confint(probit)

# McFadden's Pseudo R-squared
probit0<-update(probit, formula= Y6 ~ 1)
McFadden<- 1-as.vector(logLik(probit)/logLik(probit0))
McFadden

#Chi Squared
#wald.test(b = coef(probit), Sigma = vcov(probit), Terms = 1:13)

#X1 gender
#X2 age
#X3 age squared
#X4 migration
#X5 principle *
#X6 punishment *
#X7 knowledge ***
#X8 ethnicity
#X9 area
#X10 trust *
#X11 educational index *
#X12 State Campeche
#X13 State Oaxaca ***
#X14 State Quintana Roo ***
#X15 Primary Income Agriculture
#X16 Primary Income Business
#X17 Primary Income Services
#X18 Primary Income Wood (.)
#X19 Primary Income Other
#X20 Deforestation
#X21 Agricultural Income Under 480
#X22 Agricultural Income 481 - 720
#X23 Agricultural Income 721-960
#X24 Agricultural Income 961-1440
#X25 Agricultural Income 1441 and over

?glm


#########################
#ACTUAL VOTE -- Quintana Roo#
#########################

print("ACTUAL VOTE:")

#Descriptive statistics
summary(Y7)
summary(X)

table(Y7)
table(Y7)/sum(table(Y7))

# Probit model coefficients
probit<- glm(Y7 ~ X, family=binomial (link="probit"))
summary(probit)



# Probit model average marginal effects
ProbitScalar <- mean(dnorm(predict(probit, type = "link")))
ProbitScalar * coef(probit)

# Probit model predicted probabilities
pprobit<- predict(probit, type="response")
summary(pprobit)


#Confidence Intervals - R

confint(probit)

# McFadden's Pseudo R-squared
probit0<-update(probit, formula= Y7 ~ 1)
McFadden<- 1-as.vector(logLik(probit)/logLik(probit0))
McFadden

#Chi Squared
#wald.test(b = coef(probit), Sigma = vcov(probit), Terms = 1:13)

#X1 gender
#X2 age
#X3 age squared
#X4 migration
#X5 principle *
#X6 punishment *
#X7 knowledge ***
#X8 ethnicity
#X9 area
#X10 trust *
#X11 educational index *
#X12 State Campeche
#X13 State Oaxaca ***
#X14 State Quintana Roo ***
#X15 Primary Income Agriculture
#X16 Primary Income Business
#X17 Primary Income Services
#X18 Primary Income Wood (.)
#X19 Primary Income Other
#X20 Deforestation
#X21 Agricultural Income Under 480
#X22 Agricultural Income 481 - 720
#X23 Agricultural Income 721-960
#X24 Agricultural Income 961-1440
#X25 Agricultural Income 1441 and over


###########
#Merge
##########




#Load data

Lider<-read.csv(file="/Users/rowanvasquez/Documents/BID/EncuestaLiderComunitario.csv", header=TRUE, fileEncoding="latin1", stringsAsFactors=FALSE)



Lider$LOCALIDAD<-Lider$Localidad

Lider$LOCALIDAD <- toupper(Lider$LOCALIDAD) #change to upper case

table(Lider$LOCALIDAD)
table(PSA$LOCALIDAD)
#Edit so have same spelling

Lider$LOCALIDAD[Lider$LOCALIDAD == "ABAL\\u0087"] <- "ABAL\\xc1"
Lider$LOCALIDAD[Lider$LOCALIDAD == "ARROYO CHOAPAN"] <- "ARROYO CHO\\xc1PAN"
Lider$LOCALIDAD[Lider$LOCALIDAD == "BECANCHEN"] <- "BECANCH\\xc9N"
Lider$LOCALIDAD[Lider$LOCALIDAD == "CANCAB\\u008EN"] <- "CANCABCH\\xc9N"
Lider$LOCALIDAD[Lider$LOCALIDAD == "CATMIS"] <- "CATM\\xcdS"
Lider$LOCALIDAD[Lider$LOCALIDAD == "CHACSINKIN"] <- "CHACSINK\\xcdN"
Lider$LOCALIDAD[Lider$LOCALIDAD == "CITINCABCHEN"] <- "CITINCABCH\\xc9N"
Lider$LOCALIDAD[Lider$LOCALIDAD == "CONSTITUCI\\u0097N"] <- "CONSTITUCI\\xd3N"
Lider$LOCALIDAD[Lider$LOCALIDAD == "DZUL\\u0087"] <- "DZUL\\xc1"
Lider$LOCALIDAD[Lider$LOCALIDAD == "FORTINO J PINACHO"] <- "FORTINO J. PINACHO"
Lider$LOCALIDAD[Lider$LOCALIDAD == "HOCAB\\u0087"] <- "HOCAB\\xc1"
Lider$LOCALIDAD[Lider$LOCALIDAD == "IXTLAN DE JU\\u0087REZ"] <- "IXTL\\xc1N DE JU\\xc1REZ"
Lider$LOCALIDAD[Lider$LOCALIDAD == "JOSE MAR\\u0092A MORELOS"] <- "JOS\\xc9 MAR\\xcdA MORELOS"
Lider$LOCALIDAD[Lider$LOCALIDAD == "LIBRE UNION"] <- "LIBRE UNI\\xd3N"
Lider$LOCALIDAD[Lider$LOCALIDAD == "MAN\\u0092"] <- "MAN\\xcd"
Lider$LOCALIDAD[Lider$LOCALIDAD == "NOH BEC"] <- "NOH-BEC"
Lider$LOCALIDAD[Lider$LOCALIDAD == "PABLO GARC\\u0092A"] <- "PABLO GARC\\xcdA"
Lider$LOCALIDAD[Lider$LOCALIDAD == "PASO NUEVO LA AMACA"] <- "PASO NUEVO LA HAMACA"
Lider$LOCALIDAD[Lider$LOCALIDAD == "SABAN"] <- "SAB\\xc1N"
Lider$LOCALIDAD[Lider$LOCALIDAD == "SAHCAB\\u0087"] <- "SAHCAB\\xc1"
Lider$LOCALIDAD[Lider$LOCALIDAD == "SAN AGUST\\u0092N"] <- "SAN AGUST\\xcdN"
Lider$LOCALIDAD[Lider$LOCALIDAD == "SAN ANDRES NUXI\\u0096O"] <- "SAN ANDR\\xc9S NUXI\\xd1O"
Lider$LOCALIDAD[Lider$LOCALIDAD == "SAN JOS\\u008e CHILTEPEC"] <- "SAN JOS\\xc9 CHILTEPEC"
Lider$LOCALIDAD[Lider$LOCALIDAD == "SAN JUAN BAUTISTA ATATHAUCA"] <- "SAN JUAN BAUTISTA ATATLAHUCA"
Lider$LOCALIDAD[Lider$LOCALIDAD == "SANTA MARIA JACATEPEC"] <- "SANTA MAR\\xcdA JACATEPEC"
Lider$LOCALIDAD[Lider$LOCALIDAD == "SANTO DOMINGO NUXAA"] <- "SANTO DOMINGO NUXA\\xc1"
Lider$LOCALIDAD[Lider$LOCALIDAD == "SANTO TOMAS TEXAS"] <- "SANTO TOM\\xc1S TEXAS"
Lider$LOCALIDAD[Lider$LOCALIDAD == "SE\\u0096OR"] <- "SE\\xd1OR"
Lider$LOCALIDAD[Lider$LOCALIDAD == "TAHDZIBICHEN"] <- "TAHDZIBICH\\xc9N"
Lider$LOCALIDAD[Lider$LOCALIDAD == "TECAX"] <- "TEKAX DE \\xc1LVARO OBREG\\xd3N"
Lider$LOCALIDAD[Lider$LOCALIDAD == "TEMOZON"] <- "TEMOZ\\xd3N"
Lider$LOCALIDAD[Lider$LOCALIDAD == "TIXCUYTUN"] <- " TIXCUYT\\xdaN"
Lider$LOCALIDAD[Lider$LOCALIDAD == "VALENTIN"] <- "VALENT\\xcdN G\\xd3MEZ FAR\\xcdAS"
Lider$LOCALIDAD[Lider$LOCALIDAD == "VEGA DEL SOL"] <- "VEGA DE SOL"
Lider$LOCALIDAD[Lider$LOCALIDAD == "X-CANHA"] <- "X-CANH\\xc1"
Lider$LOCALIDAD[Lider$LOCALIDAD == "X-PICIHL"] <- "X-PICHIL"
Lider$LOCALIDAD[Lider$LOCALIDAD == "XOHUAYAN"] <- "XOHUAY\\xc1N"
Lider$LOCALIDAD[Lider$LOCALIDAD == "YAXCABA"] <- "YAXCAB\\xc1"
Lider$LOCALIDAD[Lider$LOCALIDAD == "YAXHUNAH"] <- "YAXUNAH"
Lider$LOCALIDAD[Lider$LOCALIDAD == "YOTOLIN"] <- "YOTHOL\\xcdN"
Lider$LOCALIDAD[Lider$LOCALIDAD == "ZOH-LAGUNA"] <- "ZOH-LAGUNA (\\xc1LVARO OBREG\\xd3N)"


#Merge the two data sets by Localidad, as there is only one leader per localidad


PSA <- merge(PSA, Lider, by="LOCALIDAD", all.x=TRUE)

write.csv(PSA, file = "EcosystemServicesDatabase.csv")
