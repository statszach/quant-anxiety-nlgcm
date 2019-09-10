##########################################
## Quant Anxiety Predicting Alcohol Use ##
##########################################

##Importing Data

library(readxl)
ThesisFA17Data <- read_excel("ThesisFA17Data.xlsx")
ThesisSP18Data <- read_excel("ThesisSP18Data.xlsx")

F17Data <-as.data.frame(ThesisFA17Data)
S18Data <-as.data.frame(ThesisSP18Data)

##Reverse Scoring

F17Data$Stress4B_r <- 5 - F17Data$Stress4B
F17Data$Stress5B_r <- 5 - F17Data$Stress5B
F17Data$Stress6B_r <- 5 - F17Data$Stress6B
F17Data$Stress7B_r <- 5 - F17Data$Stress7B
F17Data$Stress9B_r <- 5 - F17Data$Stress9B
F17Data$Stress10B_r <- 5 - F17Data$Stress10B
F17Data$Stress13B_r <- 5 - F17Data$Stress13B

F17Data$Stress4M_r <- 5 - F17Data$Stress4M
F17Data$Stress5M_r <- 5 - F17Data$Stress5M
F17Data$Stress6M_r <- 5 - F17Data$Stress6M
F17Data$Stress7M_r <- 5 - F17Data$Stress7M
F17Data$Stress9M_r <- 5 - F17Data$Stress9M
F17Data$Stress10M_r <- 5 - F17Data$Stress10M
F17Data$Stress13M_r <- 5 - F17Data$Stress13M

F17Data$Stress4E_r <- 5 - F17Data$Stress4E
F17Data$Stress5E_r <- 5 - F17Data$Stress5E
F17Data$Stress6E_r <- 5 - F17Data$Stress6E
F17Data$Stress7E_r <- 5 - F17Data$Stress7E
F17Data$Stress9E_r <- 5 - F17Data$Stress9E
F17Data$Stress10E_r <- 5 - F17Data$Stress10E
F17Data$Stress13E_r <- 5 - F17Data$Stress13E

S18Data$Stress4B_r <- 5 - S18Data$Stress4B
S18Data$Stress5B_r <- 5 - S18Data$Stress5B
S18Data$Stress6B_r <- 5 - S18Data$Stress6B
S18Data$Stress7B_r <- 5 - S18Data$Stress7B
S18Data$Stress9B_r <- 5 - S18Data$Stress9B
S18Data$Stress10B_r <- 5 - S18Data$Stress10B
S18Data$Stress13B_r <- 5 - S18Data$Stress13B

S18Data$Stress4M_r <- 5 - S18Data$Stress4M
S18Data$Stress5M_r <- 5 - S18Data$Stress5M
S18Data$Stress6M_r <- 5 - S18Data$Stress6M
S18Data$Stress7M_r <- 5 - S18Data$Stress7M
S18Data$Stress9M_r <- 5 - S18Data$Stress9M
S18Data$Stress10M_r <- 5 - S18Data$Stress10M
S18Data$Stress13M_r <- 5 - S18Data$Stress13M

S18Data$Stress4E_r <- 5 - S18Data$Stress4E
S18Data$Stress5E_r <- 5 - S18Data$Stress5E
S18Data$Stress6E_r <- 5 - S18Data$Stress6E
S18Data$Stress7E_r <- 5 - S18Data$Stress7E
S18Data$Stress9E_r <- 5 - S18Data$Stress9E
S18Data$Stress10E_r <- 5 - S18Data$Stress10E
S18Data$Stress13E_r <- 5 - S18Data$Stress13E

#Step 1: Subset for variables of interest (theoretical justification).

#Subsetting for variables used in analysis

SubsetvarF17 <- c("ID", "QANX1B", "QANX2B", "QANX3B", "QANX4B", 
                  "QANX1M", "QANX2M", "QANX3M", "QANX4M", 
                  "QANX1E", "QANX2E", "QANX3E", "QANX4E", 
                  "Anx1B", "Anx2B", "Anx3B", "Anx4B", "Anx5B", "Anx6B", "Anx7B", 
                  "Anx1M", "Anx2M", "Anx3M", "Anx4M", "Anx5M", "Anx6M", "Anx7M",
                  "Anx1E", "Anx2E", "Anx3E", "Anx4E", "Anx5E", "Anx6E", "Anx7E", 
                  "AlcoholB", "AlcoholM", "AlcoholE", "Gender")

F17 <- F17Data[SubsetvarF17]

F17$Gender <- as.numeric(F17$Gender)

SubsetvarS18 <- c("ID", "QANX1B", "QANX2B", "QANX3B", "QANX4B", 
                  "QANX1M", "QANX2M", "QANX3M", "QANX4M", 
                  "QANX1E", "QANX2E", "QANX3E", "QANX4E",
                  "AlcoholB", "AlcoholM", "AlcoholE", "Gender", 
                  "DASS15B", "DASS16B", "DASS17B", "DASS18B", "DASS19B", "DASS20B", "DASS21B",
                  "DASS15M", "DASS16M", "DASS17M", "DASS18M", "DASS19M", "DASS20M", "DASS21M",
                  "DASS15E", "DASS16E", "DASS17E", "DASS18E", "DASS19E", "DASS20E", "DASS21E",
                  "QuantClassB", "QuantClassM", "QuantClassE", 
                  "CigaratteB", "CigaratteM", "CigaratteE", "ECigB", "ECigM", "ECigE")

S18 <- S18Data[SubsetvarS18]


#Step 2: MCAR/MVN Test.


library(MissMech)

MCARF17 <- TestMCARNormality(data=F17)
MCARS18 <- TestMCARNormality(data=S18)

#F17 test would not compute due to linear dependency in the data. 
#S18 test failed (Hawkins Test p value < .001). 
#Since neither MCAR or Multivarate Normality could be assumed, opted for complte case analyssis.

#Step 3: Factor Analysis and Coefficient Omega.

library(psych)

F17QAnxB <- c("QANX1B", "QANX2B", "QANX3B", "QANX4B")
F17QAnxBOCC <-F17CC[F17QAnxB]
omega(F17QAnxBOCC, fm="ml") #omega = .91, alpha = .86

QANX_FAB <-fa(F17QAnxBOCC, factors=1,rotation="promax",scores="regression")
QANX_FSB <- as.data.frame(QANX_FAB$scores)
F17CC <- cbind(F17CC, QANX_FSB)
F17CC$QANX_FSB <- as.numeric(unlist(QANX_FSB))

F17QAnxM <- c("QANX1M", "QANX2M", "QANX3M", "QANX4M")
F17QAnxMOCC <-F17CC[F17QAnxM]
omega(F17QAnxMOCC, fm="ml") #omega = .95, alpha = .91

QANX_FAM <-fa(F17QAnxMOCC, factors=1,rotation="promax",scores="regression")
QANX_FSM <- as.data.frame(QANX_FAM$scores)
F17CC <- cbind(F17CC, QANX_FSM)
F17CC$QANX_FSM <- as.numeric(unlist(QANX_FSM))

F17QAnxE <- c("QANX1E", "QANX2E", "QANX3E", "QANX4E")
F17QAnxEOCC <-F17CC[F17QAnxE]
omega(F17QAnxEOCC, fm="ml") #omega = .98, alpha = .96

QANX_FAE <-fa(F17QAnxEOCC, factors=1,rotation="promax",scores="regression")
QANX_FSE <- as.data.frame(QANX_FAE$scores)
F17CC <- cbind(F17CC, QANX_FSE)
F17CC$QANX_FSE <- as.numeric(unlist(QANX_FSE))

S18QAnxB <- c("QANX1B", "QANX2B", "QANX3B", "QANX4B")
S18QAnxBOCC <-S18CC[S18QAnxB]
omega(S18QAnxBOCC, fm="ml") #omega = .96, alpha = .94

QANX_FAB <-fa(S18QAnxBOCC, factors=1,rotation="promax",scores="regression")
QANX_FSB <- as.data.frame(QANX_FAB$scores)
S18CC <- cbind(S18CC, QANX_FSB)
S18CC$QANX_FSB <- as.numeric(unlist(QANX_FSB))

S18QAnxM <- c("QANX1M", "QANX2M", "QANX3M", "QANX4M")
S18QAnxMOCC <-S18CC[S18QAnxM]
omega(S18QAnxMOCC, fm="ml") #omega = .96, alpha = .95

QANX_FAM <-fa(S18QAnxMOCC, factors=1,rotation="promax",scores="regression")
QANX_FSM <- as.data.frame(QANX_FAM$scores)
S18CC <- cbind(S18CC, QANX_FSM)
S18CC$QANX_FSM <- as.numeric(unlist(QANX_FSM))

S18QAnxE <- c("QANX1E", "QANX2E", "QANX3E", "QANX4E")
S18QAnxEOCC <-S18CC[S18QAnxE]
omega(S18QAnxEOCC, fm="ml") #omega = .96, alpha = .94

QANX_FAE <-fa(S18QAnxEOCC, factors=1,rotation="promax",scores="regression")
QANX_FSE <- as.data.frame(QANX_FAE$scores)
S18CC <- cbind(S18CC, QANX_FSE)
S18CC$QANX_FSE <- as.numeric(unlist(QANX_FSE))

F17AnxB <- c("Anx1B", "Anx2B", "Anx3B", "Anx4B", "Anx5B", "Anx6B", "Anx7B")
F17AnxBOCC <- F17CC[F17AnxB]
omega(F17AnxBOCC, fm="ml") #omega = .96, alpha = .91

ANX_FAB <-fa(F17AnxBOCC, factors=1,rotation="promax",scores="regression")
ANX_FSB <- as.data.frame(ANX_FAB$scores)
F17CC <- cbind(F17CC, ANX_FSB)
F17CC$ANX_FSB <- as.numeric(unlist(ANX_FSB))

F17AnxM <- c("Anx1M", "Anx2M", "Anx3M", "Anx4M", "Anx5M", "Anx6M", "Anx7M")
F17AnxMOCC <- F17CC[F17AnxM]
omega(F17AnxMOCC, fm="ml") #omega = .97, alpha = .96

ANX_FAM <-fa(F17AnxMOCC, factors=1,rotation="promax",scores="regression")
ANX_FSM <- as.data.frame(ANX_FAM$scores)
F17CC <- cbind(F17CC, ANX_FSM)
F17CC$ANX_FSM <- as.numeric(unlist(ANX_FSM))

F17AnxE <- c("Anx1E", "Anx2E", "Anx3E", "Anx4E", "Anx5E", "Anx6E", "Anx7E")
F17AnxEOCC <- F17CC[F17AnxE]
omega(F17AnxEOCC, fm="ml") #omega = .97, alpha = .95

ANX_FAE <-fa(F17AnxEOCC, factors=1,rotation="promax",scores="regression")
ANX_FSE <- as.data.frame(ANX_FAE$scores)
F17CC <- cbind(F17CC, ANX_FSE)
F17CC$ANX_FSE <- as.numeric(unlist(ANX_FSE))

S18AnxB <- c("DASS15B", "DASS16B", "DASS17B", "DASS18B", "DASS19B", "DASS20B", "DASS21B")
S18AnxBOCC <- S18CC[S18AnxB]
omega(S18AnxBOCC, fm="ml") #omega = .93, alpha = .9

ANX_FAB <-fa(S18AnxBOCC, factors=1,rotation="promax",scores="regression")
ANX_FSB <- as.data.frame(ANX_FAB$scores)
S18CC <- cbind(S18CC, ANX_FSB)
S18CC$ANX_FSB <- as.numeric(unlist(ANX_FSB))

S18AnxM <- c("DASS15M", "DASS16M", "DASS17M", "DASS18M", "DASS19M", "DASS20M", "DASS21M")
S18AnxMOCC <- S18CC[S18AnxM]
omega(S18AnxMOCC, fm="ml") #omega = .96, alpha = .94

ANX_FAM <-fa(S18AnxMOCC, factors=1,rotation="promax",scores="regression")
ANX_FSM <- as.data.frame(ANX_FAM$scores)
S18CC <- cbind(S18CC, ANX_FSM)
S18CC$ANX_FSM <- as.numeric(unlist(ANX_FSM))

S18AnxE <- c("DASS15E", "DASS16E", "DASS17E", "DASS18E", "DASS19E", "DASS20E", "DASS21E")
S18AnxEOCC <- S18CC[S18AnxE]
omega(S18AnxEOCC, fm="ml") #omega = .96, alpha = .94

ANX_FAE <-fa(S18AnxEOCC, factors=1,rotation="promax",scores="regression")
ANX_FSE <- as.data.frame(ANX_FAE$scores)
S18CC <- cbind(S18CC, ANX_FSE)
S18CC$ANX_FSE <- as.numeric(unlist(ANX_FSE))

#Step 5: Descriptive Statistics and Assumption Checks.

hist(S18CC$AlcoholB)
hist(S18CC$AlcoholM)
hist(S18CC$AlcoholE)
plot(density(S18CC$AlcoholB))
plot(density(S18CC$AlcoholM))
plot(density(S18CC$AlcoholE))

mbq <-lm(S18CC$AlcoholB~S18CC$QANX_FSB)
plot(mbq)
mmq <-lm(S18CC$AlcoholM~S18CC$QANX_FSM)
plot(mmq)
meq <-lm(S18CC$AlcoholE~S18CC$QANX_FSE)
plot(meq)

mba <-lm(S18CC$AlcoholB~S18CC$ANX_FSB)
plot(mba)
mma <-lm(S18CC$AlcoholM~S18CC$ANX_FSM)
plot(mma)
mea <-lm(S18CC$AlcoholE~S18CC$ANX_FSE)
plot(mea)

#Looked like issues of heavy tails in Q-Q plots, decided to transform alcohol use.

F17$AlcB1 <- (F17$AlcoholB+1)
F17$AlcM1 <- (F17$AlcoholM+1)
F17$AlcE1 <- (F17$AlcoholE+1)

F17$AlcBLog <- log(F17$AlcB1)
F17$AlcMLog <- log(F17$AlcM1)
F17$AlcELog <- log(F17$AlcE1)

S18$AlcB1 <- (S18$AlcoholB+1)
S18$AlcM1 <- (S18$AlcoholM+1)
S18$AlcE1 <- (S18$AlcoholE+1)

S18$AlcBLog <- log(S18$AlcB1)
S18$AlcMLog <- log(S18$AlcM1)
S18$AlcELog <- log(S18$AlcE1)

mbqt <-lm(S18CC$AlcBLog~S18CC$QANX_FSB)
plot(mbqt)
mmqt <-lm(S18CC$AlcMLog~S18CC$QANX_FSM)
plot(mmqt)
meqt <-lm(S18CC$AlcELog~S18CC$QANX_FSE)
plot(meqt)

mba <-lm(S18CC$AlcBLog~S18CC$ANX_FSB)
plot(mba)
mma <-lm(S18CC$AlcMLog~S18CC$ANX_FSM)
plot(mma)
mea <-lm(S18CC$AlcELog~S18CC$ANX_FSE)
plot(mea)

#honestly, it could be better.

describe(S18CC$AlcoholB)
describe(S18CC$AlcoholM)
describe(S18CC$AlcoholE)

qqnorm(S18CC$AlcoholB, main = "Q-Q Plot of Alcohol Use Last 30 Days (Beginning)")
qqnorm(S18CC$AlcoholM, main = "Q-Q Plot of Alcohol Use Last 30 Days (Middle)")
qqnorm(S18CC$AlcoholE, main = "Q-Q Plot of Alcohol Use Last 30 Days (End)")

qqnorm(S18longccAlc$alcohol, main = "Q-Q Plot of Alcohol Use Last 30 Days (All Time Points)")

qqnorm(S18CC$AlcBLog, main = "Q-Q Plot of Log of Alcohol Use Last 30 Days (Beginning)")
qqnorm(S18CC$AlcMLog, main = "Q-Q Plot of Log of Alcohol Use Last 30 Days (Middle)")
qqnorm(S18CC$AlcELog, main = "Q-Q Plot of Log of Alcohol Use Last 30 Days (End)")



#Doing LGCM with FIML imputation per committee suggestion

library(psych)

F17QAnxB <- c("QANX1B", "QANX2B", "QANX3B", "QANX4B")
F17QAnxB <-F17[F17QAnxB]

QANX_FAB <-fa(F17QAnxB, factors=1,rotation="promax",scores="regression")
QANX_FSB <- as.data.frame(QANX_FAB$scores)
F17 <- cbind(F17, QANX_FSB)
F17$QANX_FSB <- as.numeric(unlist(QANX_FSB))

F17QAnxM <- c("QANX1M", "QANX2M", "QANX3M", "QANX4M")
F17QAnxM <-F17[F17QAnxM]

QANX_FAM <-fa(F17QAnxM, factors=1,rotation="promax",scores="regression")
QANX_FSM <- as.data.frame(QANX_FAM$scores)
F17 <- cbind(F17, QANX_FSM)
F17$QANX_FSM <- as.numeric(unlist(QANX_FSM))

F17QAnxE <- c("QANX1E", "QANX2E", "QANX3E", "QANX4E")
F17QAnxE <-F17[F17QAnxE]

QANX_FAE <-fa(F17QAnxE, factors=1,rotation="promax",scores="regression")
QANX_FSE <- as.data.frame(QANX_FAE$scores)
F17 <- cbind(F17, QANX_FSE)
F17$QANX_FSE <- as.numeric(unlist(QANX_FSE))

S18QAnxB <- c("QANX1B", "QANX2B", "QANX3B", "QANX4B")
S18QAnxB <-S18[S18QAnxB]

QANX_FAB <-fa(S18QAnxB, factors=1,rotation="promax",scores="regression")
QANX_FSB <- as.data.frame(QANX_FAB$scores)
S18 <- cbind(S18, QANX_FSB)
S18$QANX_FSB <- as.numeric(unlist(QANX_FSB))

S18QAnxM <- c("QANX1M", "QANX2M", "QANX3M", "QANX4M")
S18QAnxM <-S18[S18QAnxM]

QANX_FAM <-fa(S18QAnxM, factors=1,rotation="promax",scores="regression")
QANX_FSM <- as.data.frame(QANX_FAM$scores)
S18 <- cbind(S18, QANX_FSM)
S18$QANX_FSM <- as.numeric(unlist(QANX_FSM))

S18QAnxE <- c("QANX1E", "QANX2E", "QANX3E", "QANX4E")
S18QAnxE <-S18[S18QAnxE]

QANX_FAE <-fa(S18QAnxE, factors=1,rotation="promax",scores="regression")
QANX_FSE <- as.data.frame(QANX_FAE$scores)
S18 <- cbind(S18, QANX_FSE)
S18$QANX_FSE <- as.numeric(unlist(QANX_FSE))

F17AnxB <- c("Anx1B", "Anx2B", "Anx3B", "Anx4B", "Anx5B", "Anx6B", "Anx7B")
F17AnxB <- F17[F17AnxB]

ANX_FAB <-fa(F17AnxB, factors=1,rotation="promax",scores="regression")
ANX_FSB <- as.data.frame(ANX_FAB$scores)
F17 <- cbind(F17, ANX_FSB)
F17$ANX_FSB <- as.numeric(unlist(ANX_FSB))

F17AnxM <- c("Anx1M", "Anx2M", "Anx3M", "Anx4M", "Anx5M", "Anx6M", "Anx7M")
F17AnxM <- F17[F17AnxM]

ANX_FAM <-fa(F17AnxM, factors=1,rotation="promax",scores="regression")
ANX_FSM <- as.data.frame(ANX_FAM$scores)
F17 <- cbind(F17, ANX_FSM)
F17$ANX_FSM <- as.numeric(unlist(ANX_FSM))

F17AnxE <- c("Anx1E", "Anx2E", "Anx3E", "Anx4E", "Anx5E", "Anx6E", "Anx7E")
F17AnxE <- F17[F17AnxE]

ANX_FAE <-fa(F17AnxE, factors=1,rotation="promax",scores="regression")
ANX_FSE <- as.data.frame(ANX_FAE$scores)
F17 <- cbind(F17, ANX_FSE)
F17$ANX_FSE <- as.numeric(unlist(ANX_FSE))

S18AnxB <- c("DASS15B", "DASS16B", "DASS17B", "DASS18B", "DASS19B", "DASS20B", "DASS21B")
S18AnxB <- S18[S18AnxB]

ANX_FAB <-fa(S18AnxB, factors=1,rotation="promax",scores="regression")
ANX_FSB <- as.data.frame(ANX_FAB$scores)
S18 <- cbind(S18, ANX_FSB)
S18$ANX_FSB <- as.numeric(unlist(ANX_FSB))

S18AnxM <- c("DASS15M", "DASS16M", "DASS17M", "DASS18M", "DASS19M", "DASS20M", "DASS21M")
S18AnxM <- S18[S18AnxM]

ANX_FAM <-fa(S18AnxM, factors=1,rotation="promax",scores="regression")
ANX_FSM <- as.data.frame(ANX_FAM$scores)
S18 <- cbind(S18, ANX_FSM)
S18$ANX_FSM <- as.numeric(unlist(ANX_FSM))

S18AnxE <- c("DASS15E", "DASS16E", "DASS17E", "DASS18E", "DASS19E", "DASS20E", "DASS21E")
S18AnxE <- S18[S18AnxE]

ANX_FAE <-fa(S18AnxE, factors=1,rotation="promax",scores="regression")
ANX_FSE <- as.data.frame(ANX_FAE$scores)
S18 <- cbind(S18, ANX_FSE)
S18$ANX_FSE <- as.numeric(unlist(ANX_FSE))


###################################
## CORRELATIONS AMONG PREDICTORS ##
###################################

cor.test(F17$ANX_FSB, F17$QANX_FSB)
cor.test(F17$ANX_FSM, F17$QANX_FSM)
cor.test(F17$ANX_FSE, F17$QANX_FSE)

cor.test(S18$ANX_FSB, S18$QANX_FSB)
cor.test(S18$ANX_FSM, S18$QANX_FSM)
cor.test(S18$ANX_FSE, S18$QANX_FSE)

############################################
## SUBSETTING S18 FOR PSYCH STUDENTS ONLY ##
############################################

S18.Items <- c("QuantClassB", "QuantClassM", "QuantClassE", 
               "ANX_FSB", "ANX_FSM", "ANX_FSE", "QANX_FSB", "QANX_FSM", "QANX_FSE",
               "AlcBLog", "AlcMLog", "AlcELog")

S18.Analysis <- S18[S18.Items]

S18.Analysis <- S18.Analysis %>% mutate(PSY200 =
                                          ifelse(QuantClassB == "2000001", 1, 
                                                 ifelse(QuantClassB == "2000002", 1,
                                                        ifelse(QuantClassB == "2000003", 1,
                                                               ifelse(QuantClassM == "2000001", 1, 
                                                                      ifelse(QuantClassM == "2000002", 1,
                                                                             ifelse(QuantClassM == "2000003", 1,
                                                                                    ifelse(QuantClassE == "2000001", 1, 
                                                                                           ifelse(QuantClassE == "2000002", 1,
                                                                                                  ifelse(QuantClassE == "2000003", 1,0))))))))))

S18.PSY200 <- S18.Analysis %>% filter(PSY200 == "1")

####################
## NON LINEAR LGM ##
#################### #Note: have to use fixed.x = FALSE to impute missing predictor values with FIML.


nonlin <- '           i =~ 1*AlcBLog + 1*AlcMLog + 1*AlcELog
                        s =~ 0*AlcBLog + NA*AlcMLog + 1*AlcELog
                           
                        AlcBLog ~ ANX_FSB + QANX_FSB 
                        AlcMLog ~ ANX_FSM + QANX_FSM 
                        AlcELog ~ ANX_FSE + QANX_FSE 
                      
                        AlcBLog~~resvar*AlcBLog
                        AlcMLog~~resvar*AlcMLog
                        AlcELog~~resvar*AlcELog'



ngl.f17 <- growth(nonlin, data=F17, control=list(iter.max=5000), missing="ML", fixed.x = FALSE)
summary(ngl.f17, fit.measures=TRUE)
parameterEstimates(ngl.f17, standardized=T)

#z-test for estimated slope parameter
(-0.16 - 0.5) / 0.12 #-5.50
pnorm(-5.5) # < .001

ngl.s18 <- growth(nonlin, data=S18, control=list(iter.max=5000), missing="ML", fixed.x = FALSE)
summary(ngl.s18, fit.measures=TRUE)
parameterEstimates(ngl.s18, standardized=T)

#z-test for estimated slope parameter
(-0.14 - 0.5) / 0.33 #-1.93
pnorm(-1.93) # =0.03


ngl.s18.psy200 <- growth(nonlin, data = S18.PSY200, control=list(iter.max=5000), missing="ML", fixed.x = FALSE)
summary(ngl.s18.psy200, fit.measures=TRUE)
parameterEstimates(ngl.s18.psy200, standardized=T)



nonlin.uncond <- '           i =~ 1*AlcBLog + 1*AlcMLog + 1*AlcELog
                             s =~ 0*AlcBLog + NA*AlcMLog + 1*AlcELog'

ngl.uncond.s18 <- growth(nonlin.uncond, data=S18, control=list(iter.max=5000), missing="ML", fixed.x = FALSE)
summary(ngl.uncond.s18, fit.measures=TRUE)
parameterEstimates(ngl.uncond.s18, standardized=T)
library(psych)
describe(F17$AlcBLog)
describe(F17$AlcMLog)
describe(F17$AlcELog)

describe(S18$AlcBLog)
describe(F17$AlcMLog)
describe(F17$AlcELog)

