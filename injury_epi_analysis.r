########## LOAD REQUIRED LIBS #########
library(effects, warn.conflicts = FALSE)
library(latex2exp, warn.conflicts = FALSE)
library(multcomp, warn.conflicts = FALSE)
library(pander, warn.conflicts = FALSE)
library(papeR, warn.conflicts = FALSE)
library(pubh, warn.conflicts = FALSE)

panderOptions("table.split.table", Inf)
set.alignment("right", row.names = "left", permanent = TRUE)
trellis.par.set(tactile.theme())

library(arulesCBA)
library(corrplot)
library(magrittr)
library(dplyr)
library(ggplot2)
library(gee)
library(naniar)

##### READ DATA #######
farmerdat<- read.csv("E:/w6_C_stat.csv", sep= "," , header = TRUE) #reading farmer level data

#K-means clustering by 4 groups 14,42.4) [42.4,58.1) [58.1,69.6)   [69.6,96] 
#table(discretize(farmerdat$Median, method = "cluster", breaks = 4))

#table(discretize(farmerdat$Median, method = "fixed", breaks = c(0,65, 79)))

#farmerdat$agec <- mutate((discretize(farmerdat$age,method = "fixed", breaks = c(14, 42, 58, 70, 96))))

##### MISSING DATA VIZ #####
gg_miss_upset(farmerdat, nsets = n_var_miss(farmerdat))

#############FORMATING################################
#converted all intigers to numeric
farmerdat[sapply(farmerdat, is.integer)]<-lapply(farmerdat[sapply(farmerdat, is.integer)],
                                                 as.numeric) 
#factorial/binary variables
fvar <- c("resp_exposures___1", "resp_exposures___2", "respiratory___1", "respiratory___2", 
          "respiratory___3","respiratory___4", "respiratory___5", 
          "respiratory___6", "respiratory___7", "sex",
          "operation", "percent_worktime", "operator_num", 
          "occupation", "resp_dis_num", "resp_dis_bin", "rhn_categ", "age_grp", "Livest_Y_N", 
          "resp_exposures___3", "resp_exposures___4", "resp_exposures___5", 
          "resp_exposures___6", "resp_exposures___7", "resp_exposures___8","resp_exp_other")
farmerdat[,fvar]<-lapply(farmerdat [,fvar], factor)

#data structure summary
str(farmerdat)

############################################################

#contingency table 2 by 2
tbl <- table(farmerdat$operator_num, farmerdat$percent_worktime)
tbl
mosaicplot(tbl, main = "operator by sex", color = T)
chisq.test(tbl) #chisquare test (2 groups)

#operator and age

#distribution box plot
boxplot(farmerdat$age ~ farmerdat$operator_num) 
#kruskal wallis test
kruskal.test(age~operator_num, data = farmerdat) 

#univariate analysis
tapply(farmerdat$age , farmerdat$operator_num, summary) 
aggregate(farmerdat$age~farmerdat$operator_num+farmerdat$operation+farmerdat$sex, 
          FUN = summary)

#age distribution
ggplot(data = farmerdat, mapping = aes(x = age, fill= sex, color = sex)) +
  geom_histogram(binwidth=1, alpha=0.2, position="identity") 

#density distribution
ggplot(farmerdat) + geom_density(aes(x = age, color = sex, alpha=0.5)) 

#cereal grains by state
boxplot(farmerdat$fm_Cereal_Grains ~ farmerdat$fm_state.1)
tapply(farmerdat$Median , farmerdat$fm_state.1, summary)

#Dichotamizing continous variable
farmerdat <- mutate(farmerdat, ppe_group = factor(1*(perct_resp >50), 
                                                  labels = c("good", "bad")))

#PPE use summary
tapply(farmerdat$ppe_group , farmerdat$operator_num, summary) 

#2 by 2 table
tbppe <- table(farmerdat$ppe_group , farmerdat$operator_num)
tbppe
mosaicplot(tbppe, main = "ppe and operator", color = T)
chisq.test(tbppe) #chisquare test (2 groups)

################## WORKING VARIABLES ###################
farmerdat$resp_exposures___2<- relevel(farmerdat$resp_exposures___2, ref = "0")
farmerdat$occupation<- relevel(farmerdat$occupation, ref = "2")

#GEE with Exchangable

#univariate
fit.gee2 <- gee(resp_dis_bin~resp_exposures___2, id=record_id, family=binomial,
                corstr="exchangeable", data=farmerdat) #crude

#multivariate model - adjusted analysis
fit.gee2 <- gee(resp_dis_bin~resp_exposures___2+percent_worktime+occupation+sex+age_grp+operator_num+rhn_categ+operation+
                  resp_exposures___3+resp_exposures___4+
                  resp_exposures___5+resp_exposures___6+resp_exposures___7, id=record_id, family=binomial,
                corstr="exchangeable", data=farmerdat) 

summary(fit.gee2)
glm_coef(fit.gee2)
pander(glm_coef(fit.gee2 ))

################## MODEL - Final result ###################
#GEE with Exchangable
fit.gee2 <- gee(resp_dis_bin~resp_exposures___2+age_grp+sex+resp_exposures___3+resp_exposures___4
                +resp_exposures___5+resp_exposures___6+resp_exposures___7+resp_exposures___8
                , id=record_id, 
                family=binomial,corstr="exchangeable", data=farmerdat)
summary(fit.gee2)
glm_coef(fit.gee2)
pander(glm_coef(fit.gee2 ))
