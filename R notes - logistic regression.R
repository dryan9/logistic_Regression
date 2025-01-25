#Logistic regression code from notes

library(AmesHousing)
library(tidyverse)
library(car)
library(mgcv)

#creating binary variable bonus (if sale price > 175000)
ames <- ames %>%  mutate(Bonus = ifelse(Sale_Price > 175000, 1,0))

#logistic regression model
logit.model <- glm(Bonus ~ Gr_Liv_Area + factor(Central_Air), data = ames, family = binomial(link = "logit"))

summary(logit.model)

#odds ratio and confidence interval from logistic regression model
exp(cbind(coef(logit.model), confint(logit.model)))

#Likelihood Ratio Test (LRT)

  #making an intercept only model
  logit.model.r <- glm(Bonus ~ 1, data = ames, family = binomial(link = "logit"))

#anova between full and reduced model  --> this can compare any two models as long as the reduced model is nested within the full model
anova(logit.model, logit.model.r, test = "LRT")
  #Ho: both models are the same --> a high p-value means drop the extra variables
  #Ha: extra variables make the model better  --> low p-value means keep the extra variables 

#Car::Anova for comparing full model vs. full model without one of the variables 
    #look at p-values for individual variables to decide if keep or drop
    #Ho: model is same with and without that variable --> high p-value means you can drop that variable
    #Ha: model is better with that variable --> low p-value means keep that variable
logit.model.f <- glm(Bonus ~ Gr_Liv_Area + factor(Central_Air) + factor(Fireplaces), 
                      data = ames, family = binomial(link = "logit"))

car::Anova(logit.model.f, test = "LR", type = "III")

#Checking logistic regression assumptions for continuous variables 
    #generally do this one continuous variable at a time, and binary variables do not matter and are OK to be in the model
fit.gam <- gam(Bonus ~ s(Gr_Liv_Area) + factor(Central_Air),
               data = ames, family = binomial(link = "logit"), method = "REML")
#in summary we want to see edf ~= 1 for continuous variables
    summary(fit.gam)
#in the plot we want dark middle line to be straight
    plot(fit.gam)
    
#Checking to see if the Spline adds value (aka is the line straight?)
      #ftft
      #low p-value means spline and straight line are not the same, which means that variable fails the assumption
anova(logit.model , fit.gam, test = "LRT")

#binning continuous variables
ames <- ames %>%
  mutate(Gr_Liv_Area_BIN = cut(Gr_Liv_Area, 
                               breaks = c(-Inf,1000,1500,3000,4500,Inf)))
logit.model.bin <- glm(Bonus ~ factor(Gr_Liv_Area_BIN) + factor(Central_Air), 
                       data = ames, family = binomial(link = 'logit')) 
summary(logit.model.bin)

#predicted values 
new_ames <- data.frame(Gr_Liv_Area = c(1500, 2000, 2250, 2500, 3500), 
                       Central_Air = c("N", "Y", "Y", "N", "Y")) 
new_ames <- data.frame(new_ames, 
                       'Pred' = predict(logit.model, newdata = new_ames, 
                                        type = "response"))
print(new_ames)

#Rare event sampling correction
churn <- read.csv("C:\\Users\\dryan\\OneDrive\\Documents\\NC_state\\Fall\\Logistic Regression\\tele_churn.csv")
#adding ID so that we can split data into training and testing data
churn$id <- 1:length(churn$churn)

#oversampling

set.seed(12345) 

#using 70% of data in training set
train_o <- churn %>%
  sample_frac(0.70) 

#isolating rare event
train_o_T <- train_o %>%
  filter(churn == TRUE) %>%
  slice(rep(1:n(), each = 10)) 

#isolating churn == false
train_o_F <- train_o %>%
  filter(churn == FALSE) 

train_o <- rbind(train_o_F, train_o_T) 
test_o <- churn[-train_o$id,]

#undersampling
table(churn$churn)

train_u <-churn %>% 
          group_by(churn) %>% 
          sample_n(104)

test_u <- churn[-train_u$id,]
str(train_u)
table(train_u$churn)
table(test_u$churn)

#making a model (that will be a biased model to start)
logit.model <- glm(churn ~ factor(international.plan) +
                     factor(voice.mail.plan) +
                     total.day.charge +
                     customer.service.calls,
                   data = train_u, family = binomial(link = "logit"))
summary(logit.model)

#adjusting the intercept
 #see notes on how to make the below calculations (most likely will not need this)
test_u_p_bias <- predict(logit.model, newdata = test_u, type = "response")
test_u_p <- (test_u_p_bias*(104/208)*(154/3004))/((1-test_u_p_bias)*(104/208)*(2850/3004)+test_u_p_bias*(104/208)*(154/3004))

test_u <- data.frame(test_u, 'Pred' = test_u_p)
head(test_u_p)
#weighting observations(when 1 is the rare event)
#keep the 1's= 1, and then add weight to the 0's, since their effect was reduced in the sampling
#((sample 1/sample size )*(popuation 0/population size))/((sample 0/sample size)*(population 1/population size))
weight_calc = (104*2850)/(104*154)
#will use 18.51 to align with the notes

#adding the weight varaible to the training data set, and specifically either making the weight = 1 for churn = 1, and weight = 18.51 for churn = 0
train_u$weight <- ifelse(train_u$churn == 'TRUE', 1, 18.51)

#adding weight to the logistic regression (now the model is no longer biased)
logit.model.w <- glm(churn ~ factor(international.plan) +
                       factor(voice.mail.plan) +
                       total.day.charge +
                       customer.service.calls, 
                     data = train_u, family = binomial(link = "logit"), 
                     weights = weight) 
summary(logit.model.w)

#combining categories
table(train_u$customer.service.calls, train_u$churn)

#combining categories
#first creating the new column
train_u$customer.service.calls.c <- as.character(train_u$customer.service.calls) 
#and then assigning all service calls over 3 to the 4+ column
train_u$customer.service.calls.c[which(train_u$customer.service.calls > 3)] <- "4+"
table(train_u$customer.service.calls.c, train_u$churn)