## Models- Ordinal Logistic Regression
library(tidyverse)
library(MASS)
library(car)

#read in cleaned data
new_data <- readRDS("new_data.rds")
new_data_complete<- new_data[complete.cases(new_data),]

#primary analysis
model1<- polr(depressed_2006_categ ~ days_drank + age_in_2006 + sex_male + log_income_2006, 
              data= new_data_complete,
              Hess=TRUE)
model1_null<- polr(depressed_2006_categ ~ age_in_2006 + sex_male + log_income_2006,
                   data=new_data_complete,
                   Hess=TRUE)
summary(model1)
exp(coef(model1))
exp(confint(model1))
anova(model1, model1_null) # LRT
linearHypothesis(model1, c(1,0,0,0)) # Chi-square Test

#secondary analysis
model2<- polr(depressed_2006_categ ~ days_heavy_drank + age_in_2006 + sex_male + log_income_2006,
              data=new_data_complete,
              Hess=TRUE)
model2_null<- polr(depressed_2006_categ ~ age_in_2006 + sex_male + log_income_2006,
                   data=new_data_complete,
                   Hess=TRUE)
summary(model2)
exp(coef(model2))
exp(confint(model2))
anova(model2, model2_null) # LRT
linearHypothesis(model2, c(1,0,0,0)) # Chi-square Test

#-------------------------------
## Modeling Assumptions- Proportional Odds Assumption
library(brant)

#primary analysis
brant_test_1<- brant(model1) #brant test for model1, testing H0: proportional odds assumption holds
brant_test_1[1,]
model1_1<- glm(I(as.numeric(depressed_2006_categ) >= 2) ~ days_drank + age_in_2006 + sex_male + log_income_2006,
               data=new_data,
               family="binomial")
model1_2<- glm(I(as.numeric(depressed_2006_categ) >= 3) ~ days_drank + age_in_2006 + sex_male + log_income_2006,
               data=new_data,
               family="binomial")
model1_3<- glm(I(as.numeric(depressed_2006_categ) >= 4) ~ days_drank + age_in_2006 + sex_male + log_income_2006,
               data=new_data,
               family="binomial")

pred_model1_1<- function(x){model1_1$coefficients[1] + model1_1$coefficients[2]*x}
pred_model1_2<- function(x){model1_2$coefficients[1] + model1_2$coefficients[2]*x}
pred_model1_3<- function(x){model1_3$coefficients[1] + model1_3$coefficients[2]*x}

#plot Log-odds of depression against days_drank for log regression models at each of 3 cutpoints
ggplot(data=new_data, aes(x=days_drank)) +
  stat_function(fun=pred_model1_1, geom="line", aes(color="red")) +
  stat_function(fun=pred_model1_2, geom="line", aes(color="blue")) +
  stat_function(fun=pred_model1_3, geom="line", aes(color="green")) +
  labs(title="Proportional Odds Assumption- Primary",
       x="Days of Drinking Per Month",
       y="Predicted Log Odds of Depression") +
  scale_color_identity(name="Model Fit", 
                       breaks= c("red", "blue", "green"),
                       labels=c("At Least Some of the Time",
                                "At Least Most of the Time",
                                "At Least All of the Time"),
                       guide= "legend") +
  theme(legend.position = "right", legend.title= element_text(size=10),
        legend.text= element_text(size=6))
ggsave("Plots_Tables/Proportional Odds Assumption- Primary.png", plot=last_plot())
  


#secondary analysis
brant_test_2<- brant(model2) #brant test for model2, testing H0: proportional odds assumption holds
brant_test_2[1,]
model2_1<- glm(I(as.numeric(depressed_2006_categ) >= 2) ~ days_heavy_drank + age_in_2006 + sex_male + log_income_2006,
               data=new_data,
               family="binomial")
model2_2<- glm(I(as.numeric(depressed_2006_categ) >= 3) ~ days_heavy_drank + age_in_2006 + sex_male + log_income_2006,
               data=new_data,
               family="binomial")
model2_3<- glm(I(as.numeric(depressed_2006_categ) >= 4) ~ days_heavy_drank + age_in_2006 + sex_male + log_income_2006,
               data=new_data,
               family="binomial")

pred_model2_1<- function(x){model2_1$coefficients[1] + model2_1$coefficients[2]*x}
pred_model2_2<- function(x){model2_2$coefficients[1] + model2_2$coefficients[2]*x}
pred_model2_3<- function(x){model2_3$coefficients[1] + model2_3$coefficients[2]*x}

#plot Log-odds of depression against days_heavy_drank for log regression models at each of 3 cutpoints
ggplot(data=new_data, aes(x=days_heavy_drank)) +
  stat_function(fun=pred_model2_1, geom="line", aes(color="red")) +
  stat_function(fun=pred_model2_2, geom="line", aes(color="blue")) +
  stat_function(fun=pred_model2_3, geom="line", aes(color="green")) +
  labs(title="Proportional Odds Assumption- Secondary",
       x="Days of Heavy Drinking Per Month",
       y="Predicted Log Odds of Depression") +
  scale_color_identity(name="Model Fit", 
                       breaks= c("red", "blue", "green"),
                       labels=c(">= Some of the Time",
                                ">= Least Most of the Time",
                                ">= Least All of the Time"),
                       guide= "legend") +
  theme(legend.position = "right", legend.title= element_text(size=10),
        legend.text= element_text(size=6))
ggsave("Plots_Tables/Proportional Odds Assumption- Secondary.png", plot=last_plot())

#--------------------------
## Modeling Assumptions- No Multi-Collinearity
library(GGally)

#primary analysis
primary_corr_data<- new_data[,c("days_drank","age_in_2006", "sex_male", "log_income_2006")]
colnames(primary_corr_data)<- c("Days Drank", "Age", "Sex", "Log-Income")

ggpairs(data=new_data[,c("days_drank","age_in_2006", "sex_male", "log_income_2006")])
ggcorr(data=primary_corr_data, 
       name="Correlation",
       label=TRUE,label_round=3, label_size=3,
       nudge_x=-0.3, layout.exp=1) +
  ggtitle("Independent Variable Correlations- Primary")
ggsave("Plots_Tables/Multi-Collinearity Primary.png", plot=last_plot())

#secondary analysis
secondary_corr_data<- new_data[, c("days_heavy_drank", "age_in_2006", "sex_male", "log_income_2006")]
colnames(secondary_corr_data)<- c("Days Heavy", "Age", "Sex", "Log-Income")
ggcorr(data=secondary_corr_data,
       name="Correlation",
       label=TRUE, label_round=3, label_size=3,
       nudge_x=-0.3, layout.exp=1) +
  ggtitle("Independent Variable Corrs- Secondary")
ggsave("Plots_Tables/Multi-Collinearity Secondary.png", plot=last_plot())

#---------------------------
## Modeling Assumptions- Linearity (log-odds with continuous covariates)

# Primary Analysis- model1
prob_model1<- model1$fitted.values[,1]
odds_model1<- prob_model1/(1-prob_model1)
log_odds_model1<- log(odds_model1)
new_data_complete$log_odds_model1<- log_odds_model1 #predicted log-odds of depression < Some of Time

  #age
ggplot(data=new_data_complete, mapping=aes(x=jitter(age_in_2006), y=log_odds_model1))+
  geom_point() +
  geom_smooth() +
  labs(title="Primary Analysis- Linearity",
       x="Age in 2006", 
       y="Predicted Log-Odds (Depression < Some of the Time)")+
  theme_bw()
ggsave("Plots_Tables/Linearity Age Primary.png", plot=last_plot())

  #log-income
ggplot(data=new_data_complete, mapping=aes(x=log_income_2006, y=log_odds_model1))+
  geom_point() +
  geom_smooth() +
  labs(title="Primary Analysis- Linearity",
       x="Log Income in 2006", 
       y="Predicted Log-Odds (Depression < Some of the Time)")+
  theme_bw()
ggsave("Plots_Tables/Linearity Log Income Primary.png", plot=last_plot())

  #days_drank
ggplot(data=new_data_complete, mapping=aes(x=days_drank, y=log_odds_model1))+
  geom_point() +
  geom_smooth() +
  labs(title="Primary Analysis- Linearity",
       x="Days of Drinking Last Month", 
       y="Predicted Log-Odds (Depression < Some of the Time)")+
  theme_bw()
ggsave("Plots_Tables/Linearity Days Drank Primary.png", plot=last_plot())

# Secondary Analysis- model2
prob_model2<- model2$fitted.values[,1]
odds_model2<- prob_model2/(1-prob_model2)
log_odds_model2<- log(odds_model2)
new_data_complete$log_odds_model2<- log_odds_model2 #predicted log odds of depression < Some of Time

  #age
ggplot(data=new_data_complete, mapping=aes(x=jitter(age_in_2006), y=log_odds_model2)) +
  geom_point() +
  geom_smooth() +
  labs(title="Secondary Analysis- Linearity",
       x="Age in 2006", 
       y="Predicted Log-Odds (Depression < Some of the Time)")+
  theme_bw()
ggsave("Plots_Tables/Linearity Age Secondary.png", plot=last_plot())

  #log-income
ggplot(data=new_data_complete, mapping=aes(x=log_income_2006, y=log_odds_model2))+
  geom_point() +
  geom_smooth() +
  labs(title="Secondary Analysis- Linearity",
       x="Log Income in 2006", 
       y="Predicted Log-Odds (Depression < Some of the Time)")+
  theme_bw()
ggsave("Plots_Tables/Linearity Log Income Secondary.png", plot=last_plot())

  #days_heavy_drank
ggplot(data=new_data_complete, mapping=aes(x=days_heavy_drank, y=log_odds_model2)) + 
  geom_point() +
  geom_smooth() +
  labs(title="Secondary Analysis- Linearity",
       x="Days of Heavy Drinking Last Month", 
       y="Predicted Log-Odds (Depression < Some of the Time)")+
  theme_bw()
ggsave("Plots_Tables/Linearity Days Heavy Drank Secondary.png", plot=last_plot())
  
