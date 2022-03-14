## Script name: models_and_modelling_assumptions_limitations.R
##
## Purpose of script: Run ordinal logistic regression analysis and check modelling assumptions
##
## Author: Taylor Keating
##
## Email: tkeatin@uw.edu
##
## Note: all created plots and tables are saved in Plot_Tables subdirectory

## Models- Ordinal Logistic Regression
library(tidyverse)
library(MASS)
library(car)
library(knitr)
library(kableExtra)
setwd("~/Documents/GitHub/young_adult_alcohol_use")
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
model1_no_age<- polr(depressed_2006_categ ~ days_drank + sex_male + log_income_2006,
                     data=new_data_complete,
                     Hess=TRUE)
model1_no_sex<- polr(depressed_2006_categ ~ days_drank + age_in_2006 + log_income_2006,
                     data=new_data_complete,
                     Hess=TRUE)
model1_no_income<- polr(depressed_2006_categ ~ days_drank + age_in_2006 + sex_male,
                        data=new_data_complete,
                        Hess=TRUE)
  #Estimates- primary analysis
estimates_model1_table<- coef(summary(model1))
pvals_model1<- round(pnorm(abs(estimates_model1_table[,"t value"]), lower.tail=FALSE)*2 , digits=4)
estimates_model1_table<- cbind(estimates_model1_table, "p value"= pvals_model1)
  #Exp Estimates- primary analysis
exp_estimates_model1<- round(exp(estimates_model1_table[,1]), digits=3)
exp_ci_low_model1<- round(exp(estimates_model1_table[,1] + qnorm(0.025)*estimates_model1_table[,2]), digits=3)
exp_ci_high_model1<- round(exp(estimates_model1_table[,1] + qnorm(0.975)*estimates_model1_table[,2]), digits=3)
exp_estimates_model1_table<- cbind("Odds Estimate"= exp_estimates_model1, 
                                   "95% Low"= exp_ci_low_model1, 
                                   "95% High"= exp_ci_high_model1,
                                   "p-value"= pvals_model1)
  #Odds Scale Estimates table- primary analysis
kable(exp_estimates_model1_table, caption="Odds Scale Estimates- Primary Analysis") %>%
  kable_styling(bootstrap_options= c("striped", "bordered", "condensed"), full_width = FALSE) %>%
  pack_rows(index=c("Odds Ratios" = 4, "Intercepts"= 3)) %>%
  save_kable("Plots_Tables/Odds Scale Estimates- Primary Analysis.pdf")
  #OR Estimates- primary analysis with LRT p-vals
exp_or_estimates_model1<- exp(coef(model1))
exp_or_ci_model1<- exp(confint(model1))
or_p_vals_model1<- c(anova(model1, model1_null)[2,7],
                     anova(model1, model1_no_age)[2,7],
                     anova(model1, model1_no_sex)[2,7],
                     anova(model1, model1_no_income)[2,7]) #LRT



#secondary analysis
model2<- polr(depressed_2006_categ ~ days_heavy_drank + age_in_2006 + sex_male + log_income_2006,
              data=new_data_complete,
              Hess=TRUE)
model2_null<- polr(depressed_2006_categ ~ age_in_2006 + sex_male + log_income_2006,
                   data=new_data_complete,
                   Hess=TRUE)
model2_no_age<- polr(depressed_2006_categ ~ days_heavy_drank + sex_male + log_income_2006,
                   data=new_data_complete,
                   Hess=TRUE)
model2_no_sex<- polr(depressed_2006_categ ~ days_heavy_drank + age_in_2006 + log_income_2006,
                   data=new_data_complete,
                   Hess=TRUE)
model2_no_income<- polr(depressed_2006_categ ~ days_heavy_drank + age_in_2006 + sex_male,
                   data=new_data_complete,
                   Hess=TRUE)
  #Estimates- secondary analysis
estimates_model2_table<- coef(summary(model2))
pvals_model2<- round(pnorm(abs(estimates_model2_table[,"t value"]), lower.tail=FALSE)*2 , digits=4)
estimates_model2_table<- cbind(estimates_model2_table, "p value"= pvals_model2)
  #Exp Estimates- secondary analysis
exp_estimates_model2<- round(exp(estimates_model2_table[,1]), digits=3)
exp_ci_low_model2<- round(exp(estimates_model2_table[,1] + qnorm(0.025)*estimates_model2_table[,2]), digits=3)
exp_ci_high_model2<- round(exp(estimates_model2_table[,1] + qnorm(0.975)*estimates_model2_table[,2]), digits=3)
exp_estimates_model2_table<- cbind("Odds Estimate"= exp_estimates_model2, 
                                   "95% Low"= exp_ci_low_model2, 
                                   "95% High"= exp_ci_high_model2,
                                   "p-value"= pvals_model2)
  #Odds Scale Estimates Table- secondary analysis
kable(exp_estimates_model2_table, caption="Odds Scale Estimates- Secondary Analysis") %>%
  kable_styling(bootstrap_options= c("striped", "bordered", "condensed"), full_width = FALSE) %>%
  pack_rows(index=c("Odds Ratios" = 4, "Intercepts"= 3)) %>%
  save_kable("Plots_Tables/Odds Scale Estimates- Secondary Analysis.pdf")
  #OR Estimates- secondary analysis with LRT p-vals
exp_estimates_model2<- exp(coef(model2))
exp_ci_model2<- exp(confint(model2))
p_vals_model2<- c(anova(model2, model2_null)[2,7],
                  anova(model2, model2_no_age)[2,7],
                  anova(model2, model2_no_sex)[2,7],
                  anova(model2, model2_no_income)[2,7]) #LRT

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
prop_odds_plot_prim<-
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
ggsave("Plots_Tables/Proportional Odds Assumption- Primary.png", plot=prop_odds_plot_prim)
  


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
prop_odds_plot_secon<- 
  ggplot(data=new_data, aes(x=days_heavy_drank)) +
    stat_function(fun=pred_model2_1, geom="line", aes(color="red")) +
    stat_function(fun=pred_model2_2, geom="line", aes(color="blue")) +
    stat_function(fun=pred_model2_3, geom="line", aes(color="green")) +
    labs(title="Proportional Odds Assumption- Secondary",
        x="Days of Heavy Drinking Per Month",
        y="Predicted Log Odds of Depression") +
    scale_color_identity(name="Model Fit", 
                        breaks= c("red", "blue", "green"),
                        labels=c("At Least Some of the Time",
                                  "At Least Most of the Time",
                                  "At Least All of the Time"),
                        guide= "legend") +
    theme(legend.position = "right", legend.title= element_text(size=10),
          legend.text= element_text(size=6))
ggsave("Plots_Tables/Proportional Odds Assumption- Secondary.png", plot=prop_odds_plot_secon)

#--------------------------
## Modeling Assumptions- No Multi-Collinearity

#plotting correlations
library(GGally)
  #primary analysis
primary_corr_data<- new_data[,c("days_drank","age_in_2006", "sex_male", "log_income_2006")]
colnames(primary_corr_data)<- c("Days Drank", "Age", "Sex", "Log-Income")
multi_colin_prim<-
  ggcorr(data=primary_corr_data, 
        name="Correlation",
        label=TRUE,label_round=3, label_size=3,
        nudge_x=-0.3, layout.exp=1) +
    ggtitle("Independent Variable Correlations- Primary")
ggsave("Plots_Tables/Multi-Collinearity Primary.png", plot=multi_colin_prim)

  #secondary analysis
secondary_corr_data<- new_data[, c("days_heavy_drank", "age_in_2006", "sex_male", "log_income_2006")]
colnames(secondary_corr_data)<- c("Days Heavy", "Age", "Sex", "Log-Income")
multi_colin_secon<-
  ggcorr(data=secondary_corr_data,
        name="Correlation",
        label=TRUE, label_round=3, label_size=3,
        nudge_x=-0.3, layout.exp=1) +
    ggtitle("Independent Variable Correlations- Secondary")
ggsave("Plots_Tables/Multi-Collinearity Secondary.png", plot=multi_colin_secon)

#VIF
  #primary analysis
model1_vif<- lm(depressed_2006 ~ days_drank + age_in_2006 + sex_male + log_income_2006,
                data=new_data_complete)
vif_model1<- as.data.frame(round(vif(model1_vif), digits=4))
colnames(vif_model1)<- c("VIF")
kable(vif_model1, caption="VIF- Primary Analysis") %>% 
  kable_styling(bootstrap_options = c("striped", "bordered", "condensed"), full_width = FALSE) %>%
  save_kable("Plots_Tables/VIF_Primary.pdf")
  
#secondary analysis
model2_vif<- lm(depressed_2006 ~ days_heavy_drank + age_in_2006 + sex_male + log_income_2006,
                data=new_data_complete)
vif_model2<- as.data.frame(round(vif(model2_vif), digits=4))
colnames(vif_model2)<- c("VIF")
kable(vif_model2, caption="VIF- Secondary Analysis") %>% 
  kable_styling(bootstrap_options = c("striped", "bordered", "condensed"), full_width = FALSE) %>%
  save_kable("Plots_Tables/VIF_Secondary.pdf")

#---------------------------
## Modeling Assumptions- Linearity (log-odds with continuous covariates)

# Primary Analysis- model1
prob_model1<- model1$fitted.values
log_odds_model1<- log(prob_model1/(1-prob_model1)) #predicted log-odds
predictors_model1<- c("age_in_2006","log_income_2006", "days_drank")
pred_log_odds_model1_data<- new_data_complete %>% 
  dplyr::select(predictors_model1) %>%
  mutate(None= log_odds_model1[,1], #none of time
         Some= log_odds_model1[,2], #some of time
         Most= log_odds_model1[,3], #most of time
         All= log_odds_model1[,4]) #all of time
pred_log_odds_model1_data<- pred_log_odds_model1_data %>%
  pivot_longer(c(None, Some, Most, All),
               names_to= "depression_category",
               values_to= "predicted_log_odds")

  #age
age_linearity_prim<- 
  ggplot(data=pred_log_odds_model1_data, 
         mapping=aes(x=jitter(age_in_2006), y=predicted_log_odds)) +
    geom_point(size=0.1) +
    geom_smooth() +
    facet_wrap(~depression_category, scales="free_y") +
    labs(title="Linearity with Age- Primary",
        x="Age in 2006",
        y="Predicted Log Odds of Depression") +
    theme_bw()
ggsave("Plots_Tables/Linearity Age Primary.png", plot=age_linearity_prim)
  #log-income
log_income_linearity_prim<-
  ggplot(data=pred_log_odds_model1_data,
         mapping=aes(x=log_income_2006, y=predicted_log_odds)) +
    geom_point(size=0.1) +
    geom_smooth() +
    facet_wrap(~depression_category, scales="free_y") +
    labs(title="Linearity Log Income- Primary",
        x="Log Income in 2006",
        y="Predicted Log Odds of Depression") +
    theme_bw()
ggsave("Plots_Tables/Linearity Log Income Primary.png", plot=log_income_linearity_prim)
  #days_drank
days_drank_linearity_prim<-
  ggplot(data=pred_log_odds_model1_data,
         mapping=aes(x=days_drank, y=predicted_log_odds)) +
    geom_point(size=0.1) +
    geom_smooth() +
    facet_wrap(~depression_category, scales="free_y") +
    labs(title="Linearity Days Drank- Primary",
        x="Days of Drinking Last Month",
        y="Predicted Log Odds of Depression") +
    theme_bw()
ggsave("Plots_Tables/Linearity Days Drank Primary.png", plot=days_drank_linearity_prim)


# Secondary Analysis- model2
prob_model2<- model2$fitted.values
log_odds_model2<- log(prob_model2/(1-prob_model2)) #predicted log-odds
predictors_model2<- c("age_in_2006","log_income_2006", "days_heavy_drank")
pred_log_odds_model2_data<- new_data_complete %>% 
  dplyr::select(predictors_model2) %>%
  mutate(None= log_odds_model2[,1], #none of time
         Some= log_odds_model2[,2], #some of time
         Most= log_odds_model2[,3], #most of time
         All= log_odds_model2[,4]) #all of time
pred_log_odds_model2_data<- pred_log_odds_model2_data %>%
  pivot_longer(c(None, Some, Most, All),
               names_to= "depression_category",
               values_to= "predicted_log_odds")

  #age
age_linearity_secon<- 
  ggplot(data=pred_log_odds_model2_data, 
         mapping=aes(x=jitter(age_in_2006), y=predicted_log_odds)) +
  geom_point(size=0.1) +
  geom_smooth() +
  facet_wrap(~depression_category, scales="free_y") +
  labs(title="Linearity with Age- Secondary",
       x="Age in 2006",
       y="Predicted Log Odds of Depression") +
  theme_bw()
ggsave("Plots_Tables/Linearity Age Secondary.png", plot=age_linearity_secon)
  #log-income
log_income_linearity_secon<-
  ggplot(data=pred_log_odds_model2_data,
         mapping=aes(x=log_income_2006, y=predicted_log_odds)) +
  geom_point(size=0.1) +
  geom_smooth() +
  facet_wrap(~depression_category, scales="free_y") +
  labs(title="Linearity Log Income- Second",
       x="Log Income in 2006",
       y="Predicted Log Odds of Depression") +
  theme_bw()
ggsave("Plots_Tables/Linearity Log Income Secondary.png", plot=log_income_linearity_secon)
  #days_heavy_drank
days_heavy_drank_linearity_secon<-
  ggplot(data=pred_log_odds_model2_data,
         mapping=aes(x=days_heavy_drank, y=predicted_log_odds)) +
  geom_point(size=0.1) +
  geom_smooth() +
  facet_wrap(~depression_category, scales="free_y") +
  labs(title="Linearity Days Heavy Drank",
       x="Days of Heavy Drinking Last Month",
       y="Predicted Log Odds of Depression") +
  theme_bw()
ggsave("Plots_Tables/Linearity Days Heavy Drank Secondary.png", plot=days_heavy_drank_linearity_secon)