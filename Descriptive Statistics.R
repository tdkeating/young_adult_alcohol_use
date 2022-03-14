## Script name: Descriptive Statistics.R
##
## Purpose of script: Get descriptive statistics from analysis 
##            - summary of predictor variables
##            - mean and std dev of days_drank and days_heavy_drank for each depression category
##            - distributions of subject's answers to days_drank and days_heavy_drank
##            - distributions of subject's answers to income and log(income + 1)
##
## Author: Taylor Keating
##
## Email: tkeatin@uw.edu
##
## Note: all created plots and tables are saved in Plot_Tables subdirectory

library(tidyverse)
library(knitr)
library(kableExtra)
setwd("~/Documents/GitHub/young_adult_alcohol_use")
#read in cleaned data
new_data <- readRDS("new_data.rds")

#summary of predictor variables
cov_table<- new_data %>% summarise(across(c(4,6,7,12,13), function(x){list(n=sum(!is.na(x)), 
                                                                           missing=sum(is.na(x)),
                                                                           mean=round(mean(x, na.rm=TRUE),2),
                                                                           sd=round(sd(x, na.rm=TRUE),2),
                                                                           min=min(x, na.rm=TRUE),
                                                                           median=median(x, na.rm=TRUE),
                                                                           max=max(x, na.rm=TRUE))}
                                          )
                                   )
cov_table<- as.data.frame(cov_table)
colnames(cov_table)<- c("Age", "Male Sex", "Income", "Days Drinking", "Days Heavy Drinking")
rownames(cov_table)<- c("Number of Observations", "Number Missing", "Mean", "Std Dev", "Min", "Median", "Max")
cov_table[3,3]<- round(as.numeric(cov_table[3,3]), 0)
cov_table[4,3]<- round(as.numeric(cov_table[4,3]), 0)
kable(cov_table, caption= "Table 1- Summary of Covariates") %>% 
  kable_styling(bootstrap_options= c("striped", "bordered"), full_width = FALSE) %>%
  add_header_above(c(" "= 1, "Variable"= 5)) %>%
  save_kable("Plots_Tables/Table1-Covariates_Summary.pdf")

#mean and std dev of days_drank and days_heavy_drank for each depression category
outcome_summary_table<- new_data %>% group_by(depressed_2006_categ) %>% summarise(n=n(), 
                                                                                  mean_days_drank=mean(days_drank, na.rm=TRUE),
                                                                                  sd_days_drank=sd(days_drank, na.rm=TRUE),
                                                                                  mean_days_heavy_drank=mean(days_heavy_drank, na.rm=TRUE),
                                                                                  sd_days_heavy_drank=sd(days_heavy_drank, na.rm=TRUE))
additional_outcome_summary<- new_data %>% group_by(is.na(depressed_2006_categ)) %>% summarise(n=n(), 
                                                                                              mean_days_drank=mean(days_drank, na.rm=TRUE),
                                                                                              sd_days_drank=sd(days_drank, na.rm=TRUE),
                                                                                              mean_days_heavy_drank=mean(days_heavy_drank, na.rm=TRUE),
                                                                                              sd_days_heavy_drank=sd(days_heavy_drank, na.rm=TRUE))
additional_outcome_summary<- additional_outcome_summary %>% rename(depressed_2006_categ= "is.na(depressed_2006_categ)")
additional_outcome_summary[,1]<- c("Total", "NA")
outcome_summary_table<- rbind(outcome_summary_table, additional_outcome_summary)
outcome_summary_table<- outcome_summary_table[-5, ]
outcome_summary_table[,3:6]<- round(outcome_summary_table[,3:6], digits=2)
kable(outcome_summary_table, col.names = c("Depression", "Number Obs", "Mean Days Drinking per Month", 
                                           "StdDev Days Drinking per Month", "Mean Days Heavy Drinking per Month", 
                                           "StdDev Days Heavy Drinking per Month"),
      caption= "Table 2- Summaries of Drinking for Each Depression Category") %>% 
  kable_styling(bootstrap_options= c("striped", "bordered", "condensed"), full_width=FALSE) %>%
  save_kable("Plots_Tables/Table2-OutcomeSummaries.pdf")



#distributions of subject's answers to days_drank and days_heavy_drank
dist_days_drank<- 
  ggplot(data=new_data, aes(x=days_drank)) +
    geom_histogram(binwidth=1) +
    labs(title="Distribution of Days of Drinking",
        x="Days of Drinking Per Month",
        y="Count") +
    theme_bw()
ggsave("Plots_Tables/Distribution of Days Drinking.png", plot=dist_days_drank)
dist_days_heavy_drank<-
  ggplot(data=new_data, aes(x=days_heavy_drank)) +
    geom_histogram(binwidth=1) +
    labs(title="Distribution of Days of Heavy Drinking",
        x="Days of Heavy Drinking Per Month",
        y="Count") +
    theme_bw()
ggsave("Plots_Tables/Distribution of Days Heavy Drinking.png", plot=dist_days_heavy_drank)


#distributions of subject's answers to income and log(income + 1)
dist_income<-
  ggplot(data=new_data, mapping=aes(income_2006)) + 
    geom_histogram() +
    theme_bw() + 
    labs(title="Distribution of Incomes", x="Income in 2006 (USD)", y="Count")
ggsave("Plots_Tables/Distribution of Incomes.png", plot=dist_income)
dist_log_income<-
  ggplot(data=new_data, mapping=aes(log_income_2006)) +
    geom_histogram() +
    theme_bw() +
    labs(title="Distribution of Log-Incomes", x="Log(Income + 1)", y="Count")
ggsave("Plots_Tables/Distribution of Log-Incomes.png", plot=dist_log_income)