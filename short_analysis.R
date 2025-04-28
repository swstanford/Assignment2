#load data
df_cohort<- read.csv('/Users/simeng/Library/Mobile Documents/com~apple~CloudDocs/Documents/Grad School/HRP 203/Wk4/data/cohort.csv')

#Run install.package("tidyverse") 
library(tidyverse) 

#Create table describing the demographic of those who did vs did not have a cardiac event 
df_cohort %>% 
    group_by(cardiac) %>% 
    summarize (count = n(), 
               percent_smoking = mean(smoke)*100,
               percent_female = mean(female)*100,
               average_age = mean(age),
               average_cost = mean(cost)
    )

#Multivariable regression of cardiac event against smoking status, female sex, age, and cost 
cardiac_glm <- glm(cardiac ~ smoke + female + age + cost, data = df_cohort, family="binomial")
summary (cardiac_glm)

#boxplot of cost for those who did vs did not have a cardiac event 
df_cohort_plot = 
    df_cohort %>% 
    mutate(cardiac_group = if_else(cardiac == 1, true ="With Cardiac Event", false="Without Cardiac Event")) 
        
        ggplot(df_cohort_plot, aes(x = cardiac_group, y = cost)) + 
    geom_boxplot() + 
    xlab("Cardiac Event") + 
    ylab("Cost") +
    ggtitle("Comparing Cost between Patients with vs without Cardiac Event")
    
