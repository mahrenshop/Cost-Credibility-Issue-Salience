# Purpose: Load and analyze UK conjoint data
# Programmed by: Mats Ahrenshop
# Date: 03 Sep 2019

setwd("C:\\Users\\User\\Documents\\Studium\\Oxf\\THESIS\\Substantial\\Observational Analysis\\Data")
rm(list = ls())
library(cjoint)
library(tidyverse)
library(reshape2)
library(multiwayvcov)
library(cregg)
library(ggpubr)
library(stargazer)
library(devtools)
library(qualtRics)
library(here)
library(miceadds)

#### Get data in shape ####
uk <- read_survey("uk.csv")
uk$id <- 1:nrow(uk)
demog_vars <- c("id", "Q214", "Q215_1", "Q215_2", "Q215_3", "Q215_4", "Q215_5", "Q215_6",
                "Q216", "Q112", "Q113", "Q114", "Q115", "Q212_1", "Q116", "Q210", "Q211")  

#### Isolate demographic variables ####
demog <- uk %>%
  select(demog_vars)

#### Candidate conjoint ####

colnames(uk)[which(names(uk) == "Q223")] <- "choice1"
colnames(uk)[which(names(uk) == "Q225")] <- "choice2"
colnames(uk)[which(names(uk) == "Q227")] <- "choice3"
colnames(uk)[which(names(uk) == "Q229")] <- "choice4"
colnames(uk)[which(names(uk) == "Q231")] <- "choice5"
colnames(uk)[which(names(uk) == "Q233")] <- "choice6"


# Function to rearrange dataframe into analysis friendly format
cand_sort <-function(raw_df, group = "control") {
  
  if (!(group %in% c("control","treat"))) {print("ERROR: UNKOWN GROUP"); break}
  
  if (group == "control") {
    
    key <- c("Policy Issue", 
             "Policy Cost", 
             "Policy Effectiveness", 
             "Candidate Credibility", 
             "Candidate Gender",
             "Candidate Party")
    
  } else {
    
    key <- c("Average Donation", 
             "Total Donations", 
             "Largest Donor", 
             "Proportion of Campaign Funds from Largest Donor", 
             "Origin of Donations",
             "Party",
             "Ideology",
             "Previously held elected office?")
  }
  
  df_a <- matrix(ncol = length(key)+5, nrow = nrow(raw_df)*6)
  df_b <- matrix(ncol = length(key)+5, nrow = nrow(raw_df)*6)
  
  for (round in 1:6) {
    
    ref <- ifelse(group == "control", paste0("F-",round))
    print(ref)
    rows <- ((round-1)*nrow(raw_df)+1):(round*nrow(raw_df))
    
    for (i in 1:length(key)) {
      df_a[rows,i] <- ifelse(raw_df[[paste0(ref,'-1')]] == key[i],raw_df[[paste0(ref,'-1-1')]],df_a[rows,i])
      df_a[rows,i] <- ifelse(raw_df[[paste0(ref,'-2')]] == key[i],raw_df[[paste0(ref,'-1-2')]],df_a[rows,i])
      df_a[rows,i] <- ifelse(raw_df[[paste0(ref,'-3')]] == key[i],raw_df[[paste0(ref,'-1-3')]],df_a[rows,i])
      df_a[rows,i] <- ifelse(raw_df[[paste0(ref,'-4')]] == key[i],raw_df[[paste0(ref,'-1-4')]],df_a[rows,i])
      df_a[rows,i] <- ifelse(raw_df[[paste0(ref,'-5')]] == key[i],raw_df[[paste0(ref,'-1-5')]],df_a[rows,i])
      df_a[rows,i] <- ifelse(raw_df[[paste0(ref,'-6')]] == key[i],raw_df[[paste0(ref,'-1-6')]],df_a[rows,i])
      
      if (length(key) > 6) {
        df_a[rows,i] <- ifelse(raw_df[[paste0(ref,'-7')]] == key[i],raw_df[[paste0(ref,'-1-7')]],df_a[rows,i])
        df_a[rows,i] <- ifelse(raw_df[[paste0(ref,'-8')]] == key[i],raw_df[[paste0(ref,'-1-8')]],df_a[rows,i])
      }
      
    }
    
    for (i in 1:length(key)) {
      df_b[rows,i] <- ifelse(raw_df[[paste0(ref,'-1')]] == key[i],raw_df[[paste0(ref,'-2-1')]],df_b[rows,i])
      df_b[rows,i] <- ifelse(raw_df[[paste0(ref,'-2')]] == key[i],raw_df[[paste0(ref,'-2-2')]],df_b[rows,i])
      df_b[rows,i] <- ifelse(raw_df[[paste0(ref,'-3')]] == key[i],raw_df[[paste0(ref,'-2-3')]],df_b[rows,i])
      df_b[rows,i] <- ifelse(raw_df[[paste0(ref,'-4')]] == key[i],raw_df[[paste0(ref,'-2-4')]],df_b[rows,i])
      df_b[rows,i] <- ifelse(raw_df[[paste0(ref,'-5')]] == key[i],raw_df[[paste0(ref,'-2-5')]],df_b[rows,i])
      df_b[rows,i] <- ifelse(raw_df[[paste0(ref,'-6')]] == key[i],raw_df[[paste0(ref,'-2-6')]],df_b[rows,i])
      
      if (length(key) > 6) {
        df_b[rows,i] <- ifelse(raw_df[[paste0(ref,'-7')]] == key[i],raw_df[[paste0(ref,'-2-7')]],df_b[rows,i])
        df_b[rows,i] <- ifelse(raw_df[[paste0(ref,'-8')]] == key[i],raw_df[[paste0(ref,'-2-8')]],df_b[rows,i])
      }
    }
    
    # Candidate profile
    df_a[rows,length(key)+1] <- "1"
    df_b[rows,length(key)+1] <- "2"
    
    # Vote choice by subject
    choice_col <- ifelse(group == "control", paste0("choice",round), paste0(round,"_cand_t_choice"))
    
    df_a[rows,length(key)+2] <- ifelse(raw_df[,grep(choice_col,names(raw_df))] == "Candidate 1",1,0)
    df_b[rows,length(key)+2] <- ifelse(raw_df[,grep(choice_col,names(raw_df))] == "Candidate 2",1,0)
    
    # Control or treat
    df_a[rows,length(key)+3] <- group
    df_b[rows,length(key)+3] <- group
    
    # Conjoint round
    df_a[rows,length(key)+4] <- round
    df_b[rows,length(key)+4] <- round
    
    # ID
    df_a[rows,length(key)+5] <- raw_df$id
    df_b[rows,length(key)+5] <- raw_df$id
    
  }
  
  if (group == "control") {
    
    colnames(df_a) <- c("issue", "cost", "effect", "cred", "gender", "party", "cand" ,"vote","group","round", "id")
    colnames(df_b) <- c("issue", "cost", "effect", "cred", "gender", "party", "cand" ,"vote","group","round", "id")
    
  } else {
    
    colnames(df_a) <- c("average", "total", "largest", "prop", "origin",
                        "party","cand_ideology","office","cand","vote","group","round", "participantid")
    
    colnames(df_b) <- c("average", "total", "largest", "prop", "origin",
                        "party","cand_ideology","office","cand","vote","group","round", "participantid")
  }
  return(rbind(df_a,df_b))
}

demog$id <- as.character(demog$id)

# Select only those cols relevant for conjoint, incl. participant id to join later
cand_control <- uk %>%
  select(grep("choice",colnames(.)),
         grep("F-",colnames(.)),
         id) %>%
  na.omit() %>%
  cand_sort(., group = "control") %>%
  as_tibble(.) %>%
  left_join(demog, by = "id")

## calculate estimates manually NOT USED ##
cluster_glm <- function(data, formula, cluster, type) {
  
  mod <- glm.cluster(formula = formula,
                     data = data,
                     cluster = cluster,
                     family = if(type == "logit") {
                       binomial(link="logit")
                     } else { 
                       gaussian
                     }
  ) %>%
    summary(.) %>%
    as.data.frame(.) %>%
    mutate(coef_name = rownames(.),
           ci = 1.96*`Std. Error`,
           sig = if(type == "logit") {ifelse(`Pr(>|z|)` < 0.001,"***",
                                             ifelse(`Pr(>|z|)` < 0.01, "**",
                                                    ifelse(`Pr(>|z|)` < 0.05,"*","")))}
           else {ifelse(`Pr(>|t|)` < 0.001,"***",
                        ifelse(`Pr(>|t|)` < 0.01, "**",
                               ifelse(`Pr(>|t|)` < 0.05,"*","")))})
  
}

cand_control$id <- as.factor(cand_control$id)
cand_control$issue <- as.factor(cand_control$issue)
cand_control$cost <- as.factor(cand_control$cost)
cand_control$effect <- as.factor(cand_control$effect)
cand_control$cred <- as.factor(cand_control$cred)
cand_control$gender <- as.factor(cand_control$gender)
cand_control$party <- as.factor(cand_control$party)
cand_control$vote <- as.integer(cand_control$vote)

## Prepare factor levels for better readability

cand_control$cost2 <- NA
cand_control$cost2[cand_control$cost == "Less money available for other public expenditures"] <- "Opportunity cost"
cand_control$cost2[cand_control$cost == "Disclosure of private data"] <- "Private data"
cand_control$cost2[cand_control$cost == "Raising income tax"] <- "Income taxes"
cand_control$cost2[cand_control$cost == "No costs"] <- "No costs"
cand_control$cost2 <- as.factor(cand_control$cost2)
sum(is.na(cand_control$cost2))


cand_control$effect2 <- NA
cand_control$effect2[cand_control$effect == "Has been shown by experts to perform well"] <- "Effective"
cand_control$effect2[cand_control$effect == "Has been shown by experts to have little effect"] <- "Not effective"
cand_control$effect2 <- as.factor(cand_control$effect2)
sum(is.na(cand_control$effect2))


cand_control$cred2 <- NA
cand_control$cred2[cand_control$cred == "Has a reputation for keeping campaign promises"] <- "Credible"
cand_control$cred2[cand_control$cred == "Has a reputation for not keeping campaign promises"] <- "Not credible"
cand_control$cred2 <- as.factor(cand_control$cred2)
sum(is.na(cand_control$cred2))


## AMCEs ##
cand_control$issue <- factor(cand_control$issue,
                             levels = c("Anti-Corruption", "Domestic Security", "Economy"))
cand_control$cost2 <- factor(cand_control$cost2,
                             levels = c("Private data", "Income taxes", "Opportunity cost", "No costs"))
cand_control$effect2 <- factor(cand_control$effect2,
                             levels = c("Not effective", "Effective"))
amces <- cj(cand_control, vote ~ issue + cost2 + effect2 + cred2 + gender + party,
             id = ~id,
            feature_labels = list(issue = "Issue", cost2 = "Costs", effect2 = "Effective",
                                  cred2 = "Credibility", gender = "Gender", party = "Party"))
uk_amce <- plot(amces, legend_title = "Attribute", size = 2) +
  xlab("") +
  theme_minimal(base_size = 13)
uk_amce
ggsave("uk_amce.pdf", plot = uk_amce, height = 7, width = 6)


# CONDITIONAL AMCE INCOME ##
cand_control$income2 <- NA
cand_control$income2[cand_control$Q115 == "Less than £10,000" | cand_control$Q115 == "£10,000 - £19,999"] <- "Low"
cand_control$income2[cand_control$Q115 == "£40,000 - £59,999" | cand_control$Q115 == "£20,000 - £39,999"] <- "Middle"
cand_control$income2[cand_control$Q115 == "£100,000 and over" | cand_control$Q115 == "£60,000 - £99,999"] <- "High"
cand_control$income2 <- as.factor(cand_control$income2)

cand_control$issue <- factor(cand_control$issue,
                             levels = c("Domestic Security", "Anti-Corruption", "Economy"))
cand_control$cost2 <- factor(cand_control$cost2,
                             levels = c("No costs", "Private data", "Income taxes", "Opportunity cost"))
amces2 <- cj(cand_control, vote ~ issue + cost2 + effect2 + cred2 + gender + party,
             id = ~id, by = ~income2,
             feature_labels = list(issue = "Issue", cost2 = "Costs", effect2 = "Effective",
                                   cred2 = "Credibility", gender = "Gender", party = "Party"))
uk_amce_income <- plot(amces2, group = "income2", vline = 0, legend_title = "Income", size = 2) +
  xlab("") +
  theme_minimal(base_size = 13)
uk_amce_income
ggsave("uk_amce_income.pdf", plot = uk_amce_income, height = 7, width = 6)


## ACIES ##
## Costs
cand_control$issue <- factor(cand_control$issue,
                             levels = c("Economy", "Domestic Security", "Anti-Corruption"))
acies1 <- cj(cand_control, vote ~ issue + effect2 + cred2 + gender + party,
             id = ~id, by = ~cost2,
             feature_labels = list(issue = "Issue", effect2 = "Effective",
                                   cred2 = "Credibility", gender = "Gender", party = "Party"))
uk_acie_cost <- plot(acies1, group = "cost2", vline = 0, legend_title = "Cost", size = 2, legend_pos = "bottom") +
  theme_minimal(base_size = 13)
uk_acie_cost
ggsave("uk_acie_cost.pdf", plot = uk_acie_cost, height = 7, width = 6)

## Effectiveness
cand_control$issue <- factor(cand_control$issue,
                             levels = c("Economy", "Domestic Security", "Anti-Corruption"))
acies2 <- cj(cand_control, vote ~ issue + cost2 + cred2 + gender + party,
             id = ~id, by = ~effect2,
             feature_labels = list(issue = "Issue", cost2 = "Costs",
                                   cred2 = "Credibility", gender = "Gender", party = "Party"))
uk_acie_effect <- plot(acies2, group = "effect2", vline = 0, legend_title = "Effective", size = 2) +
  theme_minimal(base_size = 13)
uk_acie_effect
ggsave("uk_acie_effect.pdf", plot = uk_acie_effect, height = 7, width = 6)

### COMBINE ALL 4 PLOTS ###
uk_cj_results <- ggarrange(uk_amce, uk_amce_income, uk_acie_cost, uk_acie_effect,
          nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"))
ggsave("uk_cj_results.pdf", plot = uk_cj_results, height = 9, width = 12)


## SALIENCE BUDGET ALLOCATION
budget <- data.frame(issue = c("Anti-Corruption", "Economic Prosperity", "Domestic Security",
                               "Environment", "Education", "Social Equality"),
                     mean = c(mean(cand_control$Q215_1, na.rm = T), mean(cand_control$Q215_2, na.rm = T),
                              mean(cand_control$Q215_3, na.rm = T), mean(cand_control$Q215_4, na.rm = T),
                              mean(cand_control$Q215_5, na.rm = T), mean(cand_control$Q215_6, na.rm = T))
                     )

budget$issue <- factor(budget$issue, levels = c("Education", "Economic Prosperity", "Domestic Security",
                                                "Environment", "Social Equality", "Anti-Corruption"))


uk_budget <- ggplot(data = budget, aes(x = issue, y = mean)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(mean, 1)), vjust = 1.6, color = "white", size = 3.5) +
  ylab("%") + xlab("") +
  theme_minimal(base_size = 12)
uk_budget
ggsave("uk_budget.pdf", plot = uk_budget, height = 7, width = 9)

## REGRESSION MODELS OF BUDGET ALLOCATIONS ##
# Corruption salience
cand_control$income2 <- factor(cand_control$income2,
                               levels = c("High", "Low", "Middle"))

income_effect1 <- lm(Q215_1 ~ income2 + Q114, data = cand_control)
summary(income_effect1)

# Economy salience
income_effect2 <- lm(Q215_2 ~ income2 + Q114, data = cand_control)
summary(income_effect2)
stargazer(income_effect1, income_effect2, type = "latex")


#### DIAGNOSTIC TESTS ####

cand_control$issue <- factor(cand_control$issue,
                             levels = c("Anti-Corruption", "Domestic Security", "Economy"))
carryover <- cj(cand_control, vote ~ issue + cost2 + effect2 + cred2 + gender + party,
             id = ~id, by = ~round,
             feature_labels = list(issue = "Issue", cost2 = "Costs", effect2 = "Effective",
                                   cred2 = "Credibility", gender = "Gender", party = "Party"))
uk_carry <- plot(carryover, group = "round", vline = 0, legend_title = "Round", size = 2) +
  theme_minimal(base_size = 13)
uk_carry


cand_control$issue <- factor(cand_control$issue,
                             levels = c("Anti-Corruption", "Domestic Security", "Economy"))
profile_order <- cj(cand_control, vote ~ issue + cost2 + effect2 + cred2 + gender + party,
                id = ~id, by = ~cand,
                feature_labels = list(issue = "Issue", cost2 = "Costs", effect2 = "Effective",
                                      cred2 = "Credibility", gender = "Gender", party = "Party"))
uk_profile <- plot(profile_order, group = "cand", vline = 0, legend_title = "Candidate", size = 2) +
  theme_minimal(base_size = 13)
uk_profile

## COMBINE INTO PLOT XY ##
uk_tests <- ggarrange(uk_carry, uk_profile, ncol = 2)
ggsave("uk_tests.pdf", plot = uk_tests, height = 6, width = 10)

## Attention check
prop.table(table(cand_control$Q214 == 6)) # knowledge questions with true answer = 6

## Subject sample statistics
data$Q112 <- as.numeric(data$Q112)
summary(data$Q112)

prop.table(table(data$Q113))

summary(data$Q212_1)

prop.table(table(data$Q114))

prop.table(table(data$Q115))
