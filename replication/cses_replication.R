# Purpose: Load and analyze CSES2 data
# Programmed by: Mats Ahrenshop
# Date: 03 Sep 2019

setwd("C:\\Users\\User\\Documents\\Studium\\Oxf\\THESIS\\Substantial\\Observational Analysis\\Data\\Replication files")
rm(list = ls())
library(stargazer)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(lme4)
library(sjPlot)
library(sjmisc)
library(Hmisc)

load("cses2.Rdata")

### DATA CLEANING ###

# Select countries based on whether MIP question includes corruption and important variables

cses <- subset(x = cses2,
               subset = (cses2$B1006_NAM == "Albania") | (cses2$B1006_NAM == "Brazil") |
                 (cses2$B1006_NAM == "Bulgaria") | (cses2$B1006_NAM == "Chile") |
                 (cses2$B1006_NAM == "France") | (cses2$B1006_NAM == "Germany") |
                 (cses2$B1006_NAM == "Hungary") | (cses2$B1006_NAM == "Ireland") |
                 (cses2$B1006_NAM == "Israel") | (cses2$B1006_NAM == "Japan") |
                 (cses2$B1006_NAM == "Kyrgyzstan") | (cses2$B1006_NAM == "Mexico") |
                 (cses2$B1006_NAM == "Netherlands") | (cses2$B1006_NAM == "Peru") |
                 (cses2$B1006_NAM == "Philippines") | (cses2$B1006_NAM == "Poland") |
                 (cses2$B1006_NAM == "Portugal") | (cses2$B1006_NAM == "Romania") |
                 (cses2$B1006_NAM == "Russian Federation") | (cses2$B1006_NAM == "Republic of Korea"),
               select = c(B1004, B1006_NAM, B1005, B1028, B3009, B3044, B3004_1, B3005_1, B3006_1,
                          B3006_2, B3007_1, B3007_2, B3010, B3011, B2001, B2002, B2003, B3045, B2025,
                          B2011, B2012, B3012, B2030,
                          B2020, B2005, B2026, B2029, B2028, B2027, B3028, B3029_1, B3029_2, B3029_3, B2010)
)
colnames(cses) <- c("ID_election", "country", "ID_resp", "quest_year", "mip", "corrupt", "turnout",
                    "vote_pres", "vote_lh1", "vote_lh2", "vote_uh1", "vote_uh2",
                    "performance_mip", "performance_gen", "age", "gender", "edu", "lr", "religion",
                    "occup", "class", "satis_dem", "urban", "hh_income", "union", "lang", "ethnic",
                    "race", "region", "pid_binary", "pid_which1", "pid_which2", "pid_which3", "employment")

cses$country[cses$country == "Russian Federation"] <- "Russia"
cses$country[cses$country == "Republic of Korea"] <- "South Korea"

# Add macro-level CPI data from respective years
cses$cpi <- NA
cses$cpi[cses$country == "Albania"] <- 24
cses$cpi[cses$country == "Bulgaria"] <- 39
cses$cpi[cses$country == "Brazil"] <- 40
cses$cpi[cses$country == "Chile"] <- 73
cses$cpi[cses$country == "Germany"] <- 73
cses$cpi[cses$country == "France"] <- 63
cses$cpi[cses$country == "Hungary"] <- 49
cses$cpi[cses$country == "Ireland"] <- 69
cses$cpi[cses$country == "Israel"] <- 70
cses$cpi[cses$country == "Japan"] <- 69
cses$cpi[cses$country == "Kyrgyzstan"] <- 23
cses$cpi[cses$country == "Mexico"] <- 36
cses$cpi[cses$country == "Netherlands"] <- 90
cses$cpi[cses$country == "Peru"] <- 33
cses$cpi[cses$country == "Philippines"] <- 26
cses$cpi[cses$country == "Poland"] <- 41
cses$cpi[cses$ID_election == "PRT_2002"] <- 63
cses$cpi[cses$ID_election == "PRT_2005"] <- 65
cses$cpi[cses$country == "Romania"] <- 29
cses$cpi[cses$country == "Russia"] <- 28
cses$cpi[cses$country == "South Korea"] <- 45


# Add macro-level polity data from respective years
cses$pol <- NA
cses$pol[cses$country == "Albania"] <- 9
cses$pol[cses$country == "Bulgaria"] <- 9
cses$pol[cses$country == "Brazil"] <- 8
cses$pol[cses$country == "Chile"] <- 9
cses$pol[cses$country == "Germany"] <- 10
cses$pol[cses$country == "France"] <- 9
cses$pol[cses$country == "Hungary"] <- 10
cses$pol[cses$country == "Ireland"] <- 10
cses$pol[cses$country == "Israel"] <- 6
cses$pol[cses$country == "Japan"] <- 10
cses$pol[cses$country == "Kyrgyzstan"] <- 3
cses$pol[cses$country == "Mexico"] <- 8
cses$pol[cses$country == "Netherlands"] <- 10
cses$pol[cses$country == "Peru"] <- 9
cses$pol[cses$country == "Philippines"] <- 8
cses$pol[cses$country == "Poland"] <- 9
cses$pol[cses$ID_election == "PRT_2002"] <- 10
cses$pol[cses$ID_election == "PRT_2005"] <- 10
cses$pol[cses$country == "Romania"] <- 9
cses$pol[cses$country == "Russia"] <- 6
cses$pol[cses$country == "South Korea"] <- 8

## CODE MIP (consult CSES codebook for number codes)
# Albania
cses$mip[cses$mip == 2 & cses$country == "Albania"] <- "corruption"
cses$mip[(cses$mip == 18 | cses$mip == 10) & (cses$country == "Albania")] <- "security"
cses$mip[(cses$mip == 1 | cses$mip == 3 | cses$mip == 13 | cses$mip == 14 | cses$mip == 21 | cses$mip == 23 | cses$mip == 25) & (cses$country == "Albania")] <- "economy"

# Brazil
cses$mip[(cses$mip == 4 | cses$mip == 17) & cses$country == "Brazil"] <- "corruption"
cses$mip[(cses$mip == 2 | cses$mip == 13) & (cses$country == "Brazil")] <- "security"
cses$mip[(cses$mip == 1 | cses$mip == 3 | cses$mip == 7 | cses$mip == 10 | cses$mip == 11) & (cses$country == "Brazil")] <- "economy"

# Bulgaria
cses$mip[cses$mip == 2 & cses$country == "Bulgaria"] <- "corruption"
cses$mip[(cses$mip == 6 | cses$mip == 8) & (cses$country == "Bulgaria")] <- "security"
cses$mip[(cses$mip == 3 | cses$mip == 4 | cses$mip == 9 | cses$mip == 19) & (cses$country == "Bulgaria")] <- "economy"

# Chile
cses$mip[cses$mip == 12 & cses$country == "Chile"] <- "corruption"
cses$mip[(cses$mip == 7 | cses$mip == 8 | cses$mip == 18) & (cses$country == "Chile")] <- "security"
cses$mip[(cses$mip == 1 | cses$mip == 2 | cses$mip == 3 | cses$mip == 4 | cses$mip == 5 | cses$mip == 23) & (cses$country == "Chile")] <- "economy"

# France
cses$mip[cses$mip == 18 & cses$country == "France"] <- "corruption" # political-financial scandals
cses$mip[(cses$mip == 1) & (cses$country == "France")] <- "security"
cses$mip[(cses$mip == 2 | cses$mip == 19) & (cses$country == "France")] <- "economy"

# Germany
cses$mip[cses$mip == 13 & cses$country == "Germany"] <- "corruption" # corruption of parties/politicians
cses$mip[(cses$mip == 14 | cses$mip == 18) & (cses$country == "Germany")] <- "security"
cses$mip[(cses$mip == 2 | cses$mip == 20) & (cses$country == "Germany")] <- "economy"

# Hungary
cses$mip[cses$mip == 6 & cses$country == "Hungary"] <- "corruption"
cses$mip[(cses$mip == 17 | cses$mip == 89) & (cses$country == "Hungary")] <- "security"
cses$mip[(cses$mip == 1 | cses$mip == 5 | cses$mip == 10 | cses$mip == 14 | cses$mip == 26 | cses$mip == 30 | cses$mip == 72) & (cses$country == "Hungary")] <- "economy"

# Ireland
cses$mip[cses$mip == 5 & cses$country == "Ireland"] <- "corruption" # corruption and dishonesty in politics
cses$mip[(cses$mip == 2 | cses$mip == 3) & (cses$country == "Ireland")] <- "security"
cses$mip[(cses$mip == 8 | cses$mip == 9) & (cses$country == "Ireland")] <- "economy"

# Israel
cses$mip[cses$mip == 31 & cses$country == "Israel"] <- "corruption" # corruption/rule of law
cses$mip[(cses$mip == 1 | cses$mip == 5 | cses$mip == 22 | cses$mip == 23) & (cses$country == "Israel")] <- "security"
cses$mip[(cses$mip == 13 | cses$mip == 14 | cses$mip == 15 | cses$mip == 16) & (cses$country == "Israel")] <- "economy"

# Japan
cses$mip[cses$mip == 9 & cses$country == "Japan"] <- "corruption" # political corruption / distrust in politicians
cses$mip[(cses$mip == 23) & (cses$country == "Japan")] <- "security"
cses$mip[(cses$mip == 2 | cses$mip == 13 | cses$mip == 14) & (cses$country == "Japan")] <- "economy"

# Kyrgyzstan
cses$mip[cses$mip == 3 & cses$country == "Kyrgyzstan"] <- "corruption"
cses$mip[(cses$mip == 8 | cses$mip == 10 | cses$mip == 13) & (cses$country == "Kyrgyzstan")] <- "security"
cses$mip[(cses$mip == 1 | cses$mip == 12 | cses$mip == 14 | cses$mip == 17) & (cses$country == "Kyrgyzstan")] <- "economy"

# Mexico
cses$mip[cses$mip == 5 & cses$country == "Mexico"] <- "corruption" # corruption/impunity
cses$mip[(cses$mip == 2 | cses$mip == 10 | cses$mip == 17) & (cses$country == "Mexico")] <- "security"
cses$mip[(cses$mip == 1 | cses$mip == 3 | cses$mip == 4 | cses$mip == 8 | cses$mip == 25) & (cses$country == "Mexico")] <- "economy"

# Netherlands
cses$mip[cses$mip == 6 & cses$country == "Netherlands"] <- "corruption"
cses$mip[(cses$mip == 7 | cses$mip == 25) & (cses$country == "Netherlands")] <- "security"
cses$mip[(cses$mip == 3 | cses$mip == 8 | cses$mip == 15 | cses$mip == 30) & (cses$country == "Netherlands")] <- "economy"

# Peru
cses$mip[cses$mip == 9 & cses$country == "Peru"] <- "corruption"
cses$mip[(cses$mip == 5 | cses$mip == 7) & (cses$country == "Peru")] <- "security"
cses$mip[(cses$mip == 1 | cses$mip == 2 | cses$mip == 3 | cses$mip == 4 | cses$mip == 18) & (cses$country == "Peru")] <- "economy"

# Philippines
cses$mip[cses$mip == 11 & cses$country == "Philippines"] <- "corruption" # corruption/public morality
cses$mip[(cses$mip == 30 | cses$mip == 31 | cses$mip == 32 | cses$mip == 33 | cses$mip == 35 | cses$mip == 40 | cses$mip == 43) & (cses$country == "Philippines")] <- "security"
cses$mip[(cses$mip == 1 | cses$mip == 2 | cses$mip == 3 | cses$mip == 4 | cses$mip == 5) & (cses$country == "Philippines")] <- "economy"

# Poland
cses$mip[cses$mip == 12 & cses$country == "Poland"] <- "corruption" # combat corruption, theft, state overspending
cses$mip[(cses$mip == 10 | cses$mip == 11) & (cses$country == "Poland")] <- "security"
cses$mip[(cses$mip == 1 | cses$mip == 2 | cses$mip == 3 | cses$mip == 5 | cses$mip == 6 | cses$mip == 22 | cses$mip == 25) & (cses$country == "Poland")] <- "economy"

# Portugal 2002
cses$mip[cses$mip == 13 & cses$ID_election == "PRT_2002"] <- "corruption"
cses$mip[(cses$mip == 5 | cses$mip == 6) & (cses$ID_election == "PRT_2002")] <- "security"
cses$mip[(cses$mip == 1 | cses$mip == 3 | cses$mip == 7 | cses$mip == 9 | cses$mip == 12 | cses$mip == 19) & (cses$ID_election == "PRT_2002")] <- "economy"

# Portugal 2005
cses$mip[cses$mip == 51 & cses$ID_election == "PRT_2005"] <- "corruption" # political corruption
cses$mip[(cses$mip == 53 | cses$mip == 56 | cses$mip == 78 | cses$mip == 91) & (cses$ID_election == "PRT_2005")] <- "security"
cses$mip[(cses$mip == 9 | cses$mip == 32 | cses$mip == 37 | cses$mip == 41 | cses$mip == 42 | cses$mip == 49) & (cses$ID_election == "PRT_2005")] <- "economy"

# Romania
cses$mip[cses$mip == 2 & cses$country == "Romania"] <- "corruption"
cses$mip[(cses$mip == 5 | cses$mip == 19 | cses$mip == 25) & (cses$country == "Romania")] <- "security"
cses$mip[(cses$mip == 1 | cses$mip == 3 | cses$mip == 6 | cses$mip == 11) & (cses$country == "Romania")] <- "economy"

# Russia
cses$mip[cses$mip == 30 & cses$country == "Russia"] <- "corruption"
cses$mip[(cses$mip == 15 | cses$mip == 21 | cses$mip == 28 | cses$mip == 31) & (cses$country == "Russia")] <- "security"
cses$mip[(cses$mip == 1 | cses$mip == 2 | cses$mip == 3 | cses$mip == 4 | cses$mip == 8 | cses$mip == 13 | cses$mip == 14) & (cses$country == "Russia")] <- "economy"

# South Korea
cses$mip[cses$mip == 62 & cses$country == "South Korea"] <- "corruption" # cleaning of corruption
cses$mip[(cses$mip == 75 | cses$mip == 78) & (cses$country == "South Korea")] <- "security"
cses$mip[(cses$mip == 1 | cses$mip == 2 | cses$mip == 3 | cses$mip == 4 | cses$mip == 21 | cses$mip == 44) & (cses$country == "South Korea")] <- "economy"

## Handling missing data
describe(cses$mip)
cses$mip[cses$mip == 999] <- NA
csesc <- subset(cses,
                subset = cses$mip != 996 & cses$mip != 997 & cses$mip != 998) # 997 refused; 998 don't know; exclude via subset

# Work with subsetted data now
## CREATE BINARY CORRUPTION MIP
csesc$mip_corrupt <- NA
csesc$mip_corrupt <- ifelse(csesc$mip == "corruption", 1, 0)

csesc$mip_econ <- NA
csesc$mip_econ <- ifelse(csesc$mip == "economy", 1, 0)

csesc$mip_security <- NA
csesc$mip_security <- ifelse(csesc$mip == "security", 1, 0)

## CODE INCUMBENT VOTE

# Clean vote variables
csesc$vote_pres[csesc$vote_pres == 94 | csesc$vote_pres == 96 | csesc$vote_pres == 97 |
                  csesc$vote_pres == 98 | csesc$vote_pres == 99] <- NA
csesc$vote_lh1[csesc$vote_lh1 == 94 | csesc$vote_lh1 == 96 | csesc$vote_lh1 == 97 |
                 csesc$vote_lh1 == 98 | csesc$vote_lh1 == 99] <- NA
csesc$vote_uh1[csesc$vote_uh1 == 94 | csesc$vote_uh1 == 96 | csesc$vote_uh1 == 97 |
                 csesc$vote_uh1 == 98 | csesc$vote_uh1 == 99] <- NA

unique(csesc$vote_pres)
unique(csesc$vote_lh1)
unique(csesc$vote_uh1)


## Stack binary incumbent vote variable
csesc$incumbent <- NA

csesc$incumbent[csesc$country == "Albania" & csesc$vote_lh1 == 1] <- 1
csesc$incumbent[csesc$country == "Albania" & csesc$vote_lh1 != 1 & !is.na(csesc$vote_lh1)] <- 0

csesc$incumbent[csesc$country == "Bulgaria" & csesc$vote_lh1 == 2] <- 1
csesc$incumbent[csesc$country == "Bulgaria" & csesc$vote_lh1 != 2 & !is.na(csesc$vote_lh1)] <- 0

csesc$incumbent[csesc$country == "Brazil" & csesc$vote_pres == 45] <- 1
csesc$incumbent[csesc$country == "Brazil" & csesc$vote_pres != 45 & !is.na(csesc$vote_pres)] <- 0

csesc$incumbent[csesc$country == "Chile" & csesc$vote_pres == 5] <- 1
csesc$incumbent[csesc$country == "Chile" & csesc$vote_pres != 5 & !is.na(csesc$vote_pres)] <- 0

csesc$incumbent[csesc$country == "Germany" & csesc$vote_lh1 == 1] <- 1
csesc$incumbent[csesc$country == "Germany" & csesc$vote_lh1 != 1 & !is.na(csesc$vote_lh1)] <- 0

csesc$incumbent[csesc$country == "France" & csesc$vote_pres == 5] <- 1
csesc$incumbent[csesc$country == "France" & csesc$vote_pres != 5 & !is.na(csesc$vote_pres)] <- 0

csesc$incumbent[csesc$country == "Hungary" & csesc$vote_lh1 == 10] <- 1
csesc$incumbent[csesc$country == "Hungary" & csesc$vote_lh1 != 10 & !is.na(csesc$vote_lh1)] <- 0

csesc$incumbent[csesc$country == "Ireland" & csesc$vote_lh1 == 1] <- 1
csesc$incumbent[csesc$country == "Ireland" & csesc$vote_lh1 != 1 & !is.na(csesc$vote_lh1)] <- 0

csesc$incumbent[csesc$country == "Israel" & csesc$vote_lh1 == 1] <- 1
csesc$incumbent[csesc$country == "Israel" & csesc$vote_lh1 != 1 & !is.na(csesc$vote_lh1)] <- 0

csesc$incumbent[csesc$country == "Japan" & csesc$vote_uh1 == 1] <- 1
csesc$incumbent[csesc$country == "Japan" & csesc$vote_uh1 != 1 & !is.na(csesc$vote_uh1)] <- 0

csesc$incumbent[csesc$country == "Kyrgyzstan" & csesc$vote_pres == 2] <- 1
csesc$incumbent[csesc$country == "Kyrgyzstan" & csesc$vote_pres != 2 & !is.na(csesc$vote_pres)] <- 0

csesc$incumbent[csesc$country == "Mexico" & csesc$vote_lh1 == 1] <- 1
csesc$incumbent[csesc$country == "Mexico" & csesc$vote_lh1 != 1 & !is.na(csesc$vote_lh1)] <- 0

csesc$incumbent[csesc$country == "Netherlands" & csesc$vote_lh1 == 1] <- 1
csesc$incumbent[csesc$country == "Netherlands" & csesc$vote_lh1 != 1 & !is.na(csesc$vote_lh1)] <- 0

csesc$incumbent[csesc$country == "Peru" & csesc$vote_lh1 == 7] <- 1
csesc$incumbent[csesc$country == "Peru" & csesc$vote_lh1 != 7 & !is.na(csesc$vote_lh1)] <- 0

csesc$incumbent[csesc$country == "Philippines" & csesc$vote_pres == 1] <- 1
csesc$incumbent[csesc$country == "Philippines" & csesc$vote_pres != 1 & !is.na(csesc$vote_pres)] <- 0

csesc$incumbent[csesc$country == "Poland" & csesc$vote_lh1 == 2] <- 1
csesc$incumbent[csesc$country == "Poland" & csesc$vote_lh1 != 2 & !is.na(csesc$vote_lh1)] <- 0

csesc$incumbent[csesc$ID_election == "PRT_2002" & csesc$vote_lh1 == 2] <- 1
csesc$incumbent[csesc$ID_election == "PRT_2002" & csesc$vote_lh1 != 2 & !is.na(csesc$vote_lh1)] <- 0

csesc$incumbent[csesc$ID_election == "PRT_2005" & csesc$vote_lh1 == 4] <- 1
csesc$incumbent[csesc$ID_election == "PRT_2005" & csesc$vote_lh1 != 4 & !is.na(csesc$vote_lh1)] <- 0

csesc$incumbent[csesc$country == "Romania" & csesc$vote_pres == 2] <- 1
csesc$incumbent[csesc$country == "Romania" & csesc$vote_pres != 2 & !is.na(csesc$vote_pres)] <- 0

csesc$incumbent[csesc$country == "Russia" & csesc$vote_pres == 4] <- 1
csesc$incumbent[csesc$country == "Russia" & csesc$vote_pres != 4 & !is.na(csesc$vote_pres)] <- 0

csesc$incumbent[csesc$country == "South Korea" & csesc$vote_lh1 == 2] <- 1
csesc$incumbent[csesc$country == "South Korea" & csesc$vote_lh1 != 2 & !is.na(csesc$vote_lh1)] <- 0


prop.table(table(csesc$incumbent))

## Inspect controls
describe(csesc$corrupt)
csesc$corrupt[csesc$corrupt == 7 | csesc$corrupt == 8 | csesc$corrupt == 9] <- NA

describe(csesc$gender)
csesc$gender[csesc$gender == 9] <- NA

describe(csesc$age) # 1 2 3 4 16, 99, 100, 997 998 999
csesc$age[csesc$age == 1 | csesc$age == 2 | csesc$age == 3 | csesc$age == 4 | csesc$age == 997 | csesc$age == 998 |
            csesc$age == 999] <- NA

describe(csesc$edu)
csesc$edu[csesc$edu == 97 | csesc$edu == 98 | csesc$edu == 99] <- NA

describe(csesc$lr) # 96 97 98 99
csesc$lr[csesc$lr == 96 | csesc$lr == 97 | csesc$lr == 98 |
           csesc$lr == 99] <- NA

describe(csesc$class) #5, 6, 7, 8, 9
csesc$class[csesc$class == 5 | csesc$class == 6 | csesc$class == 7 |
              csesc$class == 8 | csesc$class == 9] <- NA

describe(csesc$hh_income) # 6, 7, 8, 9
csesc$hh_income[csesc$hh_income == 6 | csesc$hh_income == 7 |
                  csesc$hh_income == 8 | csesc$hh_income == 9] <- NA

describe(csesc$pid_binary) # 7, 8, 9
csesc$pid_binary[csesc$pid_binary == 7 | csesc$pid_binary == 8 |
                   csesc$pid_binary == 9] <- NA

describe(csesc$performance_gen) #5, 6, 7, 8, 9
csesc$performance_gen[csesc$performance_gen == 5 | csesc$performance_gen == 6 | csesc$performance_gen == 7 |
                        csesc$performance_gen == 8 | csesc$performance_gen == 9] <- NA

describe(csesc$performance_mip) # 7, 8, 9
csesc$performance_mip[csesc$performance_mip == 7 |
                        csesc$performance_mip == 8 | csesc$performance_mip == 9] <- NA

describe(csesc$satis_dem)
csesc$satis_dem[csesc$satis_dem == 6 | csesc$satis_dem == 7 |
                  csesc$satis_dem == 8 | csesc$satis_dem == 9] <- NA

describe(csesc$employment)
csesc$employment[csesc$employment == 97 |
                   csesc$employment == 98 | csesc$employment == 99] <- NA


### DESCRIPTIVE ANALYSIS ###

## 1. Aggregate salience of corruption ##

prop.table(table(csesc$mip_econ))
prop.table(table(csesc$mip_security))
prop.table(table(csesc$mip_corrupt))

mips_general <- data.frame(issue = rep(c("Economy", "Security", "Corruption"), each = 2),
                           prop = c(0.46, 0.54, 0.11, 0.89, 0.06, 0.94),
                           yn = c("Yes", "No", "Yes", "No", "Yes", "No")
)

str(mips_general$issue)
mips_general$issue <- factor(mips_general$issue, levels = c("Economy", "Security", "Corruption"))

## PLOT FIGURE 11 MOST IMPORTANT ISSUE AGGREGATE ##
p_mip <- ggplot(mips_general, aes(x = issue, y = prop, fill = factor(yn))) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = prop), size = 4, position = position_fill(vjust = 0.5)) +
  xlab("") + ylab("Proportion") +
  scale_fill_discrete("MIP?") +
  theme_minimal(base_size = 12)
p_mip
ggsave("mip_aggregate.pdf", plot = p_mip, width = 6, height = 4)


## 2. How different is salience from perceptions and levels of corruption? ##

## Perceptions
csesc$corrupt2 <- NA
csesc$corrupt2[csesc$corrupt == 1] <- "Very widespread"
csesc$corrupt2[csesc$corrupt == 2] <- "Quite widespread"
csesc$corrupt2[csesc$corrupt == 3] <- "Not very widespread"
csesc$corrupt2[csesc$corrupt == 4] <- "Hardly happens at all"

percept <- tapply(csesc$corrupt, csesc$country, mean, na.rm = T)
cpi <- tapply(csesc$cpi, csesc$country, mean, na.rm = T)
cor(percept, cpi)
var(csesc$corrupt[csesc$country == "Romania"], na.rm = T)
var(csesc$corrupt[csesc$country == "Netherlands"], na.rm = T)

tapply(csesc$mip_corrupt, csesc$corrupt, mean)
sal_percept <- data.frame(corrupt = rep(c("Very widespread", "Quite widespread",
                                         "Not very widespread", "Hardly happens at all"), 2),
                         prop = c(0.094, 0.047, 0.019, 0.036, 0.906, 0.953, 0.981, 0.964),
                         yn = c("Yes", "Yes", "Yes", "Yes", "No", "No", "No", "No")
)

## PLOT FIGURE 14 IN APPENDIX: SALIENCE BY PERCEPTION ##
p_salp <- ggplot(sal_percept, aes(x = corrupt, y = prop, fill = factor(yn))) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = prop), size = 3, position = position_fill(vjust = 0.5)) +
  xlab("Corruption") + ylab("Proportion") +
  scale_fill_discrete("Corruption MIP?") +
  theme_minimal(base_size = 12)
p_salp
ggsave("salience_perception.pdf", plot = p_salp, width = 9, height = 7)

m_salp <- lm(mip_corrupt ~ corrupt + cpi2 + pol + edu, data = csesc)

m_sale <- lm(mip_econ ~ hh_income + employment + class, data = csesc)
stargazer(m_salp, m_sale, type = "latex")

## Levels of corruption
csesc$cpi2 <- 100 - csesc$cpi # higher values, higher corruption

cpi <- c(tapply(csesc$cpi2, csesc$country, mean) / 100)
mip <- c(tapply(csesc$mip_corrupt, csesc$country, mean))
inc <- c(tapply(csesc$incumbent, csesc$country, mean, na.rm = T))

mip_df <- data.frame(cpi = cpi, mip = mip, country = names(cpi))

## PLOT FIGURE 15 IN APPENDIX SALIENCE BY LEVEL OF CORRUPTION ##
p_sall <- ggplot(mip_df, aes(x = cpi, y = mip)) +
  geom_text(aes(label = country)) +
  labs(x = "Corruption Perceptions Index", y = "Proportion of MII = Corruption") +
  geom_smooth(method = "loess", se = F) +
  theme_minimal(base_size = 12)
p_sall
ggsave("salience_level.pdf", plot = p_sall, width = 9, height = 7)


## INCOME
csesc$income2 <- NA
csesc$income2[csesc$hh_income == 1] <- "Lowest quintile"
csesc$income2[csesc$hh_income == 2] <- "Second quintile"
csesc$income2[csesc$hh_income == 3] <- "Third quintile"
csesc$income2[csesc$hh_income == 4] <- "Fourth quintile"
csesc$income2[csesc$hh_income == 5] <- "Highest quintile"

prop.table(table(csesc$income2))

tapply(csesc$mip_corrupt, csesc$income2, mean, na.rm = T)

income_corrupt <- data.frame(income = rep(c("Q1", "Q2", "Q3",
                                            "Q4", "Q5"), each = 2),
                             prop = c(0.04, (1-0.04), 0.05, (1-0.05), 0.05, 1-(0.05), 0.07, (1-0.07), 0.10, (1-0.10)),
                             yn = c("Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No")
)

income_corrupt$income_new <- factor(income_corrupt$income,
                                    levels = c("Q1", "Q2",
                                               "Q3", "Q4",
                                               "Q5"))

## PLOT FIGURE 12 SALIENCE BY INCOME
cor <- ggplot(income_corrupt, aes(x = income_new, y = prop, fill = factor(yn))) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = prop), size = 4, position = position_fill(vjust = 0.5)) +
  xlab("Income Quintile") + ylab("Proportion") +
  scale_fill_discrete("Corruption MIP?") +
  theme_minimal(base_size = 12)
cor

tapply(csesc$mip_econ, csesc$income2, mean, na.rm = T)

income_econ <- data.frame(income = rep(c("Q1", "Q2", "Q3",
                                         "Q4", "Q5"), each = 2),
                          prop = c(0.51, (1-0.51), 0.51, (1-0.51), 0.50, 1-(0.50), 0.43, (1-0.43), 0.42, (1-0.42)),
                          yn = c("Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No")
)

income_econ$income_new <- factor(income_econ$income,
                                 levels = c("Q1", "Q2", "Q3",
                                            "Q4", "Q5"))

econ <- ggplot(income_econ, aes(x = income_new, y = prop, fill = factor(yn))) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = prop), size = 4, position = position_fill(vjust = 0.5)) +
  xlab("Income Quintile") + ylab("Proportion") +
  scale_fill_discrete("Economy MIP?") +
  theme_minimal(base_size = 12)
econ

tapply(csesc$mip_security, csesc$income2, mean, na.rm = T)

income_sec <- data.frame(income = rep(c("Q1", "Q2", "Q3",
                                        "Q4", "Q5"), each = 2),
                         prop = c(0.09, (1-0.09), 0.11, (1-0.11), 0.10, 1-(0.10), 0.12, (1-0.12), 0.10, (1-0.10)),
                         yn = c("Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No")
)

income_sec$income_new <- factor(income_sec$income,
                                levels = c("Q1", "Q2", "Q3",
                                           "Q4", "Q5"))

sec <- ggplot(income_sec, aes(x = income_new, y = prop, fill = factor(yn))) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = prop), size = 4, position = position_fill(vjust = 0.5)) +
  xlab("Income Quintile") + ylab("") +
  scale_fill_discrete("Security MIP?") +
  theme_minimal(base_size = 12)
sec

p_income <- ggarrange(
  cor,
  ggarrange(econ, sec, ncol = 2, labels = c("B", "C")),
  nrow = 2,
  labels = "A"
)
ggsave("salience_income.pdf", plot = p_income, height = 6, width = 8)


## REGRESSION MODELS: SALIENCE ON INCOME (REPRODUCTION OF TABLE 6)
income_c <- glmer(mip_corrupt ~ hh_income + gender + age + edu + lr + class + employment + pid_binary + (1 | country),
                  family = binomial(link = "logit"), data = csesc)

income_e <- glmer(mip_econ ~ hh_income + gender + age + edu + lr + class + employment + pid_binary + (1 | country),
                  family = binomial(link = "logit"), data = csesc)

income_s <- glmer(mip_security ~ hh_income + gender + age + edu + lr + class + employment + pid_binary + (1 | country),
                  family = binomial(link = "logit"), data = csesc)

stargazer(income_c, income_e, income_s, type = "latex")


## DISAPPROVEMENT OF CORRUPT PRACTICES
load("wvs6.rdata") # Load World Values Survey Wave 6 data
wvs_c <- WV6_Data_R[, c("V2", "V202")] # country; corruption justifiable

unique(wvs_c$V202)
wvs_c$V202[wvs_c$V202 == -1 | wvs_c$V202 == -2 | wvs_c$V202 == -5] <- NA

prop.table(table(wvs_c$V202))
wvs_df <- data.frame(cat = c("never justifiable", "2", "3", "4", "5", "6", "7", "8", "9", "always justifiable"),
                     prop = prop.table(table(wvs_c$V202))
                     
)

wvs_c <- na.omit(wvs_c)

length(unique(wvs_c$V2))

disapp <- ggplot(wvs_df, aes(x = prop.Var1, y = prop.Freq)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("How justifiable is corruption?") + ylab("Proportion") +
  theme_minimal(base_size = 12)
disapp
ggsave("corrupt_disapp.pdf", plot = disapp, height = 6, width = 6)



## 5. MIP EFFECT ON VOTE CHOICE ##

## GLM
vote_int <- glm(incumbent ~ cpi*mip_corrupt, family = binomial(link = "logit"), data = csesc)

vote_int_c <- glm(incumbent ~ cpi*mip_corrupt + performance_mip + performance_gen + satis_dem + gender + edu + age + lr + hh_income + class + pid_binary,
                  family = binomial(link = "logit"),
                  data = csesc)

vote_int_m <- glmer(incumbent ~ cpi*mip_corrupt + (1 | country),
                 family = binomial(link = "logit"), data = csesc)

vote_int_mc <- glmer(incumbent ~ cpi*mip_corrupt + performance_mip + performance_gen + satis_dem + gender + edu + age + lr + hh_income + class + pid_binary + (1 | country),
                     family = binomial(link = "logit"),
                     data = csesc)

stargazer(vote_int, vote_int_c, vote_int_m, vote_int_mc, type = "latex")

vote_interact <- plot_model(vote_int, type = "int", terms = c("cpi", "mip_corrupt"),
                            legend.title = "Corruption MIP?", title = "") +
  labs(x = "CPI (original scale)", y = "Predicted probabilities of incumbent vote") +
  theme_minimal(base_size = 12)
vote_interact
ggsave("vote_interact.pdf", plot = vote_interact, height = 6, width = 8)

