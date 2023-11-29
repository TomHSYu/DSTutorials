##########################################
##########################################
# STATS 209: Final Project Codes 
# Author: Tom Yu
# Last Updated: 11/30/2021
##########################################
##########################################


##########################################
# STATS 209: Final Project Coding Part 1. 
# Purpose: Data Gathering & Cleaning
# Last Updated: 11/8/2021
##########################################

### Set the working directory: 
setwd('C:/Users/thy53/OneDrive/Documents/Classes/STATS 209/Project')
### Load packages 
library(readxl)

### 1. Crosswalk Data Cleaning
house_leadership <- read_xlsx('Data/House_Member_Crosswalk/house_assignments.xlsx', 
                              col_names = T, skip =0)
house_roster <- read_xlsx('Data/House_Member_Crosswalk/house_members.xlsx', 
                          col_names = T,
                          skip = 1)

# Just keeping the 108th Congress onwards
house_raw <- subset(house_roster, cong > 107)
house_leadership_raw <- subset(house_leadership, Congress > 107)

# Collapsing the leadership data to the representative-congress level
house_leadership_clean <- house_leadership_raw[, c(1,3,10, 18,19)]
colnames(house_leadership_clean)[1] <- 'cong'
house_leadership_clean <- aggregate(house_leadership_clean[, c(3)], 
                                    list(house_leadership_clean$cong, 
                                         house_leadership_clean$id), sum)
colnames(house_leadership_clean)[1:2] <- c('cong', 'id')

# Merge the two together - keeping the only necessary variable from the leadership table
house_raw <- merge(house_raw, house_leadership_clean, by = c('id', 'cong'), all.x = T) 

# Confirmed: all members with N/A for the leadership position either did not hold any leadership position or were out of the office
house_raw$`Senior Party Member` <- ifelse(is.na(house_raw$`Senior Party Member`), 
                                          0, house_raw$`Senior Party Member`)

# Fix the name to match the SOD data (switch first and last name, confirm the need for the middle name later)
test <- data.frame(do.call('rbind', strsplit(as.character(house_raw$name), ',', fixed=T)))
test <- as.data.frame(apply(test, 1:2, toupper))
test$X3 <- paste(test$X2, test$X1, sep = ' ')
house_clean <- cbind(test$X3, house_raw)
house_clean <- house_clean[, -c(5, 14)] # removing the old name and notes from the raw data 
colnames(house_clean)[1] <- 'representative'
house_clean$representative <- substring(house_clean$representative, 2)

house_clean$cd_id <- paste0(house_clean$state_po, house_clean$district)
house_clean$leader <- ifelse(house_clean$`Senior Party Member` > 0, 1, 0)

house_crosswalk <- house_clean[, c(1,3,8,10,14:15)]
house_crosswalk <- house_crosswalk[order(house_crosswalk$representative), ]


### 2. Ideology/Average Voter data - Partisan Balance - Data 
ideology_data_10 <- read.csv('Data/cd_113_TW_ideology_estimates.csv')
ideology_data_00 <- read.csv('Data/cd_112_TW_ideology_estimates_v2.csv')

# Renaming the variables for the merging 
colnames(ideology_data_10)[2] <- 'mrp_mean_2010'
colnames(ideology_data_00)[2] <- 'mrp_mean_2000'

ideology_data_00$cd_id <- paste0(ideology_data_00$abb, ideology_data_00$cd_fips%%100)
ideology_data_00_clean <- ideology_data_00[, c(1:7,12:14)]

ideology_data_10$cd_id <- paste0(ideology_data_10$abb, ideology_data_10$cd_fips%%100)
ideology_data_10_clean <- ideology_data_10[, c(1:7,11, 13:14)]

# only keep necessary variables
mrp_keep_vars <- c('mrp_mean_2000', 'mrp_mean_2010', 'cd_id', 
                   'pres_2008', 'pres_2012')

ideology_data_00_clean <- ideology_data_00_clean[, colnames(ideology_data_00_clean) %in% mrp_keep_vars]
ideology_data_10_clean <- ideology_data_10_clean[, colnames(ideology_data_10_clean) %in% mrp_keep_vars]

# Adjusting the current dollars to 2010 dollars
inflation_data <- read_xlsx('Data/inflation_rates.xlsx')

### 3. FAADS Data (Rev. 11/7/2021)
load('Data/faads_new.Rda')
faads_raw <- faads_temp_2
faads_combined_inf <- merge(faads_raw, inflation_data, by = 'year')
faads_combined_inf$total_outlays <- faads_combined_inf$total_FAADS / faads_combined_inf$inflation_rate
faads_combined_inf$total_outlays_2 <- faads_combined_inf$total_FAADS_2 / faads_combined_inf$inflation_rate

faads_combined_inf$year <- faads_combined_inf$year -1  # Check / adjust later if necessary 

# rename some at-large state conventions to be consistent with other files 
wrong_cd_ids <- c("AK0", "MT0", "ND0", "SD0", "WY0", "VT0", "DE0")

faads_combined_inf$cd_id <- ifelse(faads_combined_inf$cd_id %in% wrong_cd_ids, 
                                   paste0(substr(faads_combined_inf$cd_id, 1, 2), '1'),
                                   faads_combined_inf$cd_id)
# re-collapse data to reflect the change 
faads_clean <- aggregate(faads_combined_inf[, c('total_outlays', 'total_outlays_2')],
                         list(faads_combined_inf$year, faads_combined_inf$cd_id),
                         sum)

colnames(faads_clean)[1:2] <- c('year', 'cd_id')

# generate a congress variable for merging purposes
faads_clean$cong <- ifelse(faads_clean$year < 2005, 108, 
                           ifelse(faads_clean$year >= 2005 & faads_clean$year < 2007, 109,
                                  ifelse(faads_clean$year >= 2007 & faads_clean$year < 2009, 110,
                                         ifelse(faads_clean$year >= 2009 & faads_clean$year < 2011, 111,
                                                ifelse(faads_clean$year >= 2011 & faads_clean$year < 2013, 112,
                                                       ifelse(faads_clean$year >= 2013 & faads_clean$year < 2015, 113,
                                                              ifelse(faads_clean$year >= 2015 & faads_clean$year < 2017, 114, 115)))))))

### 4. Merging the roster and faads data
house_faads <- merge(faads_clean, house_clean, by = c('cd_id', 'cong'))

# remove unnecessary variables
keep_vars <- c('cd_id', 'year', 'total_outlays', 'total_outlays_2', 'id',
               'party_txt', 'ch_senior', 'leader')

house_faads_clean <- house_faads[, colnames(house_faads) %in% keep_vars]


### 5. Merging with the ideological data 
pre_redist <- house_faads_clean[(house_faads_clean$year < 2013), ]
post_redist <- house_faads_clean[(house_faads_clean$year >= 2013), ]

# obtain the list of representatives who were present in both pre and post periods
pre_id_list <- unique(pre_redist$id)
post_id_list <- unique(post_redist$id)

final_id_list <- intersect(pre_id_list, post_id_list)

# merge ideology daa with house_faads data
pre_house_mrp <- merge(pre_redist, ideology_data_00_clean, by = c('cd_id'),
                       all.x = T)

post_house_mrp <- merge(post_redist, ideology_data_10_clean, by = c('cd_id'),
                        all.x = T)

# collapse the data to the representative-level for merging 
pre_house_mrp_coll <- aggregate(pre_house_mrp[, c('mrp_mean_2000', 'pres_2008')],
                                by = list(pre_house_mrp$id), mean)
colnames(pre_house_mrp_coll)[1] <- 'id'

post_house_mrp_coll <- aggregate(post_house_mrp[, c('mrp_mean_2010', 'pres_2012')],
                                 by = list(post_house_mrp$id), mean)
colnames(post_house_mrp_coll)[1] <- 'id'

# merge them back to the original dataset
merged_df <- merge(house_faads_clean, pre_house_mrp_coll, by = c('id'),
                   all.x = T)
merged_df_2 <- merge(merged_df, post_house_mrp_coll, by = c('id'),
                     all.x = T)

# only keep representatives present in both pre and post-redistricting periods
house_df <- merged_df_2[merged_df_2$id %in%  final_id_list, ] 

# internal check for missing values
missing_mrp_2000 <- sum(is.na(house_df$mrp_mean_2000))
missing_mrp_2010 <- sum(is.na(house_df$mrp_mean_2010))

missing_df <- house_df[is.na(house_df$mrp_mean_2000), ] # confirmed; non-US state
house_df <- house_df[!is.na(house_df$mrp_mean_2000), ]


### 6. Generating additional variables for analysis
house_df$democrat <- ifelse(house_df$party_txt == 'D', 1, 0)
house_df$state <- substr(house_df$cd_id, 1, 2)
house_df$single_dist <- ifelse((house_df$state == 'AK' | house_df$state == 'MT' |
                                  house_df$state == 'ND' | house_df$state == 'SD' |
                                  house_df$state == 'WY' | house_df$state == 'VT' |
                                  house_df$state == 'DE'), 1, 0)

# remove non-US states
non_states <- c('AS', 'GU', 'DC', 'MP', 'PR', 'VI')
house_df <- house_df[!house_df$state %in% non_states, ]

# generate the electoral insecurity variable 
house_df$es_mrp <- ifelse(house_df$party_txt == 'D', (house_df$mrp_mean_2010 - 
                                                        house_df$mrp_mean_2000),
                          -(house_df$mrp_mean_2010 - house_df$mrp_mean_2000))

# Generate the "post" indicators for the regressions
house_df$post <- ifelse(house_df$year >= 2012, 1, 0)

# log the outcome variables
house_df$ln_pork <- log(house_df$total_outlays + 1)
house_df$ln_pork_2 <- log(house_df$total_outlays_2 + 1)

# checking for NAN values
pork_nas <- house_df[is.na(house_df$ln_pork), ]

# remove units with missing values
house_df <- house_df[!is.na(house_df$ln_pork), ]
house_df <- house_df[!is.na(house_df$ln_pork_2), ]

### Save the Datafile
save(house_df, file = 'Data/house_df.Rda')


###########################################
# STATS 209: Final Project Coding Part 2. 
# Purpose: Preliminary Analysis
# Last Updated: 11/30/2021
###########################################
# load necessary libraries
library(stargazer)
library(ggplot2)
library(plm)
library(lmtest)
library(dplyr)
library(xtable)
library(readxl)
library(fastDummies)
library(reshape)
### Load Main Data
load("Data/house_df.Rda")

### Additional data cleaning
# Remove at-large (untreated) individuals 
house_df_did_2 <- house_df[house_df$single_dist == 0, ]

# Convert some variables
house_df_did_2$year <- as.numeric(house_df_did_2$year)

# Only keep necessary variables
keep_vars <- c('id', 'cd_id', 'year', 'ch_senior', 'leader', 'es_mrp', 
               'democrat', 'state', 'ln_pork_2')
house_df_did_2 <- house_df_did_2[, colnames(house_df_did_2) %in% keep_vars]

### Preliminary analysis
summary(house_df_did_2$es_mrp)

# distribution of the treatment shock 
hist(unique(house_df_did$es_mrp), 
     main = "Histogram: Electoral Insecurity (Treatment)",
     col = "maroon", xlab = "Electoral Insecurity Measure (Change in District Preference")

# Add region and state population data
# Load state-region cross walk
state_region <- read_excel("Data/state_region_cw.xlsx")
state_cw <- state_region[, c("State Code", "Region")]
colnames(state_cw) <- c("state", "region")
house_df_did_2 <- merge(house_df_did_2, state_cw, by = "state")
house_df_did_2 <- dummy_cols(house_df_did_2, select_columns = "region")

# Load state population data
state_pop <- read_excel("Data/state_population.xlsx")
state_pop$state <- substr(state_pop$state, 2, nchar(state_pop$state))
state_pop <- merge(state_pop, state_region, by.x = "state", by.y = "State")

state_pop_long <- melt(state_pop, id.vars = c("state", "State Code", "Region"))

state_pop_long <- state_pop_long[c(1:1020), -c(1)]
state_pop_long$ln_pop <- log(as.numeric(state_pop_long$value))
state_pop_clean <- state_pop_long[, c("State Code", "variable", "ln_pop")]
colnames(state_pop_clean)[1:2] <- c("state", "year")

house_df_did_2 <- merge(house_df_did_2, state_pop_clean, by = c("state", "year"),
                        all.x = T)

# discretizing the treatment variable -- three different strata 
mrp_quantile <- quantile(house_df_did_2$es_mrp, probs = seq(0, 1, 1/3))

house_df_neg <- house_df_did_2[house_df_did_2$es_mrp < mrp_quantile[2], ]
house_df_pos <- house_df_did_2[house_df_did_2$es_mrp > mrp_quantile[3], ]
house_df_cont <- house_df_did_2[(house_df_did_2$es_mrp <= mrp_quantile[3]) & 
                                  (house_df_did_2$es_mrp >= mrp_quantile[2]), ]
# assign treatment indicator
house_df_neg$treat <- 1
house_df_pos$treat <- 1 
house_df_cont$treat <- 0 
df_list <- list(house_df_pos, house_df_neg, house_df_cont)

aggregate_mean <- list()
# Compute the aggregate mean across years for comparison of the trends 
for (i in (1:length(df_list))) {
  temp_mean <- aggregate(df_list[[i]][, c('ln_pork_2')], 
                         by = list(df_list[[i]]$year), mean)
  colnames(temp_mean) <- c('year', 'ln_pork_2')
  temp_mean$year <- as.numeric(temp_mean$year)
  temp_mean$group_flag <- as.factor(i)
  aggregate_mean[[i]] <- temp_mean
}
annual_mean <- bind_rows(aggregate_mean, .id = "column_label")
ggplot(data = annual_mean, aes(x = year, y = ln_pork_2, group = group_flag, 
                               colour = group_flag)) +
  geom_line(size = 1) +
  scale_colour_manual(name = 'Average Outcome (log Pork)', 
                      values = c('blue', '#00BFC4', 'red'), 
                      labels = c('Positive Shock', 'No Shock', 'Negative Shock')) +
  geom_vline(xintercept = 2012, colour ='#8B0000') +
  scale_x_continuous(breaks = seq(2003, 2018, 1)) +
  xlab('Year')+ ylab('FAADS - Federal Outlays Awareded (Log)') +
  theme_bw() +
  theme(text=element_text(family="Times New Roman", face="bold", size=14),
        legend.position = c(0.8, 0.8))  


# Pre-treatment balance
key_vars <- c('ln_pork_2', 'es_mrp')
covariates <- c('ch_senior', 'leader', 'democrat','region_Midwest', 
                'region_Northeast', 'region_South', 'region_West', 'ln_pop')
sumstat_table <- data.frame(matrix(vector(), length(key_vars) + length(covariates), 
                                   4, dimnames = list(c(), 
                                                      c('Overall', 'Positive', 
                                                        'Negative', 'Control'))))
df_list <- list(house_df_did_2, house_df_pos, house_df_neg, house_df_cont)
for (i in (1:length(df_list))) {
  temp_df <- df_list[[i]][df_list[[i]]$year < 2012, ]
  
  temp_agg <- aggregate(temp_df[, c('democrat', 'es_mrp', 'region_Midwest', 
                                    'region_Northeast', 'region_South', 
                                    'region_West')], by = list(temp_df$id), mean)
  sumstat_table[1, i] <- mean(temp_df$ln_pork_2)
  sumstat_table[2, i] <- mean(temp_agg$es_mrp)
  sumstat_table[3, i] <- mean(temp_df$ch_senior)
  sumstat_table[4, i] <- mean(temp_df$leader)
  sumstat_table[5, i] <- mean(temp_agg$democrat)
  sumstat_table[6, i] <- mean(temp_agg$region_Midwest)
  sumstat_table[7, i] <- mean(temp_agg$region_Northeast)
  sumstat_table[8, i] <- mean(temp_agg$region_South)
  sumstat_table[9, i] <- mean(temp_agg$region_West)
  sumstat_table[10, i] <- mean(temp_df$ln_pop)
  
}
xtable(sumstat_table, caption = 'Pre-Treatment Means by Treatment Group', 
       align = 'lrrrr')

test <- t.test(house_df_pos$es_mrp, house_df_neg$es_mrp)


tstat_table <- data.frame(matrix(vector(), length(key_vars) + length(covariates), 
                                 4, dimnames = list(c(), 
                                                    c('pos_t', 'pos_p', 
                                                      'neg_t', 'neg_p'))))
for (i in (1:2)) {
  temp_df <- df_list[[i+1]][df_list[[i+1]]$year < 2012, ]
  temp_cont <- df_list[[4]][df_list[[4]]$year < 2012, ]
  
  temp_agg <- aggregate(temp_df[, c('democrat', 'es_mrp', 'region_Midwest', 
                                    'region_Northeast', 'region_South', 
                                    'region_West')], by = list(temp_df$id), mean)
  temp_cont_agg <- aggregate(temp_cont[, c('democrat', 'es_mrp', 'region_Midwest', 
                                           'region_Northeast', 'region_South', 
                                           'region_West')], by = list(temp_cont$id), mean)
  
  if (i == 2) {
    i <- i +1
  }
  tstat_table[1, i] <- t.test(temp_df$ln_pork_2, temp_cont$ln_pork_2)$statistic
  tstat_table[2, i] <- t.test(temp_agg$es_mrp, temp_cont_agg$es_mrp)$statistic
  tstat_table[3, i] <- t.test(temp_df$ch_senior, temp_cont$ch_senior)$statistic
  tstat_table[4, i] <- t.test(temp_df$leader, temp_cont$leader)$statistic
  tstat_table[5, i] <- t.test(temp_agg$democrat, temp_cont_agg$democrat)$statistic
  tstat_table[6, i] <- t.test(temp_agg$region_Midwest, temp_cont_agg$region_Midwest)$statistic
  tstat_table[7, i] <- t.test(temp_agg$region_Northeast, temp_cont_agg$region_Northeast)$statistic
  tstat_table[8, i] <- t.test(temp_agg$region_South, temp_cont_agg$region_South)$statistic
  tstat_table[9, i] <- t.test(temp_agg$region_West, temp_cont_agg$region_West)$statistic
  tstat_table[10, i] <- t.test(temp_df$ln_pop, temp_cont$ln_pop)$statistic
  
  tstat_table[1, i+1] <- t.test(temp_df$ln_pork_2, temp_cont$ln_pork_2)$p.value
  tstat_table[2, i+1] <- t.test(temp_agg$es_mrp, temp_cont_agg$es_mrp)$p.value
  tstat_table[3, i+1] <- t.test(temp_df$ch_senior, temp_cont$ch_senior)$p.value
  tstat_table[4, i+1] <- t.test(temp_df$leader, temp_cont$leader)$p.value
  tstat_table[5, i+1] <- t.test(temp_agg$democrat, temp_cont_agg$democrat)$p.value
  tstat_table[6, i+1] <- t.test(temp_agg$region_Midwest, temp_cont_agg$region_Midwest)$p.value
  tstat_table[7, i+1] <- t.test(temp_agg$region_Northeast, temp_cont_agg$region_Northeast)$p.value
  tstat_table[8, i+1] <- t.test(temp_agg$region_South, temp_cont_agg$region_South)$p.value
  tstat_table[9, i+1] <- t.test(temp_agg$region_West, temp_cont_agg$region_West)$p.value
  tstat_table[10, i+1] <- t.test(temp_df$ln_pop, temp_cont$ln_pop)$p.value
  
}

xtable(tstat_table, caption = 'Pre-Treatment Means Difference-in-Means', 
       align = 'lrrrr')

# Save separate dataset for analysis
house_neg <- rbind(house_df_neg, house_df_cont)
save(house_neg, file = "Data/house_neg_treat.Rda")

house_pos <- rbind(house_df_pos, house_df_cont)
save(house_pos, file = "Data/house_pos_treat.Rda")


###########################################
# STATS 209: Final Project Coding Part 3. 
# Purpose: Panel Matching Analysis
# Last Updated: 11/29/2021
###########################################
# load necessary libraries
library(stargazer)
library(PanelMatch)
library(xtable)

# load the negative and positive versions of datasets 
load("Data/house_neg_treat.Rda")
load("Data/house_pos_treat.Rda")

# clean the data to match the required format for the package

# remove legislators missing 2011-12 data
in_sample <- unique(house_neg[house_neg$year == 2011, ]$id)
house_neg <- house_neg[house_neg$id %in% in_sample, ]

in_sample <- unique(house_pos[house_pos$year == 2011, ]$id)
house_pos <- house_pos[house_pos$id %in% in_sample, ]

# Change the unit id to be numeric/integer
unit_unique <- unique(c(unique(house_neg$id), unique(house_pos$id)))
temp_unique <- as.data.frame(unit_unique)
temp_unique$cong_id <- 1:nrow(temp_unique)
colnames(temp_unique)[1] <- "id"

# Merge the ide back to these datasets 
house_neg <- merge(house_neg, temp_unique, by = "id", all.x = T)
house_pos <- merge(house_pos, temp_unique, by = "id", all.x = T)

# remove year 2012 (the ambiguous year for FAADS matching)
house_neg <- house_neg[house_neg$year != 2012, ]
house_pos <- house_pos[house_pos$year != 2012, ]

# redefine the time variable per package requirement
house_neg$year2 <- ifelse(house_neg$year > 2012, house_neg$year - 1, house_neg$year)
house_pos$year2 <- ifelse(house_pos$year > 2012, house_pos$year - 1, house_pos$year)

# re-code the treatment indicator for the package
house_pos$treat2 <- ifelse((house_pos$treat == 1) & (house_pos$year < 2013), 0, 
                           house_pos$treat)

house_neg$treat2 <- ifelse((house_neg$treat == 1) & (house_neg$year < 2013), 0, 
                           house_neg$treat)

# Order the dataframes by time and id
house_neg$year2 <- as.integer(house_neg$year2)
house_pos$year2 <- as.integer(house_pos$year2)

# remove odd values with outcome values of zero
house_pos <- house_pos[house_pos$ln_pork_2 != 0, ]  # 1 obs. removed 
house_neg <- house_neg[house_neg$ln_pork_2 != 0, ]  # 1 obs. removed

# save the new dataframes for future use
save(house_neg, file = "Data/house_neg_treat.Rda")
save(house_pos, file = "Data/house_pos_treat.Rda")

# Preliminary distribution check
DisplayTreatment(unit.id = "cong_id", title = "", 
                 time.id = "year", legend.position = "none",
                 xlab = "Year", ylab = "Legislator (Congressional District)",
                 hide.y.axis.label = T,
                 treatment = "treat2", data = house_pos)

DisplayTreatment(unit.id = "cong_id",
                 time.id = "year", legend.position = "none",
                 xlab = "Year", ylab = "Legislator (Congressional District)",
                 hide.y.axis.label = T,
                 treatment = "treat2", data = house_neg)


### Comprehensive function to obtain results for each matching method
df_list <- list(house_neg, house_pos)

lag_num <- 5  # hyperparameter; tune later as needed
lead_num <- 0:5
covariates_matching <- c('ch_senior', 'leader', 'democrat', 'ln_pop') #,''region_Northeast', 'region_South',  'region_West'region_Midwest',

controls <- paste(covariates_matching, collapse = "+")
matching_formula <- formula(paste("~", controls))

remove_vars <- c('cd_id', 'state', 'region')

unweighted_cov_bal <- list()
unweighted_panel_est <- list()
unweighted_att <- list()

maha_cov_bal <- list()
maha_panel_est <- list()
maha_att <- list()

ps_cov_bal <- list()
ps_panel_est <- list()
ps_att <- list()

cbps_cov_bal <- list()
cbps_panel_est <- list()
cbps_att <- list()

for (i in (1:length(df_list))) {
  temp_df <- df_list[[i]]
  temp_df <- temp_df[, !colnames(temp_df) %in% remove_vars]
  # Unweighted Matching 
  PM.results.none <- PanelMatch(lag = lag_num, time.id = "year2", 
                                unit.id = "cong_id",
                                treatment = "treat2", 
                                refinement.method = "none",
                                data = temp_df, match.missing = TRUE,
                                qoi = "att", outcome.var = "ln_pork_2",
                                lead = lead_num, forbid.treatment.reversal = FALSE,
                                use.diagonal.variance.matrix = TRUE)
  
  temp_mset_none <- PM.results.none$att
  unweighted_att[[i]] <- temp_mset_none  
  get_covariate_balance(temp_mset_none, data = temp_df, 
                        covariates = covariates_matching,
                        plot = T, ylim = c(-0.7,0.7))
  temp_cov_bal_res <- get_covariate_balance(temp_mset_none, data = temp_df, 
                                            covariates = covariates_matching,
                                            plot = F)
  temp_cov_bal_res <- as.data.frame(temp_cov_bal_res)
  unweighted_cov_bal[[i]] <- colMeans(temp_cov_bal_res)
  unweighted_panel_est[[i]] <- PanelEstimate(sets = PM.results.none, 
                                             data = temp_df, 
                                             se.method = "bootstrap", 
                                             number.iterations = 1000,
                                             confidence.level = .95)
  # Mahalanobis matching
  PM.results.maha <- PanelMatch(lag = lag_num, 
                                time.id = "year2", unit.id = "cong_id",
                                treatment = "treat2", 
                                refinement.method = "mahalanobis",
                                data = temp_df, match.missing = TRUE,
                                covs.formula = matching_formula,
                                size.match = 5, qoi = "att" , 
                                outcome.var = "ln_pork_2",
                                lead = lead_num, forbid.treatment.reversal = FALSE,
                                use.diagonal.variance.matrix = TRUE)
  temp_mset_maha <- PM.results.maha$att
  maha_att[[i]] <- temp_mset_maha
  temp_cov_bal_res <- get_covariate_balance(temp_mset_maha, data = temp_df,
                                            covariates = covariates_matching,
                                            plot = F)
  maha_cov_bal[[i]] <- colMeans(temp_cov_bal_res)
  maha_panel_est[[i]] <- PanelEstimate(sets = PM.results.maha, 
                                       data = temp_df, 
                                       se.method = "bootstrap", 
                                       number.iterations = 1000,
                                       confidence.level = .95)
  
  # Propensity Score Matching
  PM.results.ps <- PanelMatch(lag = lag_num, 
                              time.id = "year2", unit.id = "cong_id",
                              treatment = "treat2", 
                              refinement.method = "ps.match",
                              data = temp_df, match.missing = FALSE, 
                              listwise.delete = TRUE,
                              covs.formula = matching_formula,
                              size.match = 5, qoi = "att", 
                              outcome.var = "ln_pork_2",
                              lead = lead_num, forbid.treatment.reversal = FALSE)
  
  temp_mset_ps <- PM.results.ps$att
  ps_att[[i]] <- temp_mset_ps
  temp_cov_bal_res <- get_covariate_balance(temp_mset_ps, data = temp_df,
                                            covariates = covariates_matching,
                                            plot = F)
  ps_cov_bal[[i]] <- colMeans(temp_cov_bal_res)
  ps_panel_est[[i]] <- PanelEstimate(sets = PM.results.ps, 
                                     data = temp_df, 
                                     se.method = "bootstrap", 
                                     number.iterations = 1000,
                                     confidence.level = .95)
  
  # CBPS Score Matching 
  PM.results.cbps <- PanelMatch(lag = lag_num, 
                                time.id = "year2", unit.id = "cong_id",
                                treatment = "treat2", 
                                refinement.method = "CBPS.match",
                                data = temp_df, match.missing = FALSE, 
                                listwise.delete = TRUE,
                                covs.formula = matching_formula,
                                size.match = 5, qoi = "att", 
                                outcome.var = "ln_pork_2",
                                lead = lead_num, forbid.treatment.reversal = FALSE)
  
  temp_mset_cbps <- PM.results.cbps$att
  cbps_att[[i]] <- temp_mset_cbps
  temp_cov_bal_res <- get_covariate_balance(temp_mset_cbps, data = temp_df,
                                            covariates = covariates_matching,
                                            plot = F)
  cbps_cov_bal[[i]] <- colMeans(temp_cov_bal_res)
  cbps_panel_est[[i]] <- PanelEstimate(sets = PM.results.cbps, 
                                       data = temp_df, 
                                       se.method = "bootstrap", 
                                       number.iterations = 1000,
                                       confidence.level = .95)
  
}

# Combine the results in a table 
method_list <- list(unweighted_cov_bal, maha_cov_bal, ps_cov_bal, cbps_cov_bal)
matching_bal_neg <- data.frame(matrix(vector(), 4, 4, 
                                      dimnames = list(c(), c('unweighted', 'maha', 
                                                             'ps', 'cbps'))))
matching_bal_pos <- data.frame(matrix(vector(), 4, 4, 
                                      dimnames = list(c(), c('unweighted', 'maha', 
                                                             'ps', 'cbps'))))
matching_bal <- list(matching_bal_neg, matching_bal_pos)

for (i in (1:length(matching_bal))) {
  for (j in (1:length(method_list))) {
    matching_bal[[i]][, c(j)] <- method_list[[j]][[i]]
  }
}

# Export the results for the report
xtable(matching_bal[[1]])
xtable(matching_bal[[2]])

# Sample covariate balance figures 
for (i in (1:length(df_list))) {
  get_covariate_balance(unweighted_att[[i]], data = df_list[[i]], 
                        covariates = covariates_matching,
                        plot = T, ylim = c(-1, 1), lwd = 2)
  get_covariate_balance(maha_att[[i]], data = df_list[[i]], 
                        covariates = covariates_matching,
                        plot = T, ylim = c(-1, 1), lwd = 2)
  get_covariate_balance(cbps_att[[i]], data = df_list[[i]], 
                        covariates = covariates_matching,
                        plot = T, ylim = c(-1, 1), lwd = 2)
}


# Obtain panel estimates for each method 
panel_est_list <- list(unweighted_panel_est, maha_panel_est, ps_panel_est,
                       cbps_panel_est)
method_names <- c("unweighted", "maha", "ps", "cbps")
matching_pe_neg <- data.frame(matrix(vector(), 4, 4, 
                                     dimnames = list(c(), c('estimate', 'se', 
                                                            'CI_L', 'CI_U'))))
matching_pe_pos <- data.frame(matrix(vector(), 4, 4, 
                                     dimnames = list(c(), c('estimate', 'se', 
                                                            'CI_L', 'CI_U'))))
matching_pe <- list(matching_pe_neg, matching_pe_pos)
for (i in (1:length(matching_pe))) {
  for (j in (1:length(panel_est_list))) {
    
    matching_pe[[i]][j, 1] <- mean(panel_est_list[[j]][[i]]$estimates)
    matching_pe[[i]][j, 2] <- mean(panel_est_list[[j]][[i]]$standard.error)
    matching_pe[[i]][j, 3] <- mean(summary(panel_est_list[[j]][[i]])$summary[, 3])
    matching_pe[[i]][j, 4] <- mean(summary(panel_est_list[[j]][[i]])$summary[, 4])
  }
}

# Export results 
xtable(matching_pe[[1]], digits = 3, align = "lrrrr")
xtable(matching_pe[[2]], digits = 3, align = "lrrrr")

# obtain baseline estimates (no matching, conventional DID)
covariates <- c('ch_senior', 'leader')
reg_covariates <- paste(covariates, collapse = " + ")

diff_fe_cont_reg <- function(x, dep_var, indep_var, controls){
  diff_fe_cont_reg_formula <- formula(paste(dep_var, '~', indep_var, '+', controls))
  result <- plm(diff_fe_cont_reg_formula, data = x, 
                index = c('cong_id', 'year'), 
                model = 'within', effect = 'twoways')
  result
}

dep_vars <- c('ln_pork_2')

reg_results <- list()
reg_se <- list()

for (i in (1:length(df_list))) {
  reg_results[[i]] <- diff_fe_cont_reg(df_list[[i]], dep_var = dep_vars,
                                       indep_var = 'treat2', 
                                       controls = reg_covariates)
  reg_se[[i]] <- coeftest(reg_results[[i]], 
                          vcov = vcovHC(reg_results[[i]], type = 'HC1', 
                                        cluster = 'group'))
}

# obtain confidence intervals for each group
summary(reg_results[[1]])
confint(reg_results[[1]])
summary(reg_results[[2]])
confint(reg_results[[2]])

# Obtain covariate balance without matching 
covariates_balance <- c("ch_senior", "leader", "democrat", "ln_pop")
var_balance_list <- list()
for (i in (1:length(df_list))) {
  temp_df <- df_list[[i]]
  var_balance <- c()
  for (j in (1:length(covariates_balance))) {
    
    treat_mean <- mean(temp_df[temp_df$treat == 1,][[covariates_balance[j]]])
    control_mean <- mean(temp_df[temp_df$treat == 0,][[covariates_balance[j]]])
    var_balance[j] <- treat_mean - control_mean
  }
  var_balance_list[[i]] <- var_balance
}
scale(var_balance_list[[1]])  # Negative treatment group
scale(var_balance_list[[2]])  # Positive treatment group


###########################################
# STATS 209: Final Project Coding Part 4. 
# Purpose: Synthetic Control Analysis
# Last Updated: 11/30/2021
###########################################

# Load necessary packages
library(stargazer)
library(readxl)
library(fastDummies)
library(reshape)
library(plm)
library(Synth)
library(ggplot2)
library(gsynth)
library(panelView)

# Load datasets
load("Data/house_neg_treat.Rda")
load("Data/house_pos_treat.Rda")

### 1. Vanilla synthetic control method 
# Define controls 
covariates <- c('ch_senior', 'leader', 'ln_pop')

# define function to obtain balanced data for synth function 
get_synth_df <- function(temp_df, original_df, treat_id, treat_year){
  temp_min_year <- min(temp_df$year2)
  temp_max_year <- max(temp_df$year2)
  temp_unique_years <- unique(temp_df$year2)
  
  # check that there is no odd gap in the data 
  if ((temp_max_year - temp_min_year + 1) > length(temp_unique_years)) {
    temp_seq <- seq(temp_min_year, temp_max_year)
    pre_missing_years <- c()
    post_missing_years <- c()
    for (m in (1:length(temp_seq))) {
      if (!(temp_seq[m] %in% temp_unique_years) && temp_seq[m] < treat_year + 1) {
        pre_missing_years <- c(pre_missing_years, temp_seq[m])
      } else if (!(temp_seq[m] %in% temp_unique_years) && temp_seq[m] > treat_year + 1) {
        post_missing_years <- c(post_missing_years, temp_seq[m])
      }
    }
    
    if ((length(pre_missing_years)>0)&& (length(post_missing_years) == 0)) {
      temp_min_year <- tail(pre_missing_years, n = 1) + 1
    } else if ((length(pre_missing_years)==0)&& (length(post_missing_years)>0)) {
      temp_max_year <- tail(post_missing_years, n = 1) - 1
    } else if ((length(pre_missing_years)>0)&& (length(post_missing_years)>0)){
      temp_min_year <- tail(pre_missing_years, n = 1) + 1
      temp_max_year <- tail(post_missing_years, n = 1) - 1
    }
    temp_unique_years <- seq(temp_min_year, temp_max_year)  
  }
  
  temp_controls <- original_df[original_df$treat == 0, ]
  temp_control_ids <- unique(temp_controls$cong_id)
  keep_controls <- c()
  for (k in (1:length(temp_control_ids))) {
    temp_subset <- temp_controls[temp_controls$cong_id == temp_control_ids[k],]
    temp_subset_unique_years <- unique(temp_subset$year2)
    counter <- sum(temp_unique_years %in% temp_subset_unique_years)
    if (counter == length(temp_unique_years)) {
      keep_controls <- c(keep_controls, temp_control_ids[k])
    }
  }
  temp_df <- rbind(temp_df, original_df[original_df$cong_id %in% keep_controls, ])
  temp_pdf <- pdata.frame(temp_df, index = c('cong_id', 'year'))
  temp_pdf_bal <- make.pbalanced(temp_pdf, balance.type =c("shared.time"))
  temp_pdf_bal <- as.data.frame(temp_pdf_bal)
  
  dataprep.out <- dataprep(foo = temp_pdf_bal,
                           predictors = covariates,
                           predictors.op = 'mean',
                           dependent = 'ln_pork_2',
                           unit.variable = 'cong_id',
                           time.variable = 'year2',
                           treatment.identifier = treat_id,
                           controls.identifier = keep_controls,
                           time.predictors.prior = c(temp_min_year:treat_year),
                           time.optimize.ssr = c(temp_min_year:treat_year),
                           time.plot = temp_min_year:temp_max_year)
  return(dataprep.out)
}

# store the covariate balance results in the list
df_list <- list(house_neg, house_pos)

sc_cov_res <- list()
sc_actual_res <- list()
sc_counter_res <- list()

cutpoint = 2011  # the last year of control 

for (i in (1:length(df_list))) {
  temp_df_1 <- df_list[[i]]
  treated_list <- unique(temp_df_1[temp_df_1$treat == 1, ]$cong_id)
  sc_cov_list <- list()
  sc_Y1 <- data.frame(matrix(vector(), 0, 
                             2, dimnames = list(c(), 
                                                c('year', 'outcome'))))
  sc_Y0 <- data.frame(matrix(vector(), 0, 2, dimnames = list(c(), 
                                                             c('year', 'outcome'))))
  
  for (j in (1:length(treated_list))) {
    temp_df_2 <- temp_df_1[temp_df_1$cong_id == treated_list[j], ]
    temp_dataprep <- get_synth_df(temp_df = temp_df_2, original_df = temp_df_1, 
                                  treat_id = treated_list[j], treat_year = cutpoint)
    
    temp_synth <- synth(temp_dataprep, optimxmethod = 'All')
    temp_synth_table <- synth.tab(synth.res = temp_synth, 
                                  dataprep.res = temp_dataprep)
    
    temp_y1 <- cbind(temp_dataprep[["tag"]][["time.plot"]], temp_dataprep$Y1plot)
    colnames(temp_y1) <- c("year", "outcome")
    
    temp_y0 <- cbind(temp_dataprep[["tag"]][["time.plot"]], 
                     temp_dataprep$Y0plot %*% temp_synth$solution.w)
    colnames(temp_y0) <- c("year", "outcome")
    
    sc_Y1 <- rbind(sc_Y1, as.data.frame(temp_y1))
    sc_Y0 <- rbind(sc_Y0, as.data.frame(temp_y0))
    
    sc_cov_list[[j]] <- temp_synth_table$tab.pred
    
  }
  sc_actual_res[[i]] <- aggregate(sc_Y1$outcome, by = list(sc_Y1$year), mean)
  sc_counter_res[[i]] <- aggregate(sc_Y0$outcome, by = list(sc_Y0$year), mean)
  sc_cov_res[[i]] <- sc_cov_list
}

path.plot(dataprep.res = temp_dataprep, synth.res = temp_synth)

# plot the average results for comparison
plot_directory <- 'Output'
for (i in (1:length(sc_actual_res))) {
  temp_actual_res <- sc_actual_res[[i]]
  temp_counter_res <- sc_counter_res[[i]]
  
  annual_average <- temp_actual_res
  annual_average$treat <- 1
  counter_average <- temp_counter_res
  counter_average$treat <- 0
  annual_average <- rbind(annual_average, counter_average)
  colnames(annual_average) <- c("year", "outcome", "treat")
  annual_average$treat <- as.factor(annual_average$treat)
  
  # obtain panel att estimates
  sc_post_treat <- annual_average[annual_average$year > 2011, ]
  sc_post_actual <- sc_post_treat[sc_post_treat$treat== 1, ]
  sc_post_counter <- sc_post_treat[sc_post_treat$treat== 0, ]
  sc_att_est <- mean(sc_post_actual$outcome - sc_post_counter$outcome)
  print(sc_att_est)
  
  # generate figures
  plot_name <- paste0("sc_annual_avg_", i, ".png")
  ggplot(annual_average, aes(x = year, y = outcome, color = treat,
                             linetype = treat)) + 
    geom_line(size=2) + 
    labs(x = "Year", 
         y = "FAADS (log)", color = "", linetype = "") +
    scale_color_manual(labels = c("Synthetic", "Actual"), values = c("steelblue1", "navy")) +
    scale_linetype_manual(labels = "", values = c("twodash", "solid")) +
    theme_bw() + 
    theme(legend.position = "bottom", text=element_text(size=14,  family="serif")) +
    geom_vline(xintercept = 2012, colour ='grey69', linetype="solid", size=2) + 
    guides(linetype = F)
  ggsave(plot_name, width = 13, height = 8, path = plot_directory)
  
}

# obtain covariate balance estimates 
sc_cov_bal_list <- list()
for (i in (1:length(sc_cov_res))) {
  temp_cov_res <- sc_cov_res[[i]]
  sc_cov_bal <- data.frame(matrix(vector(), 0, 4, 
                                  dimnames = list(c(), c('treated', 'counter', 
                                                         'diff', 'var_flag'))))
  for (j in (1:length(temp_cov_res))) {
    temp_res <- temp_cov_res[[j]]
    temp_res <- as.data.frame(temp_res)
    temp_res <- temp_res[ , -c(3)] 
    temp_res$diff <- temp_res$Treated - temp_res$Synthetic
    temp_res$var_flag <- 1:4
    sc_cov_bal <- rbind(sc_cov_bal, temp_res)
  }
  sc_cov_bal_list[[i]] <- sc_cov_bal
}

test <- sc_cov_bal_list[[1]]
test2 <- test[test$var_flag == 1, ]
test2$stand_diff <- (test2$diff - mean(test2$diff))/sd(test2$diff)

sc_cov_bal_res <- list()
for (i in (1:length(sc_cov_bal_list))) {
  temp_subset <- sc_cov_bal_list[[i]]
  temp_sc_bal <- c()
  for (j in (1:4)) {
    temp_var <- temp_subset[temp_subset$var_flag == j, ]
    temp_sc_bal <- c(temp_sc_bal, mean(temp_var$diff))
  }
  sc_cov_bal_res[[i]] <- temp_sc_bal
}

standard_cov_bal_neg <- (sc_cov_bal_res[[1]] - mean(sc_cov_bal_res[[1]]))/
  sd(sc_cov_bal_res[[1]])

standard_cov_bal_pos <- (sc_cov_bal_res[[2]] - mean(sc_cov_bal_res[[2]]))/
  sd(sc_cov_bal_res[[2]])

### 2. Generalized Synthetic Control (Xu, 2017)
# Data visualization
sample_df <- data(gsynth)
panelView(Y ~ D, data = simdata,  index = c("id","time"), pre.post = TRUE) 

panelView(ln_pork_2 ~ treat2, data = house_pos,  
          index = c("cong_id","year"), 
          pre.post = TRUE, 
          main = "",
          xlab = "Year", ylab = "Legislator (Congressional District)") 

panelView(ln_pork_2 ~ treat2, data = house_neg,  
          index = c("cong_id","year"), 
          pre.post = TRUE, 
          main = "",
          xlab = "Year", ylab = "Legislator (Congressional District)")

gsynth_res <- list()
for (i in (1:length(df_list))) {
  temp_df <- df_list[[i]]
  gsynth_res[[i]] <- gsynth(ln_pork_2 ~ treat2 + ch_senior + leader, 
                            data = temp_df, 
                            index = c("cong_id","year2"), 
                            force = "two-way", 
                            min.T0 = 5, 
                            CV = TRUE, r = c(0, 3), se = TRUE, inference = "parametric", 
                            nboots = 1000, parallel = FALSE)
}

# produce plots of average outcomes
plot(gsynth_res[[1]], type = "counterfactual", raw = "none", main = "")
plot(gsynth_res[[2]], type = "counterfactual", raw = "none", main = "")

# produce plots of gaps 
plot(gsynth_res[[1]], type = "gap", raw = "none", main = "", cex = 2)
plot(gsynth_res[[2]], type = "gap", raw = "none", main = "")


# obtaining ATT, SE, and CI
for (i in (1:length(df_list))) {
  print(gsynth_res[[i]]$est.avg)
}


# constructing the covariate balance based on the implied weights (better with a function)
neg_control_ids <- gsynth_res[[1]][["id.co"]]
neg_treat_ids <- gsynth_res[[1]][["id.tr"]]

neg_control_covariates <- house_neg[(house_neg$cong_id %in% neg_control_ids) &
                                      (house_neg$year < 2012),
                                    c(covariates, "ln_pork_2", "cong_id")]

neg_treat_covariates <- house_neg[(house_neg$cong_id %in% neg_treat_ids) &
                                    (house_neg$year < 2012),
                                  c(covariates, "ln_pork_2", "cong_id")]

agg_neg_cont_cov <- aggregate(list(neg_control_covariates$ln_pork_2, 
                                   neg_control_covariates$leader), 
                              by = list(neg_control_covariates$cong_id), mean)
colnames(agg_neg_cont_cov) <- c("cong_id", "ln_pork_2", "leader")

agg_neg_treat_cov <- aggregate(list(neg_treat_covariates$ln_pork_2, 
                                    neg_treat_covariates$leader), 
                               by = list(neg_treat_covariates$cong_id), mean)
colnames(agg_neg_treat_cov) <- c("cong_id", "ln_pork_2", "leader")

gsynth_weights_neg <- gsynth_res[[1]][["wgt.implied"]]
cont_ch_senior <- as.matrix(agg_neg_cont_cov$ln_pork_2)
counter_ch <- t(gsynth_weights_neg) %*% cont_ch_senior
agg_neg_treat_cov$ch_senior_counter <- counter_ch
agg_neg_treat_cov$ch_senior_counter <- agg_neg_treat_cov$ch_senior_counter + gsynth_res[[1]][["mu"]]


###########################################
# STATS 209: Final Project Coding Part 4. 
# Purpose: Synthetic Control Analysis
# Last Updated: 11/30/2021
###########################################

# Load necessary packages
library(stargazer)
library(readxl)
library(fastDummies)
library(reshape)
library(plm)
library(Synth)
library(ggplot2)
library(gsynth)
library(panelView)

# Load datasets
load("Data/house_neg_treat.Rda")
load("Data/house_pos_treat.Rda")

### 1. Vanilla synthetic control method 
# Define controls 
covariates <- c('ch_senior', 'leader', 'ln_pop')

# define function to obtain balanced data for synth function 
get_synth_df <- function(temp_df, original_df, treat_id, treat_year){
  temp_min_year <- min(temp_df$year2)
  temp_max_year <- max(temp_df$year2)
  temp_unique_years <- unique(temp_df$year2)
  
  # check that there is no odd gap in the data 
  if ((temp_max_year - temp_min_year + 1) > length(temp_unique_years)) {
    temp_seq <- seq(temp_min_year, temp_max_year)
    pre_missing_years <- c()
    post_missing_years <- c()
    for (m in (1:length(temp_seq))) {
      if (!(temp_seq[m] %in% temp_unique_years) && temp_seq[m] < treat_year + 1) {
        pre_missing_years <- c(pre_missing_years, temp_seq[m])
      } else if (!(temp_seq[m] %in% temp_unique_years) && temp_seq[m] > treat_year + 1) {
        post_missing_years <- c(post_missing_years, temp_seq[m])
      }
    }
    
    if ((length(pre_missing_years)>0)&& (length(post_missing_years) == 0)) {
      temp_min_year <- tail(pre_missing_years, n = 1) + 1
    } else if ((length(pre_missing_years)==0)&& (length(post_missing_years)>0)) {
      temp_max_year <- tail(post_missing_years, n = 1) - 1
    } else if ((length(pre_missing_years)>0)&& (length(post_missing_years)>0)){
      temp_min_year <- tail(pre_missing_years, n = 1) + 1
      temp_max_year <- tail(post_missing_years, n = 1) - 1
    }
    temp_unique_years <- seq(temp_min_year, temp_max_year)  
  }
  
  temp_controls <- original_df[original_df$treat == 0, ]
  temp_control_ids <- unique(temp_controls$cong_id)
  keep_controls <- c()
  for (k in (1:length(temp_control_ids))) {
    temp_subset <- temp_controls[temp_controls$cong_id == temp_control_ids[k],]
    temp_subset_unique_years <- unique(temp_subset$year2)
    counter <- sum(temp_unique_years %in% temp_subset_unique_years)
    if (counter == length(temp_unique_years)) {
      keep_controls <- c(keep_controls, temp_control_ids[k])
    }
  }
  temp_df <- rbind(temp_df, original_df[original_df$cong_id %in% keep_controls, ])
  temp_pdf <- pdata.frame(temp_df, index = c('cong_id', 'year'))
  temp_pdf_bal <- make.pbalanced(temp_pdf, balance.type =c("shared.time"))
  temp_pdf_bal <- as.data.frame(temp_pdf_bal)
  
  dataprep.out <- dataprep(foo = temp_pdf_bal,
                           predictors = covariates,
                           predictors.op = 'mean',
                           dependent = 'ln_pork_2',
                           unit.variable = 'cong_id',
                           time.variable = 'year2',
                           treatment.identifier = treat_id,
                           controls.identifier = keep_controls,
                           time.predictors.prior = c(temp_min_year:treat_year),
                           time.optimize.ssr = c(temp_min_year:treat_year),
                           time.plot = temp_min_year:temp_max_year)
  return(dataprep.out)
}

# store the covariate balance results in the list
df_list <- list(house_neg, house_pos)

sc_cov_res <- list()
sc_actual_res <- list()
sc_counter_res <- list()

cutpoint = 2011  # the last year of control 

for (i in (1:length(df_list))) {
  temp_df_1 <- df_list[[i]]
  treated_list <- unique(temp_df_1[temp_df_1$treat == 1, ]$cong_id)
  sc_cov_list <- list()
  sc_Y1 <- data.frame(matrix(vector(), 0, 
                             2, dimnames = list(c(), 
                                                c('year', 'outcome'))))
  sc_Y0 <- data.frame(matrix(vector(), 0, 2, dimnames = list(c(), 
                                                             c('year', 'outcome'))))
  
  for (j in (1:length(treated_list))) {
    temp_df_2 <- temp_df_1[temp_df_1$cong_id == treated_list[j], ]
    temp_dataprep <- get_synth_df(temp_df = temp_df_2, original_df = temp_df_1, 
                                  treat_id = treated_list[j], treat_year = cutpoint)
    
    temp_synth <- synth(temp_dataprep, optimxmethod = 'All')
    temp_synth_table <- synth.tab(synth.res = temp_synth, 
                                  dataprep.res = temp_dataprep)
    
    temp_y1 <- cbind(temp_dataprep[["tag"]][["time.plot"]], temp_dataprep$Y1plot)
    colnames(temp_y1) <- c("year", "outcome")
    
    temp_y0 <- cbind(temp_dataprep[["tag"]][["time.plot"]], 
                     temp_dataprep$Y0plot %*% temp_synth$solution.w)
    colnames(temp_y0) <- c("year", "outcome")
    
    sc_Y1 <- rbind(sc_Y1, as.data.frame(temp_y1))
    sc_Y0 <- rbind(sc_Y0, as.data.frame(temp_y0))
    
    sc_cov_list[[j]] <- temp_synth_table$tab.pred
    
  }
  sc_actual_res[[i]] <- aggregate(sc_Y1$outcome, by = list(sc_Y1$year), mean)
  sc_counter_res[[i]] <- aggregate(sc_Y0$outcome, by = list(sc_Y0$year), mean)
  sc_cov_res[[i]] <- sc_cov_list
}

path.plot(dataprep.res = temp_dataprep, synth.res = temp_synth)

# plot the average results for comparison
plot_directory <- 'Output'
for (i in (1:length(sc_actual_res))) {
  temp_actual_res <- sc_actual_res[[i]]
  temp_counter_res <- sc_counter_res[[i]]
  
  annual_average <- temp_actual_res
  annual_average$treat <- 1
  counter_average <- temp_counter_res
  counter_average$treat <- 0
  annual_average <- rbind(annual_average, counter_average)
  colnames(annual_average) <- c("year", "outcome", "treat")
  annual_average$treat <- as.factor(annual_average$treat)
  
  # obtain panel att estimates
  sc_post_treat <- annual_average[annual_average$year > 2011, ]
  sc_post_actual <- sc_post_treat[sc_post_treat$treat== 1, ]
  sc_post_counter <- sc_post_treat[sc_post_treat$treat== 0, ]
  sc_att_est <- mean(sc_post_actual$outcome - sc_post_counter$outcome)
  print(sc_att_est)
  
  # generate figures
  plot_name <- paste0("sc_annual_avg_", i, ".png")
  ggplot(annual_average, aes(x = year, y = outcome, color = treat,
                             linetype = treat)) + 
    geom_line(size=2) + 
    labs(x = "Year", 
         y = "FAADS (log)", color = "", linetype = "") +
    scale_color_manual(labels = c("Synthetic", "Actual"), values = c("steelblue1", "navy")) +
    scale_linetype_manual(labels = "", values = c("twodash", "solid")) +
    theme_bw() + 
    theme(legend.position = "bottom", text=element_text(size=14,  family="serif")) +
    geom_vline(xintercept = 2012, colour ='grey69', linetype="solid", size=2) + 
    guides(linetype = F)
  ggsave(plot_name, width = 13, height = 8, path = plot_directory)
  
}

# obtain covariate balance estimates 
sc_cov_bal_list <- list()
for (i in (1:length(sc_cov_res))) {
  temp_cov_res <- sc_cov_res[[i]]
  sc_cov_bal <- data.frame(matrix(vector(), 0, 4, 
                                  dimnames = list(c(), c('treated', 'counter', 
                                                         'diff', 'var_flag'))))
  for (j in (1:length(temp_cov_res))) {
    temp_res <- temp_cov_res[[j]]
    temp_res <- as.data.frame(temp_res)
    temp_res <- temp_res[ , -c(3)] 
    temp_res$diff <- temp_res$Treated - temp_res$Synthetic
    temp_res$var_flag <- 1:4
    sc_cov_bal <- rbind(sc_cov_bal, temp_res)
  }
  sc_cov_bal_list[[i]] <- sc_cov_bal
}

test <- sc_cov_bal_list[[1]]
test2 <- test[test$var_flag == 1, ]
test2$stand_diff <- (test2$diff - mean(test2$diff))/sd(test2$diff)

sc_cov_bal_res <- list()
for (i in (1:length(sc_cov_bal_list))) {
  temp_subset <- sc_cov_bal_list[[i]]
  temp_sc_bal <- c()
  for (j in (1:4)) {
    temp_var <- temp_subset[temp_subset$var_flag == j, ]
    temp_sc_bal <- c(temp_sc_bal, mean(temp_var$diff))
  }
  sc_cov_bal_res[[i]] <- temp_sc_bal
}

standard_cov_bal_neg <- (sc_cov_bal_res[[1]] - mean(sc_cov_bal_res[[1]]))/
  sd(sc_cov_bal_res[[1]])

standard_cov_bal_pos <- (sc_cov_bal_res[[2]] - mean(sc_cov_bal_res[[2]]))/
  sd(sc_cov_bal_res[[2]])

### 2. Generalized Synthetic Control (Xu, 2017)

# Data visualization
sample_df <- data(gsynth)
panelView(Y ~ D, data = simdata,  index = c("id","time"), pre.post = TRUE) 

panelView(ln_pork_2 ~ treat2, data = house_pos,  
          index = c("cong_id","year"), 
          pre.post = TRUE, 
          main = "",
          xlab = "Year", ylab = "Legislator (Congressional District)") 

panelView(ln_pork_2 ~ treat2, data = house_neg,  
          index = c("cong_id","year"), 
          pre.post = TRUE, 
          main = "",
          xlab = "Year", ylab = "Legislator (Congressional District)")

gsynth_res <- list()
for (i in (1:length(df_list))) {
  temp_df <- df_list[[i]]
  gsynth_res[[i]] <- gsynth(ln_pork_2 ~ treat2 + ch_senior + leader, 
                            data = temp_df, 
                            index = c("cong_id","year2"), 
                            force = "two-way", 
                            min.T0 = 5, 
                            CV = TRUE, r = c(0, 3), se = TRUE, inference = "parametric", 
                            nboots = 1000, parallel = FALSE)
}

# produce plots of average outcomes
plot(gsynth_res[[1]], type = "counterfactual", raw = "none", main = "")
plot(gsynth_res[[2]], type = "counterfactual", raw = "none", main = "")

# produce plots of gaps 
plot(gsynth_res[[1]], type = "gap", raw = "none", main = "", cex = 2)
plot(gsynth_res[[2]], type = "gap", raw = "none", main = "")


# obtaining ATT, SE, and CI
for (i in (1:length(df_list))) {
  print(gsynth_res[[i]]$est.avg)
}


# constructing the covariate balance based on the implied weights (better with a function)
neg_control_ids <- gsynth_res[[1]][["id.co"]]
neg_treat_ids <- gsynth_res[[1]][["id.tr"]]

neg_control_covariates <- house_neg[(house_neg$cong_id %in% neg_control_ids) &
                                      (house_neg$year < 2012),
                                    c(covariates, "ln_pork_2", "cong_id")]

neg_treat_covariates <- house_neg[(house_neg$cong_id %in% neg_treat_ids) &
                                    (house_neg$year < 2012),
                                  c(covariates, "ln_pork_2", "cong_id")]

agg_neg_cont_cov <- aggregate(list(neg_control_covariates$ln_pork_2, 
                                   neg_control_covariates$leader), 
                              by = list(neg_control_covariates$cong_id), mean)
colnames(agg_neg_cont_cov) <- c("cong_id", "ln_pork_2", "leader")

agg_neg_treat_cov <- aggregate(list(neg_treat_covariates$ln_pork_2, 
                                    neg_treat_covariates$leader), 
                               by = list(neg_treat_covariates$cong_id), mean)
colnames(agg_neg_treat_cov) <- c("cong_id", "ln_pork_2", "leader")

gsynth_weights_neg <- gsynth_res[[1]][["wgt.implied"]]
cont_ch_senior <- as.matrix(agg_neg_cont_cov$ln_pork_2)
counter_ch <- t(gsynth_weights_neg) %*% cont_ch_senior
agg_neg_treat_cov$ch_senior_counter <- counter_ch
agg_neg_treat_cov$ch_senior_counter <- agg_neg_treat_cov$ch_senior_counter + gsynth_res[[1]][["mu"]]