rm(list = ls()) #clear workspace
library(dplyr)
library(readxl)
library(ggplot2)


#########################
# Data input
#########################
setwd("C:/Users/XSIU/OneDrive/Groningen(online)/MADS/2a/digital marketing intelligence/tutorial/assignment 1")
setwd("E:/onedrive/Groningen(online)/MADS/2a/digital marketing intelligence/tutorial/assignment 1")

# Setting the file path
file_path <- "rug_wannagive_case.xlsx"

# loading sheet
wannagive_logging <- read_excel(file_path, sheet = "wannagive_logging")
coupons_logging <- read_excel(file_path, sheet = "coupons_logging")
transactions <- read_excel(file_path, sheet = "transactions")
customer_info <- read_excel(file_path, sheet = "customer_info_20241202")

# data check
head(wannagive_logging)
head(coupons_logging)
head(transactions)
head(customer_info)

#########################
# Statistical analysis
#########################
summary(wannagive_logging)
summary(coupons_logging)
summary(transactions)
summary(customer_info)

colSums(is.na(wannagive_logging))
colSums(is.na(coupons_logging))
colSums(is.na(transactions))
colSums(is.na(customer_info))

#########################
# data cleaning
#########################
# Processing the timestamp column (first two tables)
wannagive_logging <- wannagive_logging %>%
  mutate(date = as.Date(substr(timestamp, 1, 10)))

coupons_logging <- coupons_logging %>%
  mutate(date = as.Date(substr(timestamp, 1, 10)))

# Process the order_local_timestamp column (transactions table)
transactions <- transactions %>%
  mutate(date = as.Date(substr(order_local_timestamp, 1, 10)))

# transform type of streak in wannagive_logging
wannagive_logging <- wannagive_logging %>% 
  mutate(currentStreakValue = as.numeric(currentStreakValue))
wannagive_logging <- wannagive_logging %>% 
  mutate(currentStreakValue = ifelse(is.na(currentStreakValue), 0, currentStreakValue))

# weird participate check (participate before 2024-12-02)
weird_participate <- wannagive_logging %>% filter(event=="participate",date<="2024-12-02")
weird_participate
# remove weird participate 
weird_ids <- weird_participate$uniqueCustomerId
wannagive_logging <- wannagive_logging %>%
  filter(!uniqueCustomerId %in% weird_ids)

##### error: participate but no enter
problematic_customers <- wannagive_logging %>%
  group_by(uniqueCustomerId) %>%
  summarise(
    enter_count = sum(event == "enter", na.rm = TRUE),
    participate_count = sum(event == "participate", na.rm = TRUE)
  ) %>%
  filter(enter_count == 0 & participate_count > 0)
problematic_customers

wannagive_logging <- wannagive_logging %>% filter(uniqueCustomerId!="P6gojXAQp7g75AIfuSYeHjJMt4I=")
coupons_logging <- coupons_logging %>% filter(uniqueCustomerId!="P6gojXAQp7g75AIfuSYeHjJMt4I=")
customer_info <- customer_info %>% filter(unique_customer_id!="P6gojXAQp7g75AIfuSYeHjJMt4I=")
transactions <- transactions %>% filter(unique_customer_id!="P6gojXAQp7g75AIfuSYeHjJMt4I=")

# claim check
claim_check <- wannagive_logging %>% filter(action=="claimed") %>% group_by(uniqueCustomerId,card) %>% 
  summarize(n())
claim_check
# Remove duplicate claimed records for each uniqueCustomerId for each card
wannagive_logging <- wannagive_logging %>%
  filter(action == "claimed") %>%               
  distinct(uniqueCustomerId, card, .keep_all = TRUE) %>%  
  bind_rows(
    wannagive_logging %>% filter(action != "claimed")      
  )

# select check
select_check <- wannagive_logging %>% filter(action=="selected") %>% group_by(uniqueCustomerId,card) %>% 
  summarize(n())
select_check
 
# claim & participate & enter check
claim_participate_check <- wannagive_logging %>% filter(!card %in% c(
                                                                    "🔥€5 korting bij een minimale besteding van €0",
                                                                    "🔥🔥€10 korting bij een minimale besteding van €30",
                                                                    "🔥🔥🔥€15 korting bij een minimale besteding van €60",
                                                                    "🔥🔥🔥🔥€20 korting bij een minimale besteding van €80",
                                                                    "🔥🔥🔥🔥🔥€25 korting bij een minimale besteding van €100")) %>% 
  group_by(uniqueCustomerId) %>% summarize(total_enter=sum(event=="enter"),
                                           total_participate=sum(event=="participate"),
                                           total_claim=sum(action=="claimed")) %>% 
  filter(total_enter<total_participate|total_participate<total_claim)
claim_participate_check
claim_participate_error <- claim_participate_check %>% pull(uniqueCustomerId)
wannagive_logging <- wannagive_logging %>% filter(!uniqueCustomerId %in% claim_participate_error)

#########################
## Q1
#########################
wannagive_logging %>% filter(date > as.Date("2024-12-02") & date < as.Date("2024-12-25"),event == "participate") %>% 
  summarise(unique_customers = n_distinct(uniqueCustomerId))

#  unique_customers
#<int>
#  1            7764 # because we remove a user info seems like an error or hard to process

# Create a vector of all uniqueCustomerId's
wannagive_engager <- wannagive_logging %>%
  filter(date > as.Date("2024-12-02") & date < as.Date("2024-12-25"), event == "participate") %>%
  distinct(uniqueCustomerId) %>%  
  pull(uniqueCustomerId)  

#########################
## Q2
#########################

# calculation of open rate: sum(participate)/sum(enter)
open_rate <- sum(wannagive_logging$event == "participate", na.rm = TRUE)/sum(wannagive_logging$event == "enter", na.rm = TRUE)
open_rate

# Get all unique dates
dates_list <- unique(wannagive_logging$date)
dates_list

# Use sapply to calculate the total number of “participations” per day.
participate_counts <- sapply(dates_list, function(d) {
  sum(wannagive_logging$event[wannagive_logging$date == d] == "participate", na.rm = TRUE)
})
participate_counts

# Creating Data Frames
open_per_day <- data.frame(date = dates_list, participate_count = participate_counts)
open_per_day <- open_per_day %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) 
print(open_per_day)

# Use lapply to calculate the total number of “participations” per day.
participate_counts <- sapply(dates_list, function(d) {
  sum(wannagive_logging$event[wannagive_logging$date == d] == "participate", na.rm = TRUE)
})
participate_counts
# Creating Data Frames
open_per_day <- data.frame(date = dates_list, participate_count = participate_counts)
open_per_day <- open_per_day %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) 
print(open_per_day)
open_per_day <- open_per_day %>%
  filter(date >= as.Date("2024-12-03") & date <= as.Date("2024-12-24"))

# visualization
ggplot(open_per_day, aes(x = date, y = participate_count)) +
  geom_col(fill = "lightblue") +
  labs(title = "Daily open count",
       x = "Date",
       y = "Participate Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#########################
## Q3
#########################
# Calculate the difference per day
open_per_day <- open_per_day %>%
  mutate(difference = participate_count - lag(participate_count))

slice_min(open_per_day, order_by = difference, n = 1)

#        date participate_count difference
#1 2024-12-07              1357       -545

filter(wannagive_logging,date=="2024-12-07")

#?????????????????
# do we need to analyze it with regression, or we just say the decorate tip is not attractive?
#?????????????????

# Use lapply to calculate the total number of “participations” per day.
enter_counts <- sapply(dates_list, function(d) {
  sum(wannagive_logging$event[wannagive_logging$date == d] == "enter", na.rm = TRUE)
})
enter_counts
# Creating Data Frames
open_per_day <- data.frame(date = dates_list, enter_count = enter_counts)
open_per_day <- open_per_day %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) 
print(open_per_day)
open_per_day <- open_per_day %>%
  filter(date >= as.Date("2024-12-03") & date <= as.Date("2024-12-24"))

# visualization
ggplot(open_per_day, aes(x = date, y = enter_count)) +
  geom_col(fill = "lightblue") +
  labs(title = "Daily enter count",
       x = "Date",
       y = "enter Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#?????????????????
# the enter rate was low on 07.12 too
#?????????????????

# check the customer preference
preference_df <- customer_info %>%
  group_by(most_popular_category) %>%
  summarize(preference_counts=n_distinct(unique_customer_id))
preference_df

ggplot(preference_df,aes(most_popular_category,preference_counts))+
  geom_col(fill = "lightblue")+
  labs(title="preference count",
       x="preference_category",
       y="counts")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

preference_of_decoration <- 100*sum(preference_df$preference_counts[preference_df$most_popular_category %in% c("FURNITURE", "SOFT FURNISHINGS")]) /
  sum(preference_df$preference_counts)
preference_of_decoration
#5.774607% of ppl like decoration

#########################
## Q4
#########################

# unique claimed 
wannagive_logging %>% filter(action == "claimed") %>% distinct(uniqueCustomerId, card) %>% 
  summarize(unique_claimed = n_distinct(uniqueCustomerId))
          #unique_claimed: 5851

# how to calculate claim rate? claimed/participate?
claim_rate <- wannagive_logging %>% 
  filter(action == "claimed") %>% 
  distinct(uniqueCustomerId, card) %>%  
  summarise(total_claimed = n()) %>%  
  pull(total_claimed) / 
  wannagive_logging %>% 
  filter(event == "participate") %>% 
  summarise(total_participate = n()) %>% 
  pull(total_participate)
claim_rate

# claimed counts of each kind of card
claim_df <- wannagive_logging %>% 
  filter(action == "claimed") %>%
  distinct(uniqueCustomerId, card,collectableType) %>%  
  group_by(card) %>%
  summarise(claim_counts = n(),
            card_type = max(collectableType))

# Calculate counts of participation per card
open_df <- wannagive_logging %>% 
  filter(event == "participate") %>%
  group_by(card) %>%
  summarise(open_counts = n())

# merge and get claimed rate
claim_df <- left_join(claim_df, open_df, by = "card") %>%
  mutate(claimed_rate = 100 * claim_counts / open_counts)
claim_df %>% arrange(desc(claimed_rate))

ggplot(claim_df, aes(x = reorder(card, -claim_counts))) +
  geom_bar(aes(y = open_counts, fill = "Open Counts"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = claim_counts, fill = "Claim Counts"), stat = "identity", position = "dodge") +
  geom_line(aes(y = claimed_rate * max(open_counts) / 100, group = 1), 
            color = "darkgreen", linewidth = 1) + 
  geom_text(aes(y = claimed_rate * max(open_counts) / 100, 
                label = round(claimed_rate, 1)), 
            vjust = -0.5, color = "darkgreen") +
  scale_y_continuous(
    name = "Counts",
    sec.axis = sec_axis(~ . * 100 / max(claim_df$open_counts, na.rm = TRUE), name = "Claimed Rate (%)")
  ) +
  labs(title = "Claim & Open Counts vs. Claimed Rate",
       x = "Card Type",
       fill = "Legend",
       color = "Legend") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# Based on the type of each card, e.g. all coupons are combined, comparing coupons and tips with others.
claim_df %>% summarise(n_distinct(card_type))
card_type_df <- claim_df %>% group_by(card_type) %>% 
  summarize(mean_claim_counts=mean(claim_counts),
            mean_open_counts=mean(open_counts),
            mean_claimed_rate=mean(claimed_rate))
card_type_df

# Plotting bar charts（mean_claim_counts 和 mean_open_counts）
ggplot(card_type_df, aes(x = card_type)) +
  geom_col(aes(y = mean_open_counts, fill = "Open Counts"), position = "dodge") +
  geom_col(aes(y = mean_claim_counts, fill = "Claim Counts"), position = "dodge") +
  geom_line(aes(y = mean_claimed_rate * max(mean_open_counts) / 100, group = 1), 
            color = "darkgreen", linewidth = 1.2) +
  geom_text(aes(y = mean_claimed_rate * max(mean_open_counts) / 100, 
                label = round(mean_claimed_rate, 1)), 
            vjust = -0.5, color = "darkgreen") +  
  scale_y_continuous(
    name = "Counts",
    sec.axis = sec_axis(~ . * 100 / max(card_type_df$mean_open_counts), name = "Claimed Rate (%)")
  ) +
  labs(title = "Mean Claim & Open Counts vs. Claimed Rate",
       x = "Card Type",
       fill = "Legend",
       color = "Legend") +
  theme_minimal()


#########################
## Q5
#########################

order_df <- transactions %>% group_by(date) %>%
  summarize(order_count=n(),
            items_order=sum(articles))

ggplot(order_df, aes(date,order_count))+
  geom_col(fill = "lightblue") +
  labs(title="order per day",
       x="date",
       y="order number")+
  scale_x_date(date_breaks = "5 days", date_labels = "%Y-%m-%d") +  # Display a date every 5 days
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  

order_df %>% arrange(desc(order_count))
# 2024-12-18 has the most order, maybe because of the event is discount on everything

#########################
## Q6
#########################

# calculate open times & streak of each customer, and check distribution
open_streak_df <- wannagive_logging %>%
  filter(event == "participate") %>%
  group_by(uniqueCustomerId) %>%
  summarise(
    open_count = n(), 
    max_streak = max(currentStreakValue, na.rm = TRUE)  
  ) %>%
  ungroup() 

open_streak_sim_df <- wannagive_logging %>%
  filter(event == "participate",
         !card %in% c(
           "🔥€5 korting bij een minimale besteding van €0",
           "🔥🔥€10 korting bij een minimale besteding van €30",
           "🔥🔥🔥€15 korting bij een minimale besteding van €60",
           "🔥🔥🔥🔥€20 korting bij een minimale besteding van €80",
           "🔥🔥🔥🔥🔥€25 korting bij een minimale besteding van €100"
         )) %>%
  group_by(uniqueCustomerId) %>%
  summarise(
    open_count = n(),
    max_streak = max(currentStreakValue, na.rm = TRUE)
  ) %>%
  ungroup()

open_streak_df

# distribution of card open
ggplot(open_streak_df, aes(open_count))+
  geom_bar(fill = "lightblue") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +  
  labs(title = "Frequency distribution of customer card opening",
       x = "open counts",
       y = "distribution")+
  theme_minimal()
# open card more than 22 is because of the streak card

# distribution of streak
ggplot(open_streak_df, aes(max_streak))+
  geom_bar(fill = "lightblue") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +  
  labs(title = "Frequency distribution of customer card streak",
       x = "open counts",
       y = "distribution")+
  theme_minimal()
# number streak 22 is more than open 22, cuz streak correspond to open 27

# distribution of card open
ggplot(open_streak_sim_df, aes(open_count))+
  geom_bar(fill = "lightblue") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +  
  labs(title = "Frequency distribution of customer card opening",
       x = "open counts",
       y = "distribution")+
  theme_minimal()

#########################
## Q7
#########################

# create action table including open rate / claim rate / max streak
action_df <- wannagive_logging %>% filter(uniqueCustomerId %in% wannagive_engager,
                                          !card %in% c(
                                            "🔥€5 korting bij een minimale besteding van €0",
                                            "🔥🔥€10 korting bij een minimale besteding van €30",
                                            "🔥🔥🔥€15 korting bij een minimale besteding van €60",
                                            "🔥🔥🔥🔥€20 korting bij een minimale besteding van €80",
                                            "🔥🔥🔥🔥🔥€25 korting bij een minimale besteding van €100"))%>% 
  group_by(uniqueCustomerId) %>%
  summarise(
    open_rate = sum(event == "participate", na.rm = TRUE) / sum(event == "enter", na.rm = TRUE),
    claim_rate = sum(action == "claimed", na.rm = TRUE) / sum(event == "participate", na.rm = TRUE),
    max_streak = max(currentStreakValue, na.rm = TRUE)
  )

action_df[is.na(action_df)] <- 0
sapply(action_df, function(x) sum(is.infinite(x)))
##### "P6gojXAQp7g75AIfuSYeHjJMt4I=" has participate but no enter
#action_df$open_rate[is.infinite(action_df$open_rate)] <- 0
action_df

# Including revenue in clustering process
open_claim_streak_revenue <- transactions %>% select(unique_customer_id,revenue) %>% 
  group_by(unique_customer_id) %>% summarize(total_revenue=sum(revenue))
sum(is.na(open_claim_streak_revenue))
open_claim_streak_revenue <- action_df %>% left_join(open_claim_streak_revenue,by=c("uniqueCustomerId"="unique_customer_id"))
sum(is.na(open_claim_streak_revenue))
open_claim_streak_revenue$total_revenue[is.na(open_claim_streak_revenue$total_revenue)] <- 0
n_distinct(open_claim_streak_revenue$uniqueCustomerId)

# standardized the observation values

action_sd_df <- open_claim_streak_revenue
action_sd_df[,2:5] <- data.frame(scale(open_claim_streak_revenue[,2:5]))


#-----------
# Use Hierarchical for segmentation
#------------
library(cluster)                  # daisy works with mixed data types, in case all variables are continuous, results are the same as with the dist function that is giving the 
# euclidean distance
action.dist <- daisy(action_sd_df[,c(2:5)])

# Hierarchical clustering of WARD to determine the cluster number
action.hc <- hclust(action.dist, method="ward.D2")
plot(action.hc)
spreadaggloe <- cbind(as.data.frame(action.hc[1]),as.data.frame(action.hc[2]))
spreadaggloe      # positive values of merge1 and merge2 refer to earlier formed clusters (in that specific row number), negative values to singletons (true observations)
spreadaggloe[1:15,]      # positive values of merge1 and merge2 refer to earlier formed clusters (in that specific row number), negative values to singletons (true observations)
spreadaggloe[7750:7763,]      # positive values of merge1 and merge2 refer to earlier formed clusters (in that specific row number), negative values to singletons (true observations)

spreadscreee <- sort(spreadaggloe[7750:7763,c(3)], decreasing = TRUE)
plot(spreadscreee, type="l", col="red", lwd=5, xlab="clusters", ylab="Cluster distance", main="Scree Plot", xaxt="n")
axis(1, at=seq(1,18,by = 1))

# actually get 5 groups
#action.hc.segment <- cutree(action.hc, k=5)     # membership vector for 3 groups
#table(action.hc.segment)

# get the Features of each cluster
#action_hc_df <- action_df
#action_hc_df$cluster <- action.hc.segment  
#action_hc_df %>% group_by(cluster) %>% 
#  summarize(mean_open_rate = mean(open_rate),
#            mean_claim_rate = mean(claim_rate),
#            mean_streak = mean(max_streak))

# PCA virtualization
#library(RColorBrewer)

#pca <- prcomp(action_sd_df[,c(2:5)])
#pca_scores <- predict(pca, action_sd_df[,c(2:5)])
#plot(pca_scores[, 1], pca_scores[, 2], col = action.hc.segment, pch = 16, cex = 1,
#     xlab = "PC1", ylab = "PC2", main = "(ward)4 Cluster Plot with PCA Dimensions")

#-----------
# Use K-mean for segmentation
#------------
set.seed(12345)
action.k5 <- kmeans(action_sd_df[,c(2:5)], centers=5)
table(action.k5$cluster)
action.k5.segment <- action.k5$cluster


# get the Features of each cluster
action_k5_df <- open_claim_streak_revenue
action_k5_df$cluster <- action.k5.segment  
action_k5_df %>% group_by(cluster) %>% 
  summarize(mean_open_rate = mean(open_rate),
            mean_claim_rate = mean(claim_rate),
            mean_streak = mean(max_streak),
            mean_revenue = mean(total_revenue))

#     cluster  mean_open_rate  mean_claim_rate mean_streak  mean_revenue   label                       Explanation
#<int>          <dbl>           <dbl>           <dbl>        <dbl>
#  1     1      0.722          0.552           14.2         336.           Loyal Participants	         Medium activity, moderate conversion rate, longest engagement time, moderate to high profit, indicating stickiness to the platform.
#2       2      0.516          0.565           1.85         230.           Low-Engagement Converters   Lower activity and relatively high conversion rates indicate that these users, while infrequently engaged, are likely to complete a purchase when engaged.
#3       3      0.966          0.0724          1.63         199.           Explorers                   Very high activity, very low conversion rate, probably exploring the platform but not really spending money.
#4       4      0.782          0.508           3.32         1774.          High-Value Buyers           Engagement and conversion rates are high and contribute significant revenue, making them a key customer for the platform.
#5       5      0.975          0.870           1.59         189.           Micro-consumer              Very high activity and very high conversion rates, but contributes less revenue and may be frequent small spenders.



# defining Cluster Labeling
cluster_labels <- c(
  "1" = "Loyal Participants",
  "2" = "Low-Engagement Converters",
  "3" = "Explorers",
  "4" = "High-Value Buyers",
  "5" = "Micro-consumer"
)

# PCA Dimension Scaling and Clustering Visualization
pca <- prcomp(action_sd_df[, c(2:5)])
pca_scores <- predict(pca, action_sd_df[, c(2:5)])
plot(pca_scores[, 1], pca_scores[, 2], col = action.k5.segment, pch = 16, cex = 1,
     xlab = "PC1", ylab = "PC2", main = "(kmean) 5 Cluster Plot with PCA Dimensions")

# Clustering information merged with user information
userinfo_seg <- customer_info %>%  
  right_join(action_k5_df, by = c("unique_customer_id" = "uniqueCustomerId")) 

# Generate a summary table for each cluster
cluster_summary <- userinfo_seg %>%
  group_by(cluster) %>%
  summarise(
    customer_number = n(),
    female_rate = sum(gender == "F", na.rm = TRUE) / sum(!is.na(gender)),
    mean_user_length = mean(first_order_year, na.rm = TRUE),
    mean_urban = mean(`urbanisation (1 high, 5 low)`, na.rm = TRUE),
    app_rate = sum(most_used_platform == "app", na.rm = TRUE) / sum(!is.na(most_used_platform)),
    mobile_rate = sum(most_used_device == "mobile", na.rm = TRUE) / sum(!is.na(most_used_device)),
    tablet_rate = sum(most_used_device == "tablet", na.rm = TRUE) / sum(!is.na(most_used_device)),
    mean_scope_purchased = mean(unique_categories_bought, na.rm = TRUE),
    mean_order_number = mean(orders, na.rm = TRUE)
  ) %>%
  mutate(
    cluster_label = recode(as.character(cluster),
                           "1" = "Loyal Participants",
                           "2" = "Low-Engagement Converters",
                           "3" = "Explorers",
                           "4" = "High-Value Buyers",
                           "5" = "Micro-consumer")
  ) %>%
  select(cluster_label, everything(), -cluster)
print(cluster_summary)

#  Histogram of the distribution of the number of clusters
cluster_counts <- userinfo_seg %>%
  group_by(cluster) %>%
  summarise(count = n(), .groups = "drop")

ggplot(cluster_counts, aes(x = factor(cluster, labels = cluster_labels), y = count, fill = as.factor(cluster))) +
  geom_col() +
  labs(title = "Number of Customers per Cluster",
       x = "Cluster", y = "Count", fill = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  Calculation of age distribution
age_distribution <- userinfo_seg %>%
  group_by(cluster, age) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(cluster) %>%
  mutate(percentage = count / sum(count) * 100)

# Age groupings and color defining
age_distribution$age <- factor(age_distribution$age, 
                               levels = c("15-19", "20-24", "25-29", "30-34", "35-39",
                                          "40-44", "45-49", "50-54", "55-59", "60-64",
                                          "65-69", "70-74", "75-79", "80-84", "85-89",
                                          "90-94", "95-99", "105-109", "NA"))
age_colors <- c("15-19" = "#377eb8", "20-24" = "#4daf4a", "25-29" = "#984ea3",
                "30-34" = "#e41a1c", "35-39" = "#ff7f00", "40-44" = "#f781bf",
                "45-49" = "#a65628", "50-54" = "#ffff33", "55-59" = "#999999",
                "60-64" = "#66c2a5", "65-69" = "#fc8d62", "70-74" = "#8da0cb",
                "75-79" = "#e78ac3", "80-84" = "#a6d854", "85-89" = "#ffd92f",
                "90-94" = "#e5c494", "95-99" = "#b3b3b3", "105-109" = "black", "NA" = "gray")

# Age percentage visualization
ggplot(age_distribution, aes(x = factor(cluster, labels = cluster_labels), y = percentage, fill = age)) +
  geom_bar(stat = "identity", position = "fill") +  # 保持相同高度
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 3, color = "white") +
  labs(title = "Age Percentage Distribution Across Clusters",
       x = "Cluster", y = "Percentage", fill = "Age Group") +
  scale_fill_manual(values = age_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  Calculation of provincial distribution 
province_distribution <- userinfo_seg %>%
  group_by(cluster, province) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(cluster) %>%
  mutate(percentage = count / sum(count) * 100)

# Visualization of the proportion of provinces
ggplot(province_distribution, aes(x = factor(cluster, labels = cluster_labels), y = percentage, fill = province)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 3, color = "white") +
  scale_fill_manual(values = c(
    "Drenthe" = "#E41A1C", "Flevoland" = "#FF7F00", "Friesland" = "#FFD700",
    "Gelderland" = "#999900", "Groningen" = "#8A8A00", "Limburg" = "#006400",
    "Noord-Brabant" = "#008000", "Noord-Holland" = "#00A550", "Overijssel" = "#00CED1",
    "Utrecht" = "#00BFFF", "Zeeland" = "#1E90FF", "Zuid-Holland" = "#0000FF"
  )) +
  labs(title = "Province Percentage Distribution Across Clusters",
       x = "Cluster", y = "Percentage", fill = "Province") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  Distribution of purchases by categor
category_distribution <- userinfo_seg %>%
  group_by(cluster, most_popular_category) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(cluster) %>%
  mutate(percentage = count / sum(count) * 100)

# Visualization of category ratios
ggplot(category_distribution, aes(x = factor(cluster, labels = cluster_labels), y = percentage, fill = most_popular_category)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_fill(vjust = 0.5), size = 2.5, color = "white") +
  scale_fill_manual(values = c(
    "ACCESSORIES" = "#E41A1C", "BABY" = "#FF7F00", "BEACHWEAR" = "#FFD700",
    "BEAUTY" = "#999900", "ELECTRONICS" = "#8A8A00", "FMCG" = "#006400",
    "FURNITURE" = "#008000", "GARDEN" = "#00A550", "HEALTH" = "#00CED1",
    "KIDS FASHION" = "#00BFFF", "LADIES FASHION" = "#1E90FF", "LINGERIE" = "#0000FF",
    "MENS FASHION" = "#8A2BE2", "NIGHTWEAR" = "#9400D3", "OTHER" = "#D87093",
    "SHOES" = "#FF69B4", "SOFT FURNISHINGS" = "#FF1493", "SPORTS" = "#DC143C",
    "TOYS" = "#A52A2A", "NA" = "#808080"
  )) +
  labs(title = "Most Popular Category Percentage Distribution Across Clusters",
       x = "Cluster", y = "Percentage", fill = "Most Popular Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#########################
## Q8
#########################

# filter the customers that reached the ultimate streak
ultimate_df <- wannagive_logging %>% filter(currentStreakValue==22)
id_vector <- as.vector(unique(ultimate_df$uniqueCustomerId))

ultimate_user <- wannagive_logging %>% filter(uniqueCustomerId %in% id_vector)

ult_coupon_use <- transactions %>% filter(unique_customer_id %in% id_vector) %>% filter(coupon_code %in% c(
  "NIEUWJAARSKORTING1","NIEUWJAARSKORTING2","NIEUWJAARSKORTING3","NIEUWJAARSKORTING4","NIEUWJAARSKORTING5")) 

coupon_use_df <- ult_coupon_use %>% 
  group_by(coupon_code) %>% 
  summarise(coupon_use_count = n(),
            total_revenue = sum(revenue),
            mean_revenue = mean(revenue),
            total_articles = sum(articles),
            mean_articles = mean(articles)
            )
coupon_use_df

# Frequency of coupon_code distribution
ggplot(coupon_use_df, aes(x = coupon_code, y = coupon_use_count, fill = coupon_code)) +
  geom_col() +
  labs(title = "Coupon Type Distribution", x = "Coupon Code", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


library(tidyr)
# Convert data to long format
maxcoupon_use_long <- coupon_use_df %>%
  pivot_longer(cols = c(coupon_use_count, total_revenue, mean_revenue, total_articles),
               names_to = "Metric", values_to = "Value")

# Generate 4 subgraphs using facet_wrap
ggplot(maxcoupon_use_long, aes(x = coupon_code, y = Value, fill = Metric)) +
  geom_col(position = "dodge", width = 0.5) +
  facet_wrap(~ Metric, scales = "free_y") + # Separate facets for each Metric
  labs(title = "Comparison of Coupon Metrics by Metric",
       x = "Coupon Code",
       y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#########################
## Q9
#########################

transactions_before <- transactions %>% filter(date < as.Date("2024-12-03"))
transactions_during <- transactions %>% filter(date > as.Date("2024-12-02") & date < as.Date("2024-12-25"))
transactions_after <- transactions %>% filter(date > as.Date("2024-12-24"))

sum(n_distinct(transactions_before$date))
sum(n_distinct(transactions_during$date))
sum(n_distinct(transactions_after$date))

before <- transactions_before %>% group_by(unique_customer_id) %>% 
  summarize(order_frequency=n()/sum(n_distinct(transactions_before$date)),
            item_per_day = sum(articles)/sum(n_distinct(transactions_before$date)),
            revenue_per_day = sum(revenue)/sum(n_distinct(transactions_before$date)))

during <- transactions_during %>% group_by(unique_customer_id) %>% 
  summarize(order_frequency=n()/sum(n_distinct(transactions_during$date)),
            item_per_day = sum(articles)/sum(n_distinct(transactions_during$date)),
            revenue_per_day = sum(revenue)/sum(n_distinct(transactions_during$date)))

after <- transactions_after %>% group_by(unique_customer_id) %>% 
  summarize(order_frequency=n()/sum(n_distinct(transactions_after$date)),
            item_per_day = sum(articles)/sum(n_distinct(transactions_after$date)),
            revenue_per_day = sum(revenue)/sum(n_distinct(transactions_after$date)))

# Merge data and add period column
combined_df <- bind_rows(
  before %>% mutate(period = "before"),
  during %>% mutate(period = "during"),
  after %>% mutate(period = "after")
)

# Ensure that each user has data in all periods
full_customers <- expand.grid(unique_customer_id = unique(combined_df$unique_customer_id),
                              period = c("before", "during", "after"))

# Left connection filled with complete data
combined_df <- full_customers %>%
  left_join(combined_df, by = c("unique_customer_id", "period"))

# Fill missing values NA -> 0
combined_df <- combined_df %>%
  mutate(
    order_frequency = ifelse(is.na(order_frequency), 0, order_frequency),
    item_per_day = ifelse(is.na(item_per_day), 0, item_per_day),
    revenue_per_day = ifelse(is.na(revenue_per_day), 0, revenue_per_day)
  )

# Ensure that period is a factor variable
combined_df$period <- factor(combined_df$period, levels = c("before", "during", "after"))

# Impact on order frequency before and after the event 
model_order <- glm(order_frequency ~ period, data = combined_df, family = gaussian())
summary(model_order)   

# Impact on item_per_day before and after the event 
model_items <- glm(item_per_day ~ period, data = combined_df, family = gaussian())
summary(model_items)   

# Impact on revenue_per_day before and after the event 
model_revenue <- glm(revenue_per_day ~ period, data = combined_df, family = gaussian())
summary(model_revenue) 






