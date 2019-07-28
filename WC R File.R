library(dplyr)
library(tidyr)


setwd("C:\\Users\\Goliath\\Desktop\\WC Files")

#Loading in data


{
  
  communities <- read.csv("wc_communities.csv", header=T, stringsAsFactors = FALSE)
  customers <- read.csv("wc_customers.csv", header=T, stringsAsFactors = FALSE)
  community_payouts <- read.csv("wc_community_payouts.csv", header=T, stringsAsFactors = FALSE)
  policy_transactions <- read.csv("wc_policy_transactions.csv", header=T, stringsAsFactors = FALSE)
  customer_policies <- read.csv("wc_customer_policies.csv", header=T, stringsAsFactors = FALSE)
  
  population_data <- read.csv("Region_population_data.csv", header=T, stringsAsFactors = FALSE)
  
  
  new_season_names <- data.frame("season" = c("2016 major","2017 major", "2018 major","2018 minor","2019 minor", "2019 major"),
                              "new_season" = c(2016, 2017, 2018.9, 2018.1, 2019.1, 2019.9),
                              "overall_year" = c(2016, 2017, 2018, 2018, 2019, 2019),
                              "season_a" = c("2016.a", "2017.a", "2018.a", "2018.z", "2019.a", "2019.z"),
                              stringsAsFactors = FALSE)
}


#filtering out and renaming columns as needed

{
policy_transactions <- policy_transactions %>% rename(., policy_transaction_amount = transaction_amount, policy_created_at = created_at )%>% select(.,-updated_at)

customer_policies <- customer_policies  %>% select(., -date_issued, -created_at, -updated_at) %>% rename(., customer_status = status)   

customers <- customers %>% select(., -registration_date, -updated_at) %>% rename(., customer_created_at = created_at)

community_payouts <-  community_payouts %>%  select(., -created_at)  %>% rename(., community_transaction_amount = transaction_amount) %>% arrange(., )
}




#Data exploration 
{



summary(communities)
summary(customers)
summary(community_payouts)
summary(policy_transactions)
summary(customer_policies)

nrow(communities)
nrow(community_payouts)
nrow(customers)
nrow(customer_policies)
nrow(policy_transactions)





###############
#There are 105 customer policies that do not exist in policy_transaction
nrow(anti_join(policy_transactions, customer_policies, by = "customer_policy_id"))
length(unique(policy_transactions$customer_policy_id))
length(unique(customer_policies$customer_policy_id))






#All 'customers' in customer_policies exist in customer
nrow(anti_join(customers, customer_policies, by = "customer_id"))
length(unique(customer_policies$customer_id))
length(unique(customers$customer_id))

#There are some customers with more than one policy 
nrow(customer_policies)
length(unique(customer_policies$customer_id))

#Customers are unique rows
length(unique(customers$customer_id))
nrow(customers)
nrow(distinct(customers))







#All 'customers' in customer have a community
#Some cummunities exist that have no customers
{nrow(anti_join(customers, communities, by = "community_id"))
length(unique(customers$community_id))
length(unique(communities$community_id))}


#all communities that have had payouts exist in communities table
#there are communities that have not had payouts
{nrow(anti_join(community_payouts, communities, by = "community_id"))
length(unique(community_payouts$community_id))
length(unique(communities$community_id))}

}#End of data exploration and checks

#Combining all data tables together
{total_table <- full_join(policy_transactions,customer_policies, by = "customer_policy_id") %>%
  left_join(., customers, by = "customer_id") %>% 
  left_join(., communities, by = "community_id") %>%
  left_join(.,community_payouts, by = c("community_id", "season")) %>% 
  arrange(., match(season, c("2016 major", "2017 major", "2018 minor", "2018 major", "2019 minor", "2019 major")), policy_created_at, customer_created_at )
}  

#Writing table for easier load in the future
write.csv(total_table, "Combined_table.csv", row.names=FALSE)

#Total Table checks
{
nrow(total_table)
season_list <- unique(total_table$season)
}



#Creating subsets based on season
{
season_2016_major <- total_table %>% filter(., season == "2016 major")
season_2017_major <- total_table %>% filter(., season == "2017 major")
season_2018_major <- total_table %>% filter(., season == "2018 major")
season_2019_major <- total_table %>% filter(., season == "2019 major")
season_2018_minor <- total_table %>% filter(., season == "2018 minor")
season_2019_minor <- total_table %>% filter(., season == "2019 minor")
}

#


############################################################
#Section checks and labels to see if customer is new, returning, or retained 
#Rules are
#1) If customer did not exist in any previous seasons then it is a new customer
#2)if customer existed in the direct previous season it is a retained customer
#3)if a customer existed in a previous season, but it was not the direct preivous one then they are returning
#*Note that only certain regions ("GH-BA", "GH-AH", "GH-TV") looked at minor 2018 or minor 2019 for direct preivous season rules, other regions looked at major only
{
#setting all 2016 as new given there is no 2015 data
season_2016_major <- mutate(season_2016_major, returning_status = "New")

#Checking for returning status of 2017
temp_vector <- rep(NA, nrow(season_2017_major))

for(i in 1:nrow(season_2017_major)) {
  
 if(season_2017_major$customer_id[i] %in% season_2016_major$customer_id ) {
   temp_vector[i] <- "New"
 } else {
  temp_vector[i] <- "Retained"
}
 
}
 
season_2017_major <- mutate(season_2017_major, returning_status = temp_vector)
 
 
#Checking for returning status of 2018 minor season. After checking I see all new, further investigation shows "GH-BA", "GH-AH", "GH-TV" regions didn't exist in prior years

temp_vector <- rep(NA, nrow(season_2018_minor))
  
for(i in 1:nrow(season_2018_minor)) {
  
  if(season_2018_minor$customer_id[i] %in% season_2017_major$customer_id ) {
    temp_vector[i] <- "Retained"
  } else if(season_2018_minor$customer_id[i] %in% season_2016_major$customer_id ){
    temp_vector[i] <- "Returned"
  } else {
    temp_vector[i] <- "New"
  }
  
}

season_2018_minor <- mutate(season_2018_minor, returning_status = temp_vector)


#Checking for returning status of 2018 minor major
temp_vector <- rep(NA, nrow(season_2018_major))

for(i in 1:nrow(season_2018_major)) {
  
  
  #If in special regions, we need to check for minor
  if(season_2018_major$region[i] %in% c("GH-BA", "GH-AH", "GH-TV")) {
    
    if(season_2018_major$customer_id[i] %in% season_2018_minor$customer_id) {
      temp_vector[i] <- "Retained"
    } else {
      temp_vector[i] <- "New"
    }
  
   } else {
  #else for non special regions we check against major seasons   
     
    if(season_2018_major$customer_id[i] %in% season_2017_major$customer_id ) {
    temp_vector[i] <- "Retained"
  } else if(season_2018_major$customer_id[i] %in% season_2016_major$customer_id ){
    temp_vector[i] <- "Returned"
  } else {
    temp_vector[i] <- "New"
  }
          }
  
} #End of 2018 for loop
  
season_2018_major <- mutate(season_2018_major, returning_status = temp_vector)



#2019 Minor check.  Must look at 2018 minor too for returning status having skipped 2018 major.


temp_vector <- rep(NA, nrow(season_2019_minor))

for(i in 1:nrow(season_2019_minor)) {
  
  if(season_2019_minor$customer_id[i] %in% season_2018_major$customer_id ) {
    temp_vector[i] <- "Retained"
  } else if(season_2019_minor$customer_id[i] %in% season_2018_minor$customer_id ){
    temp_vector[i] <- "Returned"
    
  } else if(season_2019_minor$customer_id[i] %in% season_2017_major$customer_id ){
    temp_vector[i] <- "Returned"
    
  } else if(season_2019_minor$customer_id[i] %in% season_2016_major$customer_id ){
    temp_vector[i] <- "Returned"
    
  }else {
    temp_vector[i] <- "New"
  }
  
}

season_2019_minor <- mutate(season_2019_minor, returning_status = temp_vector)




##Check for 2019 major returning

temp_vector <- rep(NA, nrow(season_2019_major))

for(i in 1:nrow(season_2019_major)) {
  
  
  #If in special regions, we need to check for minor
  if(season_2019_major$region[i] %in% c("GH-BA", "GH-AH", "GH-TV")) {
    
    if(season_2019_major$customer_id[i] %in% season_2019_minor$customer_id) {
      temp_vector[i] <- "Retained"
    } else if(season_2019_major$customer_id[i] %in% season_2018_major$customer_id){
      temp_vector[i] <- "Returned"
    } else if(season_2019_major$customer_id[i] %in% season_2018_minor$customer_id){
      temp_vector[i] <- "Returned"
    } else if(season_2019_major$customer_id[i] %in% season_2017_major$customer_id ){
      temp_vector[i] <- "Returned"
    } else if(season_2019_major$customer_id[i] %in% season_2016_major$customer_id ){
      temp_vector[i] <- "Returned"
    }
    else {
      temp_vector[i] <- "New"
    }
    
  } else {
    #else for non special regions we check against major seasons   
    
    if(season_2019_major$customer_id[i] %in% season_2018_major$customer_id ) {
      temp_vector[i] <- "Retained"
    } else if(season_2019_major$customer_id[i] %in% season_2017_major$customer_id ){
      temp_vector[i] <- "Returned"
    } else if(season_2019_major$customer_id[i] %in% season_2016_major$customer_id ){
      temp_vector[i] <- "Returned"
    } else {
      temp_vector[i] <- "New"
    }
  }
  
} #End of 2018 for loop

season_2019_major <- mutate(season_2019_major, returning_status = temp_vector)
}



############################################################
##Following section checks for churn or retained customers
#Rules are
#1) If customer exists in the direct following season it is retained 
#2)if customer does not exist in the direct following season it is churned 
#*Note that only certain regions ("GH-BA", "GH-AH", "GH-TV") looked at minor 2018 or minor 2019 for direct following season rules, other regions looked at major only
{
#setting all 2019 as Unknown given there is no 2020 data to determine if they churned or not
season_2019_major <- mutate(season_2019_major, churn_status = "Unknown")

#Checking for churn status of 2016
temp_vector <- rep(NA, nrow(season_2016_major))

for(i in 1:nrow(season_2016_major)) {
  
  if(season_2016_major$customer_id[i] %in% season_2017_major$customer_id ) {
    temp_vector[i] <- "Retained"
  } else {
    temp_vector[i] <- "Churned"
  }
  
}

season_2016_major <- mutate(season_2016_major, churn_status = temp_vector)



#Checking 2017 for churn.
temp_vector <- rep(NA, nrow(season_2017_major))

for(i in 1:nrow(season_2017_major)) {
  
  
  #If in special regions, we need to check for minor
  if(season_2017_major$region[i] %in% c("GH-BA", "GH-AH", "GH-TV")) {
    
    if(season_2017_major$customer_id[i] %in% season_2018_minor$customer_id) {
      temp_vector[i] <- "Retained"
    } else {
      temp_vector[i] <- "Churned"
    }
    
  } else {
    #else for non special regions we check against major seasons   
    
    if(season_2017_major$customer_id[i] %in% season_2018_major$customer_id ) {
      temp_vector[i] <- "Retained"
    } else {
      temp_vector[i] <- "Churned"
    }
  }
  
} #End of 2017 for loop

season_2017_major <- mutate(season_2017_major, churn_status = temp_vector)




#Checking for churn status of 2018 minor
temp_vector <- rep(NA, nrow(season_2018_minor))

for(i in 1:nrow(season_2018_minor)) {
  
  if(season_2018_minor$customer_id[i] %in% season_2018_major$customer_id ) {
    temp_vector[i] <- "Retained"
  } else {
    temp_vector[i] <- "Churned"
  }
  
}

season_2018_minor <- mutate(season_2018_minor, churn_status = temp_vector)




#Checking 2018 major for churn.
temp_vector <- rep(NA, nrow(season_2018_major))

for(i in 1:nrow(season_2018_major)) {
  
  
  #If in special regions, we need to check for minor
  if(season_2018_major$region[i] %in% c("GH-BA", "GH-AH", "GH-TV")) {
    
    if(season_2018_major$customer_id[i] %in% season_2019_minor$customer_id) {
      temp_vector[i] <- "Retained"
    } else {
      temp_vector[i] <- "Churned"
    }
    
  } else {
    #else for non special regions we check against major seasons   
    
    if(season_2018_major$customer_id[i] %in% season_2019_major$customer_id ) {
      temp_vector[i] <- "Retained"
    } else {
      temp_vector[i] <- "Churned"
    }
  }
  
} #End of 2018 for loop

season_2018_major <- mutate(season_2018_major, churn_status = temp_vector)


#Checking for churn status of 2019 minor
temp_vector <- rep(NA, nrow(season_2019_minor))

for(i in 1:nrow(season_2019_minor)) {
  
  if(season_2019_minor$customer_id[i] %in% season_2019_major$customer_id ) {
    temp_vector[i] <- "Retained"
  } else {
    temp_vector[i] <- "Churned"
  }
  
}
season_2019_minor <- mutate(season_2019_minor, churn_status = temp_vector)


}


#Combining all seasons back into one final table
final_table <- do.call("rbind", list(season_2016_major,season_2017_major,season_2018_major, season_2019_major, season_2018_minor, season_2019_minor, season_2019_major))


#appending extra socio economic data that was found online to final table
#Making season 2 so order of seasons looking with minor will sort alphabetically in order
final_table<- (left_join(final_table, population_data, by = c("region" = "HASC"))) %>%
    left_join(., new_season_names, by = c("season" = "season")) 

#removing NA
final_table <- replace(final_table, is.na(final_table), "")




#Printing final table
write.csv(final_table, "Final_table.csv", row.names=FALSE)





#Section of attempts to label churn and new/returning customers based on a different approach. Decided not to continue with this as it wasn't coming together.
{
churn_check <- total_table %>% distinct(season, customer_id) %>% group_by(season, customer_id) %>% summarize(n = n()) %>% spread(key = season, value = n)

colnames(churn_check) <- c("customer_id", "year_2016_major",  "year_2017_major", 
                           "year_2018_major",  "year_2018_minor",  
                           "year_2019_major","year_2019_minor")

churn_check2 <- churn_check %>% mutate(., total_seasons_purchased = select(., year_2016_major, year_2017_major,  year_2018_major,  year_2019_major,
                                                                             year_2018_minor, year_2019_minor) %>%
                                         apply(1, sum, na.rm=TRUE))
                                        


#Not true churn, as there are people here who skip seasons         
churn_ids <- filter(churn_check2, total_seasons_purchased > 1)  %>% mutate_all(~replace(., is.na(.), 0))

nrow(churn_check2)
                                                                  
head(total_table)



season_2016_major <- total_table %>% filter(., season == "2016 major")
season_2017_major <- total_table %>% filter(., season == "2017 major")
season_2018_major <- total_table %>% filter(., season == "2018 major")
season_2019_major <- total_table %>% filter(., season == "2019 major")
season_2018_minor <- total_table %>% filter(., season == "2018 minor")
season_2019_minor <- total_table %>% filter(., season == "2019 minor")


unique(season_2018_minor$region)
unique(season_2019_minor$region)

#Only regions "GH-BA" "GH-AH" get checked for 2018 minor
#GH-BA, GH-AH, and "GH-TV" get checked for 2019 minor and



#rows of policies from 16 that were retained in 2017
kept_in_17 <- filter(season_2016_major,  season_2016_major$customer_id %in% season_2017_major$customer_id) 
kept_in_17 <- mutate(kept_in_17, customer_churn = "Retained")

kept_in_18 <- filter(season_2017_major, region != "GH-BA" && region != "GH-AH" && region != "GH-TV") %>%  filter(.,  season_2017_major$customer_id %in% season_2018_major$customer_id) 
kept_in_18 <- mutate(kept_in_18, customer_churn = "Retained")

kept_in_18_minors <- filter(season_2017_major, region == "GH-BA" || region == "GH-AH" || region != "GH-TV") %>%  filter(.,  season_2017_major$customer_id %in% season_2018_minor$customer_id) 
kept_in_18_minors <- mutate(kept_in_18_minors, customer_churn = "Retained")

kept_in_18_majors <- filter(season_2018_minor, region == "GH-BA" || region == "GH-AH" || region != "GH-TV") %>%  filter(.,  season_2018_minor$customer_id %in% season_2018_major$customer_id) 
kept_in_18_majors <- mutate(kept_in_18_majors, customer_churn = "Retained")






retained_17 <- filter(season_2016_major, season_2016_major$customer_id %in% season_2017_major$customer_id) 


nrow(unique(inner_join(season_2017_major, season_2018_major, by = "customer_id" )))
#1330 kept from 2017 to 2018


retained_18 <- unique(inner_join(filter(season_2018_major, region != "GH-BA" && region != "GH-AH" && region != "GH-TV"), season_2017_major, by = "customer_id" )) 
retained_18_minor <- unique(inner_join(filter(season_2018_minor, region == "GH-BA" || region == "GH-AH" || region != "GH-TV"), season_2017_major, by = "customer_id" ))
retained_18_major <- unique(inner_join(filter(season_2018_major, region == "GH-BA" || region == "GH-AH" || region != "GH-TV"), season_2018_minor, by = "customer_id" ))

retained_19 <- unique(inner_join(filter(season_2019_major, region != "GH-BA" || region != "GH-AH"), season_2018_major, by = "customer_id" ))
retained_19_minor <- unique(inner_join(filter(season_2019_minor, region == "GH-BA" || region == "GH-AH"), season_2018_major, by = "customer_id" ))

#None retained 
#retained_from_19_major <- unique(inner_join(filter(season_2019_major, region == "GH-BA" || region == "GH-AH"), season_2019_minor, by = "customer_id" ))

retained <- bind_rows(retained_from_17, retained_from_18, retained_from_18_minor,retained_from_19, retained_from_19_minor) %>%
            mutate(customer_status = "Returning", customer_churn = "Retained")

total_table

total_table2 <- left_join(total_table, retained, by= c("customer_id", "season", "policy_transaction_id.x"))


} #End of failed code process ideas 
