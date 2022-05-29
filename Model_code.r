library(tidyverse)
library(readr)
library(rvest)
library(ROCR)
library(tidytext)
library(foreach)
library(stringr)
library(SnowballC)
library(readxl)
library(ranger)

#### Section 1: Cleaning ################

#####Step1 ####
# Data sets for distance, lap and race are loaded and select relevant variables for analysis

# load distance data for 2019 
distance_data19<- foreach(race=1:8, .combine = "rbind") %do% {
  filename <- paste0("2019distance", race, ".csv")
  this.data <- read_csv(filename)
  names(this.data)<-tolower(names(this.data))
  if ("race_num" %in% names(this.data)){
    this.data<-this.data%>%rename(race=race_num)
  }
  this.data
}

# load distance data for 2020 
distance_data20<- foreach(race=1:5, .combine = "rbind") %do% {
  filename <- paste0("2020distance", race, ".csv")
  this.data <- read_csv(filename)
  names(this.data)<-tolower(names(this.data))
  if ("race_num" %in% names(this.data)){
    this.data<-this.data%>%rename(race=race_num)
  }
  this.data
}

# load distance data for 2021 
distance_data21<- foreach(race=1:13, .combine = "rbind") %do% {
  filename <- paste0("2021distance", race, ".csv")
  this.data <- read_csv(filename)
  names(this.data)<-tolower(names(this.data))
  if ("race_num" %in% names(this.data)){
    this.data<-this.data%>%rename(race=race_num)
  }
  this.data
}

# load race data for 2019 
race_data19<- foreach(race=1:8, .combine = "rbind") %do% {
  filename <- paste0("2019race", race, ".csv")
  this.data <- read_csv(filename)
  names(this.data)<-tolower(names(this.data))
  this.data<-this.data%>%
    select(drivernumber,teamname,position,gridposition,status,race,year)
  this.data
}

# load race data for 2020 
race_data20<- foreach(race=1:5, .combine = "rbind") %do% {
  filename <- paste0("2020race", race, ".csv")
  this.data <- read_csv(filename)
  names(this.data)<-tolower(names(this.data))
  this.data<-this.data%>%
    select(drivernumber,teamname,position,gridposition,status,race,year)
  this.data
}

# load race data for 2021 
race_data21<- foreach(race=1:13, .combine = "rbind") %do% {
  filename <- paste0("2021race", race, ".csv")
  this.data <- read_csv(filename)
  names(this.data)<-tolower(names(this.data))
  this.data<-this.data%>%
    select(drivernumber,teamname,position,gridposition,status,race,year)
  this.data
}

# load lap data for 2019 
lap_data19<- foreach(race=1:8, .combine = "rbind") %do% {
  filename <- paste0("2019lap", race, ".csv")
  this.data <- read_csv(filename)
  names(this.data)<-tolower(names(this.data))
  this.data<-this.data%>% drop_na(lapnumber) %>% 
    select(drivernumber,race,year,lapnumber,stint,compound,tyrelife,trackstatus)
  
  this.data
}

# load lap data for 2020 
lap_data20<- foreach(race=1:5, .combine = "rbind") %do% {
  filename <- paste0("2020lap", race, ".csv")
  this.data <- read_csv(filename)
  names(this.data)<-tolower(names(this.data))
  this.data<-this.data%>% drop_na(lapnumber) %>% 
    select(drivernumber,race,year,lapnumber,stint,compound,tyrelife,trackstatus)
  this.data
}

# load lap data for 2021 
lap_data21<- foreach(race=1:13, .combine = "rbind") %do% {
  filename <- paste0("2021lap", race, ".csv")
  this.data <- read_csv(filename)
  names(this.data)<-tolower(names(this.data))
  this.data<-this.data%>% drop_na(lapnumber) %>% 
    select(drivernumber,race,year,lapnumber,stint,compound,tyrelife,trackstatus)
  this.data
}

#combine all data 
distance<-bind_rows(distance_data19,distance_data20,distance_data21)
race<-bind_rows(race_data19,race_data20,race_data21)
laps<-bind_rows(lap_data19,lap_data20,lap_data21)

#write csv for future use

#write.csv(distance,"'data/distance.csv")
#write.csv(race,"data/race.csv")
#write.csv(laps,"data/laps.csv")


#### Step 2: Data wrangling ####

#load data
distance<-read.csv("data/distance.csv")
race<-read.csv("data/race.csv")
laps<-read.csv("data/laps.csv")


##### true lap table
distance.adj<-distance %>% group_by(year,race) %>% 
  filter(!duplicated(position)) %>% 
  arrange(year,race,position) %>% 
  mutate(cum_col=cumsum(total_lap)) %>% 
  mutate(laglap=cum_col-total_lap[1])%>%
  select(race,year,position,laglap)

#### left join to get real lap
distance.new<-distance%>%
  left_join(distance.adj,by=c("year","race","position")) %>% 
  mutate(adj_lap=lap-laglap) %>% 
  select(-lap,-laglap,-position)

#Race

#change status in race data set
race<-race %>% mutate(status=ifelse(status == "Collision" |status == "Finished"
                                  |status == "+1 Lap"|status == "+2 Laps" |status == "+3 Laps",status,"Failure"))



overtake<-distance.new %>% select(-X)


#join lap information to distance data
overtakechance<-overtake %>% 
  left_join(laps,by=c("latter"="drivernumber","race","year"="year","adj_lap"="lapnumber"))%>% 
  filter(adj_lap!=1) %>% #delete opening laps (lap 1)
  filter(trackstatus==1) %>% #filter clear track
  mutate(chance=ifelse(mean<=83.33&mean>=83.33*0.1,1,0))%>% #define mean distance for overtake chance
  group_by(former,latter,race,year) %>% mutate(leadlap=lead(adj_lap))#next lap recorded in data set
overtakechance<-overtakechance %>% select(-X) %>% 
  rename(latter.stint=stint,latter.compound=compound,latter.tyrelife=tyrelife) %>% 
  left_join(laps,by=c("former"="drivernumber","race","year"="year","adj_lap"="lapnumber"))%>% 
  rename(former.stint=stint,former.compound=compound,former.tyrelife=tyrelife) %>% 
  select(-X,-trackstatus.x,-trackstatus.y)
  
#find out drivers overtake each other within a lap
chance<-overtakechance%>%filter(chance==1) %>% select(year,former,latter,race,adj_lap)
jointest<- chance%>%
  inner_join(chance, 
             by=c("former"="latter","latter"="former","year"="year","race"="race","adj_lap"=
                    "adj_lap"))
#label those laps 
jointest$same <- 1
overtakechance<-merge(overtakechance,jointest, all.x = TRUE)%>%
  mutate(same=replace(same, is.na(same), 0))




################Section 2: Feature Engineering####################

###########Step 1:Overtake Switch##########

### create drivers list for 2019
vect19<-overtakechance%>%filter(year==2019)%>%summarise(unique(latter))%>%t()
driverlist19 <- combn(vect19,2) %>% 
  t()%>%
  as.tibble()%>%
  rename(former = V1, latter = V2) 


#empty df filled by loop
df <- c()


loop_race <- 1 #sets loop counter for race number 

while (loop_race <= 22) { #22 while looop stops after 22 races
  
  for (i in 1:nrow(driverlist19)) {
    
    test <- overtakechance%>%filter(year==2019,race== loop_race, 
                                    latter %in% c(driverlist19[i,1], driverlist19[i,2]), #filter out two drivers
                                    former %in% c(c(driverlist19[i,1], driverlist19[i,2]))
    )
    test <- test%>% 
      arrange(adj_lap)%>%
      mutate(
        overtake_switch = ifelse(former == lag(latter) & latter == lag(former),1,0)) %>% #check if they overtook each other, former latter position changes
      filter(overtake_switch == 1) #filter out overtakes
    test
    
    df <- rbind(df,test) #bind all overtake rows together
  }
  
  loop_race <- loop_race + 1 #add 1 to the while loop counter
  
}


df_2019 <- df

#write.csv(df,"df_2019.csv", row.names = FALSE)

### create drivers list for 2020
vect20<-overtakechance%>%filter(year==2020)%>%summarise(unique(latter))%>%t()
driverlist20 <- combn(vect20,2) %>% 
  t()%>%
  as.tibble()%>%
  rename(former = V1, latter = V2) 


#empty df filled by loop
df <- c()

loop_race <- 1 #sets loop counter for race number 

while (loop_race <= 22) { #22 while looop stops after 22 races
  
  for (i in 1:nrow(driverlist20)) {
    
    test <- overtakechance%>%filter(year==2020,race== loop_race, 
                                    latter %in% c(driverlist20[i,1], driverlist20[i,2]), #filter out two drivers
                                    former %in% c(c(driverlist20[i,1], driverlist20[i,2]))
    )
    test <- test%>% 
      arrange(adj_lap)%>%
      mutate(
        overtake_switch = ifelse(former == lag(latter) & latter == lag(former),1,0)) %>% #check if they overtook each other, former latter position changes
      filter(overtake_switch == 1) #filter out overtakes
    test
    
    df <- rbind(df,test) #bind all overtake rows together
  }
  
  loop_race <- loop_race + 1 #add 1 to the while loop counter
  
}

df_2020 <- df

#write.csv(df,"df_2020.csv", row.names = FALSE)


### create drivers list for 2021
vect21<-overtakechance%>%filter(year==2021)%>%summarise(unique(latter))%>%t()
driverlist21 <- combn(vect21,2) %>% 
  t()%>%
  as.tibble()%>%
  rename(former = V1, latter = V2) 


#empty df filled by loop
df <- c()


loop_race <- 1 #sets loop counter for race number 

while (loop_race <= 22) { #22 while looop stops after 22 races
  
  for (i in 1:nrow(driverlist21)) {
    
    test <- overtakechance%>%filter(year==2021,race== loop_race, 
                                    latter %in% c(driverlist21[i,1], driverlist21[i,2]), #filter out two drivers
                                    former %in% c(c(driverlist21[i,1], driverlist21[i,2]))
    )
    test <- test%>% 
      arrange(adj_lap)%>%
      mutate(
        overtake_switch = ifelse(former == lag(latter) & latter == lag(former),1,0)) %>% #check if they overtook each other, former latter position changes
      filter(overtake_switch == 1) #filter out overtakes
    test
    
    df <- rbind(df,test) #bind all overtake rows together
  }
  
  loop_race <- loop_race + 1 #add 1 to the while loop counter
  
}


df_2021 <- df

#write.csv(df,"df_2021.csv", row.names = FALSE)

df_allyears <- rbind(df_2019, df_2020, df_2021)
#write.csv(df_allyears,"df_allyears.csv", row.names = FALSE)


######## Step 2: Label Overtake Success #########


#using laps where drivers switch position
switchpos<-read.csv("data/df_allyears_new.csv")
switchpos<-switchpos %>% select(race,year,former,latter,overtake_switch,adj_lap)
finaldata<-overtakechance %>% left_join(switchpos)

# join race info & label success
finaldata<-finaldata %>% mutate(success=case_when( # label success
  chance==1 & !is.na(leadlap)& leadlap!=adj_lap+1 & overtake_switch==1 ~ 1, 
  chance==1 & is.na(leadlap)& adj_lap!=total_lap & overtake_switch==1 ~1,
  chance==1 & same==1 ~1,
  TRUE~0))
finaldata<-finaldata %>% #join former
  left_join(race, by=c("year"="year","former"="drivernumber","race"="race")) %>% 
  select(-X) %>% 
  rename(former.team=teamname,former.position=position,former.gridposition=gridposition,former.status=status)
  
finaldata<-finaldata %>% #join latter
  left_join(race, by=c("year"="year","latter"="drivernumber","race"="race")) %>% 
  select(-X) %>% 
  rename(latter.team=teamname,latter.position=position,latter.gridposition=gridposition,latter.status=status)

finaldata_chance<-finaldata%>% 
  filter(chance==1)  #only focus on overtaking chance



#######Step3: Define relationship between two drivers #######

#load driver list
driver_list<-read.csv("data/drivers_text.csv") 

#create df with 190 rows for each driver interaction   
#loop for year 19-21
driver_1921<-foreach(year=2019:2021, .combine = "rbind") %do% {
  driver<-drivers[drivers$year==year,2]%>%as.vector()%>%t() #get vector of driver names
  driver_list <- combn(driver,2)%>% #create combination
    t()%>%
    as.tibble()%>%
    rename(driver_A = V1, driver_B = V2) %>%
    mutate(year=year,  #create year variable
           pair.num=row_number()) #create index within year
  driver_list
}

#### SCRAPING LOOP 
#create empty vector to store titles of Google Search
text<-c()
#total rows 
n=nrow(driver_1921)

#run loop to scrape
for (i in 1:n) {
  #create search url based on drivers name
  driver_1921$url[i] <- 
    paste0('https://www.google.com/search?q=f1+',driver_1921$year[i], '+',driver_1921$driver_A[i], '+',driver_1921$driver_B[i],'&num=20', sep = "")
  Sys.sleep(5)
  response <- read_html(driver_1921$url[i])   
  raw_text <- html_nodes(x = response,xpath = '//h3')
  text <- rbind(text,
                tibble(pair.num=driver_1921$pair.num[i],
                       year=driver_1921$year[i],
                       text=html_text(raw_text, trim = T)))
}


#join text to driver_list data
driver_list<-driver_1921%>%left_join(text,by=c("year","pair.num"))

#save dataset for later use
#write.csv(driver_list,"drivers_text.csv")

### Sentiment Analysis################
drivers_namenum <- read_xlsx("data/driver_1921.xlsx")

# let's convert to tidy text format, where each row is a word
driver_text <- driver_list %>% 
  unnest_tokens(word, text)

# let's get rid of stop words and numbers
data(stop_words)
driver_text <- driver_text %>%
  filter(!grepl('[0-9]', word))%>% # remove numbers
  anti_join(stop_words)%>%
  group_by(year,pair.num)%>%
  mutate(word.count=n()) #get number of words for each pair

# pull out only sentiment words
driver.sent<-driver_text %>%
  inner_join(get_sentiments("bing")) %>% 
  group_by(year,pair.num,word.count)%>%
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative words

#join back driver names
driver.sent<-driver_list%>%
  select(driver_A,driver_B,pair.num,year)%>%
  left_join(driver.sent)%>%
  distinct()

#create a relationship variable using sentimental analysis results
drivers_namenum<-drivers_namenum %>% 
  select(-year) %>% 
  filter(!duplicated(driver))
#get driver number info 
relationship<-driver.sent %>% 
  left_join(drivers_namenum,by=c("driver_A"="driver"))
relationship<-relationship %>% rename(driver_A_num="driver number")

relationship<-relationship %>% 
  left_join(drivers_namenum,by=c("driver_B"="driver")) %>% 
  rename(driver_B_num="driver number")

relationship2<-relationship%>%rename("driver_B_num"="driver_A_num",
                                     "driver_A_num"="driver_B_num")
relationship<-rbind(relationship,relationship2)

# select colns for joining
relationship<- relationship %>% select(year,sentiment,driver_A_num,driver_B_num)

#match driver in the full dataset for the relationship variable
finaldata_chance <-finaldata_chance %>% 
  left_join(relationship,by=c("year"="year","former"="driver_A_num",
                              "latter"="driver_B_num"))
  
#Now we have the final full dataset!  

########Section 3: Model Building #######

# keep predictors and target
finaldata_model<-finaldata_chance %>% 
  rename(relationship=sentiment)%>%
  select(-race,-year,-total_lap,-chance,-same,
         -overtake_switch,-leadlap,-former.stint,-former,-latter)%>%#remove cols not useful for prediction
  drop_na() #keep complete cases

# 75% of the sample size
set.seed(123)
split_size <- floor(0.75 * nrow(finaldata_model))
train_ind <- sample(seq(1:nrow(finaldata_model)), size = split_size)
train <- finaldata_model[train_ind, ]
test <- finaldata_model[-train_ind, ]

#logistic
model1<-glm(success~., data = train, family = 'binomial')
summary(model1)


#compute the AUC of this model on test.
test_LR<-test
test_LR$prediction<-predict(model1, test_LR, type='response')
test.pred <- prediction(test_LR$prediction, test_LR$success)
test.perf <- performance(test.pred, "auc")
cat('the auc score is ', test.perf@y.values[[1]], "\n")
#0.7305821 

#tree
rf_model <- ranger(success~.,
                   num.trees = 100, 
                   importance="impurity",
                   respect.unordered.factors=TRUE, 
                   probability=TRUE, data = train)
test_rf<-test
test_rf$prediction<- predict(rf_model, test_rf)$predictions[,2]
test.predrf <- prediction(test_rf$prediction, test_rf$success)
test.prefrf <- performance(test.predrf, "auc")
cat('the auc score is ',test.prefrf@y.values[[1]], "\n")

#0.865

#check factor importance
imp.names<-names(rf_model$variable.importance)
importance<-rf_model$variable.importance%>%data.frame()%>%rename("importance"=".")
importance<-data.frame(imp.names,importance)%>%arrange(desc(importance))
ggplot(importance, aes(x=reorder(imp.names,importance), y=importance,fill=importance))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Information Value Summary")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")

#AUC plots
perf_LR <- performance(test.pred, "tpr", "fpr")
plot(perf_LR,
     avg= "threshold",
     lwd= 3,
     main= "ROC Curve for Logistic Regression")
abline(coef = c(0, 1))

perf_RF <- performance(test.predrf, "tpr", "fpr")
plot(perf_RF,
     avg= "threshold",
     lwd= 3,
     main= "ROC Curve for Random Forest")
abline(coef = c(0, 1))


####End of Script ####



