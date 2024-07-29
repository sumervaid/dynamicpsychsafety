library(tidyverse)
library(psych)

ping1_data <- # (12 noon, Day N)
  read_csv("Daily Survey (5-minute) [Day N, 12 Noon]_June 27, 2024_07.34.csv") ### update these with then new file names of your Qualtrics download
ping2_data <- # (3 PM, Day N)
  read_csv("Daily Survey (5-minute) [Day N, 3 PM]_June 27, 2024_07.34.csv") ### update these with then new file names of your Qualtrics download
ping3_data <- # (6 PM, Day N)
  read_csv("Daily Survey - 7 minutes [Day N, 6 PM]_June 27, 2024_07.35.csv") ### update these with then new file names of your Qualtrics download

ping1 <- ping1_data %>%
  select(
    -c(
      "EndDate",
      "Status",
      "IPAddress",
      "Progress",
      "Finished",
      "RecordedDate",
      "ResponseId",
      "RecipientLastName",
      "RecipientFirstName",
      "ExternalReference",
      "LocationLatitude",
      "LocationLongitude",
      "DistributionChannel",
      "UserLanguage"
    )
  ) %>%
  slice(-c(1:2)) %>%
  ungroup() %>%
  dplyr::mutate(dplyr::across(
    contains("psych_safety"),
    ~ case_when(
      . == "Never or almost never" ~ 1,
      . == "Sometimes" ~ 2,
      . == "Frequently" ~ 3,
      . == "Always or almost always" ~ 4,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>%
  dplyr::mutate(dplyr::across(
    contains("team_performance"),
    ~ case_when(
      . == "Very Inaccurate" ~ 1,
      . == "Moderately Inaccurate" ~ 2,
      . == "Neither Accurate nor Accurate" ~ 3,
      . == "Moderately Accurate" ~ 4,
      . == "Very Accurate" ~ 5,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>%
  dplyr::mutate( ## recoding the reverse scored item
    team_performance_3 = case_when(
      team_performance_3 == "Very Inaccurate" ~ 5,
      team_performance_3 == "Moderately Inaccurate" ~ 4,
      team_performance_3 == "Neither Accurate nor Accurate" ~ 3,
      team_performance_3 == "Moderately Accurate" ~ 2, 
      team_performance_3 == "Very Accurate" ~ 1, 
      team_performance_3 == "Skip" ~ as.numeric(NA)
    )
  ) %>%
  dplyr::mutate(dplyr::across(  
    contains("primary_team_trust"),
    ~ case_when(
      . == "Completely disagree" ~ 1,
      . == "Somewhat disagree" ~ 2,
      . == "Neither agree nor disagree" ~ 3,
      . == "Somewhat agree" ~ 4,
      . == "Strongly agree" ~ 5,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>%
  dplyr::mutate(dplyr::across( 
    contains("work_engagement"),
    ~ case_when(
      . == "Strongly disagree" ~ 1,
      . == "Somewhat disagree" ~ 2,
      . == "Neither agree nor disagree" ~ 3,
      . == "Somewhat agree" ~ 4,
      . == "Strongly agree" ~ 5,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>%
  dplyr::mutate(
    work_engagement_8 = case_when( 
      work_engagement_8=="Strongly disagree" ~ 5,
      work_engagement_8== "Somewhat disagree" ~ 4,
      work_engagement_8=="Neither agree nor disagree" ~ 2,
      work_engagement_8=="Somewhat agree" ~ 3,
      work_engagement_8=="Strongly agree" ~ 1,
      work_engagement_8=="Skip" ~ as.numeric(NA),
    )) %>%
  dplyr::mutate(dplyr::across(
    contains("work_creativity"),
    ~ case_when(
      . == "Never" ~ 1,
      . == "Very Rarely" ~ 2,
      . == "Rarely" ~ 3,
      . == "Occasionally" ~ 4,
      . == "Very Frequently" ~ 5,
      . == "Always" ~ 6,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>%
  dplyr::mutate(dplyr::across(
    contains("big5_states"),
    ~ case_when(
      . == "Not well at all" ~ 1,
      . == "Slightly well" ~ 2,
      . == "Moderately well" ~ 3,
      . == "Very well" ~ 4,
      . == "Extremely well" ~ 5,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% ungroup() %>%
  rowwise() %>%
  dplyr::mutate(
    psych_safety = mean(
      c(
        psych_safety_1,
        psych_safety_2,
        psych_safety_3,
        psych_safety_4,
        psych_safety_5
      ),
      na.rm = TRUE
    ),
    team_performance = mean(
      c(
        team_performance_1,
        team_performance_2,
        team_performance_3,
        team_performance_4,
        team_performance_5
      ),
      na.rm = TRUE
    ),
    primary_team_trust = mean(
      c(
        primary_team_trust_1,
        primary_team_trust_2,
        primary_team_trust_3,
        primary_team_trust_4,
        primary_team_trust_5
      ),
      na.rm = TRUE
    ),
    work_engagement = mean(
      c(
        work_engagement_1,
        work_engagement_2,
        work_engagement_3,
        work_engagement_4,
        work_engagement_5,
        work_engagement_6,
        work_engagement_7,
        work_engagement_8
      ),
      na.rm = TRUE
    ),
    work_creativity = mean(
      c(
        work_creativity_1,
        work_creativity_2,
        work_creativity_3,
        work_creativity_4,
        work_creativity_5,
        work_creativity_6,
        work_creativity_7,
        work_creativity_8
      )
    )
  ) %>%
  ungroup() %>%
  dplyr::rename(
    bfi.e = big5_states_1,
    bfi.a = big5_states_2,
    bfi.c = big5_states_3,
    bfi.n = big5_states_4,
    bfi.o = big5_states_5
  ) %>%
  dplyr::select(
    StartDate,
    RecipientEmail,
    `Duration (in seconds)`,
    prolific_id,
    psych_safety,
    team_performance,
    primary_team_trust,
    work_engagement,
    work_creativity,
    bfi.e,
    bfi.a,
    bfi.n,
    bfi.c,
    bfi.o
  )

ping1$StartDate<-ymd_hms(ping1$StartDate)

ping1<- ping1%>%
  arrange(RecipientEmail, StartDate) %>%
  group_by(RecipientEmail) %>%
  dplyr::mutate(time_diff = round(as.numeric(StartDate - lag(StartDate), units = 'hours')),
                duration=as.numeric(`Duration (in seconds)`)) %>%
  arrange(RecipientEmail, StartDate) %>%
  dplyr::mutate(time_diff_fil = case_when(
    is.na(time_diff)==TRUE ~ 0, ##### the first group-specific for time diff will always be NA since there are no preceding values
    is.na(time_diff)==FALSE & time_diff>=21 ~ 0, ### retain if 21 or more hours have passed since response
    is.na(time_diff)==FALSE & time_diff<21 ~ 1 ### filter out if ping-specific responses are completed within 21 hours
  ),
  duration_fil=case_when(
    is.na(duration)==FALSE & duration <= 10800 ~ 0, ### retain if survey report took less than 3 hours to complete
    is.na(duration)==FALSE & duration > 10800 ~ 1 ### filter if survey report too more than 3 hours to complete
  ))


#nppt_filtered_ping1_toolong<-length(unique(nppt_filtered_ping1_toolong$RecipientEmail))


ping1_filtered<-ping1 %>%
  #dplyr::filter(time_diff_fil==0 | duration_fil==0) %>%
  dplyr::filter(StartDate>="2024-05-20 00:00:01")


ping2 <- ping2_data %>%
  select(
    -c(
      "EndDate",
      "Status",
      "IPAddress",
      "Progress",
      "Finished",
      "RecordedDate",
      "ResponseId",
      "RecipientLastName",
      "RecipientFirstName",
      "ExternalReference",
      "LocationLatitude",
      "LocationLongitude",
      "DistributionChannel",
      "UserLanguage"
    )
  ) %>%
  slice(-c(1:2)) %>%
  ungroup() %>%
  dplyr::mutate(dplyr::across(
    contains("psych_safety"),
    ~ case_when(
      . == "Never or almost never" ~ 1,
      . == "Sometimes" ~ 2,
      . == "Frequently" ~ 3,
      . == "Always or almost always" ~ 4,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>%
  dplyr::mutate(dplyr::across(
    contains("team_performance"),
    ~ case_when(
      . == "Very Inaccurate" ~ 1,
      . == "Moderately Inaccurate" ~ 2,
      . == "Neither Accurate nor Accurate" ~ 3,
      . == "Moderately Accurate" ~ 4,
      . == "Very Accurate" ~ 5,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>%
  dplyr::mutate( ## recoding the reverse scored item
    team_performance_3 = case_when(
      team_performance_3 == "Very Inaccurate" ~ 5,
      team_performance_3 == "Moderately Inaccurate" ~ 4,
      team_performance_3 == "Neither Accurate nor Accurate" ~ 3,
      team_performance_3 == "Moderately Accurate" ~ 2, 
      team_performance_3 == "Very Accurate" ~ 1, 
      team_performance_3 == "Skip" ~ as.numeric(NA)
    )
  ) %>%
  dplyr::mutate(dplyr::across(  
    contains("primary_team_trust"),
    ~ case_when(
      . == "Completely disagree" ~ 1,
      . == "Somewhat disagree" ~ 2,
      . == "Neither agree nor disagree" ~ 3,
      . == "Somewhat agree" ~ 4,
      . == "Strongly agree" ~ 5,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>%
  dplyr::mutate(dplyr::across( 
    contains("work_engagement"),
    ~ case_when(
      . == "Strongly disagree" ~ 1,
      . == "Somewhat disagree" ~ 2,
      . == "Neither agree nor disagree" ~ 3,
      . == "Somewhat agree" ~ 4,
      . == "Strongly disagree" ~ 5,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>%
  dplyr::mutate(
    work_engagement_8 = case_when( 
      work_engagement_8=="Strongly disagree" ~ 5,
      work_engagement_8== "Somewhat disagree" ~ 4,
      work_engagement_8=="Neither agree nor disagree" ~ 2,
      work_engagement_8=="Somewhat agree" ~ 3,
      work_engagement_8=="Strongly disagree" ~ 1,
      work_engagement_8=="Skip" ~ as.numeric(NA),
    )) %>%
  dplyr::mutate(dplyr::across(
    contains("work_creativity"),
    ~ case_when(
      . == "Never" ~ 1,
      . == "Very Rarely" ~ 2,
      . == "Rarely" ~ 3,
      . == "Occasionally" ~ 4,
      . == "Very Frequently" ~ 5,
      . == "Always" ~ 6,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>%
  dplyr::mutate(dplyr::across(
    contains("big5_states"),
    ~ case_when(
      . == "Not well at all" ~ 1,
      . == "Slightly well" ~ 2,
      . == "Moderately well" ~ 3,
      . == "Very well" ~ 4,
      . == "Extremely well" ~ 5,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% ungroup() %>%
  rowwise() %>%
  dplyr::mutate(
    psych_safety = mean(
      c(
        psych_safety_1,
        psych_safety_2,
        psych_safety_3,
        psych_safety_4,
        psych_safety_5
      ),
      na.rm = TRUE
    ),
    team_performance = mean(
      c(
        team_performance_1,
        team_performance_2,
        team_performance_3,
        team_performance_4,
        team_performance_5
      ),
      na.rm = TRUE
    ),
    primary_team_trust = mean(
      c(
        primary_team_trust_1,
        primary_team_trust_2,
        primary_team_trust_3,
        primary_team_trust_4,
        primary_team_trust_5
      ),
      na.rm = TRUE
    ),
    work_engagement = mean(
      c(
        work_engagement_1,
        work_engagement_2,
        work_engagement_3,
        work_engagement_4,
        work_engagement_5,
        work_engagement_6,
        work_engagement_7,
        work_engagement_8
      ),
      na.rm = TRUE
    ),
    work_creativity = mean(
      c(
        work_creativity_1,
        work_creativity_2,
        work_creativity_3,
        work_creativity_4,
        work_creativity_5,
        work_creativity_6,
        work_creativity_7,
        work_creativity_8
      )
    )
  ) %>%
  ungroup() %>%
  dplyr::rename(
    bfi.e = big5_states_1,
    bfi.a = big5_states_2,
    bfi.c = big5_states_3,
    bfi.n = big5_states_4,
    bfi.o = big5_states_5
  ) %>%
  dplyr::select(
    StartDate,
    RecipientEmail,
    `Duration (in seconds)`,
    prolific_id,
    psych_safety,
    team_performance,
    primary_team_trust,
    work_engagement,
    work_creativity,
    bfi.e,
    bfi.a,
    bfi.n,
    bfi.c,
    bfi.o
  )

ping2$StartDate<-ymd_hms(ping2$StartDate)

ping2<- ping2%>%
  arrange(RecipientEmail, StartDate) %>%
  group_by(RecipientEmail) %>%
  dplyr::mutate(time_diff = round(as.numeric(StartDate - lag(StartDate), units = 'hours')),
                duration=as.numeric(`Duration (in seconds)`)) %>%
  arrange(RecipientEmail, StartDate) %>%
  dplyr::mutate(time_diff_fil = case_when(
    is.na(time_diff)==TRUE ~ 0, ##### the first group-specific for time diff will always be NA since there are no preceding values
    is.na(time_diff)==FALSE & time_diff>=21 ~ 0, ### retain if 21 or more hours have passed since response
    is.na(time_diff)==FALSE & time_diff<21 ~ 1 ### filter out if ping-specific responses are completed within 21 hours
  ),
  duration_fil=case_when(
    is.na(duration)==FALSE & duration <= 10800 ~ 0, ### retain if survey report took less than 3 hours to complete
    is.na(duration)==FALSE & duration > 10800 ~ 1 ### filter if survey report too more than 3 hours to complete
  ))



#nppt_filtered_ping2_toolong<-length(unique(nppt_filtered_ping2_toolong$RecipientEmail))


ping2_filtered<-ping2 %>%
  #dplyr::filter(time_diff_fil==0 | duration_fil==0) %>%
  dplyr::filter(StartDate>="2024-05-20 00:00:01")

ping2_ppt_obs<-data.frame(setdiff(ping2, ping2_filtered))
ping2_filtered_obs<-length(ping2_ppt_obs$RecipientEmail)
ping2_filtered_ppt<-length(unique(ping2_ppt_obs$RecipientEmail))



ping3 <- ping3_data %>%
  select(
    -c(
      "EndDate",
      "Status",
      "IPAddress",
      "Progress",
      "Finished",
      "RecordedDate",
      "ResponseId",
      "RecipientLastName",
      "RecipientFirstName",
      "ExternalReference",
      "LocationLatitude",
      "LocationLongitude",
      "DistributionChannel",
      "UserLanguage"
    )
  ) %>%
  slice(-c(1:2)) %>%
  ungroup() %>%
  dplyr::mutate(dplyr::across(
    contains("psych_safety"),
    ~ case_when(
      . == "Never or almost never" ~ 1,
      . == "Sometimes" ~ 2,
      . == "Frequently" ~ 3,
      . == "Always or almost always" ~ 4,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>%
  dplyr::mutate(dplyr::across(
    contains("team_performance"),
    ~ case_when(
      . == "Very Inaccurate" ~ 1,
      . == "Moderately Inaccurate" ~ 2,
      . == "Neither Accurate nor Accurate" ~ 3,
      . == "Moderately Accurate" ~ 4,
      . == "Very Accurate" ~ 5,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>%
  dplyr::mutate( ## recoding the reverse scored item
    team_performance_3 = case_when(
      team_performance_3 == "Very Inaccurate" ~ 5,
      team_performance_3 == "Moderately Inaccurate" ~ 4,
      team_performance_3 == "Neither Accurate nor Accurate" ~ 3,
      team_performance_3 == "Moderately Accurate" ~ 2, 
      team_performance_3 == "Very Accurate" ~ 1, 
      team_performance_3 == "Skip" ~ as.numeric(NA)
    )
  ) %>%
  dplyr::mutate(dplyr::across(  
    contains("primary_team_trust"),
    ~ case_when(
      . == "Completely disagree" ~ 1,
      . == "Somewhat disagree" ~ 2,
      . == "Neither agree nor disagree" ~ 3,
      . == "Somewhat agree" ~ 4,
      . == "Strongly agree" ~ 5,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>%
  dplyr::mutate(dplyr::across( 
    contains("work_engagement"),
    ~ case_when(
      . == "Strongly disagree" ~ 1,
      . == "Somewhat disagree" ~ 2,
      . == "Neither agree nor disagree" ~ 3,
      . == "Somewhat agree" ~ 4,
      . == "Strongly disagree" ~ 5,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>%
  dplyr::mutate(
    work_engagement_8 = case_when( 
      work_engagement_8=="Strongly disagree" ~ 5,
      work_engagement_8== "Somewhat disagree" ~ 4,
      work_engagement_8=="Neither agree nor disagree" ~ 2,
      work_engagement_8=="Somewhat agree" ~ 3,
      work_engagement_8=="Strongly disagree" ~ 1,
      work_engagement_8=="Skip" ~ as.numeric(NA),
  )) %>%
  dplyr::mutate(dplyr::across(  
    contains("team_learning"),
    ~ case_when(
      . == "Completely disagree" ~ 1,
      . == "Somewhat disagree" ~ 2,
      . == "Neither agree nor disagree" ~ 3,
      . == "Somewhat agree" ~ 4,
      . == "Completely agree" ~ 5,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>%
  dplyr::mutate(dplyr::across(
    contains("work_creativity"),
    ~ case_when(
      . == "Never" ~ 1,
      . == "Very Rarely" ~ 2,
      . == "Rarely" ~ 3,
      . == "Occasionally" ~ 4,
      . == "Very Frequently" ~ 5,
      . == "Always" ~ 6,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>%
  dplyr::mutate(dplyr::across(
    contains("big5_states"),
    ~ case_when(
      . == "Not well at all" ~ 1,
      . == "Slightly well" ~ 2,
      . == "Moderately well" ~ 3,
      . == "Very well" ~ 4,
      . == "Extremely well" ~ 5,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% ungroup() %>%
  rowwise() %>%
  dplyr::mutate(
    psych_safety = mean(
      c(
        psych_safety_1,
        psych_safety_2,
        psych_safety_3,
        psych_safety_4,
        psych_safety_5
      ),
      na.rm = TRUE
    ),
    team_performance = mean(
      c(
        team_performance_1,
        team_performance_2,
        team_performance_3,
        team_performance_4,
        team_performance_5
      ),
      na.rm = TRUE
    ),
    primary_team_trust = mean(
      c(
        primary_team_trust_1,
        primary_team_trust_2,
        primary_team_trust_3,
        primary_team_trust_4,
        primary_team_trust_5
      ),
      na.rm = TRUE
    ),
    work_engagement = mean(
      c(
        work_engagement_1,
        work_engagement_2,
        work_engagement_3,
        work_engagement_4,
        work_engagement_5,
        work_engagement_6,
        work_engagement_7,
        work_engagement_8
      ),
      na.rm = TRUE
    ),
    work_creativity = mean(
      c(
        work_creativity_1,
        work_creativity_2,
        work_creativity_3,
        work_creativity_4,
        work_creativity_5,
        work_creativity_6,
        work_creativity_7,
        work_creativity_8
      )
    ),
    team_learning=mean(
      c(
        team_learning_1,
        team_learning_2,
        team_learning_3,
        team_learning_4,
        team_learning_5,
        team_learning_6,
        team_learning_7,
        team_learning_8,
        team_learning_9,
        team_learning_10,
        team_learning_11,
        team_learning_12,
        team_learning_13,
        team_learning_14,
        team_learning_15,
        team_learning_16,
        team_learning_17,
        team_learning_18,
        team_learning_19,
        team_learning_20,
        team_learning_21,
        team_learning_22,
        team_learning_23,
        team_learning_24,
        team_learning_25,
        team_learning_26,
        team_learning_27,
        team_learning_28
      )
    )
  ) %>%
  ungroup() %>%
  dplyr::rename(
    bfi.e = big5_states_1,
    bfi.a = big5_states_2,
    bfi.c = big5_states_3,
    bfi.n = big5_states_4,
    bfi.o = big5_states_5,
    ritual_occurence = occurence,
    ritual_time = ritual_time, 
    ritual_activity = activity,
    ritual_purpose = purpose,
    ritual_origin = origin,
    rituality = rituality, 
    ritual_meaningfulness = meaningfulness
  ) %>%
  dplyr::select(
    StartDate,
    RecipientEmail,
    `Duration (in seconds)`,
    prolific_id,
    psych_safety,
    team_performance,
    primary_team_trust,
    work_engagement,
    work_creativity,
    team_learning,
    bfi.e,
    bfi.a,
    bfi.n,
    bfi.c,
    bfi.o, 
    ritual_occurence,
    ritual_time,
    ritual_time,
    ritual_activity,
    ritual_purpose,
    ritual_origin,
    rituality,
    ritual_meaningfulness
  )

ping3$StartDate<-ymd_hms(ping3$StartDate)

ping3<- ping3%>%
  arrange(RecipientEmail, StartDate) %>%
  group_by(RecipientEmail) %>%
  dplyr::mutate(time_diff = round(as.numeric(StartDate - lag(StartDate), units = 'hours')),
                duration=as.numeric(`Duration (in seconds)`)) %>%
  arrange(RecipientEmail, StartDate) %>%
  dplyr::mutate(time_diff_fil = case_when(
    is.na(time_diff)==TRUE ~ 0, ##### the first group-specific for time diff will always be NA since there are no preceding values
    is.na(time_diff)==FALSE & time_diff>=21 ~ 0, ### retain if 21 or more hours have passed since response
    is.na(time_diff)==FALSE & time_diff<21 ~ 1 ### filter out if ping-specific responses are completed within 21 hours
  ),
  duration_fil=case_when(
    is.na(duration)==FALSE & duration <= 10800 ~ 0, ### retain if survey report took less than 3 hours to complete
    is.na(duration)==FALSE & duration > 10800 ~ 1 ### filter if survey report too more than 3 hours to complete
  ))


ping3_filtered<-ping3 %>%
  #dplyr::filter(time_diff_fil==0 | duration_fil==0) %>%
  dplyr::filter(StartDate>="2024-05-20 00:00:01")

ping3_ppt_obs<-data.frame(setdiff(ping3, ping3_filtered))
ping3_filtered_obs<-length(ping3_ppt_obs$RecipientEmail)
ping3_filtered_ppt<-length(unique(ping3_ppt_obs$RecipientEmail))


library(jtools)
ggplot(data=ping3, aes(x=team_learning)) + geom_histogram() + xlab("Team Learning") + ylab("Count") +  geom_vline(aes(xintercept = mean(na.omit(team_learning))),col='gray',size=2) + geom_vline(aes(xintercept=sd(na.omit(team_learning))), col='black',size=2) + theme_apa()


inspect3<-ping3 %>%
  select(prolific_id, StartDate, time_diff)

data <- rbind(ping1_filtered, ping2_filtered, ping3_filtered)

data$StartDate <- ymd_hms(data$StartDate)

data <- data %>%
  arrange(prolific_id, StartDate) %>%
  ungroup() %>%
  group_by(prolific_id) %>%
  dplyr::mutate(count=n()) %>%
  ungroup() %>%
  dplyr::filter(
    RecipientEmail !="svaid@hbs.edu" #filter out test respondents
  ) 


##### Filtering out people who did not report any change in the mean psychological safety across observations
data_filtered<- data %>%
  ungroup() %>%
  group_by(RecipientEmail) %>%
  mutate(
    sd_psych_safety=sd(psych_safety, na.rm=TRUE)) %>%
  ungroup() %>%
  dplyr::mutate(
    psych_safety_const_filter=case_when(
      is.na(sd_psych_safety)==FALSE & sd_psych_safety==0 ~ 1, 
      is.na(sd_psych_safety)==FALSE & sd_psych_safety>0 ~ 0)
  ) %>%
  dplyr::filter(time_diff_fil==0 & duration_fil==0 & psych_safety_const_filter==0) %>%
  ungroup() %>%
  group_by(RecipientEmail) %>%
  dplyr::mutate(
    count=n(),
    clus_size_filter=case_when(
      is.na(count)==FALSE & count< 5 ~ 1, ### as per pre-registration, filtering out people with fewer than 5 observations across the study
      is.na(count)==FALSE & count> 5 ~ 0
  ))

filtered_respondents<-n_distinct(data$RecipientEmail)-n_distinct(data_filtered$RecipientEmail)
filtered_observations<-nrow(data)-nrow(data_filtered)


### Computing Response Rate
n_respondents<-length(unique(data_filtered$prolific_id))
days<-7
obs_per_day<-3
max_obs_ppt<-days*obs_per_day
max_obs<-max_obs_ppt*n_respondents
actual_obs<- nrow(distinct(data))
completion_rate<-(actual_obs/max_obs)*100


 # dplyr::mutate(
  #  obs_ppt_filter=case_when(
   #   count<=5 ~ 1,
    #  count>5 ~ 0))


data<-data_filtered %>%
  ungroup () %>%
  group_by(prolific_id) %>%
  mutate(
    lag_psych_safety=lag(psych_safety)
  )


data_filtered <- data_filtered %>%
  mutate(date = format(StartDate, "%Y-%m-%d")) %>%
  mutate(
    ritual_occurence = case_when(
      ritual_occurence == "Yes (Indicate the number of time such activities occurred):" ~ 1,
      ritual_occurence == "No" ~ 0,
      ritual_occurence == "Skip" ~ as.numeric(NA)
    ),
    rituality = case_when(
      rituality == "Not at all" ~ 1,
      rituality == "Low" ~ 2,
      rituality == "Slightly" ~ 3,
      rituality == "Neutral" ~ 4,
      rituality == "Moderately" ~ 5,
      rituality == "Very" ~ 6,
      rituality == "Extremely" ~ 7
    ),
    ritual_meaningfulness = case_when(
      ritual_meaningfulness == "Not at all" ~ 1,
      ritual_meaningfulness == "Low" ~ 2,
      ritual_meaningfulness == "Slightly" ~ 3,
      ritual_meaningfulness == "Neutral" ~ 4,
      ritual_meaningfulness == "Moderately" ~ 5,
      ritual_meaningfulness == "Very" ~ 6,
      ritual_meaningfulness == "Extremely" ~ 7
    )
  )

#### Date Wrangling and Study Day Labelling

data_filtered<-data_filtered %>%
  mutate(date=date(StartDate)) %>%
  ungroup() %>%
  mutate(
    study_day=case_when(
      date=="2024-05-20" ~ 1,
      date=="2024-05-21" ~ 2,
      date=="2024-05-22" ~ 3,
      date=="2024-05-23" ~ 4,
      date=="2024-05-24" ~ 5,
      date=="2024-05-28" ~ 6,
      date=="2024-05-29" ~ 7,
      date=="2024-06-17" ~ 1,
      date=="2024-06-18" ~ 2,
      date=="2024-06-20" ~ 3,
      date=="2024-06-21" ~ 4,
      date=="2024-06-24" ~ 5,
      date=="2024-06-25" ~ 6,
      date=="2024-06-26" ~ 7,
    )
  ) %>%
  filter(!is.na(study_day)) %>%
  arrange(RecipientEmail, StartDate) %>%
  group_by(RecipientEmail) %>%
  mutate(
    lag_psych_safety=lag(psych_safety)
  )


##### Generate Descriptive Statistics for EMA Data
diary_descr_avgbp<-data_filtered %>%
  ungroup() %>%
  select(-c(StartDate, RecipientEmail, prolific_id, contains("ritual"), contains("fil"))) %>%
  psych::describe()


##### Generate Mplus File for Psych Safety analysis
library(MplusAutomation)
mplus_data<-data_filtered %>%
  arrange(RecipientEmail, study_day) %>%
  ungroup() %>%
  group_by(RecipientEmail) %>%
  mutate(
    id=cur_group_id()
  ) %>% ungroup() %>% select(-c(StartDate, RecipientEmail, date, `Duration (in seconds)`, `prolific_id`,
              contains("ritual"), count, sd_psych_safety, psych_safety_const_filter, clus_size_filter, time_diff_fil, duration_fil, time_diff, duration))

#prepareMplusData(mplus_data, filename="diary_data.dat")

### Inspect Mplus Data to make sure everything is airtight
library(haven)
inspect<-read.table("diary_data.dat")

#### Generate MPlus file for Ritual analysis
rituals_mplus<-data_filtered %>%
  arrange(RecipientEmail, study_day) %>%
  ungroup() %>%
  group_by(RecipientEmail) %>%
  mutate(
    id=cur_group_id()) %>%
      ungroup()%>% select(-c(psych_safety, ritual_origin, ritual_purpose, ritual_time, ritual_activity, bfi.e, bfi.a, bfi.c, bfi.o, StartDate, RecipientEmail, date, `Duration (in seconds)`, `prolific_id`,
                count, sd_psych_safety, psych_safety_const_filter, clus_size_filter, time_diff_fil, duration_fil, time_diff, duration)) %>%
  rename(
    ocr=ritual_occurence, 
    rit=rituality,
    eng=work_engagement,
    perf=team_performance,
    trust=primary_team_trust,
    cre=work_creativity
  ) %>%
  drop_na(ocr)

  
prepareMplusData(rituals_mplus, filename="ritual_data.dat")

### Output the filtered daily diary dataset as a .csv file 

data_filtered<-data_filtered %>%
  group_by(RecipientEmail) %>%
  mutate(
    id=cur_group_id()
  )

library(lme4)
library(lmerTest)
model1<-lmer(
  ritual_meaningfulness ~ bfi.e + (1|prolific_id), data=ritual_data
)

summary(model1)

#write.csv(data_filtered, "ema_data.csv")



