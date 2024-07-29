library(tidyverse)
library(lubridate)

#### Reading in the baseline survey
baseline_managers_data <-
  read_csv("Baseline Survey - Study 2 - Managers_July 3, 2024_02.28.csv") %>%
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
      "UserLanguage",
      "Q_RecaptchaScore"
    )
  ) %>% slice(-c(1:2))


baseline_managers <- baseline_managers_data %>%
  dplyr::mutate(dplyr::across(
    contains("psych_empow"),
    ~ case_when(
      . == "1\nVery strongly disagree" ~ 1,
      . == "2\n Strongly disagree" ~ 2,
      . == "3\nDisagree" ~ 3,
      . == "4\nNeutral" ~ 4,
      . == "5\nAgree\n"  ~ 5,
      . == "6\nStrongly agree" ~ 6,
      . == "7\nVery strongly agree" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  ))  %>% dplyr::mutate(dplyr::across(
    contains("job_sat"),
    ~ case_when(
      . == "1\nStrongly Disagree\n" ~ 1,
      . == "2\n Disagree"  ~ 2,
      . == "3\nSomewhat disagree" ~ 3,
      . == "4\nNeither agree nor disagree" ~ 4,
      . == "5\nAgree\n"  ~ 5,
      . == "6\nSomewhat agree" ~ 6,
      . == "7\nStrongly agree" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    c("job_sat_3", "job_sat_5"),
    ~ case_when(
      . == 1 ~ 7,
      . == 2 ~ 6,
      . == 3 ~ 5,
      . == 4 ~ 4,
      . == 5 ~ 3,
      . == 6 ~ 2,
      . == 7 ~ 1,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    contains("work_engagement"),
    ~ case_when(
      . == "1\nAlmost Never" ~ 1,
      . == "2\nRarely"   ~ 2,
      . == "3\nSometimes"  ~ 3,
      . == "4\nOften" ~ 4,
      . == "5\nVery often"  ~ 5,
      . == "6\nAlways" ~ 6,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    c("work_engagement_13", "work_engagement_14"),
    ~ case_when(
      . == 1 ~ 6,
      . == 2 ~ 5,
      . == 3 ~ 4,
      . == 4 ~ 3,
      . == 5 ~ 2,
      . == 6 ~ 1,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    contains("team_learning"),
    ~ case_when(
      . == "Very inaccurate \n1\n \n" ~ 1,
      . == "Inaccurate\n2\n"   ~ 2,
      . == "Moderately inaccurate\n3\n"  ~ 3,
      . == "Neither accurate nor inaccurate\n4\n" ~ 4,
      . == "Moderately accurate\n5\n" ~ 5,
      . == "Accurate\n6\n" ~ 6,
      . == "Very accurate\n7\n" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  ))  %>% dplyr::mutate(dplyr::across(
    contains("psych_safety"),
    ~ case_when(
      . == "Very inaccurate \n1\n \n" ~ 1,
      . == "Inaccurate\n2\n"   ~ 2,
      . == "Moderately inaccurate\n3\n"  ~ 3,
      . == "Neither accurate nor inaccurate\n4\n" ~ 4,
      . == "Moderately accurate\n5\n" ~ 5,
      . == "Accurate\n6\n" ~ 6,
      . == "Very accurate\n7\n" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    contains(c(
      "psych_safety_1", "psych_safety_3", "psych_safety_5"
    )),
    ~ case_when(
      . == 1 ~ 7,
      . == 2 ~ 6,
      . == 3 ~ 5,
      . == 4 ~ 4,
      . == 5 ~ 3,
      . == 6 ~ 2,
      . == 7 ~ 1,
      . == "Skip" ~ as.numeric(NA)
    )
  ))  %>% dplyr::mutate(dplyr::across(
    contains("voice_helping"),
    ~ case_when(
      . == "1\nStrongly Disagree\n" ~ 1,
      . == "2\n Disagree"  ~ 2,
      . == "3\nSomewhat disagree" ~ 3,
      . == "4\nNeither agree nor disagree" ~ 4,
      . == "5\nAgree\n"  ~ 5,
      . == "6\nSomewhat agree" ~ 6,
      . == "7\nStrongly agree" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    contains("team_trust"),
    ~ case_when(
      . == "1\nStrongly Disagree\n" ~ 1,
      . == "2\n Disagree"  ~ 2,
      . == "3\nSomewhat disagree" ~ 3,
      . == "4\nNeither agree nor disagree" ~ 4,
      . == "5\nAgree\n"  ~ 5,
      . == "6\nSomewhat agree" ~ 6,
      . == "7\nStrongly agree" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    contains("tipi"),
    ~ case_when(
      . == "1\nStrongly Disagree\n" ~ 1,
      . == "2\n Disagree"  ~ 2,
      . == "3\nSomewhat disagree" ~ 3,
      . == "4\nNeither agree nor disagree" ~ 4,
      . == "5\nAgree\n"  ~ 5,
      . == "6\nSomewhat agree" ~ 6,
      . == "7\nStrongly agree" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    contains("panas"),
    ~ case_when(
      . == "1\nVery Rarely or Never" ~ 1,
      . == "2\nRarely" ~ 2,
      . == "3\nSometimes\n" ~ 3,
      . == "4 \nOften"  ~ 4,
      . == "5\nAlways or almost always\n \n"  ~ 5,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>%
  dplyr::mutate(dplyr::across(
    contains("org_culture"),
    ~ case_when(
      . == "Never" ~ 1,
      . == "Sometimes" ~ 2,
      . == "Often" ~ 3,
      . == "Always"  ~ 4,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>%
  dplyr::mutate(sex = case_when(Gender == "Female" ~ 1,
                                Gender == "Male" ~ 0),
                age = as.numeric(Age)) %>%
  dplyr::mutate(
    married = case_when(
      marital_status == "No, I am not married and/or in a marriage-like relationship" ~ "Yes",
      marital_status == "Yes, I am married and/or in a marriage-like relationship" ~ "No",
      marital_status == "Skip" ~ NA
    ),
    family_income_coded = case_when(
      family_income == "Don't Know" ~ NA,
      family_income == "Skip" ~ NA,
      TRUE ~ family_income
    ),
    mother_education_coded = case_when(
      mother_education == "Some grade school" ~ 1,
      mother_education == "Grade school" ~ 2,
      mother_education == "Some high school" ~ 3,
      mother_education == "High school/GED" ~ 4,
      mother_education == "Some college" ~ 5,
      mother_education == "Completed college" ~ 6,
      mother_education == "Post-grad degree" ~ 7,
      mother_education == "Skip" ~ as.numeric(NA)
    ),
    father_education_coded = case_when(
      father_education == "Some grade school" ~ 1,
      father_education == "Grade school" ~ 2,
      father_education == "Some high school" ~ 3,
      father_education == "High school/GED" ~ 4,
      father_education == "Some college" ~ 5,
      father_education == "Completed college" ~ 6,
      father_education == "Post-grad degree" ~ 7,
      father_education == "Skip" ~ as.numeric(NA)
    ),
    race_coded_one = case_when(
      Race == "Prefer not to say" ~ NA,
      str_detect(Race, "White or Caucasian") ~ "White",
      str_detect(Race, "Black or African American") ~ "Black",
      str_detect(Race, "American Indian/Native American or Alaska Native") ~ "Native American",
      str_detect(Race, "Asian") ~ "Asian",
      str_detect(
        Race,
        "White or Caucasian,American Indian/Native American or Alaska Native"
      ) ~ "White & Native American (Biracial)",
      str_detect(Race, "White or Caucasian,Black or African American,Other") ~
        "White, Black and Hispanic (Multiethnic)",
      str_detect(Race, "White or Caucasian,Black or African American") ~
        "White & Black (Biracial)",
      str_detect(Race, "White or Caucasian,Asian") ~ "White & Asian (Biracial)",
      str_detect(Race_6_TEXT, "Hispanic") ~ "Hispanic",
      str_detect(Race_6_TEXT, "hispanic") ~ "Hispanic",
      str_detect(Race_6_TEXT, "Latina") ~ "Hispanic",
      str_detect(Race_6_TEXT, "Hispanic and Filipino") ~ "Hispanic",
      str_detect(Race_6_TEXT, "hispanic") ~ "Hispanic",
      str_detect(Race_6_TEXT, "Spanish") ~ "Hispanic",
      str_detect(Race_6_TEXT, "Middle Eastern") ~ "Middle Eastern",
      TRUE ~ Race
    ),
    race_coded = case_when(
      Race == "Other" & race_coded_one == "Hispanic" ~ "Hispanic",
      Race == "Other" &
        race_coded_one == "Middle Eastern" ~ "Middle Eastern",
      str_detect(Race, ",Other") &
        race_coded_one == "Asian" &
        str_detect(Race_6_TEXT, "Hispanic") ~ "Asian & Hispanic (Biracial)",
      str_detect(Race, ",Other") &
        race_coded_one == "White" &
        str_detect(Race_6_TEXT, "Spanish") ~ "White & Hispanic (Biracial)",
      str_detect(Race, "White or Caucasian,Black or African American,Other") &
        str_detect(Race_6_TEXT, "Hispanic") ~ "White, Black & Hispanic (Multiethnic)",
      str_detect(Race, "Asian,Other") &
        str_detect(Race_6_TEXT, "Hispanic and Filipino") ~ "Asian & Hispanic (Biracial)",
      TRUE ~ race_coded_one
    ),
    attention_check_1_pass = if_else(attention_check_1 == "$10,000", 1, 0),
    attention_check_2_pass = if_else(attention_check_2...152 == "$90,000", 1, 0),
    attention_check_3_pass = if_else(attention_check_2...37 == "$90,000", 1, 0),
    study_datetime = ymd_hms(StartDate),
    study_date = date(study_datetime),
    company_start_date = mdy(primary_job_tenure),
    tenure_days = round(difftime(study_date, company_start_date, units = 'days'), 1),
    tenure_years = as.numeric(round(tenure_days / 365, 2)),
    performancepay = case_when(
      pfp == "No I do not receive performance based pay." ~ "No",
      pfp == "Yes, I receive performance based pay." ~ "Yes",
      pfp == "Skip" ~ NA
    )
  ) %>%
  ungroup() %>%
  rowwise() %>%
  dplyr::transmute(
    StartDate,
    tenure_years,
    performancepay,
    sex,
    age,
    married,
    family_income_coded,
    mother_education_coded,
    father_education_coded,
    race_coded,
    prolific_id,
    attention_check_1_pass,
    attention_check_2_pass,
    attention_check_3_pass,
    psych_empowerment = mean(
      c(
        psych_empow_1,
        psych_empow_2,
        psych_empow_3,
        psych_empow_4,
        psych_empow_5,
        psych_empow_6,
        psych_empow_7,
        psych_empow_8,
        psych_empow_9,
        psych_empow_10,
        psych_empow_11,
        psych_empow_12
      ),
      na.rm = TRUE
    ),
    job_satisfaction = mean(
      c(job_sat_1,
        job_sat_2,
        job_sat_3,
        job_sat_4,
        job_sat_5),
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
        work_engagement_8,
        work_engagement_9,
        work_engagement_10,
        work_engagement_11,
        work_engagement_12,
        work_engagement_13,
        work_engagement_14,
        work_engagement_15,
        work_engagement_16,
        work_engagement_17
      ),
      na.rm = TRUE
    ),
    team_learning = mean(
      c(
        team_learning_1,
        team_learning_2,
        team_learning_3,
        team_learning_4,
        team_learning_5,
        team_learning_6,
        team_learning_7
      ),
      na.rm = TRUE
    ),
    psychological_safety = mean(
      c(
        psych_safety_1,
        psych_safety_2,
        psych_safety_3,
        psych_safety_4,
        psych_safety_5,
        psych_safety_6,
        psych_safety_7
      ),
      na.rm = TRUE
    ),
    helping_behaviors = mean(
      c(
        voice_helping_1,
        voice_helping_2,
        voice_helping_3,
        voice_helping_4,
        voice_helping_5,
        voice_helping_6,
        voice_helping_7,
        voice_helping_8
      ),
      na.rm = TRUE
    ),
    voice_behaviors = mean(
      c(
        voice_helping_9,
        voice_helping_10,
        voice_helping_11,
        voice_helping_12,
        voice_helping_13,
        voice_helping_14
      ),
      na.rm = TRUE
    ),
    team_trust = mean(
      c(
        team_trust_1,
        team_trust_2,
        team_trust_3,
        team_trust_4,
        team_trust_5
      ),
      na.rm = TRUE
    ),
    org_culture_encouragement = mean(
      c(
        org_culture_1,
        org_culture_2,
        org_culture_3,
        org_culture_4,
        org_culture_5,
        org_culture_6,
        org_culture_7,
        org_culture_8,
        org_culture_9,
        org_culture_10,
        org_culture_2_1,
        org_culture_2_2,
        org_culture_2_3,
        org_culture_2_4,
        org_culture_2_5
      ),
      na.rm = TRUE
    ),
    org_culture_creativity = mean(
      c(
        org_culture_2_6,
        org_culture_2_7,
        org_culture_2_8,
        org_culture_2_9,
        org_culture_2_10,
        org_culture_2_11
      ),
      na.rm = TRUE
    ),
    attention_check_sum = sum(
      c(
        attention_check_1_pass,
        attention_check_2_pass,
        attention_check_3_pass
      ),
      na.rm = TRUE
    )
  )

i <- baseline_managers %>%
  select(prolific_id, contains("attention"))

baseline_nonmanagers_data <-
  read_csv("Baseline Survey - Study 2 - NonManagers_July 3, 2024_02.27.csv") %>%
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
      "UserLanguage",
      "Q_RecaptchaScore"
    )
  ) %>%  slice(-c(1:2))


baseline_nonmanagers <- baseline_nonmanagers_data %>%
  dplyr::mutate(dplyr::across(
    contains("psych_empow"),
    ~ case_when(
      . == "1\nVery strongly disagree" ~ 1,
      . == "2\n Strongly disagree" ~ 2,
      . == "3\nDisagree" ~ 3,
      . == "4\nNeutral" ~ 4,
      . == "5\nAgree\n"  ~ 5,
      . == "6\nStrongly agree" ~ 6,
      . == "7\nVery strongly agree" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  ))  %>% dplyr::mutate(dplyr::across(
    contains("job_sat"),
    ~ case_when(
      . == "1\nStrongly Disagree\n" ~ 1,
      . == "2\n Disagree"  ~ 2,
      . == "3\nSomewhat disagree" ~ 3,
      . == "4\nNeither agree nor disagree" ~ 4,
      . == "5\nAgree\n"  ~ 5,
      . == "6\nSomewhat agree" ~ 6,
      . == "7\nStrongly agree" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    c("job_sat_3", "job_sat_5"),
    ~ case_when(
      . == 1 ~ 7,
      . == 2 ~ 6,
      . == 3~ 5,
      . == 4 ~ 4,
      . == 5 ~ 3,
      . == 6 ~ 2,
      . == 7 ~ 1,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    contains("work_engagement"),
    ~ case_when(
      . == "1\nAlmost Never" ~ 1,
      . == "2\nRarely"   ~ 2,
      . == "3\nSometimes"  ~ 3,
      . == "4\nOften" ~ 4,
      . == "5\nVery often"  ~ 5,
      . == "6\nAlways" ~ 6,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    c("work_engagement_13", "work_engagement_14"),
    ~ case_when(
      . == 1 ~ 6,
      . == 2 ~ 5,
      . == 3 ~ 4,
      . == 4 ~ 3,
      . == 5 ~ 2,
      . == 6 ~ 1,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    contains("team_learning"),
    ~ case_when(
      . == "Very inaccurate \n1\n \n" ~ 1,
      . == "Inaccurate\n2\n"   ~ 2,
      . == "Moderately inaccurate\n3\n"  ~ 3,
      . == "Neither accurate nor inaccurate\n4\n" ~ 4,
      . == "Moderately accurate\n5\n" ~ 5,
      . == "Accurate\n6\n" ~ 6,
      . == "Very accurate\n7\n" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  ))  %>% dplyr::mutate(dplyr::across(
    contains("psych_safety"),
    ~ case_when(
      . == "Very inaccurate \n1\n \n" ~ 1,
      . == "Inaccurate\n2\n"   ~ 2,
      . == "Moderately inaccurate\n3\n"  ~ 3,
      . == "Neither accurate nor inaccurate\n4\n" ~ 4,
      . == "Moderately accurate\n5\n" ~ 5,
      . == "Accurate\n6\n" ~ 6,
      . == "Very accurate\n7\n" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    contains(c(
      "psych_safety_1", "psych_safety_3", "psych_safety_5"
    )),
    ~ case_when(
      . == 1 ~ 7,
      . == 2 ~ 6,
      . == 3 ~ 5,
      . == 4 ~ 4,
      . == 5 ~ 3,
      . == 6 ~ 2,
      . == 7 ~ 1,
      . == "Skip" ~ as.numeric(NA)
    )
  ))  %>% dplyr::mutate(dplyr::across(
    contains("voice_helping"),
    ~ case_when(
      . == "1\nStrongly Disagree\n" ~ 1,
      . == "2\n Disagree"  ~ 2,
      . == "3\nSomewhat disagree" ~ 3,
      . == "4\nNeither agree nor disagree" ~ 4,
      . == "5\nAgree\n"  ~ 5,
      . == "6\nSomewhat agree" ~ 6,
      . == "7\nStrongly agree" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    contains("team_trust"),
    ~ case_when(
      . == "1\nStrongly Disagree\n" ~ 1,
      . == "2\n Disagree"  ~ 2,
      . == "3\nSomewhat disagree" ~ 3,
      . == "4\nNeither agree nor disagree" ~ 4,
      . == "5\nAgree\n"  ~ 5,
      . == "6\nSomewhat agree" ~ 6,
      . == "7\nStrongly agree" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    contains("tipi"),
    ~ case_when(
      . == "1\nStrongly Disagree\n" ~ 1,
      . == "2\n Disagree"  ~ 2,
      . == "3\nSomewhat disagree" ~ 3,
      . == "4\nNeither agree nor disagree" ~ 4,
      . == "5\nAgree\n"  ~ 5,
      . == "6\nSomewhat agree" ~ 6,
      . == "7\nStrongly agree" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    contains("panas"),
    ~ case_when(
      . == "1\nVery Rarely or Never" ~ 1,
      . == "2\nRarely" ~ 2,
      . == "3\nSometimes\n" ~ 3,
      . == "4 \nOften"  ~ 4,
      . == "5\nAlways or almost always\n \n"  ~ 5,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>%
  dplyr::mutate(dplyr::across(
    contains("org_culture"),
    ~ case_when(
      . == "Never" ~ 1,
      . == "Sometimes" ~ 2,
      . == "Often" ~ 3,
      . == "Always"  ~ 4,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>%
  dplyr::mutate(sex = case_when(Gender == "Female" ~ 1,
                                Gender == "Male" ~ 0),
                age = as.numeric(Age)) %>%
  dplyr::mutate(
    married = case_when(
      marital_status == "No, I am not married and/or in a marriage-like relationship" ~ "Yes",
      marital_status == "Yes, I am married and/or in a marriage-like relationship" ~ "No",
      marital_status == "Skip" ~ NA
    ),
    family_income_coded = case_when(
      family_income == "Don't Know" ~ NA,
      family_income == "Skip" ~ NA,
      TRUE ~ family_income
    ),
    mother_education_coded = case_when(
      mother_education == "Some grade school" ~ 1,
      mother_education == "Grade school" ~ 2,
      mother_education == "Some high school" ~ 3,
      mother_education == "High school/GED" ~ 4,
      mother_education == "Some college" ~ 5,
      mother_education == "Completed college" ~ 6,
      mother_education == "Post-grad degree" ~ 7,
      mother_education == "Skip" ~ as.numeric(NA)
    ),
    father_education_coded = case_when(
      father_education == "Some grade school" ~ 1,
      father_education == "Grade school" ~ 2,
      father_education == "Some high school" ~ 3,
      father_education == "High school/GED" ~ 4,
      father_education == "Some college" ~ 5,
      father_education == "Completed college" ~ 6,
      father_education == "Post-grad degree" ~ 7,
      father_education == "Skip" ~ as.numeric(NA)
    ),
    race_coded_one = case_when(
      Race == "Prefer not to say" ~ NA,
      str_detect(Race, "White or Caucasian") ~ "White",
      str_detect(Race, "Black or African American") ~ "Black",
      str_detect(Race, "American Indian/Native American or Alaska Native") ~ "Native American",
      str_detect(Race, "Asian") ~ "Asian",
      str_detect(
        Race,
        "White or Caucasian,American Indian/Native American or Alaska Native"
      ) ~ "White & Native American (Biracial)",
      str_detect(Race, "White or Caucasian,Black or African American,Other") ~
        "White, Black and Hispanic (Multiethnic)",
      str_detect(Race, "White or Caucasian,Black or African American") ~
        "White & Black (Biracial)",
      str_detect(Race, "White or Caucasian,Asian") ~ "White & Asian (Biracial)",
      str_detect(Race_6_TEXT, "Hispanic") ~ "Hispanic",
      str_detect(Race_6_TEXT, "hispanic") ~ "Hispanic",
      str_detect(Race_6_TEXT, "Latina") ~ "Hispanic",
      str_detect(Race_6_TEXT, "Hispanic and Filipino") ~ "Hispanic",
      str_detect(Race_6_TEXT, "hispanic") ~ "Hispanic",
      str_detect(Race_6_TEXT, "Spanish") ~ "Hispanic",
      str_detect(Race_6_TEXT, "Middle Eastern") ~ "Middle Eastern",
      TRUE ~ Race
    ),
    race_coded = case_when(
      Race == "Other" & race_coded_one == "Hispanic" ~ "Hispanic",
      Race == "Other" &
        race_coded_one == "Middle Eastern" ~ "Middle Eastern",
      Race == "Other" & is.na(Race_6_TEXT) == TRUE ~ NA,
      str_detect(Race, ",Other") &
        race_coded_one == "Asian" &
        str_detect(Race_6_TEXT, "Hispanic") ~ "Asian & Hispanic (Biracial)",
      str_detect(Race, ",Other") &
        race_coded_one == "White" &
        str_detect(Race_6_TEXT, "Spanish") ~ "White & Hispanic (Biracial)",
      str_detect(Race, "White or Caucasian,Black or African American,Other") &
        str_detect(Race_6_TEXT, "Hispanic") ~ "White, Black & Hispanic (Multiethnic)",
      str_detect(Race, "Asian,Other") &
        str_detect(Race_6_TEXT, "Hispanic and Filipino") ~ "Asian & Hispanic (Biracial)",
      TRUE ~ race_coded_one
    ),
    attention_check_1_pass = if_else(attention_check_1 == "$10,000", 1, 0),
    attention_check_2_pass = if_else(attention_check_2...152 == "$90,000", 1, 0),
    attention_check_3_pass = if_else(attention_check_2...37 == "$90,000", 1, 0),
    study_datetime = ymd_hms(StartDate),
    study_date = date(study_datetime),
    company_start_date = mdy(primary_job_tenure),
    tenure_days = round(difftime(study_date, company_start_date, units = 'days'), 1),
    tenure_years = as.numeric(round(tenure_days / 365, 2)),
    performancepay = case_when(
      pfp == "No I do not receive performance based pay." ~ "No",
      pfp == "Yes, I receive performance based pay." ~ "Yes",
      pfp == "Skip" ~ NA
    )
  ) %>%
  ungroup() %>%
  rowwise() %>%
  dplyr::transmute(
    StartDate,
    tenure_years,
    performancepay,
    sex,
    age,
    married,
    family_income_coded,
    mother_education_coded,
    father_education_coded,
    race_coded,
    prolific_id,
    attention_check_1_pass,
    attention_check_2_pass,
    attention_check_3_pass,
    psych_empowerment = mean(
      c(
        psych_empow_1,
        psych_empow_2,
        psych_empow_3,
        psych_empow_4,
        psych_empow_5,
        psych_empow_6,
        psych_empow_7,
        psych_empow_8,
        psych_empow_9,
        psych_empow_10,
        psych_empow_11,
        psych_empow_12
      ),
      na.rm = TRUE
    ),
    job_satisfaction = mean(
      c(job_sat_1,
        job_sat_2,
        job_sat_3,
        job_sat_4,
        job_sat_5),
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
        work_engagement_8,
        work_engagement_9,
        work_engagement_10,
        work_engagement_11,
        work_engagement_12,
        work_engagement_13,
        work_engagement_14,
        work_engagement_15,
        work_engagement_16,
        work_engagement_17
      ),
      na.rm = TRUE
    ),
    team_learning = mean(
      c(
        team_learning_1,
        team_learning_2,
        team_learning_3,
        team_learning_4,
        team_learning_5,
        team_learning_6,
        team_learning_7
      ),
      na.rm = TRUE
    ),
    psychological_safety = mean(
      c(
        psych_safety_1,
        psych_safety_2,
        psych_safety_3,
        psych_safety_4,
        psych_safety_5,
        psych_safety_6,
        psych_safety_7
      ),
      na.rm = TRUE
    ),
    helping_behaviors = mean(
      c(
        voice_helping_1,
        voice_helping_2,
        voice_helping_3,
        voice_helping_4,
        voice_helping_5,
        voice_helping_6,
        voice_helping_7,
        voice_helping_8
      ),
      na.rm = TRUE
    ),
    voice_behaviors = mean(
      c(
        voice_helping_9,
        voice_helping_10,
        voice_helping_11,
        voice_helping_12,
        voice_helping_13,
        voice_helping_14
      ),
      na.rm = TRUE
    ),
    team_trust = mean(
      c(
        team_trust_1,
        team_trust_2,
        team_trust_3,
        team_trust_4,
        team_trust_5
      ),
      na.rm = TRUE
    ),
    org_culture_encouragement = mean(
      c(
        org_culture_1,
        org_culture_2,
        org_culture_3,
        org_culture_4,
        org_culture_5,
        org_culture_6,
        org_culture_7,
        org_culture_8,
        org_culture_9,
        org_culture_10,
        org_culture_2_1,
        org_culture_2_2,
        org_culture_2_3,
        org_culture_2_4,
        org_culture_2_5
      ),
      na.rm = TRUE
    ),
    org_culture_creativity = mean(
      c(
        org_culture_2_6,
        org_culture_2_7,
        org_culture_2_8,
        org_culture_2_9,
        org_culture_2_10,
        org_culture_2_11
      ),
      na.rm = TRUE
    ),
    attention_check_sum = sum(
      c(
        attention_check_1_pass,
        attention_check_2_pass,
        attention_check_3_pass
      ),
      na.rm = TRUE
    )
  )

baseline_managers_round2_data <-
  read_csv("Baseline Survey (Round 2) - Study 2 - Managers_July 3, 2024_02.28.csv") %>%
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
      "UserLanguage",
      "Q_RecaptchaScore"
    )
  ) %>%  slice(-c(1:2))


baseline_managers_round2 <- baseline_managers_round2_data %>%
  dplyr::mutate(dplyr::across(
    contains("psych_empow"),
    ~ case_when(
      . == "1\nVery strongly disagree" ~ 1,
      . == "2\n Strongly disagree" ~ 2,
      . == "3\nDisagree" ~ 3,
      . == "4\nNeutral" ~ 4,
      . == "5\nAgree\n"  ~ 5,
      . == "6\nStrongly agree" ~ 6,
      . == "7\nVery strongly agree" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  ))  %>% dplyr::mutate(dplyr::across(
    contains("job_sat"),
    ~ case_when(
      . == "1\nStrongly Disagree\n" ~ 1,
      . == "2\n Disagree"  ~ 2,
      . == "3\nSomewhat disagree" ~ 3,
      . == "4\nNeither agree nor disagree" ~ 4,
      . == "5\nAgree\n"  ~ 5,
      . == "6\nSomewhat agree" ~ 6,
      . == "7\nStrongly agree" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    c("job_sat_3", "job_sat_5"),
    ~ case_when(
      . == 1 ~ 7,
      . == 2 ~ 6,
      . == 3 ~ 5,
      . == 4 ~ 4,
      . == 5 ~ 3,
      . == 6 ~ 2,
      . == 7 ~ 1,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    contains("work_engagement"),
    ~ case_when(
      . == "1\nAlmost Never" ~ 1,
      . == "2\nRarely"   ~ 2,
      . == "3\nSometimes"  ~ 3,
      . == "4\nOften" ~ 4,
      . == "5\nVery often"  ~ 5,
      . == "6\nAlways" ~ 6,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    c("work_engagement_13", "work_engagement_14"),
    ~ case_when(
      . == 1 ~ 6,
      . == 2 ~ 5,
      . == 3 ~ 4,
      . == 4 ~ 3,
      . == 5 ~ 2,
      . == 6 ~ 1,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    contains("team_learning"),
    ~ case_when(
      . == "Very inaccurate \n1\n \n" ~ 1,
      . == "Inaccurate\n2\n"   ~ 2,
      . == "Moderately inaccurate\n3\n"  ~ 3,
      . == "Neither accurate nor inaccurate\n4\n" ~ 4,
      . == "Moderately accurate\n5\n" ~ 5,
      . == "Accurate\n6\n" ~ 6,
      . == "Very accurate\n7\n" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  ))  %>% dplyr::mutate(dplyr::across(
    contains("psych_safety"),
    ~ case_when(
      . == "Very inaccurate \n1\n \n" ~ 1,
      . == "Inaccurate\n2\n"   ~ 2,
      . == "Moderately inaccurate\n3\n"  ~ 3,
      . == "Neither accurate nor inaccurate\n4\n" ~ 4,
      . == "Moderately accurate\n5\n" ~ 5,
      . == "Accurate\n6\n" ~ 6,
      . == "Very accurate\n7\n" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    contains(c(
      "psych_safety_1", "psych_safety_3", "psych_safety_5"
    )),
    ~ case_when(
      . == 1 ~ 7,
      . == 2 ~ 6,
      . == 3 ~ 5,
      . == 4 ~ 4,
      . == 5 ~ 3,
      . == 6 ~ 2,
      . == 7 ~ 1,
      . == "Skip" ~ as.numeric(NA)
    )
  ))  %>% dplyr::mutate(dplyr::across(
    contains("voice_helping"),
    ~ case_when(
      . == "1\nStrongly Disagree\n" ~ 1,
      . == "2\n Disagree"  ~ 2,
      . == "3\nSomewhat disagree" ~ 3,
      . == "4\nNeither agree nor disagree" ~ 4,
      . == "5\nAgree\n"  ~ 5,
      . == "6\nSomewhat agree" ~ 6,
      . == "7\nStrongly agree" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    contains("team_trust"),
    ~ case_when(
      . == "1\nStrongly Disagree\n" ~ 1,
      . == "2\n Disagree"  ~ 2,
      . == "3\nSomewhat disagree" ~ 3,
      . == "4\nNeither agree nor disagree" ~ 4,
      . == "5\nAgree\n"  ~ 5,
      . == "6\nSomewhat agree" ~ 6,
      . == "7\nStrongly agree" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    contains("tipi"),
    ~ case_when(
      . == "1\nStrongly Disagree\n" ~ 1,
      . == "2\n Disagree"  ~ 2,
      . == "3\nSomewhat disagree" ~ 3,
      . == "4\nNeither agree nor disagree" ~ 4,
      . == "5\nAgree\n"  ~ 5,
      . == "6\nSomewhat agree" ~ 6,
      . == "7\nStrongly agree" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    contains("panas"),
    ~ case_when(
      . == "1\nVery Rarely or Never" ~ 1,
      . == "2\nRarely" ~ 2,
      . == "3\nSometimes\n" ~ 3,
      . == "4 \nOften"  ~ 4,
      . == "5\nAlways or almost always\n \n"  ~ 5,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>%
  dplyr::mutate(dplyr::across(
    contains("org_culture"),
    ~ case_when(
      . == "Never" ~ 1,
      . == "Sometimes" ~ 2,
      . == "Often" ~ 3,
      . == "Always"  ~ 4,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>%
  dplyr::mutate(sex = case_when(Gender == "Female" ~ 1,
                                Gender == "Male" ~ 0),
                age = as.numeric(Age)) %>%
  dplyr::mutate(
    married = case_when(
      marital_status == "No, I am not married and/or in a marriage-like relationship" ~ "Yes",
      marital_status == "Yes, I am married and/or in a marriage-like relationship" ~ "No",
      marital_status == "Skip" ~ NA
    ),
    family_income_coded = case_when(
      family_income == "Don't Know" ~ NA,
      family_income == "Skip" ~ NA,
      TRUE ~ family_income
    ),
    mother_education_coded = case_when(
      mother_education == "Some grade school" ~ 1,
      mother_education == "Grade school" ~ 2,
      mother_education == "Some high school" ~ 3,
      mother_education == "High school/GED" ~ 4,
      mother_education == "Some college" ~ 5,
      mother_education == "Completed college" ~ 6,
      mother_education == "Post-grad degree" ~ 7,
      mother_education == "Skip" ~ as.numeric(NA)
    ),
    father_education_coded = case_when(
      father_education == "Some grade school" ~ 1,
      father_education == "Grade school" ~ 2,
      father_education == "Some high school" ~ 3,
      father_education == "High school/GED" ~ 4,
      father_education == "Some college" ~ 5,
      father_education == "Completed college" ~ 6,
      father_education == "Post-grad degree" ~ 7,
      father_education == "Skip" ~ as.numeric(NA)
    ),
    race_coded_one = case_when(
      Race == "Prefer not to say" ~ NA,
      str_detect(Race, "White or Caucasian") ~ "White",
      str_detect(Race, "Black or African American") ~ "Black",
      str_detect(Race, "American Indian/Native American or Alaska Native") ~ "Native American",
      str_detect(Race, "Asian") ~ "Asian",
      str_detect(
        Race,
        "White or Caucasian,American Indian/Native American or Alaska Native"
      ) ~ "White & Native American (Biracial)",
      str_detect(Race, "White or Caucasian,Black or African American,Other") ~
        "White, Black and Hispanic (Multiethnic)",
      str_detect(Race, "White or Caucasian,Black or African American") ~
        "White & Black (Biracial)",
      str_detect(Race, "White or Caucasian,Asian") ~ "White & Asian (Biracial)",
      str_detect(Race_6_TEXT, "Hispanic") ~ "Hispanic",
      str_detect(Race_6_TEXT, "hispanic") ~ "Hispanic",
      str_detect(Race_6_TEXT, "Latina") ~ "Hispanic",
      str_detect(Race_6_TEXT, "Hispanic and Filipino") ~ "Hispanic",
      str_detect(Race_6_TEXT, "hispanic") ~ "Hispanic",
      str_detect(Race_6_TEXT, "Spanish") ~ "Hispanic",
      str_detect(Race_6_TEXT, "Middle Eastern") ~ "Middle Eastern",
      TRUE ~ Race
    ),
    race_coded = case_when(
      Race == "Other" & race_coded_one == "Hispanic" ~ "Hispanic",
      Race == "Other" &
        race_coded_one == "Middle Eastern" ~ "Middle Eastern",
      str_detect(Race, ",Other") &
        race_coded_one == "Asian" &
        str_detect(Race_6_TEXT, "Hispanic") ~ "Asian & Hispanic (Biracial)",
      str_detect(Race, ",Other") &
        race_coded_one == "White" &
        str_detect(Race_6_TEXT, "Spanish") ~ "White & Hispanic (Biracial)",
      str_detect(Race, "White or Caucasian,Black or African American,Other") &
        str_detect(Race_6_TEXT, "Hispanic") ~ "White, Black & Hispanic (Multiethnic)",
      str_detect(Race, "Asian,Other") &
        str_detect(Race_6_TEXT, "Hispanic and Filipino") ~ "Asian & Hispanic (Biracial)",
      TRUE ~ race_coded_one
    ),
    attention_check_1_pass = if_else(attention_check_1 == "$10,000", 1, 0),
    attention_check_2_pass = if_else(attention_check_2...154 == "$90,000", 1, 0),
    attention_check_3_pass = if_else(attention_check_2...39 == "$90,000", 1, 0),
    study_datetime = ymd_hms(StartDate),
    study_date = date(study_datetime),
    company_start_date = mdy(primary_job_tenure),
    tenure_days = round(difftime(study_date, company_start_date, units = 'days'), 1),
    tenure_years = as.numeric(round(tenure_days / 365, 2)),
    performancepay = case_when(
      pfp == "No I do not receive performance based pay." ~ "No",
      pfp == "Yes, I receive performance based pay." ~ "Yes",
      pfp == "Skip" ~ NA
    )
  ) %>%
  ungroup() %>%
  rowwise() %>%
  dplyr::transmute(
    StartDate,
    tenure_years,
    performancepay,
    sex,
    age,
    married,
    family_income_coded,
    mother_education_coded,
    father_education_coded,
    race_coded,
    prolific_id,
    attention_check_1_pass,
    attention_check_2_pass,
    attention_check_3_pass,
    psych_empowerment = mean(
      c(
        psych_empow_1,
        psych_empow_2,
        psych_empow_3,
        psych_empow_4,
        psych_empow_5,
        psych_empow_6,
        psych_empow_7,
        psych_empow_8,
        psych_empow_9,
        psych_empow_10,
        psych_empow_11,
        psych_empow_12
      ),
      na.rm = TRUE
    ),
    job_satisfaction = mean(
      c(job_sat_1,
        job_sat_2,
        job_sat_3,
        job_sat_4,
        job_sat_5),
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
        work_engagement_8,
        work_engagement_9,
        work_engagement_10,
        work_engagement_11,
        work_engagement_12,
        work_engagement_13,
        work_engagement_14,
        work_engagement_15,
        work_engagement_16,
        work_engagement_17
      ),
      na.rm = TRUE
    ),
    team_learning = mean(
      c(
        team_learning_1,
        team_learning_2,
        team_learning_3,
        team_learning_4,
        team_learning_5,
        team_learning_6,
        team_learning_7
      ),
      na.rm = TRUE
    ),
    psychological_safety = mean(
      c(
        psych_safety_1,
        psych_safety_2,
        psych_safety_3,
        psych_safety_4,
        psych_safety_5,
        psych_safety_6,
        psych_safety_7
      ),
      na.rm = TRUE
    ),
    helping_behaviors = mean(
      c(
        voice_helping_1,
        voice_helping_2,
        voice_helping_3,
        voice_helping_4,
        voice_helping_5,
        voice_helping_6,
        voice_helping_7,
        voice_helping_8
      ),
      na.rm = TRUE
    ),
    voice_behaviors = mean(
      c(
        voice_helping_9,
        voice_helping_10,
        voice_helping_11,
        voice_helping_12,
        voice_helping_13,
        voice_helping_14
      ),
      na.rm = TRUE
    ),
    team_trust = mean(
      c(
        team_trust_1,
        team_trust_2,
        team_trust_3,
        team_trust_4,
        team_trust_5
      ),
      na.rm = TRUE
    ),
    org_culture_encouragement = mean(
      c(
        org_culture_1,
        org_culture_2,
        org_culture_3,
        org_culture_4,
        org_culture_5,
        org_culture_6,
        org_culture_7,
        org_culture_8,
        org_culture_9,
        org_culture_10,
        org_culture_2_1,
        org_culture_2_2,
        org_culture_2_3,
        org_culture_2_4,
        org_culture_2_5
      ),
      na.rm = TRUE
    ),
    org_culture_creativity = mean(
      c(
        org_culture_2_6,
        org_culture_2_7,
        org_culture_2_8,
        org_culture_2_9,
        org_culture_2_10,
        org_culture_2_11
      ),
      na.rm = TRUE
    ),
    attention_check_sum = sum(
      c(
        attention_check_1_pass,
        attention_check_2_pass,
        attention_check_3_pass
      ),
      na.rm = TRUE
    )
  )

baseline_nonmanagers_round2_data <-
  read_csv("Baseline Survey (Round 2) - Study 2 - NonManagers_July 3, 2024_02.29.csv") %>%
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
      "UserLanguage",
      "Q_RecaptchaScore"
    )
  ) %>%  slice(-c(1:2))


baseline_nonmanagers_round2 <- baseline_nonmanagers_round2_data %>%
  dplyr::mutate(dplyr::across(
    contains("psych_empow"),
    ~ case_when(
      . == "1\nVery strongly disagree" ~ 1,
      . == "2\n Strongly disagree" ~ 2,
      . == "3\nDisagree" ~ 3,
      . == "4\nNeutral" ~ 4,
      . == "5\nAgree\n"  ~ 5,
      . == "6\nStrongly agree" ~ 6,
      . == "7\nVery strongly agree" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  ))  %>% dplyr::mutate(dplyr::across(
    contains("job_sat"),
    ~ case_when(
      . == "1\nStrongly Disagree\n" ~ 1,
      . == "2\n Disagree"  ~ 2,
      . == "3\nSomewhat disagree" ~ 3,
      . == "4\nNeither agree nor disagree" ~ 4,
      . == "5\nAgree\n"  ~ 5,
      . == "6\nSomewhat agree" ~ 6,
      . == "7\nStrongly agree" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    c("job_sat_3", "job_sat_5"),
    ~ case_when(
      . == 1 ~ 7,
      . == 2 ~ 6,
      . == 3 ~ 5,
      . == 4 ~ 4,
      . == 5 ~ 3,
      . == 6 ~ 2,
      . == 7 ~ 1,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    contains("work_engagement"),
    ~ case_when(
      . == "1\nAlmost Never" ~ 1,
      . == "2\nRarely"   ~ 2,
      . == "3\nSometimes"  ~ 3,
      . == "4\nOften" ~ 4,
      . == "5\nVery often"  ~ 5,
      . == "6\nAlways" ~ 6,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    c("work_engagement_13", "work_engagement_14"),
    ~ case_when(
      . == 1 ~ 6,
      . == 2 ~ 5,
      . == 3 ~ 4,
      . == 4 ~ 3,
      . == 5 ~ 2,
      . == 6 ~ 1,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    contains("team_learning"),
    ~ case_when(
      . == "Very inaccurate \n1\n \n" ~ 1,
      . == "Inaccurate\n2\n"   ~ 2,
      . == "Moderately inaccurate\n3\n"  ~ 3,
      . == "Neither accurate nor inaccurate\n4\n" ~ 4,
      . == "Moderately accurate\n5\n" ~ 5,
      . == "Accurate\n6\n" ~ 6,
      . == "Very accurate\n7\n" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  ))  %>% dplyr::mutate(dplyr::across(
    contains("psych_safety"),
    ~ case_when(
      . == "Very inaccurate \n1\n \n" ~ 1,
      . == "Inaccurate\n2\n"   ~ 2,
      . == "Moderately inaccurate\n3\n"  ~ 3,
      . == "Neither accurate nor inaccurate\n4\n" ~ 4,
      . == "Moderately accurate\n5\n" ~ 5,
      . == "Accurate\n6\n" ~ 6,
      . == "Very accurate\n7\n" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    contains(c(
      "psych_safety_1", "psych_safety_3", "psych_safety_5"
    )),
    ~ case_when(
      . == 1 ~ 7,
      . == 2 ~ 6,
      . == 3 ~ 5,
      . == 4 ~ 4,
      . == 5 ~ 3,
      . == 6 ~ 2,
      . == 7 ~ 1,
      . == "Skip" ~ as.numeric(NA)
    )
  ))  %>% dplyr::mutate(dplyr::across(
    contains("voice_helping"),
    ~ case_when(
      . == "1\nStrongly Disagree\n" ~ 1,
      . == "2\n Disagree"  ~ 2,
      . == "3\nSomewhat disagree" ~ 3,
      . == "4\nNeither agree nor disagree" ~ 4,
      . == "5\nAgree\n"  ~ 5,
      . == "6\nSomewhat agree" ~ 6,
      . == "7\nStrongly agree" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    contains("team_trust"),
    ~ case_when(
      . == "1\nStrongly Disagree\n" ~ 1,
      . == "2\n Disagree"  ~ 2,
      . == "3\nSomewhat disagree" ~ 3,
      . == "4\nNeither agree nor disagree" ~ 4,
      . == "5\nAgree\n"  ~ 5,
      . == "6\nSomewhat agree" ~ 6,
      . == "7\nStrongly agree" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    contains("tipi"),
    ~ case_when(
      . == "1\nStrongly Disagree\n" ~ 1,
      . == "2\n Disagree"  ~ 2,
      . == "3\nSomewhat disagree" ~ 3,
      . == "4\nNeither agree nor disagree" ~ 4,
      . == "5\nAgree\n"  ~ 5,
      . == "6\nSomewhat agree" ~ 6,
      . == "7\nStrongly agree" ~ 7,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>% dplyr::mutate(dplyr::across(
    contains("panas"),
    ~ case_when(
      . == "1\nVery Rarely or Never" ~ 1,
      . == "2\nRarely" ~ 2,
      . == "3\nSometimes\n" ~ 3,
      . == "4 \nOften"  ~ 4,
      . == "5\nAlways or almost always\n \n"  ~ 5,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>%
  dplyr::mutate(dplyr::across(
    contains("org_culture"),
    ~ case_when(
      . == "Never" ~ 1,
      . == "Sometimes" ~ 2,
      . == "Often" ~ 3,
      . == "Always"  ~ 4,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>%
  dplyr::mutate(sex = case_when(Gender == "Female" ~ 1,
                                Gender == "Male" ~ 0),
                age = as.numeric(Age)) %>%
  dplyr::mutate(
    married = case_when(
      marital_status == "No, I am not married and/or in a marriage-like relationship" ~ "Yes",
      marital_status == "Yes, I am married and/or in a marriage-like relationship" ~ "No",
      marital_status == "Skip" ~ NA
    ),
    family_income_coded = case_when(
      family_income == "Don't Know" ~ NA,
      family_income == "Skip" ~ NA,
      TRUE ~ family_income
    ),
    mother_education_coded = case_when(
      mother_education == "Some grade school" ~ 1,
      mother_education == "Grade school" ~ 2,
      mother_education == "Some high school" ~ 3,
      mother_education == "High school/GED" ~ 4,
      mother_education == "Some college" ~ 5,
      mother_education == "Completed college" ~ 6,
      mother_education == "Post-grad degree" ~ 7,
      mother_education == "Skip" ~ as.numeric(NA)
    ),
    father_education_coded = case_when(
      father_education == "Some grade school" ~ 1,
      father_education == "Grade school" ~ 2,
      father_education == "Some high school" ~ 3,
      father_education == "High school/GED" ~ 4,
      father_education == "Some college" ~ 5,
      father_education == "Completed college" ~ 6,
      father_education == "Post-grad degree" ~ 7,
      father_education == "Skip" ~ as.numeric(NA)
    ),
    race_coded_one = case_when(
      Race == "Prefer not to say" ~ NA,
      str_detect(Race, "White or Caucasian") ~ "White",
      str_detect(Race, "Black or African American") ~ "Black",
      str_detect(Race, "American Indian/Native American or Alaska Native") ~ "Native American",
      str_detect(Race, "Asian") ~ "Asian",
      str_detect(
        Race,
        "White or Caucasian,American Indian/Native American or Alaska Native"
      ) ~ "White & Native American (Biracial)",
      str_detect(Race, "White or Caucasian,Black or African American,Other") ~
        "White, Black and Hispanic (Multiethnic)",
      str_detect(Race, "White or Caucasian,Black or African American") ~
        "White & Black (Biracial)",
      str_detect(Race, "White or Caucasian,Asian") ~ "White & Asian (Biracial)",
      str_detect(Race_6_TEXT, "Hispanic") ~ "Hispanic",
      str_detect(Race_6_TEXT, "hispanic") ~ "Hispanic",
      str_detect(Race_6_TEXT, "Latina") ~ "Hispanic",
      str_detect(Race_6_TEXT, "Hispanic and Filipino") ~ "Hispanic",
      str_detect(Race_6_TEXT, "hispanic") ~ "Hispanic",
      str_detect(Race_6_TEXT, "Spanish") ~ "Hispanic",
      str_detect(Race_6_TEXT, "Middle Eastern") ~ "Middle Eastern",
      TRUE ~ Race
    ),
    race_coded = case_when(
      Race == "Other" & race_coded_one == "Hispanic" ~ "Hispanic",
      Race == "Other" &
        race_coded_one == "Middle Eastern" ~ "Middle Eastern",
      str_detect(Race, ",Other") &
        race_coded_one == "Asian" &
        str_detect(Race_6_TEXT, "Hispanic") ~ "Asian & Hispanic (Biracial)",
      str_detect(Race, ",Other") &
        race_coded_one == "White" &
        str_detect(Race_6_TEXT, "Spanish") ~ "White & Hispanic (Biracial)",
      str_detect(Race, "White or Caucasian,Black or African American,Other") &
        str_detect(Race_6_TEXT, "Hispanic") ~ "White, Black & Hispanic (Multiethnic)",
      str_detect(Race, "Asian,Other") &
        str_detect(Race_6_TEXT, "Hispanic and Filipino") ~ "Asian & Hispanic (Biracial)",
      TRUE ~ race_coded_one
    ),
    attention_check_1_pass = if_else(attention_check_1 == "$10,000", 1, 0),
    attention_check_2_pass = if_else(attention_check_2...154 == "$90,000", 1, 0),
    attention_check_3_pass = if_else(attention_check_2...39 == "$90,000", 1, 0),
    study_datetime = ymd_hms(StartDate),
    study_date = date(study_datetime),
    company_start_date = mdy(primary_job_tenure),
    tenure_days = round(difftime(study_date, company_start_date, units = 'days'), 1),
    tenure_years = as.numeric(round(tenure_days / 365, 2)),
    performancepay = case_when(
      pfp == "No I do not receive performance based pay." ~ "No",
      pfp == "Yes, I receive performance based pay." ~ "Yes",
      pfp == "Skip" ~ NA
    )
  ) %>%
  ungroup() %>%
  rowwise() %>%
  dplyr::transmute(
    StartDate,
    tenure_years,
    performancepay,
    sex,
    age,
    married,
    family_income_coded,
    mother_education_coded,
    father_education_coded,
    race_coded,
    prolific_id,
    attention_check_1_pass,
    attention_check_2_pass,
    attention_check_3_pass,
    psych_empowerment = mean(
      c(
        psych_empow_1,
        psych_empow_2,
        psych_empow_3,
        psych_empow_4,
        psych_empow_5,
        psych_empow_6,
        psych_empow_7,
        psych_empow_8,
        psych_empow_9,
        psych_empow_10,
        psych_empow_11,
        psych_empow_12
      ),
      na.rm = TRUE
    ),
    job_satisfaction = mean(
      c(job_sat_1,
        job_sat_2,
        job_sat_3,
        job_sat_4,
        job_sat_5),
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
        work_engagement_8,
        work_engagement_9,
        work_engagement_10,
        work_engagement_11,
        work_engagement_12,
        work_engagement_13,
        work_engagement_14,
        work_engagement_15,
        work_engagement_16,
        work_engagement_17
      ),
      na.rm = TRUE
    ),
    team_learning = mean(
      c(
        team_learning_1,
        team_learning_2,
        team_learning_3,
        team_learning_4,
        team_learning_5,
        team_learning_6,
        team_learning_7
      ),
      na.rm = TRUE
    ),
    psychological_safety = mean(
      c(
        psych_safety_1,
        psych_safety_2,
        psych_safety_3,
        psych_safety_4,
        psych_safety_5,
        psych_safety_6,
        psych_safety_7
      ),
      na.rm = TRUE
    ),
    helping_behaviors = mean(
      c(
        voice_helping_1,
        voice_helping_2,
        voice_helping_3,
        voice_helping_4,
        voice_helping_5,
        voice_helping_6,
        voice_helping_7,
        voice_helping_8
      ),
      na.rm = TRUE
    ),
    voice_behaviors = mean(
      c(
        voice_helping_9,
        voice_helping_10,
        voice_helping_11,
        voice_helping_12,
        voice_helping_13,
        voice_helping_14
      ),
      na.rm = TRUE
    ),
    team_trust = mean(
      c(
        team_trust_1,
        team_trust_2,
        team_trust_3,
        team_trust_4,
        team_trust_5
      ),
      na.rm = TRUE
    ),
    org_culture_encouragement = mean(
      c(
        org_culture_1,
        org_culture_2,
        org_culture_3,
        org_culture_4,
        org_culture_5,
        org_culture_6,
        org_culture_7,
        org_culture_8,
        org_culture_9,
        org_culture_10,
        org_culture_2_1,
        org_culture_2_2,
        org_culture_2_3,
        org_culture_2_4,
        org_culture_2_5
      ),
      na.rm = TRUE
    ),
    org_culture_creativity = mean(
      c(
        org_culture_2_6,
        org_culture_2_7,
        org_culture_2_8,
        org_culture_2_9,
        org_culture_2_10,
        org_culture_2_11
      ),
      na.rm = TRUE
    ),
    attention_check_sum = sum(
      c(
        attention_check_1_pass,
        attention_check_2_pass,
        attention_check_3_pass
      ),
      na.rm = TRUE
    )
  )

baseline_data <-
  rbind(
    baseline_managers,
    baseline_nonmanagers,
    baseline_managers_round2,
    baseline_nonmanagers_round2
  ) %>%
  ungroup () %>%
  mutate_all(~ ifelse(is.nan(.), NA, .)) %>%
  ungroup() %>%
  group_by(prolific_id) %>%
  arrange(prolific_id, StartDate) %>%
  filter(row_number() == 1) %>%
  select(-StartDate) %>%
  ungroup()

### People who did not complete or failed 2 or more attention checks
# Compute number of respondents who failed the attention check
# NOTE: the vast, vast majority of these are folks who did not answer this and other questions in the survey
# This likely because of people who were filtered out after the initial questions due to ineligibility

failed_attention_checks <- baseline_data %>%
  filter(attention_check_sum <= 1)

# Perform filtering

baseline_data <- baseline_data %>%
  filter(attention_check_sum >= 2)

#### Read in Screener Data

screener <-
  read_csv("Screener (Study 2 - Dynamic Learning in Organizations)_July 3, 2024_02.42.csv") %>%
  select(
    StartDate,
    prolific_id,
    primary_team_size,
    primary_team_size_1_TEXT,
    contains("team_interdependence"),
    direct_reports,
    direct_reports_1_TEXT
  ) %>%
  group_by(prolific_id) %>%
  arrange(prolific_id, StartDate) %>%
  filter(row_number() == 1) %>%
  select(-StartDate) %>%
  ungroup() %>%
  dplyr::mutate(dplyr::across(
    contains("team_interdependence"),
    ~ case_when(
      . == "0" ~ 1,
      . == "1" ~ 2,
      . == "3" ~ 3,
      . == "4"  ~ 4,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>%
  dplyr::mutate(dplyr::across(
    c(
      "team_interdependence_6",
      "team_interdependence_7",
      "team_interdependence_8"
    ),
    ~ case_when(
      . == "0" ~ 4,
      . == "1" ~ 3,
      . == "3" ~ 2,
      . == "4"  ~ 1,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>%
  ungroup() %>%
  rowwise() %>%
  transmute(
    prolific_id,
    primary_team_size_1_TEXT,
    direct_reports,
    direct_reports_1_TEXT,
    team_interdependence = mean(
      c(
        team_interdependence_1,
        team_interdependence_2,
        team_interdependence_3,
        team_interdependence_4,
        team_interdependence_5,
        team_interdependence_6,
        team_interdependence_7,
        team_interdependence_8
      ),
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  rename(team_size = primary_team_size_1_TEXT,
         direct_reports_number = direct_reports_1_TEXT) %>%
  mutate(
    direct_reports = case_when(
      direct_reports == "No" ~ "No",
      direct_reports == "Yes (indicate how many):" ~ "Yes"
    ),
    seniority = case_when(
      !is.na(direct_reports_number) == TRUE &
        as.numeric(direct_reports_number) >= 2 ~ "Manager",
      !is.na(direct_reports_number) == TRUE &
        as.numeric(direct_reports_number) <= 2 ~ "Non Manager",
      !is.na(direct_reports_number) == FALSE ~ "Non Manager"
    )
  ) %>%
  mutate_all(~ ifelse(is.nan(.), NA, .)) %>%
  drop_na(direct_reports)


##### Merge with baseline data

baseline_screener <- baseline_data %>%
  drop_na(prolific_id) %>%
  left_join(screener, by = "prolific_id") %>%
  mutate(
    team_size = as.numeric(team_size),
    direct_reports_number = as.numeric(direct_reports_number)
  )

##### Anonymize baseline data

baseline_screener$id <-
  sapply(baseline_screener$prolific_id, digest::digest, algo = "md5")
#write.csv(baseline_screener, "baseline-clean.csv")


#### Data Export for Ashley's Collaborator


#### Extract Relevant Variables for Kaylee and Riddhi

baseline_primo <- baseline_screener %>%
  select(
    id,
    psychological_safety,
    race_coded,
    mother_education_coded,
    father_education_coded,
    sex,
    age
  )

baseline_primo <- baseline_primo %>%
  ungroup() %>%
  rowwise() %>%
  mutate(ses = sum(mother_education_coded, father_education_coded, na.rm =
                     TRUE)) %>%
  ungroup()

baseline_primo$race <- factor(baseline_primo$race_coded)

baseline_primo <- baseline_primo %>%
  select(
    id,
    ses,
    psychological_safety,
    race,
    sex,
    age,
    mother_education_coded,
    father_education_coded
  )

#write.csv(baseline_primo, "primo_data_baseline.csv")

model1 <-
  lm(psychological_safety ~ ses, data = baseline_primo)
model2 <-
  lm(psychological_safety ~ mother_education_coded, data = baseline_primo)
model3 <-
  lm(psychological_safety ~ father_education_coded, data = baseline_primo)

baseline_primo$race <- relevel(baseline_primo$race, ref = "White")

model4 <- lm(psychological_safety ~ race, data = baseline_primo)
summary(model4)                        