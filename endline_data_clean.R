library(tidyverse)
library(lubridate)
endline_data <-
  read_csv("Endline Survey_July 26, 2024_08.52.csv") %>%
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


endline <- endline_data %>%
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
      . == "1\nStrongly Disagree\n" ~ 7,
      . == "2\n Disagree"  ~ 6,
      . == "3\nSomewhat disagree" ~ 5,
      . == "4\nNeither agree nor disagree" ~ 4,
      . == "5\nAgree\n"  ~ 3,
      . == "6\nSomewhat agree" ~ 2,
      . == "7\nStrongly agree" ~ 1,
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
      . == "1\nAlmost Never" ~ 6,
      . == "2\nRarely"   ~ 5,
      . == "3\nSometimes"  ~ 4,
      . == "4\nOften" ~ 3,
      . == "5\nVery often"  ~ 2,
      . == "6\nAlways" ~ 1,
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
      . == "Very inaccurate \n1\n \n" ~ 7,
      . == "Inaccurate\n2\n"   ~ 6,
      . == "Moderately inaccurate\n3\n"  ~ 5,
      . == "Neither accurate nor inaccurate\n4\n" ~ 4,
      . == "Moderately accurate\n5\n" ~ 3,
      . == "Accurate\n6\n" ~ 2,
      . == "Very accurate\n7\n" ~ 1,
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
  )) %>%
  dplyr::mutate(dplyr::across(
    contains(c(
      "tipi_2", "tipi_6", "tipi_8", "tipi_4", "tipi_10"
    )),
    ~ case_when(
      . == "1\nStrongly Disagree\n" ~ 7,
      . == "2\n Disagree"  ~ 6,
      . == "3\nSomewhat disagree" ~ 5,
      . == "4\nNeither agree nor disagree" ~ 4,
      . == "5\nAgree\n"  ~ 3,
      . == "6\nSomewhat agree" ~ 2,
      . == "7\nStrongly agree" ~ 1,
      . == "Skip" ~ as.numeric(NA)
    )
  )) %>%
  dplyr::mutate(dplyr::across(
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
  dplyr::mutate(
    attention_check_1_pass = if_else(attention_check_1 == "$10,000", 1, 0),
    attention_check_2_pass = if_else(attention_check_2 == "$90,000", 1, 0)
  ) %>%
  dplyr::mutate(dplyr::across(
    contains("expr_primary_job"),
    ~ case_when(
      . == "1\nNever\n" ~ "Never",
      . == "2\nRarely\n" ~ "Rarely",
      . == "3\nOccasionally\n" ~ "Occasionally",
      . == "4\nSometimes\n" ~ "Sometimes",
      . == "5\nOften\n" ~ "Often",
      . == "6\nVery Often\n" ~ "Very Often",
      . == "7\nAlways\n" ~ "Always",
      . == "Skip" ~ NA
    )
  )) %>%
  dplyr::mutate(dplyr::across(
    contains("objectification"),
    ~ case_when(
      . == "1\nNever\n" ~ "Never",
      . == "2\nA few times\n" ~ "A few times",
      . == "3\nSome of the time\n" ~ "Some of the time",
      . == "4\nMost of the time\n" ~ "Most of the time",
      . == "5\nAll the time\n" ~ "All the time",
      . == "6\nVery Often\n" ~ "Very often",
      . == "Skip" ~ NA
    )
  )) %>%
  dplyr::mutate(
    performancepay = case_when(
      tfp == "No I do not receive performance based pay." ~ "No",
      tfp == "Yes, I receive performance based pay." ~ "Yes",
      tfp == "Skip" ~ NA
    ),
    primary_team_change = case_when(
      primary_team_change == "Yes [indicate when your primary team changed]" ~ "Yes",
      primary_team_change == "No" ~ "No",
      primary_team_change == "Skip" ~ NA
    ),
    primary_team_change_date = mdy(primary_team_change_1_TEXT),
    job_change = case_when(
      job_change == "Yes [indicate when your primary job change]" ~ "Yes",
      job_change == "No" ~ "No",
      job_change == "Skip" ~ NA,
    ),
    job_change_date = mdy(job_change_1_TEXT),
    sig_event = case_when(
      sig_event == "Yes [indicate when the significant life event took place]" ~ "Yes",
      sig_event == "No" ~ "No",
      sig_event == "Skip" ~ NA
    ),
    sig_event_date = mdy(sig_event_1_TEXT),
    time_taken_off = case_when(
      time_taken_off == "Yes [Indicate the date you started your time off]" ~ "Yes",
      time_taken_off=="No" ~ "No",
      time_taken_off=="Skip" ~ NA
    ),
    time_taken_off_duration = as.numeric(time_off_dur)
  ) %>% ungroup() %>%
  rowwise() %>%
  dplyr::transmute(
    StartDate,
    primary_team_change,
    primary_team_change_date, 
    job_change,
    job_change_date,
    sig_event,
    sig_event_date,
    time_taken_off,
    time_taken_off_duration,
    performancepay,
    objectification_1,
    objectification_2,
    expr_primary_job_1,
    expr_primary_job_2,
    expr_primary_job_3,
    performancepay,
    prolific_pid,
    attention_check_1_pass,
    attention_check_2_pass,
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
    attention_check_sum = sum(c(
      attention_check_1_pass,
      attention_check_2_pass
    ),
    na.rm = TRUE),
    positive_feeling = sum(
      c(panas_1,
        panas_3,
        panas_5,
        panas_7,
        panas_10,
        panas_12),
      na.rm = TRUE
    ),
    negative_feeling = sum(c(
      panas_2,
      panas_4,
      panas_6,
      panas_8,
      panas_9,
      panas_11
    ), na.rm = TRUE),
    bfi.e = mean(c(panas_1, panas_6), na.rm = TRUE),
    bfi.a = mean(c(panas_2, panas_7), na.rm = TRUE),
    bfi.o = mean(c(panas_5, panas_10), na.rm = TRUE),
    bfi.c = mean(c(panas_3, panas_8), na.rm = TRUE),
    bfi.n = mean(c(panas_4, panas_9), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate_all(~ ifelse(is.nan(.), NA, .))
