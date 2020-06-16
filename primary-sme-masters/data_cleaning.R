library(tidyverse)

# most up-to-date data
# url <- "https://projects.fivethirtyeight.com/polls-page/"
poll_file <- "president_primary_polls.csv"
# download.file(paste0(url, poll_file), poll_file, mode = "wb")

## read data from 538
polls <- read_csv(poll_file,
                  col_types = cols(.default = col_character(), 
                                   question_id = col_double(),
                                   poll_id = col_double(),
                                   cycle = col_double(),
                                   pollster_id = col_double(),
                                   sponsor_ids = col_number(),
                                   pollster_rating_id = col_double(),
                                   sample_size = col_double(),
                                   internal = col_logical(),
                                   tracking = col_logical(),
                                   nationwide_batch = col_logical(),
                                   candidate_id = col_double(),
                                   pct = col_double()))

summary(polls)
# print(tail(polls, n = 20), width = Inf)

# number of unique pollsters: 139 as of 4/15/2020
# length(unique(polls$pollster_id))

# number of unique polls: 1124 as of 4/15/2020
# length(unique(polls$poll_id))

# cleaning up some data types
polls$start_date <- as.Date(polls$start_date, "%m/%d/%y")
polls$end_date <- as.Date(polls$end_date, "%m/%d/%y")
polls$fte_grade <- as.factor(polls$fte_grade)
polls$state <- as.factor(polls$state)
polls$pollster <- as.factor(polls$pollster)
polls$sponsors <- as.factor(polls$sponsors)
polls$methodology <- as.factor(polls$methodology)
polls$population <- as.factor(polls$population)
polls$population_full <- as.factor(polls$population_full)
polls$stage <- as.factor(polls$stage)
polls$party <- as.factor(polls$party)
polls$answer <- as.factor(polls$answer)

# summary(polls)

# summary(polls$state)

# polls <- filter(polls, party == "DEM")

# polls that included Republican candidates
poll_id_REP <- unique(pull(polls[polls$party == "REP", "poll_id"]))

# exclude polls that included Republican candidates --> just want to analyze polls focusing on primary
polls <- polls %>% filter(!poll_id %in% poll_id_REP)

## state-specific datasets: more for Wright model?
## probably also for how I would implement Michaelis

cands <- c("Biden", "Sanders", "Warren", "Buttigieg", "Klobuchar")
# filter(unique(polls$answer), !unique(polls$answer) %in% cands)
polls$ans_oth <- fct_other(polls$answer, keep = cands)

# Iowa dataset
polls_ia <- polls %>% 
              filter(state == "Iowa") %>% 
              dplyr::select(question_id, poll_id, state, pollster_id, pollster, sponsor_ids, sponsors, 
                     sample_size, population, methodology, start_date, end_date,
                     party, answer, ans_oth, candidate_id, pct)

summary(polls_ia) # Dec 2018 to Feb 2020
length(unique(polls_ia$poll_id)) # 59
length(unique(polls_ia$pollster_id)) # 19

# New Hampshire dataset
polls_nh <- polls %>% 
              filter(state == "New Hampshire") %>%
              dplyr::select(question_id, poll_id, state, pollster_id, pollster, sponsor_ids, sponsors, 
                            sample_size, population, methodology, start_date, end_date,
                            party, answer, ans_oth, candidate_id, pct)

summary(polls_nh) # Jan 2019 to Feb 2020
length(unique(polls_nh$poll_id)) # 60
length(unique(polls_nh$pollster_id)) # 18

# Nevada dataset
polls_nv <- polls %>% 
              filter(state == "Nevada") %>% 
              dplyr::select(question_id, poll_id, state, pollster_id, pollster, sponsor_ids, sponsors, 
                            sample_size, population, methodology, start_date, end_date,
                            party, answer, ans_oth, candidate_id, pct)

summary(polls_nv) # Mar 2019 to Feb 2020
length(unique(polls_nv$poll_id)) # 20
length(unique(polls_nv$pollster_id)) # 15

# South Carolina dataset
polls_sc <- polls %>% 
              filter(state == "South Carolina") %>%
              dplyr::select(question_id, poll_id, state, pollster_id, pollster, sponsor_ids, sponsors, 
                            sample_size, population, methodology, start_date, end_date,
                            party, answer, ans_oth, candidate_id, pct)

summary(polls_sc) # Jan 2019 to Feb 2020
length(unique(polls_sc$poll_id)) # 51
length(unique(polls_sc$pollster_id)) # 24

# Minnesota dataset
polls_mn <- polls %>% 
              filter(state == "Minnesota") %>%
              dplyr::select(question_id, poll_id, state, pollster_id, pollster, sponsor_ids, sponsors, 
                            sample_size, population, methodology, start_date, end_date,
                            party, answer, ans_oth, candidate_id, pct)

summary(polls_mn) # June 2019 to Mar 2020
length(unique(polls_mn$poll_id)) # 6
length(unique(polls_mn$pollster_id)) # 6

# Virginia dataset
polls_va <- polls %>% 
              filter(state == "Virginia") %>%
              dplyr::select(question_id, poll_id, state, pollster_id, pollster, sponsor_ids, sponsors, 
                            sample_size, population, methodology, start_date, end_date,
                            party, answer, ans_oth, candidate_id, pct)

summary(polls_va) # April 2019 to Mar 2020
length(unique(polls_va$poll_id)) # 10
length(unique(polls_va$pollster_id)) # 8

# Washington dataset (?)
polls_wa <- polls %>% 
              filter(state == "Washington") %>%
              dplyr::select(question_id, poll_id, state, pollster_id, pollster, sponsor_ids, sponsors, 
                            sample_size, population, methodology, start_date, end_date,
                            party, answer, ans_oth, candidate_id, pct)

summary(polls_wa) # July 2019 to Mar 2020
length(unique(polls_wa$poll_id)) # 7
length(unique(polls_wa$pollster_id)) # 5

### Super Tuesday
# Alabama, Arkansas, California, Colorado,
# Maine, Mass, Minnesota, NC, Oklahoma, Tennessee, Texas,
# Utah, Vermont, Virginia (14)

ST <- c("Alabama", "Arkansas", "California", "Colorado",
        "Maine", "Massachusetts", "Minnesota", "North Carolina",
        "Oklahoma", "Tennessee", "Texas", "Utah",
        "Vermont", "Virginia")

polls_ST <- polls %>% 
    filter(state %in% ST) %>%
    dplyr::select(question_id, poll_id, state, pollster_id, pollster, sponsor_ids, sponsors, 
                  sample_size, population, methodology, start_date, end_date,
                  party, answer, ans_oth, candidate_id, pct)

summary(polls_ST) # February 2019 to Mar 2020
length(unique(polls_ST$poll_id)) # 163
length(unique(polls_ST$pollster_id)) # 46

