#################################
#                               #
#       Coding Challenge        #
#           Part 1              #
#                               #
#################################

# 1. Use the dataset we have used previously ("sources-of-strength_w1-w4_plus_demographics_by_wave_05_10_2021.sav"),
#    create a smaller dataset that includes the following variables:
#    - SUBJECT_ID, CONDITION_W1, SCHOOLS_W1
#    - Any variable that starts with "GENDER", "RACE", or "SEXUAL"
#    - Any variable that starts with "BULLYING_PERP" or "GEN_WELL_BEING"
# 2. Use the scale_score function below to add a scale score for Bullying perpetration and General Well Being
#    to the dataset. Each scale score should be the mean of the item responses with a minimum of 3 valid responses.
# 3. The dataset is currently in "wide" format. Create a separate version of the data in "long" format.
#    The tidyr package can help with this.
# 4. Using with the wide or long dataset, whichever you think is easier, answer:
#    a. How many students identified as transgender each wave?
#    b. Create a new Transgender variable where a student is coded 1 if they identified as transgender in ANY wave
#       and coded 0 if they identified as cisgender in ALL waves. How many students identified as transgender?
# 5. Create a new Gender variable that combines the dichotomous responses to GENDER_FEMALE_W1, GENDER_MALE_W1, and GENDER_OTHER_W1.
#    Using your new variable, how many students identified as:
#    a. Female only
#    b. Male only
#    c. Other only
#    d. Female and Male


#### Scale Score Function ####
# data - a data.frame
# items -  a character vector of column names
# type - should the score be the sum or the mean of item responses?
# min.valid = The minimum number of valid responses to receive a score.
#
# example:
# scale_score(myData, items = c("Item1", "Item2", "Item3", "Item4", "Item5"), type = "sum", min.valid = 1)

scale_score <- function(data, items, type = c("sum", "mean"), min.valid = NULL){
  
  score <- rowSums(data[, items], na.rm = TRUE)
  
  if(type == "mean"){
    c <- rowSums(!is.na(data[, items]), na.rm = TRUE)
    score <- score / c
  }
  
  nacount <- rowSums(is.na(data[, items]), na.rm = TRUE)
  
  if(!is.null(min.valid)){
    score <- ifelse((length(items) - nacount) < min.valid, NA, score)
  } else {
    score <- ifelse(nacount == length(items), NA, score)
  }
  
  return(score)
}


## Step 1
library(dplyr)
sos.orig <- haven::read_sav("sources-of-strength_w1-w4_plus_demographics_by_wave_05_10_2021.sav")
sos <- sos.orig %>%
  select(SUBJECT_ID, CONDITION_W1, SCHOOLS_W1,
         starts_with("GENDER"), starts_with("RACE"), starts_with("SEXUAL"),
         starts_with("BULLYING_PERP"), starts_with("GEN_WELL_BEING"),
         -all_of(paste0("Gender_W", 1:4)), -Gender,-Race,
         -all_of(paste0("SexualOr_W", 1:4)))

## Step 2
library(purrr)
# Define scales
bp.items <- paste0("BULLYING_PERP_", 1:9)
gwb.items <- paste0("GEN_WELL_BEING_", 1:8)

# replicate item list for each wave
all.items <- c(lapply(1:4, function(x) paste0(bp.items, "_W", x)),
               lapply(1:4, function(x) paste0(gwb.items, "_W", x))) %>%
  set_names(c(paste0("Bullying_Perp_W", 1:4),paste0("General_Well.being_W", 1:4)))

# calculate score for each scale in each wave; bind into a dataframe
scores <- map_dfc(.x = all.items, ~scale_score(sos, items = .x, type = "mean", min.valid = 3))

# add scores to original dataframe
sos.scores <- bind_cols(sos, scores)
# check if scoring function worked properly
sos.scores %>% select(all_of(all.items$Bullying_Perp_W1), Bullying_Perp_W1) %>% View()

# Step 3
sos.scores.long <- tidyr::gather(sos.scores, "Variable", "Value", -c(SUBJECT_ID, CONDITION_W1, SCHOOLS_W1)) %>%
  mutate(Wave = stringr::str_sub(Variable, start = -1),
         Variable = stringr::str_remove(Variable, "_W[0-9]|_w[0-9]")) %>%
  tidyr::spread(Variable, Value)

# Step 4
table(sos.scores.long$Wave, sos.scores.long$GENDER_TRANSGENDER, useNA = "always")
transgender <- ifelse(is.na(scale_score(sos.scores, paste0("GENDER_TRANSGENDER_W", 1:4), "sum")), 0, 1)
table(transgender, useNA = "always")

# Step 5
gender <- case_when(sos.scores$GENDER_FEMALE_W1 == 1 & sos.scores$GENDER_MALE_W1 == 1 ~ "Both",
                    sos.scores$GENDER_FEMALE_W1 == 1 & is.na(sos.scores$GENDER_MALE_W1) & is.na(sos.scores$GENDER_OTHER_W1) ~ "Female",
                    sos.scores$GENDER_MALE_W1 == 1 & is.na(sos.scores$GENDER_FEMALE_W1) & is.na(sos.scores$GENDER_OTHER_W1) ~ "Male",
                    sos.scores$GENDER_OTHER_W1 == 1 & is.na(sos.scores$GENDER_FEMALE_W1) & is.na(sos.scores$GENDER_MALE_W1) ~ "Other",
                    is.na(sos.scores$GENDER_OTHER_W1) & is.na(sos.scores$GENDER_FEMALE_W1) & is.na(sos.scores$GENDER_MALE_W1) ~ NA_character_,
                    TRUE ~ "Other")

table(gender, useNA = "always")
