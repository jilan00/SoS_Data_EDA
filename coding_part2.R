#################################
#                               #
#       Coding Challenge        #
#           Part 2              #
#                               #
#################################

# 1. Using the dataset and scale_score function below, add a scale score for Bullying perpetration (variables that begin with "BULLYING_PERP") and 
#    General Well Being (variables that begin with "GEN_WELL_BEING") to the dataset. Each scale score should be the mean of the 
#    item responses with a minimum of 3 valid responses. What are the moments (mean, variance, skewness, and kurtosis) for 
#    Bullying Perpetration and General Well-being at each wave?
# 2. Create a subset of the dataset with only wave 1 observations. How many students have missing data (NA) on all of the
#    following variables: Black, Indigeneous, Asian, Hispanic, Multiracial, Islander, and White?
# 3. With the full dataset, run the example code (under @example) for the catacode function below to create a new Race_Ethnicity variable.
#    a. What are the count (i.e. frequency) and proportion of responses for the new Race_Ethnicity variable?
#    Now join the new Race_Ethnicity variable onto the wave 1 dataset you created in Step 2.
#    b. What is the mean and standard deviation of Bullying Perpetration for each Race_Ethnicity group?
#    c. Using ggplot2, create a plot of the mean Bullying Perpetration score for each Race_Ethnicity group.
# 4. With the full dataset, create a second variable using the catacode function with the same race and ethnicity variables (Black:White) and
#    the "multiple" approach. You will want to change the new.name argument so this new variable has a different name than the one created in Step 3.
#    a. What are the count (i.e. frequency) and proportion of responses for the new Race_Ethnicity variable?
#    Now join the new Race_Ethnicity variable onto the wave 1 dataset you created in Step 2.
#    b. What is the mean and standard deviation of Bullying Perpetration for each Race_Ethnicity group based on the "multiple" approach?
#    c. Using ggplot2, create a plot of the mean Bullying Perpetration score for each race/ethnic group and faceted by
#       coding approach (i.e., approach = "priority" vs. approach = "multiple")
# 5. Repeat steps 3 and 4 with General Well-being score instead of Bullying Perpetration and with Bisexual, Gay_lesbian, Other_SexOr, Questioning,
#    and Straight instead of the race/ethnicity variables. In the approach = "priority" condition (Step 3), use c("Other_SexOr", "Bisexual", "Gay_lesbian")

## load data
sos.long2 <- readRDS(file = "sos_long.rds")

#### Scale Score Function ####
#' @param data a \code{data.frame}
#' @param items a character vector of column names
#' @param type should the score be the sum or the mean of item responses?
#' @param min.valid the minimum number of valid responses to receive a score.
#'
#' @example 
#' scale_score(myData, items = c("Item1", "Item2", "Item3", "Item4", "Item5"), type = "sum", min.valid = 1)

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


#### Coding check-all-that-apply responses into a single variable #####
#' @param data \code{data.frame}. If \code{time} is specified, data must be in "long" format with rows being one time point per subject
#' @param id column name for the id variable in \code{data}
#' @param approach one of "longer", "all", "multiple", "priority", or "mode". See Details.
#' @param ... <\code{\link[tidyr]{tidyr_tidy_select}}> column names in data indicating the dichotomous check-all-that-apply variables to combine
#' @param endorse value indicating endorsement of the category. Common values are 1 (default), "yes", or 2 (for SPSS data).
#' @param time column name for the time variable in \code{data}
#' @param priority character vector of one or more names supplied to \code{...} indicating the order to prioritize response categories when \code{approach} is "priority" or "mode".
#' @param new.name character; column name for the created variable
#' @param sep separator to use between values when \code{approach = "all"}
#' @return \code{data.frame}
#' 
#' @details 
#' When \code{approach} is "multiple", "priority", or "mode", subjects with missing data for all columns supplied to ... are removed.
#' 
#' \itemize{
#' \item \code{"longer"} Produces a longer \code{data.frame} with a row for each subject by category option (by time, if specified)
#' \item \code{"all"} Produces \code{data.frame} with a row per subject (and time point, if specified) with new variable comprised of all categories endorsed separated by \code{sep}.
#' }
#' The remaining 3 approaches produce one code across all time points. The output is a data.frame with one row for each subject
#' \itemize{
#' \item \code{"multiple"} If subject endorsed multiple categories within or across time, code as "Multiple".
#' \item \code{"priority"} Same as "multiple" unless subject endorsed category specified in priority argument at any point, then code as priority
#' \item \code{"mode"} Coded as category with the mode (i.e., most common) endorsement across all time points. Ties coded as "Multiple"; Up to 2 categories can be prioritized over the mode by specifying priority argument
#' }
#' 
#' @example 
#' catacode(sos.long, id = StudentID, approach = "priority", Black:White, time = Wave, priority = c("Indigenous", "Islander"), new.name = "Race_Ethnicity")



catacode <- function(data, id, approach, ...,
                     endorse = 1, time = NULL, priority = NULL,
                     new.name = "variable", sep = "-"){
  
  require(dplyr)
  require(tidyr)
  
  if(!approach %in% c("longer", "all", "multiple", "priority", "mode")){
    stop("approach must be one of 'longer', 'by_time', 'multiple', 'priority', or 'mode'")
  }
  
  catacols <- quos(...)
  idv <- enquo(id)
  tv <- enquo(time)
  
  ## Pivot data to longer format with one row for each id x category (x time) combination
  data <- data %>%
    dplyr::select(!!idv, !!tv, !!!catacols)  %>%
    tidyr::pivot_longer(c(!!!catacols), names_to = "new", values_to = "response") 
  
  if(approach == "longer"){
    
    names(data)[names(data) == "new"] <- new.name
    return(data)
  }
  
  if(approach == "all"){
    
    ## Determines all response categories endorsed
    output <- data %>%
      mutate(response = ifelse(response == endorse, new, NA)) %>%                   # Changing values of 1 to the name it represents
      tidyr::pivot_wider(names_from = new, values_from = response) %>%              # Pivot data to wider format with one row for each id x time combination
      unite(col = "new", -!!idv, -!!tv, remove = TRUE, na.rm = TRUE, sep = sep) %>%     # remove = FALSE allows us to examine if the uniting worked
      mutate(new = ifelse(new == "", NA, new))                            # If subject responded NA to all categories for all waves, code as NA rather than ""
    names(data)[names(data) == "new"] <- new.name                                # Note: Combinations are currently in the order in which the variable appears in the dataset
    
    return(output)
  }
  
  if(approach %in% c("multiple", "priority", "mode")){
    
    ## Summarizes each subject's response pattern
    across.prep <- data %>% 
      filter(response == endorse) %>%                    
      group_by(!!idv, new) %>%
      summarize(n_time = n(), .groups = "drop_last") # number of times a subject identified as a certain category
  }
  
  if(approach == "multiple"){
    
    # Combines any multiple responses as Multiple
    output <- across.prep %>%
      summarize(demo = case_when(n() == 1 ~ new,                           # gave the same response across time
                                 TRUE ~ "Multiple"), .groups = "keep") %>% # If subject > 1 response, they were categorized as Multiple
      unique()                                                         # removes redundant rows; produces unique code for each subject
    names(output)[names(output) == "demo"] <- new.name
    
    return(output)
  }
  
  ## priority argument check
  if(approach %in% c("priority", "mode")){
    
    if(is.null(priority)){
      
      stop("Must specify priority argument when approach is priority' or 'mode'.")
      
      
    } else if(!all(priority %in% unique(across.prep$new))){
      stop("Values in priority argument must match column names supplied to ...")
    }
  }
  
  
  if(approach == "priority"){
    
    if(length(priority) == 1){
      
      
      # Prioritizing 1 response
      output <- across.prep %>%
        summarize(demo = case_when(sum(new == priority) > 0 ~ priority,   # If subject gave priority at any point, code as priority
                                   n() == 1 ~ new,                        # gave the same response across waves
                                   TRUE ~ "Multiple"), .groups = "keep")  # gave multiple response across waves
      
    } else if(length(priority) == 2){
      
      output <- across.prep %>%
        summarize(demo = case_when(sum(new == priority[[1]]) > 0 ~ priority[[1]],   # If subject gave priority at any point, code as priority
                                   sum(new == priority[[2]]) > 0 ~ priority[[2]],
                                   n() == 1 ~ new,                        # gave the same response across waves
                                   TRUE ~ "Multiple"), .groups = "keep")  # gave multiple response across waves
      
    } else if(length(priority) == 3){
      output <- across.prep %>%
        summarize(demo = case_when(sum(new == priority[[1]]) > 0 ~ priority[[1]],   # If subject gave priority at any point, code as priority
                                   sum(new == priority[[2]]) > 0 ~ priority[[2]],
                                   sum(new == priority[[3]]) > 0 ~ priority[[3]],
                                   n() == 1 ~ new,                        # gave the same response across waves
                                   TRUE ~ "Multiple"), .groups = "keep")  # gave multiple response across waves
      
    } else if(length(priority) == 4){
      output <- across.prep %>%
        summarize(demo = case_when(sum(new == priority[[1]]) > 0 ~ priority[[1]],   # If subject gave priority at any point, code as priority
                                   sum(new == priority[[2]]) > 0 ~ priority[[2]],
                                   sum(new == priority[[3]]) > 0 ~ priority[[3]],
                                   sum(new == priority[[4]]) > 0 ~ priority[[4]],
                                   n() == 1 ~ new,                        # gave the same response across waves
                                   TRUE ~ "Multiple"), .groups = "keep")  # gave multiple response across waves
    } else {stop("priority argument maximum length is 4.")}
    
    output <- output %>%
      unique()                # removes redundant rows; produces unique code for each subject
    names(output)[names(output) == "demo"] <- new.name
    return(output)
  }
  
  if(approach == "mode"){
    
    # The mode response with ties coded as multiple
    if(is.null(priority)){
      
      intermediate <- across.prep %>%
        summarize(demo = case_when(n() == 1 ~ new,                                  # gave the same response across waves
                                   sum(n_time == max(n_time)) > 1 ~ "Multiple",   # ties - gave multiple response with equal frequency
                                   n_time == max(n_time) ~ new,                   # gave multiple response across waves but one at a higher frequency
                                   TRUE ~ "Temp"), .groups = "keep")                # temporary code given to rows that will be removed
      
      
    } else if(length(priority) == 1){
      
      # A priority, then the mode response with ties coded as multiple 
      intermediate <- across.prep %>%
        summarize(demo = case_when(sum(new == priority) > 0 ~ priority,             # If subject gave priority at any point, code as priority
                                   n() == 1 ~ new,                                  # gave the same response across waves
                                   sum(n_time == max(n_time)) > 1 ~ "Multiple",   # ties - gave multiple response with equal frequency
                                   n_time == max(n_time) ~ new,                   # gave multiple response across waves but one at a higher frequency
                                   TRUE ~ "Temp"), .groups = "keep")                # temporary code given to rows that will be removed
      
    } else if(length(priority) == 2){
      
      # A priority, then the mode response with ties coded as multiple 
      intermediate <- across.prep %>%
        summarize(demo = case_when(sum(new == priority[[1]]) > 0 ~ priority[[1]],  # If subject gave priority at any point, code as priority
                                   sum(new == priority[[2]]) > 0 ~ priority[[2]],
                                   n() == 1 ~ new,                                  # gave the same response across waves
                                   sum(n_time == max(n_time)) > 1 ~ "Multiple",   # ties - gave multiple response with equal frequency
                                   n_time == max(n_time) ~ new,                   # gave multiple response across waves but one at a higher frequency
                                   TRUE ~ "Temp"), .groups = "keep")                # temporary code given to rows that will be removed
    } else {stop("When approach = 'mode', the maximum length for the priority argument is 2.")}
    
    output <- intermediate %>%
      filter(demo != "Temp") %>%
      unique()                    # removes redundant rows; produces unique code for each subject
    names(intermediate)[names(intermediate) == "demo"] <- new.name
    return(output)
  }
}


# --------    Step 1  ---------------
library(dplyr)
library(skimr)

## creating general well-being and bullying perpetration scores
sos.long2 <- sos.long2 %>% 
  mutate(General_Well.being = scale_score(., items = paste0("GEN_WELL_BEING_", 1:8), type = "mean", min.valid = 3),
         Bullying_Perp = scale_score(., items = paste0("BULLYING_PERP_", 1:9), type = "mean", min.valid = 3))

#### calculating moments ####
# Define statistics to calculate
# This creates a new skim function
moments <- skim_with(numeric = sfl(mean = ~mean(., na.rm = TRUE), variance = ~var(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE),
                                   skew = ~datawizard::skewness(.)[[1]], kurtosis = ~datawizard::kurtosis(.)[[1]]), append = FALSE)

## uses our new function (moments) to calculate the stats
moments.by.wave <- sos.long2 %>%
  group_by(Wave) %>%
  moments(General_Well.being, Bullying_Perp)


# --------    Step 2  ---------------

sos.w1 <- sos.long2 %>%
  filter(Wave == 1)

re.cols <- c("Black", "Indigenous", "Asian", "Hispanic", "Multiracial", "Islander",  "White")

# TRUE or FALSE: Is the sum of missing values == size of re.cols (i.e., 7)?
table(rowSums(is.na(sos.w1[,re.cols])) == length(re.cols))

# --------    Step 3  ---------------

library(gtsummary)
race.priority <- catacode(sos.long2, id = StudentID, approach = "priority", Black:White, time = Wave,
                       priority = c("Indigenous", "Islander"), new.name = "Race_Ethnicity_P")

# a
tbl_summary(race.priority, include = Race_Ethnicity_P)


sos.w1 <- sos.w1 %>%
  left_join(race.priority)

# b
# bpr stands for bullying, priority, race
bpr <- sos.w1 %>%
  group_by(Race_Ethnicity_P) %>%
  moments(Bullying_Perp)

# c
library(ggplot2)
bpr.plot <- bpr %>%
  filter(!is.na(Race_Ethnicity_P)) %>%
  ggplot(aes(x = Race_Ethnicity_P, y = numeric.mean)) +
  geom_col() +
  theme_bw()


# --------    Step 4  ---------------

race.multiple <- catacode(sos.long2, id = StudentID, approach = "multiple", Black:White, time = Wave, new.name = "Race_Ethnicity_M")

# a
tbl_summary(race.multiple, include = Race_Ethnicity_M)


sos.w1 <- sos.w1 %>%
  left_join(race.multiple)

# b
# bmr stands for bullying, multiple, race
bmr <- sos.w1 %>%
  group_by(Race_Ethnicity_M) %>%
  moments(Bullying_Perp)

# c
bpmr <- bmr %>% mutate(Approach = "Multiple") %>%
  rename(Race_Ethnicity = Race_Ethnicity_M) %>%
  bind_rows(mutate(bpr, Approach = "Priority") %>%
              rename(Race_Ethnicity = Race_Ethnicity_P))

bpmr.plot <- bpmr %>%
  filter(!is.na(Race_Ethnicity)) %>%
  ggplot(aes(x = Race_Ethnicity, y = numeric.mean)) +
  geom_col() +
  theme_bw() +
  facet_wrap(~Approach)


# --------    Step 5  ---------------

#### Priority ####
sexor.priority <- catacode(sos.long2, id = StudentID, approach = "priority", Bisexual, Gay_lesbian, Other_SexOr, Questioning, Straight,
                           priority = c("Other_SexOr", "Bisexual", "Gay_lesbian"), time = Wave, new.name = "Sexual_Orientation_P")

# a
tbl_summary(sexor.priority, include = Sexual_Orientation_P)


sos.w1 <- sos.w1 %>%
  left_join(sexor.priority)

# b
# gps stands for general wellbeing, priority, sexual orientation
gps <- sos.w1 %>%
  group_by(Sexual_Orientation_P) %>%
  moments(General_Well.being)

# c
gps.plot <- gps %>%
  filter(!is.na(Sexual_Orientation_P)) %>%
  ggplot(aes(x = Sexual_Orientation_P, y = numeric.mean)) +
  geom_col() +
  theme_bw() 


#### Multiple ####
sexor.multiple <- catacode(sos.long2, id = StudentID, approach = "multiple", Bisexual, Gay_lesbian, Other_SexOr, Questioning, Straight,
                           time = Wave, new.name = "Sexual_Orientation_M")

# a
tbl_summary(sexor.multiple, include = Sexual_Orientation_M)


sos.w1 <- sos.w1 %>%
  left_join(sexor.multiple)

# b
# gms stands for general wellbeing, multiple, sexual orientation
gms <- sos.w1 %>%
  group_by(Sexual_Orientation_M) %>%
  moments(General_Well.being)

# c
gpms <- gms %>% mutate(Approach = "Multiple") %>%
  rename(Sexual_Orientation = Sexual_Orientation_M) %>%
  bind_rows(mutate(gps, Approach = "Priority") %>%
              rename(Sexual_Orientation = Sexual_Orientation_P))

gpms.plot <- gpms %>%
  filter(!is.na(Sexual_Orientation)) %>%
  ggplot(aes(x = Sexual_Orientation, y = numeric.mean)) +
  geom_col() +
  theme_bw() +
  facet_wrap(~Approach)
