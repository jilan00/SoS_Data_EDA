#################################
#                               #
#       Coding Challenge        #
#           Part 3              #
#                               #
#################################

# This challenge will focus on project and data management, in addition to further exploring coding approaches.
# We will use the scale_score and catacode functions (see below), and the dataset we have used before. For each step,
# please add headers and comments to your code to remind yourself the purpose of each action. 

## load data
sos.long <- readRDS(file = "school.rds") # This is the how you named the file in Coding Challenge 2
sos.long.k <- readRDS(file = "sos_long.rds") # This is what I have the file named as


# 1. Create an RStudio project and save this script into the same folder as the project.
#    Using RStudio Projects: https://support.rstudio.com/hc/en-us/articles/200526207-Using-RStudio-Projects
# 2. In an R script (it can be this one or you can create a new one), load the data and functions. Adding to the code I started (located below catacode function),
#    add code to convert all General Well Being variables (variables starting with "GEN_WELL_BEING") from from a labelled class to 
#    a factor with "values" as the levels using the mutate and across functions from dplyr and the labelled::to_factor function.
# 3. Using the scale_score function, append a scale score for Bullying perpetration (variables that begin with "BULLYING_PERP") and 
#    General Well Being (variables that begin with "GEN_WELL_BEING") to the dataset. Each scale score should be the mean of the 
#    item responses. The min.valid argument should be 1 less than the number of items in the scale. Then create a table 
#    showing the mean and standard deviation of the Bullying Perpetration and General Well-Being scales at each wave.
#    One option for creating the table is using the gtsummary::tbl_summary function, but you may use another if you prefer.
#    Be sure to assign the table to an object (i.e., use <- or =)
# 4. Use the catacode function with id = StudentID, approach = "all", the variables Black:White, time = Wave, and new.name = "Race".
#    This creates a dataframe with all race identifications for a student in each way. Using this dataframe, create:
#     a) a table showing how many unique racial identities there are in each wave.
#     b) a table showing the count and percentage of students who identified as 1, 2, 3, or 4 unique races across the 4 waves.
#    Be sure to assign each table to an object
# 5. Use the catacode function with id = StudentID, approach = "priority", the variables Female, Male, and Other_Gender in ..., time = Wave,
#    priority = "Other_Gender", and new.name = "Gender". Add the new Gender variable to the dataset in Step 3. Create a table showing the 
#    gender breakdown by wave. Be sure to assign the table to an object.
# 6. Using ggplot, create a line graph showing the change is scale scores over time for each Gender. In other words, have Wave on the x-axis,
#    scale score on the y-axis with the plot facetted by score type (Bullying Perpetration and General Well-Being), and group = Gender.
#    Be sure to assign the plot to an object.
# 7. Save all objects you have created to an RData file using the save function. In an Rmarkdown (or Rnotebook), load the saved RData file
#    using the load function. Then use R code to display the tables from steps 3 - 5 and plot from step 6. Additionally, use headers and text
#    to describe what each table is displaying.



  

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
    names(output)[names(output) == "new"] <- new.name                                # Note: Combinations are currently in the order in which the variable appears in the dataset
    
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


library(dplyr)
library(gtsummary)

#### Step 2  ####
sos <- sos.long.k %>%
  mutate(across(.cols = c(starts_with("BULLYING_PERP")), ~haven::zap_labels(.)), #labelled::to_factor(., levels = "values")),
         across(School:Straight, ~haven::zap_labels(.)))

#### Step 3 ####
# Defining scale constructs
gwb.items <- paste0("GEN_WELL_BEING_", 1:8)
bp.items <- paste0("BULLYING_PERP_", 1:9)

# calculating general well-being and bully perp scores and adding to dataset
sos <- sos %>% 
  mutate(General_Well_being = scale_score(., items = gwb.items, type = "mean", min.valid = length(gwb.items) - 1),
         Bullying_Perp = scale_score(., items = bp.items, type = "mean", min.valid = length(bp.items) - 1))

# Calculating score descriptive statistics
score.descrips <- sos %>%
  tbl_summary(include = c("General_Well_being", "Bullying_Perp"),
                         by = "Wave",
                         statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  as_flex_table()

#### Step 4 ###
# All racial identities for each student in each wave 
allrace <- catacode(sos, id = StudentID, approach = "all", Black:White, time = Wave, new.name = "Race")

# How many unique racial identities there are in each wave?
unique.race.wave <- allrace %>% group_by(Wave) %>%
  summarize(Racial_Identities = length(unique(Race)))

# How many races a student identified with and how many waves they endorsed that identity
unique.race.id <- allrace %>%
  group_by(StudentID, Race) %>%
  summarize(Each_Race = n()) %>%
  filter(!is.na(Race)) %>%
  mutate(Race_count = n())

# The count and percentage of students who identified as 1, 2, 3, or 4 unique races across the 4 waves.
unique.race.id.tab <- unique.race.id %>%
  select(StudentID, Race_count) %>% unique() %>%
  tbl_summary(include = "Race_count") %>%
  as_flex_table()


#### Step 5 ###
# Coding gender 
gender <- catacode(sos, id = StudentID, approach = "priority", Female, Male, Other_Gender,
                   time = Wave, priority = "Other_Gender", new.name = "Gender")
sos <- left_join(sos, gender, by = "StudentID")

gender.by.wave <- sos %>%
  tbl_summary(include = c("Gender"),
              by = "Wave") %>%
  as_flex_table()

#### Step 6 ####
gender.score.plot <- sos %>%
  select(StudentID, Wave, Gender, Bullying_Perp, General_Well_being) %>%
  tidyr::gather(Variable, Score, Bullying_Perp, General_Well_being) %>%
  filter(!is.na(Gender)) %>%
  ggplot(aes(x = Wave, y = Score, colour = Gender, group = Gender)) +
  stat_summary(geom="line", fun = "mean") +
  stat_summary(geom="point", fun = "mean") +
  # geom_line() +
  # geom_point() +
  theme_bw() +
  facet_wrap(~Variable)

#### Step 7 ####
save(score.descrips,unique.race.wave, unique.race.id.tab,
     gender.by.wave, gender.score.plot,
     file = "coding_part3_results.RData")
