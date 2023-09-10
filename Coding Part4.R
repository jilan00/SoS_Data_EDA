library(dplyr)
library(purrr)
library(haven)
library(tidyverse)

IES_Goal<-load("/Users/jilan/Downloads/IES_Goal2_Data (1).RData")
IES_Goal

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




catacode <- function(data, id, approach, ...,
                     endorse = 1, time = NULL, priority = NULL,
                     new.name = "variable", sep = "-"){
  
  if(!approach %in% c("longer", "all", "multiple", "priority", "mode")){
    stop("approach must be one of 'longer', 'by_time', 'multiple', 'priority', or 'mode'")
  }
  
  catacols <- quos(...)
  idv <- enquo(id)
  tv <- enquo(time)
  
  ## Pivot data to longer format with one row for each id x category (x time) combination
  data <- data %>%
    dplyr::select(!!idv, !!tv, !!!catacols) %>%
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






# Method1:Student Self Report
'Part 1: Utilize "Multiple" Measurement'


#After exploring the dataset, we found that a lot of students who identified themselves as race_dont know or race_other_specifiy actually put value "1" for identifying themselves as "Hispanic", we are going to find these students and modify their responses by prioritizing "Hispanic" race'

student_self<-dial.sslf %>%
  mutate(RACE_DONT_KNOW_W1=ifelse(RACE_DONT_KNOW_W1==1&HISPANIC_W1==1,0,RACE_DONT_KNOW_W1))

#Then we are going to apply the scale_score function 
b<-c('BULLYING_PERP_1_W1','BULLYING_PERP_2_W1','BULLYING_PERP_3_W1','BULLYING_PERP_4_W1',
     'BULLYING_PERP_5_W1','BULLYING_PERP_6_W1','BULLYING_PERP_7_W1','BULLYING_PERP_8_W1',
     'BULLYING_PERP_9_W1') 
#create a list of items


#Apply the Score Function
student_self<-student_self %>%
  mutate(score=scale_score(., items =b, type = "mean"))%>%
  dplyr::select(STUDENT_ID,score,RACE_WHITE_W1:HAITIAN_W1) %>%
  dplyr::select(-RACE_OTHER_SPECIFY_W1)



#Then apply the catacode function to calculate each student's race
new_race_student<-catacode(student_self, id = STUDENT_ID, approach = "multiple", RACE_WHITE_W1:HAITIAN_W1, new.name = "Race_Ethnicity")


#Combine the new race variable with student_self dataset and then visualize the result
student_self<-left_join(student_self,new_race_student,by='STUDENT_ID')
student_self
student_self_summary<-student_self%>%
  group_by(Race_Ethnicity)%>%
  
  summarize(Score=mean(score),count=n())
student_self_summary

ggplot(aes(x=Score,y=count,color=Race_Ethnicity),data=student_self_summary)+geom_bar(stat='identity',aes(fill=Race_Ethnicity))+geom_text(aes(label=count,vjust=-4))+scale_y_continuous(limits = c(0,200))
#Intrepret the result: After referring back to the codebook, we can conclude that all race ethnicity's score is below 1 which means 
#the general school environment is friendly and immune from bullying. Among all the race groups, Black students and Haitian students are more likely 
#to be bullied. White students have the lowest frequency of being bullied.


'Part 2: Utilize "Priority" Measurement'
new_race_student2<-catacode(student_self, id = STUDENT_ID, approach = "priority", RACE_WHITE_W1:HAITIAN_W1, priority = 'HISPANIC_W1',new.name = "Race_Ethnicity2")


#Combine the new race variable with student_self dataset and then visualize the result
student_self2<-left_join(student_self,new_race_student2,by='STUDENT_ID')

student_self_summary2<-student_self2%>%
  group_by(Race_Ethnicity2)%>%
  summarize(Score=mean(score),count=n()) 
student_self_summary2

student_self_graph2<-ggplot(aes(x=Score,y=count),data=student_self_summary2) +geom_bar(stat='identity',aes(fill=Race_Ethnicity2))+geom_text(aes(label=count,vjust=-4))+scale_y_continuous(limits = c(0,200))
student_self_graph2


'Part 3: Change the standard for priority'
new_race_student3<-catacode(student_self, id = STUDENT_ID, approach = "priority", RACE_WHITE_W1:HAITIAN_W1, priority = c('HISPANIC_W1','HAITIAN_W1'),new.name = "Race_Ethnicity3")

student_self3<-left_join(student_self,new_race_student3,by='STUDENT_ID')

student_self3<-student_self3%>%
  mutate(Race_Ethnicity3=(ifelse(Race_Ethnicity3=="RACE_DONT_KNOW_W1",NA,Race_Ethnicity3)))

student_self_summary3<-student_self3%>%
  group_by(Race_Ethnicity3)%>%
  summarize(Score=mean(score),count=n()) 
student_self_summary3


#Then in order to compare three results together, we need to combine two summary tables and add their different approaches

student_self_summary<-student_self_summary %>%
  mutate(Approach='Multiple')

student_self_summary2<-student_self_summary2 %>%
  mutate(Approach='Priority',Race_Ethnicity=Race_Ethnicity2)%>%
   dplyr::select(-Race_Ethnicity2)%>%
  dplyr::select(Race_Ethnicity,Score,count,Approach)

student_self_summary3<-student_self_summary3 %>%
  mutate(Approach='Priority2',Race_Ethnicity=Race_Ethnicity3)%>%
  dplyr::select(-Race_Ethnicity3)%>%
  dplyr::select(Race_Ethnicity,Score,count,Approach)

student_self_combined<-rbind(student_self_summary,student_self_summary2,student_self_summary3)

student_compared<-ggplot(student_self_combined,aes(x=Score,y=count))+geom_bar(stat = 'identity',aes(fill=Race_Ethnicity))+facet_wrap(.~Approach)+geom_text(aes(label=count,vjust=-4))+scale_y_continuous(limits = c(0,200))
student_compared
#----------------------------------------------------------------
#Teacher self-report dataset
#Part 1: Utilize "Multiple Method"

#The first step is also to modify teachers' self-report races
teacher_self<-dial.tslf %>%
  mutate(RACE_OTHER_W1=ifelse(RACE_OTHER_W1==1&HISPANIC_W1==1,0,RACE_OTHER_W1)) 


#Then we are going to apply the scale_score function 
b2<-c('BULLY_PREV_PROF_DEV_1_W1','BULLY_PREV_PROF_DEV_2_W1','BULLY_PREV_PROF_DEV_3_W1','BULLY_PREV_PROF_DEV_4_W1',
      'BULLY_PREV_PROF_DEV_5_W1','BULLY_PREV_PROF_DEV_6_W1','BULLY_PREV_PROF_DEV_7_W1','BULLY_PREV_PROF_DEV_8_W1',
      'BULLY_PREV_PROF_DEV_9_W1')

teacher_self<-teacher_self %>%
  mutate(score=scale_score(., items=b2, type = "mean"))%>%
  dplyr::select(TEACHER_ID_W1,score,RACE_WHITE_W1:HAITIAN_W1) %>%
  dplyr::select(-RACE_OTHER_SPECIFY_W1)

#Then apply the catacode function to calculate each teacher's race
#First, we use "Multiple" Method
new_race_teacher<-catacode(teacher_self, id = TEACHER_ID_W1, approach = "multiple", RACE_WHITE_W1:HAITIAN_W1, new.name = "Race_Ethnicity")


#Combine the new race variable with teacher_self dataset and then visualize the result
teacher_self<-left_join(teacher_self,new_race_teacher,by='TEACHER_ID_W1')
teacher_self_summary<-teacher_self%>%
  group_by(Race_Ethnicity)%>%
  summarize(Score=mean(score),count=n()) 

teacher_self_graph<-ggplot(aes(x=Score,y=count),data=teacher_self_summary) +geom_bar(stat='identity',aes(fill=Race_Ethnicity))+geom_text(aes(label=count,vjust=-4))+scale_y_continuous(limits = c(0,50))
teacher_self_graph

#Intrepret the result: We can see that the score for teachers' responses aren't influenced by race significantly. 
#Nearly the average response score for teachers from all races are above 2. It means that generally, teachers are satisfied with the 
#bullying prevention actions.


'Part 2: Utilize "Priority" Measurement'

new_race_teacher2<-catacode(teacher_self, id = TEACHER_ID_W1, approach = "priority", RACE_WHITE_W1:HAITIAN_W1,priority='HISPANIC_W1',new.name = "Race_Ethnicity2")

##Combine the new race variable with teacher_self dataset and then visualize the result
teacher_self2<-left_join(teacher_self,new_race_teacher2,by='TEACHER_ID_W1')
teacher_self_summary2<-teacher_self2%>%
  group_by(Race_Ethnicity2)%>%
  summarize(Score=mean(score),count=n()) 

teacher_self_graph2<-ggplot(aes(x=Score,y=count),data=teacher_self_summary2) +geom_bar(stat='identity',aes(fill=Race_Ethnicity2))+geom_text(aes(label=count,vjust=-4))+scale_y_continuous(limits = c(0,50))
teacher_self_graph2



'Part 3: Change the standard of Priority Measurement'
new_race_teacher3<-catacode(teacher_self, id = TEACHER_ID_W1, approach = "priority", RACE_WHITE_W1:HAITIAN_W1, priority = c('HISPANIC_W1','HAITIAN_W1'),new.name = "Race_Ethnicity3")

teacher_self3<-left_join(teacher_self,new_race_teacher3,by='TEACHER_ID_W1')

teacher_self3<-teacher_self3%>%
  mutate(Race_Ethnicity3=(ifelse(Race_Ethnicity3=="RACE_DONT_KNOW_W1",NA,Race_Ethnicity3)))

teacher_self_summary3<-teacher_self3%>%
  group_by(Race_Ethnicity3)%>%
  summarize(Score=mean(score),count=n()) 
teacher_self_summary3

#Then in order to compare three results together, we need to combine two summary tables and add their different approaches

teacher_self_summary<-teacher_self_summary %>%
  mutate(Approach='Multiple')

teacher_self_summary2<-teacher_self_summary2 %>%
  mutate(Approach='Priority',Race_Ethnicity=Race_Ethnicity2) %>%
  dplyr::select(-Race_Ethnicity2) %>%
  dplyr::select(Race_Ethnicity,Score,count,Approach)


teacher_self_summary3<-teacher_self_summary3 %>%
  mutate(Approach='Priority2',Race_Ethnicity=Race_Ethnicity3)%>%
  dplyr::select(-Race_Ethnicity3)%>%
  dplyr::select(Race_Ethnicity,Score,count,Approach)


teacher_self_combined<-rbind(teacher_self_summary,teacher_self_summary2,teacher_self_summary3)

teacher_compared<-ggplot(teacher_self_combined,aes(x=Score,y=count))+geom_bar(stat = 'identity',aes(fill=Race_Ethnicity))+facet_wrap(.~Approach)+geom_text(aes(label=count,vjust=-4))+scale_y_continuous(limits = c(0,50))
teacher_compared

