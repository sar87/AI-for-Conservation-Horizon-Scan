#This code produces the analysis of the Horizon scan workshop results. This code was written by Dr Sam Reynolds (Conservation Science Group, Department of Zoology, Cambridge University, The David Attenborough Building, Pembroke Street, Cambridge CB2 3QZ, UK) and Dr Eleanor Tew (Forestry England, 620 Bristol Business Park, Coldharbour Lane, Bristol BS16 1EJ, United Kingdom).

library(readxl)
library(dplyr)
library(data.table)

#Set Working Directory
setwd("xxx")
#This is a file which contains the list of idea titles and the unique idea ID numbers
Idea_Names_Master<-read.csv("xxx/Idea_Names_Workshop.csv")

All_files <- list.files() #Empty list from working directory
Partial_files <- list() #Empty list for the partially complete files
Complete_files <- list() #Empty list for the Complete files

#This loop reads in all the files in the working directory and then assigns them to the correct list, complete or partially completed. 

for(i in 1:length(All_files)){
  tryCatch({
    Temp <- as.data.frame(read_excel(All_files[i]))
    Temp$Application_ID <- Idea_Names_Master$Application_ID[match(Temp$Title, Idea_Names_Master$Title)]
    Temp <- Temp[which(Temp$Application_ID != 19),]
    
    if(!is.numeric(Temp$`User Score (1 - 1000)`)) {
      Temp$`User Score (1 - 1000)` <- as.numeric(Temp$`User Score (1 - 1000)`)
    }
    
    if(any(is.na(Temp$`User Score (1 - 1000)`))){
      Partial_files[[length(Partial_files)+1]] <- Temp
      names(Partial_files)[length(Partial_files)] <- All_files[i]
      # Partial_files <- c(Partial_files, All_files[i])
    } else {
      Complete_files[[length(Complete_files)+1]] <- Temp
      names(Complete_files)[length(Complete_files)] <- All_files[i]
      # Complete_files <- c(Complete_files, All_files[i])
    }
  })
}

# Print results and check if the files have been assigned to the correct list
print(Partial_files)

print(Complete_files)


######### Complete Lists and Table generation
{
  
  Complete_scores_list <- list()
  
  # Function to concatenate notes made by experts  
  concatenate_notes <- function(df) {
    df %>%
      group_by(`Application_ID`) %>%
      summarise(Concatenated_Notes = paste(`Notes`, collapse = " ")) %>%
      ungroup()
  }
  
  #### Table for Complete Files ####
  
  for(i in 1:length(Complete_files)){
    tryCatch({
      Temp <- Complete_files[[i]]
      Temp$Application_ID <- Idea_Names_Master$Application_ID[match(Temp$Title, Idea_Names_Master$Title)]
      Temp <- Temp[which(Temp$Application_ID != 19),]
      
      if(!is.numeric(Temp$`User Score (1 - 1000)`)) {
        Temp$`User Score (1 - 1000)` <- as.numeric(Temp$`User Score (1 - 1000)`)
      }
      if(any(is.na(Temp$`User Score (1 - 1000)`))){
        print(paste("NA values found in User Score (1 - 1000), iteration:", i, "File:", file_path))
      }
      if(any(Temp$`User Score (1 - 1000)` == 0)){
        stop(paste("ERROR: Zero value found in User Score (1 - 1000), iteration:", i, "File:", file_path))
      }
      Temp$Rank <- rank(-Temp$`User Score (1 - 1000)`)
      
      if(nrow(Temp) == 27){
        Complete_scores_list[[i]] <- Temp
        names(Complete_scores_list)[i] <- names(Complete_files)[i]
        if(length(unique(Temp$`User Score (1 - 1000)`)) < 27){
          stop(paste("Not 27 unique, iteration:", i, "File:", file_path))
        }
      } else {
        stop(paste("ERROR: Not 27 rows, iteration:", i, "File:", file_path))
      }
    }, error = function(e) {
      warning(paste("Error processing file:", file_path, "\nError:", e$message))
    })
  }
  
  
  ## CREATE TABLE OF COMPLETED MEDIAN RANKS ##
  
  #creates a dataframe out of the list that is produced in the above loop
  complete_combined_df <- bind_rows(Complete_scores_list, .id = "Source")
  complete_combined_df <- complete_combined_df[which(complete_combined_df$Application_ID != 19),]
  
  
  #makes a smaller data frame containing only the columns application ID, Ideas and Rank
  complete_score_df <- complete_combined_df %>% select(Application_ID,Title, Rank)
  
  #Calculates the median rank for each Application ID number
  complete_median_ranks <- complete_score_df %>%
    group_by(Application_ID) %>%
    summarise(Median_Rank = median(Rank))
  
  #Joins the median ranks
  complete_All_Ranks<-complete_median_ranks %>%
    left_join(Idea_Names_Master, by = "Application_ID") %>%
    select(Application_ID, Title, Median_Rank) %>%
    distinct()
  
  #writes the all ranks document to a .csv file at the specified save path
  write.csv(complete_All_Ranks, file.choose(), row.names = FALSE)
  
  ##################################################
  New_table_C <- Complete_scores_list[[1]]
  New_table_C <- New_table_C[,which(colnames(New_table_C) %in% c("Application_ID", "Title", "Rank"))]
  colnames(New_table_C)[3] <- gsub(".xlsx", "", unlist(strsplit(names(Complete_scores_list)[1], "_"))[length(unlist(strsplit(names(Complete_scores_list)[1], "_")))])
  for(i in 2:length(Complete_scores_list)){
    Temp <- Complete_scores_list[[i]]
    New_table_C[,(ncol(New_table_C)+1)] <- Temp$Rank[match(New_table_C$Application_ID, Temp$Application_ID)]
    colnames(New_table_C)[ncol(New_table_C)] <- gsub(".xlsx", "", unlist(strsplit(names(Complete_scores_list)[i], "_"))[length(unlist(strsplit(names(Complete_scores_list)[i], "_")))])
  }
  
  # Concatenate notes from all items in the column "Notes"
  all_notes <- sapply(1:nrow(New_table_C), function(j) {
    notes <- sapply(Complete_scores_list, function(x) x$Notes[match(New_table_C$Application_ID[j], x$Application_ID)])
    notes <- notes[!is.na(notes)] # Remove NA values
    paste(notes, collapse = " --##-- ")
  })
  
  #Add median ranks
  New_table_C <- merge(New_table_C, complete_median_ranks[, c("Application_ID", "Median_Rank")], by = "Application_ID", all.x = TRUE)
  
  # Add the concatenated notes as a new column in New_table_C
  New_table_C$All_Notes <- all_notes
  
  write.csv(New_table_C, file.choose(), row.names = FALSE)
}

# Partial Scores List and Table Generation

#To incorporate their partially scored lists with the lists of participants who scored all ideas, we computed a ‘division factor’. The division factor is the ratio of total number of ideas to the number of ideas scored, plus 1. To calculate the final rank we multiplied the rank by the division factor.This creates a buffer in the ranking scale that allows for the possibility that ideas unranked by a participant could potentially be ranked higher than their partially ranked ideas if they were to be evaluated. 

# This loop adjusts the partially ranked scores to be incorporated with the complete scored lists.  

Partial_scores_list <- list()

for(i in 1:length(Partial_files)) {
  Temp <- Partial_files[[i]]
  Temp$Application_ID <- Idea_Names_Master$Application_ID[match(Temp$Title, Idea_Names_Master$Title)]
  Temp <- Temp[which(Temp$Application_ID != 19),]
  
  # Convert User Score to numeric
  if(!is.numeric(Temp$`User Score (1 - 1000)`)) {
    Temp$`User Score (1 - 1000)` <- as.numeric(Temp$`User Score (1 - 1000)`)
  }
  Temp$Rank <- rank(-Temp$`User Score (1 - 1000)`)
  
  # Count entries in the 'Rank' column
  row_count <- nrow(Temp)
  rank_count <- sum(!is.na(Temp$'User Score (1 - 1000)') & Temp$`User Score (1 - 1000)` != "")
  division_factor<- row_count/(rank_count+1)
  
  # Create new column 'new rank' by multiplying 'Rank' by the division factor
  Temp$new_rank <- Temp$Rank * division_factor
  
  # Store the processed data frame
  Partial_scores_list[[i]] <- Temp
  names(Partial_scores_list)[i] <- names(Partial_files)[i]
}

#function to remove ranks that are automatically allocated by R to unscored ideas in partially scored score sheets
kill <- function(data_insert){
  data_insert[which(is.na(data_insert$`User Score (1 - 1000)`)), "new_rank"] <- NA
  return(data_insert)
}
Partial_scores_list <- lapply(Partial_scores_list, kill)

#creates a dataframe out of the list that is produced in the above loop
partial_combined_df <- bind_rows(Partial_scores_list, .id = "Source")
partial_combined_df[which(is.na(partial_combined_df$`User Score (1 - 1000)`)),"new_rank"] <- NA #applies kill function

#makes a smaller data frame containing only the columns application ID, Ideas and Rank
partial_score_df <- partial_combined_df %>% select(Application_ID, Title, new_rank, Category)

#Calculates the median rank for each Application ID number
partial_median_ranks <- partial_score_df %>%
  group_by(Application_ID) %>%
  summarise(Median_Rank = median(new_rank, na.rm = TRUE))

#Joins the median ranks
Partial_all_Ranks<-partial_median_ranks %>%
  left_join(Idea_Names_Master, by = "Application_ID") %>%
  select(Application_ID, Title, Category, Median_Rank) %>%
  distinct()

### Produces table from the list of partially scored files

New_table_p <- Partial_scores_list[[1]]
New_table_p <- New_table_p[,which(colnames(New_table_p) %in% c("Application_ID", "Category", "Title", "new_rank"))]
colnames(New_table_p)[4] <- gsub(".xlsx", "", unlist(strsplit(names(Partial_scores_list)[1], "_"))[length(unlist(strsplit(names(Partial_scores_list)[1], "_")))])
for(i in 2:length(Partial_scores_list)){
  Temp <- Partial_scores_list[[i]]
  New_table_p[,(ncol(New_table_p)+1)] <- Temp$new_rank[match(New_table_p$Application_ID, Temp$Application_ID)]
  colnames(New_table_p)[ncol(New_table_p)] <- gsub(".xlsx", "", unlist(strsplit(names(Partial_scores_list)[i], "_"))[length(unlist(strsplit(names(Partial_scores_list)[i], "_")))])
}

# Concatenate notes from all items in the column "Notes"
all_notes <- sapply(1:nrow(New_table_p), function(j) {
  notes <- sapply(Partial_scores_list, function(x) x$Notes[match(New_table_p$Application_ID[j], x$Application_ID)])
  notes <- notes[!is.na(notes)] # Remove NA values
  paste(notes, collapse = " --##-- ")
})

#Add median ranks
New_table_p <- merge(New_table_p, partial_median_ranks[, c("Application_ID", "Median_Rank")], by = "Application_ID", all.x = TRUE)

# Add the concatenated notes as a new column in New_table
New_table_p$All_Notes <- all_notes

ncol(New_table_p) #see how many columns there are col num - 3 = number of participants
View(New_table_p)

### Creates the combined table with Complete and Partially scored files

All_scores_table <- New_table_C[,  1:(which(colnames(New_table_C) == "Median_Rank")-1)]
New_table_p <- New_table_p[order(New_table_p$Application_ID),]

# All_scores_table <- cbind(All_scores_table, New_table_p)
All_scores_table <- cbind(All_scores_table, New_table_p[,4:(ncol(New_table_p)-2)])

for(i in 1:nrow(All_scores_table)){
  All_scores_table[i,"Median_rank"] <- median(as.numeric(All_scores_table[i,which(colnames(All_scores_table) == "Burgess"):which(colnames(All_scores_table) == "Strassburg")]), na.rm = TRUE)
}

All_scores_table$Final_rank <- rank(All_scores_table$Median_rank)

write.csv(All_scores_table, file.choose(), row.names = FALSE)
