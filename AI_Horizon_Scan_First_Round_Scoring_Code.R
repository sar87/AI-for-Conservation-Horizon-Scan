library(readxl)
library(dplyr)
library(data.table)

#Set Working Directory
setwd("xxx Score Sheet Ranking")

#replace with file location of Idea_Names_First_Round.csv
Idea_Names_Master<-read.csv("xxx Idea_Names_First_Round.csv")

All_files <- list.files()
Scores_list <- list()

#Function which concatonates the notes written by each participant
concatenate_notes <- function(df) {
  df %>%
    group_by(`Application_ID`) %>%
    summarise(Concatenated_Notes = paste(`Notes`, collapse = " ")) %>%
    ungroup()
}
#Loop to check that each file is correctly completed

#look for NA values
for(i in 1:length(All_files)){
  Temp <- as.data.frame(read_excel(All_files[i]))
  if(!is.numeric(Temp$`User Score (1 - 1000)`)) {
    Temp$`User Score (1 - 1000)` <- as.numeric(Temp$`User Score (1 - 1000)`)
  }
  if(any(is.na(Temp$`User Score (1 - 1000)`))){
    print(paste("NA values found in User Score (1 - 1000), iteration:", i, "File:", All_files[i]))
  }
  #Looking for zero scores
  if(any(Temp$`User Score (1 - 1000)` == 0)){
    stop(paste("ERROR: Zero value found in User Score (1 - 1000), iteration:", i, "File:", All_files[i]))
  }
  #Calculates the ranks of each issue in each sheet, adding a new column called Rank
  Temp$Rank <- rank(-Temp$`User Score (1 - 1000)`)

  #Checking the there are 104 ideas in the document
  if(nrow(Temp) == 104){
    Scores_list[[i]] <- Temp
    names(Scores_list)[i] <- All_files[i]
    if(length(unique(Temp$`User Score (1 - 1000)`)) < 104){
      stop(paste("Not 104 unique, iteration:"), i, "File", All_files[i] )
    }
  }else{
    stop(paste("ERROR: Not 104 rows, iteration:", i, "File", All_files[i]))
  }
}

#creates a dataframe out of the list that is produced in the above loop
combined_df <- bind_rows(Scores_list, .id = "Source")
#makes a smaller data frame containing only the columns application ID, Ideas and Rank
Score_df <- combined_df %>% select(Application_ID,Ideas, Rank)

#Calculates the median rank for each Application ID number
median_ranks <- Score_df %>%
  group_by(Application_ID) %>%
  summarise(Median_Rank = median(Rank))

#Joins the median ranks
All_Ranks<-median_ranks %>%
  left_join(Idea_Names_Master, by = "Application_ID") %>%
  select(Application_ID, Ideas, Median_Rank) %>%
  distinct()

#Creates table of final ranks
New_table <- Scores_list[[1]]
New_table <- New_table[,which(colnames(New_table) %in% c("Application_ID", "Ideas", "Rank"))]
colnames(New_table)[3] <- gsub(".xlsx", "", unlist(strsplit(names(Scores_list)[1], "_"))[length(unlist(strsplit(names(Scores_list)[1], "_")))])
for(i in 2:length(Scores_list)){
  Temp <- Scores_list[[i]]
  New_table[,(ncol(New_table)+1)] <- Temp$Rank[match(New_table$Application_ID, Temp$Application_ID)]
  colnames(New_table)[ncol(New_table)] <- gsub(".xlsx", "", unlist(strsplit(names(Scores_list)[i], "_"))[length(unlist(strsplit(names(Scores_list)[i], "_")))])
}

# Concatenate notes from all items in the column "Notes"
all_notes <- sapply(1:nrow(New_table), function(j) {
  notes <- sapply(Scores_list, function(x) x$Notes[match(New_table$Application_ID[j], x$Application_ID)])
  notes <- notes[!is.na(notes)] # Remove NA values
  paste(notes, collapse = " --##-- ")
})

#Add median ranks
New_table <- merge(New_table, median_ranks[, c("Application_ID", "Median_Rank")], by = "Application_ID", all.x = TRUE)

# Add the concatenated notes as a new column in New_table
New_table$All_Notes <- all_notes

ncol(New_table) #see how many columns there are col num - 3 = number of participants
View(New_table)

write.csv(New_table, file.choose(), row.names = FALSE)

#####





