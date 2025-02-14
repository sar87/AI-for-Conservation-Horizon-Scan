# AI-for-Conservation-Horizon-Scan
This contains the code to run the analysis for the AI for Conservation Horizon Scan 2024.

# Analysing the scoring from the first round

Set the working directory to the folder that contains the completed score sheets.

For the code to work as written, the scoresheet headers would need to be: Application_ID, Category,	Title,	User Score (1 - 1000),	Notes

The Idea_Names_First_Round.csv file is required to run this code, as written. This serves as a groundtruth for which Application ID links to which idea title.

The code creates a table with the idea rank for each participant, an overall median rank for each idea, and an overall rank for each idea.

# Horizon Scan Workshop Analysis Code

Set the working directory to the folder that contains the completed score sheets.

For the code to work as written, the scoresheet headers would need to be: Application_ID, Category,	Title,	User Score (1 - 1000),	Notes

The Idea_names_master.csv file is required to run this code, as written. This serves as a groundtruth for which Application ID links to which idea title. 

This code takes in all the scoresheets and calculates the median rank for each idea, accross all participants. It also deals with combining score sheets where all ideas were scored, with scoresheets that were only partially complete. This is described below:

Not all online participants could attend the full workshop. To incorporate their partially scored lists with the lists of participants who scored all ideas, we computed a ‘division factor’. The division factor is the ratio of total number of ideas to the number of ideas scored, plus one (Eq. 1):

DF = TI/(SI + 1)
                Eq. 1

Where DF is the division factor, TI is the total number of ideas and SI is the number of ideas scored by the expert.  The partially scored lists were then ranked (e.g. if only 10 ideas were scored, they were assigned ranks 1 through 10, with rank 1 being the most significant idea). To calculate the appropriate final rank for integration with the fully ranked score sheets, we used the following equation (Eq. 2): 

FR = PR*DF
         Eq. 2

Where FR is the final rank, PR is the rank given when all partially scored ideas are ranked and DF denotes the division factor. 

This creates a buffer in the ranking scale that allows for the possibility that ideas unranked by a participant could potentially be ranked higher than their partially ranked ideas if they were to be evaluated. This adjustment expands the range of the partial rankings to align with a full ranking scale, leaving space for potentially unranked items between them. Ideas that were not scored by a participant, did not have a rank imputed and were left blank. Therefore, participants only contributed to the ranking of items they were present to score.

The code creates a table with the idea rank for each participant, an overall median rank for each idea, and an overall rank for each idea.

If you have any issues with the code, please contact me. 

# Authorship

Code was written by Dr Sam A. Reynolds (Conservation Science Group, Department of Zoology, Cambridge University, The David Attenborough Building, Pembroke Street, Cambridge CB2 3QZ, UK) and Dr Eleanor R. Tew (Forestry England, 620 Bristol Business Park, Coldharbour Lane, Bristol BS16 1EJ, United Kingdom).
