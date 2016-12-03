# Create a folder for the new day 
today_day = as.numeric(format(Sys.Date(), "%d"))
folder_name = paste0("Day", today_day)
if(!dir.exists(folder_name))
  dir.create(folder_name)
setwd(folder_name)

# Inside the folder create files for the text
text_file_names = paste0("Day", today_day, "_", 1:2, ".txt")
for(i in text_file_names){
  if(!file.exists(i))
    file.create(i)
}

# And one for the input
input_file_name = paste0("Input", today_day, "_1.txt")
if(!file.exists(input_file_name))
  file.create(input_file_name)

# Finally the R code
R_file_name = paste0("Day", today_day, ".R")
if(!file.exists(R_file_name)){
  cat('###################### PART 1 ######################

###################### PART 2 ######################', file = R_file_name)
}
  
