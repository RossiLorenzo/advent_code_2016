library(magrittr)

###################### PART 1 ######################
# Read and clean data
all_codes = readLines("Input4_1.txt")
all_codes_df = data.frame(
  names = gsub("-[0-9].*", "", all_codes),
  names_no_hash = gsub("-[0-9].*|-", "", all_codes),
  rooms = as.numeric(gsub("[a-z]|-|\\[|\\]", "", all_codes)),
  checksum = gsub(".*\\[|\\]", "", all_codes)
)

# Go line by line and check if conditions are satisfied
all_codes_df$decoy = apply(all_codes_df, 1, function(x){
  real_checksum = strsplit(x[2], "", fixed = TRUE)[[1]] %>% 
    table() %>% 
    sort(decreasing = TRUE) %>% 
    head(5) %>% 
    names() %>% 
    paste0(collapse = "")
  x[4] == real_checksum
})
paste("The sum is:", sum(all_codes_df$rooms[all_codes_df$decoy]))

###################### PART 2 ######################
shift_letters = function(myletter, myshift){
  if(myletter == "-")
    return(" ")
  letters[(which(letters %in% myletter) + myshift - 1) %% length(letters) + 1]
}
all_codes_df$decoded = apply(all_codes_df, 1, function(x){
  strsplit(x[1], "", fixed = TRUE)[[1]] %>% 
    sapply(function(y){
      shift_letters(y, as.numeric(x[3]))
    }) %>% 
    paste0(collapse = "")
})
paste("North Pole Objectrs are in room:", all_codes_df$rooms[all_codes_df$decoded == "northpole object storage"])
