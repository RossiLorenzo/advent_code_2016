library(stringr)
library(purrr)
library(dplyr)

###################### PART 1 ######################
# Read and clean input data
mystrings = readLines("Input7_1.txt")
ipernets = str_extract_all(mystrings, "\\[[^\\]]*\\]") %>% 
  map(str_replace_all, "\\[|\\]", "")
ips = str_extract_all(mystrings, "(^|\\])[^\\[]*") %>% 
  map(str_replace_all, "\\[|\\]", "")

# Function that check if a string contains a 4-letters ABBA
is_abba = function(x){
  x_split = strsplit(x, "", fixed = T)[[1]]
  for(i in 1:(length(x_split) - 3)){
    if(x_split[i] == x_split[i + 3] & 
       x_split[i + 1] == x_split[i + 2] & 
       x_split[i] != x_split[i + 1])
      return(TRUE)
  }
  return(FALSE)
}
# For every ip check if it is an ABBA
abba_ips = sapply(ips, function(x){
  any(sapply(x, is_abba))
})

# Now only between the ABBA ips checks if the ipernet is ABBA
filtered_ipernets = ipernets[abba_ips]
filtered_non_abba_ipernets = sapply(filtered_ipernets, function(x){
  !any(sapply(x, is_abba))
})
paste("There are", sum(filtered_non_abba_ipernets), "IPs supporting TLS")

###################### PART 2 ######################
# Function to check if it's ABA and to return the corresponding BAB
is_aba = function(x, returning = "bab"){
  x_split = strsplit(x, "", fixed = T)[[1]]
  to_return = NULL
  for(i in 1:(length(x_split) - 2)){
    if(x_split[i] == x_split[i + 2] & 
       x_split[i] != x_split[i + 1]){
      if(returning == "bab"){
        to_return = c(to_return, paste0(x_split[c(i+1, i, i+1)], collapse = ""))
      }else{
        to_return = c(to_return, paste0(x_split[c(i, i+1, i)], collapse = ""))
      }
    }
  }
  return(to_return)
}

# For every ip check if it is an ABA and save the corresponding BAB
aba_ips = lapply(1:length(ips), function(i){
  x = ips[[i]]
  babs = sapply(x, is_aba) %>% 
    unlist() %>% 
    as.character()
  if(length(babs) == 0)
    return(NULL)
  return(
    data_frame(ip_babs = babs,index = i
    )
  )
}) %>% 
  bind_rows() %>% 
  filter(ip_babs != "NULL")

# Do the same for ipernets
aba_ipernets = lapply(1:length(ipernets), function(i){
  x = ipernets[[i]]
  abas = sapply(x, is_aba, "aba") %>% 
    unlist() %>% 
    as.character()
  if(length(abas) == 0)
    return(NULL)
  return(
    data_frame(ipernets_abas = abas,index = i
    )
  )
}) %>% 
  bind_rows() %>% 
  filter(ipernets_abas != "NULL")

# Join and filter
do_join = aba_ips %>% 
  full_join(aba_ipernets) %>% 
  group_by(index) %>% 
  summarize(is_valid = any(ip_babs %in% ipernets_abas) | any(ip_babs %in% ipernets_abas)) %>% 
  ungroup()
paste("There are", sum(do_join$is_valid), "IPs supporting SSL")

