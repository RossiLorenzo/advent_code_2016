library(openssl)

###################### PART 1 ######################
mystring = "ugkcyxxp"
index = seq(0, 999999, 1)
found = 0
password = ""
while(found < 8){
  tmp_hash = md5(paste0(mystring, index))
  index = seq((max(index) + 1), (max(index) + 1) + 999999, 1)
  search_zeros = grepl("^00000", tmp_hash)
  if(sum(search_zeros) != 0){
    zeros = tmp_hash[search_zeros]
    password = paste0(password, paste0(unlist(Map(function(x){ x[6] }, strsplit(zeros, "", fixed = T))), collapse = ""))
    found = found + sum(search_zeros)
    print(paste("Found:", found))
  }
}
print(paste("The password is:", password))

###################### PART 2 ######################
mystring = "ugkcyxxp"
index = seq(0, 999999, 1)
found = 0
password = c(rep("_", 8))
while("_" %in% password){
  tmp_hash = md5(paste0(mystring, index))
  index = seq((max(index) + 1), (max(index) + 1) + 999999, 1)
  search_zeros = grepl("^00000", tmp_hash)
  if(sum(search_zeros) != 0){
    zeros = tmp_hash[search_zeros]
    print(paste0("Found: ", paste0(zeros, collapse = ", ")))
    zeros_split = strsplit(zeros, "", fixed = T) 
    for(i in seq_along(zeros_split)){
      x = zeros_split[[i]]
      position = as.numeric(x[6])
      value = x[7]
      if(!is.na(position)){
        if(position >= 0 & position <= 7){
          if(password[position + 1] == "_"){
            password[position + 1] = value
          }
        }
      }
    }
    print(paste("Tmp Password is:", paste0(password, collapse = "")))
  }
}
print(paste("The password is:", paste0(password, collapse = "")))

