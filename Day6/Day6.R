###################### PART 1 ######################
# Read the data
readcodes = readLines("Input6_1.txt")
readcodes_split = strsplit(readcodes, "", fixed = TRUE)

# Find code
final_code = ""
for(i in 1:nchar(readcodes[1])){
  tmp = unlist(Map(function(x){ x[i]}, readcodes_split))
  final_code = paste0(final_code, names(tail(sort(table(tmp)), 1)))
}
paste("The final code is:", final_code)
###################### PART 2 ######################
# Same stuff but less likely
final_code = ""
for(i in 1:nchar(readcodes[1])){
  tmp = unlist(Map(function(x){ x[i]}, readcodes_split))
  final_code = paste0(final_code, names(head(sort(table(tmp)), 1)))
}
paste("The final code is:", final_code)