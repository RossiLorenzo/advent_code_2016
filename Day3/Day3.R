###################### PART 1 ######################
# Read the input triangles
all_triangles = readLines("Input3_1.txt")
all_triangles = gsub("^ ", "", gsub(" {2,}", " ", all_triangles))
all_triangles = strsplit(all_triangles, " ", fixed = T)

# Filter out keeping only real triangles
real_triangles = Filter(function(x){ 
  all_numb = sort(as.numeric(x))
  sum(all_numb[1:2]) > all_numb[3]
}, all_triangles)
paste("There are", length(real_triangles), "real triangles")

###################### PART 2 ######################
# Reshape the data so that is by columns
all_numbers = unlist(all_triangles)
all_triangles = NULL
for(i in 1:3){
  tmp_column = all_numbers[seq(i, length(all_numbers), 3)]
  all_triangles = c(all_triangles, lapply(seq(1, length(tmp_column), 3), function(i){
    tmp_column[i : (i + 2)]
  }))
}

# Filter out keeping only real triangles
real_triangles = Filter(function(x){ 
  all_numb = sort(as.numeric(x))
  sum(all_numb[1:2]) > all_numb[3]
}, all_triangles)
paste("There are", length(real_triangles), "real triangles")

