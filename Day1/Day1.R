###################### PART 1 ######################
# Read my sequence
my_sequence = readChar("Input1_1.txt", file.size("Input1_1.txt"))
my_sequence = strsplit(my_sequence, ",", fixed = T)[[1]]

# Create a function that follows one instruction
one_interaction = function(position, facing_direction, next_step){
  turning = gsub("[0-9]| ", "", next_step)
  steps = as.numeric(gsub("L|R| ", "", next_step))
  # Case of facing north
  if(facing_direction == "N"){
    if(turning == "L")
      return(list(position = position + c(-steps, 0), facing_direction = "E"))
    return(list(position = position + c(steps, 0), facing_direction = "W"))
  }
  # Case of facing south
  if(facing_direction == "S"){
    if(turning == "L")
      return(list(position = position + c(steps, 0), facing_direction = "W"))
    return(list(position = position + c(-steps, 0), facing_direction = "E"))
  }
  # Case of facing east
  if(facing_direction == "E"){
    if(turning == "L")
      return(list(position = position + c(0, -steps), facing_direction = "S"))
    return(list(position = position + c(0, steps), facing_direction = "N"))
  }
  # Case of facing west
  if(facing_direction == "W"){
    if(turning == "L")
      return(list(position = position + c(0, steps), facing_direction = "N"))
    return(list(position = position + c(0, -steps), facing_direction = "S"))
  }
}

# Loop over the instructions to reach the final destination
my_direction = "N"
position = c(0, 0)
for(next_interaction in my_sequence){
  tmp_res = one_interaction(position, my_direction, next_interaction)
  my_direction = tmp_res$facing_direction
  position = tmp_res$position
}

# To get there the shortest way is the sum of the coordinates
paste("Easter Bunny HQ is", sum(abs(position)), "blocks away")

###################### PART 2 ######################
my_direction = "N"
position = c(0, 0)
visited = list()
first_visited = NULL; k = FALSE
for(next_interaction in my_sequence){
  tmp_res = one_interaction(position, my_direction, next_interaction)
  my_direction = tmp_res$facing_direction
  # Get all points in the middle
  x_seq = seq(position[1], tmp_res$position[1]) 
  y_seq = seq(position[2], tmp_res$position[2])
  # One of the two will be long 1 the other will >= 1
  if(length(x_seq) == 1){
    for(i in y_seq[-1]){
      visited = c(visited, list(c(x_seq, i)))
      if(length(visited) != length(unique(visited)) & !k){
        k = TRUE
        first_visited = c(x_seq, i)
      }
    }
  }else{
    for(i in x_seq[-1]){
      visited = c(visited, list(c(i, y_seq)))
      if(length(visited) != length(unique(visited)) & !k){
        k = TRUE
        first_visited = c(i, y_seq)
      }
    }
  }
  position = tmp_res$position
}

# To get there the shortest way is the sum of the coordinates
paste("Actually, Easter Bunny HQ is", sum(abs(first_visited)), "blocks away")

