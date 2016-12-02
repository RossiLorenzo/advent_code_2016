###################### PART 1 ######################
# Read the instructions
instructions = readLines("Input2_1.txt")
instructions = strsplit(instructions, "", fixed = T)

# Map number to pad position and vice-versa
number_to_pad = function(number){
  c(
    floor((number - 1) / 3), 
    ((number - 1) / 3 - floor((number - 1) / 3)) * 3
  )
}
pad_to_number = function(position){
  position[1] * 3 + position[2] + 1
}

# Do one move
one_move = function(start, direction){
  # map the start button to a position (1 is 0,0 and 5 is 1,1)
  position = number_to_pad(start)
  # move in the selected direction
  if(direction == "U")
    new_position = c(max(0, position[1] - 1), position[2])
  if(direction == "D")
    new_position = c(min(3 - 1, position[1] + 1), position[2])
  if(direction == "L")
    new_position = c(position[1], max(0, position[2] - 1))
  if(direction == "R")
    new_position = c(position[1], min(3 - 1, position[2] + 1))
  # translate to number and return
  return(pad_to_number(new_position))
}

# Run all the instructions
code = ""
my_pos = 5
for(tmp_inst in seq_along(instructions)){
  for(i in instructions[[tmp_inst]])
    my_pos = one_move(my_pos, i)
  code = paste0(code, my_pos)
}
paste0("The Code is: ", code)

###################### PART 2 ######################
# Do one move - ugly manual
one_move_2 = function(start, direction){
  # move in the selected direction
  if(direction == "U"){
    if(start %in% c("1", "2", "4", "5", "9"))
      return(start)
    if(start == "3") return("1")
    if(start == "6") return("2")
    if(start == "7") return("3")
    if(start == "8") return("4")
    if(start == "A") return("6")
    if(start == "B") return("7")
    if(start == "C") return("8")
    if(start == "D") return("B")
  }
  if(direction == "D"){
    if(start %in% c("5", "9", "A", "C", "D"))
      return(start)
    if(start == "1") return("3")
    if(start == "2") return("6")
    if(start == "3") return("7")
    if(start == "4") return("8")
    if(start == "6") return("A")
    if(start == "7") return("B")
    if(start == "8") return("C")
    if(start == "B") return("D")
  }
  if(direction == "L"){
    if(start %in% c("1", "2", "5", "A", "D"))
      return(start)
    if(start == "3") return("2")
    if(start == "4") return("3")
    if(start == "6") return("5")
    if(start == "7") return("6")
    if(start == "8") return("7")
    if(start == "9") return("8")
    if(start == "B") return("A")
    if(start == "C") return("B")
  }
  if(direction == "R"){
    if(start %in% c("1", "4", "9", "C", "D"))
      return(start)
    if(start == "2") return("3")
    if(start == "3") return("4")
    if(start == "5") return("6")
    if(start == "6") return("7")
    if(start == "7") return("8")
    if(start == "8") return("9")
    if(start == "A") return("B")
    if(start == "B") return("C")
  }
}

# Run all the instructions
code = ""
my_pos = "5"
for(tmp_inst in seq_along(instructions)){
  for(i in instructions[[tmp_inst]])
    my_pos = one_move_2(my_pos, i)
  code = paste0(code, my_pos)
}
paste0("The Code is: ", code)