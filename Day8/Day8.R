library(dplyr)
library(ggplot2)
library(reshape2)

###################### PART 1 ######################
# Read the input and parse it
instructions = readLines("Input8_1.txt")
instructions_df = data_frame(
  function_apply = if_else(grepl("rect", instructions), "rect", 
                           if_else(grepl("row", instructions), "horizontal_shift", "vertical_shift")),
  x = as.numeric(if_else(grepl("rect", instructions), gsub("x.*", "", gsub(".* ", "", instructions)), 
              if_else(grepl("row", instructions), gsub(" .*", "", gsub(".*y=", "", instructions)), gsub(" .*", "", gsub(".*x=", "", instructions))))),
  y = as.numeric(if_else(grepl("rect", instructions), gsub(".*x", "", instructions), gsub(".*by ", "", instructions)))
)

# Function to turn on rectangles
rect = function(screen, x, y){
  screen[1:y, 1:x] = 1
  return(screen)
}

# Function to shift vertically
vertical_shift = function(screen, x, y){
  screen[ , x + 1] = c(screen[(nrow(screen) - y + 1) : nrow(screen) , x + 1], screen[1:(nrow(screen) - y) , x + 1])
  return(screen)
}

# Function to shift horiontally
horizontal_shift = function(screen, x, y){
  screen[x + 1 , ] = c(screen[x + 1, (ncol(screen) - y + 1) : ncol(screen)], screen[x + 1, 1:(ncol(screen) - y)])
  return(screen)
}

# Run all the tasks
screen = matrix(0, ncol = 50, nrow = 6)
for(i in 1:nrow(instructions_df)){
  tmp_inst = paste0("screen = ", instructions_df$function_apply[i], "(screen, ", instructions_df$x[i], ", ", instructions_df$y[i], ")")
  eval(parse(text = tmp_inst)) 
}
paste("There are", sum(screen), "pixels lit")

###################### PART 2 ######################
screeen = data.frame(screen)
screen_melt = reshape2::melt(screen)
ggplot(screen_melt, aes(x = Var2, y =  -Var1, fill = factor(value))) + 
  geom_tile()