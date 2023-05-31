MontyHall <- function() {
  # Set up the game: 1 door has a car, and the other 2 have goats
  doors <- c("goat", "goat", "car")
  
  # Contestant chooses a random door
  contestant_choice <- sample(1:3, 1)
  
  # Monty opens one of the other doors with a goat
  monty_choice <- sample(setdiff(1:3, contestant_choice)[doors[setdiff(1:3, contestant_choice)] == "goat"], 1)
  
  # Contestant switches to the remaining door
  contestant_switch <- setdiff(1:3, c(contestant_choice, monty_choice))
  
  # Check if the contestant wins or loses
  if (doors[contestant_switch[1]] == "car") {
    return(1)  # Contestant wins
  } else {
    return(0)  # Contestant loses
  }
}

# Simulate the Monty Hall game show 1000 times
num_simulations <- 1000
results <- replicate(num_simulations, MontyHall())

# Calculate the probability of winning if the contestant switches
probability_switch <- mean(results)

# Print the probability of winning if the contestant switches
print(probability_switch)