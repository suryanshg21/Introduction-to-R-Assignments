#Part 1 of Question C
tennis <- function(p) {
  x <- 0  # Initialize the number of sets played
  
  # Simulate the tennis match
  while (x < 5) {
    set_winner <- sample(c("A", "B"), 1, prob = c(p, 1 - p))  # Determine the winner of the set
    
    if (set_winner == "A") {
      return(x)  # Return the number of sets played if player A wins a set
    } else {
      x <- x + 1  # Increment the number of sets played if player A loses a set
    }
  }
  
  return(x)  # Return the number of sets played if all 5 sets are played
}
#Part 2 of Question C
matches <- numeric(1000)  # Create an empty vector to store the number of matches

for (i in 1:1000) {
  matches[i] <- tennis(0.70)  # Simulate a tennis match and store the number of matches played
}

ans <- mean(matches)  # Calculate the average number of matches

# Print the average number of matches
print(ans)