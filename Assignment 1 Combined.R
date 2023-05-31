#Q: a,b
# Required libraries
library(rvest)
library(dplyr)

# URL of the webpage
url <- "https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/"

# Read the webpage
page <- read_html(url)

# Extract the table containing the information
table <- html_table(html_nodes(page, "table")[[1]])

# Extract the required columns from the table
df <- table[, c("Company Name (M.Cap)", "CMP", "Price Change", "Market Cap (Cr)", "52 Week High",
                "52 Week Low", "ROE", "P/E", "P/BV", "EV/EBITDA", "5YSales Gr(%)", "5YProfit Gr(%)")]

# Rename the columns
colnames(df) <- c("Company Name (M.Cap)", "CMP", "Price Change", "Market Cap (Cr)", "52 Week High",
                  "52 Week Low", "ROE", "P/E", "P/BV", "EV/EBITDA", "5YSales Gr(%)", "5YProfit Gr(%)")

# Remove unnecessary characters from column names
colnames(df) <- gsub("\\s+|\\(|\\)", "", colnames(df))

# Print the extracted dataset
df
View(df)
# Required libraries
library(rvest)
library(dplyr)

# Define the URLs for the chosen companies
urls <- c(
  "https://www.moneyworks4me.com/best-index/nse-stocks/infosys/INFY/52",
  "https://www.moneyworks4me.com/best-index/nse-stocks/reliance-industries/RELIANCE/1",
  "https://www.moneyworks4me.com/best-index/nse-stocks/tata-consultancy-services/TCS/7",
  "https://www.moneyworks4me.com/best-index/nse-stocks/hindustan-unilever/HINDUNILVR/24",
  "https://www.moneyworks4me.com/best-index/nse-stocks/icici-bank/ICICIBANK/30"
)

# Create an empty data frame to store the extracted information
company_data <- data.frame(matrix(ncol = 16, nrow = 0))
colnames(company_data) <- c(
  "Company", "Date", "Sales", "YoY Gr. Rt.", "Adj EPS", "YoY Gr. Rt.", "BVPS",
  "Adj Net Profit", "Cash Flow from Ops.", "Debt/CF from Ops", "Return on Equity",
  "Op. Profit Mgn", "Net Profit Mgn", "Debt to Equity",
  "Working Cap Days", "Cash Conv. Cycle"
)

# Loop through each company and extract the data
for (url in urls) {
  webpage <- read_html(url)
  
  # Extract the company name
  company_name <- webpage %>%
    html_node(".topDivRight h1") %>%
    html_text()
  
  # Extract the table data
  table_data <- webpage %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  
  # Extract the required columns (assuming they are in a specific order)
  data <- data.frame(
    Company = company_name,
    Date = table_data[[1]][, 1],
    Sales = table_data[[1]][, 2],
    `YoY Gr. Rt.` = table_data[[1]][, 3],
    `Adj EPS` = table_data[[1]][, 4],
    `YoY Gr. Rt..1` = table_data[[1]][, 5],
    BVPS = table_data[[1]][, 6],
    `Adj Net Profit` = table_data[[1]][, 7],
    `Cash Flow from Ops.` = table_data[[1]][, 8],
    `Debt/CF from Ops` = table_data[[1]][, 9],
    `Return on Equity` = table_data[[1]][, 10],
    `Op. Profit Mgn` = table_data[[1]][, 11],
    `Net Profit Mgn` = table_data[[1]][, 12],
    `Debt to Equity` = table_data[[1]][, 13],
    `Working Cap Days` = table_data[[1]][, 14],
    `Cash Conv. Cycle` = table_data[[1]][, 15]
  )
  
  # Append the extracted data to the company_data data frame
  company_data <- rbind(company_data, data)
}

# Print the extracted data frame
print(company_data)
#Q:c
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
#q:d
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
#q:e
library(rvest)
library(dplyr)

# URL of the website
url <- "https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/"

# Read the HTML content of the webpage
page <- read_html(url)

# Scrape the movie data
movie_data <- page %>%
  html_nodes(".countdown-index") %>%
  html_text() %>%
  as.integer() %>%
  setNames("Ranking") %>%
  tibble() %>%
  mutate(
    Movie = page %>% html_nodes(".article_movie_title") %>% html_text(),
    Tomato_Score = page %>% html_nodes(".tMeterScore") %>% html_text() %>% gsub("%", "", .) %>% as.integer(),
    Year = page %>% html_nodes(".start-year") %>% html_text(trim = TRUE) %>% as.integer()
  ) %>%
  mutate(Year = ifelse(is.na(Year) | Year == 0, NA, Year))

# Print the scraped data
print(movie_data)