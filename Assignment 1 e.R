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