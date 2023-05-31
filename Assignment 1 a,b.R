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