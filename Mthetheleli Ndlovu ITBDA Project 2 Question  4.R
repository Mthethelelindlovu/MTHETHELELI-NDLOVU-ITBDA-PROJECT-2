
# --------------------------------------------------
# 4.1 Web scraping and regex analysis
# Use case: Numerical information in news headlines
# --------------------------------------------------

# Install these packages once if they are not installed
packages <- c("xml2", "rvest", "stringr")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

library(xml2)
library(rvest)
library(stringr)

# STEP 1: Select the website's RSS feed
url <- "https://feeds.bbci.co.uk/news/rss.xml"

# STEP 2: Scrape the RSS data
# RSS is an XML format containing repeated news records.
feed <- read_xml(url)

records <- xml_find_all(feed, ".//item")

# Extract at least 20 records
if (length(records) < 20) {
  stop("Fewer than 20 records were returned by the feed.")
}

records <- records[seq_len(min(length(records), 50))]

# Helper function to extract XML fields safely
get_field <- function(record, field) {
  node <- xml_find_first(record, field)
  
  if (inherits(node, "xml_missing")) {
    return(NA_character_)
  }
  
  value <- xml_text(node)
  
  if (length(value) == 0 || !nzchar(trimws(value))) {
    return(NA_character_)
  }
  
  value
}

# STEP 3: Extract unstructured text
raw_data <- data.frame(
  headline = vapply(
    records, get_field, character(1),
    field = "./title"
  ),
  summary = vapply(
    records, get_field, character(1),
    field = "./description"
  ),
  link = vapply(
    records, get_field, character(1),
    field = "./link"
  ),
  stringsAsFactors = FALSE
)

# Keep the original extracted text for comparison
raw_data$headline_raw <- raw_data$headline
raw_data$summary_raw <- raw_data$summary

# STEP 4: Clean text using regular expressions

clean_text <- function(x) {
  x <- str_replace_all(x, "<[^>]+>", " ")
  x <- str_replace_all(x, "&amp;", "&")
  x <- str_replace_all(x, "&quot;", "\"")
  x <- str_replace_all(x, "&#39;|&apos;", "'")
  x <- str_replace_all(x, "&nbsp;", " ")
  x <- str_replace_all(x, "\\s+", " ")
  x <- str_trim(x)
  
  x[x == ""] <- NA_character_
  x
}

raw_data$headline_clean <- clean_text(raw_data$headline)
raw_data$summary_clean <- clean_text(raw_data$summary)

# STEP 5: Use regex to extract the first number.
# Supports integers, decimals, percentages and comma-separated numbers.

extract_number <- function(x) {
  x[is.na(x)] <- ""
  
  pattern <- "[0-9]+(?:,[0-9]{3})*(?:\\.[0-9]+)?%?"
  
  match <- str_extract(x, pattern)
  match <- str_remove(match, "%$")
  match <- str_remove_all(match, ",")
  
  suppressWarnings(as.numeric(match))
}

# Search both the headline and summary
combined_text <- paste(
  ifelse(is.na(raw_data$headline_clean),
         "", raw_data$headline_clean),
  ifelse(is.na(raw_data$summary_clean),
         "", raw_data$summary_clean)
)

raw_data$number_mentioned <- extract_number(combined_text)

# STEP 6: Calculate the headline word count
raw_data$word_count <- str_count(
  coalesce <- ifelse(
    is.na(raw_data$headline_clean),
    "", raw_data$headline_clean
  ),
  "\\b[[:alnum:]]+\\b"
)

# STEP 7: Identify missing summaries
raw_data$missing_summary <- is.na(raw_data$summary_clean)

# STEP 8: Construct the final data frame
news_df <- data.frame(
  headline = raw_data$headline_clean,
  summary = raw_data$summary_clean,
  number_mentioned = raw_data$number_mentioned,
  word_count = raw_data$word_count,
  missing_summary = raw_data$missing_summary,
  link = raw_data$link,
  stringsAsFactors = FALSE
)

# STEP 9: Inspect and analyse the results
cat("Number of records scraped:", nrow(news_df), "\n")
cat("Missing summaries:",
    sum(news_df$missing_summary), "\n")
cat("Headlines/texts containing a number:",
    sum(!is.na(news_df$number_mentioned)), "\n")

print(head(news_df, 10))
str(news_df)
summary(news_df)

# STEP 10: Save the data for submission
write.csv(
  news_df,
  "bbc_news_scraped.csv",
  row.names = FALSE,
  na = ""
)

# Also save the original, less-cleaned data
write.csv(
  raw_data,
  "bbc_news_raw.csv",
  row.names = FALSE,
  na = ""
)

#QUESTION 4.2

# --------------------------------------------------
# 4.2 Data cleaning, denoising and imputation
# --------------------------------------------------

library(stringr)

# STEP 1: Create a copy of the scraped dataset
cleaned_df <- news_df

# STEP 2: Clean textual attributes
clean_text <- function(x) {
  x <- as.character(x)
  
  # Remove HTML tags
  x <- str_replace_all(x, "<[^>]+>", " ")
  
  # Decode common HTML entities
  x <- str_replace_all(x, "&amp;", "&")
  x <- str_replace_all(x, "&quot;", "\"")
  x <- str_replace_all(x, "&#39;|&apos;", "'")
  x <- str_replace_all(x, "&nbsp;", " ")
  
  # Remove control characters and excess whitespace
  x <- str_replace_all(x, "[[:cntrl:]]", " ")
  x <- str_replace_all(x, "\\s+", " ")
  x <- str_trim(x)
  
  # Convert empty strings to missing values
  x[x == ""] <- NA_character_
  
  x
}

cleaned_df$headline <- clean_text(cleaned_df$headline)
cleaned_df$summary <- clean_text(cleaned_df$summary)

# STEP 3: Remove incomplete headlines
# A headline is essential for identifying a news record.
cleaned_df <- cleaned_df[
  !is.na(cleaned_df$headline),
]

# STEP 4: Remove duplicate records
# Ignore capitalization and surrounding whitespace.
cleaned_df$headline_key <- str_to_lower(
  str_trim(cleaned_df$headline)
)

cleaned_df <- cleaned_df[
  !duplicated(cleaned_df$headline_key),
]

cleaned_df$headline_key <- NULL

# STEP 5: Denoise text
# Normalize repeated punctuation and remove stray symbols
cleaned_df$headline <- str_replace_all(
  cleaned_df$headline,
  "([!?.,])\\1+",
  "\\1"
)

cleaned_df$headline <- str_squish(
  cleaned_df$headline
)

cleaned_df$summary <- str_squish(
  cleaned_df$summary
)

# STEP 6: Impute missing summaries
# Use an explicit label rather than inventing article content.
cleaned_df$summary[
  is.na(cleaned_df$summary)
] <- "Summary not available"

# STEP 7: Recalculate numerical attributes
# Derive word count from the cleaned headline.
cleaned_df$word_count <- str_count(
  cleaned_df$headline,
  "\\b[[:alnum:]]+\\b"
)

# STEP 8: Impute missing numerical values
# Use the median of observed values, if any exist.
if (anyNA(cleaned_df$number_mentioned)) {
  observed <- cleaned_df$number_mentioned[
    !is.na(cleaned_df$number_mentioned)
  ]
  
  if (length(observed) > 0) {
    median_value <- median(observed)
    
    cleaned_df$number_mentioned[
      is.na(cleaned_df$number_mentioned)
    ] <- median_value
  }
}

# Add a flag to distinguish observed numbers
# from imputed values.
cleaned_df$number_was_imputed <- is.na(
  news_df$number_mentioned[
    match(cleaned_df$link, news_df$link)
  ]
)

# STEP 9: Update missing-summary indicator
# The original missingness indicator should be
# retained if available for auditing.
# Here, summary text is now complete.

# STEP 10: Check data quality
cat("Rows after cleaning:", nrow(cleaned_df), "\n")
cat("Remaining missing values by column:\n")
print(colSums(is.na(cleaned_df)))

# STEP 11: Export cleaned data
write.csv(
  cleaned_df,
  "bbc_news_cleaned.csv",
  row.names = FALSE,
  na = ""
)

cat("Cleaned dataset exported to bbc_news_cleaned.csv\n")

View(cleaned_df)