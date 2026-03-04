library(tidyverse)
library(fs)
library(readxl)
library(rmarkdown)
library(janitor)
library(ckanr)
library(httr)
library(lubridate)
library(DescTools)


# Initial setup -----------------------------------------------------------

run_log <- tribble(
  ~time, ~message
)

# Logging helper function
add_log_entry <- function(log_text) {
  
  new_row = tibble_row(
    time = now(),
    message = log_text
  )
  
  run_log <<- run_log |>
    bind_rows(
      new_row
    )
  
  cat(log_text, "\n")
}

run_start_time <- now()
add_log_entry(str_c("Start time was: ", run_start_time))

if(file_exists(".env")) {
  readRenviron(".env")
  
  ckan_url <- Sys.getenv("ckan_url")
  ckan_api_token <- Sys.getenv("ckan_api_token")
  
} else {
  stop("No .env file found, create it before running this script.")
}

ckanr_setup(
  url = ckan_url, 
  key = ckan_api_token
)

# Pasted in from retrieve.R
news_releases <- read_xlsx("input/yukon.ca-news-releases-published-2018-2021.xlsx")

# Testing: limit to a subset of news releases
# news_releases <- news_releases |>
#   slice_sample(n = 10)


# Generate the current year from the date published field
# Plus hilariously over-engineered long date formatting to match previous archive resource titles
news_releases <- news_releases |> 
  mutate(
    year = str_sub(publish_date, 0L, 4L)
  ) |> 
  mutate(
    formatted_date_en = str_replace(Format(parse_date(publish_date), fmt = "mmmm d, yyyy"), "  ", " ")
  )

# Formatted date but in French:
Sys.setlocale("LC_TIME", "fr_CA.UTF-8")

news_releases <- news_releases |> 
  mutate(
    formatted_date_fr = str_replace(Format(parse_date(publish_date), fmt = "le d mmmm yyyy", lang="fr_CA"), "  ", " "),
    formatted_date = case_when(
      language == "fr" ~ formatted_date_fr,
      .default = formatted_date_en
    )
  )


# Clean up some poorly-formatted news release numbers
news_releases <- news_releases |> 
  mutate(
    news_release_number = str_replace_all(news_release_number, "#", ""),
  ) |> 
  mutate(
    news_release_number = str_replace_all(news_release_number, "=", "-"),
  )

# Order the news releases by year, language (EN then FR), then reverse chronological date for display on dataset pages
news_releases <- news_releases |>
  arrange(
    desc(year),
    language,
    desc(publish_date)
  )


# Retrieve resource URLs from CKAN ----------------------------------------

detailed_redirects_list <- tribble(
  ~to_url, ~language
)


news_release_years <- news_releases |> 
  select(year, language) |> 
  distinct()

news_release_years <- news_release_years |> 
  mutate(
    package_id = case_when(
      language == "fr" ~ str_c("communiques-de-presse-", year),
      .default = str_c("news-releases-", year)
    ) 
  )



get_resources_by_news_release_package_id <- function(package_id, language) {
  
  package_details <- package_show(
    package_id
  )
  
  for (i in seq_along(package_details$resources)) { 
    cat(package_details$resources[[i]]$url, "\n")
    
    detailed_redirects_list <<- detailed_redirects_list |>
      bind_rows(
        tibble_row(
          to_url = package_details$resources[[i]]$url,
          language = language
        )
      )
    
  }
  
}




# Retrieve all news release packages and resources ------------------------



for (i in seq_along(news_release_years$year)) { 
  
  add_log_entry(str_c("Retrieving resources for ", news_release_years$year[i], news_release_years$language[i], " : ", news_release_years$package_id[i], "\n"))
  
  get_resources_by_news_release_package_id(news_release_years$package_id[i], news_release_years$language[i])
  
}



# Get news release numbers and join with existing spreadsheet data --------

detailed_redirects_list <- detailed_redirects_list |> 
  mutate(
    news_release_number = str_extract(to_url, "\\/download\\/([A-Z0-9\\-]+).html", group = 1)
  )

# news_releases |> View()

detailed_redirects_list <- detailed_redirects_list |> 
  left_join(news_releases, by = c("news_release_number", "language")) |> 
  select(
    page_url,
    to_url,
    news_release_number,
    language
  ) |> 
  rename(
    from_url = "page_url"
  )

detailed_redirects_list |> 
  write_csv("output_log/detailed_redirects_list.csv")



# Output nginx-formatted redirects config ---------------------------------

generate_nginx_redirect_text <- function(from_url, to_url) {
  
  from_url <- str_replace(from_url, "https://yukon.ca", "")
  
  template = "location = {from_url} {{ \n  return 302 {to_url}; \n}}\n\n"
  
  output <- str_glue(
    template,
    from_url = from_url,
    to_url = to_url
    )
  
  output
}

generate_nginx_redirect_text("https://yukon.ca/en/news/government-yukon-and-yukon-first-nations-governments-meet-carmacks-yukon-forum", "https://open.yukon.ca/information/b1cbb21a-1e7d-4bd5-b4ee-01b04901749c/resource/78460964-87b6-444d-8f1a-f57cfa66a932/download/19-115.html")

detailed_redirects_list_with_nginx_text <- detailed_redirects_list |> 
  mutate(
    nginx_redirect_text = generate_nginx_redirect_text(from_url, to_url)
  )

detailed_redirects_list_text <- detailed_redirects_list_with_nginx_text |> 
  pull(nginx_redirect_text)

write_lines(
  detailed_redirects_list_text,
  file = "output_log/detailed_redirects_nginx.txt"
)
