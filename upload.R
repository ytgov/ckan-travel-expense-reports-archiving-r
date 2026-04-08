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
  
  year_cutoff <- Sys.getenv("year_cutoff")
  
} else {
  stop("No .env file found, create it before running this script.")
}

ckanr_setup(
  url = ckan_url, 
  key = ckan_api_token
)

# Pasted in from retrieve.R
# news_releases <- read_xlsx("input/yukon.ca-news-releases-published-2018-2021.xlsx")
news_releases <- read_csv("output_log/download_log.csv")

news_releases <- news_releases |> 
  filter(year < year_cutoff)

# Testing: limit to a subset of news releases
news_releases <- news_releases |>
  slice_sample(n = 10)


# # Generate the current year from the date published field
# # Plus hilariously over-engineered long date formatting to match previous archive resource titles
# news_releases <- news_releases |> 
#   mutate(
#     year = str_sub(publish_date, 0L, 4L)
#   ) |> 
#   mutate(
#     formatted_date_en = str_replace(Format(parse_date(publish_date), fmt = "mmmm d, yyyy"), "  ", " ")
#   )
# 
# # Formatted date but in French:
# Sys.setlocale("LC_TIME", "fr_CA.UTF-8")
# 
# news_releases <- news_releases |> 
#   mutate(
#     formatted_date_fr = str_replace(Format(parse_date(publish_date), fmt = "le d mmmm yyyy", lang="fr_CA"), "  ", " "),
#     formatted_date = case_when(
#       language == "fr" ~ formatted_date_fr,
#       .default = formatted_date_en
#     )
#   )
# 
# 
# # Clean up some poorly-formatted news release numbers
# news_releases <- news_releases |> 
#   mutate(
#     news_release_number = str_replace_all(news_release_number, "#", ""),
#   ) |> 
#   mutate(
#     news_release_number = str_replace_all(news_release_number, "=", "-"),
#   )
# 
# # Order the news releases by year, language (EN then FR), then reverse chronological date for display on dataset pages
# news_releases <- news_releases |>
#   arrange(
#     desc(year),
#     language,
#     desc(publish_date)
#   )

# Template text -----------------------------------------------------------

template_en_title <- "Minister travel expense reports {year}"
template_fr_title <- "Frais de déplacement des ministres {year}"

template_en_description <- "All {year} Minister travel expense reports from the Government of Yukon.\n\n[View current Minister travel expense reports](https://yukon.ca/en/your-government/performance-and-finance/find-minister-travel-expense-reports)."
template_fr_description <- "Tous les frais de déplacement des ministres de {year} du gouvernement du Yukon.\n\n[Consulter les derniers frais de déplacement des ministres](https://yukon.ca/fr/votre-gouvernement/bilan-et-finances/frais-de-deplacement-des-ministres)."


# Helper functions --------------------------------------------------------

# Thanks, Google
slugify <- function(x) {
  x %>%
    str_to_lower() %>%                        # Convert to lowercase
    iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') %>% # Convert accented characters to ASCII before removing non-ASCII below
    str_replace_all("[^a-z0-9\\s-]", "") %>%  # Remove non-alphanumeric (except spaces/hyphens)
    str_squish() %>%                          # Remove extra whitespace
    str_replace_all("\\s+", "-")              # Replace spaces with hyphens
}

get_name_from_title <- function(title) {
  slugify(title)
}

get_title_by_year <- function(year, language = "en") {
  
  if(language == "en") {
    str_glue(
      template_en_title,
      year = {{year}}
      )
  }
  else {
    str_glue(
      template_fr_title,
      year = {{year}}
    )
  }
  
}

get_description_by_year <- function(year, language = "en") {
  
  if(language == "en") {
    str_glue(
      template_en_description,
      year = {{year}}
    )
  }
  else {
    str_glue(
      template_fr_description,
      year = {{year}}
    )
  }
  
}

create_news_release_package_if_needed <- function(news_year, news_language = "en") {
  
  package_name <- get_name_from_title(get_title_by_year(news_year, news_language))
  
  if(news_language == "en") {
    language_name = "english"
  }
  else {
    language_name = "french"
  }
  
  # Check if package exists
  result = tryCatch({
    
    package_show(
      id = package_name
      )

    
  }, error = function(e) {
    add_log_entry(str_c("Creating package ", package_name))
    
    package_create(
      name = get_name_from_title(get_title_by_year(news_year, news_language)),
      title = get_title_by_year(news_year, news_language),
      notes = get_description_by_year(news_year, news_language),
      type = "information",
      license_id = "OGL-Yukon-2.0",
      owner_org = "576049b0-490d-45d2-b236-31aef7a16ffe",
      
      extras = list(
        internal_contact_email = "ecoinfo@yukon.ca",
        internal_contact_name = "ECO Info",
        publication_required_under_atipp_act = "Yes",
        publication_type_under_atipp_act = "organizational_responsibilities_and_functions",
        language = language_name
      )
    )
    
  })

}



# Add resources year-by-year ----------------------------------------------

news_release_years <- news_releases |> 
  select(year) |> 
  distinct() |> 
  arrange(year) |> 
  pull(year)


add_resources_by_year <- function(news_year, news_language = "en") {
  
  current_year_news_releases <- news_releases |> 
    filter(year == news_year) |> 
    filter(language == news_language)
  
  add_log_entry(str_c("For ", news_year, " in ", news_language, " there are: ", count(current_year_news_releases)$n, " news releases."))
  
  
  # Retrieve the current year's dataset (and create it first if needed)
  parent_dataset <- create_news_release_package_if_needed(news_year, news_language)
  
  # Add resources for each row in current_year_news_releases
  for (i in seq_along(current_year_news_releases$url)) { 
    
    # html_resource_path <- path("output", current_year_news_releases$language[i], current_year_news_releases$year[i], str_c(current_year_news_releases$news_release_number[i], ".html"))
    html_resource_path <- current_year_news_releases$filepath[i]
    
    add_log_entry(str_c("Uploading resource for ", current_year_news_releases$title[i], " from path ", html_resource_path))
    
    parent_dataset |> 
      resource_create(
        name = str_c(current_year_news_releases$title[i]),
        # description = current_year_news_releases$meta_description[i],
        description = "",
        upload = html_resource_path
      )
    
    Sys.sleep(0.4)
    
  }
  
  
}


# For each of the years in the source spreadsheet, add all of that year's resources:

# English
for (i in seq_along(news_release_years)) { 
  
  add_resources_by_year(news_release_years[i], "en")
  
}

# French
for (i in seq_along(news_release_years)) { 
  
  add_resources_by_year(news_release_years[i], "fr")
  
}


run_end_time <- now()
run_elapsed_hours <- round(time_length(interval(run_start_time, run_end_time), "hours"), digits = 2)

add_log_entry(str_c("End time was: ", run_end_time))
add_log_entry(str_c("Elapsed time was: ", run_elapsed_hours, " hours"))

# Write the log files to CSV:
run_log |> 
  write_csv("output_log/upload_log.csv")

