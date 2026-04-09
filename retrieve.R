library(tidyverse)
library(fs)
library(readxl)
library(rmarkdown)
library(janitor)
library(rvest)


# Helper functions --------------------------------------------------------

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

download_log <- tribble(
  ~time, ~url, ~filesize, ~filepath, ~language, ~year, ~date, ~title
)

add_download_log_entry <- function(url, filesize, filepath, language, year, date, title) {
  
  new_row = tibble_row(
    time = now(),
    url = url,
    filesize = filesize,
    filepath = filepath,
    language = language,
    year = year,
    date = date,
    title = title
  )
  
  download_log <<- download_log |>
    bind_rows(
      new_row
    )
}

# Start retrieval process -------------------------------------------------

run_start_time <- now()
add_log_entry(str_c("Start time was: ", run_start_time))


# Get the index page and retrieve URLs
get_expense_report_urls <- function(language = "en") {
  
  if(language == "fr") {
    index_url <- "https://yukon.ca/fr/votre-gouvernement/bilan-et-finances/frais-de-deplacement-des-ministres"
  }
  else {
    index_url <- "https://yukon.ca/en/your-government/performance-and-finance/find-minister-travel-expense-reports"
  }
  
  
  index_html <- read_html(index_url) |> 
    html_element("article")
  
  expense_urls <- index_html |> 
    html_elements("ul li a") |> 
    html_attr(name = "href")
  
  expense_urls
  
}

html_template_start <- read_file("templates/start.html")
html_template_end <- read_file("templates/end.html")

meta_archived_date <- str_sub(now(), 0L, 10L)

retrieve_individual_expense_report <- function(page_url, language) {
  
  if(language == "fr") {
    author <- "Gouvernement du Yukon"
    archive_alert_message_text <- 'Ce page a été archivé. <a href="https://yukon.ca/fr/votre-gouvernement/bilan-et-finances/frais-de-deplacement-des-ministres">Consulter les derniers frais de déplacement des ministres</a>.'
  }
  else {
    author <- "Government of Yukon"
    archive_alert_message_text <- 'This page has been archived. <a href="https://yukon.ca/en/your-government/performance-and-finance/find-minister-travel-expense-reports">View current Minister travel expense reports</a>.'
  }
  
  add_log_entry(str_c("Downloading ", page_url))
  
  expense_report_html <- read_html(page_url)
  
  expense_report_date <- expense_report_html |> 
    html_element("meta[name=dcterms\\.date]") |> 
    html_attr(name = "content")
  
  expense_report_title <- expense_report_html |> 
    html_element("h1") |> 
    html_text() |> 
    str_trim()
  
  expense_report_main <- expense_report_html |> 
    html_element("main")
  
  # Determine year from the dcterms.date metadata (hopefully corresponds to creation date, not modification date?)
  year <- str_sub(expense_report_date, 0L, 4L)
  
  # Check if that file already exists:
  filename <- path_file(page_url)
  
  html_output_path <- path("output", language, year, str_c(filename, ".html"))
  
  # if(file_exists(html_output_path)) {
  #   add_log_entry(str_c("- Path ", html_output_path, " already exists."))
  #   return()
  # }
  
  # Be gentle to the server between requests!
  Sys.sleep(0.2)
  
  
  # Remove unnecessarily <main> child elements:
  
  remove_node_feedback_form <- expense_report_main |> 
    html_node("div#block-page-feedback-webform")
  
  remove_node_date_modified <- expense_report_main |> 
    html_node("div.node-date-modified")
  
  remove_node_breadcrumbs <- expense_report_main |> 
    html_node("div.region-breadcrumb")
  
  remove_node_related_tasks <- expense_report_main |> 
    html_node("div.block-views-blockrelated-tasks-block-1")

  remove_node_historical_navbar <- expense_report_main |> 
    html_node("aside.layout-sidebar-first")
  
  
  # Thanks to
  # https://stackoverflow.com/a/50769954/756641
  xml2::xml_remove(remove_node_feedback_form)
  xml2::xml_remove(remove_node_date_modified)
  xml2::xml_remove(remove_node_breadcrumbs)
  xml2::xml_remove(remove_node_related_tasks)
  xml2::xml_remove(remove_node_historical_navbar)
  
  
  # Create the output directory if it doesn't already exists:
  
  dir_create(path("output", language, year))
  
  formatted_html_template_start <- html_template_start |> 
    str_glue(
      title = expense_report_title,
      language = language,
      # TODO: update this:
      description = expense_report_title,
      author = author,
      date = expense_report_date,
      archived_date = meta_archived_date,
      # news_release_number = news_release_number,
      page_url = page_url,
      archive_alert_message_text = archive_alert_message_text
    )
    
  # Update image paths (still loading from Yukon.ca for now)
  # Update Yukon.ca links too
  formatted_expense_report_html <- as.character(expense_report_main) |> 
    str_replace_all('src="/sites/default/', 'src="https://yukon.ca/sites/default/') |> 
    str_replace_all('href="/en/', 'href="https://yukon.ca/en/') |> 
    str_replace_all('href="/fr/', 'href="https://yukon.ca/fr/')
  
  # Log how much text there was as a retrieval error check
  # Plus add a bunch of metadata used during the upload process
  add_download_log_entry(page_url, str_length(formatted_expense_report_html), html_output_path, language, year, expense_report_date, expense_report_title)
  
  news_release_output <- str_c(
    formatted_html_template_start,
    formatted_expense_report_html,
    html_template_end
  )
  
  write_file(
    news_release_output,
    file = html_output_path
    )
  
}


expense_urls_en <- get_expense_report_urls(language = "en")
expense_urls_fr <- get_expense_report_urls(language = "fr")

# Manually add the 2016 entries that are on a different page
# https://yukon.ca/en/your-government/performance-and-finance/find-minister-travel-expense-reports/2016-minister-travel
expense_urls_en <- append(expense_urls_en, c(
  "https://yukon.ca/en/your-government/performance-and-finance/find-minister-travel-expense-reports/travel-expense-10",
  "https://yukon.ca/en/your-government/performance-and-finance/find-minister-travel-expense-reports/travel-expense-18"
))

# https://yukon.ca/fr/votre-gouvernement/bilan-et-finances/frais-de-deplacement-des-ministres/frais-de-deplacement-des
expense_urls_fr <- append(expense_urls_fr, c(
  "https://yukon.ca/fr/votre-gouvernement/bilan-et-finances/frais-de-deplacement-des-ministres/frais-de-deplacement-pour-5",
  "https://yukon.ca/fr/votre-gouvernement/bilan-et-finances/frais-de-deplacement-des-ministres/frais-de-deplacement-pour-13"
))


for (i in seq_along(expense_urls_en)) { 
  
  retrieve_individual_expense_report(expense_urls_en[i], "en")

}

for (i in seq_along(expense_urls_fr)) { 
  
  retrieve_individual_expense_report(expense_urls_fr[i], "fr")
  
}


run_end_time <- now()
run_elapsed_hours <- round(time_length(interval(run_start_time, run_end_time), "hours"), digits = 2)

add_log_entry(str_c("End time was: ", run_end_time))
add_log_entry(str_c("Elapsed time was: ", run_elapsed_hours, " hours"))

# Write the log files to CSV:
run_log |> 
  write_csv("output_log/run_log.csv")

if(count(download_log) > 0) {
  download_log |> 
    write_csv("output_log/download_log.csv")
}
