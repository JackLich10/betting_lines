### Scrape DraftKings for daily betting lines
# Author: Jack Lichtenstein, modified from Lee Sharpe

# Load libraries
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(httr)))
suppressMessages(suppressWarnings(library(janitor)))
suppressMessages(suppressWarnings(library(jsonlite)))
suppressMessages(suppressWarnings(library(purrr)))
suppressMessages(suppressWarnings(library(readr)))
suppressMessages(suppressWarnings(library(stringi)))
suppressMessages(suppressWarnings(library(stringr)))
suppressMessages(suppressWarnings(library(tidyr)))

# Function to scrape DraftKings for daily betting lines
get_dk_lines <- function(type = c("NFL", "NBA")) {

  # Formulate url
  base_url <- "https://sportsbook.draftkings.com/leagues/"

  append <- switch (type,
    "NFL" = "football/3",
    "NBA" = "basketball/88670846"
  )

  url <- paste0(base_url, append)

  cat(paste0("Scraping ", type," DraftKings lines...\n"))

  # Make request
  response <- url %>%
    httr::RETRY("GET", url = ., times = 10) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    stringi::stri_trans_general("latin-ascii") %>%
    stringi::stri_split_lines1()

  # extract betting json
  bets_str <- response[nchar(response) == max(nchar(response))]
  start_char <- stringr::str_locate(bets_str, "\\{")
  dk_bets <- substr(bets_str, start_char, nchar(bets_str)-1) %>%
    jsonlite::fromJSON(flatten = TRUE)

  # collecting the event name for the offers
  level_name <- dk_bets[["offers"]] %>% names()

  cat("Cleaning data...\n")

  # unnest and clean
  bets <- dplyr::tibble(data = dk_bets[["offers"]][[level_name]]) %>%
    tidyr::unnest_wider(1) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ dplyr::na_if(., ""))) %>%
    janitor::remove_empty(which = "cols") %>%
    dplyr::rename(bet_type = label) %>%
    dplyr::select(-dplyr::any_of(c(
      "providerOfferId", "providerId", "source", "main"
    ))) %>%
    tidyr::unnest(outcomes) %>%
    janitor::clean_names() %>%
    dplyr::mutate(date = Sys.Date(),
                  dplyr::across(dplyr::starts_with("odds_american"), readr::parse_number),
                  probability = ifelse(odds_american > 0,
                                       (100 / (odds_american + 100)),
                                       (-odds_american / (-odds_american + 100)))) %>%
    # select what is needed
    dplyr::select(dplyr::any_of(c(
      "date", "bet_type",
      "suspended" = "is_suspended", "open" = "is_open",
      "label", "participant", "probability",
      "odds_american", "odds_decimal", "odds_fractional",
      "line", "event_id" = "provider_event_id",
      "subcategory_id" = "offer_subcategory_id",
      "bet_type_id" = "bet_offer_type_id",
      "criterion_id" = "provider_criterion_id",
      "offer_id" = "provider_offer_id", "hidden"
    )))

  if ("hidden" %in% colnames(bets)) {
    bets <- bets %>%
      dplyr::filter(is.na(hidden)) %>%
      dplyr::select(-hidden)
  }

  cat(paste0("Done with ", type, ".\n"))

  return(bets)
}

nfl_bets <- get_dk_lines(type = "NFL")
nba_bets <- get_dk_lines(type = "NBA")


# Function to save DraftKings lines
save_lines <- function(type = c("NFL", "NBA")) {

  if (type == "NFL") {
    bets <- nfl_bets
  } else if (type == "NBA") {
    bets <- nba_bets
  }

  type <- tolower(type)

  # Create directory
  if (!dir.exists(here::here(paste0("data/", type)))) {
    dir.create(here::here(paste0("data/", type)))
  }

  # Path to write to
  path <- paste0("data/", type, "/draftkings_", Sys.Date(), "_lines.csv")
  readr::write_csv(bets, path)
}

purrr::walk(c("NFL", "NBA"), save_lines)

