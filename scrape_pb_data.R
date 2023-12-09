# This script scrapes PB data from the Scot Gov website. This is a learning exercise,
# after chatting to my colleague Martin about his bake off wikipedia web scraping.
# I referred to https://r4ds.hadley.nz/webscraping for this, and used the amazing https://selectorgadget.com/ to identify the CSS selectors.

# Aim: To build a dataset of public bodies from the Scottish Government's online list.

library(tidyverse)
library(rvest)

# Location of the public bodies list
html <- read_html("https://www.gov.scot/publications/national-public-bodies-directory/pages/executive-non-departmental-public-bodies/")

# =========================================================
# Scraping websites
# =========================================================
website_last_updated <- html_element(html, "#sg-meta__last-updated-date") |>
  html_text2()

# Grabs the main columns of text which holds PB name, address, CE, and other information
public_bodies <- html_elements(html, ".js-content-wrapper p")

# We parse this information into a list - not all public bodies are perfectly captured however,
# with some being split over two list elements. But this essentially holds all the information we # # # need for one public body type. Just need to figure out how to extract.
pb_details <- public_bodies |>
  html_text() |>
  str_split("\n")

# Focusing just on the public body name is easier, however:
pb_names <- public_bodies |>
  html_element("strong") |>
  html_text2()

# So let's just get the names of all bodies first. Define a tibble which holds the URLs
root_to_scrape <- "https://www.gov.scot/publications/national-public-bodies-directory/pages/"


# Define tibble which holds the specific subpage the different type of bodies are listed, and the
# html tag associated with the relevant information.
urls_to_scrape <- tribble(
  ~path_ends, ~type, ~html_elements,
  "advisory-non-departmental-public-bodies/", "Advisory NDPB", ".js-content-wrapper p",
  "executive-non-departmental-public-bodies/", "Executive NDPB", ".js-content-wrapper p",
  "tribunals/", "Tribunal", ".js-content-wrapper p",
  "public-corporations/", "Public Corporation", ".js-content-wrapper p",
  "health-bodies/", "Health Bodies", ".js-content-wrapper p",
  "non-ministerial-offices/", "NMO", ".js-content-wrapper p",
  "commissioners-and-ombudsmen/", "Commissioners and Ombudsmen", ".js-content-wrapper p",
  "other-significant-national-bodies/", "Other Significant Bodies", ".js-content-wrapper p"
)

urls_to_scrape <-
  urls_to_scrape |>
  mutate(url = paste0(root_to_scrape, path_ends)) |>
  select(-path_ends) |>
  as.list()

all_public_bodies <- pmap_df(
  urls_to_scrape,
  .f = function(url, html_elements, type) {
    returndf <- tibble(
      pb =
        read_html(url) |>
        html_elements(html_elements) |>
        html_element("strong") |>
        html_text2()
    )

    returndf$type <- type

    return(returndf)
  }
) |>
  filter(!is.na(pb))


# =========================================================
# Count up totals
# =========================================================

all_public_bodies

summary_table <- gt::gt(all_public_bodies |> count(type)) |>
  gt::cols_label(n = "Count", type = "Public Body Type") |>
  gt::tab_footnote(
    footnote = glue::glue(
      "Table last generated on {Sys.Date()}. Website data last updated on {website_last_updated}."
    )
  )

summary_table

gt::gtsave(summary_table, filename = "pb_count.png")
