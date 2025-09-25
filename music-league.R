
library(tidyverse)

setwd("~/MIT Dropbox/Laura Royden/musicleague")

dirs <- list.dirs(path = ".", recursive = FALSE, full.names = FALSE)
dirs <- dirs[str_starts(dirs, "export_")]

base_dir <- getwd()

all_data <- data.frame()
for (folder in dirs) {
  setwd(file.path(base_dir, folder))
  
  votes <- read_csv("votes.csv")
  competitors <- read_csv("competitors.csv")
  rounds <- read_csv("rounds.csv")
  submissions <- read_csv("submissions.csv")
  
  
  ## clean up duplicate column names
  
  votes <- votes %>%
    rename(Voter_Comment = Comment,
           Vote_Time = Created,
           Voter_ID = `Voter ID`,
           Round_ID = `Round ID`)
  
  rounds <- rounds %>%
    rename(Round_Time = Created,
           Round_Name = Name,
           Round_Description = Description,
           Round_ID = ID)
  
  submissions <- submissions %>%
    rename(Submissions_Time = Created,
           Submitter_ID = `Submitter ID`,
           Submitter_Comment = Comment,
           Round_ID = `Round ID`)
  
  competitors <- competitors %>%
    rename(Person_ID = ID)
  
  # join votes and submission
  join <- left_join(votes, submissions, by=c("Spotify URI", "Round_ID"))
  
  # join rounds onto it
  join <- left_join(join, rounds, by="Round_ID")
  
  
  # join competitors onto it
  join <- join %>%
    left_join(competitors, by = c("Voter_ID" = "Person_ID")) %>%
    rename(Voter_Name = Name) %>%
    left_join(competitors, by = c("Submitter_ID" = "Person_ID")) %>%
    rename(Submitter_Name = Name) %>%
    select(-Voter_ID, -Submitter_ID) %>%
    mutate(Source_Export = folder)
  
  
  all_data <- rbind(all_data, join)
  
  rm(competitors, join, rounds, submissions, votes)
}

setwd("~/MIT Dropbox/Laura Royden/musicleague")

all_data <- all_data %>% 
  select(Title,	Album,	`Artist(s)`, Submitter_Name,	Submitter_Comment,	Round_Name, Round_Description,	Voter_Name,	`Points Assigned`,	Voter_Comment,	`Playlist URL`,	Vote_Time,	Round_ID,	Submissions_Time,	`Visible To Voters`, Round_Time,	Source_Export)




just_submissions <- all_data %>% select(
  Title, `Artist(s)`, Album, Submitter_Name, Submitter_Comment, Round_Name, Round_Description
) %>% unique()



legacy <- all_data %>% group_by(Submitter_Name) %>%
  summarize(sum = sum(`Points Assigned`), leagues = n_distinct(Source_Export), abs = sum(abs(`Points Assigned`)) ) %>%
  arrange(desc(sum))

shorten_with_tooltip <- function(x, n = 80) {
  x <- replace_na(x, "")
  needs <- nchar(x) > n
  shown <- ifelse(needs, paste0(substr(x, 1, n), "…"), x)
  # escape HTML in text to be safe
  esc <- htmltools::htmlEscape
  sprintf('<span title="%s">%s</span>', esc(x), esc(shown))
}

linkify <- function(url) {
  url <- replace_na(url, "")
  ifelse(nchar(url) == 0, "",
         sprintf('<a href="%s" target="_blank" rel="noopener">link</a>',
                 htmltools::htmlEscape(url)))
}

dt_opts <- list(
  pageLength = 25,
  lengthMenu = c(25, 50, 100, 250),
  dom = "Bfrtip",                           # Buttons + filter + table
  buttons = c("copy", "csv", "excel"),
  deferRender = TRUE,                       # speeds up initial draw
  scrollX = TRUE
)



## Just submissions

display_sub <- just_submissions %>% 
  mutate(
    Round_Description = shorten_with_tooltip(Round_Description, 120),
    Submitter_Comment = shorten_with_tooltip(Submitter_Comment, 120)
  )

datatable(
  display_sub,
  filter = "top",
  options = dt_opts,
  escape = FALSE,
  rownames = FALSE,
  extensions = c("Buttons", "FixedHeader")
)



## Legacy points

datatable(
  legacy,
  filter = "top",
  options = dt_opts,
  rownames = FALSE,
  extensions = c("Buttons", "FixedHeader")
)




## Full table
# Build a clean display frame with transformed columns
display_all <- all_data %>% 
  mutate(
    `Playlist URL` = linkify(`Playlist URL`),
    Round_Description = shorten_with_tooltip(Round_Description, 120),
    Submitter_Comment = shorten_with_tooltip(Submitter_Comment, 120),
    Voter_Comment = shorten_with_tooltip(Voter_Comment, 120)
  )

datatable(
  display_all,
  filter = "top",
  options = dt_opts,
  escape = FALSE,         # we added safe HTML (links + tooltips)
  rownames = FALSE,
  extensions = c("Buttons", "FixedHeader"),
  class = "stripe hover order-column row-border"
)


