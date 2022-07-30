### Set up some useful data

library(dplyr)

## NFL
# load sharpe data
load_sharpe_data <- function(file_name) {
  url <- paste0("https://raw.githubusercontent.com/nflverse/nfldata/master/data/", file_name,".csv")
  suppressWarnings({ df <- readr::read_csv(url, col_types = readr::cols()) })
  return(df)
}

# NFL teams data
nfl_teams <- load_sharpe_data("teams") %>%
  dplyr::filter(season == max(season)) %>%
  dplyr::select(dplyr::any_of(c(
    "team_abbr" = "team", "team_id" = "nfl_team_id",
    "team" = "full", "draft_kings"
  ))) %>%
  dplyr::left_join(nflfastR::teams_colors_logos %>%
                     dplyr::select(team_abbr, team_id, dplyr::starts_with("team_color"),
                                   team_logo_espn),
                   by = c("team_abbr", "team_id")) %>%
  dplyr::mutate(
    team_color = dplyr::case_when(
      team_abbr == "LA" ~ "#003594",
      T ~ team_color),
    team_color2 = dplyr::case_when(
      team_abbr == "LA" ~ "#FFA300",
      T ~ team_color2))

## NBA
# NBA teams data
nba_teams <- nbastatR::nba_teams() %>%
  dplyr::filter(isNonNBATeam == 0,
                idTeam != 1612709930) %>%
  janitor::clean_names() %>%
  tidyr::separate(colors_team,
                  into = c("primary_color", "secondary_color", "tertiary_color"),
                  sep = ",",
                  extra = "merge",
                  fill = "right") %>%
  dplyr::mutate(draft_kings = paste0(slug_team, " ", team_name_full),
                draft_kings = dplyr::case_when(
                  slug_team == "GSW" ~ "GS Warriors",
                  slug_team == "SAS" ~ "SA Spurs",
                  slug_team == "NYK" ~ "NY Knicks",
                  slug_team == "NOP" ~ "NO Pelicans",
                  slug_team == "PHX" ~ "PHO Suns",
                  slug_team == "LAL" ~ "LA Lakers",
                  slug_team == "LAC" ~ "LA Clippers",
                  T ~ draft_kings)) %>%
  dplyr::select(dplyr::any_of(c(
    "team" = "name_team", "id_team",
    "team_abbr" = "slug_team", "draft_kings",
    "primary_color", "secondary_color", "tertiary_color",
    "team_logo" = "url_thumbnail_team"
  )))

# write to .csv
readr::write_csv(nfl_teams, "data/draftkings_nfl.csv")
readr::write_csv(nba_teams, "data/draftkings_nba.csv")
