nfl_players <- nflfastR::fast_scraper_roster(seasons = 2020)

nba_bets <- readRDS(url("https://github.com/JackLich10/betting_lines/blob/main/data/nba/drafkings_nba_lines.rds?raw=true"))

nba_bets %>%
  filter(event_id == "aa378522-b893-42da-1d8e-08d92a57373c") %>% View()


nfl_bets <- readRDS(url("https://github.com/JackLich10/betting_lines/blob/main/data/nfl/drafkings_nfl_lines.rds?raw=true"))

nfl_bets %>%
  filter(date == max(date)) %>% View()

nfl_bets %>%
  filter(bet_type == "Division Winner") %>% View()

nfl_bets %>%
  filter(subcategory_id == 7293)

pbp <- purrr::map_df(c("401333568", "401326609"), espnscrapeR::get_nfl_pbp)

pbp %>%
  dplyr::filter(drive_id %in% c(4013335682, 4013335684,
                                4013266091, 4013266093, 4013266095, 4013266097),
                # start_down %in% c(1, 2),
                !play_type %in% c("Kickoff", "Field Goal Good", "Punt", "Penalty",
                                  "End Period", "Two-minute warning")) %>%
  dplyr::mutate(play_type = ifelse(play_type == "Rush", "Rush", "Pass"),
                down_type = ifelse(start_down < 3, "Early Down", "Late Down")) %>%
  group_by(down_type, play_type) %>%
  summarise(plays = dplyr::n_distinct(play_id),
            yards_per_play = mean(yards_gained),
            .groups = "drop") %>%
  group_by(down_type) %>%
  mutate(pct = plays/sum(plays)) %>%
  ungroup() %>%
  select(down_type, play_type, plays, pct, yards_per_play) %>%
  gt::gt() %>%
  gt::cols_label(down_type = "",
                 play_type = "",
                 plays = "Plays",
                 yards_per_play = "Yards/Play") %>%
  gt::tab_header(title = gt::md("**New York Jets pre-season play types by down**"),
                 subtitle = gt::md("*Early = 1, 2 downs | Zach Wilson drives only*")) %>%
  gt::fmt_number(columns = dplyr::contains("yards_per_play"),
                 decimals = 2) %>%
  gt::fmt_percent(columns = dplyr::contains("pct"),
                  decimals = 1) %>%
  gt::tab_style(style = list(
    gt::cell_borders(
      sides = "left",
      color = "black",
      weight = gt::px(3))),
    locations = list(
      gt::cells_body(
        columns = c(yards_per_play)))) %>%
  gt::tab_style(style = list(
    gt::cell_borders(
      sides = "bottom",
      color = "black",
      weight = gt::px(3))),
    locations = list(
      gt::cells_column_labels(
        columns = gt::everything()))) %>%
  gt::data_color(columns = dplyr::ends_with("yards_per_play"),
                 colors = scales::col_numeric(
                   palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
                   domain = NULL)) %>%
  espnscrapeR::gt_theme_538() %>%
  gt::tab_source_note(paste0("Table: @jacklich10 | Data: ESPN"))

gt::gtsave(.Last.value, paste0("~/Desktop/plays.png"))

nfl_bets %>%
  filter(stringr::str_detect(bet_type, "Regular Season MVP")) %>%
  mutate(label = stringr::str_squish(stringr::str_remove(label, "\\(.*")),
         label = stringr::str_replace(label, " Jr", " Jr."),
         label = case_when(
           label == "DeShaun Watson" ~ "Deshaun Watson",
           T ~ label),
         label = ffscrapr::dp_cleannames(label, use_name_database = T)) %>%
  left_join(nfl_players %>%
              select(full_name, team_abbr = team, position, headshot_url) %>%
              mutate(full_name = ffscrapr::dp_cleannames(full_name, use_name_database = T),
                     team_abbr = case_when(
                       full_name == "Sam Darnold" ~ "CAR",
                       full_name == "Carson Wentz" ~ "IND",
                       full_name == "Matthew Stafford" ~ "LA",
                       T ~ team_abbr)),
            by = c("label" = "full_name")) %>%
  mutate(
    level = case_when(
      position == "QB" ~ 1,
      position == "RB" ~ 2,
      position == "WR" ~ 3,
      position == "DT" ~ 4,
      position == "DE" ~ 5,
      T ~ 6)) %>%
  arrange(level) %>%
  distinct(label, .keep_all = T) %>%
  arrange(desc(probability)) %>%
  select(date, bet_type, label, position, team_abbr, probability, headshot_url) %>%
  left_join(nfl_teams,
            by = c("team_abbr")) %>%
  mutate(label = forcats::fct_reorder(label, probability)) %>%
  head(20) %>%
  ggplot(aes(probability, label)) +
  geom_col(aes(color = team_color2, fill = team_color),
           width = 0.85) +
  geom_text(aes(label = label),
            hjust = 1.25, size = 3, color = "white", fontface = "bold.italic") +
  ggimage::geom_image(aes(probability, label, image = headshot_url),
                      asp = 1.618, size = 0.05, hjust = -1, inherit.aes = F) +
  geom_text(aes(label = scales::percent(probability, accuracy = 0.1)),
            hjust = -1.25, size = 3, fontface = "bold.italic") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1)),
                     labels = scales::percent) +
  scale_color_identity() +
  scale_fill_identity() +
  jacklich::theme_jack(aspect = T) +
  theme(axis.text.y = element_blank()) +
  labs(title = "Patrick Mahomes is the 2022 NFL Regular Season MVP favorite",
       subtitle = "Top 20 players | As of July 18, 2021",
       x = "Implied 2022 Regular Season MVP odds",
       y = NULL,
       caption = "Chart: @jacklich10 | Data: DraftKings")

ggsave(filename = "~/Desktop/mvp_odds.png", .Last.value, dpi = 1000, height = 5*1.2, width = 8*1.2)



