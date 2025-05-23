require(dplyr)
require(readr)
require(BradleyTerry2)
require(tidyr)
require(purrr)

# load exported batches of sentences
all_files <- list.files("data/spcfy.batch1", pattern = "^pairs_batch_\\d+\\.csv$", full.names = TRUE)

results <- lapply(all_files, function(f) {
  read_csv(f, col_names = FALSE, show_col_types = FALSE)
}) %>%
  bind_rows() %>%
  setNames(c("index","winner","loser")) %>%
  distinct(winner, loser, .keep_all=TRUE)

# Bradley Terry time
# Construct win/loss matrix
wins_df <- results %>%
  count(winner, loser, name = "wins")

losses_df <- wins_df %>%
  mutate(
    temp = winner,
    winner = loser,
    loser = temp,
    losses = wins
  ) %>%
  select(-wins, -temp)

bt_df <- full_join(wins_df, losses_df, by = c("winner", "loser")) %>%
  mutate(
    wins = coalesce(wins, 0),
    losses = coalesce(losses, 0)
  )

players <- union(bt_df$winner, bt_df$loser)

bt_df <- bt_df %>%
  mutate(
    winner = factor(winner, levels = players),
    loser  = factor(loser,  levels = players)
  )

# Fit Bradley-Terry model
bt_model <- BTm(
  outcome = cbind(bt_df$wins, bt_df$losses),
  player1 = bt_df$winner,
  player2 = bt_df$loser,
  data = bt_df
)

summary(bt_model)

# Extract ability scores
bt_abilities <- BTabilities(bt_model)
sentence_levels <- levels(bt_df$winner)

bsdt <- tibble(
  gpt_output = sentence_levels,
  specificity_score = bt_abilities[, "ability"]
) %>%
  mutate(specificity_score = as.numeric(specificity_score))

write_csv(bsdt, "data/specificF.csv")
