# PROCEDURE
# run this in terminal:
# for i in {1..n} 
# Rscript data/run.batch.R, do
# end 
# where n = number of batches

args <- commandArgs(trailingOnly = TRUE)
batch <- as.integer(args[1])

require(dplyr)
require(readr)
require(httr)
require(jsonlite)
require(tidyr)
require(vroom)
require(purrr)

api_key <- "x~x"

# Load precomputed pairs
pairs <- readRDS("data/pairs.rds")

batch_size <- 5000
batch_start <- (batch - 1) * batch_size + 1
batch_end <- min(batch * batch_size, length(pairs))
pair_batch <- pairs[batch_start:batch_end]
index_batch <- batch_start:batch_end

prompt_base <- paste0(
  "Linguists and other scientists have developed methods that allow them to predict a sentence’s level of specificity based on its characteristics and content. Drawing from academic articles in computational linguistics, on which you have already been trained, which of the following two sentences is more specific (you must select one):\n\n"
)

base_body <- list(
  model = "gpt-4o",
  temperature = 0
)

safe_compare <- function(pair, i) {
  if (i %% 100 == 0) cat(Sys.time(), "- Completed:", i, "pairs\n")
  
  this_prompt <- paste0(
    prompt_base,
    "Sentence A: ", pair[1], "\n",
    "Sentence B: ", pair[2]
  )
  
  body <- modifyList(base_body, list(
    messages = list(list(role = "user", content = this_prompt))
  ))
  
  result <- tryCatch({
    res <- RETRY("POST",
                 url = "https://api.openai.com/v1/chat/completions",
                 times = 3,
                 pause_min = 1,
                 pause_cap = 5,
                 timeout(60),
                 httr::add_headers(Authorization = paste("Bearer", api_key)),
                 httr::content_type_json(),
                 body = toJSON(body, auto_unbox = TRUE),
                 encode = "json")
    Sys.sleep(0.2)
    content(res)$choices[[1]]$message$content
  }, error = function(e) NULL)
  
  if (is.null(result) || length(result) == 0 || is.na(result) || !is.character(result) || !nzchar(result[1])) {
    return(NULL)
  }
  
  result <- trimws(result)
  
  if (grepl(pair[1], result, fixed = TRUE)) {
    winner <- pair[1]; loser <- pair[2]
  } else {
    winner <- pair[2]; loser <- pair[1]
  }
  
  tibble(index = i, winner = winner, loser = loser)
}

results <- list()
for (i in seq_along(pair_batch)) {
  res <- safe_compare(pair_batch[[i]], index_batch[i])
  results[[length(results) + 1]] <- res
  
  if (i %% 100 == 0 || i == length(pair_batch)) {
    interim <- purrr::compact(results)
    if (length(interim) > 0) {
      df <- bind_rows(interim)
      batch_file <- sprintf("data/pairs_batch_%03d.csv", batch)
      vroom_write(df, batch_file, delim = ",", append = TRUE)
      results <- list()
    }
  }
}
