getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
set.seed(2026)

# ==============================================================================
# 1. LIBRARIES & CONFIGURATION
# ==============================================================================
library(dplyr)
library(stringr)
library(text2vec)
library(data.table)
library(stopwords)
library(tibble)
library(readr)

EMBEDDING_PATH <- "dolma_300_2024_1.2M.100_combined.txt"
EXPORT_DIR     <- "C:/Users/unity/Documents/Dissertation/Outputs/"

dir.create(EXPORT_DIR, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# 2. HELPER FUNCTION
# ==============================================================================
expand_with_threshold <- function(seeds, embedding_matrix, threshold, top_n = Inf){
  valid_seeds <- seeds[seeds %in% rownames(embedding_matrix)]
  if(length(valid_seeds) == 0){
    warning("No seed words found in embeddings")
    return(tibble(word = character(), similarity = numeric()))
  }
  
  # Calculate average seed vector
  seed_vector <- colMeans(embedding_matrix[valid_seeds, , drop = FALSE])
  
  # Compute cosine similarity across all embedding terms
  similarities <- sim2(x = embedding_matrix, y = matrix(seed_vector, nrow = 1), method = "cosine", norm = "l2")[, 1]
  names(similarities) <- rownames(embedding_matrix)
  
  # Filter solely by length, standard English stopwords, and threshold (no exclusions or top_n cap)
  tibble(word = names(similarities), similarity = as.numeric(similarities)) %>%
    filter(str_detect(word, "^[a-z]{3,}$"), !(word %in% stopwords("en")), similarity >= threshold) %>%
    arrange(desc(similarity)) %>%
    slice_head(n = top_n)
}

# ==============================================================================
# 3. LOAD EMBEDDINGS
# ==============================================================================
cat("\nLoading word embeddings from:", EMBEDDING_PATH, "...\n")
vectors <- fread(EMBEDDING_PATH, header = FALSE, sep = " ", fill = TRUE, quote = "", colClasses = c("character", rep("numeric", 300)))
embedding_matrix <- as.matrix(vectors[, -1])
rownames(embedding_matrix) <- vectors[[1]]
rm(vectors)
gc()
cat("Embeddings successfully loaded into memory.\n")

# ==============================================================================
# 4. SEED DICTIONARIES
# ==============================================================================
seed_dictionaries <- list(
  vulnerability  = c("helplessness", "hopelessness", "poverty", "suffering", "victim", "powerless", "depend"),
  charity_agency = c("protect", "benevolence", "giver", "aid", "assistance", "rescuing", "heroic", "savior")
)

# ==============================================================================
# 5. SENSITIVITY ANALYSIS (0.75 TO 0.80)
# ==============================================================================
cat("\n========================================================================\n")
cat("RUNNING SENSITIVITY ANALYSIS (0.75 to 0.80)\n")
cat("========================================================================\n")

thresholds_to_test <- seq(0.70, 0.76, by = 0.01)
sensitivity_results <- list()

for(dict_name in names(seed_dictionaries)) {
  
  cat("\n------------------------------------------------------------------------\n")
  cat("DICTIONARY:", toupper(dict_name), "\n")
  cat("------------------------------------------------------------------------\n")
  
  seeds <- seed_dictionaries[[dict_name]]
  
  for(thresh in thresholds_to_test) {
    
    # Run expansion with top_n = Inf (unrestricted word list)
    expanded_df <- expand_with_threshold(
      seeds = seeds, 
      embedding_matrix = embedding_matrix, 
      threshold = thresh, 
      top_n = Inf
    )
    
    words <- expanded_df$word
    
    # Save output for CSV export
    sensitivity_results[[paste(dict_name, thresh, sep = "_")]] <- tibble(
      Dictionary = dict_name,
      Threshold  = thresh,
      Word_Count = length(words),
      Words      = paste(words, collapse = ", ")
    )
    
    # Print progress and word lists to console
    cat(sprintf("\n[ %s | Threshold: %.2f | Words Retained: %d ]\n", toupper(dict_name), thresh, length(words)))
    
    if(length(words) > 0) {
      cat(paste(str_wrap(paste(words, collapse = ", "), width = 90), collapse = "\n"), "\n")
    } else {
      cat("No words met this similarity threshold.\n")
    }
  }
}

# Export sensitivity table to CSV
sensitivity_df <- bind_rows(sensitivity_results)
export_file <- file.path(EXPORT_DIR, "standalone_sensitivity_analysis.csv")
write_csv(sensitivity_df, export_file)

cat("\n========================================================================\n")
cat("SENSITIVITY ANALYSIS COMPLETE!\nSummary saved to:", export_file, "\n")
cat("========================================================================\n")