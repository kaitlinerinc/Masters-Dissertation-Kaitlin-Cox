# ==============================================================================
# 0. CLEAR ENVIRONMENT (CRITICAL FIX)
# ==============================================================================
rm(list = ls()) # Clears old data frames so old column names don't cause errors
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ==============================================================================
# 1. LIBRARIES
# ==============================================================================
library(dplyr)
library(stringr)
library(tidyr)
library(udpipe)
library(text2vec)
library(data.table)
library(lmtest)
library(sandwich)
library(ggplot2)
library(purrr)
library(forcats)
library(broom)
library(readr)
library(stringi)
library(splines)
library(irr)
library(readxl)
library(openxlsx)

# ==============================================================================
# 2. CONFIGURATION & MODELS
# ==============================================================================
NEWSPAPER_INPUT <- "C:/Users/unity/Documents/Dissertation/Newspaper_Ads.rds"
MAILOUT_INPUT   <- "C:/Users/unity/Documents/Dissertation/Mailout_Ads.rds"
EMBEDDING_PATH  <- "dolma_300_2024_1.2M.100_combined.txt"
MODEL_FILE      <- "english-ewt-ud-2.5-191206.udpipe"
EXPORT_DIR      <- "C:/Users/unity/Documents/Dissertation/Outputs/"

# Load/Download UDPipe Model
if (!file.exists(MODEL_FILE)) {
  MODEL_FILE <- udpipe_download_model(language = "english-ewt")$file_model
}
ud_model <- udpipe_load_model(MODEL_FILE)

# Ensure export directory exists
dir.create(EXPORT_DIR, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# 3. HELPER FUNCTIONS
# ==============================================================================
build_regex_pattern <- function(word_list){
  if (length(word_list) == 0) return("^$") 
  escaped <- str_replace_all(word_list, "([\\.^$|()\\[\\]{}*+?\\\\-])", "\\\\\\1")
  paste0("\\b(", paste(escaped, collapse = "|"), ")\\b")
}

prepare_ads <- function(df_raw, source_label){
  df_raw %>%
    distinct(Ad_Base_ID, .keep_all = TRUE) %>%
    mutate(
      Source_Type = source_label,
      text = CombinedAdText %>%
        str_to_lower() %>%
        str_replace_all("[[:space:]]+", " ") %>%
        str_trim(),
      total_tokens = str_count(text, "\\S+")
    ) %>%
    filter(!is.na(text), text != "")
}

expand_with_threshold <- function(seeds, matrix, threshold = 0.80, top_n = 50){
  valid <- seeds[seeds %in% rownames(matrix)]
  if (length(valid) == 0) return(character(0))
  
  seed_vec <- colMeans(matrix[valid, , drop = FALSE])
  
  cos_sim <- sim2(
    x = matrix,
    y = matrix(seed_vec, nrow = 1),
    method = "cosine",
    norm = "l2"
  )
  
  sim_scores <- cos_sim[, 1]
  above_threshold_words <- names(sim_scores[sim_scores >= threshold])
  sorted_words <- names(sort(sim_scores[above_threshold_words], decreasing = TRUE))
  
  hubs_and_stopwords <- c(
    # Original stopwords
    "the", "a", "an", "and", "or", "but", "if", "because", "as", "until", "while",
    "of", "at", "by", "for", "with", "about", "against", "between", "into", "through",
    "during", "before", "after", "above", "below", "to", "from", "up", "down", "in", "out",
    "on", "off", "over", "under", "again", "further", "then", "once", "here", "there",
    "when", "where", "why", "how", "all", "any", "both", "each", "few", "more", "most",
    "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than", "too", "very",
    "i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours",
    "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself",
    "they", "them", "their", "theirs", "themselves", "what", "which", "who", "whom",
    "this", "that", "these", "those", "am", "is", "are", "was", "were", "be", "been", "being",
    "have", "has", "had", "having", "do", "does", "did", "doing", "would", "should", "could", "ought",
    "can", "will", "one", "like", "just", "time", "also", "get", "people", "make", "even", "now", 
    "know", "first", "well", "good", "way", "think", "work", "much", "new", "use", "many", "said",
    "look", "see", "want", "give", "take", "year", "years", "day", "days", "two", "three", "come",
    "back", "also", "made", "may", "go", "going", "come", "came", "put", "take", "took", "find", 
    "almost", "still", "despite", "nothing", "become", "becomes", "becoming", "brought", "especially", 
    "due", "causes", "rest", "long", "bring", "part", "ways", "addition", "based", "continues",
    
    # Newly added stopwords from diagnostic run 1
    "indeed", "rather", "yet", "ultimately", "otherwise", "somehow", "often", 
    "badly", "life", "lives", "situation", "others", "neither", "fact", "nowhere", 
    "realize", "leaving", "imagine", "knowing", "meant", "alive", "cause", 
    "unfortunately", "suffering", "sick", "ill", "alone", "poor", "suffer", 
    "importantly", "important", "importance", "beyond", "thus", "within", "whose", 
    "regardless", "particular", "however", "regard", "therefore", "hence", "whether", 
    "furthermore", "truly", "bringing", "means", "present", "concerned", "sense", 
    "focus", "critical", "needs",
    
    # Newly added stopwords from diagnostic run 2
    "sometimes", "perhaps", "feel", "reason", "ironically", "hardly", "though", 
    "particularly", "aware", "possibly", "kind", "seemingly", "never", "leave", 
    "likely", "convinced", "apparently", "obviously", "person", "seriously", 
    "nobody", "seem", "remain", "gone", "namely", "world", "greater", "latter", 
    "moreover", "similarly", "among", "continue", "terms", "secondly", "solely", 
    "towards", "given", "human", "attention", "concern", "experience", "involved"
  )
  
  cleaned_words <- sorted_words[
    str_detect(sorted_words, "^[a-z]{3,}$") & 
      !(sorted_words %in% hubs_and_stopwords)
  ]
  
  return(head(cleaned_words, top_n))
}

# ==============================================================================
# 4. DATA INGESTION & CODES MERGE
# ==============================================================================
newspaper_df <- prepare_ads(readRDS(NEWSPAPER_INPUT), "Newspaper")
mailout_df   <- prepare_ads(readRDS(MAILOUT_INPUT), "Mailout")
df           <- bind_rows(newspaper_df, mailout_df)

coding_file <- "C:/Users/unity/Documents/Dissertation/Advertisement Coding.xlsx"
coded_df    <- read_excel(coding_file) 

df <- df %>%
  left_join(coded_df %>% select(Ad_Base_ID, Is_International, Is_Animal), by = "Ad_Base_ID") %>%
  mutate(
    Is_International = case_when(
      Source_Type == "Mailout" ~ 1L,
      TRUE ~ as.integer(Is_International)
    ),
    Is_Animal = case_when(
      Source_Type == "Mailout" ~ 0L,
      TRUE ~ as.integer(Is_Animal)
    )
  ) %>%
  filter(!is.na(Is_International), !is.na(Is_Animal))

df <- df %>% filter(Is_Animal == 0)

# ==============================================================================
# 5. DICTIONARY & VECTOR EMBEDDING EXPANSION
# ==============================================================================
vulnerability_words <- c(
  "helpless","vulnerable","victim","suffering","desperate", 
  "weak","hopeless","abandoned","destitute","impoverished",
  "dying","needy","fragile"
)

physical_needs_words <- c(
  "hunger","hungry","starvation","thirst","cold","sick","disease",
  "homeless","food","water","shelter","famine","malnutrition"
)

agency_words <- c(
  "autonomy", "dignity", "self-reliance", "capable", "independent", 
  "thriving", "self-sufficient", "leading"
)

saviorism_words <- c("rescue", "save", "transform", "deliver", "bestow", 
                     "salvation", "lift", "intervene")

primitive_words <- c("remote", "traditional", "tribal", "ancient", "isolated", "untouched", "simple")

vectors <- fread(EMBEDDING_PATH, header = FALSE, sep = " ", fill = TRUE,
                 quote = "", 
                 colClasses = c("character", rep("numeric", 300)))

word_names <- vectors[[1]]
glove_matrix <- as.matrix(vectors[, -1])
rownames(glove_matrix) <- word_names
rm(vectors); gc()

expanded_vulnerability <- expand_with_threshold(vulnerability_words, glove_matrix, threshold = 0.80, top_n = 50)
expanded_physical      <- expand_with_threshold(physical_needs_words, glove_matrix, threshold = 0.80, top_n = 50)
expanded_agency        <- expand_with_threshold(agency_words, glove_matrix, threshold = 0.80, top_n = 50)

pat_v <- build_regex_pattern(expanded_vulnerability)
pat_p <- build_regex_pattern(expanded_physical)
pat_e <- build_regex_pattern(expanded_agency)

df <- df %>%
  mutate(
    vulnerability_hits = stri_count_regex(text, pat_v),
    physical_hits      = stri_count_regex(text, pat_p),
    agency_hits        = stri_count_regex(text, pat_e),
    
    # CRITICAL: This is where Agency_Density is created!
    Vulnerability_Density = vulnerability_hits / (total_tokens + 1e-6) * 1000,
    Physical_Density      = physical_hits / (total_tokens + 1e-6) * 1000,
    Agency_Density        = agency_hits / (total_tokens + 1e-6) * 1000
  )

# ==============================================================================
# 7. REGRESSION MODELING & TESTING
# ==============================================================================
# Prepare analysis_df
analysis_df <- df %>%
  mutate(log_tokens = log(total_tokens + 1))

# Define model datasets
newspaper_df <- analysis_df %>% filter(Source_Type == "Newspaper")

# Models: Focusing on the three primary thematic densities
m_vul <- lm(Vulnerability_Density ~ Is_International + log_tokens, newspaper_df)
m_phy <- lm(Physical_Density ~ Is_International + log_tokens, newspaper_df)
m_age <- lm(Agency_Density ~ Is_International + log_tokens, newspaper_df)

# Tests
cat("\n--- Regression Results: Vulnerability ---\n")
coeftest(m_vul, vcov = vcovHC(m_vul, "HC3"))

cat("\n--- Regression Results: Physical Needs ---\n")
coeftest(m_phy, vcov = vcovHC(m_phy, "HC3"))

cat("\n--- Regression Results: Agency ---\n")
coeftest(m_age, vcov = vcovHC(m_age, "HC3"))

# ==============================================================================
# 8. EXPORT RESULTS
# ==============================================================================
write_csv(analysis_df, paste0(EXPORT_DIR, "final_dataset.csv"))
saveRDS(analysis_df, paste0(EXPORT_DIR, "final_dataset.rds"))

# ==============================================================================
# 9. SENSITIVITY ANALYSIS (Cleaned)
# ==============================================================================
thresholds_to_test <- seq(0.75, 0.95, by = 0.05)
sensitivity_results <- list()

cat("\n--- Running Sensitivity Analysis (Agency Dictionary) ---\n")

for (thresh in thresholds_to_test) {
  # Expand Agency dictionary at the current threshold
  temp_expanded <- expand_with_threshold(agency_words, glove_matrix, threshold = thresh, top_n = 50)
  temp_pat      <- build_regex_pattern(temp_expanded)
  
  # Update temp data for the specific threshold
  temp_df <- analysis_df %>%
    filter(Source_Type == "Newspaper") %>%
    mutate(
      temp_hits = stri_count_regex(text, temp_pat),
      temp_Density = temp_hits / (total_tokens + 1e-6) * 1000
    )
  
  # Run regression
  temp_model <- lm(temp_Density ~ Is_International + log_tokens, data = temp_df)
  robust_test <- coeftest(temp_model, vcov = vcovHC(temp_model, "HC3"))
  
  sensitivity_results[[as.character(thresh)]] <- data.frame(
    Threshold = thresh,
    Dictionary_Size = length(temp_expanded),
    Estimate = robust_test["Is_International", "Estimate"],
    Std_Error = robust_test["Is_International", "Std. Error"],
    P_Value = robust_test["Is_International", "Pr(>|t|)"]
  )
}

sensitivity_df <- bind_rows(sensitivity_results)
print(sensitivity_df)