# ==============================================================================
# 1. LIBRARIES & CONFIGURATION
# ==============================================================================
library(dplyr)
library(stringr)
library(tidyr)
library(text2vec)
library(data.table)
library(lmtest)
library(sandwich)
library(readr)
library(stringi)
library(readxl)
library(purrr)
library(stargazer)

# Updated to use the finalized cleaned newspaper ads
NEWSPAPER_INPUT <- "C:/Users/unity/Documents/Dissertation/Final_Cleaned_Newspaper_Ads.rds"
EMBEDDING_PATH  <- "dolma_300_2024_1.2M.100_combined.txt"
EXPORT_DIR      <- "C:/Users/unity/Documents/Dissertation/Outputs/"
dir.create(EXPORT_DIR, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# 2. HELPER FUNCTIONS
# ==============================================================================
build_regex_pattern <- function(word_list){
  if (length(word_list) == 0) return("^$")
  escaped <- str_replace_all(word_list, "([\\.^$|()\\[\\]{}*+?\\\\-])", "\\\\\\1")
  paste0("\\b(", paste(escaped, collapse = "|"), ")\\b")
}

prepare_ads <- function(df_raw){
  df_raw %>%
    distinct(Ad_Base_ID, .keep_all = TRUE) %>%
    mutate(
      Source_Type = "Newspaper",
      # Normalizes the existing CleanedText column for dictionary matching
      text = CleanedText %>% 
        str_to_lower() %>% 
        str_replace_all("[^a-z\\s]", " ") %>% 
        str_replace_all("[[:space:]]+", " ") %>% 
        str_trim()
    ) %>%
    filter(!is.na(text), text != "")
}

expand_with_threshold <- function(seeds, matrix, threshold = 0.80, top_n = 50){
  valid <- seeds[seeds %in% rownames(matrix)]
  if (length(valid) == 0) return(character(0))
  
  seed_vec <- colMeans(matrix[valid, , drop = FALSE])
  cos_sim <- sim2(x = matrix, y = matrix(seed_vec, nrow = 1), method = "cosine", norm = "l2")[, 1]
  
  candidate_names <- names(cos_sim)
  is_valid_word <- str_detect(candidate_names, "^[a-z]{3,}$")
  
  stop_words <- c(
    "the", "and", "that", "this", "for", "with", "from", "your", "our", "us", "we", "you",
    "their", "they", "them", "which", "will", "can", "would", "could", "should", "must",
    "have", "has", "had", "been", "are", "was", "were", "what", "who", "whom", "its", 
    "any", "all", "not", "but", "also", "than", "then", "into", "upon", "these", "those", 
    "because", "still", "almost", "indeed", "rather", "yet", "somehow", "such", "both",
    "much", "many", "most", "other", "another", "some", "every", "very", "just", "like",
    "is", "as", "in", "to", "of", "a", "an", "be", "themselves", "others", "one", "whose", 
    "neither", "nothing", "nowhere", "everyone", "someone", "anyone", "everything", 
    "anything", "somewhere", "more", "less", "among", "beyond", "within", "through", 
    "while", "when", "due", "where", "why", "how", "although", "though", "unless", 
    "until", "since", "without", "during", "before", "after", "nor", "importantly", 
    "thus", "therefore", "hence", "furthermore", "however", "whether", "ultimately", 
    "simply", "only", "otherwise", "regardless", "besides", "moreover", "especially", 
    "despite", "instead", "perhaps", "maybe", "often", "always", "never", "sometimes", 
    "already", "even", "too", "quite", "really", "ironically", "hardly", "particularly", 
    "possibly", "seemingly", "likely", "apparently", "truly", "similarly", "solely", 
    "namely", "secondly", "effectively","additionally", "directly", "now", "become", 
    "becomes", "becoming", "being", "make", "makes", "making", "made", "take", 
    "takes", "taking", "took", "keep", "keeps", "keeping", "kept", "use", "uses",
    "using", "used", "bring", "brings", "brought", "bringing", "leave", "leaves",
    "leaving", "left", "having", "do", "does", "doing", "did", "done", "get", 
    "gets", "getting", "got", "gotten", "go", "goes", "going", "gone", "went", 
    "come", "comes", "coming", "came", "say", "says", "saying", "said", "able",
    "unable", "knowing", "meant", "feel", "aware", "convinced", "find", "consider",
    "want", "require", "intended", "involved", "recognized", 
    "maintain", "maintaining", "work", "working", "may", "might", "continue", 
    "choose", "important", "importance", "particular", "means", "purpose", "way",
    "ways", "addition", "fact", "sense", "part", "rest", "present", "cause", 
    "causes", "caused", "situation", "realize", "realizing", "realized", 
    "imagine", "imagining", "imagined", "concern", "concerned", "regard", "regarding", 
    "own", "well", "certain", "sure", "life", "lives", "people", "alive", "human", 
    "world", "there", "long", "reason", "kind", "possible", "necessary", "effective", 
    "better", "new", "further", "future", "greater", "latter", "terms",
    "attention", "critical", "focus", "focused", "committed", "sad", "shame",
    "unhappy", "fear", "afraid", "fear", "afraid", "frustrated", "confused", "feeling",
    "felt", "moment", "spite", "unfortunate"
  )
  is_not_stopword <- !(candidate_names %in% stop_words)
  
  clean_sim <- cos_sim[is_valid_word & is_not_stopword]
  clean_sim <- sort(clean_sim[clean_sim >= threshold], decreasing = TRUE)
  res <- head(names(clean_sim), top_n)
  
  return(res)
}

print_report <- function(expanded, name, target_df) {
  matches <- expanded[expanded %in% unique(unlist(str_split(target_df$text, "\\W+")))]
  cat("\n--- DICTIONARY:", toupper(name), "---\n")
  cat("Total clean words:", length(expanded), "| Found in corpus:", length(matches), "\n")
  cat(paste(str_wrap(paste(expanded, collapse = ", "), width = 70), collapse = "\n"), "\n")
}

# ==============================================================================
# 3. DATA LOADING & VECTOR EXPANSION
# ==============================================================================
df <- prepare_ads(readRDS(NEWSPAPER_INPUT))

coded_df <- read_excel("C:/Users/unity/Documents/Dissertation/Advertisement Coding.xlsx")

df <- df %>%
  left_join(coded_df %>% select(Ad_Base_ID, Is_International, Is_Animal),
            by = "Ad_Base_ID") %>%
  filter(Is_Animal == 0, !is.na(Is_International))

vectors <- fread(
  EMBEDDING_PATH, header = FALSE, sep = " ", fill = TRUE, quote = "",
  colClasses = c("character", rep("numeric", 300))
)

glove_matrix <- as.matrix(vectors[, -1])
rownames(glove_matrix) <- vectors[[1]]
rm(vectors)
gc()

dics <- list(
  victimhood = unique(c(
   "helpless", "unable", "powerless", "vulnerable"
  )),
  physical_needs = c("food", "water", "hunger", "malnutrition", "disease", "medicine", "shelter", "survive"),
  charity_agency = c("help", "support", "provide", "rescue", "save", "help", "deliver", "fund", "rescue", "sponsor", "empower", "protect"),
  beneficiary_passivity = c("received", "rely", "depend", "given", "provided", "supported", "rescued", "supplied", "awaiting", "need")
)

expanded_dictionaries <- map(dics, ~expand_with_threshold(.x, glove_matrix, 0.80, 50))

# ----------------- Manual Pruning -----------------
victim_noise <- c("enough", "longer", "away", "eventually", "far", "giving", "living", "none", "out", "about", "lack", "supposed", "wonder", "happens", "ever", "except", "mind", "little", "seriously", "turn", "else", "whole", "over", "once", "mention", "taken", "person", "nobody", "seem", "remain", "equally", "completely", "unaware", "dealing", "surely", "seemed", "unfortunately", "sadly", "bad", "terrible", "worse", "worst", "difficult")
charity_noise <- c("lastly", "full", "good", "order", "out", "put", "easy", "works", "ready", "advantage", "key", "time", "rely", "required", "additional", "let", "enough", "access", "longer", "safe", "easier", "step", "example", "effort", "opportunity", "choice", "ability", "change", "need", "needs", "needed", "forget", "supposed", "hope", "once", "alone", "wish", "great", "see", "remember", "know", "ever", "turn", "whatever", "soon", "finally", "again", "surely", "first", "here", "same", "wonder", "taken", "him", "next", "yes", "right", "about", "thing", "wanted", "wants", "else")
passivity_noise <- c("towards", "remain", "based", "vital", "continues", "role", "recognize", "far", "demands", "organization", "presence", "considered", "developing", "matter", "matters", "primarily", "highly", "lack", "social", "current", "strong", "personal", "same", "true", "certainly", "regards", "country", "alone", "foremost", "significant", "commitment", "knowledge", "supporting", "established", "establish", "establishing", "understanding", "experience", "subject", "first", "taken", "status", "third", "prior", "consideration", "exception", "individual", "following", "person", "allowed", "chosen", "interest", "example", "requested", "either", "under", "none", "request", "initially", "respect", "except", "second", "least", "gave", "time", "stated", "previously", "mentioned", "assumed", "case", "return", "number")

expanded_dictionaries$victimhood            <- setdiff(expanded_dictionaries$victimhood, victim_noise)
expanded_dictionaries$charity_agency        <- setdiff(expanded_dictionaries$charity_agency, charity_noise)
expanded_dictionaries$beneficiary_passivity <- setdiff(expanded_dictionaries$beneficiary_passivity, passivity_noise)

expanded_dictionaries$beneficiary_passivity <- setdiff(expanded_dictionaries$beneficiary_passivity, expanded_dictionaries$charity_agency)
expanded_dictionaries$victimhood            <- setdiff(expanded_dictionaries$victimhood, expanded_dictionaries$charity_agency)
expanded_dictionaries$victimhood            <- setdiff(expanded_dictionaries$victimhood, expanded_dictionaries$beneficiary_passivity)

# ==============================================================================
# 5. DENSITY CALCULATION & REGRESSIONS
# ==============================================================================
df <- df %>%
  mutate(
    vic_hits       = stri_count_regex(text, build_regex_pattern(expanded_dictionaries$victimhood)),
    char_age_hits  = stri_count_regex(text, build_regex_pattern(expanded_dictionaries$charity_agency)),
    bene_pass_hits = stri_count_regex(text, build_regex_pattern(expanded_dictionaries$beneficiary_passivity)),
    
    # Calculate densities directly using the pre-calculated Dictionary_Words column
    Victimhood_Density     = vic_hits / (Dictionary_Words + 1e-6) * 1000,
    Charity_Agency_Density = char_age_hits / (Dictionary_Words + 1e-6) * 1000,
    Bene_Pass_Density      = bene_pass_hits / (Dictionary_Words + 1e-6) * 1000,
    
    Agency_Passivity_Ratio = log((char_age_hits + 1) / (bene_pass_hits + 1))
  )

# Using Total_Tokens from the existing column
analysis_df <- df %>% mutate(log_tokens = log(Total_Tokens + 1))

# Regressions (Include NoiseRatio from the existing column)
m_vic   <- lm(Victimhood_Density ~ Is_International + log_tokens + NoiseRatio, analysis_df)
m_char  <- lm(Charity_Agency_Density ~ Is_International + log_tokens + NoiseRatio, analysis_df)
m_bene  <- lm(Bene_Pass_Density ~ Is_International + log_tokens + NoiseRatio, analysis_df)
m_ratio <- lm(Agency_Passivity_Ratio ~ Is_International + log_tokens + NoiseRatio, analysis_df) 

# Extract robust standard errors for accurate p-values (Heteroskedasticity-consistent)
robust_se <- list(
  sqrt(diag(vcovHC(m_vic, type = "HC3"))),
  sqrt(diag(vcovHC(m_char, type = "HC3"))),
  sqrt(diag(vcovHC(m_bene, type = "HC3"))),
  sqrt(diag(vcovHC(m_ratio, type = "HC3")))
)

# Print results with robust standard errors
cat("\n\n==================== ROBUST REGRESSION RESULTS ====================\n")
cat("\n--- VICTIMHOOD / DEFICIT (Combined) ---\n")
print(coeftest(m_vic, vcov = vcovHC(m_vic, type = "HC3")))

cat("\n--- CHARITY AGENCY ---\n")
print(coeftest(m_char, vcov = vcovHC(m_char, type = "HC3")))

cat("\n--- BENEFICIARY PASSIVITY ---\n")
print(coeftest(m_bene, vcov = vcovHC(m_bene, type = "HC3")))

cat("\n--- RATIO: AGENCY vs PASSIVITY ---\n")
print(coeftest(m_ratio, vcov = vcovHC(m_ratio, type = "HC3")))

# Print Dictionary Reports
cat("\n\n==================== FINAL DICTIONARY REPORTS ====================\n")
walk2(expanded_dictionaries, names(expanded_dictionaries), ~print_report(.x, .y, df))

# ==============================================================================
# 6. DESCRIPTIVE STATS & EXPORTS
# ==============================================================================
descriptive_stats <- df %>%
  mutate(sentence_count = stri_count_regex(RawCombinedText, "[.!?]+")) %>%
  summarise(
    Total_word_count = sum(Total_Tokens, na.rm = TRUE),
    Mean_words_per_ad = mean(Total_Tokens, na.rm = TRUE),
    Median_words_per_ad = median(Total_Tokens, na.rm = TRUE),
    SD_words_per_ad = sd(Total_Tokens, na.rm = TRUE),
    Mean_sentences_per_ad = mean(sentence_count, na.rm = TRUE)
  )

print(descriptive_stats)

# Table: OLS Regression Results using Robust Standard Errors
stargazer(m_vic, m_char, m_bene, m_ratio, 
          type = "text", 
          se = robust_se, 
          title = "OLS Regression Results: Theme Densities & Ratios (Robust SEs)",
          dep.var.labels = c("Victimhood", "Charity Agency", "Beneficiary Passivity", "Log(Agency/Passivity)"),
          covariate.labels = c("Is International (Yes=1)", "Log Tokens (Word Count)", "OCR Noise Ratio"),
          out = paste0(EXPORT_DIR, "OLS_Regression_Results_Robust.html"))

# Table: Theme scores by International vs Domestic (Summary Stats Table)
theme_summary <- analysis_df %>%
  group_by(Is_International) %>%
  summarise(
    Mean_Victimhood = mean(Victimhood_Density, na.rm = TRUE),
    SD_Victimhood = sd(Victimhood_Density, na.rm = TRUE),
    Mean_Charity = mean(Charity_Agency_Density, na.rm = TRUE),
    SD_Charity = sd(Charity_Agency_Density, na.rm = TRUE),
    Mean_Beneficiary_Pass = mean(Bene_Pass_Density, na.rm = TRUE),
    SD_Beneficiary_Pass = sd(Bene_Pass_Density, na.rm = TRUE),
    Mean_Ratio = mean(Agency_Passivity_Ratio, na.rm = TRUE),
    SD_Ratio = sd(Agency_Passivity_Ratio, na.rm = TRUE) 
  )
print(theme_summary)