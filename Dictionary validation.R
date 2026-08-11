# ==============================================================================
# 1. LIBRARIES & CONFIGURATION
# ==============================================================================

set.seed(2026)

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(readr)
  library(stringi)
  library(purrr)
  library(tibble)
  library(MASS)
  library(sandwich)
  library(lmtest)
  library(ggplot2)
  library(modelsummary)
  library(broom)
  library(pscl)
  library(tidytext)
  library(xtable)
  library(marginaleffects)
  library(patchwork)
  library(readxl)
  library(car)
  library(quanteda) # Required for KWIC
})

NEWSPAPER_INPUT <- "C:/Users/unity/Documents/Github/Dissertation/Final_Cleaned_Newspaper_Ads.rds"
CODING_FILE_INPUT <- "C:/Users/unity/Documents/Github/Dissertation/Advertisement Coding.xlsx"

EXPORT_DIR <- "C:/Users/unity/Documents/Github/Dissertation/"
APPENDIX_DIR <- file.path(EXPORT_DIR, "appendix")

dir.create(APPENDIX_DIR, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# 2. HELPER FUNCTIONS (Imported from main script)
# ==============================================================================

# Advanced regex builder to handle special characters safely
build_regex_pattern <- function(word_list){
  if(length(word_list) == 0) return("(?!x)x")
  escaped <- str_replace_all(word_list, "([\\.^$|()\\[\\]{}*+?\\\\-])", "\\\\\\1")
  paste0("\\b(", paste(escaped, collapse="|"), ")\\b")
}

# Text preparation matching the main analysis exactly
prepare_ads <- function(df_raw){
  df_raw %>%
    distinct(Ad_Base_ID, .keep_all = TRUE) %>%
    mutate(
      Source_Type = "Newspaper",
      text = CleanedText %>% replace_na("") %>% str_to_lower() %>%
        str_replace_all("[.!?]+", " zzbreakzz ") %>% 
        str_replace_all("[^a-z\\s]", " ") %>%         
        str_replace_all("\\s+", " ") %>%              
        str_trim()
    ) %>%
    filter(text != "")
}

# ==============================================================================
# 3. DATA LOADING & PREPARATION
# ==============================================================================

# Load main ads dataset and coding file
df_raw <- readRDS(NEWSPAPER_INPUT)
coding_df <- read_excel(CODING_FILE_INPUT)

# Merge, clean text, and set up modeling variables matching main script
results_df <- df_raw %>%
  left_join(
    coding_df %>% dplyr::select(Ad_Base_ID, Is_International),
    by = "Ad_Base_ID"
  ) %>%
  prepare_ads() %>%
  mutate(
    # Use Dictionary_Words from main script for offset to ensure exact alignment
    log_valid_words = log(Dictionary_Words + 1),
    # Ensure Is_International matches character/factor type expected during model training
    Is_International = as.character(Is_International),
    Region = factor(
      ifelse(
        Is_International == "1" | Is_International == 1,
        "Global South",
        "Global North"
      ),
      levels = c("Global North", "Global South")
    )
  )

# ==============================================================================
# 4. DICTIONARIES
# ==============================================================================

vuln_dict <- unique(c(
  "helpless", "inability", "powerless", "vulnerable", "dependency", "unable", "suffer", "fear",
  "suffering", "struggling", "poor", "weak", "lack", "desperate", "struggle",
  "afraid", "shame", "unwilling", "harm", "hurt", "impossible", "desperately", "danger",
  "ill", "rely", "affect", "failure", "relying", "victim", "struggles", "abuse",
  "overwhelming", "losing", "abused", "dangers", "dependencies",
  "failures", "feared", "fearing", "fears", "harmed", "harming", "harms", "hurting", "hurts",
  "inabilities", "lacked", "lacking", "lacks", "poorer", "poorest", "relied",
  "relies", "shamed", "shames", "shaming", "struggled", "suffered", "sufferings", "suffers",
  "unfortunates", "victims", "weaker", "weakest"  
))

charity_donor_agency_dict <- unique(c(
  "help", "support", "provide", "rescue", "fund", "donate", "helping", "bring", "providing", "give",
  "aid", "provided", "assistance", "giving", "raise", "care", "helped", "supporting", "bringing",
  "assist", "offer", "offered", "save", "given", "funds", "funding", "contribute", "helps", "aided",
  "aiding", "aids", "assisted", "assisting", "assists", "brings", "brought", "contributed",
  "contributes", "contributing", "donated", "donates", "donating",
  "funded", "fundings", "gave", "givens", "gives", "offering", "offers", "provides",
  "raised", "raises", "raising", "rescued", "rescues", "rescuing", "saved", "saves", "saving",
  "supported", "supports" 
))

# ==============================================================================
# 5. KWIC EXPORT (Keyword in Context)
# ==============================================================================

corp <- corpus(results_df, text_field = "text")
toks <- tokens(corp) 

kwic_export <- function(dictionary, dict_name, window = 5, examples = 10) {
  
  kw <- kwic(
    toks, 
    pattern = dictionary,
    valuetype = "fixed",
    window = window
  )
  
  kw_df <- as.data.frame(kw)
  
  # Random sample if there are too many hits
  if(nrow(kw_df) > examples){
    set.seed(2026)
    kw_df <- kw_df %>% slice_sample(n = examples) 
  }
  
  # === ADDED: Print to Console ===
  cat(paste("\n=========================================\n"))
  cat(paste("KWIC Examples:", dict_name, "\n"))
  cat(paste("=========================================\n"))
  print(as_tibble(kw_df)) # Using as_tibble for cleaner console output
  cat("\n")
  
  # Print to LaTeX file
  print(
    xtable(kw_df, caption = paste("Keyword-in-Context Examples:", str_replace_all(dict_name, "_", " "))),
    file = file.path(APPENDIX_DIR, paste0(dict_name, "_KWIC.tex")),
    include.rownames = FALSE,
    sanitize.text.function = identity
  )
  
  return(kw_df)
}

# Run the KWIC exports
kwic_vulnerability <- kwic_export(vuln_dict, "Vulnerability")
kwic_charity <- kwic_export(charity_donor_agency_dict, "Charity_Donor_Agency")


# ==============================================================================
# 6. DYNAMIC FREQUENT WORD REMOVAL
# ==============================================================================

# Create the tokenized dataframe required by find_top_words
token_df <- results_df %>% unnest_tokens(word, text)

# Automate finding top words dynamically based on actual corpus frequencies
find_top_words <- function(dictionary, n = 5) {
  token_df %>%
    filter(word %in% dictionary) %>%
    count(word, sort = TRUE) %>%
    slice_head(n = n) %>%
    pull(word)
}

# Dynamically pick the top 5 words
top_vulnerability <- find_top_words(vuln_dict, n = 5)
top_charity <- find_top_words(charity_donor_agency_dict, n = 5)

cat(paste("\n=========================================\n"))
cat("Dictionary Reductions\n")
cat(paste("=========================================\n"))
cat("Removed Vulnerability words:", paste(top_vulnerability, collapse = ", "), "\n")
cat("Removed Charity words:", paste(top_charity, collapse = ", "), "\n\n")

# Reduce dictionaries
vuln_dict_reduced <- setdiff(vuln_dict, top_vulnerability)
charity_dict_reduced <- setdiff(charity_donor_agency_dict, top_charity)


# ==============================================================================
# 7. SENSITIVITY ANALYSIS MODELS & EXPORT
# ==============================================================================

results_sensitivity <- results_df %>%
  mutate(
    vuln_hits = stri_count_regex(
      text,
      build_regex_pattern(vuln_dict_reduced),
      opts_regex = stri_opts_regex(case_insensitive = TRUE)
    ),
    
    charity_donor_agency_hits = stri_count_regex(
      text,
      build_regex_pattern(charity_dict_reduced),
      opts_regex = stri_opts_regex(case_insensitive = TRUE)
    )
  )

# Negative Binomial Sensitivity Models
model_vuln_sensitivity <- glm.nb(
  vuln_hits ~ Is_International + NoiseRatio + offset(log_valid_words),
  data = results_sensitivity
)

model_charity_sensitivity <- glm.nb(
  charity_donor_agency_hits ~ Is_International + NoiseRatio + offset(log_valid_words),
  data = results_sensitivity
)

coef_map <- c(
  "(Intercept)" = "Intercept",
  "Is_International1" = "Global South",
  "Is_International" = "Global South",
  "NoiseRatio" = "Noise Ratio"
)

# === ADDED: Print beautifully formatted table directly to console ===
cat(paste("\n=========================================\n"))
cat("Sensitivity Regression Results (IRR)\n")
cat(paste("=========================================\n"))
modelsummary(
  list(
    "Vulnerability" = model_vuln_sensitivity,
    "Charity/Donor Agency" = model_charity_sensitivity
  ),
  vcov = "robust",
  exponentiate = TRUE,
  coef_map = coef_map,
  estimate = "{estimate}{stars}",
  statistic = "({conf.low}, {conf.high})",
  gof_map = c("nobs", "aic", "bic", "logLik"),
  output = "markdown" # This renders the table directly in the console
)

# Export robust sensitivity models identically to main analysis (LaTeX File)
modelsummary(
  list(
    "Vulnerability (Reduced Dict)" = model_vuln_sensitivity,
    "Charity/Donor Agency (Reduced Dict)" = model_charity_sensitivity
  ),
  vcov = "robust",
  exponentiate = TRUE,
  coef_map = coef_map,
  estimate = "{estimate}{stars}",
  statistic = "({conf.low}, {conf.high})",
  gof_map = c("nobs", "aic", "bic", "logLik"),
  title = "Sensitivity Analysis: NB Models (Top 5 Frequency Dictionary Words Removed)",
  notes = list("Notes: Estimates are Incidence Rate Ratios (IRR). 95% Confidence Intervals in parentheses based on robust standard errors."),
  output = file.path(APPENDIX_DIR, "Sensitivity_Dictionary_Reduction_Models.tex"),
  escape = FALSE
)

# === ADDED: Standard summaries for deep-dive in console ===
cat(paste("\n=========================================\n"))
cat("Detailed Model Summary: Vulnerability (Reduced Dict)\n")
cat(paste("=========================================\n"))
print(summary(model_vuln_sensitivity))

cat(paste("\n=========================================\n"))
cat("Detailed Model Summary: Charity/Donor Agency (Reduced Dict)\n")
cat(paste("=========================================\n"))
print(summary(model_charity_sensitivity))

cat("\nSensitivity scripts complete! LaTeX outputs saved to:", APPENDIX_DIR, "\n")

##################################################################################
coverage_ads <- results_sensitivity %>%
  summarise(
    Advertisements = n(),
    Ads_with_Vulnerability = sum(vuln_hits > 0),
    Ads_with_Charity = sum(charity_donor_agency_hits > 0),
    Vulnerability_Percent = round(100 * Ads_with_Vulnerability / Advertisements, 1),
    Charity_Percent = round(100 * Ads_with_Charity / Advertisements, 1)
  )

coverage_ads