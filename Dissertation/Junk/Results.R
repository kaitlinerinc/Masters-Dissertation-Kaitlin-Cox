setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
set.seed(2026)

# ==============================================================================
# 1. LIBRARIES & CONFIGURATION
# ==============================================================================
library(dplyr)
library(stringr)
library(tidyr)
library(text2vec)
library(data.table)
library(readr)
library(stringi)
library(readxl)
library(purrr)
library(stopwords)
library(tibble)
library(lexicon)
library(MASS)
library(sandwich)
library(lmtest)
library(car)
library(ggplot2)
library(modelsummary)
library(broom)
library(pscl)
library(tidytext)

NEWSPAPER_INPUT <- "C:/Users/unity/Documents/Dissertation/Final_Cleaned_Newspaper_Ads.rds"
CODING_FILE <- "C:/Users/unity/Documents/Dissertation/Advertisement Coding.xlsx"
EMBEDDING_PATH <- "dolma_300_2024_1.2M.100_combined.txt"
EXPORT_DIR <- "C:/Users/unity/Documents/Dissertation/Outputs/Results_Section/"

dir.create(EXPORT_DIR, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# 2. HELPER FUNCTIONS
# ==============================================================================
build_regex_pattern <- function(word_list){
  if(length(word_list) == 0) return("(?!x)x")
  escaped <- str_replace_all(word_list, "([\\.^$|()\\[\\]{}*+?\\\\-])", "\\\\\\1")
  paste0("\\b(", paste(escaped, collapse="|"), ")\\b")
}

print_dictionary_report <- function(dictionary, name, data){
  corpus_words <- unique(unlist(str_split(data$text, "\\s+")))
  found <- dictionary[dictionary %in% corpus_words]
  cat("\n---------------------------------\nDICTIONARY:", toupper(name), "\nTotal words:", length(dictionary), "\nFound in corpus:", length(found), "\n")
  cat(paste(str_wrap(paste(dictionary, collapse=", "), width=80), collapse="\n"), "\n")
}

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
# 3. LOAD DATA & EMBEDDINGS
# ==============================================================================
df <- prepare_ads(readRDS(NEWSPAPER_INPUT)) %>%
  left_join(read_excel(CODING_FILE) %>% dplyr::select(Ad_Base_ID, Is_International, Is_Animal), by="Ad_Base_ID") %>%
  filter(Is_Animal == 0, !is.na(Is_International))

# --- FULL DICTIONARIES ---
vulnerability_full <- c("helplessness", "hopelessness", "poverty", "suffering", "victim", "powerless", "depend",
                        "fear", "suffer", "despair", "shame", "misery", "struggle", "grief", "loneliness", "lack",
                        "inability", "struggling", "poor", "desperation", "unable", "mental", "alone", "helpless",
                        "danger", "death", "struggles", "desperate", "burden", "illness", "affected", "harm", "neglect",
                        "ill", "abuse", "violence", "forced", "vulnerable", "depression", "failure", "victims",
                        "fears", "disappointment", "insecurity", "faced", "hurt", "absence", "pity", "suffers", "dying",
                        "hopeless", "pain", "difficult", "suffered", "afraid", "impossible", "affects", "horrible",
                        "hardship", "anxiety", "miserable", "physical", "conflict", "inflicted", "anguish", "affect",
                        "escape", "losing", "plight", "absences", "abused", "abuses", "abusing", "affecting", "anxieties",
                        "burdened", "burdening", "burdens", "dangers", "deaths", "depended", "depending", "depends",
                        "depressions", "despaired", "despairing", "despairs", "disappointments", "failures", "griefs", "hardships", "harmed", "harming", "harms", "hurting", "hurts", "iller",
                        "illest", "illnesses", "ills", "inabilities", "insecurities", "lacked", "lacking", "lacks",
                        "miseries", "neglected", "neglecting", "neglects", "pained", "paining", "pains", "pitied",
                        "pities", "pitying", "plighted", "plighting", "plights", "poorer", "poorest", "struggled", "sufferings", "vulnerabler", "vulnerablest")

agency_full <- c("protect", "benevolence", "giver", "aid", "assistance", "rescuing", "heroic", "savior", "donate",
                 "gift", "giving", "give", "helping", "save", "help", "given", "saving", "receive", "benefit",
                 "gives", "care", "raise", "gave", "saved", "support", "grateful", "caring", "helped", "serve",
                 "mission", "money", "providing", "offered", "provided", "cares", "raising", "supporting",
                 "thankful", "gifts", "provide", "assist", "appreciate", "donation", "rescue", "protecting",
                 "granted", "serves", "appreciation", "grant", "contribute", "donations", "deliver", "hero",
                 "offer", "benefits", "gratitude", "action", "resources", "offering", "volunteer", "benefited",
                 "benefiting", "benefitted", "benefitting", "cared", "contributed", "contributes",
                 "contributing", "delivered", "delivering", "delivers", "donated", "donates", "donating",
                 "gifted", "gifting", "givers", "granting", "grants", "offerings", "offers", "protected",
                 "protects", "provides", "raises", "rescued", "rescues", "saves", "saviors", "served", "serving",
                 "supported", "supports", "volunteered", "volunteering", "volunteers")

# --- FINAL/TIGHTENED DICTIONARIES ---
vulnerability_final <- c(
  # Dependency / Powerlessness
  "helpless", "hopeless", "hopelessness", "helplessness", "powerless", "inability", "unable", 
  "depend", "depends", "depending", "depended", "dependent", "dependency",
  
  # Poverty / Deprivation
  "poverty", "poor", "poorest", "lack", "lacking", "lacks", "hardship", "hardships",
  
  # Suffering / Despair
  "suffer", "suffers", "suffering", "suffered", "struggle", "struggling", "struggles", "struggled", 
  "desperate", "desperation", "despair", "despaired", "despairing", "despairs",
  
  # Threat / Harm / Abuse
  "danger", "dangers", "vulnerable", "violence", "conflict", "abuse", "abused", "abuses", "abusing", 
  "neglect", "neglects", "neglected", "neglecting", "harm", "harmed", "harms", "harming",
  
  # Severe Humanitarian Vulnerability
  "illness", "death", "deaths", "dying", "forced", "plight", "plights", "victim", "victims", 
  "insecurity", "insecurities", 
  
  # Saviorism/Hierarchical Framing
  "pity", "pitied", "pities", "pitying"
)


agency_final <- c(
  "donate", "donated", "donates", "donating", "donation", "donations",
  "give", "gave", "given", "giving", "gifts", "gifted", "gifting", "giver", "givers",
  "save", "saved", "saving", "saves", "protect", "protected", "protecting", "protects", "rescue", "rescued", "help", "helps", "helping",
  "provide", "provided", "providing", "provides", "deliver", "delivered", "delivering", "delivers", "assist", "assistance",
  "support", "supported", "supporting", "supports", "serve", "served", "serving",
  "raise", "raising", "resources", "contribute", "contributed", "contributes", "volunteer", "volunteering", "volunteers",
  "heroic", "hero", "savior", "saviors"
)

# Ensure unique and sorted
vulnerability_full <- sort(unique(vulnerability_full))
agency_full <- sort(unique(agency_full))
vulnerability_final <- sort(unique(vulnerability_final))
agency_final <- sort(unique(agency_final))

# ==============================================================================
# 4. CORPUS PROXIMITY CALCULATION & DATA PREP
# ==============================================================================

results_df <- df %>%
  mutate(
    # Hits for FULL dictionaries
    vuln_full_hits = stri_count_regex(text, build_regex_pattern(vulnerability_full)),
    agency_full_hits = stri_count_regex(text, build_regex_pattern(agency_full)),
    
    # Hits for TIGHTENED dictionaries
    vuln_final_hits = stri_count_regex(text, build_regex_pattern(vulnerability_final)),
    agency_final_hits = stri_count_regex(text, build_regex_pattern(agency_final)),
    
    # Baseline vars
    log_valid_words = log(Dictionary_Words + 1),
    Region = ifelse(Is_International == 1, "Global South", "Global North")
  )

results_df$Is_International <- as.numeric(results_df$Is_International)

# ===============================
# MAIN MODELS
# ===============================

# 1. Models based on FULL dictionaries
model_vuln_full <- glm.nb(vuln_full_hits ~ Is_International + NoiseRatio + offset(log_valid_words), data = results_df)
model_charity_full <- glm.nb(agency_full_hits ~ Is_International + NoiseRatio + offset(log_valid_words), data = results_df)
model_int_full <- glm.nb(agency_full_hits ~ vuln_full_hits * Is_International + NoiseRatio + offset(log_valid_words), data = results_df)

# 2. Models based on TIGHTENED dictionaries
model_vuln_final <- glm.nb(vuln_final_hits ~ Is_International + NoiseRatio + offset(log_valid_words), data = results_df)
model_charity_final <- glm.nb(agency_final_hits ~ Is_International + NoiseRatio + offset(log_valid_words), data = results_df)
model_int_final <- glm.nb(agency_final_hits ~ vuln_final_hits * Is_International + NoiseRatio + offset(log_valid_words), data = results_df)


# ===============================
# IRRs EXPORT
# ===============================

# Function to extract and export IRRs
export_irr <- function(model, filename) {
  res <- as.data.frame(exp(cbind(IRR = coef(model), confint(model))))
  write.csv(res, file.path(EXPORT_DIR, filename))
}

export_irr(model_vuln_full, "IRR_Vulnerability_Full.csv")
export_irr(model_vuln_final, "IRR_Vulnerability_Tightened.csv")
export_irr(model_charity_full, "IRR_Charity_Full.csv")
export_irr(model_charity_final, "IRR_Charity_Tightened.csv")
export_irr(model_int_full, "IRR_Interaction_Full.csv")
export_irr(model_int_final, "IRR_Interaction_Tightened.csv")

# ===============================
# MODEL TABLE (SIDE-BY-SIDE COMPARISON)
# ===============================

# Map both variations to the same row names for clean side-by-side comparison
coef_map <- c(
  "(Intercept)"="Intercept",
  "Is_International1"="Global South",
  "Is_International"="Global South",
  "NoiseRatio"="Noise Ratio",
  "vuln_full_hits"="Vulnerability",
  "vuln_final_hits"="Vulnerability",
  "vuln_full_hits:Is_International1"="Vulnerability × Global South",
  "vuln_final_hits:Is_International1"="Vulnerability × Global South",
  "vuln_full_hits:Is_International"="Vulnerability × Global South",
  "vuln_final_hits:Is_International"="Vulnerability × Global South"
)

modelsummary(
  list(
    "Vuln (Full)" = model_vuln_full,
    "Vuln (Tight)" = model_vuln_final,
    "Agency (Full)" = model_charity_full,
    "Agency (Tight)" = model_charity_final,
    "Interact (Full)" = model_int_full,
    "Interact (Tight)" = model_int_final
  ),
  exponentiate = TRUE,
  coef_map = coef_map,
  statistic = "conf.int",
  stars = TRUE,
  output = file.path(EXPORT_DIR, "Regression_Comparison_Table.docx")
)

# ===============================
# WILCOXON & SPEARMAN TESTS
# ===============================

# FULL
write.csv(broom::tidy(wilcox.test(vuln_full_hits~Region, data=results_df)), file.path(EXPORT_DIR, "Wilcoxon_Vuln_Full.csv"), row.names=FALSE)
write.csv(broom::tidy(wilcox.test(agency_full_hits~Region, data=results_df)), file.path(EXPORT_DIR, "Wilcoxon_Agency_Full.csv"), row.names=FALSE)
write.csv(broom::tidy(cor.test(results_df$vuln_full_hits, results_df$agency_full_hits, method="spearman")), file.path(EXPORT_DIR, "Spearman_Full.csv"), row.names=FALSE)

# TIGHTENED
write.csv(broom::tidy(wilcox.test(vuln_final_hits~Region, data=results_df)), file.path(EXPORT_DIR, "Wilcoxon_Vuln_Tight.csv"), row.names=FALSE)
write.csv(broom::tidy(wilcox.test(agency_final_hits~Region, data=results_df)), file.path(EXPORT_DIR, "Wilcoxon_Agency_Tight.csv"), row.names=FALSE)
write.csv(broom::tidy(cor.test(results_df$vuln_final_hits, results_df$agency_final_hits, method="spearman")), file.path(EXPORT_DIR, "Spearman_Tight.csv"), row.names=FALSE)

# ===============================
# INTERACTION PLOTS
# ===============================
generate_interaction_plot <- function(model, vuln_var, title_text, filename) {
  
  # 1. Create the sequence of 100 values first
  vuln_seq <- seq(min(results_df[[vuln_var]], na.rm = TRUE), 
                  max(results_df[[vuln_var]], na.rm = TRUE), 
                  length.out = 100)
  
  # 2. Build the grid with a placeholder name
  newdat <- expand.grid(
    temp_vuln = vuln_seq,
    Is_International = c(0, 1),
    NoiseRatio = mean(results_df$NoiseRatio, na.rm = TRUE),
    log_valid_words = mean(results_df$log_valid_words, na.rm = TRUE)
  )
  
  # 3. Rename the placeholder to match the dynamic variable name (e.g., "vuln_full_hits")
  names(newdat)[names(newdat) == "temp_vuln"] <- vuln_var
  
  # 4. Generate predictions
  newdat$Prediction <- predict(model, newdata = newdat, type = "response")
  newdat$Region <- factor(newdat$Is_International, labels = c("Global North", "Global South"))
  
  # 5. Plot
  p <- ggplot(newdat, aes_string(x = vuln_var, y = "Prediction", color = "Region")) +
    geom_line(linewidth = 1.2) +
    theme_minimal(base_size = 13) +
    labs(
      x = "Vulnerability dictionary hits",
      y = "Predicted charity agency hits",
      title = title_text
    )
  
  ggsave(file.path(EXPORT_DIR, filename), p, width = 8, height = 6, dpi = 300)
}

# Run the plot generation
generate_interaction_plot(model_int_full, "vuln_full_hits", "Interaction (Full Dictionaries)", "Interaction_Plot_Full.png")
generate_interaction_plot(model_int_final, "vuln_final_hits", "Interaction (Tightened Dictionaries)", "Interaction_Plot_Tightened.png")

# ===============================
# DIAGNOSTICS (ZERO-INFLATED COMPARISON)
# ===============================
model_vuln_full_zi <- zeroinfl(vuln_full_hits ~ Is_International + NoiseRatio + offset(log_valid_words), data=results_df)
model_charity_full_zi <- zeroinfl(agency_full_hits ~ Is_International + NoiseRatio + offset(log_valid_words), data=results_df)

model_vuln_final_zi <- zeroinfl(vuln_final_hits ~ Is_International + NoiseRatio + offset(log_valid_words), data=results_df)
model_charity_final_zi <- zeroinfl(agency_final_hits ~ Is_International + NoiseRatio + offset(log_valid_words), data=results_df)

cat("\n--- AIC COMPARISONS ---\n")
cat("Vulnerability Full (NB vs ZI):", AIC(model_vuln_full), "vs", AIC(model_vuln_full_zi), "\n")
cat("Vulnerability Tight (NB vs ZI):", AIC(model_vuln_final), "vs", AIC(model_vuln_final_zi), "\n")
cat("Charity Full (NB vs ZI):", AIC(model_charity_full), "vs", AIC(model_charity_full_zi), "\n")
cat("Charity Tight (NB vs ZI):", AIC(model_charity_final), "vs", AIC(model_charity_final_zi), "\n")

cat("\n--- PSEUDO R-SQUARED (McFadden) ---\n")
cat("Vuln Full:", pR2(model_vuln_full)["McFadden"], "| Vuln Tight:", pR2(model_vuln_final)["McFadden"], "\n")
cat("Agency Full:", pR2(model_charity_full)["McFadden"], "| Agency Tight:", pR2(model_charity_final)["McFadden"], "\n")
cat("Int Full:", pR2(model_int_full)["McFadden"], "| Int Tight:", pR2(model_int_final)["McFadden"], "\n")


# ==============================================================================
# DICTIONARY VALIDATION: WORD FREQUENCIES (TIGHTENED ONLY FOR PLOTS)
# ==============================================================================

# Tokenize corpus
token_df <- results_df %>%
  dplyr::select(Ad_Base_ID, Region, text) %>%
  unnest_tokens(word, text)

# Helper for frequencies
save_freq <- function(dictionary, filename) {
  freq <- token_df %>%
    filter(word %in% dictionary) %>%
    count(word, sort = TRUE)
  write.csv(freq, file.path(EXPORT_DIR, filename), row.names = FALSE)
  return(freq)
}

vuln_full_freq <- save_freq(vulnerability_full, "Vulnerability_Freq_Full.csv")
vuln_tight_freq <- save_freq(vulnerability_final, "Vulnerability_Freq_Tightened.csv")
agency_full_freq <- save_freq(agency_full, "Charity_Agency_Freq_Full.csv")
agency_tight_freq <- save_freq(agency_final, "Charity_Agency_Freq_Tightened.csv")

# Plots for Tightened (Final) Dictionaries
vuln_tight_freq %>%
  slice_max(n, n = 30) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Most Frequent Vulnerability Words (Tightened)", x = NULL, y = "Frequency")
ggsave(file.path(EXPORT_DIR, "Vulnerability_Top30_Tightened.png"), width = 8, height = 6, dpi = 300)

agency_tight_freq %>%
  slice_max(n, n = 30) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Most Frequent Charity Agency Words (Tightened)", x = NULL, y = "Frequency")
ggsave(file.path(EXPORT_DIR, "Charity_Agency_Top30_Tightened.png"), width = 8, height = 6, dpi = 300)

# ==============================================================================
# RANDOM DICTIONARY HIT CONTEXT CHECK
# ==============================================================================

print_random_dictionary_examples <- function(df, dictionary, dictionary_name, examples_per_word = 10, context_words = 5) {
  cat("\n\n=========================================================\n")
  cat("DICTIONARY:", dictionary_name, "\n")
  cat("=========================================================\n")
  
  for(target_word in dictionary) {
    examples <- list()
    for(i in seq_len(nrow(df))) {
      words <- str_split(df$text[i], "\\s+")[[1]]
      hits <- which(words == target_word)
      
      if(length(hits) > 0) {
        for(hit in hits) {
          start <- max(1, hit - context_words)
          end <- min(length(words), hit + context_words)
          examples[[length(examples)+1]] <- tibble(
            Ad_Base_ID = df$Ad_Base_ID[i],
            Word = target_word,
            Context = paste(words[start:end], collapse = " ")
          )
        }
      }
    }
    
    if(length(examples) == 0) next
    word_examples <- bind_rows(examples)
    
    if(nrow(word_examples) > examples_per_word) {
      word_examples <- word_examples[sample(seq_len(nrow(word_examples)), examples_per_word), ]
    }
    
    cat("\n\n---------------------------------------------------------\n")
    cat("WORD:", toupper(target_word), "\n")
    cat("TOTAL OCCURRENCES:", nrow(bind_rows(examples)), "\n")
    cat("---------------------------------------------------------\n")
    
    for(j in seq_len(nrow(word_examples))) {
      cat("\nAd:", word_examples$Ad_Base_ID[j], "\n")
      context <- word_examples$Context[j]
      context <- str_replace(context, paste0("\\b", target_word, "\\b"), paste0(">>> ", toupper(target_word), " <<<"))
      cat(context, "\n")
    }
  }
}

# Run Context Check for TIGHTENED dictionaries to prevent console overflow
print_random_dictionary_examples(results_df, agency_final, "CHARITY_AGENCY (TIGHTENED)", examples_per_word = 5)
print_random_dictionary_examples(results_df, vulnerability_final, "VULNERABILITY (TIGHTENED)", examples_per_word = 5)


length(vulnerability_final)
length(agency_final)

print_dictionary_report(
  vulnerability_final,
  "vulnerability",
  results_df
)

print_dictionary_report(
  agency_final,
  "agency",
  results_df
)