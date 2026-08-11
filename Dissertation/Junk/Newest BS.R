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
})

NEWSPAPER_INPUT <- "C:/Users/unity/Documents/Dissertation/Final_Cleaned_Newspaper_Ads.rds"
CODING_FILE_INPUT <- "C:/Users/unity/Documents/Dissertation/Advertisement Coding.xlsx"

EXPORT_DIR <- "C:/Users/unity/Documents/Dissertation/Outputs/Results_Section/"
PRIMARY_DIR <- file.path(EXPORT_DIR, "results")
APPENDIX_DIR <- file.path(EXPORT_DIR, "appendix")

dir.create(PRIMARY_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(APPENDIX_DIR, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# 2. DATA LOADING & MERGING WITH CODING FILE
# ==============================================================================

# Load main ads dataset
df_raw <- readRDS(NEWSPAPER_INPUT)

# Load your coding file
coding_df <- read_excel(CODING_FILE_INPUT)

# Merge Is_International from the coding file using Ad_Base_ID
df <- df_raw %>%
  left_join(
    coding_df %>% dplyr::select(Ad_Base_ID, Is_International),
    by = "Ad_Base_ID"
  )

# ==============================================================================
# 3. HELPER FUNCTIONS
# ==============================================================================

build_regex_pattern <- function(word_list){
  if(length(word_list) == 0) return("(?!x)x")
  escaped <- str_replace_all(word_list, "([\\.^$|()\\[\\]{}*+?\\\\-])", "\\\\\\1")
  paste0("\\b(", paste(escaped, collapse="|"), ")\\b")
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

df <- prepare_ads(df)

# Helper: Process Word Frequencies and Plot Top 30
process_word_freq <- function(token_df, dict, dict_name, export_dir) {
  freq_df <- token_df %>%
    filter(word %in% dict) %>%
    count(word, sort = TRUE)
  
  xt <- xtable(freq_df, caption = paste("Word Frequencies:", str_replace_all(dict_name, "_", " ")))
  print(xt, file = file.path(export_dir, paste0(dict_name, "_Word_Frequencies.tex")), 
        include.rownames = FALSE, scalebox = 0.9)
  
  p <- freq_df %>%
    slice_max(n, n = 30) %>%
    ggplot(aes(reorder(word, n), n)) +
    geom_col(fill = "#2c3e50") +
    coord_flip() +
    theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0),
      axis.title = element_text(face = "bold"),
      panel.grid.major.x = element_line(color = "#ecf0f1", linewidth = 0.5)
    ) +
    labs(
      title = paste("Top 30 Most Frequent Words:", str_replace_all(dict_name, "_", " ")),
      x = NULL, y = "Corpus Frequency (Hit Count)"
    )
  
  ggsave(file.path(export_dir, paste0(dict_name, "_Top30.pdf")), plot = p, width = 8, height = 6, device = "pdf")
}

# Helper: Process Global South vs North Breakdown
process_region_breakdown <- function(token_df, dict, dict_name, export_dir){
  
  breakdown_df <- token_df %>%
    filter(word %in% dict) %>%
    count(word, Region, name = "Frequency") %>%
    tidyr::pivot_wider(
      names_from = Region,
      values_from = Frequency,
      values_fill = list(Frequency = 0)
    )
  
  # Ensure both region columns exist
  if(!"Global South" %in% names(breakdown_df)){
    breakdown_df$`Global South` <- 0
  }
  
  if(!"Global North" %in% names(breakdown_df)){
    breakdown_df$`Global North` <- 0
  }
  
  breakdown_df <- breakdown_df %>%
    mutate(
      Total = `Global South` + `Global North`,
      `South Proportion` = round(`Global South` / pmax(Total, 1), 3),
      `North Proportion` = round(`Global North` / pmax(Total, 1), 3)
    ) %>%
    arrange(desc(Total))
  
  xt <- xtable(breakdown_df, caption = paste("Region Breakdown:", str_replace_all(dict_name, "_", " ")))
  print(xt, file = file.path(export_dir, paste0(dict_name, "_Region_Breakdown.tex")), 
        include.rownames = FALSE, scalebox = 0.85)
  
  return(breakdown_df)
}

# ==============================================================================
# 4. CORPUS PROXIMITY CALCULATION & DATA PREP
# ==============================================================================

struct_vuln_dict <- unique(c(
  "poverty", "poor", "hunger", "hungry", "famine", "malnutrition", 
  "starvation", "drought", "flood", "disaster", "conflict", "war", 
  "violence", "crisis", "emergency", "disease", "illness", 
  "epidemic", "displacement", "refugee", "homeless", "hardship"
))

pers_vuln_dict <- unique(c(
  "helpless", "helplessness", "powerless", "hopeless", "desperate", 
  "desperation", "vulnerable", "vulnerability", "dependent", 
  "dependency", "unable", "defenceless", "defenseless", "abandoned", 
  "alone", "isolated", "suffering", "suffer", "struggling", 
  "struggle", "fear", "afraid", "victim", "victims", "grief", 
  "despair", "misery"
))

global_north_activity <- unique(c(
  "protect", "benevolence", "giver", "aid", "assistance", "rescuing", "heroic", "savior",
  "giving", "bring", "helping", "bringing", "give", "given", "help", "gives",
  "benefit", "brought", "care", "save", "caring", "mission", "aided", "aiding", "aids", "benefited",
  "benefiting", "benefits", "benefitted", "benefitting", "gave", "givers",
  "helped", "helps", "protected", "protecting", "protects", "saved", "saves", "saviors" 
))

results_df <- df %>%
  mutate(
    struct_vuln_hits = stri_count_regex(text, build_regex_pattern(struct_vuln_dict)),
    pers_vuln_hits = stri_count_regex(text, build_regex_pattern(pers_vuln_dict)),
    global_north_activity_hits = stri_count_regex(text, build_regex_pattern(global_north_activity)),
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
# 5. PRIMARY ANALYSIS
# ==============================================================================

model_struct_vuln <- glm.nb(struct_vuln_hits ~ Is_International + NoiseRatio + offset(log_valid_words), data = results_df)
model_pers_vuln <- glm.nb(pers_vuln_hits ~ Is_International + NoiseRatio + offset(log_valid_words), data = results_df)
model_north_activity <- glm.nb(global_north_activity_hits ~ Is_International + NoiseRatio + offset(log_valid_words), data = results_df)

coef_map <- c(
  "(Intercept)" = "Intercept",
  "Is_International1" = "Global South",
  "Is_International" = "Global South",
  "NoiseRatio" = "Noise Ratio"
)

modelsummary(
  list(
    "Structural Vulnerability" = model_struct_vuln,
    "Personal Vulnerability" = model_pers_vuln,
    "Global North Activity" = model_north_activity
  ),
  vcov = "robust",
  exponentiate = TRUE,
  coef_map = coef_map,
  estimate = "{estimate}{stars}",
  statistic = "({conf.low}, {conf.high})",
  gof_map = c("nobs", "aic", "bic", "logLik"),
  title = "Negative Binomial Regression Models Predicting Thematic Language",
  notes = list("Notes: Estimates are Incidence Rate Ratios (IRR). 95% Confidence Intervals in parentheses based on robust standard errors."),
  output = file.path(PRIMARY_DIR, "Primary_Regression_Table.tex"),
  escape = FALSE
)

token_df <- results_df %>%
  dplyr::select(Ad_Base_ID, Region, text) %>%
  unnest_tokens(word, text)

process_word_freq(token_df, struct_vuln_dict, "Structural_Vulnerability", PRIMARY_DIR)
process_word_freq(token_df, pers_vuln_dict, "Personal_Vulnerability", PRIMARY_DIR)
process_word_freq(token_df, global_north_activity, "Global_North_Activity", PRIMARY_DIR)

# ==============================================================================
# 6. APPENDIX ANALYSIS
# ==============================================================================

irr_struct <- as.data.frame(exp(cbind(IRR = coef(model_struct_vuln), confint(model_struct_vuln))))
print(xtable(irr_struct, caption = "IRR Structural Vulnerability"), file = file.path(APPENDIX_DIR, "IRR_Structural_Vulnerability.tex"), include.rownames = TRUE)

irr_pers <- as.data.frame(exp(cbind(IRR = coef(model_pers_vuln), confint(model_pers_vuln))))
print(xtable(irr_pers, caption = "IRR Personal Vulnerability"), file = file.path(APPENDIX_DIR, "IRR_Personal_Vulnerability.tex"), include.rownames = TRUE)

irr_north <- as.data.frame(exp(cbind(IRR = coef(model_north_activity), confint(model_north_activity))))
print(xtable(irr_north, caption = "IRR Global North Activity"), file = file.path(APPENDIX_DIR, "IRR_Global_North_Activity.tex"), include.rownames = TRUE)

model_struct_no_noise <- glm.nb(struct_vuln_hits ~ Is_International + offset(log_valid_words), data = results_df)
model_pers_no_noise <- glm.nb(pers_vuln_hits ~ Is_International + offset(log_valid_words), data = results_df)
model_north_no_noise <- glm.nb(global_north_activity_hits ~ Is_International + offset(log_valid_words), data = results_df)

modelsummary(
  list(
    "Struct Vuln (No Noise)" = model_struct_no_noise,
    "Pers Vuln (No Noise)" = model_pers_no_noise,
    "North Activity (No Noise)" = model_north_no_noise
  ),
  vcov = "robust",
  exponentiate = TRUE,
  coef_map = coef_map,
  statistic = "conf.int",
  stars = TRUE,
  output = file.path(APPENDIX_DIR, "Robustness_Models_No_Noise.tex") 
)

wilcox_struct <- broom::tidy(wilcox.test(struct_vuln_hits ~ Region, data = results_df))
print(xtable(wilcox_struct, caption = "Wilcoxon Rank Sum Test: Structural Vulnerability"), file = file.path(APPENDIX_DIR, "Wilcoxon_Structural_Vuln.tex"), include.rownames = FALSE)

wilcox_pers <- broom::tidy(wilcox.test(pers_vuln_hits ~ Region, data = results_df))
print(xtable(wilcox_pers, caption = "Wilcoxon Rank Sum Test: Personal Vulnerability"), file = file.path(APPENDIX_DIR, "Wilcoxon_Personal_Vuln.tex"), include.rownames = FALSE)

wilcox_north <- broom::tidy(wilcox.test(global_north_activity_hits ~ Region, data = results_df))
print(xtable(wilcox_north, caption = "Wilcoxon Rank Sum Test: Global North Activity"), file = file.path(APPENDIX_DIR, "Wilcoxon_Global_North_Activity.tex"), include.rownames = FALSE)

cor_data <- results_df %>% dplyr::select(struct_vuln_hits, pers_vuln_hits, global_north_activity_hits)
spearman_matrix <- cor(cor_data, method = "spearman", use = "complete.obs")
colnames(spearman_matrix) <- c("Structural Vulnerability", "Personal Vulnerability", "Global North Activity")
rownames(spearman_matrix) <- c("Structural Vulnerability", "Personal Vulnerability", "Global North Activity")
print(xtable(spearman_matrix, caption = "Spearman Correlation Matrix"), file = file.path(APPENDIX_DIR, "Spearman_Correlation_Matrix.tex"), include.rownames = TRUE, scalebox = 0.85)

model_struct_zi <- zeroinfl(struct_vuln_hits ~ Is_International + NoiseRatio + offset(log_valid_words), data = results_df)
model_pers_zi <- zeroinfl(pers_vuln_hits ~ Is_International + NoiseRatio + offset(log_valid_words), data = results_df)
model_north_zi <- zeroinfl(global_north_activity_hits ~ Is_International + NoiseRatio + offset(log_valid_words), data = results_df)

aic_struct <- AIC(model_struct_vuln, model_struct_zi)
aic_pers <- AIC(model_pers_vuln, model_pers_zi)
aic_north <- AIC(model_north_activity, model_north_zi)

zi_aic_comparison <- tibble(
  Model = c("Structural Vulnerability (NB)", "Structural Vulnerability (ZI)", 
            "Personal Vulnerability (NB)", "Personal Vulnerability (ZI)", 
            "Global North Activity (NB)", "Global North Activity (ZI)"),
  df = c(aic_struct$df, aic_pers$df, aic_north$df),
  AIC = c(aic_struct$AIC, aic_pers$AIC, aic_north$AIC)
)

# Text wrap the long model names to avoid overflow
xt_zi <- xtable(zi_aic_comparison, caption = "AIC Comparison: Standard Negative Binomial vs. Zero-Inflated Models")
align(xt_zi) <- c("l", "p{8cm}", "c", "c") 
print(xt_zi, file = file.path(APPENDIX_DIR, "Zero_Inflated_AIC_Comparison.tex"), include.rownames = FALSE)

struct_breakdown <- process_region_breakdown(token_df, struct_vuln_dict, "Structural_Vulnerability", APPENDIX_DIR)
pers_breakdown <- process_region_breakdown(token_df, pers_vuln_dict, "Personal_Vulnerability", APPENDIX_DIR)
north_breakdown <- process_region_breakdown(token_df, global_north_activity, "Global_North_Activity", APPENDIX_DIR)

# ==============================================================================
# TABLE 1: DESCRIPTIVE STATISTICS
# ==============================================================================

descriptive_table <- results_df %>%
  mutate(Region = as.character(Region)) %>%
  group_by(Region) %>%
  summarise(
    Advertisements = n(),
    `Mean Words` = round(mean(Dictionary_Words), 1),
    `SD Words` = round(sd(Dictionary_Words), 1),
    `Median Words` = median(Dictionary_Words),
    `Mean Noise` = round(mean(NoiseRatio), 3),
    `SD Noise` = round(sd(NoiseRatio), 3),
    `Mean Structural` = round(mean(struct_vuln_hits), 2),
    `SD Structural` = round(sd(struct_vuln_hits), 2),
    `Mean Personal` = round(mean(pers_vuln_hits), 2),
    `SD Personal` = round(sd(pers_vuln_hits), 2),
    `Mean Activity` = round(mean(global_north_activity_hits), 2),
    `SD Activity` = round(sd(global_north_activity_hits), 2)
  )

# This table has 12 columns and needs heavy scaling to fit onto a standard page
xt_desc <- xtable(descriptive_table, caption = "Descriptive Statistics")
print(xt_desc, file = file.path(PRIMARY_DIR, "Table1_Descriptive_Statistics.tex"), include.rownames = FALSE, scalebox = 0.65)

# ==============================================================================
# MODEL DIAGNOSTICS & OVERDISPERSION
# ==============================================================================

diagnostics <- tibble(
  Model = c("Structural Vulnerability", "Personal Vulnerability", "Global North Activity"),
  AIC = c(AIC(model_struct_vuln), AIC(model_pers_vuln), AIC(model_north_activity)),
  BIC = c(BIC(model_struct_vuln), BIC(model_pers_vuln), BIC(model_north_activity)),
  LogLik = c(as.numeric(logLik(model_struct_vuln)), as.numeric(logLik(model_pers_vuln)), as.numeric(logLik(model_north_activity))),
  `Pseudo R2` = c(
    pscl::pR2(model_struct_vuln)["McFadden"],
    pscl::pR2(model_pers_vuln)["McFadden"],
    pscl::pR2(model_north_activity)["McFadden"]
  )
)

print(xtable(diagnostics, caption = "Model Diagnostics"), 
      file = file.path(APPENDIX_DIR, "Model_Diagnostics.tex"), include.rownames = FALSE, scalebox = 0.9)

dispersion <- tibble(
  Model = c("Structural Vulnerability", "Personal Vulnerability", "Global North Activity"),
  Theta = c(model_struct_vuln$theta, model_pers_vuln$theta, model_north_activity$theta)
)

print(xtable(dispersion, caption = "Model Dispersion (Theta)"), 
      file = file.path(APPENDIX_DIR, "Dispersion_Theta.tex"), include.rownames = FALSE)

# ==============================================================================
# POISSON VS NEGATIVE BINOMIAL MODEL COMPARISON
# ==============================================================================

pois_struct <- glm(struct_vuln_hits ~ Is_International + NoiseRatio + offset(log_valid_words), family = poisson, data = results_df)
pois_pers <- glm(pers_vuln_hits ~ Is_International + NoiseRatio + offset(log_valid_words), family = poisson, data = results_df)
pois_activity <- glm(global_north_activity_hits ~ Is_International + NoiseRatio + offset(log_valid_words), family = poisson, data = results_df)

print(xtable(broom::tidy(lmtest::lrtest(pois_struct, model_struct_vuln)), caption = "LRT: Structural Vulnerability"), 
      file = file.path(APPENDIX_DIR, "LRT_Structural.tex"), include.rownames = FALSE)
print(xtable(broom::tidy(lmtest::lrtest(pois_pers, model_pers_vuln)), caption = "LRT: Personal Vulnerability"), 
      file = file.path(APPENDIX_DIR, "LRT_Personal.tex"), include.rownames = FALSE)
print(xtable(broom::tidy(lmtest::lrtest(pois_activity, model_north_activity)), caption = "LRT: Global North Activity"), 
      file = file.path(APPENDIX_DIR, "LRT_Global_North_Activity.tex"), include.rownames = FALSE)

# ==============================================================================
# SAMPLE FLOW TABLE
# ==============================================================================

sample_flow <- tibble(
  Stage = c(
    "Advertisements initially retrieved from newspaper database",
    "Advertisements after aggregation by advertisement ID",
    "Advertisements after non-human advertisement removal",
    "Advertisements retained after duplicate/repeat removal",
    "Final analytical sample"
  ),
  N = c(584, 362, 336, 81, 81)
)

# Use p{width} alignment to wrap the very long strings in the Stage column
xt_flow <- xtable(sample_flow, caption = "Sample Selection Flow for Newspaper Advertisement Corpus")
align(xt_flow) <- c("l", "p{12cm}", "c")
print(xt_flow, file = file.path(PRIMARY_DIR, "Sample_Flow_Table.tex"), include.rownames = FALSE)

# ==============================================================================
# MARGINAL EFFECTS PLOTS + TABLE
# ==============================================================================

extract_predictions <- function(model, outcome_name){
  prediction_data <- data.frame(
    Is_International = c("0", "1"),
    NoiseRatio = mean(results_df$NoiseRatio, na.rm = TRUE),
    log_valid_words = mean(results_df$log_valid_words, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  
  predictions(
    model,
    newdata = prediction_data,
    type = "response"
  ) %>%
    mutate(
      Outcome = outcome_name,
      Region = factor(
        ifelse(
          Is_International == "1",
          "Global South",
          "Global North"
        ),
        levels = c("Global North", "Global South")
      )
    ) %>%
    dplyr::select(
      Outcome,
      Region,
      estimate,
      conf.low,
      conf.high
    )
}

marginal_table <- bind_rows(
  extract_predictions(model_struct_vuln, "Structural Vulnerability"),
  extract_predictions(model_pers_vuln, "Personal Vulnerability"),
  extract_predictions(model_north_activity, "Global North Activity")
)

print(xtable(marginal_table, caption = "Predicted Thematic Language Counts by Advertisement Region"),
      file = file.path(PRIMARY_DIR, "Marginal_Effects_Table.tex"), include.rownames = FALSE, scalebox = 0.9)

plot_marginal_effects <- function(prediction_df, outcome_name){
  prediction_df %>%
    filter(Outcome == outcome_name) %>%
    ggplot(aes(x = Region, y = estimate)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .15, linewidth = .8) +
    theme_classic(base_size = 14) +
    labs(
      title = outcome_name,
      x = NULL,
      y = "Predicted keyword count"
    )
}

marginal_struct_plot <- plot_marginal_effects(marginal_table, "Structural Vulnerability")
marginal_personal_plot <- plot_marginal_effects(marginal_table, "Personal Vulnerability")
marginal_activity_plot <- plot_marginal_effects(marginal_table, "Global North Activity")

combined_marginal_plot <- marginal_struct_plot | marginal_personal_plot | marginal_activity_plot
combined_marginal_plot <- combined_marginal_plot + plot_annotation(title = "Predicted Thematic Language by Advertisement Region")

ggsave(file.path(PRIMARY_DIR, "Combined_Marginal_Effects.pdf"), combined_marginal_plot, width = 12, height = 5, device = "pdf")
ggsave(file.path(PRIMARY_DIR, "Marginal_Effects_Structural_Vulnerability.pdf"), marginal_struct_plot, width = 5, height = 5, device = "pdf")
ggsave(file.path(PRIMARY_DIR, "Marginal_Effects_Personal_Vulnerability.pdf"), marginal_personal_plot, width = 5, height = 5, device = "pdf")
ggsave(file.path(PRIMARY_DIR, "Marginal_Effects_Global_North_Activity.pdf"), marginal_activity_plot, width = 5, height = 5, device = "pdf")

pearson_dispersion <- function(model){
  sum(residuals(model, type="pearson")^2) /
    df.residual(model)
}

dispersion_diagnostics <- tibble(
  Model = c(
    "Structural Vulnerability",
    "Personal Vulnerability",
    "Donor/Charity Agency"
  ),
  Pearson_Dispersion = c(
    pearson_dispersion(model_struct_vuln),
    pearson_dispersion(model_pers_vuln),
    pearson_dispersion(model_north_activity)
  )
)

print(
  xtable(dispersion_diagnostics, caption="Pearson Dispersion Diagnostics"),
  file = file.path(APPENDIX_DIR, "Pearson_Dispersion_Diagnostics.tex"),
  include.rownames = FALSE
)

vif(
  lm(
    pers_vuln_hits ~ Is_International + NoiseRatio,
    data = results_df
  )
)

vif(
  lm(
    global_north_activity_hits ~ Is_International + NoiseRatio,
    data = results_df
  )
)