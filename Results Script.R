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

NEWSPAPER_INPUT <- "C:/Users/unity/Documents/Github/Dissertation/Final_Cleaned_Newspaper_Ads.rds"
CODING_FILE_INPUT <- "C:/Users/unity/Documents/Github/Dissertation/Advertisement Coding.xlsx"

EXPORT_DIR <- "C:/Users/unity/Documents/Github/Dissertation/"
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

# Helper: Compute true robust Confidence Intervals for exponentiated coefficients (IRR)
extract_robust_irr <- function(model){
  # Use HC3 robust standard errors matching default modelsummary("robust")
  robust_cov <- sandwich::vcovHC(model, type = "HC3")
  est <- coef(model)
  se <- sqrt(diag(robust_cov))
  
  # Calculate robust CI (Wald)
  ci_low <- est - 1.96 * se
  ci_high <- est + 1.96 * se
  
  df <- data.frame(
    IRR = exp(est),
    `2.5 %` = exp(ci_low),
    `97.5 %` = exp(ci_high),
    check.names = FALSE
  )
  return(df)
}

# ==============================================================================
# 4. CORPUS PROXIMITY CALCULATION & DATA PREP
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

results_df <- df %>%
  mutate(
    vuln_hits = stri_count_regex(text, build_regex_pattern(vuln_dict)),
    charity_donor_agency_hits = stri_count_regex(text, build_regex_pattern(charity_donor_agency_dict)),
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

model_vuln <- glm.nb(vuln_hits ~ Is_International + NoiseRatio + offset(log_valid_words), data = results_df)
model_charity_agency <- glm.nb(charity_donor_agency_hits ~ Is_International + NoiseRatio + offset(log_valid_words), data = results_df)

coef_map <- c(
  "(Intercept)" = "Intercept",
  "Is_International1" = "Global South",
  "Is_International" = "Global South",
  "NoiseRatio" = "Noise Ratio"
)

modelsummary(
  list(
    "Vulnerability" = model_vuln,
    "Charity/Donor Agency" = model_charity_agency
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

process_word_freq(token_df, vuln_dict, "Vulnerability", PRIMARY_DIR)
process_word_freq(token_df, charity_donor_agency_dict, "Charity_Donor_Agency", PRIMARY_DIR)

# ==============================================================================
# 6. APPENDIX ANALYSIS
# ==============================================================================

# Extract correctly calculated Robust Standard Error IRR & CIs
irr_vuln <- extract_robust_irr(model_vuln)
print(xtable(irr_vuln, caption = "IRR Vulnerability (Robust CIs)"), file = file.path(APPENDIX_DIR, "IRR_Vulnerability.tex"), include.rownames = TRUE)

irr_charity <- extract_robust_irr(model_charity_agency)
print(xtable(irr_charity, caption = "IRR Charity/Donor Agency (Robust CIs)"), file = file.path(APPENDIX_DIR, "IRR_Charity_Donor_Agency.tex"), include.rownames = TRUE)

model_vuln_no_noise <- glm.nb(vuln_hits ~ Is_International + offset(log_valid_words), data = results_df)
model_charity_no_noise <- glm.nb(charity_donor_agency_hits ~ Is_International + offset(log_valid_words), data = results_df)

modelsummary(
  list(
    "Vulnerability (No Noise)" = model_vuln_no_noise,
    "Charity Agency (No Noise)" = model_charity_no_noise
  ),
  vcov = "robust",
  exponentiate = TRUE,
  coef_map = coef_map,
  statistic = "conf.int",
  stars = TRUE,
  output = file.path(APPENDIX_DIR, "Robustness_Models_No_Noise.tex") 
)

cor_data <- results_df %>% dplyr::select(vuln_hits, charity_donor_agency_hits)
spearman_matrix <- cor(cor_data, method = "spearman", use = "complete.obs")
colnames(spearman_matrix) <- c("Vulnerability", "Charity/Donor Agency")
rownames(spearman_matrix) <- c("Vulnerability", "Charity/Donor Agency")
print(xtable(spearman_matrix, caption = "Spearman Correlation Matrix"), file = file.path(APPENDIX_DIR, "Spearman_Correlation_Matrix.tex"), include.rownames = TRUE, scalebox = 0.85)

model_vuln_zi <- zeroinfl(vuln_hits ~ Is_International + NoiseRatio + offset(log_valid_words), data = results_df)
model_charity_zi <- zeroinfl(charity_donor_agency_hits ~ Is_International + NoiseRatio + offset(log_valid_words), data = results_df)

aic_vuln <- AIC(model_vuln, model_vuln_zi)
aic_charity <- AIC(model_charity_agency, model_charity_zi)

zi_aic_comparison <- tibble(
  Model = c("Vulnerability (NB)", "Vulnerability (ZI)", 
            "Charity/Donor Agency (NB)", "Charity/Donor Agency (ZI)"),
  df = c(aic_vuln$df, aic_charity$df),
  AIC = c(aic_vuln$AIC, aic_charity$AIC)
)

# Text wrap the long model names to avoid overflow
xt_zi <- xtable(zi_aic_comparison, caption = "AIC Comparison: Standard Negative Binomial vs. Zero-Inflated Models")
align(xt_zi) <- c("l", "p{8cm}", "c", "c") 
print(xt_zi, file = file.path(APPENDIX_DIR, "Zero_Inflated_AIC_Comparison.tex"), include.rownames = FALSE)

vuln_breakdown <- process_region_breakdown(token_df, vuln_dict, "Vulnerability", APPENDIX_DIR)
charity_breakdown <- process_region_breakdown(token_df, charity_donor_agency_dict, "Charity_Donor_Agency", APPENDIX_DIR)

# ==============================================================================
# TABLE 1: DESCRIPTIVE STATISTICS
# ==============================================================================

descriptive_table <- results_df %>%
  mutate(Region = as.character(Region)) %>%
  group_by(Region) %>%
  summarise(
    Advertisements = n(),
    `Words (Mean ± SD)` = sprintf("%.1f ± %.1f", mean(Dictionary_Words, na.rm = TRUE), sd(Dictionary_Words, na.rm = TRUE)),
    `Words (Median)` = median(Dictionary_Words, na.rm = TRUE),
    `Noise (Mean ± SD)` = sprintf("%.3f ± %.3f", mean(NoiseRatio, na.rm = TRUE), sd(NoiseRatio, na.rm = TRUE)),
    `Vulnerability (Mean ± SD)` = sprintf("%.2f ± %.2f", mean(vuln_hits, na.rm = TRUE), sd(vuln_hits, na.rm = TRUE)),
    `Charity (Mean ± SD)` = sprintf("%.2f ± %.2f", mean(charity_donor_agency_hits, na.rm = TRUE), sd(charity_donor_agency_hits, na.rm = TRUE))
  )

# By combining Mean and SD into a single column format, the column count drops from 12 to 8, making it fit nicely in LaTeX
xt_desc <- xtable(descriptive_table, caption = "Descriptive Statistics")
print(xt_desc, file = file.path(PRIMARY_DIR, "Table1_Descriptive_Statistics.tex"), include.rownames = FALSE, scalebox = 0.85)

# ==============================================================================
# MODEL DIAGNOSTICS & OVERDISPERSION
# ==============================================================================

diagnostics <- tibble(
  Model = c("Vulnerability", "Charity/Donor Agency"),
  AIC = c(AIC(model_vuln), AIC(model_charity_agency)),
  BIC = c(BIC(model_vuln), BIC(model_charity_agency)),
  LogLik = c(as.numeric(logLik(model_vuln)), as.numeric(logLik(model_charity_agency))),
  `Pseudo R2` = c(
    pscl::pR2(model_vuln)["McFadden"],
    pscl::pR2(model_charity_agency)["McFadden"]
  )
)

print(xtable(diagnostics, caption = "Model Diagnostics"), 
      file = file.path(APPENDIX_DIR, "Model_Diagnostics.tex"), include.rownames = FALSE, scalebox = 0.9)

dispersion <- tibble(
  Model = c("Vulnerability", "Charity/Donor Agency"),
  Theta = c(model_vuln$theta, model_charity_agency$theta)
)

print(xtable(dispersion, caption = "Model Dispersion (Theta)"), 
      file = file.path(APPENDIX_DIR, "Dispersion_Theta.tex"), include.rownames = FALSE)

# ==============================================================================
# POISSON VS NEGATIVE BINOMIAL MODEL COMPARISON
# ==============================================================================

pois_vuln <- glm(vuln_hits ~ Is_International + NoiseRatio + offset(log_valid_words), family = poisson, data = results_df)
pois_charity <- glm(charity_donor_agency_hits ~ Is_International + NoiseRatio + offset(log_valid_words), family = poisson, data = results_df)

print(xtable(broom::tidy(lmtest::lrtest(pois_vuln, model_vuln)), caption = "LRT: Vulnerability"), 
      file = file.path(APPENDIX_DIR, "LRT_Vulnerability.tex"), include.rownames = FALSE)
print(xtable(broom::tidy(lmtest::lrtest(pois_charity, model_charity_agency)), caption = "LRT: Charity/Donor Agency"), 
      file = file.path(APPENDIX_DIR, "LRT_Charity_Donor_Agency.tex"), include.rownames = FALSE)

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
    vcov = "robust", # Enforces robust SEs for confidence intervals in marginal plots
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
  extract_predictions(model_vuln, "Vulnerability"),
  extract_predictions(model_charity_agency, "Charity/Donor Agency")
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

marginal_vuln_plot <- plot_marginal_effects(marginal_table, "Vulnerability")
marginal_charity_plot <- plot_marginal_effects(marginal_table, "Charity/Donor Agency")

combined_marginal_plot <- marginal_vuln_plot | marginal_charity_plot
combined_marginal_plot <- combined_marginal_plot + plot_annotation(title = "Predicted Thematic Language by Advertisement Region")

ggsave(file.path(PRIMARY_DIR, "Combined_Marginal_Effects.pdf"), combined_marginal_plot, width = 12, height = 5, device = "pdf")
ggsave(file.path(PRIMARY_DIR, "Marginal_Effects_Vulnerability.pdf"), marginal_vuln_plot, width = 5, height = 5, device = "pdf")
ggsave(file.path(PRIMARY_DIR, "Marginal_Effects_Charity_Donor_Agency.pdf"), marginal_charity_plot, width = 5, height = 5, device = "pdf")

pearson_dispersion <- function(model){
  sum(residuals(model, type="pearson")^2) /
    df.residual(model)
}

dispersion_diagnostics <- tibble(
  Model = c(
    "Vulnerability",
    "Charity/Donor Agency"
  ),
  Pearson_Dispersion = c(
    pearson_dispersion(model_vuln),
    pearson_dispersion(model_charity_agency)
  )
)

print(
  xtable(dispersion_diagnostics, caption="Pearson Dispersion Diagnostics"),
  file = file.path(APPENDIX_DIR, "Pearson_Dispersion_Diagnostics.tex"),
  include.rownames = FALSE
)

vif(
  lm(
    vuln_hits ~ Is_International + NoiseRatio,
    data = results_df
  )
)

vif(
  lm(
    charity_donor_agency_hits ~ Is_International + NoiseRatio,
    data = results_df
  )
)