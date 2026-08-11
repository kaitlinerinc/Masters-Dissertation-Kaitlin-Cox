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
APPENDIX_DIR <- file.path(EXPORT_DIR, "appendix")

dir.create(APPENDIX_DIR, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# 2. DICTIONARIES & HELPER FUNCTIONS
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

build_regex_pattern <- function(word_list){
  if(length(word_list) == 0) return("(?!x)x")
  escaped <- str_replace_all(word_list, "([\\.^$|()\\[\\]{}*+?\\\\-])", "\\\\\\1")
  paste0("\\b(", paste(escaped, collapse="|"), ")\\b")
}

prepare_ads <- function(df_raw, unique_campaigns = TRUE){
  if(unique_campaigns) {
    df_raw <- df_raw %>% distinct(Ad_Base_ID, .keep_all = TRUE)
  }
  
  df_raw %>%
    mutate(
      Source_Type = "Newspaper",
      text = CleanedText %>% replace_na("") %>% str_to_lower() %>%
        str_replace_all("[.!?]+", " zzbreakzz ") %>% 
        str_replace_all("[^a-z\\s]", " ") %>%         
        str_replace_all("\\s+", " ") %>%             
        str_trim(),
      vuln_hits = stri_count_regex(text, build_regex_pattern(vuln_dict)),
      charity_donor_agency_hits = stri_count_regex(text, build_regex_pattern(charity_donor_agency_dict)),
      log_valid_words = log(Dictionary_Words + 1),
      Is_International = as.character(Is_International),
      Region = factor(
        ifelse(Is_International == "1" | Is_International == 1, "Global South", "Global North"),
        levels = c("Global North", "Global South")
      )
    ) %>%
    filter(text != "")
}

extract_robust_irr <- function(model){
  robust_cov <- sandwich::vcovHC(model, type = "HC3")
  est <- coef(model)
  se <- sqrt(diag(robust_cov))
  ci_low <- est - 1.96 * se
  ci_high <- est + 1.96 * se
  
  data.frame(
    IRR = exp(est),
    `2.5 %` = exp(ci_low),
    `97.5 %` = exp(ci_high),
    check.names = FALSE
  )
}

# ==============================================================================
# 3. DATA LOADING, MERGING & DATASETS CREATION
# ==============================================================================

df_raw <- readRDS(NEWSPAPER_INPUT)
coding_df <- read_excel(CODING_FILE_INPUT)

# Merge Is_International from coding file
df_merged <- df_raw %>%
  left_join(
    coding_df %>% dplyr::select(Ad_Base_ID, Is_International),
    by = "Ad_Base_ID"
  )

# Create analytical datasets
results_unique <- prepare_ads(df_merged, unique_campaigns = TRUE)

# Generate tokens for the unique dataset
token_unique <- results_unique %>%
  dplyr::select(Ad_Base_ID, Region, text) %>%
  unnest_tokens(word, text)

# ==============================================================================
# 4. ESSENTIAL MODELS FOR APPENDIX DIAGNOSTICS
# ==============================================================================

model_vuln_unique <- glm.nb(vuln_hits ~ Is_International + NoiseRatio + offset(log_valid_words), data = results_unique)
model_charity_unique <- glm.nb(charity_donor_agency_hits ~ Is_International + NoiseRatio + offset(log_valid_words), data = results_unique)

coef_map <- c(
  "(Intercept)" = "Intercept",
  "Is_International1" = "Global South",
  "Is_International" = "Global South",
  "NoiseRatio" = "Noise Ratio"
)

# ==============================================================================
# 5. APPENDIX SECTION OUTPUTS (Appendix Dir)
# ==============================================================================

# --- Appendix A: Dictionary Development ---
dict_df_vuln <- data.frame(Terms = paste(vuln_dict, collapse = ", "))
print(xtable(dict_df_vuln, caption = "Table A1: Vulnerability Dictionary"), 
      file = file.path(APPENDIX_DIR, "Table_A1_Vulnerability_Dictionary.tex"), include.rownames = FALSE)

dict_df_charity <- data.frame(Terms = paste(charity_donor_agency_dict, collapse = ", "))
print(xtable(dict_df_charity, caption = "Table A2: Charity/Donor Agency Dictionary"), 
      file = file.path(APPENDIX_DIR, "Table_A2_Charity_Dictionary.tex"), include.rownames = FALSE)


# --- Appendix B: Word Frequency Tables ---
generate_freq_table <- function(token_df, dict, caption_text, filename) {
  freq_df <- token_df %>% filter(word %in% dict) %>% count(word, sort = TRUE)
  print(xtable(freq_df, caption = caption_text), 
        file = file.path(APPENDIX_DIR, filename), include.rownames = FALSE, scalebox = 0.9)
}
generate_freq_table(token_unique, vuln_dict, "Table B1: Vulnerability Word Frequencies", "Table_B1_Vuln_Freq.tex")
generate_freq_table(token_unique, charity_donor_agency_dict, "Table B2: Charity/Donor Agency Word Frequencies", "Table_B2_Charity_Freq.tex")


# --- Appendix C: Regional Word Breakdown ---
generate_region_breakdown <- function(token_df, dict, caption_text, filename){
  breakdown_df <- token_df %>%
    filter(word %in% dict) %>% count(word, Region, name = "Frequency") %>%
    tidyr::pivot_wider(names_from = Region, values_from = Frequency, values_fill = list(Frequency = 0))
  
  if(!"Global South" %in% names(breakdown_df)) breakdown_df$`Global South` <- 0
  if(!"Global North" %in% names(breakdown_df)) breakdown_df$`Global North` <- 0
  
  breakdown_df <- breakdown_df %>%
    mutate(Total = `Global South` + `Global North`,
           `South Proportion` = round(`Global South` / pmax(Total, 1), 3)) %>%
    arrange(desc(Total)) %>%
    dplyr::select(Word = word, `Global North`, `Global South`, `South Proportion`)
  
  print(xtable(breakdown_df, caption = caption_text), 
        file = file.path(APPENDIX_DIR, filename), include.rownames = FALSE, scalebox = 0.85)
}
generate_region_breakdown(token_unique, vuln_dict, "Table C1: Vulnerability words by region", "Table_C1_Vuln_Region_Breakdown.tex")
generate_region_breakdown(token_unique, charity_donor_agency_dict, "Table C2: Charity/donor agency words by region", "Table_C2_Charity_Region_Breakdown.tex")


# --- Appendix D: Regression Robustness ---
irr_vuln <- extract_robust_irr(model_vuln_unique)
irr_charity <- extract_robust_irr(model_charity_unique)
combined_irr <- bind_rows(irr_vuln %>% mutate(Model = "Vulnerability"), irr_charity %>% mutate(Model = "Charity/Agency"))
print(xtable(combined_irr, caption = "Table D1: Robust IRRs and Confidence Intervals"), 
      file = file.path(APPENDIX_DIR, "Table_D1_Robust_IRRs.tex"), include.rownames = TRUE)

model_vuln_no_noise <- glm.nb(vuln_hits ~ Is_International + offset(log_valid_words), data = results_unique)
model_charity_no_noise <- glm.nb(charity_donor_agency_hits ~ Is_International + offset(log_valid_words), data = results_unique)
modelsummary(
  list("Vuln (No Noise)" = model_vuln_no_noise, "Charity (No Noise)" = model_charity_no_noise),
  vcov = "robust", exponentiate = TRUE, coef_map = coef_map, statistic = "conf.int", stars = TRUE,
  title = "Table D2: Models excluding OCR Noise Ratio",
  output = file.path(APPENDIX_DIR, "Table_D2_Robustness_No_Noise.tex")
)


# --- Appendix E: Model Diagnostics ---
diag_df <- tibble(
  Model = c("Vulnerability", "Charity/Donor Agency"),
  AIC = c(AIC(model_vuln_unique), AIC(model_charity_unique)),
  BIC = c(BIC(model_vuln_unique), BIC(model_charity_unique)),
  LogLik = c(as.numeric(logLik(model_vuln_unique)), as.numeric(logLik(model_charity_unique))),
  `McFadden R2` = c(pscl::pR2(model_vuln_unique)["McFadden"], pscl::pR2(model_charity_unique)["McFadden"])
)
print(xtable(diag_df, caption = "Table E1: Model diagnostics"), file = file.path(APPENDIX_DIR, "Table_E1_Model_Diagnostics.tex"), include.rownames = FALSE)

disp_df <- tibble(Model = c("Vulnerability", "Charity/Donor Agency"), Theta = c(model_vuln_unique$theta, model_charity_unique$theta))
print(xtable(disp_df, caption = "Table E2: Dispersion (theta)"), file = file.path(APPENDIX_DIR, "Table_E2_Dispersion_Theta.tex"), include.rownames = FALSE)

pearson_df <- tibble(
  Model = c("Vulnerability", "Charity/Donor Agency"),
  Pearson_Dispersion = c(sum(residuals(model_vuln_unique, type="pearson")^2)/df.residual(model_vuln_unique),
                         sum(residuals(model_charity_unique, type="pearson")^2)/df.residual(model_charity_unique))
)
print(xtable(pearson_df, caption="Table E3: Pearson dispersion diagnostics"), file = file.path(APPENDIX_DIR, "Table_E3_Pearson_Diagnostics.tex"), include.rownames = FALSE)

pois_vuln <- glm(vuln_hits ~ Is_International + NoiseRatio + offset(log_valid_words), family = poisson, data = results_unique)
pois_charity <- glm(charity_donor_agency_hits ~ Is_International + NoiseRatio + offset(log_valid_words), family = poisson, data = results_unique)
print(xtable(broom::tidy(lmtest::lrtest(pois_vuln, model_vuln_unique)), caption = "Table E4: LRT Vulnerability (Poisson vs NB)"), file = file.path(APPENDIX_DIR, "Table_E4_LRT_Vuln.tex"), include.rownames = FALSE)
print(xtable(broom::tidy(lmtest::lrtest(pois_charity, model_charity_unique)), caption = "Table E4: LRT Charity/Donor Agency (Poisson vs NB)"), file = file.path(APPENDIX_DIR, "Table_E4_LRT_Charity.tex"), include.rownames = FALSE)

model_vuln_zi <- zeroinfl(vuln_hits ~ Is_International + NoiseRatio + offset(log_valid_words), data = results_unique)
model_charity_zi <- zeroinfl(charity_donor_agency_hits ~ Is_International + NoiseRatio + offset(log_valid_words), data = results_unique)
zi_aic <- tibble(
  Model = c("Vuln (NB)", "Vuln (ZI)", "Charity (NB)", "Charity (ZI)"),
  df = c(AIC(model_vuln_unique, model_vuln_zi)$df, AIC(model_charity_unique, model_charity_zi)$df),
  AIC = c(AIC(model_vuln_unique, model_vuln_zi)$AIC, AIC(model_charity_unique, model_charity_zi)$AIC)
)
print(xtable(zi_aic, caption = "Table E5: Zero-inflated vs. standard negative binomial AIC comparison"), file = file.path(APPENDIX_DIR, "Table_E5_Zero_Inflated_AIC.tex"), include.rownames = FALSE)


# --- Appendix F: Correlation and Collinearity ---
cor_matrix <- cor(results_unique %>% dplyr::select(vuln_hits, charity_donor_agency_hits), method = "spearman", use = "complete.obs")
colnames(cor_matrix) <- rownames(cor_matrix) <- c("Vulnerability", "Charity/Donor Agency")
print(xtable(cor_matrix, caption = "Table F1: Spearman correlation matrix"), file = file.path(APPENDIX_DIR, "Table_F1_Spearman_Correlation.tex"), include.rownames = TRUE)

vif_vuln <- vif(lm(vuln_hits ~ Is_International + NoiseRatio, data = results_unique))
vif_charity <- vif(lm(charity_donor_agency_hits ~ Is_International + NoiseRatio, data = results_unique))
vif_df <- data.frame(
  Variable = names(vif_vuln),
  `Vulnerability VIF` = as.numeric(vif_vuln),
  `Charity/Agency VIF` = as.numeric(vif_charity),
  check.names = FALSE
)
print(xtable(vif_df, caption = "Table F2: Variance Inflation Factors (VIF)"), file = file.path(APPENDIX_DIR, "Table_F2_VIF.tex"), include.rownames = FALSE)