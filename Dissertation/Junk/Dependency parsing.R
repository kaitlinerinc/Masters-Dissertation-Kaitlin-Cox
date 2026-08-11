# ==============================================================================
# 1. SETUP & LIBRARIES
# ==============================================================================
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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
library(udpipe)

# Paths
NEWSPAPER_INPUT <- "C:/Users/unity/Documents/Dissertation/Newspaper_Ads.rds"
MAILOUT_INPUT   <- "C:/Users/unity/Documents/Dissertation/Mailout_Ads.rds"
MODEL_FILE      <- "english-ewt-ud-2.5-191206.udpipe"

# ==============================================================================
# 2. HELPER FUNCTIONS
# ==============================================================================
prepare_ads <- function(df_raw, source_label){
  df_raw %>%
    distinct(Ad_Base_ID, .keep_all = TRUE) %>%
    mutate(
      Source_Type = source_label,
      text = CombinedAdText %>% str_to_lower() %>% str_replace_all("[[:space:]]+", " ") %>% str_trim(),
      total_tokens = str_count(text, "\\S+")
    ) %>%
    filter(!is.na(text), text != "")
}

# ==============================================================================
# 3. DATA LOADING
# ==============================================================================
df <- bind_rows(prepare_ads(readRDS(NEWSPAPER_INPUT), "Newspaper"), 
                prepare_ads(readRDS(MAILOUT_INPUT), "Mailout"))

coded_df <- read_excel("C:/Users/unity/Documents/Dissertation/Advertisement Coding.xlsx") 
df <- df %>% 
  left_join(coded_df %>% select(Ad_Base_ID, Is_International, Is_Animal), by = "Ad_Base_ID") %>%
  filter(Is_Animal == 0, !is.na(Is_International))

# ==============================================================================
# 4. TEXT PARSING (DEPENDENCY ANALYSIS)
# ==============================================================================
if (!file.exists(MODEL_FILE)) udpipe_download_model(language = "english-ewt")
ud_model <- udpipe_load_model(MODEL_FILE)

# Run annotation (This step may take a few minutes)
annotations <- udpipe_annotate(ud_model, x = df$text, doc_id = df$Ad_Base_ID)
parsed      <- as.data.frame(annotations) %>% mutate(across(c(lemma, token, dep_rel), tolower))

# ==============================================================================
# 5. FRAMING ANALYSIS
# ==============================================================================
charity_entities     <- c("charity","organization","ngo","donor","supporter","volunteer","staff","we","us","our","you","your")
beneficiary_entities <- c("community","communities","child","children","family","families","people","person","they","them",
                          "their","victim","women","girls","survivor", "i")

charity_agency_verbs     <- c("help","save","provide","deliver","aid","support","protect","transform")
beneficiary_agency_verbs <- c("build","teach","organize","lead","rebuild","work","create","participate","advocate","develop","empower")
dependency_verbs         <- c("need","depend","rely","require","wait")
suffering_verbs          <- c("suffer","starve","struggle","endure","die","lack")

verbs <- parsed %>% filter(upos %in% c("VERB","AUX")) %>% select(doc_id, sentence_id, token_id, verb_lemma = lemma)
arguments <- parsed %>% filter(dep_rel %in% c("nsubj","obj","iobj","nsubj:pass")) %>% select(doc_id, sentence_id, token_id, head_token_id, lemma)

relations <- arguments %>%
  inner_join(verbs, by = c("doc_id","sentence_id","head_token_id" = "token_id")) %>%
  mutate(
    entity_type = case_when(
      lemma %in% charity_entities ~ "Charity",
      lemma %in% beneficiary_entities ~ "Beneficiary",
      TRUE ~ NA_character_
    ),
    frame_type = case_when(
      verb_lemma %in% charity_agency_verbs ~ "CharityAgency",
      verb_lemma %in% suffering_verbs ~ "Suffering",
      verb_lemma %in% dependency_verbs ~ "BeneficiaryAgency",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(entity_type), !is.na(frame_type))

frame_wide <- relations %>%
  group_by(doc_id, entity_type, frame_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = c(entity_type, frame_type), values_from = n, values_fill = 0)

# Ensure all columns exist
required_cols <- c("Charity_CharityAgency", "Beneficiary_CharityAgency", "Beneficiary_Suffering", "Beneficiary_BeneficiaryAgency")
for(col in required_cols) if(!(col %in% colnames(frame_wide))) frame_wide[[col]] <- 0

frame_wide <- frame_wide %>%
  mutate(
    Charity_Agency_Score = Charity_CharityAgency / (Charity_CharityAgency + Beneficiary_CharityAgency + 1e-6),
    Beneficiary_Dependency_Score = Beneficiary_Suffering / (Beneficiary_Suffering + Beneficiary_BeneficiaryAgency + 1e-6),
    Beneficiary_Agency_Score = Beneficiary_BeneficiaryAgency / (Beneficiary_Suffering + Beneficiary_BeneficiaryAgency + 1e-6)
  )

analysis_df <- df %>%
  left_join(frame_wide, by = c("Ad_Base_ID" = "doc_id")) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))

# ==============================================================================
# 6. VALIDATION
# ==============================================================================
inspect_top_frames <- function(target_df, score_col, top_n = 5) {
  top_ads <- target_df %>% arrange(desc(!!sym(score_col))) %>% slice(1:top_n) %>% pull(Ad_Base_ID)
  cat("\n======================================================================\n")
  cat("  VALIDATING FRAME SCORE:", toupper(score_col), "\n")
  cat("======================================================================\n")
  
  for (ad_id in top_ads) {
    cat("\n--- Ad ID:", ad_id, "---\n")
    ad_sentences <- parsed %>% filter(doc_id == ad_id) %>% group_by(sentence_id) %>% 
      summarise(sentence = paste(token, collapse = " ")) %>% pull(sentence)
    cat(paste(str_wrap(ad_sentences[1:min(2, length(ad_sentences))], width = 70), collapse = "\n"), "\n")
  }
}

inspect_top_frames(analysis_df, "Charity_Agency_Score")
inspect_top_frames(analysis_df, "Beneficiary_Dependency_Score")
inspect_top_frames(analysis_df, "Beneficiary_Agency_Score")

# saveRDS(parsed, "parsed_text_data.rds") # Optional: save to avoid re-running udpipe