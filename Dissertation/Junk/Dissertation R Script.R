setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(quanteda)
library(readxl)
library(stringr)

#Data Cleaning

# 1. Load the data
charity_ads <- read_excel("Charity Ads.xlsx")

# 2. Define the Final Cleaning Function
clean_charity_tokens_final <- function(text_vector) {
  if (is.null(text_vector)) stop("Column not found.")
  text_vector[is.na(text_vector)] <- ""
  
  # --- Stage 1: Specific OCR & Noise Patterns ---
  # Remove URLs and Emails
  clean_text <- str_replace_all(text_vector, "[\\w\\.-]+@[\\w\\.-]+\\.[a-z]{2,}", " ")
  clean_text <- str_replace_all(clean_text, "www\\.\\S+|http\\S+", " ")
  
  # Remove OCR artifacts (e.g., "pl ll ll pp" or "р tzoz t26t")
  # Targets repetitive 1-2 character clusters or flipped/corrupted text
  clean_text <- str_replace_all(clean_text, "\\b[a-z]{1,2}(\\s+[a-z]{1,2}){2,}\\b", " ")
  
  # Remove alphanumeric OCR codes/IDs (e.g., RTKS-ZXCS-HSBT, RTLJ-ETCZ-SCCZ, 20LA20)
  clean_text <- str_replace_all(clean_text, "\\b([A-Z0-9]{4}-){2,}[A-Z0-9]{4}\\b", " ")
  clean_text <- str_replace_all(clean_text, "\\b[A-Z0-9]{2,}[0-9]{2,}[A-Z0-9]*\\b", " ")
  
  # Remove UK Postcodes (e.g., SL1 4PY, SW8 4AA)
  clean_text <- str_replace_all(clean_text, "\\b[A-Z]{1,2}\\d[A-Z\\d]?\\s*\\d[A-Z]{2}\\b", " ")
  
  # Remove Scottish/Irish Charity numbers (e.g., SC039426, CHY123)
  clean_text <- str_replace_all(clean_text, "(?i)\\b(sc|chy)\\d{4,7}\\b", " ")
  
  # --- Stage 2: Deduplication of Lines ---
  clean_text <- sapply(clean_text, function(x) {
    lines <- str_split(x, "\\r\\n|\\n")[[1]]
    unique_lines <- unique(str_trim(lines))
    paste(unique_lines, collapse = " ")
  }) %>% unname() %>% as.character()
  
  # --- Stage 3: Quanteda Tokenization ---
  chari_corpus <- corpus(clean_text)
  
  toks <- tokens(chari_corpus, 
                 remove_punct = TRUE, 
                 remove_symbols = TRUE, 
                 remove_numbers = TRUE) %>%
    tokens_tolower() %>%
    # Final 'Message' isolation: Remove administrative/coupon vocabulary
    tokens_remove(c("freepost", "charity", "registered", "road", "slough", 
                    "email", "website", "telephone", "numbers", "fr", "regulator",
                    "mr", "mrs", "ms", "name", "address", "detach", "form", "plus",
                    "postcode", "stamp", "required", "online", "call", "phone", 
                    "london", "england", "wales", "scotland", "ireland", "title",
                    "surname", "first", "last", "tick", "privacy", "policy", 
                    "fundraising", "back", "please", "just", "can", "us", "get")) %>%
    tokens_remove(stopwords("english"))
  
  return(as.character(sapply(toks, paste, collapse = " ")))
}

# 3. Execute Pipeline
unique_ads_final <- charity_ads %>%
  mutate(Cleaned_Message = clean_charity_tokens_final(`Uncleaned Text`)) %>%
  # Filter out very short results (meaningless OCR noise)
  filter(nchar(Cleaned_Message) > 30) %>% 
  # Group by the message to keep only one copy of each ad
  distinct(Cleaned_Message, .keep_all = TRUE) %>%
  mutate(Cleaned_Message = str_squish(Cleaned_Message))

# 4. Final Output Verification
print(paste("Original rows:", nrow(charity_ads)))
print(paste("Unique ads found:", nrow(unique_ads_final)))

# Display final cleaned messages
head(unique_ads_final$Cleaned_Message)

##################################################################################
#Dictionary and Embedding
glove_file <- "glove.6B.100d.txt" 

# Read the file and create a matrix
vectors <- read.table(glove_file, header = FALSE, sep = " ", quote = "", 
                      row.names = 1, stringsAsFactors = FALSE, fill = TRUE)
glove_matrix <- as.matrix(vectors)

# --- Step 2: Define Expand Function using Cosine Similarity ---
expand_with_glove <- function(seeds, matrix, top_n = 5) {
  # Filter seeds that exist in GloVe vocabulary
  valid_seeds <- seeds[seeds %in% rownames(matrix)]
  
  if(length(valid_seeds) == 0) return(seeds)
  
  # Calculate average vector for the seed words
  seed_vec <- colMeans(matrix[valid_seeds, , drop = FALSE])
  
  # Calculate cosine similarity between seed vector and all words in GloVe
  cos_sim <- sim2(x = matrix, y = matrix(seed_vec, nrow = 1), method = "cosine", norm = "l2")
  
  # Get the top N most similar words
  similar_words <- head(sort(cos_sim[,1], decreasing = TRUE), top_n + length(valid_seeds))
  return(unique(names(similar_words)))
}

# --- Step 3: Expand Your Specific Categories ---
initial_seeds <- list(
  vulnerability = c("helpless", "victim", "vulnerable"),
  empowerment = c("leader", "strong", "survivor"),
  physical_needs = c("hunger", "cold", "thirst")
)

# Expand each list
expanded_lists <- lapply(initial_seeds, expand_with_glove, matrix = glove_matrix, top_n = 10)

# Print check: see how GloVe expanded "Deprivation"
print(expanded_lists$deprivation) 
####################################################################################
get_density <- function(text, word_list) {
  if (is.na(text) || text == "") return(0)
  tokens <- unlist(strsplit(tolower(text), "\\W+"))
  if (length(tokens) == 0) return(0)
  
  hits <- sum(tokens %in% word_list)
  return((hits / length(tokens)) * 100)
}

# Calculate density for each ad
final_analysis <- unique_ads_final %>%
  mutate(
    Vuln_Density = map_dbl(Cleaned_Message, ~get_density(.x, expanded_lists$vulnerability)),
    Stren_Density = map_dbl(Cleaned_Message, ~get_density(.x, expanded_lists$strength)),
    Depr_Density = map_dbl(Cleaned_Message, ~get_density(.x, expanded_lists$deprivation))
  )
############################################################################################
plot_data <- final_analysis %>%
  pivot_longer(cols = ends_with("_Density"), names_to = "Category", values_to = "Score")

ggplot(plot_data, aes(x = `Domestic/International`, y = Score, fill = `Domestic/International`)) +
  geom_boxplot() +
  facet_grid(Topic ~ Category) + # Compare categories across your Topics
  theme_minimal() +
  labs(title = "Language Density Comparison",
       subtitle = "Domestic vs. International Charity Ads",
       y = "Keyword Density (%)")
#########################################################################################
library(udpipe)
library(tidyverse)

# Download and load the English model
dl <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(dl$file_model)

# Define our Actor Groups
actors <- list(
  Charity_Donor = c("you", "we", "us", "our", "donor", "supporter", "volunteer"),
  Recipient = c("they", "them", "community", "child", "children", "animal", "pet", "patient")
)

# 2. Annotate the Text
# This takes the cleaned text and identifies the grammatical role of every word
annotation <- udpipe_annotate(ud_model, x = unique_ads_final$Cleaned_Message, 
                              doc_id = as.numeric(unique_ads_final$Identifier)) %>% 
  as.data.frame()

voice_analysis <- annotation %>%
  mutate(
    # Normalize words for matching
    doc_id = as.numeric(doc_id),
    lemma = tolower(lemma),
    
    # Identify the Actor
    Actor_Group = case_when(
      lemma %in% actors$Charity_Donor ~ "Charity/Donor",
      lemma %in% actors$Recipient     ~ "Recipient",
      TRUE ~ NA_character_
    ),
    
    # Identify the Voice based on Dependency Relation (dep_rel)
    Voice = case_when(
      dep_rel == "nsubj" ~ "Active (Subject)",
      dep_rel %in% c("obj", "obl", "iobj") ~ "Passive (Object/Receiver)",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Actor_Group) & !is.na(Voice))

# Merge back with your original Topic and Scope metadata
final_voice_df <- voice_analysis %>%
  inner_join(unique_ads_final, by = c("doc_id" = "Identifier"))

voice_summary <- final_voice_df %>%
  group_by(Subject, `International/Domestic`, Actor_Group, Voice) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Visualization
ggplot(voice_summary, aes(x = Actor_Group, y = percentage, fill = Voice)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(`International/Domestic` ~ Subject) +
  theme_minimal() +
  labs(
    title = "Voice Analysis: Who is the 'Doer' vs. the 'Receiver'?",
    subtitle = "Comparing Charity/Donor and Recipient roles by Topic and Scope",
    y = "Percentage of Grammatical Occurrences (%)",
    x = "Actor"
  )