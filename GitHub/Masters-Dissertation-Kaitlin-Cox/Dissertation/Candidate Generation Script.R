getwd()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

set.seed(2026)

# Load Necessary Libraries

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

#Load Data and Embedding

NEWSPAPER_INPUT <- "C:/Users/unity/Documents/Github/Dissertation/Final_Cleaned_Newspaper_Ads.rds"

CODING_FILE <- "C:/Users/unity/Documents/Github/Dissertation/Advertisement Coding.xlsx"

EMBEDDING_PATH <- "dolma_300_2024_1.2M.100_combined.txt"


dir.create(EXPORT_DIR, showWarnings = FALSE, recursive = TRUE)



# Functions

#Formats words so they are searchable

build_regex_pattern <- function(word_list){
  
  if(length(word_list) == 0) return("(?!x)x")
  
  escaped <- str_replace_all(word_list, "([\\.^$|()\\[\\]{}*+?\\\\-])", "\\\\\\1")
  
  paste0("\\b(", paste(escaped, collapse="|"), ")\\b")
  
}


#Standardizes and cleans text

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

#Expands the dictionaries with selected thresholds

expand_with_threshold <- function(seeds, embedding_matrix, threshold, top_n = 250){
  
  valid_seeds <- seeds[seeds %in% rownames(embedding_matrix)]
  
  if(length(valid_seeds)==0){
    
    warning("No seed words found in embeddings")
    
    return(tibble(word = character(), similarity = numeric()))
    
  }
  
  seed_vector <- colMeans(embedding_matrix[valid_seeds, , drop=FALSE])
  
  similarities <- sim2(x = embedding_matrix, y = matrix(seed_vector, nrow=1), method="cosine", norm="l2")[,1]
  
  names(similarities) <- rownames(embedding_matrix)
  
  
  
  tibble(word = names(similarities), similarity = as.numeric(similarities)) %>%
    
    filter(str_detect(word, "^[a-z]{3,}$"), !(word %in% stopwords("en")), similarity >= threshold) %>%
    
    arrange(desc(similarity)) %>% slice_head(n=top_n)
  
}


#Prints a report of the dictionary results

print_dictionary_report <- function(dictionary, name, data){
  
  corpus_words <- unique(unlist(str_split(data$text, "\\s+")))
  
  found <- dictionary[dictionary %in% corpus_words]
  
  cat("\n---------------------------------\nDICTIONARY:", toupper(name), "\nTotal words:", length(dictionary), "\nFound in corpus:", length(found), "\n")
  
  cat(paste(str_wrap(paste(dictionary, collapse=", "), width=80), collapse="\n"), "\n")
  
}

#Expands the word to capture different grammatical forms

expand_with_lemmas <- function(dictionary){
  
  lemma_matches <- lexicon::hash_lemmas %>% filter(lemma %in% dictionary) %>% pull(token)
  
  unique(c(dictionary, lemma_matches))
  
}


#Prepare the dataset

df <- prepare_ads(readRDS(NEWSPAPER_INPUT)) %>%
  
  left_join(read_excel(CODING_FILE) %>% dplyr::select(Ad_Base_ID, Is_International, Is_Animal), by="Ad_Base_ID") %>%
  
  filter(Is_Animal == 0, !is.na(Is_International))



cat("\nAdvertisements retained:", nrow(df), "\n")


#Load embeddings

vectors <- fread(EMBEDDING_PATH, header=FALSE, sep=" ", fill=TRUE, quote="", colClasses=c("character", rep("numeric",300)))

embedding_matrix <- as.matrix(vectors[,-1])

rownames(embedding_matrix) <- vectors[[1]]

rm(vectors); gc()



# Set seed dictionaries

seed_dictionaries <- list(
  
  vulnerability = c("helpless", "inability", "powerless", "vulnerable", "dependency"),
  
  charity_agency = c("help", "support", "provide", "rescue", "fund", "donate")
  
)

#Expand with two different cosine similarity thresholds

vulnerability_raw <- expand_with_threshold(
  
  seeds = seed_dictionaries$vulnerability, 
  
  embedding_matrix = embedding_matrix, 
  
  threshold = 0.76, 
  
  top_n = 250
  
)



charity_raw <- expand_with_threshold(
  
  seeds = seed_dictionaries$charity_agency, 
  
  embedding_matrix = embedding_matrix, 
  
  threshold = 0.84, 
  
  top_n = 250
  
)


expanded_dictionaries <- list(
  
  vulnerability = unique(c(seed_dictionaries$vulnerability, vulnerability_raw$word)),
  
  charity_agency = unique(c(seed_dictionaries$charity_agency, charity_raw$word))
  
)


write_csv(vulnerability_raw, file.path(EXPORT_DIR, "vulnerability_similarity.csv"))

write_csv(charity_raw, file.path(EXPORT_DIR, "charity_agency_similarity.csv"))


#Exclude some of the unrelated vulnerability words (after manual determination)

unrelated_vulnerability <- c(
  
  "overcome", "aware", "otherwise", "situation", "dealing", "unaware", "consequence", "become",
  
  "realize", "rather", "others", "becomes", "difficult", "unfortunately", "knowing", "consequences",
  
  "self", "neither", "failing", "sense", "worse", "somehow", "often", "hence", "physically",
  
  "thus", "recognize", "despite", "extent", "problem", "therefore", "reason", "completely", "regardless",
  
  "cause", "without", "cope", "concerned", "concern", "fact", "blame", "however", "circumstances", "indeed",
  
  "fail", "secondly", "understand", "lives", "remain", "situations", "sometimes", "likely", "similarly",
  
  "entirely", "due", "spite", "seemingly", "problems", "longer", "ways", "realizing", "importantly", "nothing",
  
  "becoming", "people", "letting", "might", "inevitably", "means", "ignore", "simply", "acknowledge",
  
  "especially", "sadly", "still", "meant", "yet", "clearly", "needing", "realise", "reasons",
  
  "conversely", "terrible", "possible", "life", "seem", "happens", "mind", "necessarily", "reality",
  
  "even", "possibly", "either", "attention", "supposed", "imagine", "doubt", "perhaps", "result",
  
  "bad", "trying", "kind", "must", "matter", "way", "obvious", "none", "else", "essentially",
  
  "feel", "causes", "particularly", "person", "potentially", "obviously", "able", "trouble", "feeling",
  
  "rest", "seems", "enough", "causing", "caused", "equally", "consequently", "always", "mental", "ultimately",
  
  "wanting", "sort", "perceived", "ironically", "turn", "responsibility", "existence", "actions", "emotional",
  
  "effectively", "change", "surely", "badly", "overcame", "overcomes", "overcoming"
  
)

#Exclude some of the unrelated charity words (after manual determination)

unrelated_charity <- c(
  
  "need", "needs", "needed", "also", "can", "continue", "working", "work", "opportunity", "able", "well", "make",
  
  "allow", "take", "must", "now", "part", "possible", "one", "new", "future", "addition", "important", "effort",
  
  "may", "receive", "plan", "want", "manage", "ensure", "whether", "keep", "many", "efforts", "meet", "currently",
  
  "necessary", "another", "resources", "importantly", "others", "put", "managed", "purpose", "share", "money", "consider",
  
  "however", "including", "sure", "plans", "wish", "directly", "making", "additional", "find", "ways", "every",
  
  "current", "access", "means", "might", "way", "time", "responsible", "together", "keeping", "specifically",
  
  "advance", "hold", "include", "program", "special", "come", "get", "today", "let", "hope", "individual", "encourage",
  
  "information", "change", "required", "choice", "willing", "organization", "personal", "choose", "please", "already",
  
  "use", "see", "continuing", "deal", "ask", "business", "intended", "planning", "know", "without", "good",
  
  "right", "full", "besides", "require", "decide", "example", "dedicated", "priority", "alone", "much", "ready",
  
  "simply", "involved", "available", "lastly", "expect", "benefit"
  
)

#Apply the exclusions

unrelated_vulnerability_expanded <- expand_with_lemmas(unrelated_vulnerability)

unrelated_charity_expanded <- expand_with_lemmas(unrelated_charity)

expanded_dictionaries <- lapply(expanded_dictionaries, expand_with_lemmas)

expanded_dictionaries$vulnerability <- setdiff(expanded_dictionaries$vulnerability, unrelated_vulnerability_expanded)

expanded_dictionaries$charity_agency <- setdiff(expanded_dictionaries$charity_agency, unrelated_charity_expanded)

expanded_dictionaries$charity_agency <- setdiff(expanded_dictionaries$charity_agency, expanded_dictionaries$vulnerability)


#Print dictionaries
#These are not final dictionaries as candidate terms were manually reviewed


cat("\n================ FINAL DICTIONARY REPORTS ================\n")

walk2(expanded_dictionaries, names(expanded_dictionaries), ~print_dictionary_report(.x, .y, df))
