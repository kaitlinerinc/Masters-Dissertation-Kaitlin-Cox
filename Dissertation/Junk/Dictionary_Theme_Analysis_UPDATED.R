getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
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

NEWSPAPER_INPUT <- "C:/Users/unity/Documents/Dissertation/Final_Cleaned_Newspaper_Ads.rds"
CODING_FILE <- "C:/Users/unity/Documents/Dissertation/Advertisement Coding.xlsx"
EMBEDDING_PATH <- "dolma_300_2024_1.2M.100_combined.txt"
EXPORT_DIR <- "C:/Users/unity/Documents/Dissertation/Outputs/"

dir.create(EXPORT_DIR, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# 2. HELPER FUNCTIONS
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

print_dictionary_report <- function(dictionary, name, data){
  corpus_words <- unique(unlist(str_split(data$text, "\\s+")))
  found <- dictionary[dictionary %in% corpus_words]
  cat("\n---------------------------------\nDICTIONARY:", toupper(name), "\nTotal words:", length(dictionary), "\nFound in corpus:", length(found), "\n")
  cat(paste(str_wrap(paste(dictionary, collapse=", "), width=80), collapse="\n"), "\n")
}

expand_with_lemmas <- function(dictionary){
  lemma_matches <- lexicon::hash_lemmas %>% filter(lemma %in% dictionary) %>% pull(token)
  unique(c(dictionary, lemma_matches))
}

count_actor_dictionary_proximity <- function(text, actors, dictionary, window = 4){
  tokens <- str_split(text, "\\s+")[[1]]
  tokens <- tokens[tokens != ""]
  if(length(tokens)==0) return(0)
  
  actor_positions <- which(tokens %in% actors)
  dict_positions <- which(tokens %in% dictionary)
  if(length(actor_positions)==0 | length(dict_positions)==0) return(0)
  
  count <- 0
  for(a in actor_positions){
    distance <- abs(dict_positions - a)
    count <- count + sum(distance <= window & distance > 0)
  }
  return(count)
}

# ==============================================================================
# 3. LOAD DATA & EMBEDDINGS
# ==============================================================================
df <- prepare_ads(readRDS(NEWSPAPER_INPUT)) %>%
  left_join(read_excel(CODING_FILE) %>% dplyr::select(Ad_Base_ID, Is_International, Is_Animal), by="Ad_Base_ID") %>%
  filter(Is_Animal == 0, !is.na(Is_International))

cat("\nAdvertisements retained:", nrow(df), "\n")

vectors <- fread(EMBEDDING_PATH, header=FALSE, sep=" ", fill=TRUE, quote="", colClasses=c("character", rep("numeric",300)))
embedding_matrix <- as.matrix(vectors[,-1])
rownames(embedding_matrix) <- vectors[[1]]
rm(vectors); gc()

# ==============================================================================
# 4. SEED DICTIONARIES & SEMANTIC EXPANSION (ASYMMETRIC THRESHOLDS)
# ==============================================================================
seed_dictionaries <- list(
  vulnerability = c("helplessness", "hopelessness", "poverty", "suffering", "victim", "powerless", "depend"),
  charity_agency = c("protect", "benevolence", "giver", "aid", "assistance", "rescuing", "heroic", "savior")
)

vulnerability_raw <- expand_with_threshold(
  seeds = seed_dictionaries$vulnerability, 
  embedding_matrix = embedding_matrix, 
  threshold = 0.75, 
  top_n = 1000
)

charity_raw <- expand_with_threshold(
  seeds = seed_dictionaries$charity_agency, 
  embedding_matrix = embedding_matrix, 
  threshold = 0.75, 
  top_n = 1000
)

expanded_dictionaries <- list(
  vulnerability = unique(c(seed_dictionaries$vulnerability, vulnerability_raw$word)),
  charity_agency = unique(c(seed_dictionaries$charity_agency, charity_raw$word))
)

write_csv(vulnerability_raw, file.path(EXPORT_DIR, "vulnerability_similarity.csv"))
write_csv(charity_raw, file.path(EXPORT_DIR, "charity_agency_similarity.csv"))

# ==============================================================================
# 5. MORPHOLOGY, DICTIONARY CLEANING & EXCLUSIONS
# ==============================================================================
unrelated_vulnerability <- c("lives", "life", "consequence", "sense", "consequences", "ultimately", "self", "spite", "situation",
                             "circumstances", "frustration", "realize", "guilt", "reality", "knowing", "overwhelming", "others", "mental", 
                             "worse", "anger", "concern", "despite", "aware", "emotional", "sadness", "feeling",
                             "extent", "doubt", "fact", "indeed", "existence", "dealing", "unfortunate", "apparent", "terrible", "realizing", "affected",
                             "experiencing", "feelings", "beyond", "perceived", "people", "truth",
                             "cope", "mind", "ironically", "unfortunately", "cause", "sad", "worst", "blame", "hence",
                             "nothing", "reason", "thus", "leads", "neither", "regardless", "becomes", "often",
                             "causes", "kind", "acknowledge", "reasons", "understand",
                             "surely", "evident", "sadly", "caused", "rather", "desire", "result",
                             "problem", "recognize", "however", "perhaps", "secondly", "yet", "due", "imagine",
                             "inevitably", "become", "serious", "person", "believe", "concerned",
                             "meant", "therefore", "problems", "confusion", "faced", "sort", "wonder",
                             "attention", "feel", "somehow", "happens", "situations", "realise", "means", "brought", "ways", "moment", "responsibility",
                             "necessarily", "difficult", "humanity", "impossible", "clearly",
                             "bad", "inevitable", "affects", "describe", "belief", "without", "supposed", "matter", "truly",
                             "necessity", "otherwise", "ignorance", "unaware", "latter", "true", "might",
                             "sometimes", "importantly", "past", "regard", "especially", "mean", "psychological",
                             "hardly", "else", "even", "exists", "obvious", "likely", "none", "given", "way", 
                             "child", "realization", "seek", "lead", "almost", "similarly", "much", "merely",
                             "possibility", "never", "still", "thoughts", "convinced", "felt", "partly", "simply", "seeing",
                             "happiness", "understood", "seemingly", "physical", "rest", "conversely", "denial", "human",
                             "towards", "paradoxically", "immediate", "continuing", "relate", "extreme",
                             "lies", "longer", "circumstance", "seems", "failing", "worry", "possible", "dealt",
                             "whether", "believing", "whose", "oftentimes", "sees", "explain", "meaning",
                             "point", "always", "affect", "escape", "losing", "absences", "contraries",
                             "acknowledged", "acknowledges", "acknowledging", "affecting", "angered", "angering", "angers",
                             "apparenter", "apparentest", "attentions", "became", "becoming", "beliefs",
                             "believed", "believes", "blamed", "blames", "blaming", "contrary", "giving", "thought", "confronted",
                             "causing", "children", "childs", "concerning", "concerns", "conflicted", "conflicting",
                             "conflicts", "confusions", "coped", "copes", "coping", "denials", "depended",
                             "depending", "depends", "described", "describes", "describing", "desired", "overcame", "overcomes", "overcoming",
                             "desires", "desiring", "doubted", "doubting", "doubts", "dues", "escaped", "escapes", "escaping", "evened", "evening", "evenner",
                             "evennest", "evens", "existences", "explained", "explaining", "explains", "extents", "extremer",
                             "extremes", "extremest", "facts", "feels", "felts",
                             "frustrations", "guilts",
                             "humanities", "humans", "imagined", "imagines", "imagining", "kinder", "kindest", "kinds", "lacked",
                             "latters", "leading", "led", "lifes", "likelier", "likeliest", "mattered",
                             "mattering", "matters", "meaner", "meanest", "meanings", "mighted", "mighting", "mights",
                             "minded", "minding", "minds", "moments", "more", "most", "necessities",
                             "nothings", "oftener", "oftenest",
                             "pasts", "peopled", "peoples", "peopling", "persons", "physicals",
                             "pointed", "pointing", "points", "possibilities", "possibles", "realised",
                             "realises", "realising", "realities", "realizations", "realized", "realizes", "reasoned",
                             "reasoning", "recognized", "recognizes", "recognizing", "regarded", "regarding", "regards",
                             "related", "relates", "relating", "responsibilities", "rested", "resting", "rests", "resulted",
                             "resulting", "results", "sadder", "saddest", "seeking", "seeks", "selves", "sensed", "senses",
                             "sensing", "sorted", "sorting", "sorts", "sought", "spited",
                             "spites", "spiting", "stilled", "stiller", "stillest", "stilling", "stills", "truer", "truest", "truths", "unabled", "unables", "unabling", "understanding",
                             "understands", "wondered", "wondering",
                             "wonders", "worried", "worries", "worrying", "worsted", "worsting", "worsts" 
)

unrelated_charity <- c("must", "another", "one", "upon", "besides", "part", "also", "promise", "hope", "peace", "necessary",
                       "important", "keeping", "personal", "effort", "intended", "god", "respect", "wish",
                       "take", "able", "sacrifice", "taken", "great", "whatever", "lastly", "every", "well",
                       "responsible", "takes", "mention", "efforts", "comes", "keep",
                       "let", "good", "willing", "continue", "opportunity", "consider", "everyone", "alive", "worthy",
                       "forget", "finding", "make", "man", "future", "idea", "turn", "instead",
                       "particular", "present", "vital", "example", "certainly", "chosen", "taking", "can",
                       "makes", "come", "spirit", "supposedly", "return", "individual",  "ability", "presumably",
                       "force", "possibly", "actions", "may", "put", "enough", "hopes",
                       "faith", "order", "certain", "actually", "trust", "now", "honor", "time", "see",
                       "committed", "anyone", "ever", "making", "intention", "either", "wants",
                       "importance", "putting", "special", "succeed", "accept", "essentially", "behalf",
                       "requires", "someone", "specifically", "follow", "except", "goes", "hands", "hold",
                       "though", "meet", "strong", "although", "aside", "puts", "know", "priority", "yes", "carry",
                       "success", "blessing", "exactly", "proper", "long", "abilities", "abler", "ablest", "accepted",
                       "accepting", "accepts", "asides", "behalfs", "best", "better", "blessings", "came", "canned", "canning", "cans", "carried",
                       "carries", "carrying", "coming", "considered", "considering", "considers", "continued",
                       "continues", "could", "enoughs", "examples", "excepted", "excepting", "excepts", "faiths", "findings", "followed", "following", "follows",
                       "forces", "forcing", "forgets", "forgetting", "forgot", "forgotten", "futures",
                       "gods", "gooder", "goodest", "goods", "greater", "greatest", "greats", "held", "helpings",
                       "heroics", "holding", "holds", "honored", "honoring", "honors", "hoped", "hoping", "ideas",
                       "importances", "individuals", "intentions", "keeps", "kept", "knew", "known", "knows", "lets",
                       "letting", "longed", "longest", "longing", "longs", "made", "makings", "manned", "manning", "mans",
                       "mayed", "maying", "mays", "meeting", "meets", "men", "mentioned", "mentioning", "mentions", "met",
                       "missions", "musts", "necessaries", "ones", "opportunities", "ordered", "ordering", "orders",
                       "parted", "particulars", "parting", "parts", "personals", "presented", "presenting", "presents",
                       "priorities", "promised", "promises", "promising", "purposed",
                       "purposes", "purposing", "respected", "respecting", "respects", "returned", "returning", "returns",
                       "sacrificed", "sacrifices", "sacrificing", "savings", "saw", "seen", "specials", "spirited", "spiriting", "spirits", "stronger", "strongest",
                       "succeeded", "succeeding", "succeeds", "successes", "takings", "timed",
                       "times", "timing", "took", "trusted", "trusting", "trusts", "turned", "turning", "turns", "welled",
                       "welling", "wells", "wished", "wishes", "wishing", "worthier",
                       "worthies", "worthiest", "wrought", "yeah", "yeses", "yesses", "purpose", "life", "others", "supposed", "importantly", "meant",
                       "means", "indeed", "thus", "seek", "lives", "neither", "true", "responsibility",
                       "needed", "self", "needs", "ultimately", "without", "regard", "person",
                       "perhaps", "sense", "therefore", "ways", "whose", "regardless", "yet",
                       "truly", "kind", "surely", "become", "possible", "recognize", "none", "whether", "fact",
                       "however", "attention", "believe", "beyond", "way", "mind", "hence", "simply", "always", "sort",
                       "rest", "rather", "immediate", "secondly", "nothing", "truth", "reason",
                       "aware", "necessity", "even", "concerned", "need", "doubt", "latter", "regard", "otherwise", "merely", "human", "desire", "wonder", "child", "brings",
                       "humanity", "realize", "moment", "meaning", "much", "especially", 
                       "people", "imagine", "despite", "seeking", "else", "never", "circumstances", "concern", "matter", "similarly", "grateful", "reasons", "needing",
                       "serve", "understand", "unfortunately", "many", "sure", "spite", "solely", "role", "rely",
                       "foremost", "choice", "presence", "remember", "thing", "furthermore", "possibility",
                       "maintain", "first", "understanding", "becomes", "finally", "end", "acknowledge", "still",
                       "loving", "leave", "towards", "stand", "wise", "something", "involved", "far", "chance",
                       "instance", "freedom", "somehow", "whole", "critical", "place", "right", "everybody", "trouble",
                       "allow", "remind", "allowing", "knowledge", "clear", "convinced", "moreover", "assume", "hand",
                       "ostensibly", "full", "things", "believed", "unlike", "ask", "situation", "chose",
                       "done", "finds", "actual", "personally", "believes", "together", "want", "equally",
                       "attempt", "focus", "least", "safe", "anything", "find", "suppose", "effectively", "unless",
                       "whenever", "encouraged", "step", "called", "entire", "addition", "call", "leads", "sought",
                       "virtue", "proves", "say", "clearly", "valuable", "survive", "often",
                       "particularly", "meantime", "honest", "action", "implies", "real", "already", "necessarily",
                       "hardly", "tell", "regarding", "allowed", "prove", "encourage", "existence", "complete",
                       "impossible", "commitment", "thirdly", "lead", "father", "change", "extraordinary", "lost",
                       "world", "difficult", "past", "children", "act", "kindness", "like", "remain", "point", "acknowledged", "acknowledges",
                       "acknowledging", "acted", "acting", "actioned", "actioning", "acts", "additions",
                       "allows", "asked", "asking", "asks", "assumed", "assumes",
                       "assuming", "attempted", "attempting", "attempts", "attentions", "became", "becoming",
                       "believing", "calling",
                       "calls", "chanced", "chances", "chancing", "changed", "changes", "changing", "childs", "choicer",
                       "choices", "choicest", "cleared", "clearer", "clearest", "clearing", "clears", "commitments",
                       "completed", "completer", "completes", "completest", "completing", "concerning", "concerns",
                       "depended", "depending", "depends", "desired", "desires", "desiring", "doubted", "doubting",
                       "doubts", "encourages", "encouraging", "ended", "ending", "ends", "established", "establishes",
                       "evened", "evening", "evenner", "evennest", "evens", "existences", "facts",
                       "fars", "farther", "farthest", "fathered", "fathering", "fathers", "firsts", "foci", "focused",
                       "focuses", "focusing", "focussed", "focusses", "focussing", "found", "freedoms", "fulled",
                       "fuller", "fullest", "fulling", "fulls", "further", "furthest",
                       "handing", "heroes", "humanities", "humans", "imagined", "imagines", "imagining", "instanced",
                       "instances", "instancing", "kinder", "kindest", "kindnesses", "kinds", "knowledges", "latters",
                       "leading", "leaves", "leaving", "led", "left", "lifes", "liked", "likes", "liking", "maintained",
                       "maintaining", "maintains", "mattered", "mattering", "matters", "meanings", "mighted",
                       "mighting", "mights", "minded", "minding", "minds", "moments", "more", "most", "necessities",
                       "nothings", "oftener", "oftenest", "pasts", "peopled", "peoples", "peopling", "persons", "placed",
                       "places", "placing", "pointed", "pointing", "points", "possibilities", "possibles", "presences",
                       "proved", "proven", "proving", "realer", "reales", "realest", "realized",
                       "realizes", "realizing", "reals", "reasoned", "reasoning", "recognized", "recognizes",
                       "recognizing", "regarded", "reis", "relied", "relies", "relying", "remained", "remaining",
                       "remains", "remembered", "remembering", "remembers", "reminded", "reminding", "reminds",
                       "responsibilities", "rested", "resting", "rests", "righted", "righter",
                       "rightest", "righting", "rights", "roles", "safer", "safes", "safest", "said",
                       "saying", "says", "seeks", "selves", "sensed", "senses", "sensing",
                       "situations", "sorted", "sorting", "sorts", "spited", "spites", "spiting", "standing", "stands",
                       "stepped", "stepping", "steps", "stilled", "stiller", "stillest", "stilling", "stills", "stood", "supposes", "supposing", "surer", "surest", "survived", "survives",
                       "surviving", "telling", "tells", "thoughts", "told", "troubled", "troubles", "troubling",
                       "truer", "truest", "truths", "unabled", "unables", "unabling", "understandings", "understands",
                       "understood", "valuables", "virtues", "wanted", "wanting", "wholes", "wised", "wiser", "wises",
                       "wisest", "wising", "wondered", "wondering", "wonders", "worlds",
                       "thought", "everything", "deal", "task", "assured", "consideration", "demands", "creation", "dealing",
                       "exception", "immediately", "advantage", "trying", "thanks", "wherever", "fail", "family",
                       "devoted", "nature", "seems", "eventually", "mean", "away", "gain", "appears", "directly",
                       "apparently", "likely", "loved", "fortunate", "speak", "fortunately", "obviously",
                       "soon", "extent", "accomplished", "successful", "assure", "direct", "fulfill", "apart",
                       "offered", "absolutely", "gone", "manner", "ironically",
                       "advantaged", "advantages", "advantaging", "assures", "assuring",
                       "considerations", "creations", "deals", "dealt",
                       "exceptions", "extents", "failed", "failing", "fails", "families", "fulfilled",
                       "fulfilling", "fulfills", "gained", "gaining", "gains", "givens", "lacked", "likelier",
                       "likeliest", "manners", "meaner", "meanest", "natures",
                       "sooner", "soonest", "spake", "speaking", "speaks", "spoke",
                       "spoken", "tasked", "tasking", "tasks")

unrelated_vulnerability_expanded <- expand_with_lemmas(unrelated_vulnerability)
unrelated_charity_expanded <- expand_with_lemmas(unrelated_charity)

expanded_dictionaries <- lapply(expanded_dictionaries, expand_with_lemmas)

expanded_dictionaries$vulnerability <- setdiff(expanded_dictionaries$vulnerability, unrelated_vulnerability_expanded)
expanded_dictionaries$charity_agency <- setdiff(expanded_dictionaries$charity_agency, unrelated_charity_expanded)
expanded_dictionaries$charity_agency <- setdiff(expanded_dictionaries$charity_agency, expanded_dictionaries$vulnerability)

cat("\n================ FINAL DICTIONARY REPORTS ================\n")
walk2(expanded_dictionaries, names(expanded_dictionaries), ~print_dictionary_report(.x, .y, df))








# ==============================================================================
# 6. ACTOR DICTIONARIES & PROXIMITY ANALYSIS ON CORPUS
# ==============================================================================
charity_actors <- c("we", "our", "us", "charity", "organization", "organisation", "staff", "worker",
                    "workers", "volunteer", "volunteers", "donor", "donors", "supporter", "supporters",
                    "you", "your", "yourself")
beneficiary_actors <- c("they", "them", "their", "people", "person", "individual", "individuals",
                        "community", "communities", "families", "family", "children", "child")

df <- df %>%
  mutate(
    vulnerability_hits = stri_count_regex(text, build_regex_pattern(expanded_dictionaries$vulnerability)),
    charity_agency_hits = stri_count_regex(text, build_regex_pattern(expanded_dictionaries$charity_agency)),
    charity_actor_agency_hits = map_int(text, ~count_actor_dictionary_proximity(.x, charity_actors, expanded_dictionaries$charity_agency, window=4)),
    beneficiary_actor_vulnerability_hits = map_int(text, ~count_actor_dictionary_proximity(.x, beneficiary_actors, expanded_dictionaries$vulnerability, window=4)),
    ratio_vuln_to_charity = (vulnerability_hits + 1) / (charity_agency_hits + 1),
    ratio_prox_vuln_to_charity = (beneficiary_actor_vulnerability_hits + 1) / (charity_actor_agency_hits + 1)
  )

cat("\nCorpus analysis and proximity calculation completed successfully.\n")

# ==============================================================================
# 7. PRONOUN COUNTS & DESCRIPTIVE STATISTICS
# ==============================================================================
df <- df %>%
  mutate(
    count_we = stri_count_regex(text, "\\bwe\\b"),
    count_us = stri_count_regex(text, "\\bus\\b"),
    count_our = stri_count_regex(text, "\\bour\\b"),
    count_you = stri_count_regex(text, "\\byou\\b"),
    count_your = stri_count_regex(text, "\\byour\\b"),
    count_they = stri_count_regex(text, "\\bthey\\b"),
    count_them = stri_count_regex(text, "\\bthem\\b"),
    count_their = stri_count_regex(text, "\\btheir\\b"),
    total_agency_pronouns = count_we + count_us + count_our + count_you + count_your,
    total_beneficiary_pronouns = count_they + count_them + count_their
  )

summary_table <- df %>%
  group_by(Is_International) %>%
  summarise(
    Ads = n(),
    Mean_Tokens = round(mean(Total_Tokens, na.rm = TRUE), 2),
    Mean_Noise = round(mean(NoiseRatio, na.rm = TRUE), 3),
    Mean_Vulnerability = round(mean(vulnerability_hits), 2),
    Mean_Charity_Agency = round(mean(charity_agency_hits), 2),
    Mean_Agency_Pronouns = round(mean(total_agency_pronouns), 2),
    Mean_Beneficiary_Pronouns = round(mean(total_beneficiary_pronouns), 2)
  )

write_csv(summary_table, file.path(EXPORT_DIR, "descriptive_statistics.csv"))
cat("\n================ DESCRIPTIVE STATISTICS ================\n")
print(summary_table)

coverage_table <- df %>%
  summarise(
    Total_Ads = n(),
    Ads_With_Vulnerability = sum(vulnerability_hits > 0),
    Ads_With_Charity = sum(charity_agency_hits > 0),
    Ads_With_Proximity_Vulnerability = sum(beneficiary_actor_vulnerability_hits > 0),
    Ads_With_Proximity_Charity = sum(charity_actor_agency_hits > 0)
  )

write_csv(coverage_table, file.path(EXPORT_DIR, "dictionary_coverage.csv"))
cat("\n================ COVERAGE ================\n")
print(coverage_table)

# ==============================================================================
# 8. REGRESSION MODELS & DIAGNOSTICS
# ==============================================================================
analysis_df <- df %>% mutate(log_valid_words = log(Dictionary_Words + 1))

model_vulnerability <- glm.nb(vulnerability_hits ~ Is_International + NoiseRatio + offset(log_valid_words), data = analysis_df)
model_charity <- glm.nb(charity_agency_hits ~ Is_International + NoiseRatio + offset(log_valid_words), data = analysis_df)

model_vulnerability_proximity <- glm.nb(beneficiary_actor_vulnerability_hits ~ Is_International + NoiseRatio + offset(log_valid_words), data = analysis_df)
model_charity_proximity <- glm.nb(charity_actor_agency_hits ~ Is_International + NoiseRatio + offset(log_valid_words), data = analysis_df)

robust_output <- function(model){ coeftest(model, vcov = sandwich) }

cat("\n=================================================\nNEGATIVE BINOMIAL RESULTS\n=================================================\n")
cat("\n--- Vulnerability ---\n"); print(robust_output(model_vulnerability))
cat("\n--- Charity Agency ---\n"); print(robust_output(model_charity))
cat("\n--- Vulnerability Proximity ---\n"); print(robust_output(model_vulnerability_proximity))
cat("\n--- Charity Proximity ---\n"); print(robust_output(model_charity_proximity))

capture.output(summary(model_vulnerability), file = file.path(EXPORT_DIR, "model_vulnerability.txt"))
capture.output(summary(model_charity), file = file.path(EXPORT_DIR, "model_charity_agency.txt"))
capture.output(summary(model_vulnerability_proximity), file = file.path(EXPORT_DIR, "model_vulnerability_proximity.txt"))
capture.output(summary(model_charity_proximity), file = file.path(EXPORT_DIR, "model_charity_proximity.txt"))

# Model Diagnostics
check_nb_model <- function(model, name){
  cat("\n====================================\n", name, "\n====================================\n")
  theta <- model$theta
  cat("Theta dispersion parameter:", round(theta,3), "\n")
  if(theta < 1){ cat("Warning: strong overdispersion detected.\n") } else { cat("Negative binomial dispersion acceptable.\n") }
  cat("Residual deviance:", round(deviance(model),2), "\n")
}

check_nb_model(model_vulnerability, "Vulnerability Model")
check_nb_model(model_charity, "Charity Agency Model")
check_nb_model(model_vulnerability_proximity, "Vulnerability Proximity Model")
check_nb_model(model_charity_proximity, "Charity Proximity Model")

# IRR Results
extract_IRR <- function(model, name){
  summary(model)$coefficients %>% as.data.frame() %>% rownames_to_column("Variable") %>%
    mutate(
      IRR = exp(Estimate),
      Lower95 = exp(Estimate - 1.96*`Std. Error`),
      Upper95 = exp(Estimate + 1.96*`Std. Error`),
      Model = name
    )
}

irr_results <- bind_rows(
  extract_IRR(model_vulnerability, "Vulnerability"),
  extract_IRR(model_charity, "Charity Agency"),
  extract_IRR(model_vulnerability_proximity, "Vulnerability Proximity"),
  extract_IRR(model_charity_proximity, "Charity Proximity")
)

write_csv(irr_results, file.path(EXPORT_DIR, "negative_binomial_IRR_results.csv"))
cat("\n================ IRR RESULTS ================\n")
print(irr_results)

# Group Differences & Non-parametric Tests
group_tests <- df %>%
  group_by(Is_International) %>%
  summarise(
    N = n(),
    Mean_Vulnerability = mean(vulnerability_hits, na.rm = TRUE),
    Mean_Charity = mean(charity_agency_hits, na.rm = TRUE),
    Mean_Beneficiary_Proximity = mean(beneficiary_actor_vulnerability_hits, na.rm = TRUE),
    Mean_Charity_Proximity = mean(charity_actor_agency_hits, na.rm = TRUE),
    Mean_Base_Ratio = mean(ratio_vuln_to_charity, na.rm = TRUE),
    Mean_Proximity_Ratio = mean(ratio_prox_vuln_to_charity, na.rm = TRUE)
  )

write_csv(group_tests, file.path(EXPORT_DIR, "group_comparison_results.csv"))

wilcox_results <- tibble(
  Outcome = c("Vulnerability", "Charity Agency", "Beneficiary Proximity", "Charity Proximity", "Proximity Ratio"),
  p_value = c(
    wilcox.test(vulnerability_hits ~ Is_International, data = df)$p.value,
    wilcox.test(charity_agency_hits ~ Is_International, data = df)$p.value,
    wilcox.test(beneficiary_actor_vulnerability_hits ~ Is_International, data = df)$p.value,
    wilcox.test(charity_actor_agency_hits ~ Is_International, data = df)$p.value,
    wilcox.test(ratio_prox_vuln_to_charity ~ Is_International, data = df)$p.value
  )
)

write_csv(wilcox_results, file.path(EXPORT_DIR, "nonparametric_group_tests.csv"))

# Final Data & Correlation Exports
saveRDS(df, file.path(EXPORT_DIR, "final_analysis_dataset.rds"))
write_csv(df, file.path(EXPORT_DIR, "final_analysis_dataset.csv"))

cor(df$vulnerability_hits, df$beneficiary_actor_vulnerability_hits, use = "complete.obs")
cor(df$vulnerability_hits, df$beneficiary_actor_vulnerability_hits, method = "spearman", use = "complete.obs")

cat("\n================ PIPELINE COMPLETE ================\nFinal advertisements analyzed:", nrow(df), "\nOutputs saved to:", EXPORT_DIR, "\n")





##########################################################################################################################################################




# ==============================================================================
# 9. BINARY VULNERABILITY PRESENCE MODEL
# ==============================================================================

analysis_df <- analysis_df %>%
  mutate(
    vulnerability_present = ifelse(vulnerability_hits > 0, 1, 0),
    charity_present = ifelse(charity_agency_hits > 0, 1, 0),
    vulnerability_proximity_present = ifelse(beneficiary_actor_vulnerability_hits > 0, 1, 0)
  )



model_vulnerability_binary <- glm(
  vulnerability_present ~ Is_International + NoiseRatio,
  data = analysis_df,
  family = binomial(link="logit")
)

summary(model_vulnerability_binary)

binary_OR <- function(model){
  
  summary(model)$coefficients %>%
    as.data.frame() %>%
    rownames_to_column("Variable") %>%
    mutate(
      Odds_Ratio = exp(Estimate),
      Lower95 = exp(Estimate - 1.96*`Std. Error`),
      Upper95 = exp(Estimate + 1.96*`Std. Error`)
    )
}


vulnerability_OR <- binary_OR(model_vulnerability_binary)

print(vulnerability_OR)

write_csv(
  vulnerability_OR,
  file.path(EXPORT_DIR,"binary_vulnerability_odds_ratios.csv")
)






























# ==============================================================================
# TABLE 1: DESCRIPTIVE STATISTICS (GLOBAL NORTH VS. GLOBAL SOUTH)
# ==============================================================================

table_1 <- df %>%
  mutate(Region = ifelse(Is_International == 1, "Global South", "Global North")) %>%
  group_by(Region) %>%
  summarise(
    Total_Ads = n(),
    Total_Document_Length = sum(Total_Tokens, na.rm = TRUE),
    Mean_Tokens_Per_Ad = round(mean(Total_Tokens, na.rm = TRUE), 2),
    Median_Tokens_Per_Ad = round(median(Total_Tokens, na.rm = TRUE), 2),
    SD_Words_Per_Ad = round(sd(Total_Tokens, na.rm = TRUE), 2),
    Mean_OCR_Noise = round(mean(NoiseRatio, na.rm = TRUE), 3)
  )

# Export and print Table 1
write_csv(table_1, file.path(EXPORT_DIR, "table_1_descriptive_stats.csv"))
cat("\n================ TABLE 1: DESCRIPTIVE STATISTICS ================\n")
print(table_1)

# ==============================================================================
# DYNAMICALLY FILL IN THE COMPARISON SENTENCE
# ==============================================================================

# Extract mean tokens for each region
mean_north <- table_1 %>% filter(Region == "Global North") %>% pull(Mean_Tokens_Per_Ad)
mean_south <- table_1 %>% filter(Region == "Global South") %>% pull(Mean_Tokens_Per_Ad)

if (mean_north > mean_south) {
  diff_words <- round(mean_north - mean_south, 2)
  cat(paste0("\nGlobal North advertisements have on average ", diff_words, " more words than Global South advertisements.\n"))
} else {
  diff_words <- round(mean_south - mean_north, 2)
  cat(paste0("\nGlobal South advertisements have on average ", diff_words, " more words than Global North advertisements.\n"))
}


# ==============================================================================
# TABLE 2: THEME PROPORTIONS & IMAGE EXPORT
# ==============================================================================

table_2 <- tibble(
  Theme = c("Vulnerability", "Charity Activity", "Vulnerability Proximity", "Charity Activity Proximity"),
  Proportion = c(
    mean(df$vulnerability_hits > 0, na.rm = TRUE),
    mean(df$charity_agency_hits > 0, na.rm = TRUE),
    mean(df$beneficiary_actor_vulnerability_hits > 0, na.rm = TRUE),
    mean(df$charity_actor_agency_hits > 0, na.rm = TRUE)
  )
) %>% mutate(Proportion = round(Proportion, 4))

# Export Table 2 as CSV
write_csv(table_2, file.path(EXPORT_DIR, "table_2_theme_proportions.csv"))

# Export Table 2 as an Image (PNG)
png(file.path(EXPORT_DIR, "table_2_theme_proportions.png"), width = 900, height = 250, res = 100)
grid.draw(tableGrob(table_2))
dev.off()

cat("\n================ TABLE 2: THEME PROPORTIONS ================\n")
print(table_2)

# Dynamic Text Output Fill
max_row <- table_2 %>% filter(Proportion == max(Proportion)) %>% slice(1)
min_row <- table_2 %>% filter(Proportion == min(Proportion)) %>% slice(1)

cat(paste0("\nThe highest proportion found was for ", max_row$Theme, " with a proportion of ", max_row$Proportion, 
           " and the lowest proportion found was for ", min_row$Theme, " with a proportion of ", min_row$Proportion, ".\n"))




library(dplyr)
library(stargazer)

# Descriptive statistics by group
desc_vulnerability <- analysis_df %>%
  group_by(Is_International) %>%
  summarise(
    N = n(),
    Vulnerability = mean(vulnerability_hits, na.rm = TRUE),
    Dictionary_Words = mean(Dictionary_Words, na.rm = TRUE),
    Noise_Ratio = mean(NoiseRatio, na.rm = TRUE)
  )

stargazer(
  as.data.frame(desc_vulnerability),
  summary = FALSE,
  rownames = FALSE,
  type = "text",
  title = "Table 3a. Descriptive Statistics for Vulnerability by Advertisement Type",
  digits = 2
)

# Optional HTML output
stargazer(
  as.data.frame(desc_vulnerability),
  summary = FALSE,
  rownames = FALSE,
  type = "html",
  out = "Outputs/Table3a_Descriptive_Vulnerability.html",
  digits = 2
)



library(stargazer)
library(lmtest)
library(sandwich)

# Robust standard errors
robust_se <- sqrt(diag(vcovHC(model_vulnerability, type = "HC1")))

stargazer(
  model_vulnerability,
  type = "text",               # Change to "html" or "latex" for Word/LaTeX
  title = "Table 3b. Negative Binomial Regression Predicting Vulnerability Language",
  se = list(robust_se),
  digits = 3,
  
  dep.var.labels = "Vulnerability Word Count",
  
  covariate.labels = c(
    "International Advertisement",
    "OCR Noise Ratio"
  ),
  
  omit.stat = c("LL", "aic"),
  intercept.bottom = FALSE,
  single.row = FALSE,
  
  add.lines = list(
    c("Offset", "Log(Dictionary Words)"),
    c("Observations", nobs(model_vulnerability))
  ),
  
  notes = c(
    "Negative binomial regression with robust (HC1) standard errors.",
    "Models include an offset for the log of dictionary words."
  )
)




library(lmtest)
library(sandwich)
library(stargazer)

# Robust coefficient test
robust <- coeftest(model_vulnerability,
                   vcov = vcovHC(model_vulnerability, type = "HC1"))

# Extract robust SEs and p-values
robust_se <- robust[,2]
robust_p  <- robust[,4]

stargazer(
  model_vulnerability,
  se = list(robust_se),
  p = list(robust_p),
  type = "text",
  digits = 3,
  dep.var.labels = "Vulnerability Word Count",
  intercept.bottom = FALSE,
  omit.stat = c("LL","aic")
)



library(modelsummary)

modelsummary(
  model_vulnerability,
  statistic = c("std.error", "p.value"),
  output = "markdown"
)















# ==============================================================================
# CHARITY AGENCY: DESCRIPTIVE STATISTICS + NEGATIVE BINOMIAL MODEL
# ==============================================================================

# Libraries
library(dplyr)
library(MASS)
library(knitr)
library(broom)

# ------------------------------------------------------------------------------
# 1. DESCRIPTIVE STATISTICS
# ------------------------------------------------------------------------------

# Overall descriptive statistics
charity_agency_descriptives <- ads_final %>%
  summarise(
    N = n(),
    Mean = mean(charity_agency_hits, na.rm = TRUE),
    SD = sd(charity_agency_hits, na.rm = TRUE),
    Median = median(charity_agency_hits, na.rm = TRUE),
    Minimum = min(charity_agency_hits, na.rm = TRUE),
    Maximum = max(charity_agency_hits, na.rm = TRUE)
  )

print(charity_agency_descriptives)


# Descriptives by Global North / Global South
charity_agency_by_group <- ads_final %>%
  group_by(Is_International) %>%
  summarise(
    N = n(),
    Mean = mean(charity_agency_hits, na.rm = TRUE),
    SD = sd(charity_agency_hits, na.rm = TRUE),
    Median = median(charity_agency_hits, na.rm = TRUE),
    Minimum = min(charity_agency_hits, na.rm = TRUE),
    Maximum = max(charity_agency_hits, na.rm = TRUE)
  )

print(charity_agency_by_group)


# Optional: proportions of ads containing charity agency language
charity_agency_presence <- ads_final %>%
  mutate(
    charity_agency_present = ifelse(charity_agency_hits > 0, 1, 0)
  ) %>%
  group_by(Is_International) %>%
  summarise(
    N = n(),
    Ads_with_agency_language = sum(charity_agency_present),
    Proportion = mean(charity_agency_present)
  )

print(charity_agency_presence)


# ------------------------------------------------------------------------------
# 2. NEGATIVE BINOMIAL MODEL
# ------------------------------------------------------------------------------

# Ensure grouping variable is a factor
ads_final <- ads_final %>%
  mutate(
    Is_International = factor(
      Is_International,
      levels = c(0,1),
      labels = c("Global_North", "Global_South")
    )
  )


# Negative binomial regression
nb_charity_agency <- glm.nb(
  charity_agency_hits ~ Is_International,
  data = ads_final
)


# Model output
summary(nb_charity_agency)


# ------------------------------------------------------------------------------
# 3. TIDY RESULTS TABLE
# ------------------------------------------------------------------------------

charity_agency_model_results <- tidy(
  nb_charity_agency,
  exponentiate = TRUE,
  conf.int = TRUE
)

print(charity_agency_model_results)


# ------------------------------------------------------------------------------
# 4. DISPERSION CHECK
# ------------------------------------------------------------------------------

# Compare Poisson and NB AIC values
poisson_charity <- glm(
  charity_agency_hits ~ Is_International,
  family = poisson,
  data = ads_final
)

AIC(poisson_charity)
AIC(nb_charity_agency)


# ------------------------------------------------------------------------------
# 5. WRITE RESULTS
# ------------------------------------------------------------------------------

write.csv(
  charity_agency_model_results,
  "charity_agency_negative_binomial_results.csv",
  row.names = FALSE
)





# ==============================================================================
# LIBRARIES REQUIRED FOR PRESENTATION
# ==============================================================================
library(dplyr)
library(readxl)
library(grid)
library(gridExtra)
library(lmtest)
library(sandwich)
library(broom)
library(modelsummary)
library(readr)
library(tibble)

EXPORT_DIR <- "C:/Users/unity/Documents/Dissertation/Outputs/"
dir.create(EXPORT_DIR, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# TABLE 1: PUBLICATION & DUPLICATE REMOVAL (EXCLUDING ANIMAL ADS)
# ==============================================================================
# Filter the original coding sheet to exclude animal ads
coding <- read_excel("Advertisement Coding.xlsx") %>% 
  filter(Is_Animal == 0)

before <- coding %>%
  group_by(Is_International) %>%
  summarise(Original_Ads = n(), .groups = "drop")

# Ensure the final dataset (df) also strictly excludes animal ads
after <- df %>%
  filter(Is_Animal == 0) %>%
  group_by(Is_International) %>%
  summarise(Final_Ads = n(), .groups = "drop")

table_1_duplicates <- before %>%
  left_join(after, by = "Is_International") %>%
  mutate(
    Region = ifelse(Is_International == 1, "Global South", "Global North"),
    Percent_Original = round(100 * Original_Ads / sum(Original_Ads), 1),
    Duplicates_Removed = Original_Ads - Final_Ads,
    Percent_Final = round(100 * Final_Ads / sum(Final_Ads), 1)
  ) %>%
  dplyr::select(Region, Original_Ads, Percent_Original, Duplicates_Removed, Final_Ads, Percent_Final)

write_csv(table_1_duplicates, file.path(EXPORT_DIR, "table_1_duplicate_removal.csv"))
cat("\n================ TABLE 1: DUPLICATE REMOVAL (NO ANIMAL ADS) ================\n")
print(table_1_duplicates)

# ==============================================================================
# TABLE 2: CORPUS DESCRIPTIVE STATISTICS
# ==============================================================================
table_2_descriptives <- df %>%
  mutate(Region = ifelse(Is_International == 1, "Global South", "Global North")) %>%
  group_by(Region) %>%
  summarise(
    Total_Ads = n(),
    Total_Document_Length = sum(Total_Tokens, na.rm = TRUE),
    Mean_Tokens_Per_Ad = round(mean(Total_Tokens, na.rm = TRUE), 2),
    Median_Tokens_Per_Ad = round(median(Total_Tokens, na.rm = TRUE), 2),
    SD_Words_Per_Ad = round(sd(Total_Tokens, na.rm = TRUE), 2),
    Mean_OCR_Noise = round(mean(NoiseRatio, na.rm = TRUE), 3)
  )

write_csv(table_2_descriptives, file.path(EXPORT_DIR, "table_2_descriptive_stats.csv"))
cat("\n================ TABLE 2: DESCRIPTIVE STATISTICS ================\n")
print(table_2_descriptives)

# ==============================================================================
# TABLE 3: THEME PROPORTIONS
# ==============================================================================
table_3_themes <- tibble(
  Theme = c("Vulnerability", "Charity Activity", "Vulnerability Proximity", "Charity Activity Proximity"),
  Proportion = c(
    mean(df$vulnerability_hits > 0, na.rm = TRUE),
    mean(df$charity_agency_hits > 0, na.rm = TRUE),
    mean(df$beneficiary_actor_vulnerability_hits > 0, na.rm = TRUE),
    mean(df$charity_actor_agency_hits > 0, na.rm = TRUE)
  )
) %>% mutate(Proportion = round(Proportion, 4))

write_csv(table_3_themes, file.path(EXPORT_DIR, "table_3_theme_proportions.csv"))

# Export Table 3 as an Image (PNG)
png(file.path(EXPORT_DIR, "table_3_theme_proportions.png"), width = 900, height = 250, res = 100)
grid.draw(tableGrob(table_3_themes))
dev.off()

cat("\n================ TABLE 3: THEME PROPORTIONS ================\n")
print(table_3_themes)

# ==============================================================================
# TABLE 4: REGRESSION RESULTS (MODELSUMMARY) - NEGATIVE BINOMIAL
# ==============================================================================
# Define a clean map for variable names
clean_coefs <- c(
  "(Intercept)" = "Intercept",
  "Is_International" = "International Ad (Global South)",
  "NoiseRatio" = "OCR Noise Ratio"
)

nb_models <- list(
  "Vuln. Count" = model_vulnerability,
  "Charity Count" = model_charity,
  "Vuln. Proximity" = model_vulnerability_proximity,
  "Charity Proximity" = model_charity_proximity
)

modelsummary(
  nb_models,
  vcov = "HC3",                        # Applies HC3 robust standard errors natively
  coef_map = clean_coefs,              # Renames variables for publication
  stars = TRUE,                        # Adds significance asterisks
  gof_omit = "IC|Log|F|RMSE",          # Cleans up the bottom metrics
  title = "Table 4: Negative Binomial Regression Predicting Themes and Proximity (HC3 SEs)",
  output = file.path(EXPORT_DIR, "table_4_negative_binomial_models.html")
)

# ==============================================================================
# TABLE 5: REGRESSION RESULTS (MODELSUMMARY) - OLS RATIOS
# ==============================================================================
ols_models <- list(
  "Base Ratio" = model_ratio_vuln_charity,
  "Prox. Ratio" = model_ratio_prox_vuln_charity
)

modelsummary(
  ols_models,
  vcov = "HC3",                        # Applies HC3 robust standard errors natively
  coef_map = clean_coefs,
  stars = TRUE,
  gof_omit = "IC|Log|F|RMSE",
  title = "Table 5: OLS Regression for Theme Ratios (HC3 SEs)",
  output = file.path(EXPORT_DIR, "table_5_ols_ratio_models.html")
)

# ==============================================================================
# TABLE 6: INCIDENCE RATE RATIOS (IRR)
# ==============================================================================
extract_IRR <- function(model, name){
  # Extracting HC3 robust standard errors manually just for this custom table
  robust_se <- sqrt(diag(vcovHC(model, type = "HC3")))
  
  summary(model)$coefficients %>% 
    as.data.frame() %>% 
    rownames_to_column("Variable") %>%
    mutate(
      `Robust SE (HC3)` = robust_se[Variable],
      IRR = exp(Estimate),
      Lower95 = exp(Estimate - 1.96 * `Robust SE (HC3)`),
      Upper95 = exp(Estimate + 1.96 * `Robust SE (HC3)`),
      Model = name
    ) %>%
    dplyr::select(Model, Variable, IRR, Lower95, Upper95)
}

irr_results <- bind_rows(
  extract_IRR(model_vulnerability, "Vulnerability"),
  extract_IRR(model_charity, "Charity Agency"),
  extract_IRR(model_vulnerability_proximity, "Vulnerability Proximity"),
  extract_IRR(model_charity_proximity, "Charity Proximity")
)

write_csv(irr_results, file.path(EXPORT_DIR, "table_6_irr_results.csv"))
cat("\n================ TABLE 6: IRR RESULTS ================\n")
print(irr_results)

# ==============================================================================
# GRAPHS: OLS RESIDUAL DIAGNOSTICS
# ==============================================================================
ratio_diagnostics <- function(model, name){
  png(file.path(EXPORT_DIR, paste0(name, "_diagnostics.png")), width=1200, height=900)
  par(mfrow=c(2,2)) 
  plot(model) 
  dev.off()
}

ratio_diagnostics(model_ratio_vuln_charity, "graph_1_ratio_vuln_to_charity")
ratio_diagnostics(model_ratio_prox_vuln_charity, "graph_2_ratio_prox_vuln_to_charity")

cat("\n================ PRESENTATION EXPORTS COMPLETE ================\n")

















# ==============================================================================
# TABLE 2: CORPUS DESCRIPTIVE STATISTICS
# ==============================================================================
table_2_descriptives <- df %>%
  mutate(Region = ifelse(Is_International == 1, "Global South", "Global North")) %>%
  group_by(Region) %>%
  summarise(
    Total_Ads = n(),
    Total_Document_Length = sum(Total_Tokens, na.rm = TRUE),
    Mean_Tokens_Per_Ad = round(mean(Total_Tokens, na.rm = TRUE), 2),
    Median_Tokens_Per_Ad = round(median(Total_Tokens, na.rm = TRUE), 2),
    SD_Words_Per_Ad = round(sd(Total_Tokens, na.rm = TRUE), 2),
    Mean_OCR_Noise = round(mean(NoiseRatio, na.rm = TRUE), 3)
  )

write_csv(table_2_descriptives, file.path(EXPORT_DIR, "table_2_descriptive_stats.csv"))
cat("\n================ TABLE 2: DESCRIPTIVE STATISTICS ================\n")
print(table_2_descriptives)

# ==============================================================================
# TABLE 3: THEME PROPORTIONS
# ==============================================================================
table_3_themes <- tibble(
  Theme = c("Vulnerability", "Charity Activity", "Vulnerability Proximity", "Charity Activity Proximity"),
  Proportion = c(
    mean(df$vulnerability_hits > 0, na.rm = TRUE),
    mean(df$charity_agency_hits > 0, na.rm = TRUE),
    mean(df$beneficiary_actor_vulnerability_hits > 0, na.rm = TRUE),
    mean(df$charity_actor_agency_hits > 0, na.rm = TRUE)
  )
) %>% mutate(Proportion = round(Proportion, 4))

write_csv(table_3_themes, file.path(EXPORT_DIR, "table_3_theme_proportions.csv"))
cat("\n================ TABLE 3: THEME PROPORTIONS ================\n")
print(table_3_themes)

# ==============================================================================
# TABLE 4: REGRESSION RESULTS (MODELSUMMARY) - NEGATIVE BINOMIAL
# ==============================================================================
# Define a clean map for variable names
clean_coefs <- c(
  "(Intercept)" = "Intercept",
  "Is_International" = "International Ad (Global South)",
  "NoiseRatio" = "OCR Noise Ratio"
)

nb_models <- list(
  "Vuln. Count" = model_vulnerability,
  "Charity Count" = model_charity,
  "Vuln. Proximity" = model_vulnerability_proximity,
  "Charity Proximity" = model_charity_proximity
)

modelsummary(
  nb_models,
  vcov = "HC3",                        # Applies HC3 robust standard errors natively
  coef_map = clean_coefs,              # Renames variables for publication
  stars = TRUE,                        # Adds significance asterisks
  gof_omit = "IC|Log|F|RMSE",          # Cleans up the bottom metrics
  title = "Table 4: Negative Binomial Regression Predicting Themes and Proximity (HC3 SEs)",
  output = file.path(EXPORT_DIR, "table_4_negative_binomial_models.html")
)

# ==============================================================================
# TABLE 6: INCIDENCE RATE RATIOS (IRR) FOR NEGATIVE BINOMIAL
# ==============================================================================
extract_IRR <- function(model, name){
  # Extracting HC3 robust standard errors manually just for this custom table
  robust_se <- sqrt(diag(vcovHC(model, type = "HC3")))
  
  summary(model)$coefficients %>% 
    as.data.frame() %>% 
    rownames_to_column("Variable") %>%
    mutate(
      `Robust SE (HC3)` = robust_se[Variable],
      IRR = exp(Estimate),
      Lower95 = exp(Estimate - 1.96 * `Robust SE (HC3)`),
      Upper95 = exp(Estimate + 1.96 * `Robust SE (HC3)`),
      Model = name
    ) %>%
    dplyr::select(Model, Variable, IRR, Lower95, Upper95)
}

irr_results <- bind_rows(
  extract_IRR(model_vulnerability, "Vulnerability"),
  extract_IRR(model_charity, "Charity Agency"),
  extract_IRR(model_vulnerability_proximity, "Vulnerability Proximity"),
  extract_IRR(model_charity_proximity, "Charity Proximity")
)

write_csv(irr_results, file.path(EXPORT_DIR, "table_6_irr_results.csv"))
cat("\n================ TABLE 6: IRR RESULTS ================\n")
print(irr_results)














# ==============================================================================
# RESULTS SECTION TABLES AND FIGURES
# ==============================================================================

library(dplyr)
library(ggplot2)
library(stargazer)
library(broom)
library(sandwich)
library(lmtest)
library(modelsummary)
library(patchwork)
library(scales)

RESULT_DIR <- "Outputs/Results_Figures/"
dir.create(RESULT_DIR, showWarnings = FALSE, recursive = TRUE)


# ==============================================================================
# CREATE REGION VARIABLE
# ==============================================================================

results_df <- df %>%
  mutate(
    Region = ifelse(
      Is_International == 1,
      "Global South",
      "Global North"
    )
  )


# ==============================================================================
# TABLE 1: SAMPLE DESCRIPTION
# ==============================================================================

table_sample <- results_df %>%
  group_by(Region) %>%
  summarise(
    Advertisements = n(),
    Percentage = round(100*n()/nrow(results_df),1),
    Mean_Words = round(mean(Total_Tokens, na.rm=T),2),
    Mean_Noise = round(mean(NoiseRatio, na.rm=T),3)
  )

write.csv(
  table_sample,
  file.path(RESULT_DIR,"Table1_sample_description.csv"),
  row.names=FALSE
)


# ==============================================================================
# LONG FORMAT TABLE FOR RESULTS SECTION
# ==============================================================================

theme_table_long <- tibble(
  
  Theme = c(
    "Vulnerability",
    "Charity Agency",
    "Vulnerability Proximity",
    "Charity Agency Proximity"
  ),
  
  Proportion_Ads = c(
    mean(results_df$vulnerability_hits > 0),
    mean(results_df$charity_agency_hits > 0),
    mean(results_df$beneficiary_actor_vulnerability_hits > 0),
    mean(results_df$charity_actor_agency_hits > 0)
  ) * 100,
  
  Mean_Hits = c(
    mean(results_df$vulnerability_hits),
    mean(results_df$charity_agency_hits),
    mean(results_df$beneficiary_actor_vulnerability_hits),
    mean(results_df$charity_actor_agency_hits)
  ),
  
  SD = c(
    sd(results_df$vulnerability_hits),
    sd(results_df$charity_agency_hits),
    sd(results_df$beneficiary_actor_vulnerability_hits),
    sd(results_df$charity_actor_agency_hits)
  )
) %>%
  
  mutate(
    across(
      c(Proportion_Ads, Mean_Hits, SD),
      ~round(.x,2)
    )
  )


theme_table_long


write.csv(
  theme_table_long,
  file.path(RESULT_DIR,"Table2_theme_prevalence_hits.csv"),
  row.names = FALSE
)

# ==============================================================================
# FIGURE 1: SAMPLE DISTRIBUTION
# ==============================================================================

p1 <- ggplot(results_df,
             aes(x=Region))+
  geom_bar()+
  theme_minimal()+
  labs(
    title="Advertisement Distribution by Region",
    x="Advertisement Type",
    y="Number of Advertisements"
  )


ggsave(
  file.path(RESULT_DIR,"Figure1_sample_distribution.png"),
  p1,
  width=7,
  height=5
)


# ==============================================================================
# FIGURE 2: VULNERABILITY LANGUAGE
# ==============================================================================

p2 <- ggplot(results_df,
             aes(
               x=Region,
               y=vulnerability_hits
             ))+
  geom_boxplot()+
  theme_minimal()+
  labs(
    title="Vulnerability Language by Advertisement Type",
    x="",
    y="Vulnerability Word Count"
  )


ggsave(
  file.path(RESULT_DIR,"Figure2_vulnerability.png"),
  p2,
  width=7,
  height=5
)



# ==============================================================================
# FIGURE 3: CHARITY AGENCY LANGUAGE
# ==============================================================================

p3 <- ggplot(results_df,
             aes(
               x=Region,
               y=charity_agency_hits
             ))+
  geom_boxplot()+
  theme_minimal()+
  labs(
    title="Charity Agency Language by Advertisement Type",
    x="",
    y="Charity Agency Word Count"
  )


ggsave(
  file.path(RESULT_DIR,"Figure3_charity_agency.png"),
  p3,
  width=7,
  height=5
)


# ==============================================================================
# FIGURE 4: PROXIMITY MEASURES
# ==============================================================================


proximity_long <- results_df %>%
  dplyr::select(
    Region,
    beneficiary_actor_vulnerability_hits,
    charity_actor_agency_hits
  )%>%
  pivot_longer(
    cols=-Region,
    names_to="Measure",
    values_to="Count"
  )


p4 <- ggplot(
  proximity_long,
  aes(
    x=Region,
    y=Count
  )
)+
  geom_boxplot()+
  facet_wrap(~Measure, scales="free")+
  theme_minimal()+
  labs(
    title="Actor-Theme Proximity Measures",
    x="",
    y="Count"
  )


ggsave(
  file.path(RESULT_DIR,"Figure4_proximity.png"),
  p4,
  width=9,
  height=5
)



# ==============================================================================
# FIGURE 5: OCR / TEXT CONTROL CHECK
# ==============================================================================


p5 <- ggplot(results_df,
             aes(
               x=Region,
               y=NoiseRatio
             ))+
  geom_boxplot()+
  theme_minimal()+
  labs(
    title="OCR Noise Ratio by Advertisement Type",
    x="",
    y="Noise Ratio"
  )


ggsave(
  file.path(RESULT_DIR,"Figure5_OCR_noise.png"),
  p5,
  width=7,
  height=5
)



# ==============================================================================
# TABLE 3: NEGATIVE BINOMIAL MODELS
# ==============================================================================


robust_nb <- function(model){
  
  coeftest(
    model,
    vcov=sandwich
  )
  
}


model_list <- list(
  "Vulnerability" = model_vulnerability,
  "Charity Agency" = model_charity,
  "Beneficiary Passivity" = model_vulnerability_proximity,
  "Charity Agency Proximity" = model_charity_proximity
)



modelsummary(
  model_list,
  statistic="({std.error})",
  stars=TRUE,
  output=file.path(
    RESULT_DIR,
    "Table3_negative_binomial_models.html"
  )
)



# ==============================================================================
# TABLE 4: INCIDENCE RATE RATIOS
# ==============================================================================


extract_IRR <- function(model,name){
  
  broom::tidy(model) %>%
    mutate(
      IRR=exp(estimate),
      Lower=exp(estimate-1.96*std.error),
      Upper=exp(estimate+1.96*std.error),
      Model=name
    )
  
}


irr_table <- bind_rows(
  extract_IRR(model_vulnerability,"Vulnerability"),
  extract_IRR(model_charity,"Charity Agency"),
  extract_IRR(model_vulnerability_proximity,
              "Beneficiary Vulnerability Proximity"),
  extract_IRR(model_charity_proximity,
              "Charity Agency Proximity")
)


write.csv(
  irr_table,
  file.path(
    RESULT_DIR,
    "Table4_IRR_results.csv"
  ),
  row.names=FALSE
)



# ==============================================================================
# FIGURE 6: IRR PLOT
# ==============================================================================


irr_plot <- irr_table %>%
  filter(term=="Is_International") %>%
  ggplot(
    aes(
      x=Model,
      y=IRR,
      ymin=Lower,
      ymax=Upper
    )
  )+
  geom_point(size=3)+
  geom_errorbar(width=.2)+
  geom_hline(
    yintercept=1,
    linetype="dashed"
  )+
  coord_flip()+
  theme_minimal()+
  labs(
    title="Effect of Global South Advertisement Status",
    y="Incidence Rate Ratio",
    x=""
  )


ggsave(
  file.path(RESULT_DIR,"Figure6_IRR_plot.png"),
  irr_plot,
  width=8,
  height=5
)



# ==============================================================================
# TABLE 5: NONPARAMETRIC GROUP TESTS
# ==============================================================================


wilcox_table <- tibble(
  
  Outcome=c(
    "Vulnerability",
    "Charity Agency",
    "Beneficiary Passivity",
    "Charity Agency Proximity"
  ),
  
  p_value=c(
    
    wilcox.test(
      vulnerability_hits~Region,
      data=results_df
    )$p.value,
    
    
    wilcox.test(
      charity_agency_hits~Region,
      data=results_df
    )$p.value,
    
    
    wilcox.test(
      beneficiary_actor_vulnerability_hits~Region,
      data=results_df
    )$p.value,
    
    
    wilcox.test(
      charity_actor_agency_hits~Region,
      data=results_df
    )$p.value
    
  )
  
)


write.csv(
  wilcox_table,
  file.path(
    RESULT_DIR,
    "Table5_group_tests.csv"
  ),
  row.names=FALSE
)



cat(
  "\nRESULTS SECTION OUTPUT COMPLETE\nSaved to:",
  RESULT_DIR
)


#############################################################################################












# ==============================================================================
# RESULTS SECTION TABLES AND FIGURES
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(modelsummary)
library(broom)
library(lmtest)
library(sandwich)
library(gridExtra)
library(scales)


RESULT_DIR <- "Outputs/Results_Section/"
dir.create(
  RESULT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# PREPARE DATA
# ==============================================================================


results_df <- df %>%
  mutate(
    Region = case_when(
      Is_International == 1 ~ "Global South",
      Is_International == 0 ~ "Global North"
    )
  )


# ==============================================================================
# 4.1 DESCRIPTIVE STATISTICS
# ==============================================================================


# -------------------------------
# TABLE 1: SAMPLE CHARACTERISTICS
# -------------------------------


table1_sample <- results_df %>%
  group_by(Region) %>%
  summarise(
    Advertisements = n(),
    
    Mean_Words =
      round(mean(Total_Tokens, na.rm=TRUE),2),
    
    Median_Words =
      round(median(Total_Tokens, na.rm=TRUE),2),
    
    SD_Words =
      round(sd(Total_Tokens,na.rm=TRUE),2),
    
    Mean_OCR_Noise =
      round(mean(NoiseRatio,na.rm=TRUE),3)
  )


write.csv(
  table1_sample,
  file.path(
    RESULT_DIR,
    "Table1_Sample_Characteristics.csv"
  ),
  row.names=FALSE
)



# Figure 1: Advertisement numbers


fig1 <- ggplot(
  results_df,
  aes(x=Region)
)+
  geom_bar()+
  theme_minimal()+
  labs(
    title="Advertisement Distribution by Region",
    x=NULL,
    y="Number of Advertisements"
  )


ggsave(
  file.path(
    RESULT_DIR,
    "Figure1_Advertisement_Distribution.png"
  ),
  fig1,
  width=7,
  height=5
)



# ==============================================================================
# 4.2 VULNERABILITY REPRESENTATIONS
# ==============================================================================


# -------------------------------
# TABLE 2: Vulnerability descriptives
# -------------------------------


table2_vulnerability <- results_df %>%
  group_by(Region) %>%
  summarise(
    
    N=n(),
    
    Mean_Vulnerability =
      round(mean(vulnerability_hits),2),
    
    SD_Vulnerability =
      round(sd(vulnerability_hits),2),
    
    Ads_With_Vulnerability =
      sum(vulnerability_hits>0),
    
    Mean_Beneficiary_Vulnerability_Proximity =
      round(
        mean(
          beneficiary_actor_vulnerability_hits
        ),
        2
      )
    
  )


write.csv(
  table2_vulnerability,
  file.path(
    RESULT_DIR,
    "Table2_Vulnerability_Descriptives.csv"
  ),
  row.names=FALSE
)



# -------------------------------
# Figure 2: Vulnerability counts
# -------------------------------


fig2 <- ggplot(
  results_df,
  aes(
    x=Region,
    y=vulnerability_hits
  )
)+
  geom_boxplot()+
  theme_minimal()+
  labs(
    title="Vulnerability Language by Advertisement Type",
    x=NULL,
    y="Vulnerability Word Count"
  )


ggsave(
  file.path(
    RESULT_DIR,
    "Figure2_Vulnerability_Counts.png"
  ),
  fig2,
  width=7,
  height=5
)



# -------------------------------
# Figure 3: Beneficiary proximity
# -------------------------------


fig3 <- ggplot(
  results_df,
  aes(
    x=Region,
    y=beneficiary_actor_vulnerability_hits
  )
)+
  geom_boxplot()+
  theme_minimal()+
  labs(
    title="Beneficiary-Vulnerability Proximity",
    x=NULL,
    y="Proximity Count"
  )


ggsave(
  file.path(
    RESULT_DIR,
    "Figure3_Beneficiary_Vulnerability_Proximity.png"
  ),
  fig3,
  width=7,
  height=5
)



# -------------------------------
# Table 3: Vulnerability NB Model
# -------------------------------


modelsummary(
  list(
    "Vulnerability" =
      model_vulnerability,
    
    "Beneficiary Vulnerability Proximity" =
      model_vulnerability_proximity
  ),
  
  statistic =
    "({std.error})",
  
  stars=TRUE,
  
  output =
    file.path(
      RESULT_DIR,
      "Table3_Vulnerability_Models.html"
    )
  
)



# ==============================================================================
# 4.3 CHARITY AGENCY AND BENEFICIARY PASSIVITY
# ==============================================================================



# -------------------------------
# TABLE 4: Charity descriptives
# -------------------------------


table4_charity <- results_df %>%
  group_by(Region)%>%
  summarise(
    
    N=n(),
    
    Mean_Charity_Agency =
      round(
        mean(charity_agency_hits),
        2
      ),
    
    SD_Charity_Agency =
      round(
        sd(charity_agency_hits),
        2
      ),
    
    Ads_With_Charity_Agency =
      sum(charity_agency_hits>0),
    
    
    Mean_Charity_Actor_Proximity =
      round(
        mean(charity_actor_agency_hits),
        2
      )
    
  )



write.csv(
  table4_charity,
  file.path(
    RESULT_DIR,
    "Table4_Charity_Agency_Descriptives.csv"
  ),
  row.names=FALSE
)



# -------------------------------
# Figure 4: Charity agency
# -------------------------------


fig4 <- ggplot(
  results_df,
  aes(
    x=Region,
    y=charity_agency_hits
  )
)+
  geom_boxplot()+
  theme_minimal()+
  labs(
    title="Charity Agency Language by Advertisement Type",
    x=NULL,
    y="Charity Agency Word Count"
  )



ggsave(
  file.path(
    RESULT_DIR,
    "Figure4_Charity_Agency.png"
  ),
  fig4,
  width=7,
  height=5
)



# -------------------------------
# Figure 5: Charity proximity
# -------------------------------


fig5 <- ggplot(
  results_df,
  aes(
    x=Region,
    y=charity_actor_agency_hits
  )
)+
  geom_boxplot()+
  theme_minimal()+
  labs(
    title="Charity Actor-Agency Proximity",
    x=NULL,
    y="Proximity Count"
  )



ggsave(
  file.path(
    RESULT_DIR,
    "Figure5_Charity_Actor_Proximity.png"
  ),
  fig5,
  width=7,
  height=5
)



# -------------------------------
# Table 5: Charity NB Models
# -------------------------------


modelsummary(
  
  list(
    
    "Charity Agency" =
      model_charity,
    
    "Charity Actor Proximity" =
      model_charity_proximity
    
  ),
  
  statistic="({std.error})",
  
  stars=TRUE,
  
  output=
    file.path(
      RESULT_DIR,
      "Table5_Charity_Models.html"
    )
  
)



# ==============================================================================
# FIGURE 1: THEME DISTRIBUTION BY BENEFICIARY REGION
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)


# ------------------------------------------------------------------------------
# Prepare data
# ------------------------------------------------------------------------------

theme_data <- analysis_df %>%
  mutate(
    Region = case_when(
      Is_International == 1 ~ "Global South",
      Is_International == 0 ~ "Global North"
    )
  ) %>%
  dplyr::select(
    Region,
    vulnerability_prop,
    vulnerability_prox_prop,
    charity_agency_prop,
    charity_agency_prox_prop
  ) %>%
  pivot_longer(
    cols = c(
      vulnerability_prop,
      vulnerability_prox_prop,
      charity_agency_prop,
      charity_agency_prox_prop
    ),
    names_to = "Theme",
    values_to = "Proportion"
  ) %>%
  mutate(
    Theme = recode(
      Theme,
      vulnerability_prop = "Vulnerability",
      vulnerability_prox_prop = "Beneficiary\nVulnerability",
      charity_agency_prop = "Charity Agency",
      charity_agency_prox_prop = "Charity Agency\nProximity"
    )
  )


# ==============================================================================
# FIGURE 1: Theme distribution by beneficiary region
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)


# Create plotting dataset

theme_data <- results_df %>%
  mutate(
    Region = ifelse(Is_International == 1,
                    "Global South",
                    "Global North")
  ) %>%
  dplyr::select(
    Region,
    vulnerability_hits,
    beneficiary_actor_vulnerability_hits,
    charity_agency_hits,
    charity_actor_agency_hits
  ) %>%
  pivot_longer(
    cols = c(
      vulnerability_hits,
      beneficiary_actor_vulnerability_hits,
      charity_agency_hits,
      charity_actor_agency_hits
    ),
    names_to = "Theme",
    values_to = "Count"
  ) %>%
  mutate(
    Theme = dplyr::recode(
      Theme,
      vulnerability_hits = "Vulnerability",
      beneficiary_actor_vulnerability_hits = 
        "Vulnerability\n(Proximity)",
      charity_agency_hits = "Charity Agency",
      charity_actor_agency_hits =
        "Charity Agency\n(Proximity)"
    )
  )


# Plot

figure1 <- ggplot(
  theme_data,
  aes(
    x = Region,
    y = Count,
    fill = Region
  )
) +
  
  geom_boxplot(
    alpha = 0.6,
    outlier.shape = NA
  ) +
  
  geom_jitter(
    width = 0.15,
    alpha = 0.35,
    size = 1.8
  ) +
  
  facet_wrap(
    ~Theme,
    scales = "free_y",
    ncol = 2
  ) +
  
  labs(
    title = "Distribution of Thematic Language by Beneficiary Region",
    x = NULL,
    y = "Number of Dictionary Matches"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    legend.position = "none",
    strip.text = element_text(face="bold"),
    plot.title = element_text(face="bold")
  )


figure1


# Save

ggsave(
  "Figure1_Theme_Distribution_by_Region.png",
  figure1,
  width = 10,
  height = 8,
  dpi = 300
)






# ==============================================================================
# FIGURE 3: Charity Agency and Vulnerability Language by Beneficiary Region
# ==============================================================================

library(dplyr)
library(ggplot2)


# Create plotting dataset

figure3_data <- results_df %>%
  mutate(
    Region = ifelse(
      Is_International == 1,
      "Global South",
      "Global North"
    )
  ) %>%
  dplyr::select(
    Region,
    charity_agency_hits,
    vulnerability_hits
  )


# Create scatterplot

figure3 <- ggplot(
  figure3_data,
  aes(
    x = charity_agency_hits,
    y = vulnerability_hits,
    colour = Region
  )
) +
  
  geom_point(
    size = 3,
    alpha = 0.7
  ) +
  
  geom_smooth(
    method = "lm",
    se = FALSE,
    linetype = "dashed"
  ) +
  
  labs(
    title = "Relationship Between Charity Agency and Vulnerability Language",
    subtitle = "By beneficiary region",
    x = "Charity Agency Language (Dictionary Matches)",
    y = "Vulnerability Language (Dictionary Matches)",
    colour = "Beneficiary Region"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )


figure3

ggsave(
  "Figure3_Charity_Agency_Vulnerability_Scatterplot.png",
  figure3,
  width = 8,
  height = 6,
  dpi = 300
)


figure3_data <- results_df %>%
  mutate(
    Region = ifelse(
      Is_International == 1,
      "Global South",
      "Global North"
    )
  ) %>%
  dplyr::select(
    Region,
    charity_actor_agency_hits,
    beneficiary_actor_vulnerability_hits
  )


figure3 <- ggplot(
  figure3_data,
  aes(
    x = charity_actor_agency_hits,
    y = beneficiary_actor_vulnerability_hits,
    colour = Region
  )
) +
  geom_point(
    size = 3,
    alpha = 0.7
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linetype = "dashed"
  ) +
  labs(
    title = "Charity Agency and Beneficiary Vulnerability Proximity",
    x = "Charity Actor Agency Language",
    y = "Beneficiary Vulnerability Language",
    colour = "Beneficiary Region"
  ) +
  theme_minimal(base_size = 13)

figure3


# Save

ggsave(
  "Figure3_Charity_Agency_Vulnerability_Proximity_Scatterplot.png",
  figure3,
  width = 8,
  height = 6,
  dpi = 300
)






library(quanteda)

# Create a quanteda corpus from your dataframe
my_corpus <- corpus(analysis_df, text_field = "text")

# Extract the Key Word In Context (KWIC) for Vulnerability words
# window = 5 means 5 words before and 5 words after
vuln_kwic <- kwic(tokens(my_corpus), 
                  pattern = expanded_dictionaries$vulnerability, 
                  window = 5)

# Convert to a readable dataframe and view the first 20 hits
vuln_kwic_df <- as.data.frame(vuln_kwic)
head(vuln_kwic_df, 50)

# Optional: Filter to see ONLY how International Ads use the words
# (Requires linking the docnames back to your Is_International variable)

