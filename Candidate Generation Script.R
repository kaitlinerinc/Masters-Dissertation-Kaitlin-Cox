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

# --- CONFIGURATION VARIABLES ---
NEWSPAPER_INPUT <- "C:/Users/unity/Documents/Dissertation/Final_Cleaned_Newspaper_Ads.rds"
CODING_FILE     <- "C:/Users/unity/Documents/Dissertation/Advertisement Coding.xlsx"
EMBEDDING_PATH  <- "dolma_300_2024_1.2M.100_combined.txt"
EXPORT_DIR      <- "C:/Users/unity/Documents/Dissertation/Outputs/"

# Single threshold for all dictionaries
COSINE_THRESHOLD <- 0.75 
TOP_N_WORDS      <- 500

dir.create(EXPORT_DIR, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# 2. HELPER FUNCTIONS
# ==============================================================================
build_regex_pattern <- function(word_list) {
  if (length(word_list) == 0) return("(?!x)x")
  escaped <- str_replace_all(word_list, "([\\.^$|()\\[\\]{}*+?\\\\-])", "\\\\\\1")
  paste0("\\b(", paste(escaped, collapse = "|"), ")\\b")
}

prepare_ads <- function(df_raw) {
  df_raw %>%
    distinct(Ad_Base_ID, .keep_all = TRUE) %>%
    mutate(
      Source_Type = "Newspaper",
      text = CleanedText %>% 
        replace_na("") %>% 
        str_to_lower() %>%
        str_replace_all("[.!?]+", " zzbreakzz ") %>% 
        str_replace_all("[^a-z\\s]", " ") %>%        
        str_replace_all("\\s+", " ") %>%             
        str_trim()
    ) %>%
    filter(text != "")
}

expand_with_threshold <- function(seeds,
                                  embedding_matrix,
                                  threshold,
                                  top_n = 250) {
  
  valid_seeds <- seeds[seeds %in% rownames(embedding_matrix)]
  
  if (length(valid_seeds) == 0) {
    warning("No seed words found in embeddings")
    return(tibble(word = character(), similarity = numeric()))
  }
  
  sims <- sim2(
    x = embedding_matrix,
    y = embedding_matrix[valid_seeds, , drop = FALSE],
    method = "cosine",
    norm = "l2"
  )
  
  max_similarity <- apply(sims, 1, max)
  
  tibble(
    word = rownames(embedding_matrix),
    similarity = max_similarity
  ) %>%
    filter(
      str_detect(word, "^[a-z]{3,}$"),
      !(word %in% stopwords("en")),
      similarity >= threshold
    ) %>%
    arrange(desc(similarity)) %>%
    slice_head(n = top_n)
}

# ==============================================================================
# 3. LOAD DATA & EMBEDDINGS
# ==============================================================================
df <- prepare_ads(readRDS(NEWSPAPER_INPUT)) %>%
  left_join(read_excel(CODING_FILE) %>% dplyr::select(Ad_Base_ID, Is_International, Is_Animal), by = "Ad_Base_ID") %>%
  filter(Is_Animal == 0, !is.na(Is_International))

cat("\nAdvertisements retained:", nrow(df), "\n")

vectors <- fread(EMBEDDING_PATH, header = FALSE, sep = " ", fill = TRUE, quote = "", colClasses = c("character", rep("numeric", 300)))
embedding_matrix <- as.matrix(vectors[, -1])
rownames(embedding_matrix) <- vectors[[1]]
rm(vectors); gc()

# ==============================================================================
# 4. SEED DICTIONARIES & SEMANTIC EXPANSION
# ==============================================================================
seed_dictionaries <- list(
  vulnerability            = c("helplessness", "hopelessness", "suffering", "victim", "dependancy", "vulnerable", "starving", "poverty"),
  global_north_agency      = c("protect", "benevolence", "giver", "aid", "assistance", "rescuing", "heroic", "savior")
)

# Iterates through all seed dictionaries using the single threshold
expanded_raw <- map(seed_dictionaries, ~expand_with_threshold(
  seeds            = .x, 
  embedding_matrix = embedding_matrix, 
  threshold        = COSINE_THRESHOLD, 
  top_n            = TOP_N_WORDS
))

# Automatically write out CSVs for all dictionaries
iwalk(expanded_raw, ~write_csv(.x, file.path(EXPORT_DIR, paste0(.y, "_similarity.csv"))))

# Combine original seed words with newly found expanded words
expanded_dictionaries <- map2(seed_dictionaries, expanded_raw, ~unique(c(.x, .y$word)))

# ==============================================================================
# 5. MORPHOLOGY, DICTIONARY CLEANING & EXCLUSIONS
# ==============================================================================

unrelated_vulnerability <- c("overcome", "life", "consequences", "experiencing", "cope", "consequence",
                             "self", "emotional", "situation",
                             "dealing", "concern", "ultimately",
                             "overwhelming", "causes", "sense", "realize", "despite", "cause",
                             "others", "feeling",
                             "serious", "suicide", "people", "caused", "knowing", "aware",
                             "leads", "ironically", "severe", "realizing", 
                             "circumstances", "unfortunately", "problem", "problems", "reality", "fact",
                             "psychological", "addiction", "doubt", "mind", 
                             "feelings", "causing", "concerned",
                             "concerning", "concerns", "coped", "copes", "coping", "facts",
                             "lifes", "minded", "minding", "minds", 
                             "overcame", "overcomes", "overcoming", "peopled", "peoples", "peopling",
                             "realities", "realized", "realizes", "selves", "sensed", "senses", "sensing",
                             "severer", "severest",  "situations", "spited",
                             "spites", 'spiting', "suicides", "unabled", "unables", "unabling",
                             "vulnerabler", "vulnerablest", "worsted", "worsting", 
                             "suspect", murder, inequality, loneliness, struggle, perpetrator,
                             sadness, hungry, abuse, worse, fear, death, affected, protect, treating,
                             incident, worthlessness, person, alleged, starve, mental, accused, misery,
                             ill, harm, dying, struggling, unfortunate, innocent, case, someone, protecting,
                             terrible, allegedly, potentially, poor, guilty, murdered, apathy, particularly,
                             attempted, depression, child, due, inflicted, sickness, grief, condition,
                             starved, homelessness, worst, woman, alone, struggles, crime, sick, chronic,
                             painful, confronted, witness, shame, despondency, burden, endured, spite,
                             economic, threat, endure, treat, failure, pains, criminal, somebody, accident,
                             affects, horrible, hurt, debilitating, reasons, claiming, police, another,
                             sadly, claimed, result, indeed, stress, assaulted, lack, unaware, apparently,
                             arrest, unemployment, especially, reason, often, happened, difficulties,
                             exposed, susceptible, danger, bad, threats, suspects, admitted, risk, surely,
                             trauma, arrested, caught, hence, nothing, likely, finds, remain, vulnerability,
                             brought, rape, however, violence, taken, thus,
                             somehow, heart, illnesses, symptoms, extent, believe, unable, happens, losing,
                             threatened, anxiety, telling, killed, dangerous, affecting, sees, possibly,
                             mother, prevent, weak, neither, dealt, regardless, fragile,
                             existence, absence, convinced, physical, yet, perhaps, sad, difficult, lost,
                             loathing, become, issues, man, clearly, starvation, imagine, truly, presumed,
                             supposed, desperation, unhappiness, claims, afflicted, emptiness, much, prone,
                             may, increasingly, even, similarly, sometimes, worry, alleviate, therefore,
                             killing, importantly, survive, might, possibility, feel, children, suspected,
                             never, extremely, individuals, abused, although, facing, lead, absences,
                             abuses, abusing, accidents, alleviated, alleviates, alleviating, anxieties,
                             arresting, arrests, assaulting, assaults, became, becomes, becoming, believed,
                             believes, believing, burdened, burdening, burdens, cased, cases, casing,
                             childs, chronics, conditioned, conditioning, conditions, crimes, criminals,
                             dangers, deaths, dependancies, depressions, despaired, despairing, despairs,
                             diseases, dues, endures, enduring, evened, evening, evenner, evennest,
                             evens, existences, extents, facings, failures, feared, fearing, fears, feels,
                             felt, griefs, guiltier, guiltiest, harmed, harming, harms, hearts, hungrier,
                             hungriest, hurting, hurts, iller, illest, ills, imagined, imagines, imagining,
                             incidents, inequalities, innocents, killings, lacked, lacking, lacks, leading,
                             led, likelier, likeliest, manned, manning, mans, mayed, maying, mays, men,
                             mighted, mighting, mights, miseries, more, most, mothered, mothering, mothers,
                             murdering, murders, nothings, oftener, oftenest, pained, paining, perpetrators,
                             persons, physicals, policed, polices, policing, poorer, poorest, possibilities,
                             prevented, preventing, prevents, protected, protects, raped, rapes, raping,
                             reasoned, reasoning, remained, remaining, remains, resulted, resulting, results,
                             risked, risking, risks, sadder, saddest, shamed, shames, shaming, sicker,
                             sickest, sicknesses, starvations, starves, stressed, stresses, stressing,
                             struggled, sufferings, survived, survives, surviving, suspecting, tellings,
                             traumas, traumata, treated, treats, unfortunates, vulnerabilities, weaker,
                             weakest, witnessed, witnesses, witnessing, women, worried, worries, worrying,
                             worsts 
                             
)

unrelated_charity <- c(
  "must", "another", "one", "upon", "besides", "part", "also", "promise", "hope", "peace", "necessary",
  "important", "keeping", "personal", "effort", "intended", "god", "respect", "wish",
  "take", "able", "sacrifice", "taken", "great", "whatever", "lastly", "every", "well",
  "responsible", "takes", "mention", "efforts", "comes", "keep",
  "let", "good", "willing", "continue", "opportunity", "consider", "everyone", "alive", "worthy",
  "forget", "finding", "make", "man", "future", "idea", "turn", "instead",
  "particular", "present", "vital", "example", "certainly", "chosen", "taking", "can",
  "makes", "come", "spirit", "supposedly", "return", "individual", "ability", "presumably",
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
  "believing", "calling", "calls", "chanced", "chances", "chancing", "changed", "changes", "changing", "childs", "choicer",
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
  "spoken", "tasked", "tasking", "tasks"
)

# Align exclusion lists with seed dictionary keys
unrelated_words <- list(
  vulnerability            = unrelated_vulnerability,
  global_north_agency      = unrelated_charity
)

# Expand both the dictionaries and the exclusions to include lemmas
unrelated_expanded    <- map(unrelated_words, expand_with_lemmas)
expanded_dictionaries <- map(expanded_dictionaries, expand_with_lemmas)

# Subtract excluded words from their respective dictionaries
expanded_dictionaries <- map2(expanded_dictionaries, unrelated_expanded, setdiff)

# Custom rule: Global North agency terms cannot overlap with vulnerability terms
if (!is.null(expanded_dictionaries$global_north_agency) && !is.null(expanded_dictionaries$vulnerability)) {
  expanded_dictionaries$global_north_agency <- setdiff(
    expanded_dictionaries$global_north_agency, 
    expanded_dictionaries$vulnerability
  )
}

# Print reports for all dictionaries
cat("\n================ FINAL DICTIONARY REPORTS ================\n")
walk2(expanded_dictionaries, names(expanded_dictionaries), ~print_dictionary_report(.x, .y, df))
