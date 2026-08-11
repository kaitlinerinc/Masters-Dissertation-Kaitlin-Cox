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

expand_with_lemmas <- function(dictionary){
  lemma_matches <- lexicon::hash_lemmas %>% filter(lemma %in% dictionary) %>% pull(token)
  unique(c(dictionary, lemma_matches))
}

# ==============================================================================
# 3. LOAD DATA & EMBEDDINGS
# ==============================================================================
df <- prepare_ads(readRDS(NEWSPAPER_INPUT)) %>%
  left_join(read_excel(CODING_FILE) %>% dplyr::select(Ad_Base_ID, Is_International, Is_Animal), by="Ad_Base_ID") %>%
  filter(Is_Animal == 0, !is.na(Is_International))

vectors <- fread(EMBEDDING_PATH, header=FALSE, sep=" ", fill=TRUE, quote="", colClasses=c("character", rep("numeric",300)))
embedding_matrix <- as.matrix(vectors[,-1])
rownames(embedding_matrix) <- vectors[[1]]
rm(vectors); gc()

# ==============================================================================
# 4. SEED DICTIONARIES & SEMANTIC EXPANSION
# ==============================================================================
seed_dictionaries <- list(
  vulnerability = c("helplessness", "hopelessness", "poverty", "suffering", "victim", "powerless", "depend"),
  charity_agency = c("protect", "benevolence", "giver", "aid", "assistance", "rescuing", "heroic", "savior", "donate", "gift")
)

vulnerability_raw <- expand_with_threshold(seed_dictionaries$vulnerability, embedding_matrix, 0.75, 1000)
charity_raw <- expand_with_threshold(seed_dictionaries$charity_agency, embedding_matrix, 0.75, 1000)

expanded_dictionaries <- list(
  vulnerability = unique(c(seed_dictionaries$vulnerability, vulnerability_raw$word)),
  charity_agency = unique(c(seed_dictionaries$charity_agency, charity_raw$word))
)

# ==============================================================================
# 5. MORPHOLOGY, DICTIONARY CLEANING & EXCLUSIONS
# ==============================================================================
unrelated_vulnerability <- c("lives", "life", "consequence", "sense", "consequences", "ultimately", "self", "spite", "situation",
                             "circumstances", "frustration", "realize", "guilt", "reality", "knowing", "overwhelming", "others", 
                             "worse", "anger", "concern", "despite", "aware", "emotional", "sadness", "feeling",
                             "extent", "doubt", "fact", "indeed", "existence", "dealing", "unfortunate", "apparent", "terrible", "realizing",
                             "experiencing", "feelings", "beyond", "perceived", "people", "truth",
                             "cope", "mind", "ironically", "unfortunately", "cause", "sad", "worst", "blame", "hence",
                             "nothing", "reason", "thus", "leads", "neither", "regardless", "becomes", "often",
                             "causes", "kind", "acknowledge", "reasons", "understand",
                             "surely", "evident", "sadly", "caused", "rather", "desire", "result",
                             "problem", "recognize", "however", "perhaps", "secondly", "yet", "due", "imagine",
                             "inevitably", "become", "serious", "person", "believe", "concerned",
                             "meant", "therefore", "problems", "confusion", "sort", "wonder",
                             "attention", "feel", "somehow", "happens", "situations", "realise", "means", "brought", "ways", "moment", "responsibility",
                             "necessarily", "humanity", "clearly",
                             "bad", "inevitable", "describe", "belief", "without", "supposed", "matter", "truly",
                             "necessity", "otherwise", "ignorance", "unaware", "latter", "true", "might",
                             "sometimes", "importantly", "past", "regard", "especially", "mean", "psychological",
                             "hardly", "else", "even", "exists", "obvious", "likely", "none", "given", "way", 
                             "child", "realization", "seek", "lead", "almost", "similarly", "much", "merely",
                             "possibility", "never", "still", "thoughts", "convinced", "felt", "partly", "simply", "seeing",
                             "happiness", "understood", "seemingly","rest", "conversely", "denial", "human",
                             "towards", "paradoxically", "immediate", "continuing", "relate", "extreme",
                             "lies", "longer", "circumstance", "seems", "failing", "worry", "possible", "dealt",
                             "whether", "believing", "whose", "oftentimes", "sees", "explain", "meaning",
                             "point", "always", "contraries",
                             "acknowledged", "acknowledges", "acknowledging", "angered", "angering", "angers",
                             "apparenter", "apparentest", "attentions", "became", "becoming", "beliefs",
                             "believed", "believes", "blamed", "blames", "blaming", "contrary", "giving", "thought", "confronted",
                             "causing", "children", "childs", "concerning", "concerns", "conflicted", "conflicting",
                             "conflicts", "confusions", "denials",
                             "described", "describes", "describing", "desired", "overcome", "overcame", "overcomes", "overcoming",
                             "desires", "desiring", "doubted", "doubting", "doubts", "dues", "escaped", "escapes", "escaping", "evened", "evening", "evenner",
                             "evennest", "evens", "existences", "explained", "explaining", "explains", "extents", "extremer",
                             "extremes", "extremest", "facts", "feels", "felts",
                             "frustrations", "guilts",
                             "humanities", "humans", "imagined", "imagines", "imagining", "kinder", "kindest", "kinds",
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
                       "responsible", "takes", "mention", "efforts", "comes", "keep", "bring", "brought",
                       "let", "good", "willing", "continue", "opportunity", "consider", "everyone", "alive", "worthy",
                       "forget", "finding", "make", "man", "future", "idea", "turn", "instead",
                       "particular", "present", "vital", "example", "certainly", "chosen", "taking", "can",
                       "makes", "come", "spirit", "supposedly", "return", "individual",  "ability", "presumably",
                       "force", "possibly", "may", "put", "enough", "hopes", "devoted", "noveljess",
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
                       "people", "imagine", "despite", "seeking", "else", "never", "circumstances", "concern", "matter", "similarly", "reasons", "needing",
                       "understand", "unfortunately", "many", "sure", "spite", "solely", "role", "rely",
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
                       "particularly", "meantime", "honest", "implies", "real", "already", "necessarily",
                       "hardly", "tell", "regarding", "allowed", "prove", "encourage", "existence", "complete",
                       "impossible", "commitment", "thirdly", "lead", "father", "change", "extraordinary", "lost",
                       "world", "difficult", "past", "children", "kindness", "like", "remain", "point", "acknowledged", "acknowledges",
                       "acknowledging", "acted", "acting", "actioned", "actioning", "additions",
                       "allows", "asked", "asking", "asks", "assumed", "assumes",
                       "assuming", "attempted", "attempting", "attempts", "attentions", "became", "becoming",
                       "believing", "calling", "work", "recieve",
                       "calls", "chanced", "chances", "chancing", "childs", "choicer",
                       "choices", "choicest", "cleared", "clearer", "clearest", "clearing", "clears", "commitments",
                       "completed", "completer", "completes", "completest", "completing", "concerning", "concerns",
                       "desired", "desires", "desiring", "doubted", "doubting",
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
                       "absolutely", "gone", "manner", "ironically",
                       "advantaged", "advantages", "advantaging", "assures", "assuring",
                       "considerations", "creations", "deals", "dealt",
                       "exceptions", "extents", "failed", "failing", "fails", "families", "fulfilled",
                       "fulfilling", "fulfills", "gained", "gaining", "gains", "givens", "likelier",
                       "likeliest", "manners", "meaner", "meanest", "natures",
                       "sooner", "soonest", "spake", "speaking", "speaks", "spoke",
                       "spoken", "tasked", "tasking", "tasks",
                       
                       
                       
                       
                       "happy", "love", "get", "choose", "decide", "send", "received", "choosing",
                       "dedicated", "charity", "share", "forever", "new", "day", "thank", "ready", "accomplish", "proud",
                       "sent", "friends", "set", "expect", "just", "anybody", "decided",
                       "establish", "enter", "advance", "please", "going", "deserve", "somebody",
                       "reward", "receiving", "helps", "since", "begin", "nobody", "wonderful", "home",
                       "luck", "name", "plan", "free", "actions", "today", "little",
                       "appropriate", "almost", "talk", "members", "organization", "whoever", "thinking", "course",
                       "join", "beloved", "gather", "question", "welcome", "basically", "extra", "interest",
                       "hopefully", "learn", "anyway", "mother", "sadly", "friend", "heart", "required", "meanwhile",
                       "interested", "think", "beginning", "ensure", "back", "happens", "agree", "created", "reach",
                       "next", "sometimes", "try", "getting", "sending", "worth", "suggest", "pay",
                       "useful", "really", "perfect", "advice", "maybe", "learned", "living", "fair", "answer", "waiting",
                       "note", "lot", "hard", "case", "seriously", "helpful", "treat", "reminder",
                       "folks", "sharing", "managed", "worry", "looking", "look", "spend", "expecting", "entirely",
                       "parents", "additionally", "favor", "secret", "similar", "stay", "use",
                       "dead", "lucky", "definitely", "outside", "essential", "guarantee", "sufficient",
                       "normally", "close", "probably", "included", "current", "active", "grace",
                       "behind", "glad", "second", "pleased", "shared", "ultimate", "message",
                       "status", "require", "within", "surprise", "assisting", "feel", "happen", "raised",
                       "years", "literally", "claim", "less", "bear", "praise", "explain", "sign",
                       "participate", "convince", "deserves", "two", "additional", "request", "progress",
                       "thousands", "citizens", "creating", "challenge", "story", "fully", "act", "potential",
                       "accomplishes", "accomplishing", "advanced", "advances", "advancing",
                       "advices", "agreed", "agreeing", "agrees", "aided", "aiding", "aids", "answered", "answering",
                       "answers", "appreciated", "appreciates", "appreciating", "appreciations", "appropriated",
                       "appropriates", "appropriating", "assisted", "assists", "backed", "backing", "backs", "bearing",
                       "bears", "began", "beginnings", "begins", "begun", "behinds", "behinds.", "beloveds",
                       "bore", "born", "borne", "cased", "cases", "casing",
                       "challenged", "challenges", "challenging", "charities", "chooses", "claimed", "claiming",
                       "claims", "closed", "closer", "closes", "closest", "closing",
                       "convinces", "convincing", "coursed", "courses", "coursing", "currents", "days",
                       "deader", "deadest", "deads", "decides", "deciding",
                       "deserved", "deserving", "ensured", "ensures", "ensuring",
                       "entered", "entering", "enters", "essentials", "establishing", "expected", "expects",
                       "explained", "explaining", "explains", "extras", "faired", "fairer", "fairest", "fairing",
                       "fairs", "favored", "favoring", "favors", "feeling", "feels", "felt", "freed", "freeing", "freer",
                       "frees", "freest", "gathered", "gathering", "gathers", "gets",
                       "gladder", "gladdest", "got", "gotten", "graced", "graces", "gracing",
                       "happened", "happening", "happier", "happiest",
                       "harder", "hardest", "hearts", "homed", "homes", "homing", "interesting", "interests", "joined",
                       "joining", "joins", "juster", "justest", "learning", "learns", "learnt", "littler", "littlest",
                       "livings", "looked", "looks", "lots", "loves", "lucked", "luckier", "luckiest", "lucking",
                       "lucks", "messaged", "messages", "messaging", "moneys", "monies", "mothered", "mothering",
                       "mothers", "named", "names", "naming", "newer", "newest", "nobodies", "noted", "notes", "noting",
                       "organizations", "outsides", "paid", "participated", "participates",
                       "participating", "payed", "paying", "pays", "perfected", "perfecting", "perfects", "planned",
                       "planning", "plans", "pleases", "pleasing", "potentials", "praised", "praises", "praising",
                       "progressed", "progresses", "progressing", "prouder", "proudest",
                       "questioned", "questioning", "questions", "reached", "reaches",
                       "reaching", "readied", "readier", "readies", "readiest", "readying", "receives", "reminders",
                       "requested", "requesting", "requests", "requiring",
                       "seconded", "seconding", "seconds", "secrets",
                       "sends", "sets", "setting", "shares", "signed", "signing", "signs", "spending", "spends", "spent",
                       "staid", "statuses", "stayed", "staying", "stays", "stories", "suggested", "suggesting",
                       "suggests", "surprised", "surprises", "surprising", "talked",
                       "talking", "talks", "thanked", "thanking", "thinked", "thinks", "todays", "treated", "treating",
                       "treats", "tried", "tries", "twos", "used", "uses", "using",
                       "welcomed", "welcomes", "welcoming", "workings", "worried", "worries", "worrying",
                       "worths")

unrelated_vulnerability_expanded <- expand_with_lemmas(unrelated_vulnerability)
unrelated_charity_expanded <- expand_with_lemmas(unrelated_charity)

expanded_dictionaries <- lapply(expanded_dictionaries, expand_with_lemmas)

expanded_dictionaries$vulnerability <- setdiff(expanded_dictionaries$vulnerability, unrelated_vulnerability_expanded)
expanded_dictionaries$charity_agency <- setdiff(expanded_dictionaries$charity_agency, unrelated_charity_expanded)
expanded_dictionaries$charity_agency <- setdiff(expanded_dictionaries$charity_agency, expanded_dictionaries$vulnerability)

cat("\n================ FINAL DICTIONARY REPORTS ================\n")
walk2(expanded_dictionaries, names(expanded_dictionaries), ~print_dictionary_report(.x, .y, df))

# ==============================================================================
# 5. CORPUS PROXIMITY CALCULATION & DATA PREP
# ==============================================================================
results_df <- df %>%
  mutate(
    vulnerability_hits = stri_count_regex(text, build_regex_pattern(expanded_dictionaries$vulnerability)),
    charity_agency_hits = stri_count_regex(text, build_regex_pattern(expanded_dictionaries$charity_agency)),
    vulnerability_present = ifelse(vulnerability_hits > 0, 1, 0),
    log_valid_words = log(Dictionary_Words + 1),
    Region = ifelse(Is_International == 1, "Global South", "Global North")
  )

# ===============================
# MAIN MODELS
# ===============================

model_vulnerability <- glm.nb(
  vulnerability_hits ~ Is_International + NoiseRatio +
    offset(log_valid_words),
  data = results_df
)

model_charity <- glm.nb(
  charity_agency_hits ~ Is_International + NoiseRatio +
    offset(log_valid_words),
  data = results_df
)

model_binary <- glm(
  vulnerability_present ~ Is_International + NoiseRatio,
  family = binomial(),
  data = results_df
)

# ===============================
# INTERACTION MODEL
# ===============================

model_main <- glm.nb(
  charity_agency_hits ~ vulnerability_hits +
    Is_International +
    NoiseRatio +
    offset(log_valid_words),
  data = results_df
)

model_interaction <- glm.nb(
  charity_agency_hits ~ vulnerability_hits *
    Is_International +
    NoiseRatio +
    offset(log_valid_words),
  data = results_df
)

lrt <- anova(model_main, model_interaction, test = "LRT")

write.csv(
  broom::tidy(lrt),
  "Likelihood_Ratio_Test.csv",
  row.names = FALSE
)

# ===============================
# IRRs
# ===============================

write.csv(
  as.data.frame(exp(cbind(IRR=coef(model_vulnerability),
                          confint(model_vulnerability)))),
  "IRR_Vulnerability.csv"
)

write.csv(
  as.data.frame(exp(cbind(IRR=coef(model_charity),
                          confint(model_charity)))),
  "IRR_Charity.csv"
)

write.csv(
  as.data.frame(exp(cbind(IRR=coef(model_interaction),
                          confint(model_interaction)))),
  "IRR_Interaction.csv"
)

# ===============================
# MODEL TABLE
# ===============================

coef_map <- c(
  "(Intercept)"="Intercept",
  "Is_International1"="Global South",
  "Is_International"="Global South",
  "NoiseRatio"="Noise Ratio",
  "vulnerability_hits"="Vulnerability",
  "vulnerability_hits:Is_International1"="Vulnerability × Global South"
)

modelsummary(
  list(
    "Vulnerability" = model_vulnerability,
    "Charity Agency" = model_charity,
    "Interaction" = model_interaction
  ),
  exponentiate = TRUE,
  coef_map = coef_map,
  statistic = "conf.int",
  stars = TRUE,
  output = file.path(EXPORT_DIR, "Regression_Tables.docx")
)

# ===============================
# WILCOXON TESTS
# ===============================

write.csv(
  broom::tidy(wilcox.test(vulnerability_hits~Region,data=results_df)),
  "Wilcoxon_Vulnerability.csv",
  row.names=FALSE
)

write.csv(
  broom::tidy(wilcox.test(charity_agency_hits~Region,data=results_df)),
  "Wilcoxon_Charity.csv",
  row.names=FALSE
)

# ===============================
# SPEARMAN CORRELATION
# ===============================

write.csv(
  broom::tidy(cor.test(
    results_df$vulnerability_hits,
    results_df$charity_agency_hits,
    method="spearman")),
  "Spearman.csv",
  row.names=FALSE
)

# ===============================
# INTERACTION PLOT
# ===============================

newdat <- expand.grid(
  vulnerability_hits=seq(min(results_df$vulnerability_hits),
                         max(results_df$vulnerability_hits),
                         length.out=100),
  Is_International=c(0,1),
  NoiseRatio=mean(results_df$NoiseRatio),
  log_valid_words=mean(results_df$log_valid_words)
)

newdat$Prediction <- predict(model_interaction,
                             newdata=newdat,
                             type="response")

write.csv(
  newdat,
  "Interaction_Predictions.csv",
  row.names = FALSE
)

newdat$Region <- factor(newdat$Is_International,
                        labels=c("Global North","Global South"))

p <- ggplot(newdat,
            aes(vulnerability_hits,Prediction,color=Region))+
  geom_line(linewidth=1.2)+
  theme_minimal(base_size=13)+
  labs(
    x="Vulnerability dictionary hits",
    y="Predicted charity agency hits",
    title="Interaction between Vulnerability and Beneficiary Region"
  )

ggsave("Interaction_Plot.png",p,width=8,height=6,dpi=300)

# ===============================
# DIAGNOSTICS
# ===============================

library(pscl)

model_vulnerability_zi <- zeroinfl(
  vulnerability_hits~Is_International+NoiseRatio+
    offset(log_valid_words),
  data=results_df)

model_charity_zi <- zeroinfl(
  charity_agency_hits~Is_International+NoiseRatio+
    offset(log_valid_words),
  data=results_df)

print(AIC(model_vulnerability,model_vulnerability_zi))
print(AIC(model_charity,model_charity_zi))

print(summary(model_vulnerability))
print(summary(model_charity))
print(summary(model_interaction))

pscl::pR2(model_vulnerability)
pscl::pR2(model_charity)
pscl::pR2(model_interaction)

# ==============================================================================
# DICTIONARY VALIDATION: WORD FREQUENCIES
# ==============================================================================

library(tidytext)

# Tokenize corpus
token_df <- results_df %>%
  dplyr::select(Ad_Base_ID, Region, text) %>%
  unnest_tokens(word, text)

# Vulnerability frequencies
vulnerability_freq <- token_df %>%
  filter(word %in% expanded_dictionaries$vulnerability) %>%
  count(word, sort = TRUE)

write.csv(
  vulnerability_freq,
  file.path(EXPORT_DIR, "Vulnerability_Word_Frequencies.csv"),
  row.names = FALSE
)

# Charity Agency frequencies
charity_freq <- token_df %>%
  filter(word %in% expanded_dictionaries$charity_agency) %>%
  count(word, sort = TRUE)

write.csv(
  charity_freq,
  file.path(EXPORT_DIR, "Charity_Agency_Word_Frequencies.csv"),
  row.names = FALSE
)

# Top 30 vulnerability words plot
vulnerability_freq %>%
  slice_max(n, n = 30) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Most Frequent Vulnerability Dictionary Words",
    x = NULL,
    y = "Frequency"
  )

ggsave(
  file.path(EXPORT_DIR, "Vulnerability_Top30.png"),
  width = 8,
  height = 6,
  dpi = 300
)

# Top 30 charity agency words plot
charity_freq %>%
  slice_max(n, n = 30) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Most Frequent Charity Agency Dictionary Words",
    x = NULL,
    y = "Frequency"
  )

ggsave(
  file.path(EXPORT_DIR, "Charity_Agency_Top30.png"),
  width = 8,
  height = 6,
  dpi = 300
)





# ==============================================================================
# DICTIONARY HIT CONTEXT VALIDATION
# Prints every dictionary occurrence with +/- 5 surrounding words
# ==============================================================================

library(dplyr)
library(stringr)
library(purrr)


# ------------------------------------------------------------------------------
# Function to print dictionary hits with context
# ------------------------------------------------------------------------------

print_dictionary_hits <- function(df, 
                                  dictionary, 
                                  dictionary_name,
                                  context_words = 5) {
  
  cat("\n\n=========================================================\n")
  cat("DICTIONARY:", dictionary_name, "\n")
  cat("=========================================================\n")
  
  
  for (i in seq_len(nrow(df))) {
    
    text <- df$cleaned_text[i]
    ad_id <- df$Ad_Base_ID[i]
    
    # tokenize
    words <- str_split(text, "\\s+")[[1]]
    
    # find matches
    hits <- which(words %in% dictionary)
    
    if(length(hits) == 0) next
    
    
    for(hit in hits) {
      
      start <- max(1, hit - context_words)
      end <- min(length(words), hit + context_words)
      
      before <- words[start:(hit-1)]
      after <- words[(hit+1):end]
      
      context <- c(before,
                   paste0(">>> ", toupper(words[hit]), " <<<"),
                   after)
      
      context <- paste(context, collapse = " ")
      
      
      cat("\n---------------------------------------------------------\n")
      cat("Advertisement:", ad_id, "\n")
      cat("Dictionary:", dictionary_name, "\n")
      cat("Matched word:", words[hit], "\n\n")
      cat(context, "\n")
      cat("---------------------------------------------------------\n")
    }
  }
}





# DICTIONARY HIT CONTEXT VALIDATION
# Prints every dictionary occurrence with +/- 5 surrounding words
# Uses actual dataframe variables: results_df$text and results_df$Ad_Base_ID
# ==============================================================================

library(dplyr)
library(stringr)
library(purrr)
library(readr)


# ------------------------------------------------------------------------------
# Function: Extract dictionary hits with surrounding context
# ------------------------------------------------------------------------------

extract_dictionary_hits <- function(df,
                                    dictionary,
                                    dictionary_name,
                                    context_words = 5) {
  
  output <- list()
  counter <- 1
  
  for(i in seq_len(nrow(df))) {
    
    ad_id <- df$Ad_Base_ID[i]
    text <- df$text[i]
    
    # Skip empty OCR text
    if(is.na(text) || str_trim(text) == "") next
    
    # Tokenize
    words <- unlist(str_split(text, "\\s+"))
    
    # Remove empty tokens
    words <- words[words != ""]
    
    if(length(words) == 0) next
    
    
    # Find dictionary matches
    hits <- which(words %in% dictionary)
    
    if(length(hits) == 0) next
    
    
    # Extract every hit
    for(hit in hits) {
      
      start <- max(1, hit - context_words)
      end <- min(length(words), hit + context_words)
      
      before <- if(hit > 1) {
        words[start:(hit-1)]
      } else {
        character(0)
      }
      
      after <- if(hit < length(words)) {
        words[(hit+1):end]
      } else {
        character(0)
      }
      
      context <- paste(
        c(
          before,
          paste0(">>> ", words[hit], " <<<"),
          after
        ),
        collapse = " "
      )
      
      
      output[[counter]] <- tibble(
        Ad_Base_ID = ad_id,
        Dictionary = dictionary_name,
        Matched_Word = words[hit],
        Position = hit,
        Context = context
      )
      
      counter <- counter + 1
      
    }
  }
  
  
  bind_rows(output)
}



# ==============================================================================
# RUN VALIDATION
# ==============================================================================


# Vulnerability dictionary hits
vulnerability_context <- extract_dictionary_hits(
  df = results_df,
  dictionary = expanded_dictionaries$vulnerability,
  dictionary_name = "VULNERABILITY",
  context_words = 5
)


# Charity agency dictionary hits
charity_context <- extract_dictionary_hits(
  df = results_df,
  dictionary = expanded_dictionaries$charity_agency,
  dictionary_name = "CHARITY_AGENCY",
  context_words = 5
)



# Combine results
dictionary_context_results <- bind_rows(
  vulnerability_context,
  charity_context
)



# View all hits in RStudio
View(dictionary_context_results)

