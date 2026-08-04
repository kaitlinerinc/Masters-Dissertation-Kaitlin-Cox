library(dplyr)
library(readr)
library(stringr)
library(igraph)

# ==============================================================================
# PATHS
# ==============================================================================

ocr_rds <-
  "C:/Users/unity/Documents/Dissertation/Newspaper_Ads.rds"

cluster_csv <-
  "C:/Users/unity/Documents/Dissertation/Duplicate_Clusters.csv"

# ==============================================================================
# SETTINGS
# ==============================================================================

jaccard_threshold <- 0.90

# Compare advertisements whose lengths differ by no more than ±20%
length_tolerance <- 0.20

preview_length <- 150

# ==============================================================================
# LOAD DATA
# ==============================================================================

ads <- readRDS(ocr_rds)

cat("Loaded", nrow(ads), "advertisements.\n")

# ==============================================================================
# TOKENISE
# ==============================================================================

token_sets <- lapply(
  ads$CleanedText,
  function(x){
    
    words <- unlist(str_split(x,"\\s+"))
    
    words <- words[words!=""]
    
    unique(words)
    
  }
)

token_counts <- lengths(token_sets)

# ==============================================================================
# JACCARD FUNCTION
# ==============================================================================

jaccard_similarity <- function(a,b){
  
  intersection <- length(intersect(a,b))
  
  if(intersection==0)
    return(0)
  
  union <- length(union(a,b))
  
  intersection/union
  
}

# ==============================================================================
# BUILD GRAPH OF DUPLICATES
# ==============================================================================

edges <- list()

counter <- 1

n <- nrow(ads)

cat("\nFinding duplicate links...\n\n")

for(i in 1:(n-1)){
  
  len_i <- token_counts[i]
  
  candidate_rows <- which(
    
    token_counts[(i+1):n] >= len_i*(1-length_tolerance) &
      token_counts[(i+1):n] <= len_i*(1+length_tolerance)
    
  )
  
  if(length(candidate_rows)==0)
    next
  
  candidate_rows <- candidate_rows+i
  
  for(j in candidate_rows){
    
    sim <- jaccard_similarity(
      token_sets[[i]],
      token_sets[[j]]
    )
    
    if(sim >= jaccard_threshold){
      
      edges[[counter]] <-
        
        data.frame(
          
          from=i,
          to=j,
          
          similarity=sim
          
        )
      
      counter <- counter+1
      
    }
    
  }
  
  if(i%%25==0){
    
    cat("Processed",i,"/",n,"\n")
    
  }
  
}

# ==============================================================================
# NO DUPLICATES FOUND
# ==============================================================================

if(length(edges)==0){
  
  cat("\nNo duplicate clusters found.\n")
  
  quit(save="no")
  
}

edges <- bind_rows(edges)

# ==============================================================================
# CREATE GRAPH
# ==============================================================================

g <- graph_from_data_frame(
  edges,
  directed=FALSE,
  vertices=data.frame(id=1:n)
)

clusters <- components(g)

ads$Cluster <- clusters$membership

# ==============================================================================
# KEEP ONLY CLUSTERS WITH >1 ADVERTISEMENT
# ==============================================================================

cluster_sizes <- table(ads$Cluster)

ads <- ads |>
  
  mutate(
    
    Cluster_Size=
      cluster_sizes[as.character(Cluster)]
    
  ) |>
  
  filter(Cluster_Size>1)

# ==============================================================================
# CREATE REVIEW TABLE
# ==============================================================================

cluster_table <-
  
  ads |>
  
  arrange(Cluster) |>
  
  group_by(Cluster) |>
  
  summarise(
    
    Advertisements=
      paste(
        Ad_Base_ID,
        collapse=", "
      ),
    
    Number_of_Ads=n(),
    
    Preview=
      substr(
        first(CleanedText),
        1,
        preview_length
      ),
    
    Keep_ID=
      first(Ad_Base_ID),
    
    Remove_IDs=
      paste(
        Ad_Base_ID[-1],
        collapse=", "
      ),
    
    Duplicate="",
    
    Notes="",
    
    .groups="drop"
    
  )

write_csv(
  cluster_table,
  cluster_csv
)

cat("Duplicate clustering complete.\n\n")

cat("Clusters found:",
    nrow(cluster_table),
    "\n")

cat("Advertisements involved:",
    sum(cluster_table$Number_of_Ads),
    "\n")

cat("\nSaved to:\n")

cat(cluster_csv,"\n")
