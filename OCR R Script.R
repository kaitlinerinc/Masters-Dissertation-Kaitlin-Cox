getwd()
# ==============================================================================
# OCR CHARITY ADVERTISEMENT PROCESSING PIPELINE
# Master's Dissertation Pipeline

rm(list = ls())

# ==============================================================================
# LIBRARIES
# ==============================================================================

library(tesseract)
library(magick)
library(dplyr)
library(stringr)
library(readr)
library(hunspell)
library(quanteda)

# ==============================================================================
# PATHS
# ==============================================================================

paths <- list(
  
  newspaper_dir =
    "C:/Users/unity/Documents/Dissertation/Newspaper Database",
  
  newspaper_rds =
    "C:/Users/unity/Documents/Dissertation/Newspaper_Ads.rds",
  
  mailout_dir =
    "C:/Users/unity/Documents/Dissertation/Mailout Database",
  
  mailout_rds =
    "C:/Users/unity/Documents/Dissertation/Mailout_Ads.rds"
  
)

# ==============================================================================
# TESSERACT ENGINES
# ==============================================================================

engine_psm3 <- tesseract(
  "eng",
  options = list(
    tessedit_pageseg_mode = 3,
    preserve_interword_spaces = 1
  )
)

engine_psm6 <- tesseract(
  "eng",
  options = list(
    tessedit_pageseg_mode = 6,
    preserve_interword_spaces = 1
  )
)

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#-------------------------------------------------------
# Count valid English words
#-------------------------------------------------------

count_dictionary_words <- function(text){
  
  tokens <- unlist(str_split(text, "\\s+"))
  
  tokens <- tokens[tokens != ""]
  
  if(length(tokens) == 0)
    return(0)
  
  sum(hunspell_check(tokens))
  
}

#-------------------------------------------------------
# Calculate OCR Noise Ratio
#-------------------------------------------------------

calculate_noise_ratio <- function(text){
  
  tokens <- unlist(str_split(text, "\\s+"))
  
  tokens <- tokens[tokens != ""]
  
  if(length(tokens) == 0)
    return(NA)
  
  dictionary_words <- sum(hunspell_check(tokens))
  
  1 - (dictionary_words / length(tokens))
  
}

#-------------------------------------------------------
# Initial OCR line cleaning
#-------------------------------------------------------

clean_ocr_lines <- function(text){
  
  lines <- str_split(text, "\\n")[[1]]
  
  lines <- trimws(lines)
  
  lines <- lines[lines != ""]
  
  lines <- lines[str_length(lines) > 3]
  
  lines <- lines[
    !str_detect(
      lines,
      "^[[:punct:][:space:]]+$"
    )
  ]
  
  unique(lines)
  
}

#-------------------------------------------------------
# Remove duplicated lines after panel aggregation
#-------------------------------------------------------

remove_duplicate_lines <- function(text){
  
  lines <- unlist(
    str_split(text, "\\|")
  )
  
  lines <- trimws(lines)
  
  lines <- lines[lines != ""]
  
  lines <- unique(lines)
  
  paste(lines, collapse = " ")
  
}

#-------------------------------------------------------
# Adaptive image preprocessing
#-------------------------------------------------------

preprocess_image <- function(file){
  
  img <- image_read(file)
  
  info <- image_info(img)
  
  width <- info$width
  
  if(width < 1200){
    
    resize_amount <- "400%"
    
  } else if(width < 2200){
    
    resize_amount <- "300%"
    
  } else{
    
    resize_amount <- "100%"
    
  }
  
  img |>
    image_convert(colorspace = "gray") |>
    image_normalize() |>
    image_contrast(sharpen = TRUE) |>
    image_resize(resize_amount)
  
}

#-------------------------------------------------------
# OCR helper
#-------------------------------------------------------

run_single_ocr <- function(image, engine){
  
  text <- tryCatch(
    
    {
      
      ocr(image, engine = engine)
      
    },
    
    error = function(e){
      
      ""
      
    }
    
  )
  
  cleaned_lines <- clean_ocr_lines(text)
  
  cleaned_text <- paste(
    cleaned_lines,
    collapse = " | "
  )
  
  list(
    
    text = cleaned_text,
    
    dictionary =
      count_dictionary_words(
        tolower(cleaned_text)
      ),
    
    noise =
      calculate_noise_ratio(
        tolower(cleaned_text)
      )
    
  )
  
}

#-------------------------------------------------------
# Automatically choose the better OCR result
#-------------------------------------------------------

select_best_ocr <- function(img){
  
  result_psm3 <- run_single_ocr(
    img,
    engine_psm3
  )
  
  result_psm6 <- run_single_ocr(
    img,
    engine_psm6
  )
  
  # Calculate dictionary retention
  psm3_tokens <- length(
    unlist(str_split(result_psm3$text, "\\s+"))
  )
  
  psm6_tokens <- length(
    unlist(str_split(result_psm6$text, "\\s+"))
  )
  
  psm3_retention <- ifelse(
    psm3_tokens > 0,
    result_psm3$dictionary / psm3_tokens,
    0
  )
  
  psm6_retention <- ifelse(
    psm6_tokens > 0,
    result_psm6$dictionary / psm6_tokens,
    0
  )
  
  # Select OCR with highest meaningful word retention
  if(psm3_retention > psm6_retention){
    
    return(result_psm3$text)
    
  }
  
  if(psm6_retention > psm3_retention){
    
    return(result_psm6$text)
    
  }
  
  # Tie breaker: retain more words
  if(result_psm3$dictionary > result_psm6$dictionary){
    
    return(result_psm3$text)
    
  }
  
  result_psm6$text
  
}

# ==============================================================================
# MAIN OCR FUNCTION
# ==============================================================================

process_charity_ocr <- function(
    input_folder,
    output_rds
){
  
  valid_extensions <-
    "\\.(jpg|jpeg|png|bmp|tiff|webp)$"
  
  image_files <-
    list.files(
      input_folder,
      pattern = valid_extensions,
      ignore.case = TRUE,
      full.names = TRUE
    )
  
  if(length(image_files) == 0){
    
    stop(
      paste(
        "No images found in",
        input_folder
      )
    )
    
  }
  
  cat(
    "\n=====================================\n"
  )
  
  cat(
    "Processing",
    length(image_files),
    "images...\n"
  )
  
  cat(
    "=====================================\n\n"
  )
  
  results <- lapply(
    
    image_files,
    
    function(file_path){
      
      file_name <- basename(file_path)
      
      cat(
        "OCR:",
        file_name,
        "\n"
      )
      
      img <- preprocess_image(file_path)
      
      extracted_text <- select_best_ocr(img)
      raw_prefix <- str_extract(
        file_name,
        "^[0-9]+[a-zA-Z]?"
      )
      
      ad_base_id <- case_when(
        
        raw_prefix == "010a" ~ "010a",
        
        raw_prefix == "010b" ~ "010b",
        
        TRUE ~ str_replace(
          raw_prefix,
          "[a-zA-Z]$",
          ""
        )
        
      )
      
      data.frame(
        
        Ad_Base_ID = ad_base_id,
        
        FileName = file_name,
        
        ExtractedText = extracted_text,
        
        stringsAsFactors = FALSE
        
      )
      
    }
    
  )
  
  raw_df <- bind_rows(results)
  
  # ==============================================================================
  # AGGREGATE MULTI-PANEL ADVERTISEMENTS
  # ==============================================================================
  
  cat(
    "\nAggregating advertisement panels...\n"
  )
  
  final_df <-
    
    raw_df |>
    
    group_by(
      Ad_Base_ID
    ) |>
    
    summarise(
      
      Source_Files =
        
        paste(
          FileName,
          collapse = " & "
        ),
      
      RawCombinedText =
        
        paste(
          ExtractedText,
          collapse = " | "
        ),
      
      .groups = "drop"
      
    )
  
  # ==============================================================================
  # REMOVE DUPLICATE LINES CREATED BY MULTI-PANEL ADS
  # ==============================================================================
  
  final_df <-
    
    final_df |>
    
    mutate(
      
      RawCombinedText =
        
        sapply(
          
          RawCombinedText,
          
          remove_duplicate_lines
          
        )
      
    )
  
  # ==============================================================================
  # METHODOLOGY-COMPLIANT CLEANING
  # ==============================================================================
  
  cat(
    "Cleaning OCR text...\n"
  )
  
  final_df <-
    
    final_df |>
    
    mutate(
      
      CleanedText =
        
        RawCombinedText |>
        
        str_remove_all(
          "(?i)https?://\\S+|www\\.\\S+"
        ) |>
        
        str_remove_all(
          "(?i)\\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\b"
        ) |>
        
        str_remove_all(
          "(?i)\\b[A-Z]{1,2}[0-9][0-9A-Z]?\\s?[0-9][A-Z]{2}\\b"
        ) |>
        
        str_remove_all(
          "(?i)registered\\s+charity\\s+(?:number|no\\.?|numbers|nos\\.?)\\s*[0-9\\s,]+"
        ) |>
        
        str_remove_all(
          "(?i)charity\\s+(?:number|no\\.?|numbers|nos\\.?)\\s*[0-9\\s,]+"
        ) |>
        
        str_remove_all(
          "[0-9]+"
        ) |>
        
        str_replace_all(
          "[[:punct:]]",
          " "
        ) |>
        
        str_replace_all(
          "\\s+",
          " "
        ) |>
        
        str_squish() |>
        
        tolower()
      
    )
  
  # ==============================================================================
  # TOKEN COUNTS
  # ==============================================================================
  
  cat(
    "Calculating token statistics...\n"
  )
  
  final_df$Total_Tokens <- sapply(
    
    str_split(
      final_df$CleanedText,
      "\\s+"
    ),
    
    function(x){
      sum(x != "")
    }
    
  )
  
  # ==============================================================================
  # DICTIONARY WORD COUNTS
  # ==============================================================================
  
  final_df$Dictionary_Words <-
    
    sapply(
      
      final_df$CleanedText,
      
      count_dictionary_words
      
    )
  
  # ==============================================================================
  # OCR NOISE RATIO
  # ==============================================================================
  
  final_df <-
    
    final_df |>
    
    mutate(
      
      NoiseRatio =
        
        ifelse(
          
          Total_Tokens > 0,
          
          1 -
            (
              Dictionary_Words /
                Total_Tokens
            ),
          
          NA
          
        )
      
    )
  
  # ==============================================================================
  # REMOVE EMPTY DOCUMENTS
  # ==============================================================================
  
  final_df <-
    
    final_df |>
    
    filter(
      
      Total_Tokens > 0
      
    )
  
  # ==============================================================================
  # CREATE CORPUS
  # ==============================================================================
  
  ad_corpus <-
    
    corpus(
      
      final_df,
      
      docid_field = "Ad_Base_ID",
      
      text_field = "CleanedText"
      
    )
  
  ad_tokens <-
    
    tokens(
      
      ad_corpus,
      
      remove_punct = FALSE,
      
      remove_symbols = FALSE
      
    )
  
  ad_dfm <-
    
    dfm(
      
      ad_tokens
      
    )
  
  # ==============================================================================
  # OPTIONAL OCR QUALITY SUMMARY
  # ==============================================================================
  
  
  ocr_summary <-
    
    final_df |>
    
    summarise(
      
      Total_Advertisements = n(),
      
      Mean_Tokens =
        round(
          mean(
            Total_Tokens,
            na.rm = TRUE
          ),
          2
        ),
      
      Median_Tokens =
        median(
          Total_Tokens,
          na.rm = TRUE
        ),
      
      Mean_Dictionary_Words =
        round(
          mean(
            Dictionary_Words,
            na.rm = TRUE
          ),
          2
        ),
      
      Mean_Noise_Ratio =
        round(
          mean(
            NoiseRatio,
            na.rm = TRUE
          ),
          3
        ),
      
      High_Noise_Ads =
        sum(
          NoiseRatio > 0.50,
          na.rm = TRUE
        ),
      
      .groups = "drop"
      
    )
  
  
  # ==============================================================================
  # FINAL REPORTING
  # ==============================================================================
  
  
  cat(
    "\n=====================================\n"
  )
  
  cat(
    "OCR PIPELINE COMPLETE\n"
  )
  
  cat(
    "=====================================\n\n"
  )
  
  
  cat(
    "Advertisements processed:",
    nrow(raw_df),
    "\n"
  )
  
  
  cat(
    "Advertisements after aggregation:",
    nrow(raw_df |>
           
           distinct(
             
             Ad_Base_ID
             
           )),
    "\n"
  )
  
  
  cat(
    "Final advertisement count:",
    nrow(final_df),
    "\n"
  )
  
  
  cat(
    "Mean OCR noise ratio:",
    round(
      mean(
        final_df$NoiseRatio,
        na.rm = TRUE
      ),
      3
    ),
    "\n"
  )
  
  
  cat(
    "Mean token count:",
    round(
      mean(
        final_df$Total_Tokens,
        na.rm = TRUE
      ),
      2
    ),
    "\n\n"
  )
  
  
  
  # ==============================================================================
  # SAVE OCR OUTPUT
  # ==============================================================================
  
  
  cat(
    "Saving processed dataset...\n"
  )
  
  
  saveRDS(
    
    final_df,
    
    output_rds
    
  )
  
  
  cat(
    "Saved:",
    output_rds,
    "\n"
  )
  
  
  # Return results to R environment
  
  return(
    
    list(
      
      advertisements = final_df,
      
      ocr_summary = ocr_summary
    )
    
  )
  
  
}



# ==============================================================================
# EXECUTION BLOCK
# ==============================================================================


cat(
  "\nStarting newspaper advertisement OCR pipeline...\n"
)


newspaper_results <-
  
  process_charity_ocr(
    
    input_folder =
      paths$newspaper_dir,
    
    output_rds =
      paths$newspaper_rds
    
  )



cat(
  "\nStarting mailout advertisement OCR pipeline...\n"
)



mailout_results <-
  
  process_charity_ocr(
    
    input_folder =
      paths$mailout_dir,
    
    output_rds =
      paths$mailout_rds
    
  )



# ==============================================================================
# FINAL PIPELINE SUMMARY
# ==============================================================================


cat(
  "\n=====================================\n"
)

cat(
  "ALL OCR PROCESSING COMPLETE\n"
)

cat(
  "=====================================\n\n"
)


cat(
  "Newspaper advertisements:",
  nrow(
    newspaper_results$advertisements
  ),
  "\n"
)


cat(
  "Mailout advertisements:",
  nrow(
    mailout_results$advertisements
  ),
  "\n"
)


cat(
  "\nSaved files:\n"
)


cat(
  paths$newspaper_rds,
  "\n"
)


cat(
  paths$mailout_rds,
  "\n"
)


cat(
  "\nPipeline finished successfully.\n"
)