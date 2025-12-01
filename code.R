# ==============================================================================
# COMPREHENSIVE MEDIA ANALYSIS: GENERATIVE AI IN CROATIAN EDUCATION (2023-2025)
# ==============================================================================
# Author: Research Analysis Pipeline
# Purpose: Full analytical pipeline for Croatian media coverage of AI in education
# Data: Determ media dataset (~4,424 filtered articles from 25M records)
# ==============================================================================

# Clear environment
rm(list = ls())
gc()

# ==============================================================================
# SECTION 0: CONFIGURATION
# ==============================================================================

# --- File Paths (MODIFY THESE) ---
CONFIG <- list(
  # Input
  duckdb_path = "D:/LUKA/DetermDB/determDB.duckdb",
  table_name = "media_data",

  # Output directory (will be created if doesn't exist)
  output_dir = "D:/LUKA/Academic/HKS/Clanci/AI u obrazovanju/Analysis_Output",

  # Analysis parameters
  memory_limit = "16GB",
  min_articles_for_outlet = 10,  # Minimum articles to include outlet in analysis
  sample_size_for_validation = 200,  # Random sample for manual validation

  # Date range
  start_date = "2023-01-01",
  end_date = "2025-12-31"
)

# Create output directory
if (!dir.exists(CONFIG$output_dir)) {
  dir.create(CONFIG$output_dir, recursive = TRUE)
  message("Created output directory: ", CONFIG$output_dir)
}

# ==============================================================================
# SECTION 1: PACKAGE MANAGEMENT
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("SECTION 1: Loading Required Packages")
message(paste(rep("=", 70), collapse = ""), "\n")

required_packages <- c(
  # Data manipulation
  "dplyr", "tidyr", "stringr", "lubridate", "forcats",
  # Database

  "duckdb", "DBI",
  # Text analysis
  "tidytext", "quanteda", "quanteda.textstats", "quanteda.textplots",
  # Visualization
  "ggplot2", "ggthemes", "scales", "patchwork", "ggrepel",
  "RColorBrewer", "viridis",
  # Network analysis
  "igraph", "ggraph", "tidygraph",
  # Statistical analysis
  "changepoint", "zoo", "broom",
  # Export
  "openxlsx", "knitr", "rmarkdown",
  # Misc
  "progress"
)

# Install missing packages
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages) > 0) {
    message("Installing: ", paste(new_packages, collapse = ", "))
    install.packages(new_packages, dependencies = TRUE)
  }
}

install_if_missing(required_packages)

# Load all packages
invisible(lapply(required_packages, library, character.only = TRUE))

# Set global options
options(
  dplyr.summarise.inform = FALSE,
  scipen = 999,
  stringsAsFactors = FALSE
)

# Set locale for Croatian
#Sys.setlocale("LC_ALL", "Croatian")

message("All packages loaded successfully.\n")

# ==============================================================================
# SECTION 2: DATA LOADING AND INITIAL CLEANING
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("SECTION 2: Loading and Cleaning Data")
message(paste(rep("=", 70), collapse = ""), "\n")

# --- 2.1 Connect to Database ---
message("Connecting to DuckDB...")
con <- dbConnect(duckdb::duckdb(), dbdir = CONFIG$duckdb_path, read_only = TRUE)
dbExecute(con, paste0("SET memory_limit='", CONFIG$memory_limit, "';"))

# --- 2.2 Define Search Patterns ---

# PATTERN 1: AI/Technology terms
ai_pattern <- "(?i)umjetn.*intelig|\\bAI\\b|ChatGPT|GPT|OpenAI|generativn|jezičn.*model|LLM|chatbot|\\bbot\\b|robot|algoritam|\\bstroj|Photomath|Gemini|Copilot|Claude|Anthropic|Midjourney|DALL-E|Stable.?Diffusion|Bard|Bing.?Chat|neuronsk|strojn.*učenj|dubok.*učenj"

# PATTERN 2: Education terms
edu_pattern <- "(?i)obrazov|\\bškol|fakult|sveučili|\\bfaks|\\bnastav|\\bučen|student|profesor|učitelj|ravnatelj|ministarstv|kurikulum|akademsk|pedagog|didaktik|CARNET|e.?učenj|e.?nastav|digit.*pismenost|informatik|predavanj"

# PATTERN 3: Task/Output terms
task_pattern <- "(?i)esej|zadać|seminar|diplomsk|\\brad\\b|\\btest|\\bispit|matura|prepisiv|\\bvaranj|plagij|ocjen|pisan.*rad|završn|kolokvij|projekt|domaći|prezentacij|referat|pristupn"

# --- 2.3 Execute Query ---
query <- sprintf("
  SELECT *
  FROM %s
  WHERE
    SOURCE_TYPE = 'web'
    AND DATETIME >= '%s'
    AND DATETIME <= '%s'
    AND regexp_matches(TITLE, '%s')
    AND (
         regexp_matches(TITLE, '%s')
         OR
         regexp_matches(TITLE, '%s')
    );
", CONFIG$table_name, CONFIG$start_date, CONFIG$end_date,
                 ai_pattern, edu_pattern, task_pattern)

message("Executing database query...")
start_time <- Sys.time()
raw_data <- dbGetQuery(con, query)
end_time <- Sys.time()
message("Query completed in: ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds")
message("Initial articles retrieved: ", format(nrow(raw_data), big.mark = ","))

# --- 2.4 Full Text Validation ---
message("\nApplying full-text validation filter...")

validated_data <- raw_data %>%
  filter(
    !is.na(FULL_TEXT) &
      nchar(FULL_TEXT) > 100 &  # Minimum text length
      str_detect(FULL_TEXT, ai_pattern) &
      (str_detect(FULL_TEXT, edu_pattern) | str_detect(FULL_TEXT, task_pattern))
  )

message("After validation: ", format(nrow(validated_data), big.mark = ","), " articles")
message("Removed: ", format(nrow(raw_data) - nrow(validated_data), big.mark = ","), " articles (clickbait/false positives)")

# --- 2.5 Data Cleaning ---
message("\nCleaning and standardizing data...")

clean_data <- validated_data %>%
  mutate(
    # Parse dates
    DATE = as.Date(DATE),
    DATETIME = as.POSIXct(DATETIME),

    # Extract temporal components
    year = year(DATE),
    month = month(DATE),
    year_month = floor_date(DATE, "month"),
    week = floor_date(DATE, "week"),
    day_of_week = wday(DATE, label = TRUE, abbr = FALSE),
    quarter = quarter(DATE),

    # Clean source names
    source_clean = str_trim(str_to_lower(FROM)),

    # Text length metrics
    title_length = nchar(TITLE),
    text_length = nchar(FULL_TEXT),
    word_count = str_count(FULL_TEXT, "\\S+"),

    # Create unique ID if not exists
    article_id = row_number()
  ) %>%
  # Remove duplicates based on title and date
  distinct(TITLE, DATE, .keep_all = TRUE) %>%
  arrange(DATETIME)

message("Final clean dataset: ", format(nrow(clean_data), big.mark = ","), " articles")

# Close database connection
dbDisconnect(con, shutdown = TRUE)

# ==============================================================================
# SECTION 3: OUTLET CLASSIFICATION
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("SECTION 3: Classifying Media Outlets")
message(paste(rep("=", 70), collapse = ""), "\n")

# --- 3.1 Define Outlet Typology ---

# Major Croatian outlets classification (expand as needed)
outlet_classification <- tribble(
  ~pattern,                    ~outlet_type,        ~outlet_name_clean,
  "24sata",                    "Tabloid",           "24sata",
  "index",                     "Tabloid",           "Index.hr",
  "jutarnji",                  "Quality",           "Jutarnji list",
  "vecernji",                  "Quality",           "Večernji list",
  "slobodna.*dalmacija",       "Regional",          "Slobodna Dalmacija",
  "novi.*list",                "Regional",          "Novi list",
  "glas.*slavonije",           "Regional",          "Glas Slavonije",
  "dnevnik",                   "Quality",           "Dnevnik.hr",
  "hrt",                       "Public",            "HRT",
  "n1",                        "Quality",           "N1",
  "net\\.hr",                  "Tabloid",           "Net.hr",
  "tportal",                   "Quality",           "Tportal",
  "telegram",                  "Quality",           "Telegram",
  "bug",                       "Specialized_Tech",  "Bug.hr",
  "zimo",                      "Specialized_Tech",  "Zimo.hr",
  "skolski.*portal",           "Specialized_Edu",   "Školski portal",
  "srednja",                   "Specialized_Edu",   "Srednja.hr",
  "studentski",                "Specialized_Edu",   "Studentski.hr",
  "educentar",                 "Specialized_Edu",   "Educentar",
  "poslovni",                  "Business",          "Poslovni dnevnik",
  "lider",                     "Business",          "Lider",
  "forbes.*hrvatska",          "Business",          "Forbes Hrvatska",
  "nacional",                  "Quality",           "Nacional",
  "express",                   "Tabloid",           "Express.hr",
  "dnevno",                    "Tabloid",           "Dnevno.hr",
  "direktno",                  "Tabloid",           "Direktno.hr",
  "rtl",                       "Commercial_TV",     "RTL.hr",
  "nova.*tv",                  "Commercial_TV",     "Nova TV"
)

# --- 3.2 Apply Classification ---

classify_outlet <- function(source_name) {
  source_lower <- str_to_lower(source_name)

  for (i in 1:nrow(outlet_classification)) {
    if (str_detect(source_lower, outlet_classification$pattern[i])) {
      return(list(
        type = outlet_classification$outlet_type[i],
        name = outlet_classification$outlet_name_clean[i]
      ))
    }
  }
  return(list(type = "Other", name = source_name))
}

# Apply classification
message("Classifying outlets...")
clean_data <- clean_data %>%
  rowwise() %>%
  mutate(
    outlet_info = list(classify_outlet(FROM)),
    outlet_type = outlet_info$type,
    outlet_name = outlet_info$name
  ) %>%
  ungroup() %>%
  select(-outlet_info)

# Summary of outlet types
outlet_summary <- clean_data %>%
  count(outlet_type, sort = TRUE) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

message("\nOutlet Type Distribution:")
print(outlet_summary)

# ==============================================================================
# SECTION 4: FRAME DICTIONARIES (CROATIAN)
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("SECTION 4: Building Frame Dictionaries")
message(paste(rep("=", 70), collapse = ""), "\n")

# --- 4.1 Primary Frames ---

frame_dictionaries <- list(

  # FRAME 1: THREAT/PANIC
  THREAT = c(
    # Core threat terms
    "prijetnja", "opasnost", "opasno", "rizik", "rizično",
    # Cheating/plagiarism
    "varanje", "varati", "prevara", "prevarant", "plagijat", "plagiranje",
    "prepisivanje", "prepisati", "kopiranje",
    # Prohibition/ban
    "zabrana", "zabraniti", "zabranjeno", "zabranjuje",
    # Destruction/end
    "uništiti", "uništava", "uništenje", "smrt", "kraj", "propast",
    # Crisis language
    "kriza", "krizno", "alarm", "upozorenje", "hitno",
    # Negative outcomes
    "šteta", "štetno", "negativno", "problematično", "zabrinjavajuće",
    # Fear
    "strah", "bojati", "panika", "zastrašujuće"
  ),

  # FRAME 2: OPPORTUNITY/TOOL
  OPPORTUNITY = c(
    # Tool terminology
    "alat", "sredstvo", "instrument", "pomoć", "pomoćnik", "asistent",
    # Positive outcomes
    "prilika", "mogućnost", "potencijal", "prednost", "korist", "koristan",
    # Improvement
    "poboljšati", "poboljšanje", "unaprijediti", "unapređenje",
    "napredak", "napredovanje", "razvoj",
    # Efficiency
    "učinkovit", "učinkovitost", "efikasan", "efikasnost", "produktivnost",
    "olakšati", "olakšava", "ubrzati", "ubrzava",
    # Future/innovation
    "budućnost", "inovacija", "inovativan", "revolucija", "revolucionaran",
    "moderan", "modernizacija", "transformacija",
    # Positive framing
    "pozitivno", "uspjeh", "uspješno", "izvrsno", "odlično"
  ),

  # FRAME 3: REGULATION/POLICY
  REGULATION = c(
    # Rules and regulations
    "pravilnik", "pravilo", "propisi", "regulativa", "regulacija",
    "smjernice", "upute", "protokol",
    # Legal terms
    "zakon", "zakonski", "zakonodavstvo", "zakonit", "nezakonit",
    "pravni", "legislativa",
    # Policy actors
    "ministarstvo", "ministar", "vlada", "vladino", "državno",
    "agencija", "institucija", "tijelo",
    # Policy actions
    "dopušteno", "dopuštenje", "dozvola", "dozvoljen", "odobrenje",
    "zabrana", "zabranjeno", "ograničenje",
    # Implementation
    "primjena", "provedba", "implementacija", "uvođenje",
    "odluka", "mjera", "mjere"
  ),

  # FRAME 4: DISRUPTION/CHANGE
  DISRUPTION = c(
    # Change terminology
    "promjena", "promijeniti", "mijenjati", "transformacija",
    "preobrazba", "preokret",
    # Adaptation
    "prilagodba", "prilagoditi", "prilagođavanje", "adaptacija",
    "snalaziti", "nositi se",
    # Inevitability
    "neizbježno", "nezaustavljivo", "neminovno",
    # Revolution
    "revolucija", "revolucionaran", "prekretnica", "nova era",
    "novi način", "novo doba",
    # Evolution
    "evolucija", "evoluirati", "napredak", "napredovanje",
    # Disruption
    "disrupcija", "disruptivan", "poremećaj", "potres"
  ),

  # FRAME 5: HUMAN REPLACEMENT
  REPLACEMENT = c(
    # Job loss
    "zamjena", "zamijeniti", "zamjenjuje", "istisnuti",
    "gubitak posla", "bez posla",
    # Obsolescence
    "nepotreban", "suvišan", "zastario", "zastarjelo",
    # Teacher specific
    "učitelj nepotreban", "profesor nepotreban", "bez učitelja",
    "zamijeni učitelja", "zamijeni profesora",
    # Automation
    "automatizacija", "automatizirano", "automatski",
    # Competition
    "nadmašiti", "bolji od čovjeka", "brži od čovjeka",
    # Human skills
    "ljudski faktor", "ljudska komponenta", "ljudski dodir"
  ),

  # FRAME 6: QUALITY/TRUST
  QUALITY = c(
    # Errors
    "halucinacija", "halucinacije", "greška", "greške", "pogreška",
    "netočno", "netočnost", "krivo", "pogrešno",
    # Reliability
    "pouzdanost", "pouzdan", "nepouzdan", "vjerodostojnost",
    "vjerodostojan", "točnost", "točan",
    # Verification
    "provjera", "provjeriti", "verificirati", "verifikacija",
    "kontrola", "kontrolirati",
    # Quality
    "kvaliteta", "kvalitetan", "nekvalitetan", "loš",
    # Critical thinking
    "kritički", "kritičko mišljenje", "kritička analiza",
    "procjena", "procjenjivanje", "evaluacija"
  ),

  # FRAME 7: EQUITY/ACCESS
  EQUITY = c(
    # Inequality
    "nejednakost", "nejednako", "jaz", "razlika", "raskorak",
    # Access
    "pristup", "pristupačnost", "dostupnost", "dostupan",
    "nedostupan", "ograničen pristup",
    # Digital divide
    "digitalni jaz", "digitalna podjela", "digitalna nejednakost",
    # Socioeconomic
    "siromašan", "siromašni", "bogat", "imućan",
    "socioekonomski", "društveni status",
    # Fairness
    "pravednost", "pravedno", "nepravedno", "fer"
  ),

  # FRAME 8: COMPETENCE/SKILLS
  COMPETENCE = c(
    # Skills
    "vještine", "vještina", "kompetencije", "kompetencija",
    "sposobnost", "sposobnosti",
    # Literacy
    "pismenost", "digitalna pismenost", "medijska pismenost",
    "informatička pismenost",
    # Critical thinking
    "kritičko mišljenje", "kritičko razmišljanje",
    "analitičko mišljenje", "problemsko rješavanje",
    # Learning
    "učiti", "naučiti", "obrazovanje", "edukacija",
    "osposobljavanje", "trening", "usavršavanje"
  )
)

# --- 4.2 Actor Dictionaries ---

actor_dictionaries <- list(

  STUDENTS = c(
    "student", "studenti", "studentica", "studentice",
    "učenik", "učenici", "učenica", "učenice",
    "đak", "đaci", "đakinja",
    "maturant", "maturanti", "maturantica",
    "srednjoškolac", "srednjoškolci",
    "osnovnoškolac", "osnovnoškolci",
    "brucoš", "brucoši", "apsolvent"
  ),

  TEACHERS = c(
    "učitelj", "učitelji", "učiteljica", "učiteljice",
    "nastavnik", "nastavnici", "nastavnica",
    "profesor", "profesori", "profesorica", "profesorice",
    "predavač", "predavači", "predavačica",
    "mentor", "mentori", "mentorica",
    "odgajatelj", "odgajateljica"
  ),

  ADMINISTRATORS = c(
    "ravnatelj", "ravnatelji", "ravnateljica",
    "dekan", "dekani", "dekanica",
    "rektor", "rektori", "rektorica",
    "prorektor", "prorektori",
    "voditelj", "voditeljica",
    "koordinator", "koordinatorica"
  ),

  INSTITUTIONS = c(
    "škola", "škole", "gimnazija", "srednja škola",
    "osnovna škola", "fakultet", "fakulteti",
    "sveučilište", "sveučilišta", "veleučilište",
    "ministarstvo", "ministarstva",
    "carnet", "azoo", "agencija za odgoj i obrazovanje",
    "ncvvo", "aso"
  ),

  TECH_COMPANIES = c(
    "openai", "microsoft", "google", "meta",
    "anthropic", "deepmind", "nvidia",
    "chatgpt", "gpt-4", "gpt-3", "gpt",
    "gemini", "bard", "copilot", "claude"
  ),

  EXPERTS = c(
    "stručnjak", "stručnjaci", "stručnjakinja",
    "ekspert", "eksperti", "ekspertica",
    "znanstvenik", "znanstvenici", "znanstvenica",
    "istraživač", "istraživači", "istraživačica",
    "analitičar", "analitičari"
  ),

  POLICY_MAKERS = c(
    "ministar", "ministri", "ministrica",
    "državni tajnik", "državna tajnica",
    "zastupnik", "zastupnici", "zastupnica",
    "premijer", "vlada", "sabor",
    "političar", "političari"
  )
)

# --- 4.3 Sentiment Dictionaries (Croatian) ---

sentiment_dictionary <- list(

  POSITIVE = c(
    # General positive
    "dobar", "dobro", "odličan", "odlično", "sjajan", "sjajno",
    "izvrstan", "izvrsno", "fantastičan", "fenomenalan",
    "pozitivan", "pozitivno", "uspješan", "uspješno", "uspjeh",
    # Achievement
    "postignuće", "napredak", "poboljšanje", "rast",
    # Emotion
    "zadovoljan", "zadovoljstvo", "sreća", "sretan",
    "optimizam", "optimističan", "nada", "nadanje",
    # Quality
    "kvalitetan", "koristan", "vrijedan", "produktivan",
    "učinkovit", "efikasan", "praktičan"
  ),

  NEGATIVE = c(
    # General negative
    "loš", "loše", "negativan", "negativno", "grozan", "grozno",
    "užasan", "strašan", "katastrofa", "katastrofalan",
    # Problems
    "problem", "problematičan", "poteškoća", "prepreka",
    "neuspjeh", "propast", "pad", "pogoršanje",
    # Emotion
    "nezadovoljan", "nezadovoljstvo", "razočarani", "razočaranje",
    "pesimizam", "pesimističan", "strah", "bojazan",
    # Quality
    "nekvalitetan", "beskoristan", "bezvrijedan", "neproduktivan",
    "neučinkovit", "neefikasan"
  ),

  NEUTRAL_INTENSIFIERS = c(
    "značajan", "značajno", "velik", "veliko", "znatan", "znatno",
    "važan", "važno", "ključan", "ključno", "bitan", "bitno"
  )
)

message("Frame dictionaries created:")
message("  - Primary frames: ", length(frame_dictionaries))
message("  - Actor categories: ", length(actor_dictionaries))
message("  - Sentiment categories: ", length(sentiment_dictionary))

# ==============================================================================
# SECTION 5: FRAME ANALYSIS FUNCTIONS
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("SECTION 5: Applying Frame Analysis")
message(paste(rep("=", 70), collapse = ""), "\n")

# --- 5.1 Frame Detection Function ---

detect_frames <- function(text, dictionaries) {
  if (is.na(text)) return(setNames(rep(0, length(dictionaries)), names(dictionaries)))

  text_lower <- str_to_lower(text)

  frame_counts <- sapply(names(dictionaries), function(frame_name) {
    pattern <- paste0("\\b(", paste(dictionaries[[frame_name]], collapse = "|"), ")\\b")
    sum(str_count(text_lower, pattern))
  })

  return(frame_counts)
}

# --- 5.2 Frame Presence (Binary) ---

detect_frame_presence <- function(text, dictionaries) {
  if (is.na(text)) return(setNames(rep(FALSE, length(dictionaries)), names(dictionaries)))

  text_lower <- str_to_lower(text)

  frame_presence <- sapply(names(dictionaries), function(frame_name) {
    pattern <- paste0("\\b(", paste(dictionaries[[frame_name]], collapse = "|"), ")\\b")
    str_detect(text_lower, pattern)
  })

  return(frame_presence)
}

# --- 5.3 Extract Matched Terms ---

extract_matched_terms <- function(text, dictionaries) {
  if (is.na(text)) return(setNames(rep("", length(dictionaries)), names(dictionaries)))

  text_lower <- str_to_lower(text)

  matched_terms <- sapply(names(dictionaries), function(frame_name) {
    pattern <- paste0("\\b(", paste(dictionaries[[frame_name]], collapse = "|"), ")\\b")
    matches <- str_extract_all(text_lower, pattern)[[1]]
    paste(unique(matches), collapse = ", ")
  })

  return(matched_terms)
}

# --- 5.4 Apply Frame Analysis to Dataset ---

message("Applying frame analysis to all articles...")
pb <- progress_bar$new(
  format = "  Processing [:bar] :percent ETA: :eta",
  total = nrow(clean_data), clear = FALSE, width = 60
)

# Create frame columns
frame_results <- lapply(1:nrow(clean_data), function(i) {
  pb$tick()

  # Combine title and full text for analysis
  combined_text <- paste(
    clean_data$TITLE[i],
    clean_data$FULL_TEXT[i],
    sep = " "
  )

  # Get frame counts
  frame_counts <- detect_frames(combined_text, frame_dictionaries)
  frame_presence <- detect_frame_presence(combined_text, frame_dictionaries)

  # Get actor counts
  actor_counts <- detect_frames(combined_text, actor_dictionaries)
  actor_presence <- detect_frame_presence(combined_text, actor_dictionaries)

  # Get sentiment
  sentiment_counts <- detect_frames(combined_text, sentiment_dictionary)

  # Combine results
  c(
    setNames(frame_counts, paste0("frame_", names(frame_counts), "_count")),
    setNames(frame_presence, paste0("frame_", names(frame_presence), "_present")),
    setNames(actor_counts, paste0("actor_", names(actor_counts), "_count")),
    setNames(actor_presence, paste0("actor_", names(actor_presence), "_present")),
    setNames(sentiment_counts, paste0("sentiment_", names(sentiment_counts), "_count"))
  )
})

# Convert to dataframe and bind
frame_df <- bind_rows(lapply(frame_results, as.data.frame.list))
clean_data <- bind_cols(clean_data, frame_df)

# --- 5.5 Calculate Derived Metrics ---

message("\nCalculating derived metrics...")

clean_data <- clean_data %>%
  mutate(
    # Dominant frame (highest count)
    dominant_frame = apply(
      select(., starts_with("frame_") & ends_with("_count")), 1,
      function(x) {
        frame_names <- c("THREAT", "OPPORTUNITY", "REGULATION", "DISRUPTION",
                         "REPLACEMENT", "QUALITY", "EQUITY", "COMPETENCE")
        if (all(x == 0)) return("NONE")
        frame_names[which.max(x)]
      }
    ),

    # Total frame intensity (sum of all frame counts)
    frame_intensity = rowSums(select(., starts_with("frame_") & ends_with("_count"))),

    # Frame count (number of frames present)
    frame_count = rowSums(select(., starts_with("frame_") & ends_with("_present"))),

    # Sentiment score
    sentiment_score = sentiment_POSITIVE_count - sentiment_NEGATIVE_count,
    sentiment_category = case_when(
      sentiment_score > 2 ~ "Positive",
      sentiment_score < -2 ~ "Negative",
      TRUE ~ "Neutral"
    ),

    # Actor diversity
    actor_diversity = rowSums(select(., starts_with("actor_") & ends_with("_present"))),

    # Primary actor (most mentioned)
    primary_actor = apply(
      select(., starts_with("actor_") & ends_with("_count")), 1,
      function(x) {
        actor_names <- c("STUDENTS", "TEACHERS", "ADMINISTRATORS", "INSTITUTIONS",
                         "TECH_COMPANIES", "EXPERTS", "POLICY_MAKERS")
        if (all(x == 0)) return("NONE")
        actor_names[which.max(x)]
      }
    )
  )

message("Frame analysis complete.")

# ==============================================================================
# SECTION 6: TEMPORAL ANALYSIS
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("SECTION 6: Temporal Analysis")
message(paste(rep("=", 70), collapse = ""), "\n")

# --- 6.1 Monthly Aggregation ---

monthly_stats <- clean_data %>%
  group_by(year_month) %>%
  summarise(
    n_articles = n(),

    # Frame means
    across(starts_with("frame_") & ends_with("_count"),
           ~ mean(., na.rm = TRUE), .names = "mean_{.col}"),

    # Frame proportions
    prop_THREAT = mean(frame_THREAT_present, na.rm = TRUE),
    prop_OPPORTUNITY = mean(frame_OPPORTUNITY_present, na.rm = TRUE),
    prop_REGULATION = mean(frame_REGULATION_present, na.rm = TRUE),
    prop_DISRUPTION = mean(frame_DISRUPTION_present, na.rm = TRUE),
    prop_REPLACEMENT = mean(frame_REPLACEMENT_present, na.rm = TRUE),
    prop_QUALITY = mean(frame_QUALITY_present, na.rm = TRUE),
    prop_EQUITY = mean(frame_EQUITY_present, na.rm = TRUE),
    prop_COMPETENCE = mean(frame_COMPETENCE_present, na.rm = TRUE),

    # Sentiment
    mean_sentiment = mean(sentiment_score, na.rm = TRUE),
    prop_positive = mean(sentiment_category == "Positive", na.rm = TRUE),
    prop_negative = mean(sentiment_category == "Negative", na.rm = TRUE),

    # Actors
    prop_students_mentioned = mean(actor_STUDENTS_present, na.rm = TRUE),
    prop_teachers_mentioned = mean(actor_TEACHERS_present, na.rm = TRUE),

    # Article characteristics
    mean_word_count = mean(word_count, na.rm = TRUE),
    n_outlets = n_distinct(outlet_name),

    .groups = "drop"
  )

# --- 6.2 Change Point Detection ---

message("Detecting change points in coverage volume...")

# Prepare time series
ts_volume <- ts(monthly_stats$n_articles,
                start = c(year(min(monthly_stats$year_month)),
                          month(min(monthly_stats$year_month))),
                frequency = 12)

# Detect change points
cpt_volume <- cpt.mean(ts_volume, method = "PELT", penalty = "BIC")

# Extract change point dates
if (length(cpts(cpt_volume)) > 0) {
  change_points_volume <- monthly_stats$year_month[cpts(cpt_volume)]
  message("Volume change points detected at: ",
          paste(format(change_points_volume, "%Y-%m"), collapse = ", "))
} else {
  change_points_volume <- NULL
  message("No significant volume change points detected.")
}

# --- 6.3 Frame Shift Detection ---

message("Detecting frame shifts...")

# Threat frame time series
if (nrow(monthly_stats) > 5) {
  ts_threat <- ts(monthly_stats$prop_THREAT)
  cpt_threat <- cpt.mean(ts_threat, method = "PELT", penalty = "BIC")

  if (length(cpts(cpt_threat)) > 0) {
    change_points_threat <- monthly_stats$year_month[cpts(cpt_threat)]
    message("THREAT frame change points: ",
            paste(format(change_points_threat, "%Y-%m"), collapse = ", "))
  }
}

# --- 6.4 Define Narrative Phases ---

# Based on change point analysis and manual inspection
# (These can be adjusted based on actual data patterns)

clean_data <- clean_data %>%
  mutate(
    narrative_phase = case_when(
      DATE < as.Date("2023-06-01") ~ "Phase 1: Emergence & Panic",
      DATE < as.Date("2024-01-01") ~ "Phase 2: Debate & Assessment",
      DATE < as.Date("2024-09-01") ~ "Phase 3: Integration & Policy",
      TRUE ~ "Phase 4: Normalization"
    )
  )

phase_summary <- clean_data %>%
  group_by(narrative_phase) %>%
  summarise(
    n_articles = n(),
    date_range = paste(min(DATE), "to", max(DATE)),
    dominant_frames = paste(names(sort(table(dominant_frame), decreasing = TRUE)[1:3]),
                            collapse = ", "),
    .groups = "drop"
  )

message("\nNarrative Phase Summary:")
print(phase_summary)

# ==============================================================================
# SECTION 7: SOURCE ANALYSIS
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("SECTION 7: Source/Outlet Analysis")
message(paste(rep("=", 70), collapse = ""), "\n")

# --- 7.1 Outlet Statistics ---

outlet_stats <- clean_data %>%
  group_by(outlet_name, outlet_type) %>%
  summarise(
    n_articles = n(),
    date_first = min(DATE),
    date_last = max(DATE),

    # Frame profiles
    pct_threat = mean(frame_THREAT_present, na.rm = TRUE) * 100,
    pct_opportunity = mean(frame_OPPORTUNITY_present, na.rm = TRUE) * 100,
    pct_regulation = mean(frame_REGULATION_present, na.rm = TRUE) * 100,

    # Sentiment
    mean_sentiment = mean(sentiment_score, na.rm = TRUE),
    pct_positive = mean(sentiment_category == "Positive", na.rm = TRUE) * 100,
    pct_negative = mean(sentiment_category == "Negative", na.rm = TRUE) * 100,

    # Actors
    pct_students = mean(actor_STUDENTS_present, na.rm = TRUE) * 100,
    pct_teachers = mean(actor_TEACHERS_present, na.rm = TRUE) * 100,

    # Content
    mean_word_count = mean(word_count, na.rm = TRUE),

    .groups = "drop"
  ) %>%
  filter(n_articles >= CONFIG$min_articles_for_outlet) %>%
  arrange(desc(n_articles))

message("Top 15 outlets by coverage volume:")
print(head(outlet_stats, 15) %>% select(outlet_name, outlet_type, n_articles,
                                        pct_threat, pct_opportunity, mean_sentiment))

# --- 7.2 Outlet Type Comparison ---

outlet_type_stats <- clean_data %>%
  group_by(outlet_type) %>%
  summarise(
    n_articles = n(),
    n_outlets = n_distinct(outlet_name),

    # Frames
    pct_threat = mean(frame_THREAT_present, na.rm = TRUE) * 100,
    pct_opportunity = mean(frame_OPPORTUNITY_present, na.rm = TRUE) * 100,
    pct_regulation = mean(frame_REGULATION_present, na.rm = TRUE) * 100,
    pct_replacement = mean(frame_REPLACEMENT_present, na.rm = TRUE) * 100,

    # Sentiment
    mean_sentiment = mean(sentiment_score, na.rm = TRUE),

    # Content depth
    mean_word_count = mean(word_count, na.rm = TRUE),
    mean_frame_count = mean(frame_count, na.rm = TRUE),

    .groups = "drop"
  ) %>%
  arrange(desc(n_articles))

message("\nOutlet Type Comparison:")
print(outlet_type_stats)

# --- 7.3 Agenda Setting Analysis ---

# Who publishes first on key topics?
first_movers <- clean_data %>%
  group_by(dominant_frame) %>%
  slice_min(DATE, n = 5) %>%
  count(outlet_name, sort = TRUE) %>%
  slice_head(n = 10)

message("\nFirst movers by frame:")
print(first_movers)

# ==============================================================================
# SECTION 8: ACTOR ANALYSIS
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("SECTION 8: Actor Representation Analysis")
message(paste(rep("=", 70), collapse = ""), "\n")

# --- 8.1 Actor Frequency ---

actor_frequency <- clean_data %>%
  summarise(
    across(starts_with("actor_") & ends_with("_count"), sum, .names = "total_{.col}"),
    across(starts_with("actor_") & ends_with("_present"), sum, .names = "articles_{.col}")
  ) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
  mutate(
    type = ifelse(str_detect(metric, "total_"), "Total Mentions", "Articles Present"),
    actor = str_extract(metric, "(?<=actor_)[A-Z_]+(?=_)") %>%
      str_replace_all("_", " ") %>%
      str_to_title()
  ) %>%
  select(-metric) %>%
  pivot_wider(names_from = type, values_from = value)

message("Actor Frequency Analysis:")
print(actor_frequency)

# --- 8.2 Actor Co-occurrence ---

actor_cols <- clean_data %>%
  select(starts_with("actor_") & ends_with("_present"))

# Create co-occurrence matrix
actor_cooccur <- crossprod(as.matrix(actor_cols))
diag(actor_cooccur) <- 0

# Convert to dataframe
actor_cooccur_df <- as.data.frame(actor_cooccur) %>%
  rownames_to_column("actor1") %>%
  pivot_longer(-actor1, names_to = "actor2", values_to = "cooccurrence") %>%
  mutate(
    actor1 = str_extract(actor1, "(?<=actor_)[A-Z_]+(?=_present)"),
    actor2 = str_extract(actor2, "(?<=actor_)[A-Z_]+(?=_present)")
  ) %>%
  filter(actor1 < actor2)  # Remove duplicates

message("\nTop Actor Co-occurrences:")
print(actor_cooccur_df %>% arrange(desc(cooccurrence)) %>% head(10))

# --- 8.3 Actor-Frame Associations ---

actor_frame_assoc <- clean_data %>%
  group_by(primary_actor) %>%
  summarise(
    n = n(),
    pct_threat = mean(frame_THREAT_present) * 100,
    pct_opportunity = mean(frame_OPPORTUNITY_present) * 100,
    pct_regulation = mean(frame_REGULATION_present) * 100,
    pct_replacement = mean(frame_REPLACEMENT_present) * 100,
    mean_sentiment = mean(sentiment_score),
    .groups = "drop"
  ) %>%
  filter(primary_actor != "NONE") %>%
  arrange(desc(n))

message("\nActor-Frame Associations:")
print(actor_frame_assoc)

# ==============================================================================
# SECTION 9: KEYWORD AND TOPIC ANALYSIS
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("SECTION 9: Keyword and Topic Analysis")
message(paste(rep("=", 70), collapse = ""), "\n")

# --- 9.1 Create Corpus ---

message("Creating text corpus...")

# Create quanteda corpus
corpus_articles <- corpus(
  clean_data,
  docid_field = "article_id",
  text_field = "FULL_TEXT"
)

# Add metadata
docvars(corpus_articles, "date") <- clean_data$DATE
docvars(corpus_articles, "year_month") <- clean_data$year_month
docvars(corpus_articles, "outlet") <- clean_data$outlet_name
docvars(corpus_articles, "outlet_type") <- clean_data$outlet_type
docvars(corpus_articles, "phase") <- clean_data$narrative_phase
docvars(corpus_articles, "dominant_frame") <- clean_data$dominant_frame

# --- 9.2 Tokenization and DFM ---

message("Tokenizing and creating document-feature matrix...")

# Croatian stopwords (expand as needed)
croatian_stopwords <- c(
  # Common Croatian stopwords
  "i", "je", "se", "u", "na", "da", "za", "su", "s", "o", "od", "ne",
  "ali", "biti", "što", "koji", "koja", "koje", "kao", "tako", "može",
  "bi", "ili", "samo", "već", "do", "iz", "sve", "još", "a", "to",
  "kako", "kad", "kada", "te", "ga", "mu", "im", "ih", "tom", "toga",
  "tim", "taj", "ta", "te", "ti", "tu", "tko", "zbog", "prema", "preko",
  "prije", "poslije", "nakon", "tijekom", "između", "oko", "kroz",
  "jer", "dok", "nego", "no", "pa", "ma", "li", "ni", "niti",
  # Web-specific
  "www", "http", "https", "hr", "com", "foto", "video", "nastavak",
  # Numbers and misc
  "jedan", "jedna", "jedno", "dva", "dvije", "tri", "četiri", "pet"
)

tokens_articles <- tokens(
  corpus_articles,
  remove_punct = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE,
  remove_url = TRUE
) %>%
  tokens_tolower() %>%
  tokens_remove(c(stopwords("hr"), croatian_stopwords), padding = FALSE) %>%
  tokens_remove(pattern = "^.{1,2}$", valuetype = "regex")  # Remove 1-2 char words

# Create DFM
dfm_articles <- dfm(tokens_articles) %>%
  dfm_trim(min_termfreq = 5, min_docfreq = 3)

message("Corpus: ", ndoc(dfm_articles), " documents")
message("Features: ", nfeat(dfm_articles), " unique terms")

# --- 9.3 Top Keywords ---

# Overall top terms
top_terms_overall <- topfeatures(dfm_articles, n = 50)

# Top terms by phase
top_terms_by_phase <- lapply(unique(clean_data$narrative_phase), function(phase) {
  dfm_subset <- dfm_subset(dfm_articles, phase == docvars(dfm_articles, "phase"))
  data.frame(
    phase = phase,
    term = names(topfeatures(dfm_subset, n = 20)),
    frequency = as.numeric(topfeatures(dfm_subset, n = 20))
  )
})

top_terms_phase_df <- bind_rows(top_terms_by_phase)

message("\nTop 30 Terms Overall:")
print(head(top_terms_overall, 30))

# --- 9.4 Keyword Co-occurrence Network ---

message("Building keyword co-occurrence network...")

# Create FCM (Feature Co-occurrence Matrix)
fcm_articles <- fcm(
  tokens_articles,
  context = "window",
  window = 5,
  tri = FALSE
)

# Get top features for network
top_feats <- names(topfeatures(dfm_articles, 100))
fcm_select <- fcm_select(fcm_articles, pattern = top_feats)

# Convert to igraph
keyword_network <- graph_from_adjacency_matrix(
  as.matrix(fcm_select),
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

# Calculate network metrics
V(keyword_network)$degree <- degree(keyword_network)
V(keyword_network)$betweenness <- betweenness(keyword_network)

# --- 9.5 TF-IDF by Phase ---

message("Calculating TF-IDF by narrative phase...")

tfidf_by_phase <- lapply(unique(clean_data$narrative_phase), function(phase) {
  dfm_phase <- dfm_subset(dfm_articles, phase == docvars(dfm_articles, "phase"))
  tfidf <- dfm_tfidf(dfm_phase)

  # Get mean TF-IDF per term
  mean_tfidf <- colMeans(as.matrix(tfidf))

  data.frame(
    phase = phase,
    term = names(sort(mean_tfidf, decreasing = TRUE)[1:30]),
    tfidf = as.numeric(sort(mean_tfidf, decreasing = TRUE)[1:30])
  )
})

tfidf_phase_df <- bind_rows(tfidf_by_phase)

# ==============================================================================
# SECTION 10: VISUALIZATIONS
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("SECTION 10: Creating Visualizations")
message(paste(rep("=", 70), collapse = ""), "\n")

# Set theme
theme_analysis <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

theme_set(theme_analysis)

# Color palette for frames
frame_colors <- c(
  "THREAT" = "#e41a1c",
  "OPPORTUNITY" = "#4daf4a",
  "REGULATION" = "#377eb8",
  "DISRUPTION" = "#ff7f00",
  "REPLACEMENT" = "#984ea3",
  "QUALITY" = "#ffff33",
  "EQUITY" = "#a65628",
  "COMPETENCE" = "#f781bf",
  "NONE" = "gray70"
)

# --- 10.1 Coverage Volume Over Time ---

message("Creating temporal visualizations...")

p1_volume <- ggplot(monthly_stats, aes(x = year_month, y = n_articles)) +
  geom_col(fill = "#2c7bb6", alpha = 0.8) +
  geom_smooth(method = "loess", se = TRUE, color = "#d7191c", linewidth = 1.2) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
  labs(
    title = "Media Coverage of AI in Croatian Education",
    subtitle = "Monthly article count (2023-2025)",
    x = NULL, y = "Number of Articles"
  )

# Add change points if detected
if (!is.null(change_points_volume)) {
  p1_volume <- p1_volume +
    geom_vline(xintercept = change_points_volume,
               linetype = "dashed", color = "red", alpha = 0.7)
}

ggsave(file.path(CONFIG$output_dir, "01_coverage_volume.png"),
       p1_volume, width = 12, height = 6, dpi = 300)

# --- 10.2 Frame Evolution (Stacked Area) ---

frame_evolution <- monthly_stats %>%
  select(year_month, starts_with("prop_")) %>%
  pivot_longer(-year_month, names_to = "frame", values_to = "proportion") %>%
  mutate(frame = str_remove(frame, "prop_"))

p2_frames <- ggplot(frame_evolution, aes(x = year_month, y = proportion, fill = frame)) +
  geom_area(alpha = 0.8) +
  scale_fill_manual(values = frame_colors) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Evolution of Media Frames Over Time",
    subtitle = "Proportion of articles containing each frame",
    x = NULL, y = "Proportion", fill = "Frame"
  )

ggsave(file.path(CONFIG$output_dir, "02_frame_evolution_stacked.png"),
       p2_frames, width = 12, height = 7, dpi = 300)

# --- 10.3 Frame Lines (Individual Trends) ---

p3_frame_lines <- ggplot(frame_evolution, aes(x = year_month, y = proportion,
                                              color = frame, group = frame)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = frame_colors) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~frame, scales = "free_y", ncol = 2) +
  labs(
    title = "Individual Frame Trajectories",
    subtitle = "Monthly proportion of articles per frame",
    x = NULL, y = "Proportion"
  ) +
  theme(legend.position = "none")

ggsave(file.path(CONFIG$output_dir, "03_frame_individual_trends.png"),
       p3_frame_lines, width = 14, height = 12, dpi = 300)

# --- 10.4 Sentiment Over Time ---

p4_sentiment <- ggplot(monthly_stats, aes(x = year_month)) +
  geom_ribbon(aes(ymin = 0, ymax = pmax(mean_sentiment, 0)),
              fill = "#4daf4a", alpha = 0.5) +
  geom_ribbon(aes(ymin = pmin(mean_sentiment, 0), ymax = 0),
              fill = "#e41a1c", alpha = 0.5) +
  geom_line(aes(y = mean_sentiment), linewidth = 1.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
  labs(
    title = "Sentiment Trajectory Over Time",
    subtitle = "Mean sentiment score (positive - negative word counts)",
    x = NULL, y = "Mean Sentiment Score"
  )

ggsave(file.path(CONFIG$output_dir, "04_sentiment_over_time.png"),
       p4_sentiment, width = 12, height = 6, dpi = 300)

# --- 10.5 Outlet Type Comparison ---

outlet_type_long <- outlet_type_stats %>%
  select(outlet_type, pct_threat, pct_opportunity, pct_regulation, pct_replacement) %>%
  pivot_longer(-outlet_type, names_to = "frame", values_to = "percentage") %>%
  mutate(frame = str_remove(frame, "pct_") %>% str_to_upper())

p5_outlet_frames <- ggplot(outlet_type_long,
                           aes(x = reorder(outlet_type, percentage),
                               y = percentage, fill = frame)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = frame_colors) +
  coord_flip() +
  labs(
    title = "Frame Usage by Outlet Type",
    subtitle = "Percentage of articles containing each frame",
    x = NULL, y = "Percentage", fill = "Frame"
  )

ggsave(file.path(CONFIG$output_dir, "05_outlet_type_frames.png"),
       p5_outlet_frames, width = 10, height = 7, dpi = 300)

# --- 10.6 Top Outlets by Volume ---

p6_top_outlets <- outlet_stats %>%
  head(20) %>%
  ggplot(aes(x = reorder(outlet_name, n_articles), y = n_articles, fill = outlet_type)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Top 20 Outlets by Coverage Volume",
    subtitle = "Total articles about AI in education",
    x = NULL, y = "Number of Articles", fill = "Type"
  )

ggsave(file.path(CONFIG$output_dir, "06_top_outlets.png"),
       p6_top_outlets, width = 10, height = 8, dpi = 300)

# --- 10.7 Outlet Frame Profiles (Heatmap) ---

outlet_frame_matrix <- outlet_stats %>%
  head(20) %>%
  select(outlet_name, pct_threat, pct_opportunity, pct_regulation) %>%
  pivot_longer(-outlet_name, names_to = "frame", values_to = "percentage") %>%
  mutate(frame = str_remove(frame, "pct_") %>% str_to_title())

p7_outlet_heatmap <- ggplot(outlet_frame_matrix,
                            aes(x = frame, y = reorder(outlet_name, percentage),
                                fill = percentage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(percentage, 1)), size = 3) +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "Outlet Frame Profiles",
    subtitle = "Percentage of articles containing each frame (top 20 outlets)",
    x = NULL, y = NULL, fill = "% Articles"
  )

ggsave(file.path(CONFIG$output_dir, "07_outlet_frame_heatmap.png"),
       p7_outlet_heatmap, width = 10, height = 10, dpi = 300)

# --- 10.8 Actor Frequency ---

actor_plot_data <- actor_frequency %>%
  arrange(desc(`Total Mentions`))

p8_actors <- ggplot(actor_plot_data,
                    aes(x = reorder(actor, `Total Mentions`), y = `Total Mentions`)) +
  geom_col(fill = "#2c7bb6", alpha = 0.8) +
  geom_text(aes(label = `Articles Present`, y = `Total Mentions` + max(`Total Mentions`) * 0.02),
            hjust = 0, size = 3) +
  coord_flip() +
  labs(
    title = "Actor Representation in Coverage",
    subtitle = "Total mentions (bars) and number of articles present (labels)",
    x = NULL, y = "Total Mentions"
  )

ggsave(file.path(CONFIG$output_dir, "08_actor_frequency.png"),
       p8_actors, width = 10, height = 6, dpi = 300)

# --- 10.9 Phase Comparison ---

phase_stats <- clean_data %>%
  group_by(narrative_phase) %>%
  summarise(
    n = n(),
    threat = mean(frame_THREAT_present) * 100,
    opportunity = mean(frame_OPPORTUNITY_present) * 100,
    regulation = mean(frame_REGULATION_present) * 100,
    sentiment = mean(sentiment_score),
    .groups = "drop"
  ) %>%
  mutate(narrative_phase = factor(narrative_phase, levels = unique(clean_data$narrative_phase)))

phase_long <- phase_stats %>%
  select(narrative_phase, threat, opportunity, regulation) %>%
  pivot_longer(-narrative_phase, names_to = "frame", values_to = "percentage")

p9_phases <- ggplot(phase_long, aes(x = narrative_phase, y = percentage, fill = frame)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("threat" = "#e41a1c", "opportunity" = "#4daf4a",
                               "regulation" = "#377eb8")) +
  labs(
    title = "Frame Distribution by Narrative Phase",
    subtitle = "How dominant frames shift across coverage periods",
    x = NULL, y = "Percentage of Articles", fill = "Frame"
  ) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

ggsave(file.path(CONFIG$output_dir, "09_phase_comparison.png"),
       p9_phases, width = 11, height = 7, dpi = 300)

# --- 10.10 Day of Week Pattern ---

dow_stats <- clean_data %>%
  count(day_of_week) %>%
  mutate(percentage = n / sum(n) * 100)

p10_dow <- ggplot(dow_stats, aes(x = day_of_week, y = n)) +
  geom_col(fill = "#2c7bb6", alpha = 0.8) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
  labs(
    title = "Publication Day Patterns",
    subtitle = "Distribution of articles by day of week",
    x = NULL, y = "Number of Articles"
  )

ggsave(file.path(CONFIG$output_dir, "10_day_of_week.png"),
       p10_dow, width = 10, height = 6, dpi = 300)

# --- 10.11 Keyword Network ---

message("Creating keyword network visualization...")

# Simplify network for visualization (top edges)
E(keyword_network)$width <- E(keyword_network)$weight / max(E(keyword_network)$weight) * 5
keyword_network_simple <- delete.edges(
  keyword_network,
  which(E(keyword_network)$weight < quantile(E(keyword_network)$weight, 0.9))
)

# Plot with ggraph
set.seed(42)
p11_network <- ggraph(keyword_network_simple, layout = "fr") +
  geom_edge_link(aes(alpha = weight, width = weight), color = "gray60") +
  geom_node_point(aes(size = degree), color = "#2c7bb6") +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_edge_width(range = c(0.5, 3)) +
  scale_size(range = c(3, 12)) +
  labs(
    title = "Keyword Co-occurrence Network",
    subtitle = "Terms frequently appearing together in coverage"
  ) +
  theme_void() +
  theme(legend.position = "none")

ggsave(file.path(CONFIG$output_dir, "11_keyword_network.png"),
       p11_network, width = 14, height = 12, dpi = 300)

# --- 10.12 Frame Co-occurrence Heatmap ---

frame_cols <- clean_data %>%
  select(starts_with("frame_") & ends_with("_present"))

frame_cooccur <- crossprod(as.matrix(frame_cols))
frame_cooccur_norm <- frame_cooccur / diag(frame_cooccur)  # Normalize by diagonal

frame_cooccur_df <- as.data.frame(frame_cooccur_norm) %>%
  rownames_to_column("frame1") %>%
  pivot_longer(-frame1, names_to = "frame2", values_to = "cooccurrence") %>%
  mutate(
    frame1 = str_extract(frame1, "(?<=frame_)[A-Z]+"),
    frame2 = str_extract(frame2, "(?<=frame_)[A-Z]+")
  )

p12_frame_cooccur <- ggplot(frame_cooccur_df,
                            aes(x = frame1, y = frame2, fill = cooccurrence)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(cooccurrence, 2)), size = 3) +
  scale_fill_viridis_c(option = "magma") +
  labs(
    title = "Frame Co-occurrence Matrix",
    subtitle = "How often frames appear together (normalized)",
    x = NULL, y = NULL, fill = "Co-occurrence"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(CONFIG$output_dir, "12_frame_cooccurrence.png"),
       p12_frame_cooccur, width = 10, height = 9, dpi = 300)

# --- 10.13 Word Frequency Comparison by Phase ---

p13_tfidf <- tfidf_phase_df %>%
  group_by(phase) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder_within(term, tfidf, phase), y = tfidf)) +
  geom_col(fill = "#2c7bb6", alpha = 0.8) +
  coord_flip() +
  facet_wrap(~phase, scales = "free_y", ncol = 2) +
  scale_x_reordered() +
  labs(
    title = "Distinctive Terms by Narrative Phase",
    subtitle = "Top TF-IDF weighted terms per phase",
    x = NULL, y = "TF-IDF Score"
  )

ggsave(file.path(CONFIG$output_dir, "13_tfidf_by_phase.png"),
       p13_tfidf, width = 14, height = 12, dpi = 300)

# --- 10.14 Combined Dashboard ---

message("Creating combined dashboard...")

p_dashboard <- (p1_volume | p4_sentiment) /
  (p9_phases | p6_top_outlets) +
  plot_annotation(
    title = "AI in Croatian Education: Media Analysis Dashboard",
    subtitle = paste0("Based on ", format(nrow(clean_data), big.mark = ","),
                      " articles from ", min(clean_data$DATE), " to ", max(clean_data$DATE)),
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

ggsave(file.path(CONFIG$output_dir, "14_dashboard.png"),
       p_dashboard, width = 18, height = 14, dpi = 300)

message("All visualizations saved to: ", CONFIG$output_dir)

# ==============================================================================
# SECTION 11: STATISTICAL TESTS
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("SECTION 11: Statistical Analysis")
message(paste(rep("=", 70), collapse = ""), "\n")

# --- 11.1 Chi-Square: Frame vs Outlet Type ---

frame_outlet_table <- table(clean_data$dominant_frame, clean_data$outlet_type)
chisq_frame_outlet <- chisq.test(frame_outlet_table)

message("Chi-Square Test: Dominant Frame vs. Outlet Type")
message("  X² = ", round(chisq_frame_outlet$statistic, 2))
message("  df = ", chisq_frame_outlet$parameter)
message("  p = ", format(chisq_frame_outlet$p.value, scientific = TRUE))

# --- 11.2 ANOVA: Sentiment by Phase ---

anova_sentiment_phase <- aov(sentiment_score ~ narrative_phase, data = clean_data)
anova_summary <- summary(anova_sentiment_phase)

message("\nANOVA: Sentiment by Narrative Phase")
print(anova_summary)

# Post-hoc if significant
if (anova_summary[[1]]$`Pr(>F)`[1] < 0.05) {
  tukey_result <- TukeyHSD(anova_sentiment_phase)
  message("\nTukey HSD Post-hoc:")
  print(tukey_result)
}

# --- 11.3 Correlation: Frame Intensities ---

frame_count_cols <- clean_data %>%
  select(starts_with("frame_") & ends_with("_count"))

frame_correlations <- cor(frame_count_cols, use = "pairwise.complete.obs")

message("\nFrame Correlation Matrix:")
print(round(frame_correlations, 2))

# --- 11.4 Time Series Trend Tests ---

message("\nTrend Analysis (Mann-Kendall style):")

# Simple linear trend for key metrics
trend_volume <- lm(n_articles ~ as.numeric(year_month), data = monthly_stats)
trend_threat <- lm(prop_THREAT ~ as.numeric(year_month), data = monthly_stats)
trend_opportunity <- lm(prop_OPPORTUNITY ~ as.numeric(year_month), data = monthly_stats)

message("  Volume trend: ", ifelse(coef(trend_volume)[2] > 0, "Increasing", "Decreasing"),
        " (p = ", round(summary(trend_volume)$coefficients[2, 4], 4), ")")
message("  Threat trend: ", ifelse(coef(trend_threat)[2] > 0, "Increasing", "Decreasing"),
        " (p = ", round(summary(trend_threat)$coefficients[2, 4], 4), ")")
message("  Opportunity trend: ", ifelse(coef(trend_opportunity)[2] > 0, "Increasing", "Decreasing"),
        " (p = ", round(summary(trend_opportunity)$coefficients[2, 4], 4), ")")

# ==============================================================================
# SECTION 12: VALIDATION SAMPLE
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("SECTION 12: Generating Validation Sample")
message(paste(rep("=", 70), collapse = ""), "\n")

# Stratified sample by phase and outlet type
set.seed(42)

validation_sample <- clean_data %>%
  group_by(narrative_phase, outlet_type) %>%
  slice_sample(n = min(ceiling(CONFIG$sample_size_for_validation /
                                 (n_distinct(clean_data$narrative_phase) *
                                    n_distinct(clean_data$outlet_type))), n())) %>%
  ungroup() %>%
  slice_sample(n = CONFIG$sample_size_for_validation) %>%
  select(
    article_id, DATE, outlet_name, outlet_type, narrative_phase,
    TITLE, URL,
    dominant_frame, sentiment_category,
    # Leave space for manual coding
    frame_THREAT_present, frame_OPPORTUNITY_present, frame_REGULATION_present
  ) %>%
  mutate(
    # Empty columns for manual coding
    manual_frame = "",
    manual_sentiment = "",
    manual_notes = ""
  )

message("Validation sample created: ", nrow(validation_sample), " articles")
message("Stratification: Phase x Outlet Type")

# ==============================================================================
# SECTION 13: EXPORT ALL RESULTS
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("SECTION 13: Exporting Results")
message(paste(rep("=", 70), collapse = ""), "\n")

# --- 13.1 Create Comprehensive Excel Workbook ---

wb <- createWorkbook()

# Sheet 1: Full Dataset
addWorksheet(wb, "Full_Data")
writeData(wb, "Full_Data", clean_data)

# Sheet 2: Monthly Statistics
addWorksheet(wb, "Monthly_Stats")
writeData(wb, "Monthly_Stats", monthly_stats)

# Sheet 3: Outlet Statistics
addWorksheet(wb, "Outlet_Stats")
writeData(wb, "Outlet_Stats", outlet_stats)

# Sheet 4: Outlet Type Comparison
addWorksheet(wb, "Outlet_Type_Stats")
writeData(wb, "Outlet_Type_Stats", outlet_type_stats)

# Sheet 5: Phase Summary
addWorksheet(wb, "Phase_Summary")
writeData(wb, "Phase_Summary", phase_summary)

# Sheet 6: Actor Frequency
addWorksheet(wb, "Actor_Frequency")
writeData(wb, "Actor_Frequency", actor_frequency)

# Sheet 7: Actor-Frame Associations
addWorksheet(wb, "Actor_Frame_Assoc")
writeData(wb, "Actor_Frame_Assoc", actor_frame_assoc)

# Sheet 8: Top Terms by Phase
addWorksheet(wb, "Top_Terms_Phase")
writeData(wb, "Top_Terms_Phase", top_terms_phase_df)

# Sheet 9: TF-IDF by Phase
addWorksheet(wb, "TFIDF_Phase")
writeData(wb, "TFIDF_Phase", tfidf_phase_df)

# Sheet 10: Frame Co-occurrence
addWorksheet(wb, "Frame_Cooccurrence")
writeData(wb, "Frame_Cooccurrence", frame_cooccur_df)

# Sheet 11: Actor Co-occurrence
addWorksheet(wb, "Actor_Cooccurrence")
writeData(wb, "Actor_Cooccurrence", actor_cooccur_df)

# Sheet 12: Validation Sample
addWorksheet(wb, "Validation_Sample")
writeData(wb, "Validation_Sample", validation_sample)

# Sheet 13: Statistical Tests Summary
stat_tests_summary <- data.frame(
  Test = c("Chi-Square: Frame vs Outlet Type",
           "ANOVA: Sentiment by Phase",
           "Trend: Volume", "Trend: Threat", "Trend: Opportunity"),
  Statistic = c(round(chisq_frame_outlet$statistic, 2),
                round(anova_summary[[1]]$`F value`[1], 2),
                round(coef(trend_volume)[2], 4),
                round(coef(trend_threat)[2], 4),
                round(coef(trend_opportunity)[2], 4)),
  P_Value = c(format(chisq_frame_outlet$p.value, scientific = TRUE),
              format(anova_summary[[1]]$`Pr(>F)`[1], scientific = TRUE),
              format(summary(trend_volume)$coefficients[2, 4], scientific = TRUE),
              format(summary(trend_threat)$coefficients[2, 4], scientific = TRUE),
              format(summary(trend_opportunity)$coefficients[2, 4], scientific = TRUE)),
  Interpretation = c(
    ifelse(chisq_frame_outlet$p.value < 0.05, "Significant association", "No significant association"),
    ifelse(anova_summary[[1]]$`Pr(>F)`[1] < 0.05, "Significant phase differences", "No significant differences"),
    ifelse(coef(trend_volume)[2] > 0, "Increasing coverage", "Decreasing coverage"),
    ifelse(coef(trend_threat)[2] > 0, "Threat frame increasing", "Threat frame decreasing"),
    ifelse(coef(trend_opportunity)[2] > 0, "Opportunity frame increasing", "Opportunity frame decreasing")
  )
)
addWorksheet(wb, "Statistical_Tests")
writeData(wb, "Statistical_Tests", stat_tests_summary)

# Sheet 14: Frame Dictionaries (for reference)
dict_df <- lapply(names(frame_dictionaries), function(frame) {
  data.frame(
    frame = frame,
    terms = paste(frame_dictionaries[[frame]], collapse = ", ")
  )
}) %>% bind_rows()

addWorksheet(wb, "Frame_Dictionaries")
writeData(wb, "Frame_Dictionaries", dict_df)

# Save workbook
output_excel <- file.path(CONFIG$output_dir, "AI_Education_Analysis_Complete.xlsx")
saveWorkbook(wb, output_excel, overwrite = TRUE)
message("Excel workbook saved: ", output_excel)

# --- 13.2 Save R Objects ---

save(
  clean_data, monthly_stats, outlet_stats, outlet_type_stats,
  frame_dictionaries, actor_dictionaries, sentiment_dictionary,
  corpus_articles, dfm_articles, keyword_network,
  phase_summary, actor_frequency, actor_frame_assoc,
  file = file.path(CONFIG$output_dir, "analysis_objects.RData")
)
message("R objects saved: ", file.path(CONFIG$output_dir, "analysis_objects.RData"))

# --- 13.3 Generate Summary Report ---

summary_report <- paste0(
  "# AI in Croatian Education: Media Analysis Summary\n\n",
  "**Generated:** ", Sys.time(), "\n\n",
  "## Data Overview\n",
  "- **Total articles analyzed:** ", format(nrow(clean_data), big.mark = ","), "\n",
  "- **Date range:** ", min(clean_data$DATE), " to ", max(clean_data$DATE), "\n",
  "- **Unique outlets:** ", n_distinct(clean_data$outlet_name), "\n",
  "- **Total words analyzed:** ", format(sum(clean_data$word_count), big.mark = ","), "\n\n",

  "## Key Findings\n\n",
  "### Temporal Patterns\n",
  "- Coverage volume trend: ", ifelse(coef(trend_volume)[2] > 0, "Increasing", "Decreasing"), "\n",
  "- Change points detected: ", ifelse(!is.null(change_points_volume),
                                       paste(format(change_points_volume, "%Y-%m"), collapse = ", "),
                                       "None"), "\n\n",

  "### Dominant Frames\n",
  "- Most common frame: ", names(which.max(table(clean_data$dominant_frame))), "\n",
  "- THREAT frame trend: ", ifelse(coef(trend_threat)[2] > 0, "Increasing", "Decreasing"), "\n",
  "- OPPORTUNITY frame trend: ", ifelse(coef(trend_opportunity)[2] > 0, "Increasing", "Decreasing"), "\n\n",

  "### Source Analysis\n",
  "- Top outlet: ", outlet_stats$outlet_name[1], " (", outlet_stats$n_articles[1], " articles)\n",
  "- Dominant outlet type: ", names(which.max(table(clean_data$outlet_type))), "\n",
  "- Frame-outlet association: ", ifelse(chisq_frame_outlet$p.value < 0.05, "Significant", "Not significant"), "\n\n",

  "### Sentiment\n",
  "- Overall mean sentiment: ", round(mean(clean_data$sentiment_score), 2), "\n",
  "- Positive articles: ", round(mean(clean_data$sentiment_category == "Positive") * 100, 1), "%\n",
  "- Negative articles: ", round(mean(clean_data$sentiment_category == "Negative") * 100, 1), "%\n\n",

  "### Actor Representation\n",
  "- Most mentioned actor: ", actor_frequency$actor[1], "\n",
  "- Student-teacher mention ratio: ", round(sum(clean_data$actor_STUDENTS_count) /
                                               sum(clean_data$actor_TEACHERS_count), 2), "\n\n",

  "## Files Generated\n",
  "- Excel workbook: AI_Education_Analysis_Complete.xlsx\n",
  "- R objects: analysis_objects.RData\n",
  "- 14 visualization files (.png)\n",
  "- This summary: analysis_summary.md\n"
)

writeLines(summary_report, file.path(CONFIG$output_dir, "analysis_summary.md"))
message("Summary report saved: ", file.path(CONFIG$output_dir, "analysis_summary.md"))

# ==============================================================================
# SECTION 14: FINAL SUMMARY
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("ANALYSIS COMPLETE")
message(paste(rep("=", 70), collapse = ""))

message("\n📊 DATASET")
message("   Articles analyzed: ", format(nrow(clean_data), big.mark = ","))
message("   Date range: ", min(clean_data$DATE), " to ", max(clean_data$DATE))
message("   Unique outlets: ", n_distinct(clean_data$outlet_name))

message("\n📈 KEY FINDINGS")
message("   Dominant frame: ", names(which.max(table(clean_data$dominant_frame))))
message("   Overall sentiment: ", round(mean(clean_data$sentiment_score), 2))
message("   Most active outlet: ", outlet_stats$outlet_name[1])

message("\n📁 OUTPUT FILES")
message("   Location: ", CONFIG$output_dir)
message("   - AI_Education_Analysis_Complete.xlsx (14 sheets)")
message("   - analysis_objects.RData")
message("   - analysis_summary.md")
message("   - 14 visualization files")

message("\n⏱️  Total runtime: ", round(difftime(Sys.time(), start_time, units = "mins"), 1), " minutes")

message("\n", paste(rep("=", 70), collapse = ""))
message("Next steps:")
message("1. Review validation sample and code manually")
message("2. Refine frame dictionaries based on findings")
message("3. Investigate outlet-specific patterns")
message("4. Conduct qualitative deep-dives on key articles")
message(paste(rep("=", 70), collapse = ""), "\n")

# Clean up
gc()