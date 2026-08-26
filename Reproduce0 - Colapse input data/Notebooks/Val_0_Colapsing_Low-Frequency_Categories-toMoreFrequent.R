# 1. Defining function


collapse_logic <- function(file_name) {

  out_dir <- "Reproduce0 - Colapse input data/Data_Analysis"
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  

  base_file   <- basename(file_name)
  output_name <- gsub(".csv", "_lowfreqcollapsed.csv", base_file)
  output_path <- file.path(out_dir, output_name)
  
  log_name    <- gsub(".csv", "_changes_made.csv", base_file)
  log_path    <- file.path(out_dir, log_name)
  
 
  if (!file.exists(file_name)) {
    message("--- Arquivo não encontrado: ", file_name)
    return(NULL)
  }
  
  df_mod <- read.csv(file_name, check.names = FALSE)
  items <- names(df_mod)[sapply(df_mod, is.numeric)]
  
  message("---Porcessing: ", file_name)
  changes_made <- 0
  

  items <- setdiff(names(df_mod), c("id", "seed", "stimulus"))
  
 
  fusion_log <- data.frame(
    item           = character(),
    origem         = numeric(),
    destino        = numeric(),
    prop_original  = numeric(),
    n_casos        = numeric(),
    stringsAsFactors = FALSE
  )
  
  changes_made <- 0
  
  for (item in items) {
    # Ignore id and non numeric columns
    if (item == "id" || !is.numeric(df_mod[[item]])) next
    
    repeat {
      counts <- table(df_mod[[item]], useNA = "no")
      if (length(counts) <= 1) break 
      
      props <- prop.table(counts)
      low_freq_cats <- as.numeric(names(props[props <= threshold]))
      if (length(low_freq_cats) == 0) break
      
      # Escala com categorias reais ativas
      active_scale <- sort(as.numeric(names(counts)))
      
      target <- low_freq_cats[1]
      current_idx <- which(active_scale == target)
      
      left_val  <- if (current_idx > 1) active_scale[current_idx - 1] else NA
      right_val <- if (current_idx < length(active_scale)) active_scale[current_idx + 1] else NA
      
      c_left  <- if (!is.na(left_val)) counts[as.character(left_val)] else -1
      c_right <- if (!is.na(right_val)) counts[as.character(right_val)] else -1
      
      # Critério de desempate e escolha do vizinho mais populoso
      if (!is.na(left_val) && !is.na(right_val)) {
        destination <- if (c_left >= c_right) left_val else right_val
      } else if (!is.na(left_val)) {
        destination <- left_val
      } else if (!is.na(right_val)) {
        destination <- right_val
      } else {
        break
      }
      
      # REGISTRO NO LOG
      n_casos_target <- as.numeric(counts[as.character(target)])
      prop_target    <- as.numeric(props[as.character(target)])
      
      fusion_log <- rbind(
        fusion_log,
        data.frame(
          item          = item,
          origem        = target,
          destino       = destination,
          prop_original = round(prop_target, 4),
          n_casos       = n_casos_target,
          stringsAsFactors = FALSE
        )
      )
      
      # Executa a substituição
      df_mod[[item]][df_mod[[item]] == target] <- destination
      changes_made <- changes_made + 1
    }
  }
  
  
  print("beforeWriting")
  
  write.csv(df_mod, output_path, row.names = FALSE)
  write.csv(fusion_log,  log_path, row.names = FALSE)
  message("DONE: Save in ", output_name, " (Total: ", changes_made, ")")
} # 

# 2
likert_scale <- c(1, 2, 3, 4, 5, 6, 7)
threshold <- 0.025
#files_to_process <- paste0("ratings-", 1:6, ".csv") #used to collaps main files
#files_to_process <- paste0("ratings-","stimulus", ".csv")#used to collaps file with stimulus


# Target single dataset containing NAs
files_to_process <- c("Reproduce0 - Colapse input data/Data/multigroup_ratings-withNAs-withoutAttFails.csv")

getwd()
for (f in files_to_process) {
  collapse_logic(f)
}