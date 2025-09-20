####╔═════   ═════╗####
####💠Loading Data💠####
####╚═════   ═════╝####

cli_h2("┗ [SCRIPTS] Loading data ingestion functions")

#-------------------------------#
####🔺Data loading functions ####
#-------------------------------#

# Loading and shaping DAB data
load_od_data <- function() {
  
  soma_data <- (
    readxl::read_excel(configs$data$OD$soma_raw)
    |> janitor::clean_names()
    |> mutate(
      section = consecutive_id(background),
      od_corrected = as.numeric(od_corrected),
      od_corrected = ifelse(od_corrected < 0, 0, od_corrected),
      age = paste0("M", age),
      rat = paste0("ID", rat),
      age = factor(age, levels = c("M3", "M12", "M18")),
      rat = factor(rat),
      section = factor(section),
      genotype_recoded = ifelse(toupper(genotype) == "AD", "McGill", toupper(genotype)),
      genotype = factor(genotype_recoded, levels = c("WT", "McGill")),
      area = factor(toupper(area)),
      .keep = "unused"
    )
    |> filter(od_corrected > 0)
    |> select(rat, age, genotype, area, section, od_corrected)
    |> arrange(age, genotype, rat, area, section)
  )
  
  molecular_layer_data <- (
    readxl::read_excel(configs$data$OD$ml_raw)
    |> janitor::clean_names()
    |> mutate(
      od_corrected = ifelse(od_corrected < 0, 0, od_corrected),
      age = factor(paste0("M", age), levels = c("M3", "M12", "M18")),
      rat = factor(paste0("ID", rat)),
      genotype_recoded = ifelse(toupper(genotype) == "AD", "McGill", toupper(genotype)),
      genotype = factor(genotype_recoded, levels = c("WT", "McGill")),
      .keep = "unused"
    )
    |> select(rat, age, genotype, od_corrected)
    |> arrange(age, genotype, rat)
  )
  
  return(list(soma = soma_data, ml = molecular_layer_data))
}
