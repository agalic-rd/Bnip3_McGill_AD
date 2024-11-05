####╔═════   ═════╗####
####💠Loading Data💠####
####╚═════   ═════╝####

cli_h2("┗ [SCRIPTS] Loading data ingestion functions")

#-------------------------#
####🔺Helper functions ####
#-------------------------#




#-------------------------------#
####🔺Data loading functions ####
#-------------------------------#

# Loading and shaping DAB data
load_od_data <- function(path = configs$data$OD$od_corrected) {
  
  od_data <- (
    readxl::read_excel(path)
    |> janitor::clean_names()
    |> mutate(
      section = consecutive_id(background),
      od_corrected = as.numeric(od_corrected_new), # cell_dab_od_mean, background
      od_corrected = ifelse(od_corrected < 0, 0, od_corrected),
      age = paste0("M", age),
      rat = paste0("ID", rat),
      age = factor(age, levels = c("M3", "M12", "M18")),
      rat = factor(rat),
      section = factor(section),
      condition = factor(toupper(condition), levels = c("WT", "AD")),
      location = factor(toupper(layer)),
      .keep = "unused"
    )
    |> filter(od_corrected > 0)
    |> select(rat, age, condition, location, section, od_corrected)
    |> arrange(age, condition, rat, location, section)
  )
  
  return(od_data)
}

# Deprecated
load_od_data_old <- function(path = configs$data$OD$od_raw) {
  
  od_data <- (
    readxl::read_excel(path)
    |> janitor::clean_names()
    |> mutate(
      section = consecutive_id(background_od),
      od_corrected = cell_od_mean - background_od,
      age = paste0("M", age),
      rat = paste0("ID", rat),
      age = factor(age, levels = c("M3", "M12", "M18")),
      rat = factor(rat),
      section = factor(section),
      condition = factor(toupper(condition)),
      location = factor(toupper(layer)),
      .keep = "unused"
    )
    |> filter(od_corrected > 0)
    |> arrange(age, condition, rat, location, section)
  )
  
  return(od_data)
}
