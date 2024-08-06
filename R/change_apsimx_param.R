#' @title Changing .apsimx file parameters values
#'
#' @description This function apply parameters values changes in an . apsimx
#' using a paarameters named vector of values.
#'
#' @param file_to_run a .apsimx file path
#'
#' @param param_values a named vector of parameters values
#'
#' @return TRUE if changes are successful, FALSE otherwise
#'
#' @export
#'
change_apsimx_param <- function(exe, file_to_run, param_values) {
  # Generate config file containing parameter changes ---------------------------
  config_file <- generate_config_file(param_values)

  # Apply parameter changes to the model -----------------------------------------
  cmd <- paste(exe, file_to_run, '/Edit', config_file)
  if (.Platform$OS.type == 'unix') {
    # need to run via mono on unices
    cmd <- paste('mono', cmd)
  }
  #edit_file_stdout <- shell(cmd, translate = FALSE, intern = TRUE, mustWork = TRUE)
  edit_file_stdout <- system(cmd, wait = TRUE, intern = TRUE)

  # returning the changes status
  success <- is.null(attr(edit_file_stdout,"status"))
  if (!success) {
    print(edit_file_stdout)
  }

  return(success)

}

#' @title Changing .apsimx file parameters values
#'
#' @description This function generates a config file which can be passed
#' to apsim in order to apply parameter value changes in an .apsimx file
#' using parameters, a named vector of values.
#'
#' @param file_to_run .apsimx file path
#'
#' @param param_values a named vector of parameters values
#'
#' @return path to the config file
#'
#' @export
#'
generate_config_file <- function(param_values) {
  config_file <- tempfile('apsimOnR', fileext = '.conf')
  parameter_names <- names(param_values)
  fileConn <- file(config_file)
  lines <- vector("character", length(param_values))
  for (i in 1:length(param_values)) {
    param_name <- parameter_names[i]
    param_value <- as.character(param_values[i])
    lines[i] <- switch(
      param_name,
      ".Simulations.Replacements.Soybean.Phenology.VegetativeThermalTime.Response.X" = paste0(param_name, " = 10, 20, ", param_value, ", 40"),
      ".Simulations.Replacements.Soybean.Phenology.ReproductiveThermalTime.Response.X" = paste0(param_name, " = 10, 15, ", param_value, ", 40"),
      ".Simulations.Replacements.Soybean.Phenology.VegetativePhotoperiodModifier.XYPairs.X" = paste0(param_name, " = ", param_value, ", 15.8"),
      ".Simulations.Replacements.Soybean.Phenology.ReproductivePhotoperiodModifier.XYPairs.X" = paste0(param_name, " = ", param_value, ", 15.8"),
      ".Simulations.Replacements.Soybean.Leaf.ExtinctionCoefficient.XYPairs.Y" = paste0(param_name, " = ", param_value, ", 0.4"),
      paste(param_name, "=", param_value)
    )
  }
  writeLines(lines, fileConn)
  close(fileConn)
  return(config_file)
}
