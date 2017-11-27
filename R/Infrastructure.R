## Code for building package infrastructure. Included for documentation
## purposes, but should not be run.

gen_data_paths <- function(path = "../predictions/EPANO2") {

  # Initialize List
  file.yaml <- list()

  # get names of variables in dataset
  varlist <- yaml.load_file(file.path(path.package("airpred"),"yaml_files",
                                      "Data_Location.yml"))


  for (variable in names(varlist)) {
      if (length(varlist[[variable]]) == 2) {
        directory <- varlist[[variable]][1]
        files <- list.files(file.path(path, directory),
                            pattern = varlist[[variable]][2])
      } else {
        directory <- varlist[[variable]][2]
        files <- list.files(file.path(path, varlist[[variable]][2]),
                            pattern = varlist[[variable]][3])
      }
      if (length(files) > 0) {
        listname <- variable
        while (!is.null(file.yaml[[listname]])){
          listname <- paste0(listname, "bad")
        }
        file.yaml[[listname]] <- files
      } else {
        file.yaml[[variable]] <- c("bad", directory)
      }
    }

  return(file.yaml)
}
