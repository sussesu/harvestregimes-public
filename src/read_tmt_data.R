#' Read TmT files
#'
#' @param ftype type of file ("tree", "spp", "census", "plot", "network", 
#' "treedata_biomass", "stand_dynamics", "treedata_traits", "CWMalive" or "CWMdead")
#' @param datatype type of data wanted, has to match the folder name 
#' ("Original_data", "Processed_data", or "TMtFormat")
#' @param region region from which to look for data, has to match the folder name
#' (e.g., "NFI-Europe", "NFI-North-America")
#' @param database.code vector of database.codes that should be included. 
#' Need to match the codes in file names.
#' @param list.files.only If TRUE, the function will not read the data, but will 
#' only list and print the file names and their time stamps.
#'
#' @return list of dataframes, each list element is input read from file
#' @export

read_tmt <- function(ftype,
                     datatype="TMtFormat",
                     region="NFI-Europe", 
                     database.code = "all",
                     combine = TRUE,
                     tmt.db.dir = "./data/raw/TreeMort-database",
                     list.files.only = FALSE) {
  
  # define path based on datatype and region
  tmt_dir <- paste(tmt.db.dir, datatype, region, sep="/")
  
  # define database.codes if "all"
  if(any(database.code == "all")){
    if(region == "NFI-Europe") {
      database.code <- c("NCZ", "NFL", "FUN", "NFG", "NNL", "NPO", "NSW", "NSI", "NFR", "NSP")
    }
    if (region == "NFI-North-America") {
      database.code <- c("FIAN", "FIANE", "FIANW", "FIARM", "FIAS")
    }
    if (!(region%in%c("NFI-Europe", "NFI-North-America"))) {
      stop("Default option 'all' for database.code works only for NFI-Europe and FIA,
         for other regions specify database codes manually!")
    }
  }

  filetype <- list(tree = "01_qc-treedata_TMt_",
                spp = "02_qc-sppdata_TMt_",
                census = "03_census-info_TMt_",
                plot = "04_plot-info_TMt_",
                network = "10_qc-network_TMt_",
                treedata_biomass = "01_treedata-biomass_TMt_",
                stand_dynamics = "02_stand-level-dynamics_TMt_",
                treedata_traits = "03_treedata-traits_TMt_",
                CWMalive = "04_CWMalive_TMt_",
                CWMdead = "05_CWMdead_TMt_")
  
  fl_names <- paste0(filetype[[ftype]],database.code, ".csv")
  
  fls <- sapply(fl_names, function(x) list.files(tmt_dir, pattern=x, recursive=TRUE, full.names = TRUE))
  
  # if duplicated files (i.e., sapply in prev. line produces a matrix or list instead of vector)
  # take only the file that has been modified most recently
  # (Why does it return a matrix for TmT format fls but list for stand_dynamics???)
  if(is.matrix(fls)) {
    fls <- apply(fls, 2, function(x) {
      f_mtime <- file.info(x)$mtime
      # f <- x[f_mtime == min(f_mtime)] # old files
      f <- x[f_mtime == max(f_mtime)]
      } )
  }
  if(is.list(fls)) {
    fls <- sapply(fls, function(x) {
      f_mtime <- file.info(x)$mtime
      # f <- x[f_mtime == min(f_mtime)] # old files
      f <- x[f_mtime == max(f_mtime)]
    } )
  }
  
  # Print file info
  fls_w_time <- sapply(fls, function(x) paste(x, "\nlast modified: ", file.info(x)$mtime))
  message("Reading files:\n", paste(fls_w_time, collapse="\n"))
  # message("Reading files:\n", paste(fls, collapse="\n"))
  
  if(!list.files.only) {
    datalist <- lapply(fls, read.csv)
    
    
    # checks
    if(length(datalist) != length(database.code)) {
      stop("Problem with the length of the output datalist, does not match the number of databases")
    }
    
    # Combine to one data frame
    if(combine) {
      common_colnames <- Reduce(intersect, lapply(datalist, colnames))
      datalist <- lapply(datalist, function(x) x[,common_colnames])
      data_df <- do.call(rbind, datalist)
      data_df <- data_df %>% select(!any_of("X"))
      message("Combined files to one data frame")
      return(data_df)
    } else {
      return(datalist)
    }
  } else {
    message("\nFiles not read (to read files set 'list.files.only = FALSE')\n")
  }
  
  
}
