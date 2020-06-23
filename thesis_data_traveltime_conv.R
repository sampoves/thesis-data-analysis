
# Conversion of Helsinki Region Travel Time Matrix 2018 data into fst and then
# fst-format TTM18 to dataset in the resolution of postal code areas

# For the thesis:
# "Parking of private cars and spatial accessibility in Helsinki Capital Region"

# by Sampo Vesanen
# 24.6.2020


# This tool will convert an unchanged Helsinki Region Travel Time Matrix dataset 
# into fst format to a new location on disk, while preserving only columns 
# relevant to the travel mode private car ("from_id", "to_id", "car_r_t", 
# "car_m_t" and "car_sl_t"). The converted dataset will be compressed (required
# disk space is about 400 megabytes) and can't be accessed readily though text 
# editors such as Notepad. This tool is made for Travel Time Matrix 2018.

# As an additional feature the fst format TTM18 will be transformed to a version
# where data is aggregated by postal code areas. This operation requires only
# one megabyte of diskspace, but the calculation takes about 2.5 hours.

# The use of fst will enable the travel time comparison application to 
# recalculate the research area (Helsinki Capital Region) with a great speed,
# making the application much more responsive to an user.

# fst by Mark Klik, learn more at: https://www.fstpackage.org/index.html

# Helsinki Region Travel Time Matrix 2018 dataset by Tenkanen et al. 2018, 
# DOI: 10.13140/RG.2.2.20858.39362 

# Method by Toivonen et al. 2014, 
# http://www.helsinki.fi/science/accessibility/publications/Toivonen_etal_2014_terra.pdf
# PDF language Finnish, an abstract in English is included.



#### Initialise ----------------------------------------------------------------

# Libraries
library(fst)
library(dplyr)
library(data.table)
library(sp)
library(sf)
library(R.utils)
library(tibble)


# Working directory
wd <- "C:/Sampon/Maantiede/Master of the Universe"

# Folder paths
ttm_path <- file.path(wd, "HelsinkiTravelTimeMatrix2018")
fst_orig_fp <- file.path(wd, "TTM18") 
fst_postal_fp <- file.path(wd, "TTM18_postal")
gridzipcodes <- file.path(wd, "grid_for_r.csv")
gridpath <- file.path(wd, "python/MetropAccess_YKR_grid_EurefFIN.shp")



#### Transform unchanged TTM18 data to fst -------------------------------------

TTM18_to_fst <- function(filepath) {
  
  # Generate the fst folder structure and current filename
  splt <- unlist(strsplit(filepath, "/"))
  splt_len <- length(splt)
  filename <- paste(gsub(".txt", "", splt[splt_len]), ".fst", sep = "")
  fst_fp <- file.path(fst_orig_fp, splt[splt_len - 1])
  fullpath <- file.path(fst_fp, filename)
  
  # Conditionally create the current folder to write to. Replicate the folder 
  # structure of the original TTM18 data.
  if (!dir.exists(file.path(fst_fp))) {
    dir.create(file.path(fst_fp))
  }
  
  # data.table::fread() the current file, then write that into fst. Use maximum 
  # compression.
  res <- data.table::fread(filepath, select = c(1, 2, 14, 16, 18))
  fst::write_fst(res, fullpath, compress = 100)
}

# Conditionally create folder name of object "TTM18_foldername"
if (!dir.exists(fst_orig_fp)) {
  dir.create(fst_orig_fp)
}

# Get filepaths of all of the TTM18 data. Remove metadata textfile filepath.
# Value "ttm_path" is taken from the main file "thesis_data_traveltime.R" which 
# sources this script
all_files <- list.files(path = ttm_path, 
                        pattern = ".txt$", 
                        recursive = TRUE, 
                        full.names = TRUE)
all_files <- all_files[-grepl("META", all_files)]

# This runs for about 10 minutes because of the maximum compression.
lapply(all_files, FUN = TTM18_to_fst)



#### Prepare for aggregated TTM18 datafetch ------------------------------------

# Conditionally create folder name of object "TTM18_foldername"
if (!dir.exists(fst_postal_fp)) {
  dir.create(fst_postal_fp)
}

# Get all of the TTM18 fst filepaths
all_fst <- list.files(path = fst_orig_fp,
                      pattern = ".fst$",
                      recursive = TRUE,
                      full.names = TRUE)

# Use "ykr_ids" as the location vector for each YKR_ID. This vector will be
# queried in the reactive fetching of TTM18 data.
ykr_ids <- fst::read_fst(all_fst[1])[, 1]

# This dataframe, produced in Python, helps find the zipcodes for each YKR_ID.
# 99999 is outside of research area.
ykrid_zipcodes <- 
  read.csv(file = gridzipcodes,
           header = TRUE, 
           sep = ",",
           colClasses = c(YKR_ID = "integer", zipcode = "character"),
           stringsAsFactors = TRUE) %>%
  dplyr::select(-X)

# Create named vector for transformation of YKR_ID column to zipcode column
ykrid3 <- setNames(ykrid_zipcodes$zipcode, ykrid_zipcodes$YKR_ID)

# Group YKR_IDs to lists by their postal code areas. Next delete zipcode 99999
# from the list as unneeded.
zips <- unique(ykrid_zipcodes$zipcode)[-2]



#### Get Helsinki walking center YKR_IDs ---------------------------------------

app_crs <- sp::CRS("+init=epsg:3067")

# Read and transform the grid cells
gridi <- rgdal::readOGR(gridpath, stringsAsFactors = TRUE) %>%
  sp::spTransform(., app_crs)

# TTM18 Helsinki walking center polygon. Source: Henrikki Tenkanen. Get YKR_IDs
# which fit inside the walking center polygon.
walkingHki <- 
  data.frame(
    long = c(387678.024778, 387891.53396, 383453.380944, 383239.871737, 387678.024778),
    lat = c(6675360.99039, 6670403.35286, 6670212.21613, 6675169.85373, 6675360.99039)) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = app_crs) %>%
  dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
  sf::st_cast("POLYGON")

walking_ids <- 
  sf::st_intersection(sf::st_as_sf(gridi), walkingHki) %>%
  as(., "Spatial") %>%
  {dplyr::left_join(ggplot2::fortify(.),
                    as.data.frame(.) %>%
                      tibble::rownames_to_column(., var = "id"))} %>%
  dplyr::select(YKR_ID)
walking_ids <- walking_ids[, 1]



#### Generate TTM18 data simplified to postal code area level ------------------

# Reader function
TTM18fst_fetch <- function(x, from, to) {
  fst::read_fst(x, from = from, to = to)
}

# This large lapply() operation takes a long time to run, about 2.5 hours. It
# was not optimised further because this needs to be executed only once.
# invisible() prevents printing of lapply() output.
invisible(
  lapply(seq_along(zips), function(list_id) {
    
    # Track execution time
    starttime <- Sys.time()
    
    # Filepath and filename
    listname <- zips[list_id]
    filename <- paste("from_", listname, ".fst", sep = "")
    fullpath <- file.path(fst_postal_fp, filename)
    
    # Current ids
    thisList <- ykrid_zipcodes[ykrid_zipcodes$zipcode %in% listname, ][["YKR_ID"]]
    
    # Get all positions of ykrids needed right now. Sequences will be used to
    # reduce the amount of file reading needed in TTM18fst_fetch_orig(). The
    # sequences are locations of YKR_IDs for each current zipcode.
    these_pos <- sapply(thisList, function(idx) match(idx, ykr_ids))
    sequences <- R.utils::seqToIntervals(these_pos)
    
    res <- 
      # Fetch the curent zipcode fst data using the dataframe "sequences"
      sapply(1:nrow(sequences), function(seq_id) {
        
        # This fetches all data for the YKR_IDs of the current zipcode
        lapply(all_fst,
               FUN = TTM18fst_fetch,
               sequences[seq_id, 1],
               sequences[seq_id, 2])
      }) %>%
      data.table::rbindlist(., fill = TRUE) %>%
      
      # Generate walk to destination column. This needs the resolution of grid
      # cells.
      dplyr::mutate(ttm_sfp = 0.42,
                    ttm_wtd = case_when(to_id %in% walking_ids ~ 2.5, TRUE ~ 2),
                    zipcode = as.vector(ykrid3[as.character(to_id)])) %>%
      
      # Generate TTM18 means
      group_by(zipcode) %>%
      
      dplyr::summarise(from_zip = listname,
                       ttm_r_avg = mean(car_r_t),
                       ttm_m_avg = mean(car_m_t),
                       ttm_sl_avg = mean(car_sl_t),
                       ttm_sfp = mean(ttm_sfp),
                       ttm_wtd = mean(ttm_wtd)) %>%
      dplyr::mutate_at(vars(ttm_r_avg, ttm_m_avg, ttm_sl_avg, ttm_wtd), ~round(., 2)) %>%
      
      # Remove data for zipcode 99999, out of research area
      dplyr::filter(zipcode != "99999")
    
    # Write file
    fst::write_fst(res, fullpath, compress = 100)
    
    # Print elapsed time for each postal code area
    endtime <- Sys.time()
    timetaken <- endtime - starttime
    print(paste(listname, "took", round(timetaken, 2), "to complete."))
  })
)

# You have reached the end of this script.