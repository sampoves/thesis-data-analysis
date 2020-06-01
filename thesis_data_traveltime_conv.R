
# Conversion of Helsinki Region Travel Time Matrix 2018 data into fst

# For the thesis:
# "Parking of private cars and spatial accessibility in Helsinki Capital Region"

# by Sampo Vesanen
# 1.6.2020


# This tool will convert an unchanged Helsinki Region Travel Time Matrix dataset 
# into fst format to a new location on disk, while preserving only columns 
# relevant to the travel mode private car ("from_id", "to_id", "car_r_t", 
# "car_m_t" and "car_sl_t"). The converted dataset will be compressed and can't 
# be accessed readily though text editors such as Notepad. This tool is made
# for Travel Time Matrix 2018.

# The use of fst will enable the travel time comparison application to 
# recalculate the Helsinki region 250 x 250 meter grid with a great speed,
# making the application much more responsive to an user.

# This code needs to be run only once The filesize of the converted dataset will 
# be about 400 megabytes.

# fst by Mark Klik, learn more at: https://www.fstpackage.org/index.html

# Helsinki Region Travel Time Matrix 2018 dataset by Tenkanen et al. 2018, 
# DOI: 10.13140/RG.2.2.20858.39362 

# Method by Toivonen et al. 2014, 
# http://www.helsinki.fi/science/accessibility/publications/Toivonen_etal_2014_terra.pdf
# PDF language Finnish, an abstract in English is included.



# Libraries
library(fst)



# Use this folder path for the fst files
fst_filepath <- file.path(wd, "TTM18")

TTM18_to_fst <- function(x) {
  
  # Generate the fst folder structure and current filename
  splt <- unlist(strsplit(x, "/"))
  splt_len <- length(splt)
  filename <- paste(gsub(".txt", "", splt[splt_len]), ".fst", sep = "")
  fst_fp <- file.path(fst_filepath, splt[splt_len - 1])
  
  # Conditionally create the current folder to write to. Replicate the folder 
  # structure of the original TTM18 data.
  if (!dir.exists(file.path(fst_fp))) {
    dir.create(file.path(fst_fp))
  }
  
  # data.table::fread the current file, then write that into fst. Use maximum 
  # compression.
  res <- data.table::fread(x, select = c(1, 2, 14, 16, 18))
  fst::write_fst(res, file.path(fst_fp, filename), compress = 100)
}

# Conditionally create folder name of object "TTM18_foldername"
if (!dir.exists(fst_filepath)) {
  dir.create(fst_filepath)
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


# You have reached the end of this script.