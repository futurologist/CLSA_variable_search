library(data.table)
library(dplyr)
library(readxl)

extract_subtable <- function(data_base, list_of_codes){
  headers <- names(data_base)
  pos <- c(1)
  for(code in list_of_codes){
    pos <- c(pos, grep(code, headers))
  } 
  data_base <- data_base[, ..pos]
  data_base <- data_base[order(data_base[, 1]), ]
  return(data_base)
}

################################### YOUR INPUT GOES HERE: ###############################################

# type the path to the table of desired variables:
var_path <- "/home/ndimit2/CLSA/Extraction_script/quest_vars.txt"

# type as strings in a string array, the paths to the four different LDSC datasets: Baseline COM and TRM, Follow Up 1 COF1 and TRF1, in that order:
# if you are on the human pain genetic lab server, just LEAVE THIS AS IS 
list_of_paths_in <- c("/mnt/nfs/backup/data/clsa/raw/190213/190213_McGill_LDiatchenko_Baseline_DRU_JULY2020/190213_McGill_LDiatchenko_CoP4_1_Baseline_DRU.csv", 
                      "/mnt/nfs/backup/data/clsa/raw/190213/190213_McGill_LDiatchenko_Baseline_DRU_JULY2020/190213_McGill_LDiatchenko_Tra3_5_Baseline_DRU.csv",
                      "/mnt/nfs/backup/data/clsa/raw/190213/190213_McGill_LDiatchenko_FUP1_DRU_JULY2020/190213_McGill_LDiatchenko_CoP3_FUP1_DRU.csv",
                      "/mnt/nfs/backup/data/clsa/raw/190213/190213_McGill_LDiatchenko_FUP1_DRU_JULY2020/190213_McGill_LDiatchenko_Tra2_1_FUP1_DRU.csv")

# leave this as is:
names(list_of_paths_in) <- c('COM', 'TRM', 'COF1', 'TRF1')

# type as strings in a string array, the paths you choose fo the four different outputs: 
# Baseline COM and TRM, Follow Up 1 COF1 and TRF1, in that order:
list_of_paths_out <- c("/home/ndimit2/CLSA/Phenotypes/Raw_tables/quest_vars_COM.tsv",
                       "/home/ndimit2/CLSA/Phenotypes/Raw_tables/quest_vars_TRM.tsv",
                       "/home/ndimit2/CLSA/Phenotypes/Raw_tables/quest_vars_COF1.tsv",
                       "/home/ndimit2/CLSA/Phenotypes/Raw_tables/quest_vars_TRF1.tsv")

# leave this as is:
names(list_of_paths_out) <- c('COM', 'TRM', 'COF1', 'TRF1')

#########################################################################################################
#                                  LEAVE THE REST TO THE COMPUTER!
#########################################################################################################

#var_table <- fread(var_path) 
var_table <- read_excel(var_path)

for(set in names(list_of_paths_in)){
   
   print('Reading raw table...')
   print(set)
   dt <- fread(list_of_paths_in[set])
   print('Reading complete!')

   print('Extracting table of desired variables...')
   vars <- var_table[Dataset==set, Name]
   #print(vars)
   dt <- extract_subtable(dt, vars)
   print('Extraction comlete!')
   
   #print('table preview: ')
   #print(head(dt, 7))
   
   print('number of rows (sample size): ')
   print(nrow(dt))
   
   print('Exporting table...')
   write.table(dt, list_of_paths_out[set], append = FALSE, sep = "\t", quote = FALSE, col.names=TRUE, row.names=FALSE)
   print('Export complete!')

}
