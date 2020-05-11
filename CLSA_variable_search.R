### ALL VARIABLES of CLSA, can be downloaded from the website
all_vars <- fread("path_to\\CLSA\\CLSA_vars\\Variables.csv")

### VARIABLES of available in our CLSA datasets, a table with 2 columns:
### Name  |  Dataset
our_var <- fread("path_to\\CLSA\\CLSA_vars\\Our_CLSA_variables.txt")

###################################### FUNCTIONS: ########################################

######################## In case one has the full datasets uploaded in memory ############

get_our_vars <- function(tbl_list, dataset_list){
  our_vars <- data.table(Name = names(tbl_list[[1]]), Dataset = rep(dataset_list[[1]], length(names(tbl_list[[1]]))))
  i = 1
  for(tbl in tbl_list[c(2:length(tbl_list))]){
    i <- i+1 
    hd <- data.table(Name = names(tbl), Dataset = rep(dataset_list[[i]], length(names(tbl))))
    our_vars <- union(our_vars, hd)
  }
  return(our_vars)
}

###########################################################################################

extract_vars <- function(tbl_all_vars, list_of_abbriv, mode_list, column){
  if(mode_list == 'and'){
    bool <- rep(TRUE, nrow(tbl_all_vars))
    for(lbl in list_of_abbriv){
      bool <- grepl(lbl, pull(tbl_all_vars, column), ignore.case = TRUE) & bool
    }
  } else if(mode_list == 'or'){
    bool <- rep(FALSE, nrow(tbl_all_vars))
    for(lbl in list_of_abbriv){
      bool <- grepl(lbl, pull(tbl_all_vars, column), ignore.case = TRUE) | bool
    }
  }
  return(filter(tbl_all_vars, bool))
}

detect_our_vars <- function(our_vars, tbl_all_vars, list_of_abbriv,  mode_list, column){
  tbl_spec_vars <- extract_vars(tbl_all_vars, list_of_abbriv, mode_list, column)
  tbl_spec_vars <- left_join(tbl_spec_vars, mutate(our_vars, our_var = 1), by=c("Name", "Dataset"))
  return(tbl_spec_vars)
}

detect_our_variables <- function(our_vars, tbl_all_vars, include_col1, include_col2, mode_columns, list1, mode_list1, list2, mode_list2){
  if(include_col1 & include_col2){
    tbl_spec_vars1 <- detect_our_vars(our_vars, tbl_all_vars, list1, mode_list1, 1)
    tbl_spec_vars2 <- detect_our_vars(our_vars, tbl_all_vars, list2, mode_list2, 2)
    if(mode_columns == 'and'){return(intersect(tbl_spec_vars1, tbl_spec_vars2))}
    if(mode_columns == 'or'){ return(union(tbl_spec_vars1, tbl_spec_vars2))}
  }
  if(include_col1 & !include_col2){
    tbl_spec_vars1 <- detect_our_vars(our_vars, tbl_all_vars, list1,  mode_list1, 1)
    return( tbl_spec_vars1)
  }
  if(!include_col1 & include_col2){
    tbl_spec_vars2 <- detect_our_vars(our_vars, tbl_all_vars, list2,  mode_list2, 2)
    return( tbl_spec_vars2)
  }
}

##############################################################################################

our_vars <- get_our_vars(tbl_list, dataset_list)

### Option to search in both columns Name and Label. 
### If you want to search for expressions or words in both columns Name and Label, set
### indlude_col1 = TRUE, inlude_col2 = TRUE
vars_specified <- detect_our_variables(our_vars, all_vars, 
                                       include_col1 = TRUE, include_col2 = TRUE, mode_columns = 'and', 
                                       c("NUR"), mode_list1 = 'and', 
                                       c("nutrition", "risk"), mode_list2 = 'and')

