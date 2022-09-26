
CGIAR <- readr::read_csv(here("processed", "agimpacts_full.csv"))

# clean data --------------------------------------------------------------

# replace -9999 and -9998 values with NA

agimpacts <- CGIAR %>% 
  dplyr::na_if(-9999)

agimpacts <- agimpacts %>% 
  dplyr::na_if(-9998)

na_strings <- c("NA", "N/A")

install.packages("naniar")
require(naniar)

agimpacts <- agimpacts %>% 
  naniar::replace_with_na_all(condition = ~.x %in% na_strings)


# Load Agimpacts Data validation workbook -------------------------------

xl_validation_book <- here("data", "Agimpacts Data Validation.xlsx")

data_validation_sheet <- read_excel(path = xl_validation_book, sheet = "Data")

data_validation_sheet <- data_validation_sheet %>% dplyr::na_if("NA")

str(data_validation_sheet) # 350 rows


# Case 1: General ------------------------------------------------------------

# call data to run function on

gen_list <- list()

agimpacts_g <- agimpacts # re-run this after running the loop each time, as agimpacts_g gets superassigned in global env


# Define set of rows to apply imputation function to
rows_case_general <- data_validation_sheet %>% 
  filter(Case == "General")

str(rows_case_general) # 264 rows

# Function

data_impute_general_overwrite <- function(rows_case_general){
  
  for (i in 1:dim(rows_case_general)[1]) {
    
    
    vdf_al <- rows_case_general[i,]
    
    x <- expr(vdf_al$`1 Index column`)
    first <- as.symbol(eval(x))
    
    y <- expr(vdf_al$`2 Index column`)
    second <- as.symbol(eval(y))
    
    z <- expr(vdf_al$`3 Index column`) 
    third <- as.symbol(eval(z))
    
    # extract edit column values as variable expressions
    
    edit_one <- rlang::parse_exprs(vdf_al$`Edit column(s)`)[[1]]
    
    edit_two <-  if(
      count.fields(textConnection(vdf_al$`Edit column(s)`), sep = "; ") == 1)  NA else rlang::parse_exprs(vdf_al$`Edit column(s)`)[[2]]
    
    edit_three <- if(
      count.fields(textConnection(vdf_al$`Edit column(s)`), sep = "; ") == 1) NA else if(
        count.fields(textConnection(vdf_al$`Edit column(s)`), sep = "; ") == 2) NA else rlang::parse_exprs(vdf_al$`Edit column(s)`)[[3]]
    
    edit_four <- if(
      count.fields(textConnection(vdf_al$`Edit column(s)`), sep = "; ") == 1) NA else if(
        count.fields(textConnection(vdf_al$`Edit column(s)`), sep = "; ") == 2) NA else if(
          count.fields(textConnection(vdf_al$`Edit column(s)`), sep = "; ") == 3) NA else rlang::parse_exprs(vdf_al$`Edit column(s)`)[[4]]
    
    edit_five <- if(
      count.fields(textConnection(vdf_al$`Edit column(s)`), sep = "; ") == 1) NA else if(
        count.fields(textConnection(vdf_al$`Edit column(s)`), sep = "; ") == 2) NA else if(
          count.fields(textConnection(vdf_al$`Edit column(s)`), sep = "; ") == 3) NA else if(
            count.fields(textConnection(vdf_al$`Edit column(s)`), sep = "; ") == 4) NA else rlang::parse_exprs(vdf_al$`Edit column(s)`)[[5]]
    
    
    edit_six <- if(
      count.fields(textConnection(vdf_al$`Edit column(s)`), sep = "; ") == 1) NA else if(
        count.fields(textConnection(vdf_al$`Edit column(s)`), sep = "; ") == 2) NA else if(
          count.fields(textConnection(vdf_al$`Edit column(s)`), sep = "; ") == 3) NA else if(
            count.fields(textConnection(vdf_al$`Edit column(s)`), sep = "; ") == 4) NA else if(
              count.fields(textConnection(vdf_al$`Edit column(s)`), sep = "; ") == 5) NA else rlang::parse_exprs(vdf_al$`Edit column(s)`)[[6]]
    
    
    g <- agimpacts_g %>% 
      filter(
        if(
          !is.na(vdf_al$`1 Index column`) & !is.na(vdf_al$`2 Index column`) & !is.na(vdf_al$`3 Index column`)) 
          Reference == vdf_al$Reference & eval(first) == vdf_al$`1 Match value` & eval(second) == vdf_al$`2 Match value` & eval(third) == vdf_al$`3 Match value` # filter condition
        else if(
          !is.na(vdf_al$`1 Index column`) & !is.na(vdf_al$`2 Index column`))
          Reference == vdf_al$Reference & eval(first) == vdf_al$`1 Match value` & eval(second) == vdf_al$`2 Match value` # filter condition
        else if(
          !is.na(vdf_al$`1 Index column`))
          Reference == vdf_al$Reference & eval(first) == vdf_al$`1 Match value` # filter condition
        else if(
          is.na(vdf_al$`1 Index column`) & is.na(vdf_al$`2 Index column`) & is.na(vdf_al$`3 Index column`))
          Reference == vdf_al$Reference 
      ) %>% 
      cbind(if(!is.na(edit_one)) impute_1 = vdf_al[[edit_one]] else NA, 
            if(!is.na(edit_two)) impute_2 = vdf_al[[edit_two]] else NA, 
            if(!is.na(edit_three)) impute_3 = vdf_al[[edit_three]] else NA, 
            if(!is.na(edit_four)) impute_4 = vdf_al[[edit_four]] else NA,
            if(!is.na(edit_five)) impute_5 = vdf_al[[edit_five]] else NA,
            if(!is.na(edit_six)) impute_6 = vdf_al[[edit_six]] else NA) %>% 
      rename(impute_1 = "if (!is.na(edit_one)) impute_1 = vdf_al[[edit_one]] else NA",
             impute_2 = "if (!is.na(edit_two)) impute_2 = vdf_al[[edit_two]] else NA",
             impute_3 = "if (!is.na(edit_three)) impute_3 = vdf_al[[edit_three]] else NA",
             impute_4 = "if (!is.na(edit_four)) impute_4 = vdf_al[[edit_four]] else NA",
             impute_5 = "if (!is.na(edit_five)) impute_5 = vdf_al[[edit_five]] else NA",
             impute_6 = "if (!is.na(edit_six)) impute_6 = vdf_al[[edit_six]] else NA")
    
    if(!is.na(edit_one)) g[[edit_one]] <- g$impute_1
    if(!is.na(edit_two)) g[[edit_two]] <- g$impute_2
    if(!is.na(edit_three)) g[[edit_three]] <- g$impute_3 
    if(!is.na(edit_four)) g[[edit_four]] <- g$impute_4
    if(!is.na(edit_five)) g[[edit_five]] <- g$impute_5
    if(!is.na(edit_six)) g[[edit_six]] <- g$impute_6
    
    g <- g %>% dplyr::select(!c(impute_1, impute_2, impute_3, impute_4, impute_5, impute_6))
    
    # print(g)
    
    # print(dim(agimpacts_f[agimpacts_f$X %in% g$X, , drop = FALSE])) # THIS WORKS! https://rstudio-education.github.io/hopr/modify.html
    
    # reset column types in g (they are all character from rows_in_run) to match column types in agimpacts, by column name - actually just an issue for Temp.Change and CO2.Projected
    g$Temp.Change <- as.numeric(g$Temp.Change)
    g$CO2.Projected <- as.numeric(g$CO2.Projected)
    
    
    # overwrite row in agimpacts by X, superassign to agimpacts_e in global environment in order to handle sequential/conditional/cumulative edits
    
    agimpacts_g[agimpacts_g$X %in% g$X, , drop = FALSE] <<- g # SUPER ASSIGNMENT TO GLOBAL ENVIRONMENT  https://adv-r.hadley.nz/environments.html?q=global%20#super-assignment--
    
    # cache agimpacts table by write.csv and overwrite each iteration
    
    # save/create g as a list of tables by name of study or row number in rows_to_run [i] for future checks, append new table to the list iteratively by i inside the loop
    
    g$i <- i  # keep track of which iteration produced table
    gen_list[[i]] <<- data.frame(g) # add it to list
    
    
    # print(dim(g))
    
  }
  
}

# Run

data_impute_general_overwrite(rows_case_general)

agimpacts_g %>% write.csv("agimpacts_g.csv")

imputed_data_check_general = do.call(rbind, gen_list) %>% relocate(i) %>% write.csv("Imputed_data_check_general.csv")
imputed_data_check_general <-  do.call(rbind, gen_list) # should be same as rows_to_run, 170 rows.  4586 unique rows have been corrected.

dim(imputed_data_check_general) # 5197 rows incl. multiple procedures

imputed_data_check_general %>% 
  group_by(X) %>% 
  summarise(n = n()) %>% 
  mutate(multiple = ifelse(n > 1, "Yes", "No")) %>% 
  group_by(multiple) %>% 
  tally() %>% 
  adorn_totals("row") # 4586 unique rows: 581 with multiple correction procedures, 4005 with single correction procedures applied

# Case 2: Backslash ---------------------------------------------------------


# Define set of rows to apply imputation function to 
rows_case_backslash <- data_validation_sheet %>% 
  filter(Case == "Backslash")

str(rows_case_backslash) # 2 rows

# Function

# DO THIS - we can either modify the string containing \n in agimpacts_NA, i.e. remove all backslashes in main dataset and data validation dataset, or
# we can find a way to write in \n in the Excel reference such that R reads this as such and not as \\n
# or enter a new line in the reference? - note this doesn't work, R reads as \r\n

# data_validation_sheet %>% filter(Reference == "Karim et al., 1996\\n") %>% dplyr::select(Reference) %>% glimpse()

# agimpacts_NA %>% filter(Reference == "Karim et al., 1996\n")

# agimpacts_NA %>% filter(Reference == "Brassard (2008)") %>% dplyr::select(Comments)

agimpacts_g[agimpacts_g$Reference == "Karim et al., 1996\n", "Reference"] <- "Karim et al., 1996" # recycle
agimpacts_g[agimpacts_g$Reference == "Brassard (2008)" & agimpacts_g$Comments == "Without\n CO2", "Comments"] <- "Without CO2"

# agimpacts_g %>% filter(Reference == "Karim et al., 1996")
# agimpacts_g %>% filter(Reference == "Brassard (2008)" & Comments == "Without CO2")
# data_validation_sheet %>% filter(Reference == "Brassard (2008)" & `1 Match value` == "Without CO2") # %>% dplyr::select(`1 Match value`) %>% glimpse()

# Run 

gen_list <- list() # empties gen_list just to track number of corrections made, but agimpacts_g should still have corrections from general function run

data_impute_general_overwrite(rows_case_backslash)

agimpacts_g %>% write.csv("agimpacts_g_backslash.csv")

imputed_data_check_general_backslash = do.call(rbind, gen_list) %>% relocate(i) %>% write.csv("Imputed_data_check_general_backslash.csv")
imputed_data_check_general_backslash <-  do.call(rbind, gen_list) # 

dim(imputed_data_check_general_backslash) # 344 rows incl. multiple procedures

imputed_data_check_general_backslash %>% 
  group_by(X) %>% 
  summarise(n = n()) %>% 
  mutate(multiple = ifelse(n > 1, "Yes", "No")) %>% 
  group_by(multiple) %>% 
  tally() %>% 
  adorn_totals("row") # 344 unique rows, all with single corrections applied, which makes sense

# Case 3: Contains NA -------------------------------------------------------

# Define set of rows to apply imputation function to
rows_case_NA <- data_validation_sheet %>% 
  filter(Case == "Contains NA")

str(rows_case_NA) # 11 rows

# Function

# for these 11 rows - the NA always appears in the 1 Match value column; there are only up to 2 Edit column values; and only one Index column

data_impute_NA_overwrite <- function(rows_case_NA){
  
  for (i in 1:dim(rows_case_NA)[1]) {
    
    
    vdf_al <- rows_case_NA[i,]
    
    x <- expr(vdf_al$`1 Index column`)
    first <- as.symbol(eval(x))
    
    
    # extract edit column values as variable expressions ---------------------------------------
    
    edit_one <- rlang::parse_exprs(vdf_al$`Edit column(s)`)[[1]]
    
    edit_two <-  if(
      count.fields(textConnection(vdf_al$`Edit column(s)`), sep = "; ") == 1)  NA else rlang::parse_exprs(vdf_al$`Edit column(s)`)[[2]]
    
    
    n <- agimpacts_NA %>% 
      filter(Reference == vdf_al$Reference & is.na(eval(first))) %>% # isNA filter condition
      cbind(if(!is.na(edit_one)) impute_1 = vdf_al[[edit_one]] else NA, 
            if(!is.na(edit_two)) impute_2 = vdf_al[[edit_two]] else NA) %>% 
      rename(impute_1 = "if (!is.na(edit_one)) impute_1 = vdf_al[[edit_one]] else NA",
             impute_2 = "if (!is.na(edit_two)) impute_2 = vdf_al[[edit_two]] else NA")
    
    if(!is.na(edit_one)) n[[edit_one]] <- n$impute_1
    if(!is.na(edit_two)) n[[edit_two]] <- n$impute_2
    
    n <- n %>% dplyr::select(!c(impute_1, impute_2))
    
    # print(n)
    
    
    # reset column types in g (they are all character from rows_in_run) to match column types in agimpacts, by column name - actually just an issue for Temp.Change and CO2.Projected
    n$Temp.Change <- as.numeric(n$Temp.Change)
    n$CO2.Projected <- as.numeric(n$CO2.Projected)
    
    # overwrite row in agimpacts by X, superassign to agimpacts_e in global environment in order to handle sequential/conditional/cumulative edits
    
    agimpacts_NA[agimpacts_NA$X %in% n$X, , drop = FALSE] <<- n # SUPER ASSIGNMENT TO GLOBAL ENVIRONMENT
    
    
    # save/create as a list of tables by name of study or row number in rows_to_run [i] for future checks, append new table to the list iteratively by i inside the loop
    
    n$i <- i  # keep track of which iteration produced table
    NA_list[[i]] <<- data.frame(n) # add it to list
    
    
    # print(dim(n))
    
  }
  
}

# Run

NA_list <- list()

agimpacts_NA <- agimpacts_g # re-run this after running the loop each time, as agimpacts_g gets superassigned in global env
# since this is assigned from agimpacts_g note that this accumulates changes made from the general case function, therefore always run on most recent version of agimpacts_g

data_impute_NA_overwrite(rows_case_NA)

agimpacts_NA %>% write.csv("agimpacts_NA.csv")

imputed_data_check_NA = do.call(rbind, NA_list) %>% relocate(i) %>% write.csv("Imputed_data_check_NA.csv")
imputed_data_check_NA <-  do.call(rbind, NA_list) 

dim(imputed_data_check_NA) # 390 rows incl. multiple procedures

imputed_data_check_NA %>% 
  group_by(X) %>% 
  summarise(n = n()) %>% 
  mutate(multiple = ifelse(n > 1, "Yes", "No")) %>% 
  group_by(multiple) %>% 
  tally() %>% 
  adorn_totals("row") # 346 unique rows: 44 with multiple correction procedures, 302 with single correction procedures applied

# now agimpacts_NA is the most recent version containing corrections from running functions for the General case, backslash case and match value contains NA case  

# Case 4: Not NA -------------------------------------------------------

rows_case_notNA <- data_validation_sheet %>% 
  filter(Case == "Not NA")

dim(rows_case_notNA) # 2 rows

# can copy case NA function but tweak slightly

data_impute_notNA_overwrite <- function(rows_case_notNA){
  
  for (i in 1:dim(rows_case_notNA)[1]) {
    
    
    vdf_al <- rows_case_notNA[i,]
    
    x <- expr(vdf_al$`1 Index column`)
    first <- as.symbol(eval(x))
    
    
    # extract edit column values as variable expressions ---------------------------------------
    
    edit_one <- rlang::parse_exprs(vdf_al$`Edit column(s)`)[[1]]
    
    edit_two <-  if(
      count.fields(textConnection(vdf_al$`Edit column(s)`), sep = "; ") == 1)  NA else rlang::parse_exprs(vdf_al$`Edit column(s)`)[[2]]
    
    
    n <- agimpacts_NA %>% 
      filter(Reference == vdf_al$Reference & !is.na(eval(first))) %>% # is NOT NA filter condition
      cbind(if(!is.na(edit_one)) impute_1 = vdf_al[[edit_one]] else NA, 
            if(!is.na(edit_two)) impute_2 = vdf_al[[edit_two]] else NA) %>% 
      rename(impute_1 = "if (!is.na(edit_one)) impute_1 = vdf_al[[edit_one]] else NA",
             impute_2 = "if (!is.na(edit_two)) impute_2 = vdf_al[[edit_two]] else NA")
    
    if(!is.na(edit_one)) n[[edit_one]] <- n$impute_1
    if(!is.na(edit_two)) n[[edit_two]] <- n$impute_2
    
    n <- n %>% dplyr::select(!c(impute_1, impute_2))
    
    # print(n)
    
    
    # reset column types in g (they are all character from rows_in_run) to match column types in agimpacts, by column name - actually just an issue for Temp.Change and CO2.Projected
    n$Temp.Change <- as.numeric(n$Temp.Change)
    n$CO2.Projected <- as.numeric(n$CO2.Projected)
    
    # overwrite row in agimpacts by X, superassign to agimpacts_e in global environment in order to handle sequential/conditional/cumulative edits
    
    agimpacts_NA[agimpacts_NA$X %in% n$X, , drop = FALSE] <<- n # SUPER ASSIGNMENT TO GLOBAL ENVIRONMENT
    
    
    # save/create as a list of tables by name of study or row number in rows_to_run [i] for future checks, append new table to the list iteratively by i inside the loop
    
    n$i <- i  # keep track of which iteration produced table
    NA_list[[i]] <<- data.frame(n) # add it to list
    
    
    # print(dim(n))
    
  }
  
}

# Run

NA_list <- list() # empties NA_list but not agimpacts_NA

# always run on most recent version of agimpacts_NA

data_impute_notNA_overwrite(rows_case_notNA)

agimpacts_NA %>% write.csv("agimpacts_notNA.csv")

imputed_data_check_notNA = do.call(rbind, NA_list) %>% relocate(i) %>% write.csv("Imputed_data_check_notNA.csv")
imputed_data_check_notNA <-  do.call(rbind, NA_list) 

dim(imputed_data_check_notNA) # 40 rows

imputed_data_check_notNA %>% 
  group_by(X) %>% 
  summarise(n = n()) %>% 
  mutate(multiple = ifelse(n > 1, "Yes", "No")) %>% 
  group_by(multiple) %>% 
  tally() %>% 
  adorn_totals("row") # 40 rows, all single procedures

# agimpacts_NA is still the most recent version, with 4 case functions applied so far

# Cases 5-10: Row order -------------------------------------------------------

# Define set of rows to apply imputation function to
# Karim and Lin Erda
rows_case_order <- data_validation_sheet %>% 
  filter(Case == "Row order")

dim(rows_case_order) # 15

# Set of functions

# only one Edit column and one Index column
# split up six functions by the index of rows, since in agimpacts_NA, will have to call row to edit by index 
# in rows_case_order, and iteratively (order of running functions matters, in particular Function 3 only works if Function 1 has already been run)

# Function 1: 4 rows call every First row alternating across two rows i.e. 1st in every 2 rows already filtered by 1 index column condition (Function 1 applies to rows 1, 10, 12, 14)
# Function 2: 1 row calls every Second row alternating across two rows i.e. 2nd in every 2 rows already filtered by 1 index column condition (Function 2 applies to rows 2)
# Function 3: after running Function 1 on Lin Erda (correcting crop name), Match value on Crop name point estimates will have reduced by half, therefore can run rows 11, 13, 15 on regular match value 
# Function 4: 2 rows call every First row alternating across three rows i.e. 1st in every 3 rows already filtered by 1 index column condition (Function 3 applies to  rows 3, 6)
# Function 5: 2 rows call every Second row alternating across three rows i.e. 2nd in every 3 rows already filtered by 1 index column condition (Function 4 applies to rows 4, 7)
# Function 6: 2 rows call every Third row alternating across three rows i.e. 3rd in every 3 rows already filtered by 1 index column condition (Function 5 applies to rows 5, 8)


# Row order function 1 ----------------------------------------------------

# subset rows_case_roworder by the rows to which function 1 (filter for 1st in every 2 rows already filtered by 1 index column match value) should apply
rows_case_roworderf1 <- rows_case_order[c(1,10,12,14),]

# function 1 needs to select first of every two rows to edit  

data_impute_roworderf1_overwrite <- function(rows_case_roworderf1){
  
  for (i in 1:dim(rows_case_roworderf1)[1]) {
    
    
    vdf_al <- rows_case_roworderf1[i,]
    
    x <- expr(vdf_al$`1 Index column`)
    first <- as.symbol(eval(x))
    
    
    # extract edit column values as variable expressions
    
    edit_one <- rlang::parse_exprs(vdf_al$`Edit column(s)`)[[1]]
    
    
    n <- agimpacts_NA %>% 
      filter(Reference == vdf_al$Reference & eval(first) == vdf_al$`1 Match value`) 
    
    odd <- seq(1, dim(n)[1], 2)
    
    n <- n[odd,] %>% # and select first of every two rows to edit using seq()
      cbind(if(!is.na(edit_one)) impute_1 = vdf_al[[edit_one]] else NA) %>% 
      rename(impute_1 = "if (!is.na(edit_one)) impute_1 = vdf_al[[edit_one]] else NA")
    
    if(!is.na(edit_one)) n[[edit_one]] <- n$impute_1
    
    n <- n %>% dplyr::select(!c(impute_1))
    
    print(n)
    
    
    # reset column types in g (they are all character from rows_in_run) to match column types in agimpacts, by column name - actually just an issue for Temp.Change and CO2.Projected
    n$Temp.Change <- as.numeric(n$Temp.Change)
    n$CO2.Projected <- as.numeric(n$CO2.Projected)
    
    # overwrite row in agimpacts by X, superassign to agimpacts_e in global environment in order to handle sequential/conditional/cumulative edits
    
    agimpacts_NA[agimpacts_NA$X %in% n$X, , drop = FALSE] <<- n # SUPER ASSIGNMENT TO GLOBAL ENVIRONMENT
    
    
    # save/create as a list of tables by name of study or row number in rows_to_run [i] for future checks, append new table to the list iteratively by i inside the loop
    
    n$i <- i  # keep track of which iteration produced table
    NA_list[[i]] <<- data.frame(n) # add it to list
    
    
    # print(dim(n))
    
  }
  
}


# Run row order function 1 

NA_list <- list() # empties NA_list but not agimpacts_NA

# always run on most recent version of agimpacts_NA

data_impute_roworderf1_overwrite(rows_case_roworderf1)

agimpacts_NA %>% write.csv("agimpacts_roworderf1.csv")

imputed_data_check_roworderf1 = do.call(rbind, NA_list) %>% relocate(i) %>% write.csv("Imputed_data_check_roworderf1.csv")
imputed_data_check_roworderf1 <-  do.call(rbind, NA_list) 

dim(imputed_data_check_roworderf1) # 62 rows

imputed_data_check_roworderf1 %>% 
  group_by(X) %>% 
  summarise(n = n()) %>% 
  mutate(multiple = ifelse(n > 1, "Yes", "No")) %>% 
  group_by(multiple) %>% 
  tally() %>% 
  adorn_totals("row") # 62 rows, all single procedures


# Row order function 2 --------------------------------------------------

# subset rows_case_roworder by the rows to which function 2 (filter for 2nd in every 2 rows already filtered by 1 index column match value) should apply

rows_case_roworderf2 <- rows_case_order[c(2),] # only applies to second row in rows_case_order, Reference == Karim, Case == Row order

# function 2 needs to select second of every two rows to edit  

data_impute_roworderf2_overwrite <- function(rows_case_roworderf2){
  
  for (i in 1:dim(rows_case_roworderf2)[1]) {
    
    
    vdf_al <- rows_case_roworderf2[i,]
    
    x <- expr(vdf_al$`1 Index column`)
    first <- as.symbol(eval(x))
    
    
    # extract edit column values as variable expressions
    
    edit_one <- rlang::parse_exprs(vdf_al$`Edit column(s)`)[[1]]
    
    
    n <- agimpacts_NA %>% 
      filter(Reference == vdf_al$Reference & eval(first) == vdf_al$`1 Match value`) 
    
    even <- seq(2, dim(n)[1], 2)
    
    n <- n[even,]  %>% # select second of every two rows to edit using seq()
      cbind(if(!is.na(edit_one)) impute_1 = vdf_al[[edit_one]] else NA) %>% 
      rename(impute_1 = "if (!is.na(edit_one)) impute_1 = vdf_al[[edit_one]] else NA")
    
    if(!is.na(edit_one)) n[[edit_one]] <- n$impute_1
    
    n <- n %>% dplyr::select(!c(impute_1))
    
    # print(n)
    
    
    # reset column types in g (they are all character from rows_in_run) to match column types in agimpacts, by column name - actually just an issue for Temp.Change and CO2.Projected
    n$Temp.Change <- as.numeric(n$Temp.Change)
    n$CO2.Projected <- as.numeric(n$CO2.Projected)
    
    # overwrite row in agimpacts by X, superassign to agimpacts_e in global environment in order to handle sequential/conditional/cumulative edits
    
    agimpacts_NA[agimpacts_NA$X %in% n$X, , drop = FALSE] <<- n # SUPER ASSIGNMENT TO GLOBAL ENVIRONMENT
    
    
    # save/create as a list of tables by name of study or row number in rows_to_run [i] for future checks, append new table to the list iteratively by i inside the loop
    
    n$i <- i  # keep track of which iteration produced table
    NA_list[[i]] <<- data.frame(n) # add it to list
    
    
    # print(dim(n))
    
  }
  
}


# Run row order function 2 


NA_list <- list() # empties NA_list but not agimpacts_NA

# always run on most recent version of agimpacts_NA

data_impute_roworderf2_overwrite(rows_case_roworderf2)

agimpacts_NA %>% write.csv("agimpacts_roworderf2.csv")

imputed_data_check_roworderf2 = do.call(rbind, NA_list) %>% relocate(i) %>% write.csv("Imputed_data_check_roworderf2.csv")
imputed_data_check_roworderf2 <-  do.call(rbind, NA_list) 

dim(imputed_data_check_roworderf2) # 26 rows

imputed_data_check_roworderf2 %>% 
  group_by(X) %>% 
  summarise(n = n()) %>% 
  mutate(multiple = ifelse(n > 1, "Yes", "No")) %>% 
  group_by(multiple) %>% 
  tally() %>% 
  adorn_totals("row") # 26 rows, all single procedures


# Row order function 3 --------------------------------------------------

# subset rows_case_roworder by the rows to which function 2 (filter for 2nd in every 2 rows already filtered by 1 index column match value) should apply

rows_case_roworderf3 <- rows_case_order[c(11, 13, 15),]

# deals with Lin Erda rows_case_order 11, 13, 15, given Function 1 has already corrected Crop names in even rows to Crop (Rainfed), the remaining rows with Crop = Match Value should be corrected via general function
# however general function needs to take ONLY 1 index column match value

data_impute_roworderf3_overwrite <- function(rows_case_roworderf3){
  
  for (i in 1:dim(rows_case_roworderf3)[1]) {
    
    
    vdf_al <- rows_case_roworderf3[i,]
    
    x <- expr(vdf_al$`1 Index column`)
    first <- as.symbol(eval(x))
    
    
    # extract edit column values as variable expressions
    
    edit_one <- rlang::parse_exprs(vdf_al$`Edit column(s)`)[[1]]
    
    
    n <- agimpacts_NA %>% 
      filter(Reference == vdf_al$Reference & eval(first) == vdf_al$`1 Match value`) %>% 
      cbind(if(!is.na(edit_one)) impute_1 = vdf_al[[edit_one]] else NA) %>% 
      rename(impute_1 = "if (!is.na(edit_one)) impute_1 = vdf_al[[edit_one]] else NA")
    
    if(!is.na(edit_one)) n[[edit_one]] <- n$impute_1
    
    n <- n %>% dplyr::select(!c(impute_1))
    
    # print(n)
    
    
    # reset column types in g (they are all character from rows_in_run) to match column types in agimpacts, by column name - actually just an issue for Temp.Change and CO2.Projected
    n$Temp.Change <- as.numeric(n$Temp.Change)
    n$CO2.Projected <- as.numeric(n$CO2.Projected)
    
    # overwrite row in agimpacts by X, superassign to agimpacts_e in global environment in order to handle sequential/conditional/cumulative edits
    
    agimpacts_NA[agimpacts_NA$X %in% n$X, , drop = FALSE] <<- n # SUPER ASSIGNMENT TO GLOBAL ENVIRONMENT
    
    
    # save/create as a list of tables by name of study or row number in rows_to_run [i] for future checks, append new table to the list iteratively by i inside the loop
    
    n$i <- i  # keep track of which iteration produced table
    NA_list[[i]] <<- data.frame(n) # add it to list
    
    
    # print(dim(n))
    
  }
  
}


# Run row order function 3


NA_list <- list() # empties NA_list but not agimpacts_NA

# always run on most recent version of agimpacts_NA

data_impute_roworderf3_overwrite(rows_case_roworderf3)

agimpacts_NA %>% write.csv("agimpacts_roworderf3.csv")

imputed_data_check_roworderf3 = do.call(rbind, NA_list) %>% relocate(i) %>% write.csv("Imputed_data_check_roworderf3.csv")
imputed_data_check_roworderf3 <-  do.call(rbind, NA_list) 

dim(imputed_data_check_roworderf3) # 36 rows

imputed_data_check_roworderf3 %>% 
  group_by(X) %>% 
  summarise(n = n()) %>% 
  mutate(multiple = ifelse(n > 1, "Yes", "No")) %>% 
  group_by(multiple) %>% 
  tally() %>% 
  adorn_totals("row") # 26 rows, all single procedures




# Row order function 4 --------------------------------------------------

# subset rows_case_roworder by the rows to which function 4 (filter for 1stnd in every 3 rows already filtered by 1 index column match value) should apply

rows_case_roworderf4 <- rows_case_order[c(3, 6),] # only applies to rows 3 and 6

# function 2 needs to select second of every two rows to edit  

data_impute_roworderf4_overwrite <- function(rows_case_roworderf4){
  
  for (i in 1:dim(rows_case_roworderf4)[1]) {
    
    
    vdf_al <- rows_case_roworderf4[i,]
    
    x <- expr(vdf_al$`1 Index column`)
    first <- as.symbol(eval(x))
    
    
    # extract edit column values as variable expressions
    
    edit_one <- rlang::parse_exprs(vdf_al$`Edit column(s)`)[[1]]
    
    
    n <- agimpacts_NA %>% 
      filter(Reference == vdf_al$Reference & eval(first) == vdf_al$`1 Match value`) 
    
    first <- seq(1, dim(n)[1], 3) # select first of every three rows to edit using seq()
    
    n <- n[first,]  %>% 
      cbind(if(!is.na(edit_one)) impute_1 = vdf_al[[edit_one]] else NA) %>% 
      rename(impute_1 = "if (!is.na(edit_one)) impute_1 = vdf_al[[edit_one]] else NA")
    
    if(!is.na(edit_one)) n[[edit_one]] <- n$impute_1
    
    n <- n %>% dplyr::select(!c(impute_1))
    
    # print(n)
    
    
    # reset column types in g (they are all character from rows_in_run) to match column types in agimpacts, by column name - actually just an issue for Temp.Change and CO2.Projected
    n$Temp.Change <- as.numeric(n$Temp.Change)
    n$CO2.Projected <- as.numeric(n$CO2.Projected)
    
    # overwrite row in agimpacts by X, superassign to agimpacts_e in global environment in order to handle sequential/conditional/cumulative edits
    
    agimpacts_NA[agimpacts_NA$X %in% n$X, , drop = FALSE] <<- n # SUPER ASSIGNMENT TO GLOBAL ENVIRONMENT
    
    
    # save/create as a list of tables by name of study or row number in rows_to_run [i] for future checks, append new table to the list iteratively by i inside the loop
    
    n$i <- i  # keep track of which iteration produced table
    NA_list[[i]] <<- data.frame(n) # add it to list
    
    
    # print(dim(n))
    
  }
  
}


# Run row order function 4


NA_list <- list() # empties NA_list but not agimpacts_NA

# always run on most recent version of agimpacts_NA

data_impute_roworderf4_overwrite(rows_case_roworderf4)

agimpacts_NA %>% write.csv("agimpacts_roworderf4.csv")

imputed_data_check_roworderf4 = do.call(rbind, NA_list) %>% relocate(i) %>% write.csv("Imputed_data_check_roworderf4.csv")
imputed_data_check_roworderf4 <-  do.call(rbind, NA_list) 

dim(imputed_data_check_roworderf4) # 52 rows

imputed_data_check_roworderf4 %>% 
  group_by(X) %>% 
  summarise(n = n()) %>% 
  mutate(multiple = ifelse(n > 1, "Yes", "No")) %>% 
  group_by(multiple) %>% 
  tally() %>% 
  adorn_totals("row") # 52 rows all with singular procedures



# Row order function 5 --------------------------------------------------

# subset rows_case_roworder by the rows to which function 5 (filter for 2nd in every 3 rows already filtered by 1 index column match value) should apply

rows_case_roworderf5 <- rows_case_order[c(4, 7),] # only applies to rows 4 and 7

# function 5 needs to select second of every two rows to edit  

data_impute_roworderf5_overwrite <- function(rows_case_roworderf5){
  
  for (i in 1:dim(rows_case_roworderf5)[1]) {
    
    
    vdf_al <- rows_case_roworderf5[i,]
    
    x <- expr(vdf_al$`1 Index column`)
    first <- as.symbol(eval(x))
    
    
    # extract edit column values as variable expressions
    
    edit_one <- rlang::parse_exprs(vdf_al$`Edit column(s)`)[[1]]
    
    
    n <- agimpacts_NA %>% 
      filter(Reference == vdf_al$Reference & eval(first) == vdf_al$`1 Match value`) 
    
    second <- seq(2, dim(n)[1], 3) # select second of every three rows to edit using seq()
    
    n <- n[second,]  %>% 
      cbind(if(!is.na(edit_one)) impute_1 = vdf_al[[edit_one]] else NA) %>% 
      rename(impute_1 = "if (!is.na(edit_one)) impute_1 = vdf_al[[edit_one]] else NA")
    
    if(!is.na(edit_one)) n[[edit_one]] <- n$impute_1
    
    n <- n %>% dplyr::select(!c(impute_1))
    
    # print(n)
    
    
    # reset column types in g (they are all character from rows_in_run) to match column types in agimpacts, by column name - actually just an issue for Temp.Change and CO2.Projected
    n$Temp.Change <- as.numeric(n$Temp.Change)
    n$CO2.Projected <- as.numeric(n$CO2.Projected)
    
    # overwrite row in agimpacts by X, superassign to agimpacts_e in global environment in order to handle sequential/conditional/cumulative edits
    
    agimpacts_NA[agimpacts_NA$X %in% n$X, , drop = FALSE] <<- n # SUPER ASSIGNMENT TO GLOBAL ENVIRONMENT
    
    
    # save/create as a list of tables by name of study or row number in rows_to_run [i] for future checks, append new table to the list iteratively by i inside the loop
    
    n$i <- i  # keep track of which iteration produced table
    NA_list[[i]] <<- data.frame(n) # add it to list
    
    
    # print(dim(n))
    
  }
  
}


# Run row order function 5


NA_list <- list() # empties NA_list but not agimpacts_NA

# always run on most recent version of agimpacts_NA

data_impute_roworderf5_overwrite(rows_case_roworderf5)

agimpacts_NA %>% write.csv("agimpacts_roworderf5.csv")

imputed_data_check_roworderf5 = do.call(rbind, NA_list) %>% relocate(i) %>% write.csv("Imputed_data_check_roworderf5.csv")
imputed_data_check_roworderf5 <-  do.call(rbind, NA_list) 

dim(imputed_data_check_roworderf5) # 52 rows

imputed_data_check_roworderf5 %>% 
  group_by(X) %>% 
  summarise(n = n()) %>% 
  mutate(multiple = ifelse(n > 1, "Yes", "No")) %>% 
  group_by(multiple) %>% 
  tally() %>% 
  adorn_totals("row") # 52 rows all with singular procedures




# Row order function 6 --------------------------------------------------

# subset rows_case_roworder by the rows to which function 6 (filter for 3rd in every 3 rows already filtered by 1 index column match value) should apply

rows_case_roworderf6 <- rows_case_order[c(5, 8),] # only applies to rows 5 and 8

# function 6 needs to select third of every two rows to edit  

data_impute_roworderf6_overwrite <- function(rows_case_roworderf6){
  
  for (i in 1:dim(rows_case_roworderf6)[1]) {
    
    
    vdf_al <- rows_case_roworderf6[i,]
    
    x <- expr(vdf_al$`1 Index column`)
    first <- as.symbol(eval(x))
    
    
    # extract edit column values as variable expressions
    
    edit_one <- rlang::parse_exprs(vdf_al$`Edit column(s)`)[[1]]
    
    
    n <- agimpacts_NA %>% 
      filter(Reference == vdf_al$Reference & eval(first) == vdf_al$`1 Match value`) 
    
    third <- seq(3, dim(n)[1], 3) # select third of every three rows to edit using seq()
    
    n <- n[third,]  %>% 
      cbind(if(!is.na(edit_one)) impute_1 = vdf_al[[edit_one]] else NA) %>% 
      rename(impute_1 = "if (!is.na(edit_one)) impute_1 = vdf_al[[edit_one]] else NA")
    
    if(!is.na(edit_one)) n[[edit_one]] <- n$impute_1
    
    n <- n %>% dplyr::select(!c(impute_1))
    
    # print(n)
    
    
    # reset column types in g (they are all character from rows_in_run) to match column types in agimpacts, by column name - actually just an issue for Temp.Change and CO2.Projected
    n$Temp.Change <- as.numeric(n$Temp.Change)
    n$CO2.Projected <- as.numeric(n$CO2.Projected)
    
    # overwrite row in agimpacts by X, superassign to agimpacts_e in global environment in order to handle sequential/conditional/cumulative edits
    
    agimpacts_NA[agimpacts_NA$X %in% n$X, , drop = FALSE] <<- n # SUPER ASSIGNMENT TO GLOBAL ENVIRONMENT
    
    
    # save/create as a list of tables by name of study or row number in rows_to_run [i] for future checks, append new table to the list iteratively by i inside the loop
    
    n$i <- i  # keep track of which iteration produced table
    NA_list[[i]] <<- data.frame(n) # add it to list
    
    
    # print(dim(n))
    
  }
  
}


# Run row order function 6


NA_list <- list() # empties NA_list but not agimpacts_NA

# always run on most recent version of agimpacts_NA

data_impute_roworderf6_overwrite(rows_case_roworderf6)

agimpacts_NA %>% write.csv("agimpacts_roworderf6.csv")

imputed_data_check_roworderf6 = do.call(rbind, NA_list) %>% relocate(i) %>% write.csv("Imputed_data_check_roworderf6.csv")
imputed_data_check_roworderf6 <-  do.call(rbind, NA_list) 

dim(imputed_data_check_roworderf6) # 52 rows

imputed_data_check_roworderf6 %>% 
  group_by(X) %>% 
  summarise(n = n()) %>% 
  mutate(multiple = ifelse(n > 1, "Yes", "No")) %>% 
  group_by(multiple) %>% 
  tally() %>% 
  adorn_totals("row") # 52 rows all with singular procedures



# Case 11: Misc 1 -------------------------------------------------------

# Define set of rows to apply imputation function to
rows_case_misc1 <- data_validation_sheet %>% 
  filter(Case == "requires `2 Match value` to be changed")

dim(rows_case_misc1) # 1 row

rows_case_misc1$`2 Match value` <- "No "

# Function - note can run general function on it, but redo using agimpacts_NA, and with two index column matches

data_impute_misc1_overwrite <- function(rows_case_misc1){
  
  for (i in 1:dim(rows_case_misc1)[1]) {
    
    
    vdf_al <- rows_case_misc1[i,]
    
    x <- expr(vdf_al$`1 Index column`)
    first <- as.symbol(eval(x))
    
    y <- expr(vdf_al$`2 Index column`)
    second <- as.symbol(eval(y))
    
    # extract edit column values as variable expressions ---------------------------------------
    
    edit_one <- rlang::parse_exprs(vdf_al$`Edit column(s)`)[[1]]
    
    
    n <- agimpacts_NA %>% 
      filter(Reference == vdf_al$Reference & eval(first) == vdf_al$`1 Match value` & eval(second) == vdf_al$`2 Match value`) %>% # 
      cbind(if(!is.na(edit_one)) impute_1 = vdf_al[[edit_one]] else NA) %>% 
      rename(impute_1 = "if (!is.na(edit_one)) impute_1 = vdf_al[[edit_one]] else NA")
    
    if(!is.na(edit_one)) n[[edit_one]] <- n$impute_1
    
    
    n <- n %>% dplyr::select(!c(impute_1))
    
    # reset column types in g (they are all character from rows_in_run) to match column types in agimpacts, by column name - actually just an issue for Temp.Change and CO2.Projected
    n$Temp.Change <- as.numeric(n$Temp.Change)
    n$CO2.Projected <- as.numeric(n$CO2.Projected)
    
    # overwrite row in agimpacts by X, superassign to agimpacts_e in global environment in order to handle sequential/conditional/cumulative edits
    
    agimpacts_NA[agimpacts_NA$X %in% n$X, , drop = FALSE] <<- n # SUPER ASSIGNMENT TO GLOBAL ENVIRONMENT
    
    
    # save/create as a list of tables by name of study or row number in rows_to_run [i] for future checks, append new table to the list iteratively by i inside the loop
    
    n$i <- i  # keep track of which iteration produced table
    NA_list[[i]] <<- data.frame(n) # add it to list
    
    
    # print(dim(n))
    
  }
  
}

# Run

NA_list <- list()

data_impute_misc1_overwrite(rows_case_misc1)

agimpacts_NA %>% write.csv("agimpacts_misc1.csv")

imputed_data_check_misc1 = do.call(rbind, NA_list) %>% relocate(i) %>% write.csv("Imputed_data_check_misc1.csv")
imputed_data_check_misc1 <-  do.call(rbind, NA_list) 

dim(imputed_data_check_misc1) # 1 rows incl. multiple procedures

imputed_data_check_misc1 %>% 
  group_by(X) %>% 
  summarise(n = n()) %>% 
  mutate(multiple = ifelse(n > 1, "Yes", "No")) %>% 
  group_by(multiple) %>% 
  tally() %>% 
  adorn_totals("row") # 1 unique row

# Case 12: Misc 2 -------------------------------------------------------

# Define set of rows to apply imputation function to
rows_case_misc2 <- data_validation_sheet %>% 
  filter(Case == "Dp not working")

dim(rows_case_misc2) # 1 row

# Function


data_impute_misc2_overwrite <- function(rows_case_misc2){
  
  for (i in 1:dim(rows_case_misc2)[1]) {
    
    
    vdf_al <- rows_case_misc2[i,]
    
    x <- expr(vdf_al$`1 Index column`)
    first <- as.symbol(eval(x))
    
    
    # extract edit column values as variable expressions
    
    edit_one <- rlang::parse_exprs(vdf_al$`Edit column(s)`)[[1]]
    
    
    n <- agimpacts_NA %>% 
      filter(Reference == vdf_al$Reference & eval(first) == -22.8632) %>% # 
      cbind(if(!is.na(edit_one)) impute_1 = vdf_al[[edit_one]] else NA) %>% 
      rename(impute_1 = "if (!is.na(edit_one)) impute_1 = vdf_al[[edit_one]] else NA")
    
    
    if(!is.na(edit_one)) n[[edit_one]] <- n$impute_1
    
    
    n <- n %>% dplyr::select(!c(impute_1))
    
    # reset column types in g (they are all character from rows_in_run) to match column types in agimpacts, by column name - actually just an issue for Temp.Change and CO2.Projected
    n$Temp.Change <- as.numeric(n$Temp.Change)
    n$CO2.Projected <- as.numeric(n$CO2.Projected)
    
    # overwrite row in agimpacts by X, superassign to agimpacts_e in global environment in order to handle sequential/conditional/cumulative edits
    
    agimpacts_NA[agimpacts_NA$X %in% n$X, , drop = FALSE] <<- n # SUPER ASSIGNMENT TO GLOBAL ENVIRONMENT
    
    
    # save/create as a list of tables by name of study or row number in rows_to_run [i] for future checks, append new table to the list iteratively by i inside the loop
    
    n$i <- i  # keep track of which iteration produced table
    NA_list[[i]] <<- data.frame(n) # add it to list
    
    print(n)
    
    # print(dim(n))
    
  }
  
}

# Run

NA_list <- list()

data_impute_misc2_overwrite(rows_case_misc2)

agimpacts_NA %>% write.csv("agimpacts_misc2.csv")

imputed_data_check_misc2 = do.call(rbind, NA_list) %>% relocate(i) %>% write.csv("Imputed_data_check_misc2.csv")
imputed_data_check_misc2 <-  do.call(rbind, NA_list) 

dim(imputed_data_check_misc2) # 1 rows incl. multiple procedures

imputed_data_check_misc2 %>% 
  group_by(X) %>% 
  summarise(n = n()) %>% 
  mutate(multiple = ifelse(n > 1, "Yes", "No")) %>% 
  group_by(multiple) %>% 
  tally() %>% 
  adorn_totals("row") # 1 unique row


# Case 13: Row order function 7 -------------------------------------------

# index row 9 of rows_case_order, change comments for every 4 rows 
# instead of alternating rows of Fertilization effect and without fertilization effect, the first four rows should be Fertilization effect followed by four rows of 'Without fertilization effect'

# this function targets the FIRST group of every four rows

rows_case_roworderf7 <- rows_case_order[c(9),]

data_impute_roworderf7_overwrite <- function(rows_case_roworderf7){
  
  for (i in 1:dim(rows_case_roworderf7)[1]) {
    
    
    vdf_al <- rows_case_roworderf7[i,]
    
    
    # extract edit column values as variable expressions
    
    edit_one <- rlang::parse_exprs(vdf_al$`Edit column(s)`)[[1]]
    
    
    n <- agimpacts_NA %>% 
      filter(Reference == vdf_al$Reference) 
    
    first_four_rows <- rep(seq(from = 0, to = nrow(n)-4, by = 8), each = 4) # select first group of 4 rows every 4 rows using seq() and rep() # note can use nrow(n)
    
    # print (n)
    # print(n[1:4 + first_four_rows,])
    # }
    # }
    # data_impute_roworderf7_overwrite(rows_case_roworderf7)
    
    n <- n[1:4 + first_four_rows,]  %>% 
      cbind(impute_1 = "Fertilisation effect") 
    
    if(!is.na(edit_one)) n[[edit_one]] <- n$impute_1
    
    n <- n %>% dplyr::select(!c(impute_1))
    
    
    # print(dim(n))
    # print(dim(agimpacts_NA[agimpacts_NA$X %in% n$X,]))
    
    # reset column types in g (they are all character from rows_in_run) to match column types in agimpacts, by column name - actually just an issue for Temp.Change and CO2.Projected
    n$Temp.Change <- as.numeric(n$Temp.Change)
    n$CO2.Projected <- as.numeric(n$CO2.Projected)
    
    # overwrite row in agimpacts by X, superassign to agimpacts_e in global environment in order to handle sequential/conditional/cumulative edits
    
    agimpacts_NA[agimpacts_NA$X %in% n$X, , drop = FALSE] <<- n # SUPER ASSIGNMENT TO GLOBAL ENVIRONMENT
    
    
    # save/create as a list of tables by name of study or row number in rows_to_run [i] for future checks, append new table to the list iteratively by i inside the loop
    
    n$i <- i  # keep track of which iteration produced table
    NA_list[[i]] <<- data.frame(n) # add it to list
    
    
    # print(dim(n))
    
  }
  
}


# Run row order function 7


NA_list <- list() # empties NA_list but not agimpacts_NA

# always run on most recent version of agimpacts_NA

data_impute_roworderf7_overwrite(rows_case_roworderf7)

agimpacts_NA %>% write.csv("agimpacts_roworderf7.csv")

imputed_data_check_roworderf7 = do.call(rbind, NA_list) %>% relocate(i) %>% write.csv("Imputed_data_check_roworderf7.csv")
imputed_data_check_roworderf7 <-  do.call(rbind, NA_list) 

dim(imputed_data_check_roworderf7) # 36 rows

imputed_data_check_roworderf7 %>% 
  group_by(X) %>% 
  summarise(n = n()) %>% 
  mutate(multiple = ifelse(n > 1, "Yes", "No")) %>% 
  group_by(multiple) %>% 
  tally() %>% 
  adorn_totals("row") # 36 rows all with singular procedures

# Case 14: Row order function 8 -------------------------------------------

# index row 9 of rows_case_order, change comments for every 4 rows 
# instead of alternating rows of Fertilization effect and without fertilization effect, the first four rows should be Fertilization effect followed by four rows of 'Without fertilization effect'

# this function targets the SECOND group of every four rows

rows_case_roworderf8 <- rows_case_order[c(9),]

data_impute_roworderf8_overwrite <- function(rows_case_roworderf8){
  
  for (i in 1:dim(rows_case_roworderf8)[1]) {
    
    
    vdf_al <- rows_case_roworderf8[i,]
    
    
    # extract edit column values as variable expressions
    
    edit_one <- rlang::parse_exprs(vdf_al$`Edit column(s)`)[[1]]
    
    
    n <- agimpacts_NA %>% 
      filter(Reference == vdf_al$Reference) 
    
    second_four_rows <- rep(seq(from = 0, to = nrow(n)-4, by = 8), each = 4) # select first group of 4 rows every 4 rows using seq() and rep() # note can use nrow(n)
    
    # print (n)
    #print(n[5:8 + second_four_rows,], n = Inf)
    # }
    # }
    
    #data_impute_roworderf8_overwrite(rows_case_roworderf8)
    
    n <- n[5:8 + second_four_rows,]  %>% 
      cbind(impute_1 = "Without fertilisation effect") 
    
    if(!is.na(edit_one)) n[[edit_one]] <- n$impute_1
    
    n <- n %>% dplyr::select(!c(impute_1))
    
    
    # print(dim(n))
    # print(dim(agimpacts_NA[agimpacts_NA$X %in% n$X,]))
    
    # reset column types in g (they are all character from rows_in_run) to match column types in agimpacts, by column name - actually just an issue for Temp.Change and CO2.Projected
    n$Temp.Change <- as.numeric(n$Temp.Change)
    n$CO2.Projected <- as.numeric(n$CO2.Projected)
    
    # overwrite row in agimpacts by X, superassign to agimpacts_e in global environment in order to handle sequential/conditional/cumulative edits
    
    agimpacts_NA[agimpacts_NA$X %in% n$X, , drop = FALSE] <<- n # SUPER ASSIGNMENT TO GLOBAL ENVIRONMENT
    
    
    # save/create as a list of tables by name of study or row number in rows_to_run [i] for future checks, append new table to the list iteratively by i inside the loop
    
    n$i <- i  # keep track of which iteration produced table
    NA_list[[i]] <<- data.frame(n) # add it to list
    
    
    # print(dim(n))
    
  }
  
}


# Run row order function 7


NA_list <- list() # empties NA_list but not agimpacts_NA

# always run on most recent version of agimpacts_NA

data_impute_roworderf8_overwrite(rows_case_roworderf8)

agimpacts_NA %>% write.csv("agimpacts_roworderf8.csv")

imputed_data_check_roworderf8 = do.call(rbind, NA_list) %>% relocate(i) %>% write.csv("Imputed_data_check_roworderf8.csv")
imputed_data_check_roworderf8 <-  do.call(rbind, NA_list) 

dim(imputed_data_check_roworderf8) # 36 rows

imputed_data_check_roworderf8 %>% 
  group_by(X) %>% 
  summarise(n = n()) %>% 
  mutate(multiple = ifelse(n > 1, "Yes", "No")) %>% 
  group_by(multiple) %>% 
  tally() %>% 
  adorn_totals("row") # 36 rows all with singular procedures

# Total imputation counter before altering number of point estimates------------------------------------------------

# WITHIN duplicates: if adding all rows that have been changed across the 12 cases, we have 6368 rows, these contain duplicates WITHIN each case function for certain
sum(dim(imputed_data_check_general)[1],
    dim(imputed_data_check_general_backslash)[1],
    dim(imputed_data_check_NA)[1],
    dim(imputed_data_check_notNA)[1],
    dim(imputed_data_check_roworderf1)[1],
    dim(imputed_data_check_roworderf2)[1],
    dim(imputed_data_check_roworderf3)[1],
    dim(imputed_data_check_roworderf4)[1],
    dim(imputed_data_check_roworderf5)[1],
    dim(imputed_data_check_roworderf6)[1],
    dim(imputed_data_check_misc1)[1],
    dim(imputed_data_check_misc2)[1],
    dim(imputed_data_check_roworderf7)[1],
    dim(imputed_data_check_roworderf8))

# ACROSS duplicates: if adding all unique rows that have been changed across the 12 cases, we have 5660 rows, however these may still be duplicated ACROSS the case functions
sum(4586,344,346,40,62,26,26,52,52,52,1,1,36,36)


# Before Delete/Filter and add data entry for Alexandrov et al, count the number of rows that have changed from application of 12 case functions

AGIMPACTS_IMPUTED <- agimpacts_NA

# note that agimpacts_NA/AGIMPACTS_IMPUTED object in R should be equivalent to agimpacts_misc2.csv, check this

# install.packages("arsenal")
library(arsenal)

agimpacts_roworderf8 <- read.csv("agimpacts_roworderf8.csv")
check1 <- comparedf(agimpacts_NA, agimpacts_roworderf8)
summary(comparedf(agimpacts_NA, agimpacts_roworderf8)) 

colnames(agimpacts_NA)
colnames(agimpacts_roworderf8)

# other than X.1 generated in agimpacts_misc2, these are identical as expected. Equivalent to most recent version.

# check types/classes identical
str(AGIMPACTS_IMPUTED)

# use anti join to find unmatched records by X
# anti_join(AGIMPACTS_IMPUTED, agimpacts) # 4815



comparedf(AGIMPACTS_IMPUTED, agimpacts)
summary(comparedf(AGIMPACTS_IMPUTED, agimpacts)) # 4815 observations with some compared variables unequal (changed), 7800 values unequal (changed), representing multiple variables changed on the one row.
# 9165 obs with all compared variables equal (i.e. were correctly entered)

# install.packages("writexl")
# library(writexl)

# need a way to inspect all the observations with some compared variables unequal - summary comparedf only shows first 50 values
n.diffs(comparedf(AGIMPACTS_IMPUTED, agimpacts)) # 7800 values
diffs(comparedf(AGIMPACTS_IMPUTED, agimpacts)) 

# install.packages("compareDF")
library(compareDF)

pre_change_log <- compare_df(AGIMPACTS_IMPUTED, agimpacts) # prints a log of total changes made
pre_change_log$comparison_df # log

pre_change_log$comparison_df %>% 
  group_by(rowname) %>% 
  summarise(n=n()) # doesn't seem to count number of changes made to a single row

pre_change_log$comparison_df %>% write.csv("pre_change_log_comparison_df.csv")

pre_change_log$change_count

pre_change_log$change_count %>%
  group_by(rowname) %>% 
  summarise(n=n()) # %>% 
print(n=Inf)

pre_change_log$change_count %>% write.csv("pre_change_log_change_count.csv")

pre_change_log$change_summary # 4815 changes
pre_change_log$change_markers

pre_change_log$comparison_df %>% 
  filter(chng_type == "+")

# install.packages("htmlTable")
library(htmlTable)

print(pre_change_log$html_output)
view_html(comparison_output = pre_change_log) # only some rows displayed

# 4816 unique rows changed, 7728 values changed from cases 1-12, before performing case 13

# Case 15: Delete/Filter -------------------------------------------------------
AGIMPACTS_IMPUTED <- agimpacts_NA
AGIMPACTS_IMPUTED_E <- AGIMPACTS_IMPUTED

# Define set of rows to apply imputation function to
rows_to_delete <- data_validation_sheet %>% 
  filter(Case == "Delete/Filter")

dim(rows_to_delete) # 22

# Function

# only up to 2 index match columns

# note this doesn't have to be a for loop, could just apply for all rows in rows_to_delete

data_impute_delete <- function(rows_to_delete){
  
  for (i in 1:dim(rows_to_delete)[1]) {
    
    
    vdf_al <- rows_to_delete[i,]
    
    x <- expr(vdf_al$`1 Index column`)
    first <- as.symbol(eval(x))
    
    y <- expr(vdf_al$`2 Index column`)
    second <- as.symbol(eval(y))
    
    # extract edit column values as variable expressions ---------------------------------------
    
    edit_one <- rlang::parse_exprs(vdf_al$`Edit column(s)`)[[1]]
    
    edit_two <-  if(
      count.fields(textConnection(vdf_al$`Edit column(s)`), sep = "; ") == 1)  NA else rlang::parse_exprs(vdf_al$`Edit column(s)`)[[2]]
    
    
    n <- AGIMPACTS_IMPUTED_E %>% 
      filter(if(
        !is.na(vdf_al$`1 Index column`) & !is.na(vdf_al$`2 Index column`))
        Reference == vdf_al$Reference & eval(first) == vdf_al$`1 Match value` & eval(second) == vdf_al$`2 Match value`
        else if(
          !is.na(vdf_al$`1 Index column`))
          Reference == vdf_al$Reference & eval(first) == vdf_al$`1 Match value`
        else if(
          is.na(vdf_al$`1 Index column`) & is.na(vdf_al$`2 Index column`) & is.na(vdf_al$`3 Index column`))
          Reference == vdf_al$Reference) 
    
    #print(n)
    #  }
    #  }
    #data_impute_delete(rows_to_delete)
    AGIMPACTS_IMPUTED_E <<- AGIMPACTS_IMPUTED_E[!(AGIMPACTS_IMPUTED_E$X %in% n$X), ] 
    
    # agimpacts_NA[agimpacts_NA$X %in% n$X, , drop = FALSE] <<- n # SUPER ASSIGNMENT TO GLOBAL ENVIRONMENT
    
    
    # save/create as a list of tables by name of study or row number in rows_to_run [i] for future checks, append new table to the list iteratively by i inside the loop
    
    n$i <- i  # keep track of which iteration produced table
    NA_list[[i]] <<- data.frame(n) # add it to list
    
    
    # print(dim(n))
    
  }
  
}

# Run

NA_list <- list()

data_impute_delete(rows_to_delete)

AGIMPACTS_IMPUTED_E %>% write.csv("agimpacts_rowsdeleted.csv")

# Checks

dim(AGIMPACTS_IMPUTED_E) # 13341 rows, deleted 639 rows
imputed_data_check_rowsdeleted = do.call(rbind, NA_list) %>% relocate(i) %>% write.csv("Imputed_data_check_rowsdeleted.csv")
imputed_data_check_rowsdeleted <-  do.call(rbind, NA_list) 

dim(imputed_data_check_rowsdeleted) # 639 rows

# Case 16: Alexandrov et al data entry -------------------------------------------------------

# Define set of rows to apply imputation function to

# must close the excel sheet before loading
alexandrov_data_entry_sheet <- read_excel(path = xl_validation_book, sheet = "Alexandrov et al data only")

# Function
# merge and then fill down the other columns using data from Alexandrov et al

#  AGIMPACTS_IMPUTED_F <- AGIMPACTS_IMPUTED_E %>% full_join(alexandrov_data_entry_sheet)

#  dim(AGIMPACTS_IMPUTED_F) # 13689


# look at the data that hast just been imputed - 348 rows
#  AGIMPACTS_IMPUTED_F %>% 
#    filter(is.na(Reference)) %>% 
#    print(n=Inf) %>% 


# need to fill down X from previous last index number in X

#  tail(AGIMPACTS_IMPUTED_E) # 13980 is the last index number previously

# X needs to start from 13981, i.e. 13981 + 348 - 1 so index number 13981 : 14328. 
# check - because dropping the 639 rows in Case 15 has not rearranged the rows, 14328 - 639 = 13689, which brings us back to the dimensions of AGIMPACTS_IMPUTED_F

# save.image("agimpacts-190521.RData")

# AGIMPACTS_IMPUTED_F <- AGIMPACTS_IMPUTED_F %>% 
#   mutate(X = replace(X, is.na(Reference), c(13981:14328)))

#  tail(AGIMPACTS_IMPUTED_F)

# need to copy down values for every other column from Alexandrov et al. entries using fill, by City (lon/lat differs, the other column values are constant)

# subset AGIMPACTS_IMPUTED_F by Reference == Alexandrov et al., use this as a look up table?

lookup <- AGIMPACTS_IMPUTED_E %>% 
  filter(Reference == "Alexandrov et al.") %>% 
  group_by(City) %>% 
  slice(1) %>% 
  ungroup %>% 
  replace(c("X", "Temp.Change", "Precipitation.change", "Yield.Change", "Projec.yield.change.start", "Projec.yield.change.end"), NA)

# coalesce join function - https://alistaire.rbind.io/blog/coalescing-joins/

coalesce_join <- function(x, y, 
                          by = NULL, suffix = c(".x", ".y"), 
                          join = dplyr::full_join, ...) {
  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output
  cols <- union(names(x), names(y))
  
  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce, 
    1, 
    nchar(to_coalesce) - nchar(suffix_used)
  ))
  
  coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
    joined[[paste0(.x, suffix[1])]], 
    joined[[paste0(.x, suffix[2])]]
  ))
  names(coalesced) <- to_coalesce
  
  dplyr::bind_cols(joined, coalesced)[cols]
}

# join and coalesce non_NA entries with lookup table which contains reference Alexandrov et al data for other columns values grouped by City 

alexandrov_data_entry_filled <- coalesce_join(alexandrov_data_entry_sheet, lookup, by = "City") %>% 
  relocate(colnames(AGIMPACTS_IMPUTED_E)) %>% 
  mutate(X = c(13981:14328))

# bind to complete dataset
AGIMPACTS_IMPUTED_F <- AGIMPACTS_IMPUTED_E %>% full_join(alexandrov_data_entry_filled)

dim(AGIMPACTS_IMPUTED_F) # 13689


# create new X index column - X.1

AGIMPACTS_IMPUTED_F <- AGIMPACTS_IMPUTED_F %>% 
  mutate(X.1 = row_number()) %>% 
  relocate(X.1)

# check
tail(AGIMPACTS_IMPUTED_F) %>% glimpse()  


# save cleaned data as AGIMPACTS_FINAL and add more variables ------------------------------------

AGIMPACTS_FINAL <- AGIMPACTS_IMPUTED_F

AGIMPACTS_FINAL %>% readr::write_csv(here("processed", "agimpacts_final.csv"))

