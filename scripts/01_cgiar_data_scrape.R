# Webscrape agimpacts data for meta-analysis ----------------------------------------------------------------


## load webpage for agimpacts
agimpacts <- "http://ag-impacts.org/estimatesearch/#"
agimpacts <- read_html(agimpacts)
agimpacts
str(agimpacts)

# look at the body (nodes) of the webpage
body_nodes <- agimpacts %>% 
  html_node("body") %>% 
  html_children()


# Compact view scroll table -----------------------------------------------

# POPULATE THIS WITH NETWORK > HEADER > LAST SECTION!

form_body <- list(draw = "2", `columns[0][data]` = "Crop", `columns[0][name]` = "", 
                  `columns[0][searchable]` = "true", `columns[0][orderable]` = "true", 
                  `columns[0][search][value]` = "", `columns[0][search][regex]` = "false", 
                  `columns[1][data]` = "Multi-Model", `columns[1][name]` = "", 
                  `columns[1][searchable]` = "true", `columns[1][orderable]` = "true", 
                  `columns[1][search][value]` = "", `columns[1][search][regex]` = "false", 
                  `columns[2][data]` = "Baseline Period", `columns[2][name]` = "", 
                  `columns[2][searchable]` = "true", `columns[2][orderable]` = "true", 
                  `columns[2][search][value]` = "", `columns[2][search][regex]` = "false", 
                  `columns[3][data]` = "Projection Period", `columns[3][name]` = "", 
                  `columns[3][searchable]` = "true", `columns[3][orderable]` = "true", 
                  `columns[3][search][value]` = "", `columns[3][search][regex]` = "false", 
                  `columns[4][data]` = "Percentage Yield Change", `columns[4][name]` = "", 
                  `columns[4][searchable]` = "true", `columns[4][orderable]` = "true", 
                  `columns[4][search][value]` = "", `columns[4][search][regex]` = "false", 
                  `columns[5][data]` = "Geographical Scope", `columns[5][name]` = "", 
                  `columns[5][searchable]` = "true", `columns[5][orderable]` = "true", 
                  `columns[5][search][value]` = "", `columns[5][search][regex]` = "false", 
                  `columns[6][data]` = "Local Mean Temperature Change", `columns[6][name]` = "", 
                  `columns[6][searchable]` = "true", `columns[6][orderable]` = "true", 
                  `columns[6][search][value]` = "", `columns[6][search][regex]` = "false", 
                  `columns[7][data]` = "Climate Scenario", `columns[7][name]` = "", 
                  `columns[7][searchable]` = "true", `columns[7][orderable]` = "true", 
                  `columns[7][search][value]` = "", `columns[7][search][regex]` = "false", 
                  `columns[8][data]` = "Adaptation", `columns[8][name]` = "", 
                  `columns[8][searchable]` = "true", `columns[8][orderable]` = "true", 
                  `columns[8][search][value]` = "", `columns[8][search][regex]` = "false", 
                  `order[0][column]` = "0", `order[0][dir]` = "asc", 
                  start = "0", length = "13980", `search[value]` = "", `search[regex]` = "false", # show all 13,980 entries
                  crop = "", 
                  model = "", 
                  scale = "", 
                  climate = "",
                  baseline = "",
                  period = "",
                  country = "",
                  continents = "",
                  regions = "",
                  adaptation = "")


form_url <- "http://ag-impacts.org/wp-content/themes/agimpacts/dataTableFilter.php" 

result_json <- httr::content(httr::POST(form_url, body = form_body), "text")  


df <- jsonlite::fromJSON(result_json)
dplyr::as_tibble(df$data) # data is a list of values, try to see these values and rearrange as df

compact.df <- as.data.frame(df$data)

compact.df <- compact.df %>% 
  rename("Crop" = V1,
         "Multi-Model" = V2,
         "Baseline Period" = V3,
         "Projection Period" = V4,
         "Percentage Yield Change" = V5,
         "Geographical Scope" = V6,
         "Local Mean Temperature Change" = V7,
         "Climate Scenario" = V8,
         "Adaptation" = V9)


compact.df %>% write.csv("agimpacts_compact.csv")


# Full view scroll table --------------------------------------------------


# scrape view all fields with 40 variables as seen here: http://ag-impacts.org/fullview'
# check Developer Tools > Network > run query > XHR > same form_url


form_body_full <- list(draw = "2", `columns[0][data]` = "0", `columns[0][name]` = "", 
                       `columns[0][searchable]` = "true", `columns[0][orderable]` = "true", 
                       `columns[0][search][value]` = "", `columns[0][search][regex]` = "false", 
                       `columns[1][data]` = "1", `columns[1][name]` = "", 
                       `columns[1][searchable]` = "true", `columns[1][orderable]` = "true", 
                       `columns[1][search][value]` = "", `columns[1][search][regex]` = "false", 
                       `columns[2][data]` = "2", `columns[2][name]` = "", 
                       `columns[2][searchable]` = "true", `columns[2][orderable]` = "true", 
                       `columns[2][search][value]` = "", `columns[2][search][regex]` = "false", 
                       `columns[3][data]` = "3", `columns[3][name]` = "", 
                       `columns[3][searchable]` = "true", `columns[3][orderable]` = "true", 
                       `columns[3][search][value]` = "", `columns[3][search][regex]` = "false", 
                       `columns[4][data]` = "4", `columns[4][name]` = "", 
                       `columns[4][searchable]` = "true", `columns[4][orderable]` = "true", 
                       `columns[4][search][value]` = "", `columns[4][search][regex]` = "false", 
                       `columns[5][data]` = "5", `columns[5][name]` = "", 
                       `columns[5][searchable]` = "true", `columns[5][orderable]` = "true", 
                       `columns[5][search][value]` = "", `columns[5][search][regex]` = "false", 
                       `columns[6][data]` = "6", `columns[6][name]` = "", 
                       `columns[6][searchable]` = "true", `columns[6][orderable]` = "true", 
                       `columns[6][search][value]` = "", `columns[6][search][regex]` = "false", 
                       `columns[7][data]` = "7", `columns[7][name]` = "", 
                       `columns[7][searchable]` = "true", `columns[7][orderable]` = "true", 
                       `columns[7][search][value]` = "", `columns[7][search][regex]` = "false", 
                       `columns[8][data]` = "8", `columns[8][name]` = "", 
                       `columns[8][searchable]` = "true", `columns[8][orderable]` = "true", 
                       `columns[8][search][value]` = "", `columns[8][search][regex]` = "false", 
                       `columns[9][data]` = "9", `columns[9][name]` = "", 
                       `columns[9][searchable]` = "true", `columns[9][orderable]` = "true", 
                       `columns[9][search][value]` = "", `columns[9][search][regex]` = "false",
                       `columns[10][data]` = "10", `columns[10][name]` = "", 
                       `columns[10][searchable]` = "true", `columns[10][orderable]` = "true", 
                       `columns[10][search][value]` = "", `columns[10][search][regex]` = "false",
                       `columns[11][data]` = "11", `columns[11][name]` = "", 
                       `columns[11][searchable]` = "true", `columns[11][orderable]` = "true", 
                       `columns[11][search][value]` = "", `columns[11][search][regex]` = "false",
                       `columns[12][data]` = "12", `columns[12][name]` = "", 
                       `columns[12][searchable]` = "true", `columns[12][orderable]` = "true", 
                       `columns[12][search][value]` = "", `columns[12][search][regex]` = "false",
                       `columns[13][data]` = "13", `columns[13][name]` = "", 
                       `columns[13][searchable]` = "true", `columns[13][orderable]` = "true", 
                       `columns[13][search][value]` = "", `columns[13][search][regex]` = "false",
                       `columns[14][data]` = "14", `columns[14][name]` = "", 
                       `columns[14][searchable]` = "true", `columns[14][orderable]` = "true", 
                       `columns[14][search][value]` = "", `columns[14][search][regex]` = "false",
                       `columns[15][data]` = "15", `columns[15][name]` = "", 
                       `columns[15][searchable]` = "true", `columns[15][orderable]` = "true", 
                       `columns[15][search][value]` = "", `columns[15][search][regex]` = "false",
                       `columns[16][data]` = "16", `columns[16][name]` = "", 
                       `columns[16][searchable]` = "true", `columns[16][orderable]` = "true", 
                       `columns[16][search][value]` = "", `columns[16][search][regex]` = "false",
                       `columns[17][data]` = "17", `columns[17][name]` = "", 
                       `columns[17][searchable]` = "true", `columns[17][orderable]` = "true", 
                       `columns[17][search][value]` = "", `columns[17][search][regex]` = "false",
                       `columns[18][data]` = "18", `columns[18][name]` = "", 
                       `columns[18][searchable]` = "true", `columns[18][orderable]` = "true", 
                       `columns[18][search][value]` = "", `columns[18][search][regex]` = "false",
                       `columns[19][data]` = "19", `columns[19][name]` = "", 
                       `columns[19][searchable]` = "true", `columns[19][orderable]` = "true", 
                       `columns[19][search][value]` = "", `columns[19][search][regex]` = "false",
                       `columns[20][data]` = "20", `columns[20][name]` = "", 
                       `columns[20][searchable]` = "true", `columns[20][orderable]` = "true", 
                       `columns[20][search][value]` = "", `columns[20][search][regex]` = "false",
                       `columns[21][data]` = "21", `columns[21][name]` = "", 
                       `columns[21][searchable]` = "true", `columns[21][orderable]` = "true", 
                       `columns[21][search][value]` = "", `columns[21][search][regex]` = "false",
                       `columns[22][data]` = "22", `columns[22][name]` = "", 
                       `columns[22][searchable]` = "true", `columns[22][orderable]` = "true", 
                       `columns[22][search][value]` = "", `columns[22][search][regex]` = "false",
                       `columns[23][data]` = "23", `columns[23][name]` = "", 
                       `columns[23][searchable]` = "true", `columns[23][orderable]` = "true", 
                       `columns[23][search][value]` = "", `columns[23][search][regex]` = "false",
                       `columns[24][data]` = "24", `columns[24][name]` = "", 
                       `columns[24][searchable]` = "true", `columns[24][orderable]` = "true", 
                       `columns[24][search][value]` = "", `columns[24][search][regex]` = "false",
                       `columns[25][data]` = "25", `columns[25][name]` = "", 
                       `columns[25][searchable]` = "true", `columns[25][orderable]` = "true", 
                       `columns[25][search][value]` = "", `columns[25][search][regex]` = "false",
                       `columns[26][data]` = "26", `columns[26][name]` = "", 
                       `columns[26][searchable]` = "true", `columns[26][orderable]` = "true", 
                       `columns[26][search][value]` = "", `columns[26][search][regex]` = "false",
                       `columns[27][data]` = "27", `columns[27][name]` = "", 
                       `columns[27][searchable]` = "true", `columns[27][orderable]` = "true", 
                       `columns[27][search][value]` = "", `columns[27][search][regex]` = "false",
                       `columns[28][data]` = "28", `columns[28][name]` = "", 
                       `columns[28][searchable]` = "true", `columns[28][orderable]` = "true", 
                       `columns[28][search][value]` = "", `columns[28][search][regex]` = "false",
                       `columns[29][data]` = "29", `columns[29][name]` = "", 
                       `columns[29][searchable]` = "true", `columns[29][orderable]` = "true", 
                       `columns[29][search][value]` = "", `columns[29][search][regex]` = "false",
                       `columns[30][data]` = "30", `columns[30][name]` = "", 
                       `columns[30][searchable]` = "true", `columns[30][orderable]` = "true", 
                       `columns[30][search][value]` = "", `columns[30][search][regex]` = "false",
                       `columns[31][data]` = "31", `columns[31][name]` = "", 
                       `columns[31][searchable]` = "true", `columns[31][orderable]` = "true", 
                       `columns[31][search][value]` = "", `columns[31][search][regex]` = "false",
                       `columns[32][data]` = "32", `columns[32][name]` = "", 
                       `columns[32][searchable]` = "true", `columns[32][orderable]` = "true", 
                       `columns[32][search][value]` = "", `columns[32][search][regex]` = "false",
                       `columns[33][data]` = "33", `columns[33][name]` = "", 
                       `columns[33][searchable]` = "true", `columns[33][orderable]` = "true", 
                       `columns[33][search][value]` = "", `columns[33][search][regex]` = "false",
                       `columns[34][data]` = "34", `columns[34][name]` = "", 
                       `columns[34][searchable]` = "true", `columns[34][orderable]` = "true", 
                       `columns[34][search][value]` = "", `columns[34][search][regex]` = "false",
                       `columns[35][data]` = "35", `columns[35][name]` = "", 
                       `columns[35][searchable]` = "true", `columns[35][orderable]` = "true", 
                       `columns[35][search][value]` = "", `columns[35][search][regex]` = "false",
                       `columns[36][data]` = "36", `columns[36][name]` = "", 
                       `columns[36][searchable]` = "true", `columns[36][orderable]` = "true", 
                       `columns[36][search][value]` = "", `columns[36][search][regex]` = "false",
                       `columns[37][data]` = "37", `columns[37][name]` = "", 
                       `columns[37][searchable]` = "true", `columns[37][orderable]` = "true", 
                       `columns[37][search][value]` = "", `columns[37][search][regex]` = "false",
                       `columns[38][data]` = "38", `columns[38][name]` = "", 
                       `columns[38][searchable]` = "true", `columns[38][orderable]` = "true", 
                       `columns[38][search][value]` = "", `columns[38][search][regex]` = "false",
                       `columns[39][data]` = "39", `columns[39][name]` = "", 
                       `columns[39][searchable]` = "true", `columns[39][orderable]` = "true", 
                       `columns[39][search][value]` = "", `columns[39][search][regex]` = "false",
                       `order[0][column]` = "0", `order[0][dir]` = "asc", 
                       start = "0", length = "13990", `search[value]` = "", `search[regex]` = "false", # note that cannot show 13,980? but 13,990 works
                       crop = "", 
                       model = "", 
                       scale = "", 
                       climate = "",
                       baseline = "",
                       period = "",
                       country = "",
                       allfields = "true",
                       subcontinents = "",
                       adaptation = "")


form_url <- "http://ag-impacts.org/wp-content/themes/agimpacts/dataTableFilter.php" 

result_full_json <- httr::content(httr::POST(form_url, body = form_body_full), "text")  


df_full <- jsonlite::fromJSON(result_full_json)
dplyr::as_tibble(df_full$data) # data is a list of values, try to see these values and rearrange as df

full.df <- as.data.frame(df_full$data)


full.df <- full.df %>% 
  rename("DOI" = V1,
         "Author" = V2,
         "Year" = V3,
         "Journal" = V4,
         "Volume" = V5,
         "Issue" = V6,
         "Start Page" = V7,
         "End Page" = V8,
         "Reference" = V9,
         "Title" = V10,
         "Crop" = V11,
         "Scientific name" = V12,
         "CO2 Projected" = V13,
         "CO2 Baseline" = V14,
         "Temp Change" = V15,
         "Precipitation change" = V16,
         "Yield Change" = V17,
         "Projec yield change start" = V18,
         "Projec yield change end" = V19,
         "Adaptation" = V20,
         "Climate scenario" = V21,
         "# GCM used" = V22,
         "GCM(s)" = V23,
         "# Impact model used" = V24,
         "Impact model(s)" = V25,
         "Baseline start" = V26,
         "Baseline end" = V27,
         "Projection start" = V28,
         "Projection end" = V29,
         "Geo scope" = V30,
         "Region" = V31,
         "Country" = V32,
         "State" = V33,
         "City" = V34,
         "Latitude" = V35,
         "Longitude" = V36,
         "Spatial Scale" = V37,
         "Comments" = V38,
         "Contributor" = V39,
         "Status" = V40)


full.df %>% write.csv("agimpacts_full.csv") # saved in \processed\