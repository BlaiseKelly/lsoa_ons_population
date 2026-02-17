library(dplyr)
library(openxlsx)
library(tidyr)
library(purrr)
library(sf)
library(piggyback)

dir.create("data/")

# download the files
download.file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2022revisednov2025tomid2024/sapelsoasyoa20222024.xlsx",
              destfile = "data/2022_2024.xlsx",mode = "wb")

download.file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2019tomid2022/sapelsoasyoa20192022.xlsx",
              destfile = "data/2019_2021.xlsx",mode = "wb")

download.file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2015tomid2018/sapelsoasyoa20152018.xlsx",
              destfile = "data/2015_2018.xlsx",mode = "wb")

download.file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2011tomid2014/sapelsoasyoa20112014.xlsx",
              destfile = "data/2011_2014.xlsx",mode = "wb")

download.file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2002tomid2011females/rftlsoaunformattedtablefemales.zip",
              destfile = "data/female_2002_2011.zip",mode = "wb")

unzip("data/female_2002_2011.zip")

download.file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2002tomid2011males/rftlsoaunformattedtablemales.zip",
              destfile = "data/male_2002_2011.zip",mode = "wb")

unzip("data/male_2002_2011.zip")

# list all the files downloaded
filez = list.files("data/", pattern = ".xlsx")

# loop through each file and extract the data for each year and reshape to a df with LA and a column for each year for total pop and age and sex disaggregated
yr_tots = list()
yr_a_s = list()
yr_la_tot = list()
yr_la_as_tot = list()
for (f in filez){
  
  # create a string of years to get the sheets for each file
  yrz = stringr::str_split(gsub(".xlsx","",f), "_",simplify = TRUE)
  yrz = seq(yrz[1],yrz[2],1)
  
  for (y in yrz){
  
    # import the sheet for the respective year and get rid of the LAD columns (first 2)
  ons_pop = read.xlsx(paste0("data/",f),
                       sheet = paste0("Mid-",y," LSOA 2021"),startRow = 4)
  
  # trim to totals only
  ons_tot = ons_pop[,-c(1,2)] |> 
    select(LSOA.2021.Code,LSOA.2021.Name, Total)
  
  # rename to neater code and name format and make total column year
  names(ons_tot) = c("lsoa21cd", "lsoa21nm", y)  
  
  # for age and sex collapse the df to make one total column
  ons_a_s = ons_pop[,-c(1,2)] |> 
    select(-Total) |> 
    tidyr::pivot_longer(-c("LSOA.2021.Code","LSOA.2021.Name"),names_to = "sex_age", values_to = as.character(y))
  
  ons_pop_LA = ons_pop[,c(1,2,5)]
  
  names(ons_pop_LA) = c("LADCD", "LADNM", "Total")
  
  ons_pop_LA = ons_pop_LA |> 
    group_by(LADCD,LADNM) |> 
    summarise(Total = sum(Total))
  
  names(ons_pop_LA) = c("LADCD", "LADNM", paste0("Total_",y))
  
  la_names = names(ons_pop)[1:2]
  
  ons_pop_as_LA = ons_pop[,-c(3,4)] |> 
    select(-Total) |> 
    tidyr::pivot_longer(-la_names,names_to = "sex_age", values_to = "pop") 
  
  names(ons_pop_as_LA) = c("LADCD", "LADNM","sex_age", "pop")
  
  ons_pop_as_LA = ons_pop_as_LA |> 
    group_by(LADCD,LADNM,sex_age) |> 
    summarise(pop = sum(pop))
  
  # write to lists
  yr_tots[[as.character(y)]] = ons_tot
  yr_a_s[[as.character(y)]] = ons_a_s
  yr_la_tot[[as.character(y)]] = ons_pop_LA
  yr_la_as_tot[[as.character(y)]] = ons_pop_as_LA
  
  # show where up to
  print(paste(f,y))
  
  }
  
}
  
# left join all list items
  pop_tots = reduce(yr_tots, left_join, by = c("lsoa21cd", "lsoa21nm"))
  
  pop_a_s = reduce(yr_a_s, left_join, by = c("LSOA.2021.Code","LSOA.2021.Name","sex_age"))
  
  pop_tots_la = reduce(yr_la_tot, left_join, by = c("LADCD","LADNM"))
  
  pop_a_s_la = reduce(yr_la_as_tot, left_join, by = c("LADCD","LADNM","sex_age"))

  dir.create("outputs/")
  
  # write as single csvs
  write.csv(pop_tots, "outputs/lsoa21_pop_tot_2011_2024.csv", row.names = FALSE)  
  
  write.csv(pop_a_s, "outputs/lsoa21_pop_age_sex_2011_2024.csv", row.names = FALSE)  
  
  write.csv(pop_tots_la, "outputs/LA_pop_tot_2011_2024.csv", row.names = FALSE) 
  
  write.csv(pop_a_s_la, "outputs/LA_pop_age_sex_2011_2024.csv", row.names = FALSE) 
  
  # Add top github release
  piggyback::pb_new_release(repo = "blaisekelly/lsoa_ons_population", tag = "v0.1.1")
  
  # upload to GitHub Release so other scripts can download
  piggyback::pb_upload(
    file = paste0("outputs/lsoa21_pop_tot_2011_2024.csv"),
    repo = "blaisekelly/lsoa_ons_population",
    overwrite = TRUE,
    tag = "v0.1.1")
  
  # upload to GitHub Release so other scripts can download
  piggyback::pb_upload(
    file = paste0("outputs/lsoa21_pop_age_sex_2011_2024.csv"),
    repo = "blaisekelly/lsoa_ons_population",
    overwrite = TRUE,
    tag = "v0.1.1")
  
  # upload to GitHub Release so other scripts can download
  piggyback::pb_upload(
    file = paste0("outputs/LA_pop_tot_2011_2024.csv"),
    repo = "blaisekelly/lsoa_ons_population",
    overwrite = TRUE,
    tag = "v0.1.1")
  
  piggyback::pb_upload(
    file = paste0("outputs/LA_pop_age_sex_2011_2024.csv"),
    repo = "blaisekelly/lsoa_ons_population",
    overwrite = TRUE,
    tag = "v0.1.1")
  
  lsoa_2021_url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BFC_V10/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
  
  lsoa_2021 <- sf::st_read(lsoa_2021_url)

