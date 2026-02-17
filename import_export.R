library(dplyr)
library(openxlsx)
library(tidyr)
library(purrr)

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
for (f in filez){
  
  # create a string of years to get the sheets for each file
  yrz = stringr::str_split(gsub(".xlsx","",f), "_",simplify = TRUE)
  yrz = seq(yrz[1],yrz[2],1)
  
  for (y in yrz){
  
    # import the sheet for the respective year and get rid of the LAD columns (first 2)
  ons_pop = read.xlsx(paste0("data/",f),
                       sheet = paste0("Mid-",y," LSOA 2021"),startRow = 4)[,-c(1,2)]
  
  # trim to totals only
  ons_tot = ons_pop |> 
    select(LSOA.2021.Code,LSOA.2021.Name, Total)
  
  # rename to neater code and name format and make total column year
  names(ons_tot) = c("lsoa21cd", "lsoa21nm", y)  
  
  # for age and sex collapse the df to make one total column
  ons_a_s = ons_pop |> 
    select(-Total) |> 
    tidyr::pivot_longer(-c("LSOA.2021.Code","LSOA.2021.Name"),names_to = "sex_age", values_to = as.character(y))
  
  # write to lists
  yr_tots[[as.character(y)]] = ons_tot
  yr_a_s[[as.character(y)]] = ons_a_s
  
  # show where up to
  print(paste(f,y))
  
  }
  
}
  
# left join all list items
  pop_tots = reduce(yr_tots, left_join, by = c("lsoa21cd", "lsoa21nm"))
  
  pop_a_s = reduce(yr_a_s, left_join, by = c("LSOA.2021.Code","LSOA.2021.Name","sex_age"))

  dir.create("outputs/")
  
  # write as single csvs
  write.csv(pop_tots, "outputs/lsoa21_pop_tot_2011_2024.csv")  
  
  write.csv(pop_a_s, "outputs/lsoa21_pop_age_sex_2011_2024.csv")

