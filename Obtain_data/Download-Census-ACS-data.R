library(tidycensus)
library(tigris)
library(dplyr)
library(yaml)
library(tidyr)
library(sf)

readRenviron("~/.Renviron")

# ## Exploring:
v16 <- load_variables(2016, "acs5", cache = TRUE)
View(v16)

v0 <- load_variables(2000, "sf3", cache = TRUE)
View(v0)

setwd("C:/Users/ellen/OneDrive/MyDocs/Graduate Research/Low-cost AQ sensor epi")

get_variables <- function(plan, var, year) {
  data_key <- names(plan[[var]])
  data_key <- data_key[year <= data_key][1]
  return(plan[[var]][[data_key]])
}

variable_path <- "Getting data/census_vars3.yml" # OR "census_vars_tracts.yml" 

variable_plan <- yaml.load_file(variable_path)

## ACS 5 year data available from 2009 forward, decennial data in 2000, 2010
data_years <- c(2016)

out <- NULL
for (year in data_years) {
  year_plan <- list()
  acs_vars <- NULL
  dec_vars <- NULL
  
  ## Figure out what to download and from where
  for (var in names(variable_plan)) {
    year_plan[[var]] <- get_variables(variable_plan, var, year)
    if (year_plan[[var]] == "skip") {
      next
    }
    if (names(year_plan[[var]]) == "acs") {
      acs_vars <- union(acs_vars, 
                        union(year_plan[[var]][["acs"]][["num"]], 
                              year_plan[[var]][["acs"]][["den"]]))
    } else if (names(year_plan[[var]]) == "census") {
      dec_vars <- union(dec_vars, 
                        union(year_plan[[var]][["census"]][["num"]], 
                              year_plan[[var]][["census"]][["den"]]))
    }
  }
  
  ## Make API calls
  if (!is.null(acs_vars)) {
    acs_data <- get_acs("tract", state = "CA", # OR "block group"
                        variables = acs_vars, year = year)
    acs_data$moe <- NULL
    acs_data <- pivot_wider(acs_data,id_cols = c(GEOID, NAME),  names_from  = variable, values_from  = estimate)
  }
  
  if (!is.null(dec_vars)) {
    if (year == 2000) {
      
      ## Need to split in to sf1 and sf3
      sf1_varlist <- load_variables(year = 2000, dataset = "sf1", cache = T)
      sf3_varlist <- load_variables(year = 2000, dataset = "sf3", cache = T)
      dec_data <- get_decennial("county", 
                                variables = intersect(setdiff(dec_vars, "P004002"),sf1_varlist$name),
                                sumfile = "sf1", 
                                year = year)
      if ("P004002" %in% dec_vars) {
        dec_data <- rbind(dec_data, 
                          get_decennial("county", 
                                        variables = "P004002",
                                        sumfile = "sf1", 
                                        year = year))
      }
      dec_data <- rbind(dec_data,
                        get_decennial("county",
                                      variables = setdiff(dec_vars, sf1_varlist$name),
                                      sumfile = "sf3",
                                      year = year))
      
    } else {
      dec_data <- get_decennial("county", variables = dec_vars, year = year)
    }
    dec_data <- pivot_wider(dec_data, id_cols = c(GEOID, NAME),  names_from  = variable, values_from  = value)
  }
  
  ## Merge/unify variable names
  if (!is.null(acs_vars) & !is.null(dec_vars)) {
    data <- inner_join(acs_data, dec_data,suffix = c("", ".y"), by = c("GEOID"))
    data$NAME.y <- NULL
    rm(acs_data, dec_data)
  } else if (!is.null(acs_vars)) {
    data <- acs_data
    rm(acs_data)
  } else if (!is.null(dec_vars)) {
    data <- dec_data
    rm(dec_data)
  }
  
  ## Use variable plan to calculate values
  
  for (var in names(year_plan)) {
    if (year_plan[[var]] == "skip") {
      data[[var]] <- NA
    } else {
      data$num <- 0
      for (source_var in year_plan[[var]][[1]][["num"]]) {
        data$num <- data$num + data[[source_var]]
      }
      
      if (!is.null(year_plan[[var]][[1]][["den"]])) {
        data$den <- 0
        for (source_var in year_plan[[var]][[1]][["den"]]) {
          data$den <- data$den + data[[source_var]]
        }
        
        data[[var]] <- data$num/data$den
        data$num <- NULL
        data$den <- NULL
      } else {
        data[[var]] <- data$num
        data$num <- NULL
      }
    }
  }
  
  data <- select(data, c("GEOID","NAME", names(year_plan)))
  data$year <- year
  
  out <- rbind(out, data)
  
  
}

## For separate variables:
write.csv(out, "Getting data/Block_non-Hispanic-white.csv", row.names=FALSE)
write.csv(out, "Getting data/Tract_non-Hispanic-white.csv", row.names=FALSE)
write.csv(out, "Getting data/Block_crowded.csv", row.names = FALSE)
write.csv(out, "Getting data/Tract_crowded.csv", row.names = FALSE)
write.csv(out, "Getting data/Tract_vars.csv", row.names = FALSE)
write.csv(out, "Getting data/Block_vars.csv", row.names = FALSE)
write.csv(out, "Getting data/Tract_vars_for_block_NAs.csv", row.names = FALSE)

## Create framework to allow missingness
# map_data <- block_groups(state = "CA") # 50 MB
# map_vars<- c("COUNTYFP", "TRACTCE", "GEOID", "INTPTLAT", "INTPTLON", "geometry")
# map_data <- as.tbl(data.frame(fips = map_data$GEOID,
#                                  NAME = map_data$NAMELSAD,
#                                  land_area = as.numeric(map_data$ALAND)/2589988,
#                                   map_data[,map_vars],
#                                  stringsAsFactors = F))
# 
# write_sf(map_data, "Getting data/CA_block_groups.shp")
map_data<- read_sf("Getting data/CA_block_groups.shp")

merged_data <- NULL
for (year in data_years) {
  map_data$year <- year
  merged_data <- rbind(merged_data, map_data)
}

out <- left_join(merged_data, out, by = c("fips" = "GEOID", "year"), suffix = c("", ".y"))
out$NAME.y <- NULL
if ("population" %in% names(variable_plan)) {
  out$population_density <- out$population/out$land_area
}

Out<- out[, which(! names(out) %in% c("poverty", "no_grad"))]

write.csv(Out, "Intermediate data/census_block-group_uninterpolated.csv", row.names = F)

## Merge with tracts:
tracts<- read.csv("Getting data/Tract_vars.csv")

Out$GEOID<- as.numeric(substr(Out$fips,1,nchar(Out$fips)-1))

All<- left_join(Out, tracts, by = c("GEOID", "year"), suffix = c("", ".y"))
All$NAME.y <- NULL

write.csv(All, "Intermediate data/CA_Census_covariates.csv", row.names = F)

summary(All) # Need to discuss how to deal with missingness...

new<- read.csv("Getting data/Tract_vars_for_block_NAs.csv")

filled<- left_join(All, new, by = c("GEOID", "year"), suffix = c("", ".y"))
Filled<- as.data.frame(filled)
Filled$NAME.y<- NULL
sapply(Filled[,c(3:17, 19:34)], function(y) summary(y)[7]) # Less NAs from tracts

my_vars<- names(All)[6:16]

All<- as.data.frame(All)

for(v in my_vars){
  All[,v]<- coalesce(as.numeric(All[,v]), Filled[,paste0(v, ".y")])
}

write.csv(All, "Intermediate data/CA_Census_covariates_less_NAs.csv", row.names = F)

## Merge with shapefile for later use:
All<- read.csv("Intermediate data/CA_Census_covariates_less_NAs.csv")

map_vars<- c("COUNTYFP", "TRACTCE", "GEOID", "INTPTLAT", "INTPTLON", "geometry")
map_data$GEOID<- as.numeric(map_data$GEOID)

SHP<- inner_join(map_data[,map_vars], All, by = c("GEOID" = "fips"), 
                 suffix = c("", ".tract"))

SHP$GEOID<- as.character(SHP$GEOID)
SHP$GEOID.tract<- as.character(SHP$GEOID.tract)

write_sf(SHP[,which(names(SHP) != "NAME")], "Intermediate data/Census_vars.shp")

shp<- read_sf("Intermediate data/Census_vars.shp")
# names(shp)<- c("COUNTYFP", "TRACTCE", "GEOID",                 
#                "INTPTLAT","INTPTLON", "geometry",               
#                "land_area", "year",                   
#                "population", "hispanic_pct", "blk_pct",                
#                "white_pct", "native_pct", "asian_pct",              
#                "median_household_income", "owner_occupied", "high_needs",             
#                "single_parent", "unemployed", "no_car",                 
#                "population_density", "GEOID.tract", "no_grad",                
#                "poverty")
