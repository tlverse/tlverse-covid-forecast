library(data.table)
library(zoo)
library(here)
library(reshape2)
library(tidyverse)

na.locf2 <- function(x) na.locf(x, na.rm = FALSE)
'%nin%' <- Negate('%in%')

################################################################################
# week 2 data
################################################################################
files <- list.files(pattern = "*.csv", path = here("Data", "week2"))
path <- here("Data/week2", files)
#path <- paste0("/tlverse-covid-forecast/Data/week2/",files)
listdata <- lapply(path, read.csv)
names(listdata) <- files

nrow(dim(listdata[[2]])) #12642 rows in test
nrow(dim(listdata[[3]])) #19404 rows in training

# make some of the Province_State seperate regions
listdata[[2]]$region <- listdata[[2]]$Country_Region
listdata[[2]]$region <- ifelse(listdata[[2]]$Province_State == "St Martin",
                               "St Martin", as.character(listdata[[2]]$region))
listdata[[2]]$region <- ifelse(listdata[[2]]$Province_State == "Virgin Islands",
                               "Virgin Islands (U.S.)", as.character(listdata[[2]]$region))
listdata[[2]]$region <- ifelse(listdata[[2]]$Province_State == "Sint Maarten",
                               "Sint Maarten", as.character(listdata[[2]]$region))
listdata[[2]]$region <- ifelse(listdata[[2]]$Province_State == "Puerto Rico",
                               "Puerto Rico", as.character(listdata[[2]]$region))
listdata[[2]]$region <- ifelse(listdata[[2]]$Province_State == "Aruba",
                               "Aruba", as.character(listdata[[2]]$region))
listdata[[2]]$region <- ifelse(listdata[[2]]$Province_State == "Bermuda",
                               "Bermuda", as.character(listdata[[2]]$region))
listdata[[2]]$region <- ifelse(listdata[[2]]$Province_State == "Cayman Islands",
                               "Cayman Islands", as.character(listdata[[2]]$region))
listdata[[2]]$region <- ifelse(listdata[[2]]$Province_State == "Curacao",
                               "Curacao", as.character(listdata[[2]]$region))
listdata[[2]]$region <- ifelse(listdata[[2]]$Province_State == "Saint Barthelemy",
                               "Saint Barthelemy", as.character(listdata[[2]]$region))
listdata[[2]]$region <- ifelse(listdata[[2]]$Province_State == "Macau",
                               "Macau", as.character(listdata[[2]]$region))
listdata[[2]]$region <- ifelse(listdata[[2]]$Province_State == "Hong Kong",
                               "Hong Kong", as.character(listdata[[2]]$region))
listdata[[2]]$region <- ifelse(listdata[[2]]$Province_State == "Faroe Islands",
                               "Faroe Islands", as.character(listdata[[2]]$region))
listdata[[2]]$region <- ifelse(listdata[[2]]$Province_State == "Greenland",
                               "Greenland", as.character(listdata[[2]]$region))
listdata[[2]]$region <- ifelse(listdata[[2]]$Province_State == "French Guiana",
                               "French Guiana", as.character(listdata[[2]]$region))
listdata[[2]]$region <- ifelse(listdata[[2]]$Province_State == "French Polynesia",
                               "French Polynesia", as.character(listdata[[2]]$region))
listdata[[2]]$region <- ifelse(listdata[[2]]$Province_State == "Guadeloupe",
                               "Guadeloupe", as.character(listdata[[2]]$region))
listdata[[2]]$region <- ifelse(listdata[[2]]$Province_State == "Martinique",
                               "Martinique", as.character(listdata[[2]]$region))
listdata[[2]]$region <- ifelse(listdata[[2]]$Province_State == "Mayotte",
                               "Mayotte", as.character(listdata[[2]]$region))
listdata[[2]]$region <- ifelse(listdata[[2]]$Province_State == "New Caledonia",
                               "New Caledonia", as.character(listdata[[2]]$region))
listdata[[2]]$region <- ifelse(listdata[[2]]$Province_State == "Aruba",
                               "Aruba", as.character(listdata[[2]]$region))
listdata[[2]]$region <- ifelse(listdata[[2]]$Province_State == "Guam", "Guam", 
                               as.character(listdata[[2]]$region))
listdata[[2]]$region <- ifelse(listdata[[2]]$Province_State == "Gibraltar", 
                               "Gibraltar", as.character(listdata[[2]]$region))
listdata[[2]]$region <- ifelse(listdata[[2]]$Province_State == "Isle of Man", 
                               "Isle of Man", as.character(listdata[[2]]$region))
listdata[[2]]$region <- ifelse(listdata[[2]]$Province_State == "Montserrat", 
                               "Montserrat", as.character(listdata[[2]]$region))
listdata[[3]] <- merge(listdata[[3]], listdata[[2]][,c(2:3,5)], 
                       by = c("Province_State", "Country_Region"), all.x = TRUE)

train_regions <- listdata[[3]]$region

################################################################################
# country codes
################################################################################
urlfile <- "https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv"
country_codes <-read_csv(url(urlfile))

# add Kosovo
k <- c("Kosovo", "XK", "XKX", NA, NA, "Europe", "Southern Europe", NA, NA, NA, NA)
names(k) <- names(country_codes)
countrycodes <- rbind(country_codes, k)
countrycodes <- dplyr::arrange(countrycodes, name)

# match country names of the codes to country names in training/test data
missing <- train_regions[which(train_regions %nin% countrycodes$name)]
countrycodes$name <- ifelse(countrycodes$name == "Bolivia (Plurinational State of)",
                            "Bolivia", as.character(countrycodes$name))
countrycodes$name <- ifelse(countrycodes$name == "Brunei Darussalam", "Brunei", 
                            as.character(countrycodes$name))
countrycodes$name <- ifelse(countrycodes$name == "Congo", "Congo (Brazzaville)", 
                            as.character(countrycodes$name))
countrycodes$name <- ifelse(countrycodes$name == "Congo, Democratic Republic of the", 
                            "Congo (Kinshasa)", as.character(countrycodes$name))
countrycodes$name <- ifelse(countrycodes$name == "Côte d'Ivoire", "Cote d'Ivoire", 
                            as.character(countrycodes$name))
countrycodes$name <- ifelse(countrycodes$name == "Iran (Islamic Republic of)", 
                            "Iran", as.character(countrycodes$name))
countrycodes$name <- ifelse(countrycodes$name == "Korea, Republic of", 
                            "Korea, South", as.character(countrycodes$name))
countrycodes$name <- ifelse(countrycodes$name == "Lao People's Democratic Republic", 
                            "Laos", as.character(countrycodes$name))
countrycodes$name <- ifelse(countrycodes$name == "Moldova, Republic of", 
                            "Moldova", as.character(countrycodes$name))
countrycodes$name <- ifelse(countrycodes$name == "Russian Federation", 
                            "Russia", as.character(countrycodes$name))
countrycodes$name <- ifelse(countrycodes$name == "Syrian Arab Republic", 
                            "Syria", as.character(countrycodes$name))
countrycodes$name <- ifelse(countrycodes$name == "Taiwan, Province of China", 
                            "Taiwan*", as.character(countrycodes$name))
countrycodes$name <- ifelse(countrycodes$name == "Tanzania, United Republic of", 
                            "Tanzania", as.character(countrycodes$name))
countrycodes$name <- ifelse(countrycodes$name == "United States of America", 
                            "US", as.character(countrycodes$name))
countrycodes$name <- ifelse(countrycodes$name == "United Kingdom of Great Britain and Northern Ireland", 
                            "United Kingdom", as.character(countrycodes$name))
countrycodes$name <- ifelse(countrycodes$name == "Venezuela (Bolivarian Republic of)", 
                            "Venezuela", as.character(countrycodes$name))
countrycodes$name <- ifelse(countrycodes$name == "Viet Nam", "Vietnam", 
                            as.character(countrycodes$name))
countrycodes$name <- ifelse(countrycodes$name == "Macao", "Macau", 
                            as.character(countrycodes$name))
countrycodes$name <- ifelse(countrycodes$name == "Curaçao", "Curacao",
                            as.character(countrycodes$name))
countrycodes$name <- ifelse(countrycodes$name == "Saint Barthélemy", 
                            "Saint Barthelemy", as.character(countrycodes$name))
countrycodes$name <- ifelse(countrycodes$name == "Saint Martin (French part)", 
                            "Sint Maarten", as.character(countrycodes$name))      
countrycodes$name <- ifelse(countrycodes$name == "Sint Maarten (Dutch part)", 
                            "St Martin", as.character(countrycodes$name))       
unique(train_regions[which(train_regions %nin% countrycodes$name)]) # Diamond Princess
write.csv(countrycodes, file = here("Data", "countrycodes.csv"), row.names = FALSE)

# add country_code to test and train data
country_codes <- read.csv(file = here("Data", "countrycodes.csv"))
colnames(country_codes)[c(1,3)] <- c("region", "country_code")
test <- listdata[[2]] 
train <- listdata[[3]]
test <- merge(test, country_codes[,c(1,3)], by = "region", all.x = TRUE)
train <- merge(train, country_codes[,c(1,3)], by = "region", all.x = TRUE)
train <- unique(train)
# prep country_codes for merge with covariates
colnames(country_codes)[c(1,3)] <- c("country", "country_code")

################################################################################
# baseline covariate data
################################################################################
files <- list.files(pattern = "*.csv", 
                    path = here("Data", "covid19-global-forecasting-week-1"))
path <- here("Data/covid19-global-forecasting-week-1/", files)
#path <- paste0("~/tlverse-covid-forecast/Data/covid19-global-forecasting-week-1/",files)
listdata <- lapply(path, read.csv)
names(listdata) <- files

################### identify rows which are not countries ######################
countries <- listdata[[1]][,c(1:2)]
colnames(countries) <- c("country", "country_code")
countries <- merge(countries, country_codes[,c(1,3)], by = "country_code", all.x = TRUE)
not_countries <- countries[c(which(is.na(countries$country.y))),c(1:2)]
not_countries <- droplevels(not_countries)

######################## listdata[[1]] = air passengers ########################
dat <- listdata[[1]][,-c(1, 3:14)]
colnames(dat)[1] <- c("country_code")
dat <- dat[-which(dat$country_code %in% not_countries$country_code), ]
colnames(dat)[-1] <- substring(names(dat)[-1], 2) # remove X from e.g. X1970
datmelt <- melt(dat, "country_code")
datmelt$missing <- ifelse(is.na(datmelt$value), 1, 0)
datclean <- data.table(datmelt)[, na.locf2(.SD), by = country_code]
datclean$year <- ifelse(datclean$missing == 1, NA, 
                        as.numeric(as.character(datclean$variable)))
datclean <- datclean[, na.locf2(.SD), by = country_code]
datfinal <- datclean %>% 
  filter(variable == 2019) %>% 
  select(c(country_code, year, value))
colnames(datfinal)[2:3] <- c("air_year", "air_passengers")
datfinal <- merge(datfinal, country_codes[,c(1,3)], by = "country_code", 
                  all.x = TRUE)
sum(is.na(datfinal$country_code))
listdata[[1]] <- datfinal

########################### listdata[[3]] = economy ############################
dat <- listdata[[3]][,-c(1, 3:4, 25)]
colnames(dat)[1] <- "country"
dat$country <- ifelse(dat$country == "Brunei Darussalam", "Brunei", 
                      as.character(dat$country))
dat$country <- ifelse(dat$country == "Burma", "Myanmar", as.character(dat$country))
dat$country <- ifelse(dat$country == "C\xf4te d'Ivoire", "Cote d'Ivoire", 
                      as.character(dat$country))
dat$country <- ifelse(dat$country == "Congo, Democratic Republic of the Congo", 
                      "Congo (Kinshasa)", as.character(dat$country))
dat$country <- ifelse(dat$country == "Congo, Republic of", "Congo (Brazzaville)", 
                      as.character(dat$country))
dat$country <- ifelse(dat$country == "Kyrgyz Republic", "Kyrgyzstan", 
                      as.character(dat$country))
dat$country <- ifelse(dat$country == "S\xe3o Tom\xe9 and Pr\xedncipe", 
                      "Sao Tome and Principe", as.character(dat$country))
dat$country <- ifelse(dat$country == "Czech Republic", "Czechia", 
                      as.character(dat$country))
dat$country <- ifelse(dat$country == "Korea, North ", 
                      "Korea (Democratic People's Republic of)", 
                      as.character(dat$country))
dat$country <- ifelse(dat$country == "Kyrgyz Republic", "Kyrgyzstan", 
                      as.character(dat$country))
dat$country <- ifelse(dat$country == "Macedonia", "North Macedonia", 
                      as.character(dat$country))
dat$country <- ifelse(dat$country == "Taiwan ", "Taiwan*", 
                      as.character(dat$country))
dat$country <- ifelse(dat$country == "Tanzania, United Republic of", "Tanzania", 
                      as.character(dat$country))
dat$country <- ifelse(dat$country == "United States", "US", 
                      as.character(dat$country))
colnames(dat) <- c("country", "econ_worldrank", "econ_regionrank", 
                   "econ_2019score", "econ_propertyrights", "econ_judical",
                   "econ_gov_integrity", "econ_taxburden", "econ_govspending",
                   "econ_fiscalhealth", "econ_businessfreedom", 
                   "econ_laborfreedom", "econ_monetaryfreedom", 
                   "econ_tradefreedom", "econ_investmentfreedom", 
                   "econ_financialfreedom", "econ_tariffrate", 
                   "econ_incometaxrate", "econ_corporatetaxrate",
                   "econ_taxburdengdp", "econ_govexpendgdp", "econ_popmills",
                   "econ_gdpbills", "econ_gdpgrowthrate", 
                   "econ_yeargdpgrowthrate", "econ_gdppercapita", 
                   "econ_unemployment", "econ_inflation", "econ_fdiflowmills",
                   "econ_publicdeptofgdp")
datfinal <- merge(dat, country_codes[,c(1,3)], by = "country", all.x = TRUE)
dat_odd <- datfinal[which(is.na(datfinal$country_code)),]
dat_odd$country # "Micronesia" 
datfinal <- datfinal[-which(is.na(datfinal$country_code)),]
listdata[[3]] <- datfinal

##### listdata[[4]] = gdp_crime_data (already in Data/covid_countryinfo.csv) ###
dat <- listdata[[4]]
colnames(dat) <- c("country", "gdp_2018", "crime_index", "population2020", 
                   "smoking2016", "females2018")
dat$country <- ifelse(dat$country == "Czech Republic", "Czechia", 
                      as.character(dat$country))
dat$country <- ifelse(dat$country == "South Korea", "Korea, South", 
                      as.character(dat$country))
dat$country <- ifelse(dat$country == "United States", "US", 
                      as.character(dat$country))
datfinal <- merge(dat, country_codes[,c(1,3)], by = "country", all.x = TRUE)
sum(is.na(datfinal$country_code))
listdata[[4]] <- datfinal

################## listdata[[5]] = number of hospital beds #####################
dat <- listdata[[5]][,-c(1, 3:4)]
colnames(dat)[1] <- c("country_code")
dat <- dat[-which(dat$country_code %in% not_countries$country_code), ]
colnames(dat)[-1] <- substring(names(dat)[-1], 2) # remove X from e.g. X1970
datmelt <- melt(dat, "country_code")
datmelt$missing <- ifelse(is.na(datmelt$value), 1, 0)
datclean <- data.table(datmelt)[, na.locf2(.SD), by = country_code]
datclean$year <- ifelse(datclean$missing == 1, NA, 
                        as.numeric(as.character(datclean$variable)))
datclean <- datclean[, na.locf2(.SD), by = country_code]
datfinal <- datclean %>% 
  filter(variable == 2017) %>% 
  select(c(country_code, year, value))
colnames(datfinal)[2:3] <- c("hospbeds_year", "hospbeds_per1k")
datfinal <- merge(datfinal, country_codes[,c(1,3)], by = "country_code", 
                  all.x = TRUE)
sum(is.na(datfinal$country_code))
listdata[[5]] <- datfinal

######################## listdata[[6]] = number of doctors #####################
dat <- listdata[[6]][,-c(1, 3:4)]
colnames(dat)[1] <- c("country_code")
dat <- dat[-which(dat$country_code %in% not_countries$country_code), ]
colnames(dat)[-1] <- substring(names(dat)[-1], 2) # remove X from e.g. X1970
datmelt <- melt(dat, "country_code")
datmelt$missing <- ifelse(is.na(datmelt$value), 1, 0)
datclean <- data.table(datmelt)[, na.locf2(.SD), by = country_code]
datclean$year <- ifelse(datclean$missing == 1, NA, 
                        as.numeric(as.character(datclean$variable)))
datclean <- datclean[, na.locf2(.SD), by = country_code]
datfinal <- datclean %>% 
  filter(variable == 2019) %>% 
  select(c(country_code, year, value))
colnames(datfinal)[2:3] <- c("docs_year", "docs_per1k")
datfinal <- merge(datfinal, country_codes[,c(1,3)], by = "country_code", 
                  all.x = TRUE)
sum(is.na(datfinal$country_code))
listdata[[6]] <- datfinal

######################### listdata[[7]] = pollution ############################
dat <- listdata[[7]][,-1]
colnames(dat)[1] <- c("country_code")
dat <- dat[-which(dat$country_code %in% not_countries$country_code), ]
colnames(dat)[-1] <- substring(names(dat)[-1], 2) # remove X from e.g. X1970
colnames(dat)[-1] <- paste0("pollution_", colnames(dat)[-1]) 
datfinal <- merge(dat, country_codes[,c(1,3)], by = "country_code", 
                  all.x = TRUE)
sum(is.na(datfinal$country_code))
listdata[[7]] <- datfinal

######################## listdata[[8]] = population +65 ########################
dat <- listdata[[8]][,-c(1, 3:4)]
colnames(dat)[1] <- c("country_code")
dat <- dat[-which(dat$country_code %in% not_countries$country_code), ]
colnames(dat)[-1] <- substring(names(dat)[-1], 2) # remove X from e.g. X1970
datmelt <- melt(dat, "country_code")
datmelt$missing <- ifelse(is.na(datmelt$value), 1, 0)
datclean <- data.table(datmelt)[, na.locf2(.SD), by = country_code]
datclean$year <- ifelse(datclean$missing == 1, NA, 
                        as.numeric(as.character(datclean$variable)))
datclean <- datclean[, na.locf2(.SD), by = country_code]
datfinal <- datclean %>% 
  filter(variable == 2019) %>% 
  select(c(country_code, year, value))
colnames(datfinal)[2:3] <- c("pop65above_year", "pop65above_percent")
datfinal <- merge(datfinal, country_codes[,c(1,3)], by = "country_code", 
                  all.x = TRUE)
sum(is.na(datfinal$country_code))
listdata[[8]] <- datfinal

########## listdata[[9]] = port data (insufficient for global merge) ###########

###################### listdata[[10]] = prison data ############################
dat <- listdata[[10]][,-c(1:2, 7)]
colnames(dat)[1] <- c("country")
dat1 <- dat[,1:3]
dat1 <- dcast(data = dat1, formula = country~Year, value.var = "Count")
datmelt <- melt(dat1, "country")
datmelt$missing <- ifelse(is.na(datmelt$value), 1, 0)
datclean <- data.table(datmelt)[, na.locf2(.SD), by = country]
datclean$year <- ifelse(datclean$missing == 1, NA, 
                        as.numeric(as.character(datclean$variable)))
datclean <- datclean[, na.locf2(.SD), by = country]
datfinal1 <- datclean %>% 
  filter(variable == 2017) %>% 
  select(c(country, year, value))
colnames(datfinal1)[2:3] <- c("prisoncount_year", "prison_count")

dat2 <- dat[,c(1:2,4)]
dat2 <- dcast(data = dat2, formula = country~Year, value.var = "Rate")
datmelt <- melt(dat2, "country")
datmelt$missing <- ifelse(is.na(datmelt$value), 1, 0)
datclean <- data.table(datmelt)[, na.locf2(.SD), by = country]
datclean$year <- ifelse(datclean$missing == 1, NA, 
                        as.numeric(as.character(datclean$variable)))
datclean <- datclean[, na.locf2(.SD), by = country]
datfinal2 <- datclean %>% 
  filter(variable == 2017) %>% 
  select(c(country, year, value))
colnames(datfinal2)[2:3] <- c("prisonrate_year", "prison_rate")

datfinal <- merge(datfinal1, datfinal2, by = "country")

datfinal$country <- ifelse(datfinal$country == "Bolivia (Plurinational State of)",
                           "Bolivia", as.character(datfinal$country))
datfinal$country <- ifelse(datfinal$name == "Brunei Darussalam", "Brunei", 
                           as.character(datfinal$country))
datfinal$country <- ifelse(datfinal$name == "British Virgin Islands", 
                           "Virgin Islands (British)", as.character(datfinal$country))
datfinal$country <- ifelse(datfinal$country == "Congo", "Congo (Brazzaville)", 
                           as.character(datfinal$country))
datfinal$country <- ifelse(datfinal$country == "Democratic Republic of the Congo", 
                           "Congo (Kinshasa)", as.character(datfinal$country))
datfinal$country <- ifelse(datfinal$country == "Côte d'Ivoire", "Cote d'Ivoire", 
                           as.character(datfinal$country))
datfinal$country <- ifelse(datfinal$country == "Iran (Islamic Republic of)", 
                           "Iran", as.character(datfinal$country))
datfinal$country <- ifelse(datfinal$country == "Republic of Korea", 
                           "Korea, South", as.character(datfinal$country))
datfinal$country <- ifelse(datfinal$country == "Lao People's Democratic Republic", 
                           "Laos", as.character(datfinal$country))
datfinal$country <- ifelse(datfinal$country == "Republic of Moldova", 
                           "Moldova", as.character(datfinal$country))
datfinal$country <- ifelse(datfinal$country == "Russian Federation", 
                           "Russia", as.character(datfinal$country))
datfinal$country <- ifelse(datfinal$country == "Syrian Arab Republic", 
                           "Syria", as.character(datfinal$country))
datfinal$country <- ifelse(datfinal$country == "China, Taiwan Province of China", 
                           "Taiwan*", as.character(datfinal$country))
datfinal$country <- ifelse(datfinal$country == "United Republic of Tanzania", 
                           "Tanzania", as.character(datfinal$country))
datfinal$country <- ifelse(datfinal$country == "United States of America", 
                           "US", as.character(datfinal$country))
datfinal$country <- ifelse(datfinal$country == "Venezuela (Bolivarian Republic of)", 
                           "Venezuela", as.character(datfinal$country))
datfinal$country <- ifelse(datfinal$country == "Viet Nam", "Vietnam", 
                           as.character(datfinal$country))
datfinal$country <- ifelse(datfinal$country == "Macao Special Administrative Region of China", 
                           "Macau", as.character(datfinal$country))
datfinal$country <- ifelse(datfinal$country == "Curaçao", "Curacao",
                           as.character(datfinal$country))
datfinal$country <- ifelse(datfinal$country == "Saint Barthélemy", 
                           "Saint Barthelemy", as.character(datfinal$country))
datfinal$country <- ifelse(datfinal$country == "Saint Martin (French part)", 
                           "Sint Maarten", as.character(datfinal$country))      
datfinal$country <- ifelse(datfinal$country == "Sint Maarten (Dutch part)", 
                           "St Martin", as.character(datfinal$country)) 
datfinal$country <- ifelse(datfinal$country == "Micronesia (Federate States of)", 
                           "Micronesia", as.character(datfinal$country)) 
datfinal$country <- ifelse(datfinal$country == "Kosovo under UNSCR 1244", 
                           "Kosovo", as.character(datfinal$country)) 
datfinal$country <- ifelse(datfinal$country == "China, Hong Kong Special Administrative Region", 
                           "Hong Kong", as.character(datfinal$country)) 
datfinal$country <- ifelse(datfinal$country == "Iraq (Central Iraq)", "Iraq", 
                           as.character(datfinal$country))
datfinal$country <- ifelse(datfinal$country == "United States Virgin Islands", 
                           "Virgin Islands (U.S.)", as.character(datfinal$country))
datfinal$country <- ifelse(datfinal$country == "State of Palestine", 
                           "Palestine, State of", as.character(datfinal$country))
datfinal$country <- ifelse(datfinal$country == "Czech republic", 
                           "Czechia", as.character(datfinal$country))
datfinal$country <- ifelse(datfinal$country == "Cura\x8dao", 
                           "Curacao", as.character(datfinal$country))
datfinal <- merge(datfinal, country_codes[,c(1,3)], by = "country", all.x = TRUE)
datfinal$prison_count <- as.numeric(gsub(",","",datfinal$prison_count))
datfinal$prison_rate <- as.numeric(datfinal$prison_rate)
datfinal_fix <- datfinal[which(is.na(datfinal$country_code)),]
datfinal_fix_britain <- datfinal_fix[6:8,]
datfinal_fix_brit <- c(country = "United Kingdom", prisoncount_year = 2017, 
                       prison_count = sum(datfinal_fix_britain$prison_count),
                       prisonrate_year = 2017, 
                       prison_rate = sum(datfinal_fix_britain$prison_rate),
                       country_code = "GBR")
datfinal_fix <- rbind(datfinal_fix[-c(4:8),], datfinal_fix_brit)
datfinal_fix$country_code <-c("VGB", "BRN", "CIV", "GBR")
dfinal <- rbind(datfinal_fix, datfinal[-which(is.na(datfinal$country_code)),])[,-1]
datfinal <- merge(dfinal, country_codes[,c(1,3)], by = "country_code", all.x = TRUE)
sum(is.na(datfinal$country_code))
listdata[[10]] <- datfinal

########################## listdata[[11]] = railways data ######################
dat <- listdata[[11]][,-c(1, 3:4)]
colnames(dat)[1] <- c("country_code")
dat <- dat[-which(dat$country_code %in% not_countries$country_code), ]
colnames(dat)[-1] <- substring(names(dat)[-1], 2) # remove X from e.g. X1970
datmelt <- melt(dat, "country_code")
datmelt$missing <- ifelse(is.na(datmelt$value), 1, 0)
datclean <- data.table(datmelt)[, na.locf2(.SD), by = country_code]
datclean$year <- ifelse(datclean$missing == 1, NA, 
                        as.numeric(as.character(datclean$variable)))
datclean <- datclean[, na.locf2(.SD), by = country_code]
datfinal <- datclean %>% 
  filter(variable == 2019) %>% 
  select(c(country_code, year, value))
colnames(datfinal)[2:3] <- c("rail_year", "rail_millionpassengerkm")
datfinal <- merge(datfinal, country_codes[,c(1,3)], by = "country_code", 
                  all.x = TRUE)
sum(is.na(datfinal$country_code))
listdata[[11]] <- datfinal

####### listdata[[12]] = restaurant data (insufficient for global merge) #######

############################# listdata[[14]] = SARS data #######################
dat <- listdata[[14]]
colnames(dat)[2] <- c("country")
dat$country <- ifelse(dat$country == "Hong Kong SAR, China", "Hong Kong", 
                      as.character(dat$country))
dat$country <- ifelse(dat$country == "Taiwan, China", "Taiwan, Province of China", 
                      as.character(dat$country))
dat$country <- ifelse(dat$country == "Republic of Ireland", "Ireland", 
                      as.character(dat$country))
dat$country <- ifelse(dat$country == "Republic of Korea", "Korea, South", 
                      as.character(dat$country))
dat$country <- ifelse(dat$country == "Russian Federation", "Russia", 
                      as.character(dat$country))
dat$country <- ifelse(dat$country == "Macao SAR, China", "Macau", 
                      as.character(dat$country))
dat$country <- ifelse(dat$country =="Taiwan, Province of China", "Taiwan*", 
                      as.character(dat$country))
dat$country <- ifelse(dat$country == "United States", 
                      "US", as.character(dat$country))
dat$country <- ifelse(dat$country == "Viet Nam", "Vietnam", as.character(dat$country))
datfinal <- dat[,-1] %>%
  group_by(country) %>%
  summarize_all(~sum(.))
colnames(datfinal)[2:4] <- c("sars_cases", "sars_deaths", "sars_recovered")
datfinal <- merge(datfinal, country_codes[,c(1,3)], by = "country", all.x = TRUE)
sum(is.na(datfinal$country_code))
listdata[[14]] <- datfinal

############################## put it all together #############################
baselinelist <- list(df1 = data.frame(listdata[[1]], stringsAsFactors = F), 
                     df2 = data.frame(listdata[[3]], stringsAsFactors = F),
                     df3 = data.frame(listdata[[4]], stringsAsFactors = F),
                     df4 = data.frame(listdata[[5]], stringsAsFactors = F),
                     df5 = data.frame(listdata[[6]], stringsAsFactors = F),
                     df6 = data.frame(listdata[[7]], stringsAsFactors = F),
                     df7 = data.frame(listdata[[8]], stringsAsFactors = F),
                     df8 = data.frame(listdata[[10]], stringsAsFactors = F),
                     df9 = data.frame(listdata[[11]], stringsAsFactors = F),
                     df10 = data.frame(listdata[[14]], stringsAsFactors = F))
baseline_data <- Reduce(function(...) merge(..., by = c("country_code","country"), all = TRUE), 
                        baselinelist)
baseline_data$sars_cases <- ifelse(is.na(baseline_data$sars_cases), 0,
                                   baseline_data$sars_cases)
baseline_data$sars_deaths <- ifelse(is.na(baseline_data$sars_deaths), 0,
                                    baseline_data$sars_deaths)
baseline_data$sars_recovered <- ifelse(is.na(baseline_data$sars_recovered), 0,
                                       baseline_data$sars_recovered)
write.csv(baseline_data, file = here("Data", "baselinecovs.csv"), row.names = F)


################################################################################
# time-varying covariate data 
################################################################################

################# listdata[[2]] = covid_impact_education #######################
dat <- listdata[[2]]
all.equal(dat$Date, dat$Date.1) # TRUE
dat <- dat[,c(2:5)]
colnames(dat) <- c("country_code", "country", "edu_scale", "edu_date")
dat$date_length <- sapply(dat$edu_date, stringi::stri_length)
dat1 <- filter(dat, date_length == 10)
dat2 <- filter(dat, date_length != 10)
dat1$edu_date <- as.Date(dat1$edu_date, "%d/%m/%Y") 
dat2$edu_date <- as.Date(dat2$edu_date, "%d/%m/%y")
dat <- rbind(dat1, dat2)[,-5]
dat$country_code <- ifelse(dat$country == "China, Macao Special Administrative Region", 
                           "MAC", as.character(dat$country_code))
dat$country_code <- ifelse(dat$country == "China, Hong Kong Special Administrative Region", 
                           "HKG", as.character(dat$country_code))
datfinal <- merge(dat[,-2], country_codes[,c(1,3)], by = "country_code", all.x = TRUE)
sum(is.na(datfinal$country_code))
colnames(datfinal)[4] <- "region"
datfinal_localized <- filter(datfinal, edu_scale == "Localized")
colnames(datfinal_localized)[3] <- "edu_date_localized"
datfinal_localized$edu_date_national <- rep(NA, nrow(datfinal_localized))
datfinal_national <- filter(datfinal, edu_scale == "National")
colnames(datfinal_national)[3] <- "edu_date_national"
datfinal_national$edu_date_localized <- rep(NA, nrow(datfinal_national))
datfinal_national$edu_date_localized <- as.Date(datfinal_national$edu_date_localized)
datfinal2 <- rbind(datfinal_national, datfinal_localized)
listdata[[2]] <- datfinal2[,-2]

#################### listdata[[13]] = restrictions_info_data ###################
dat <- listdata[[13]]
dat <- dat[,colSums(is.na(dat))<nrow(dat)]
dat$restrictions_date <- as.Date(dat$restrictions_date)
dat$schools_national_date <- as.Date(dat$schools_national_date)
dat$schools_localized_date <- as.Date(dat$schools_localized_date)
dat$quarantine_date <- as.Date(dat$quarantine_date)
dat$country <- ifelse(dat$country == "China Hong Kong", "Hong Kong",
                      as.character(dat$country))
dat$country <- ifelse(dat$country == "Republic of the Congo", 
                      "Congo (Brazzaville)", as.character(dat$country))
dat$country <- ifelse(dat$country == "The Bahamas", "Bahamas",
                      as.character(dat$country))
dat$country <- ifelse(dat$country == "The Gambia", "Gambia",
                      as.character(dat$country))
dat$country <- ifelse(dat$country == "Washington DC", 
                      "District of Columbia", as.character(dat$country))
dat$country <- ifelse(dat$country == "Taiwan", "Taiwan*", 
                      as.character(dat$country))
datfinal <- merge(dat, country_codes[,c(1,3)], by = "country", all.x = TRUE)
sum(is.na(datfinal$country_code))

datfix <- datfinal[which(is.na(datfinal$country_code)),]

datfix$country <- ifelse(datfix$country == "GeorgiaUS", "Georgia", 
                         as.character(datfix$country))
us <- dplyr::filter(test, region == "US")
us_states <- unique(us$Province_State)
us_restrictions <- datfix[which(datfix$country %in% us_states),]
us_restrictions$country_code <- rep("USA", nrow(us_restrictions))
us_restrictions$region <- rep("US", nrow(us_restrictions))
us_restrictions$Province_State <- us_restrictions$country
us_restrictions$Country_Region <- rep("US", nrow(us_restrictions))
us_all <- filter(datfinal, country == "US")[,c(2:7,14:25)]
us_restrictions <- us_restrictions[,c(8:13,26:29)]
us_res <- cbind.data.frame(us_restrictions, us_all)

other_restrictions <- datfix[which(datfix$country %nin% us_states),]

region_res <- datfinal[-which(is.na(datfinal$country_code)),]
notus_res <- filter(region_res, country != "US")
colnames(notus_res)[1] <- "region"
notus_res$country_code <- as.character(notus_res$country_code)
dups <- notus_res[duplicated(notus_res$country_code),]$country_code
notus_res2 <- filter(notus_res, country_code %nin% dups)
dups <- filter(notus_res, country_code %in% dups)
nodups <- dups[c(2:3),]
notus_res <- rbind(notus_res2, nodups)

################################################################################
# put it all together
################################################################################

# combine test and train for now to avoid seperate merging of covariates
train$Date <- as.Date(train$Date)
test$Date <- as.Date(test$Date)
test$ConfirmedCases <- rep(NA, nrow(test))
test$Fatalities <- rep(NA, nrow(test))
test$Id <- rep(NA, nrow(test))
train$ForecastId <- rep(NA, nrow(train))
main <- rbind(train, test)

# make sure we have same test and train data at end
nrow(main) #32046
nrow(test) #12642
nrow(train) #19404

######################## merge time-varying covariates #########################
main_us <- filter(main, region == "US") 
main_us <- merge(main_us, us_res, by = c("Province_State", "Country_Region", 
                                         "region", "country_code"), all.x = TRUE)
main_us2 <- main_us %>%
  group_by(Province_State) %>%
  mutate(days_quarantine = ifelse(
    (is.na(quarantine_date) | (Date < min(quarantine_date))) , 0,
    Date - min(quarantine_date))) %>%
  mutate(quarantine = ifelse(
    (is.na(quarantine_date) | (Date <= min(quarantine_date))) , 0, 1)) %>%
  mutate(days_restrictions = ifelse(
    (is.na(restrictions_date) | (Date < min(restrictions_date))) , 0,
    Date - min(restrictions_date))) %>%
  mutate(restrictions = ifelse(
    (is.na(restrictions_date) | (Date <= min(restrictions_date))) , 0, 1)) %>%
  mutate(days_schools_national = ifelse(
    (is.na(schools_national_date) | (Date < min(schools_national_date))) , 0,
    Date - min(schools_national_date))) %>%
  mutate(schools_national = ifelse(
    (is.na(schools_national_date) | (Date <= min(schools_national_date))) , 0, 1)) %>%
  mutate(days_schools_localized = ifelse(
    (is.na(schools_localized_date) | (Date < min(schools_localized_date))) , 0,
    Date - min(schools_localized_date))) %>%
  mutate(schools_localized = ifelse(
    (is.na(schools_localized_date) | (Date <= min(schools_localized_date))) , 0, 1))

main_notus <- filter(main, region != "US") #26487
main_notus <- merge(main_notus, notus_res, by = c("region", "country_code"), 
                    all.x = TRUE)
main_notus2 <- main_notus %>%
  group_by(region) %>%
  mutate(days_quarantine = ifelse(
    (is.na(quarantine_date) | (Date < min(quarantine_date))) , 0,
    Date - min(quarantine_date))) %>%
  mutate(quarantine = ifelse(
    (is.na(quarantine_date) | (Date <= min(quarantine_date))) , 0, 1)) %>%
  mutate(days_restrictions = ifelse(
    (is.na(restrictions_date) | (Date < min(restrictions_date))) , 0,
    Date - min(restrictions_date))) %>%
  mutate(restrictions = ifelse(
    (is.na(restrictions_date) | (Date <= min(restrictions_date))) , 0, 1)) %>%
  mutate(days_schools_national = ifelse(
    (is.na(schools_national_date) | (Date < min(schools_national_date))) , 0,
    Date - min(schools_national_date))) %>%
  mutate(schools_national = ifelse(
    (is.na(schools_national_date) | (Date <= min(schools_national_date))) , 0, 1)) %>%
  mutate(days_schools_localized = ifelse(
    (is.na(schools_localized_date) | (Date < min(schools_localized_date))) , 0,
    Date - min(schools_localized_date))) %>%
  mutate(schools_localized = ifelse(
    (is.na(schools_localized_date) | (Date <= min(schools_localized_date))) , 0, 1))

main2 <- rbind(main_notus2, main_us2)
# for countries with no educational data, merge other educational data source
main3_noedu <- filter(main2, (is.na(schools_national_date) & is.na(schools_localized_date)))
noedu_regions <- unique(main3_noedu$region)
dat <- listdata[[2]][which(listdata[[2]]$region %in% noedu_regions),]
dat2 <- dat %>%
  group_by(region) %>%
  mutate(educ_date_national = max(edu_date_national)) %>%
  mutate(educ_date_localized = max(edu_date_localized))
dat3 <- unique(dat2[,c(1,3,5:6)])
main3_noedu_merged <- merge(main3_noedu, dat3, by = c("country_code", "region"), 
                            all.x = TRUE)
main3_noedu_merged <- main3_noedu_merged %>%
  group_by(region) %>%
  mutate(days_schools_national = ifelse(
    (is.na(educ_date_national) | (Date < educ_date_national)) , 0,
    Date - educ_date_national)) %>%
  mutate(schools_national = ifelse(
    (is.na(educ_date_national) | (Date <= educ_date_national)) , 0, 1)) %>%
  mutate(days_schools_localized = ifelse(
    (is.na(educ_date_localized) | (Date < educ_date_localized)) , 0,
    Date - educ_date_localized)) %>%
  mutate(schools_national = ifelse(
    (is.na(educ_date_localized) | (Date <= educ_date_localized)) , 0, 1))
main3_noedu_merged <- main3_noedu_merged[,-c(41:42)]

main3_edu <- filter(main2, !(is.na(schools_national_date) & is.na(schools_localized_date)))
main4 <- rbind(main3_edu, main3_noedu_merged)
main4$days_schools_localized <- ifelse(
  main4$days_schools_localized < main4$days_schools_national, 
  main4$days_schools_national, main4$days_schools_localized)
main4$schools_localized <- ifelse(
  (main4$schools_localized == 0 & main4$schools_national == 1), 
  main4$schools_national, main4$schools_localized)
main4$days_restrictions <- ifelse(
  main4$days_restrictions < main4$days_quarantine, 
  main4$days_restrictions, main4$days_quarantine)
main4$restrictions <- ifelse(
  (main4$restrictions == 0 & main4$quarantine == 1), 
  main4$quarantine, main4$restrictions)
########################## merge baseline data #################################
geo <- read.csv(file = here("Data", "region_metadata.csv"))
main5 <- merge(main4[,-10], geo, by = c("Country_Region", "Province_State"), 
               all.x = TRUE)

# add some more baseline
obese <- read.csv(file = here("Data", "WHO_obesity.csv"))
colnames(obese) <- c("country", "obese", "male_obese", "fem_obese")
overweight <- read.csv(file = here("Data", "WHO_overweight.csv"))
colnames(overweight) <- c("country", "overweight", "male_overweight", "fem_overweight")
fatdat <- merge(obese, overweight, by = "country")
fatdat$country <- ifelse(fatdat$country == "Bolivia (Plurinational State of)",
                         "Bolivia", as.character(fatdat$country))
fatdat$country <- ifelse(fatdat$country == "Brunei Darussalam", "Brunei", 
                         as.character(fatdat$country))
fatdat$country <- ifelse(fatdat$country == "Congo", "Congo (Brazzaville)", 
                         as.character(fatdat$country))
fatdat$country <- ifelse(fatdat$country == "Democratic Republic of the Congo", 
                         "Congo (Kinshasa)", as.character(fatdat$country))
fatdat$country <- ifelse(fatdat$country == "Côte d'Ivoire", "Cote d'Ivoire", 
                         as.character(fatdat$country))
fatdat$country <- ifelse(fatdat$country == "Iran (Islamic Republic of)", 
                         "Iran", as.character(fatdat$country))
fatdat$country <- ifelse(fatdat$country == "Republic of Korea", 
                         "Korea, South", as.character(fatdat$country))
fatdat$country <- ifelse(fatdat$country == "Lao People's Democratic Republic", 
                         "Laos", as.character(fatdat$country))
fatdat$country <- ifelse(fatdat$country == "Republic of Moldova", 
                         "Moldova", as.character(fatdat$country))
fatdat$country <- ifelse(fatdat$country == "Russian Federation", 
                         "Russia", as.character(fatdat$country))
fatdat$country <- ifelse(fatdat$country == "Syrian Arab Republic", 
                         "Syria", as.character(fatdat$country))
fatdat$country <- ifelse(fatdat$country == "Taiwan, Province of China", 
                         "Taiwan*", as.character(fatdat$country))
fatdat$country <- ifelse(fatdat$country == "United Republic of Tanzania", 
                         "Tanzania", as.character(fatdat$country))
fatdat$country <- ifelse(fatdat$country == "United States of America", 
                         "US", as.character(fatdat$country))
fatdat$country <- ifelse(fatdat$country == "United Kingdom of Great Britain and Northern Ireland", 
                         "United Kingdom", as.character(fatdat$country))
fatdat$country <- ifelse(fatdat$country == "Venezuela (Bolivarian Republic of)", 
                         "Venezuela", as.character(fatdat$country))
fatdat$country <- ifelse(fatdat$country == "Viet Nam", "Vietnam", 
                         as.character(fatdat$country))
fatdat$country <- ifelse(fatdat$country == "Macao", "Macau", 
                         as.character(fatdat$country))
fatdat$country <- ifelse(fatdat$country == "Curaçao", "Curacao",
                         as.character(fatdat$country))
fatdat$country <- ifelse(fatdat$country == "Saint Barthélemy", 
                         "Saint Barthelemy", as.character(fatdat$country))
fatdat$country <- ifelse(fatdat$country == "Saint Martin (French part)", 
                         "Sint Maarten", as.character(fatdat$country))      
fatdat$country <- ifelse(fatdat$country == "Sint Maarten (Dutch part)", 
                         "St Martin", as.character(fatdat$country))
fatdat$country <- ifelse(fatdat$country == "Sudan (former)", 
                         "Sudan", as.character(fatdat$country))
fatdat$country <- ifelse(fatdat$country == "Republic of North Macedonia", 
                         "North Macedonia", as.character(fatdat$country))
fatdat$country <- ifelse(fatdat$country == "Democratic People's Republic of Korea",
                         "Korea (Democratic People's Republic of)", as.character(fatdat$country))
fatdatfinal <- merge(fatdat, country_codes[,c(1,3)], by = "country", all.x = TRUE)[-166,-1]
sum(is.na(fatdatfinal$country_code))
main6 <- merge(main5, fatdatfinal, by = c("country_code"), all.x = TRUE)

baseline <- read.csv(file = here("Data", "baselinecovs.csv"))
colnames(baseline)[2] <- "region"
main7 <- merge(main6, baseline, by = c("country_code", "region"), all.x = TRUE)

# add recoveries
recoveries <- read.csv(file = here("Data", "region_date_metadata.csv"))
recoveries$Date <- as.Date(recoveries$Date)
main8 <- merge(main7, recoveries, 
               by = c("Date", "Province_State", "Country_Region"), all.x = TRUE)

# add more cases/fatalities data
# updates <- read.csv(file = here("Data", "updated_data.csv"))
# colnames(updates)[1:2] <- c("Date", "country")
# updates$country <- ifelse(updates$country == "British Virgin Islands",
#                          "Virgin Islands (British)", as.character(updates$country))
# updates$country <- ifelse(updates$country == "Cape Verde", "Cabo Verde", 
#                           as.character(updates$country))
# updates$country <- ifelse(updates$country == "Congo", "Congo (Brazzaville)", 
#                           as.character(updates$country))
# updates$country <- ifelse(updates$country == "Democratic Republic of Congo", 
#                           "Congo (Kinshasa)", as.character(updates$country))
# updates$country <- ifelse(updates$country == "Czech Republic", 
#                           "Czechia", as.character(updates$country))
# updates$country <- ifelse(updates$country == "Faeroe Islands", 
#                           "Faroe Islands", as.character(updates$country))
# updates$country <- ifelse(updates$country == "Macedonia", 
#                           "North Macedonia", as.character(updates$country))
# updates$country <- ifelse(updates$country == "Palestine", 
#                           "Palestine, State of", as.character(updates$country))
# updates$country <- ifelse(updates$country == "Sint Maarten (Dutch part)", 
#                           "Sint Maarten", as.character(updates$country))
# updates$country <- ifelse(updates$country == "South Korea", "Korea, South", 
#                           as.character(updates$country))
# updates$country <- ifelse(updates$country == "Swaziland", "Eswatini", 
#                           as.character(updates$country))
# updates$country <- ifelse(updates$country == "Taiwan", "Taiwan*", 
#                           as.character(updates$country))
# updates$country <- ifelse(updates$country == "Timor", "Timor-Leste", 
#                           as.character(updates$country))
# updates$country <- ifelse(updates$country == "United States", "US", 
#                           as.character(updates$country))
# updates$country <- ifelse(updates$country == "United States Virgin Islands", 
#                           "Virgin Islands (U.S.)", as.character(updates$country))
# updates$country <- ifelse(updates$country == "Vatican", "Holy See", 
#                           as.character(updates$country))
# updates <- merge(updates, country_codes[,c(1,3)], by = "country", all.x = TRUE)
# updates <- updates[-which(is.na(updates$country_code)),]
# updates$Date <- as.Date(updates$Date, "%m/%d/%y")
# main9 <- merge(main8, updates[,-1], by = c("Date", "country_code"), all.x = TRUE)

main9 <- main8[order(main8$Country_Region, main8$region, main8$Date),]
main9 <- main9[,-c(15:16,18:20)]
all <- main9[,c(1:8, 105, 9, 28:32,15,33:39, 10, 12:14,16:27,40:104)]

############################# imputation #######################################

# set missing SARS to 0 
all$sars_cases <- ifelse(is.na(all$sars_cases), 0, all$sars_cases)
all$sars_deaths <- ifelse(is.na(all$sars_deaths), 0, all$sars_deaths)
all$sars_recovered <- ifelse(is.na(all$sars_recovered), 0, all$sars_recovered)

# LOCF recoveries by province/state or region, create missingness indicator
all$delta_recoveries <- ifelse(is.na(all$Recoveries), 0, 1)
all$Recoveries <- ifelse((is.na(all$Recoveries) & all$ConfirmedCases == 0), 0, 
                         all$Recoveries)
all$Province_State <- ifelse(all$Province_State == "", NA, as.character(all$Province_State))
dat_state <- filter(all, !(is.na(Province_State)))
dat_state <- dat_state[order(dat_state$Province_State, dat_state$Date),]
dat_state2 <- dat_state %>% 
  group_by(Province_State) %>% 
  fill(Recoveries)
dat_nostate <- filter(all, is.na(Province_State))
dat_nostate <- dat_nostate[order(dat_nostate$region, dat_nostate$Date),]
dat_nostate2 <- dat_nostate %>% 
  group_by(region) %>% 
  fill(Recoveries)
all2 <- rbind(dat_nostate2, dat_state2)
all2 <- all2[order(all2$Country_Region, all2$region, all2$Province_State, all2$Date),]

# fix formatting of some of the economic data
econ_gdppercapita <- as.character(all2$econ_gdppercapita)
econ_gdppercapita <- substring(econ_gdppercapita, 2) 
all2$econ_gdppercapita <- as.numeric(gsub(",","", econ_gdppercapita))

econ_fdiflowmills <- as.character(all2$econ_fdiflowmills)
all2$econ_fdiflowmills <- as.numeric(gsub(",","", econ_fdiflowmills))

econ_gdpbills <- as.character(all2$econ_gdpbills)
econ_gdpbills <- substring(econ_gdpbills, 2) 
all2$econ_gdpbills <- as.numeric(gsub(",","", econ_gdpbills))

# change facs to numeric
facs <- colnames(all2)[c(48:68,70:71,73:74,76)]
all2[facs] <- sapply(all2[facs], as.character)
all2[facs] <- sapply(all2[facs], as.numeric)

# sl3-style imputation by continent
X <- data.table(all2[,c(9,11:105)])
processedX <- process_data(X, strata = "continent")
final <- data.table(all2[,c(1:8,10)], processedX)

# format global regions
final$region <- ifelse(!(is.na(final$Province_State)), 
                       as.character(final$Province_State), 
                       as.character(final$region))
final$region <- as.factor(final$region)
colnames(final)[c(1:3, 6:10)] <- c("date", "province_state", "country_region", 
                                   "id", "cases", "fatalities", "forecastid", 
                                   "recoveries")
data <- data.table(final)
############################### add in features ################################
data[, days:=as.numeric(difftime(date,min(date),unit="days"))]
data[, first_case_date:=min(date[cases>0], na.rm=TRUE),by=list(region)]
data[, case_days:=as.numeric(difftime(date, first_case_date, unit="days"))]
data[, log_case_days:=ifelse(case_days>0, log10(case_days), 0)]
data[, tenth_case_date:=min(date[cases>10], na.rm=TRUE), by=list(region)]
data[, case10_days:=as.numeric(difftime(date, tenth_case_date,unit="days")), by=list(region)]
data[, hundreth_case_date:=min(date[cases>100], na.rm=TRUE), by=list(region)]
data[, case100_days:=as.numeric(difftime(date, hundreth_case_date,unit="days")), by=list(region)]
data[, max_cases:=max(cases, na.rm=TRUE), by=list(region)]

################################################################################
# final save of all data, training, test
################################################################################

all <- data[,c(1:3,5:9,185:193,10:184)]
all <- all[order(all$country_region, all$region, all$date),]
write.csv(all, file = here("Data", "all.csv"), row.names = FALSE)

training <- all[is.na(all$forecastid),]
training <- training[order(training$id),-c(2:3,8)]
write.csv(training, file = here("Data", "training.csv"), row.names = FALSE)

test <- all[is.na(all$id),]
test <- test[order(test$forecastid),-c(2:3,5:7)]
write.csv(test, file = here("Data", "test.csv"), row.names = FALSE)