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
files <- list.files(pattern = "*.csv", 
                    path = here("Data", "week2"))
path <- paste0("~/tlverse-covid-forecast/Data/week2/",files)
listdata <- lapply(path, read.csv)
names(listdata) <- files

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

# prep country_codes for merge with covariates
colnames(country_codes)[c(1,3)] <- c("country", "country_code")

################################################################################
# baseline covariate data
################################################################################
files <- list.files(pattern = "*.csv", 
                    path = here("Data", "covid19-global-forecasting-week-1"))
path <- paste0("~/tlverse-covid-forecast/Data/covid19-global-forecasting-week-1/",files)
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
write.csv(baseline_data, file = here("Data", "baselinecovs.csv"), row.names = F)

################################################################################
# time-varying covariate data --STILL WORKING ON THIS 
################################################################################

################# listdata[[2]] = covid_impact_education #######################
dat <- listdata[[2]]
all.equal(dat$Date, dat$Date.1) # TRUE
dat <- dat[,c(2:5)]
colnames(dat) <- c("country_code", "country", "edu_scale", "edu_date")
dat$edu_date <- as.Date(dat$edu_date, "%d/%m/%Y")
dat$country_code <- ifelse(dat$country == "China, Macao Special Administrative Region", 
                           "MAC", as.character(dat$country_code))
dat$country_code <- ifelse(dat$country == "China, Hong Kong Special Administrative Region", 
                           "HKG", as.character(dat$country_code))
datfinal <- merge(dat[,-2], country_codes[,c(1,3)], by = "country_code", all.x = TRUE)
sum(is.na(datfinal$country_code))
listdata[[2]] <- datfinal

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


########################### listdata[[18]] = weather data ######################




################################################################################
# put it all together
################################################################################

main_data <- fread("Data/covid19-global-forecasting-week-1/train.csv")
old_names <- c("Id", "Province/State", "Country/Region", "Lat", "Long", "Date", 
               "ConfirmedCases", "Fatalities")
new_names <- c("id","state", "country", "lat", "long", "date", "cases", "deaths")

setnames(main_data, old_names, new_names)
main_data$date <- as.Date(main_data$date)

restrictions <- fread("Data/covid19-global-forecasting-week-1/restrictions_info_data.csv")


main_data <- merge(main_data, restrictions[,c("country","population", "tests",
                                              "testpop","density", "median_age",
                                              "urbanpop", "smokers", "sex_ratio",  
                                              "quarantine_date", 
                                              "restrictions_date", 
                                              "schools_national_date",
                                              "schools_localized_date",
                                              "hospital_bed","lung_disease")], 
                   by = c("country"), all.x = TRUE)

main_data <- main_data %>%
  group_by(country) %>%
  mutate(intervention_school_national = ifelse(date < schools_national_date,0,1)) %>%
  mutate(intervention_school_local = ifelse(date < schools_localized_date,0,1)) %>%
  mutate(intervention_restriction = ifelse(date < restrictions_date,0,1)) %>%
  mutate(intervention_quarantine = ifelse(date < quarantine_date,0,1)) %>%
  select(-c(schools_national_date,schools_localized_date,restrictions_date,quarantine_date))

data <- main_data

################################################################################
#TODO: add in features
################################################################################
data[, days:=as.numeric(difftime(date,min(date),unit="days"))]
data[, region:=paste(country, state)]
data[, first_case_date:=min(date[cases>0]), by=list(region)]
data[, case_days:=as.numeric(difftime(date, first_case_date,unit="days"))]
data[, log_case_days:=ifelse(case_days>0, log10(case_days), 0)]
data[, tenth_case_date:=min(date[cases>10]), by=list(region)]
data[, case10_days:=as.numeric(difftime(date, tenth_case_date,unit="days")), by=list(region)]
data[, hundreth_case_date:=min(date[cases>100]), by=list(region)]
data[, case100_days:=as.numeric(difftime(date, hundreth_case_date,unit="days")), by=list(region)]
data[, max_cases:=max(cases), by=list(region)]