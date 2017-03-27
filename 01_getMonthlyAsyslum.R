library(eurostat)
library(dplyr)
library(magrittr)

######      This download the very large monthly asylum data with country of origin!
dataID <- "migr_asyappctzm"

data_2015.file <- "data/data_2015only.csv"
data.sub.file <- "input/data.csv"
data.topOrigins.dw.file <- "output/top2015_asylumOrigins.csv"
data.origins_ts.file <- "input/orgin2015_ts.csv"

############################################################################################
###		Get data
############################################################################################

dat <- get_eurostat(dataID, time_format = "raw", cache = F)
data <- cbind(label_eurostat(dat), iso2 = dat$geo)

# transform dates efficiently!
times <- unique(data$time)
times <- structure(eurostat:::eurotime2date(times, last = FALSE), names = as.character(times))
data$time <- times[match(data$time, names(times))]

# subset columns - get First time applicant and not all 'Asylum applicant'!
data %<>% filter(asyl_app == 'First time applicant', sex == 'Total', age == 'Total') %>%
	select(one_of(c('citizen', 'geo', 'time', 'values', 'iso2')))

#write.csv(data, file = rawData.file,row.names = F)
data_all <- data

# subset the data for year 2015
data %<>% filter(time >= as.Date("2015-01-01"))
write.csv(data, file = data_2015.file, row.names = F)

# consider only the months with full data (with NA less than 5%) !!
# consider only the months with full data (with NA less than 5%) !!
times <- round(tapply(data$values, data$time, function(v) (sum(is.na(v))/length(v)) ) * 100)
cat("% of missing country data by month\n", times)
max.date <- max(as.Date(names(times)[times < 5]))
cat("Month until no missing data\n", as.character(max.date), " filter missing data months")
data %<>% filter(time <= as.Date(max.date))

	### CHECK ###
## check CH value with http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=migr_asyappctzm&lang=en
data %>% filter(iso2 == 'CH', citizen == "Total")
data %>% filter(iso2 == 'HU', citizen == "Total")

data.all <- data

############################################################################################
###		1. Reshape: filter, aggregate for parallel set chart
############################################################################################

citizenAgg <- c("Total", "European Union (28 countries)", "Extra EU-28")
iso2agg <- c("EU28", "TOTAL")

###		Aggregate data by geo & citizen
data %<>% filter(!citizen %in% citizenAgg, !iso2 %in% iso2agg, time <= max.date) %>%
  group_by(iso2, geo, citizen) %>% dplyr::summarise(tot = sum(values, na.rm = T )) %>% ungroup()

# remove coutry of origin always 0
citToRemove <- as.character(unlist(data %>% group_by(citizen) %>% dplyr::summarise(grandTot = sum(tot, na.rm = T)) %>% filter(grandTot == 0) %>% select(citizen)))
data %<>% filter(!citizen %in% citToRemove)


### Get the top country of origins and destinations
# find the n largest countries during the last year
ntop <- 7

sumByGeo <- data %>% group_by(iso2) %>% dplyr::summarise(sumByGeo = sum(tot, na.rm = T)) %>% ungroup()
iso2.top <- as.character(unlist(head(as.data.frame(sumByGeo[order(sumByGeo$sumByGeo, decreasing = T),'iso2']), ntop)))
iso2.top
iso2.sub <- c('DE', 'CH', 'SE', 'HU', 'IT', 'FR', 'AT', 'UK')


sumByCit <- data %>% group_by(citizen) %>% dplyr::summarise(sumByCit = sum(tot, na.rm = T)) %>% ungroup()
cit.top <- as.character(unlist(head(as.data.frame(sumByCit[order(sumByCit$sumByCit, decreasing = T),'citizen']), ntop)))


### Merge not top countries
df <- data
df$iso2 <- ifelse(as.character(df$iso2) %in% iso2.sub, as.character(df$iso2), 'other')
df$citizen <- ifelse(as.character(df$citizen) %in% cit.top, as.character(df$citizen), 'Other countries')

df %<>% group_by(iso2, citizen) %>% dplyr::summarise (values = sum(tot, na.rm = T)) %>% ungroup()


# add back the country names to merged iso2
df$geo <- as.character(unlist(data[match(df$iso2, data$iso2),'geo']))
df[which(df$iso2 == "other"), 'geo'] <- "Other European countries"


# drop unused levels
df$citizen <- factor(df$citizen)
df$iso2 <- factor(df$iso2)

citLength <- unlist(unique(df %>% group_by(iso2) %>% dplyr::summarise(nelem = length(values)) %>% select(nelem)))
geoLength <- unlist(unique(df %>% group_by(citizen) %>% dplyr::summarise(nelem = length(values)) %>% select(nelem)))

stopifnot(geoLength == length(iso2.sub) + 1, citLength == ntop + 1)

## tmp hack for long country names
df$geo <- gsub(" \\(.*\\)$", "", df$geo)
df$citizen <- gsub(" \\(.*\\)$", "", df$citizen)

write.csv(df, file = data.sub.file, row.names = F)

## get the total applications
sum_sofar <- data.all %>% filter(iso2 == "TOTAL", citizen == "Total") %>% select(values) %>% dplyr::summarise(sum(values))
print(sum_sofar)

# create the tmp translation file
labels <- c(unique(df$citizen), unique(df$geo))
write.csv(data.frame(code = paste0("code.", gsub(" ", "", labels)), en = labels), file = "input/translation_tmp.csv", row.names = F)

write.csv(sum_sofar, file = "input/sum_sofar.csv", row.names = F)


############################################################################################
###		2. Reshape: filter, aggregate for datawrapper charts of all country of origins as a timeseries
############################################################################################

nTopcit <- 14
ds <- data_all %>% filter(iso2 == "TOTAL", !citizen %in% citizenAgg, time <= max.date) %>% select(citizen, time, values)

# remove citizen always 0
citNULL <- ds %>% group_by(citizen) %>% dplyr::summarise(citsum = sum(values)) %>% ungroup() %>%
  filter(citsum == 0) %>% select(citizen) %>% unlist() %>% as.character()
ds %<>% filter(!citizen %in% citNULL )


# Get the top origin overall
sumByCit <- ds %>% group_by(citizen) %>% dplyr::summarise(sumByCit = sum(values, na.rm = T)) %>% ungroup()
head(sumByCit %>% arrange(desc(sumByCit)), nTopcit)
topOrigins <- sumByCit %>% arrange(desc(sumByCit)) %>% select(citizen) %>% slice(1:nTopcit) %>% unlist() %>% as.character()


### Merge not top origins
ds_nonmerged <- ds

ds$citizen <- ifelse(as.character(ds$citizen) %in% topOrigins, as.character(ds$citizen), 'Other countries')

ds %<>% group_by(citizen, time) %>% dplyr::summarise(value = sum(values, na.rm = T))
ds$citizen <- gsub(" \\(.*\\)$", "", ds$citizen)

# write data
write.csv(ds, file = data.origins_ts.file, row.names = F)

labels <- unique(ds$citizen)
write.csv(data.frame(code = paste0("code.", gsub(" ", "", labels)), en = labels), file = "input/translationStream_tmp.csv", row.names = F)

