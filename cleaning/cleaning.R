library(readxl)
library(dplyr)
library(purrr)
library(tidyr)

#(a) semester dataの整形
seme1 <- read.csv('semester_data_1.csv', sep=',', header=TRUE, skip=1)
seme2 <- read.csv('semester_data_2.csv', sep=',', header=TRUE)
columns <- c('unitid', 'instnm', 'semester', 'quarter', 'year', 'Y')
names(seme2) <- columns

seme_concat <- bind_rows(seme1, seme2) %>%
  select(-Y)


seme_concat$yearofsem <- NA
unique_unitids <- unique(seme_concat$unitid)
for (unitid in unique_unitids) {
  unit_data <- filter(seme_concat, unitid == !!unitid)
  change_year <- min(unit_data$year[unit_data$semester == 1 & unit_data$quarter == 0], na.rm=TRUE)
  
  if (!is.na(change_year)) {
    if (!(change_year %in% c(1991, 2010))) {
      seme_concat$yearofsem[seme_concat$unitid == unitid] <- change_year
    }
  }
}

seme_concat$yearofsem <- as.integer(seme_concat$yearofsem)

seme_concat$after <- NA
for (unitid in unique_unitids) {
  unit_data <- filter(seme_concat, unitid == !!unitid)
  change_year <- min(unit_data$yearofsem, na.rm=TRUE)
  
  if (!is.na(change_year)) {
    seme_concat$after[seme_concat$unitid == unitid & seme_concat$year >= change_year] <- 1
    seme_concat$after[seme_concat$unitid == unitid & seme_concat$year < change_year] <- 0
  }
}
seme_concat$after[is.na(seme_concat$yearofsem)] <- NA
seme_concat$after <- as.integer(seme_concat$after)

#print(sample_n(seme_concat, 10))
#write.csv(seme_concat, "seme_concat_output.csv", row.names = FALSE)

#(b) Gradrate Dataの整形
year_data <- list()

for (year in 1991:1993) {
  file_name <- paste0(year, ".xlsx")
  year_data[[paste0(year, "outcome")]] <- tryCatch(
    {
      read_excel(file_name)
    },
    error = function(e) {
      message(paste("Failed to read", file_name, ":", e))
      return(NULL)
    }
  )
}

for (year in 1995:2016) {
  file_name <- paste0(year, ".xlsx")
  year_data[[paste0(year, "outcome")]] <- tryCatch(
    {
      read_excel(file_name)
    },
    error = function(e) {
      message(paste("Failed to read", file_name, ":", e))
      return(NULL)
    }
  )
}

gradrate <- bind_rows(year_data)

gradrate <- gradrate %>%
  mutate(
    womengradrate4yr = women_gradrate_4yr * 0.01,
    tot4yrgrads = as.numeric(tot4yrgrads),
    totcohortsize = as.numeric(totcohortsize),
    gradrate4yr = round(tot4yrgrads / totcohortsize, 3),
    m_4yrgrads = as.numeric(m_4yrgrads),
    m_cohortsize = as.numeric(m_cohortsize),
    mengradrate4yr = round(m_4yrgrads / m_cohortsize, 3)
  ) %>%
  filter(year >= 1991 & year <= 2010)

#print(str(gradrate))
#write.csv(gradrate, 'gradrate_output.csv', row.names = FALSE)

#(c) Covariates Dataの整形
covariates <- read_excel('covariates.xlsx')
covariates <- covariates %>%
  rename(unitid = university_id) %>%
  mutate(unitid = gsub('aaaa','', unitid)) %>%
  mutate(unitid = as.integer(unitid))

covariates <- covariates %>%
  pivot_wider(
    id_cols = c(unitid, year),
    names_from = category,
    values_from = value,
    values_fn = first
  )

covariates <- covariates %>%
  filter(year >= 1991, year <= 2010, year != 1994) %>%
  filter(unitid %in% unique(gradrate$unitid))

#print(str(covariates))
#print(tail(covariates, 10))

#(d) Master Dataの作成
seme_concat_df <- read.csv('seme_concat_output.csv')
gradrate_df <- read.csv('gradrate_output.csv')
covariates_df <- read.csv('covariates_output.csv')

master_df <- seme_concat_df %>%
  left_join(gradrate_df, by = c('unitid','year')) %>%
  left_join(covariates_df, by = c('unitid','year'))

#print(head(master_df,10))
#print(str(master_df))
#write.csv(master_df,'master_data.csv',row.names = FALSE)