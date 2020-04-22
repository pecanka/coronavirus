read_data_ocdc = function(url, max_lag=10, lag=1) {

  file = 'data/'%.%separate_path(url)$filename

  data = if(!file.exists(file)) {
    warn("File '",file,"' does not exist.")
    if(lag<=max_lag) {
      catn("Trying previous day's data ...")
      data = read_data_ocdc(sub(y_day(lag=-lag+1), y_day(lag=-lag), url), lag=lag+1)
    } else {
      NULL
    }
  } else {
    catn("Reading file '",file,"' ...")
    read_xlsx(file)
  }

  if(is_empty(data)) error("No data loaded.")

  if('DailyCases' %notin% names(data)) {
    data %<>% mutate(Date=as.character(as.Date(dateRep)-1)) %>%
      rename(DailyCases=cases,
             DailyDeaths=deaths,
             Country=countriesAndTerritories,
             CountryID=geoId,
             CountryCode=countryterritoryCode,
             Population=popData2018) %>%
      arrange(Country, Date) %>%
      group_by(Country) %>%
      mutate(Cases=cumsum(DailyCases),
             Deaths=cumsum(DailyDeaths)) %>%
      ungroup() %>%
      select(Date, Country, Cases, Deaths, DailyCases, DailyDeaths, Population,
             ID=CountryID, Code=CountryCode)
  }

  data %>% mutate(Country=recode(Country,
      'Czech_Republic'='Czechia', 
      'United_States_of_America'='USA',
      'United_Kingdom'='UK', 
      'South_Korea'='SouthKorea'))

}

####################################################
load_data_mzcr = function(CountryCZ) {

  country_name = CountryCZ$name
  country_prefix = CountryCZ$prefix

  rda_file_country = 'data/'%.%country_prefix%.%country_name%.%'_historical_'%.%c(utilbox::t_day(),y_day())%.%'.rda' %>%
    `[`(file.exists(.)) %>% h1()
  rda_file_latest = list.files('data', country_prefix%.%country_name%.%'_latest_', full.names=TRUE) %>%
    file_sort_time() %>% h1()

  if(is_empty(rda_file_country))
    error('There is no file with the historical data for ', country_name,'.')
  if(is_empty(rda_file_latest))
    error('There is no file with the latest data for ', country_name,'.')

  catn("Loading historical data for ",country_name," from file '",rda_file_country,"'...")
  load(rda_file_country)
  catn("Loading latest data for ",country_name," from file '",rda_file_latest,"'...")
  load(rda_file_latest)

  nlist(data, latest)

}

####################################################

load_data_individual = function(countries, max_lag=as.numeric(as.Date(t_day())-as.Date('2020-01-01'))) {

  if(missing(countries)) countries = download_country_list_wom(url_wom)

  Data = Latest = NULL
  for(country in split_rows(countries)) {

    rda_file_country = 'data/'%.%country$prefix%.%country$name%.%'_historical_'%.%c(y_day(lag=0:-max_lag))%.%'.rda' %>%
      `[`(file.exists(.)) %>% h1()
    rda_file_latest = list.files('data', country$name%.%'_latest_', full.names=TRUE) %>%
      file_sort_time() %>% h1()

    if(is_empty(rda_file_country))
      error('No file with the historical data found for ', country$name,'.')
    if(is_empty(rda_file_latest))
      error('No file with the latest data found for ', country$name,'.')

    catn("Loading historical data for ",country$name," from file '",rda_file_country,"'...")
    load(rda_file_country)
    catn("Loading latest data for ",country$name," from file '",rda_file_latest,"'...")
    load(rda_file_latest)

    data %<>% add_col(CurrentlyInfected=NA) %>%
      rename(Infected=CurrentlyInfected)

    Data %<>% bind_rows(data)
    Latest %<>% bind_rows(latest)

  }

  catn("Data loaded.")

  nlist(Data, Latest)


}

####################################################

load_latest_all_wom = function() {

  rda_file_latest = list.files('data', 'wom_latest_all_countries_', full.names=TRUE) %>%
    file_sort_time() %>% h1()

  if(is_empty(rda_file_latest))
    error('There is no file with the latest data for all countries.')

  catn("Loading latest data for all countries from file '",rda_file_latest,"'...")
  load(rda_file_latest)

  latest_all


}

####################################################
