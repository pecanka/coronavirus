#source('d:/Dropbox/Projects/CoronaVirus/get_corona_data.R')

setwd2()

llib(rvest, stringr, tidyr, lubridate, plotly, rjson, readxl)

#########################################################
url_ocdc = 'https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-'%.%t_day()%.%'.xlsx'

url_wom = 'https://www.worldometers.info/coronavirus/'
url_mzcz = 'https://onemocneni-aktualne.mzcr.cz/covid-19'
url_mzcz_api = 'https://onemocneni-aktualne.mzcr.cz/api/v1/covid-19/'

do_download_data = FALSE
do_load_data = FALSE

do_force_fresh_data = FALSE
do_use_any_existing_data = FALSE

#do_process_lag = TRUE
#do_force_lag_calculation = !FALSE

do_plot = TRUE
do_plot_lag = TRUE
do_save_plotly_to_file = TRUE

countries_focus = 'all'
countries_focus = 'broad'
#countries_focus = 'narrow'

PerPopulation = 100000
Country0 = 'Italy'
Countries_focus_narrow = c("Netherlands","Germany","USA","Czechia")

Countries_focus_broad = c('Czechia','Italy','Germany','Netherlands',
  'Spain','USA','S. Korea','Austria','UK','Canada','Belgium','Denmark',
  'Norway','Poland','Sweden','Switzerland','France')

### TEMPORARILY ADD THE CZECH DEATHS MANUALLY ###
cz_daily_deaths = tribble(~Date, ~DailyDeaths,
  '2020-03-22', 1, '2020-03-23', 0, '2020-03-24', 2,
  '2020-03-25', 3, '2020-03-26', 3, '2020-03-27', 0,
  '2020-03-28', 2, '2020-03-29', 5, '2020-03-30', 7)

cz_deaths = cz_daily_deaths %>% mutate(Deaths=cumsum(DailyDeaths)) %>% select(-DailyDeaths)
### TEMPORARILY ADD THE CZECH DEATHS MANUALLY ###

data_source = "Based on data from <a href='"%.%url_wom%.%"'>Worldometers</a>, <a href='"%.%
  url_ocdc%.%"'>OCDC</a>"%.%" and <a href='"%.%url_mzcz%.%"'>mzcr.cz</a>"

#########################################################

download_country_list = function(url) {

  pattern = 'countries_'%.%ifelse(do_use_any_existing_data,"",t_day())
  f = list.files('data',pattern, full.names=TRUE) %>% file_sort_time() %>% h1()

  if(!do_force_fresh_data && length(f)>0) {
    catn("Loading list of countries ...", flush_cycle=10)
    load(f)
  } else {
    catn("Getting list of countries from the web ...")
    html = read_html(url)
    name = html %>% html_nodes('.mt_a') %>% html_text()
    surl = html %>% html_nodes('.mt_a') %>% html_attr('href')
    countries = tibble(name, url=url%.%surl) %>% unique()
    save(countries, file='data/countries_'%.%t_day()%.%'.rda')
  }

  countries

}


extract_latest_mzcr = function(html, country) {
  Tested = html %>% html_nodes('#count-test') %>% html_text() %>% force_as_integer()
  Cases = html %>% html_nodes('#count-sick') %>% html_text() %>% force_as_integer()
  Deaths = html %>% html_nodes('#count-dead') %>% html_text() %>% force_as_integer()
  Recovered = html %>% html_nodes('#count-recover') %>% html_text() %>% force_as_integer()
  tibble(Tested, Cases, Deaths, Recovered) %>%
  setNames(c('Tested','Cases','Deaths','Recovered')) %>%
  mutate(Country=country, Date=t_day()) %>%
  select(Country, Date, everything())
}

extract_latest_wom = function(html, country) {
  html %>% html_nodes('div .maincounter-number span') %>%
    html_text() %>%
    force_as_integer() %>%
    setNames(c('Cases','Deaths','Recovered')) %>%
    t() %>%
    as_tibble() %>%
    mutate(Country=country, Date=t_day(), Tests=NA) %>%
    select(Country, Date, Tests, everything())
}

extract = function(x, pattern, names) {
  x %>% str_extract(pattern) %>% str_extract("[\\[].+[\\]]") %>%
    gsub('\"nan\"|null','NA',.) %>% sub('[\\[]','c(',.) %>% sub(']',')',.) %>%
    lapply(function(x) eval(parse(text=x))) %>%
    setNames(names)
}

####################################################

download_data_ocdc = function(url, force_download=FALSE) {
  file = 'data/'%.%separate_path(url)$filename
  if(force_download || !file.exists(file)) {
    if(!url_exists(url)) {
      warn("File '",url,"' does not seem to exist.")
    } else {
      download.file(url, file, mode="wb")
    }
  }
}

####################################################

read_data_ocdc = function(url, default_to_yday=TRUE) {

  file = 'data/'%.%separate_path(url)$filename

  data = if(!file.exists(file)) {
    warn("File '",file,"' does not exist.")
    if(default_to_yday) {
      catn("Trying previous day's data ...")
      data = read_data_ocdc(sub(t_day(), y_day(), url), default_to_yday=FALSE)
    } else {
      NULL
    }
  } else {
    read_xlsx(file)
  }

  if(is_empty(data)) error("No data loaded.")

  data %>% mutate(Date=as.Date(dateRep)) %>%
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
    select(Date, Country, Cases, Deaths, DailyCases, DailyDeaths, Population)

}

####################################################

download_data_wom = function(available_countries, url) {

  if(missing(available_countries)) available_countries = download_country_list(url)

  Data = Latest = NULL
  for(country in split_rows(available_countries)) {

    country_url = country %>% pull(url)
    country %<>% pull(name)

    catn("Scrapping data for ",country," from worldometers.info ...")
    html = try(read_html(country_url))
    if('try-error' %in% class(html)) next
    #some_fresh_data_present = TRUE

    latest = extract_latest_wom(html, country)
    save(latest, file='data/'%.%country%.%'_latest_'%.%t_day('%Y-%m-%d-%H%M%S')%.%'.rda')

    series = html %>% as.character() %>% regmatches(gregexpr('series:[^}]+[}]',.)) %>% unlist()
    series_names = series %>% str_extract("name: '[^,]+',") %>% str_extract("'.+'") %>% gsub("[' ]","",.)
    series_dates = html %>% as.character() %>% regmatches(gregexpr('xAxis:[^}]+[}]',.)) %>% unlist() %>%
      extract("categories: [\\[][^\\]]+[\\]]", series_names)
    series_data = series %>% extract("data: [\\[][^\\]]+[\\]]", series_names)

    data = lapply(1:length(series_names),
                  function(i) do.call(bind_cols, list(series_dates[i], series_data[i])) %>%
                                setNames(c('Date',series_names[i]))) %>%
      setNames(series_names) %>%
      `[`(which(!duplicated(series_names))) %>%
      join_recurse(by='Date', join='full_join') %>%
      mutate(Country=country, Date=parse_date_time(Date, orders='md', tz=NULL) %>% format()) %>%
      select(Country, Date, everything())

    save(data, file='data/'%.%country%.%'_historical_'%.%t1(data$Date)%.%'.rda')

  }

  catn("Data for all countries scrapped.")

}

####################################################

download_data_mzcr = function(CountryCZ) {

  country_url = CountryCZ$url
  country = CountryCZ$name

  catn("Getting latest data for ",country,"...")
  html = try(read_html(country_url))
  if('try-error' %in% class(html)) error("Data not found.")
  #some_fresh_data_present = TRUE

  latest = extract_latest_mzcr(html, country)
  save(latest, file='data/'%.%country%.%'_latest_'%.%t_day('%Y%m%d%H%M%S')%.%'.rda')

  catn("Processing ",country,"...")

  series_cases = html %>% html_nodes('div #js-cummulative-total-persons-data') %>% html_attr('data-linechart')
  series_tests = html %>% html_nodes('div #js-cummulative-total-tests-data') %>% html_attr('data-linechart')
  series_daily_cases = html %>% html_nodes('div #js-total-persons-data') %>% html_attr('data-barchart')

  call = . %>% regmatches(gregexpr('\\[[{].*\\][}]',.)) %>% unlist() %>%
    fromJSON() %>% lapply(as_tibble) %>% bind_rows() %>%
    mutate(Date=as.character(parse_date_time(x, 'dmy')), Count=as.numeric(y)) %>%
    select(Date, Count)

  cz_cases = series_cases %>% call %>% rename(Cases=Count)
  cz_tests = series_tests %>% call %>% rename(Tested=Count)
  cz_daily_cases = series_daily_cases %>% call %>% rename(DailyCases=Count)
  cz_daily_tests = cz_tests %>% mutate(DailyTested=diff(c(0,Tested))) %>% select(-Tested)

  data = full_join_recurse(list(cz_cases, cz_deaths, cz_daily_cases, cz_daily_deaths, cz_tests, cz_daily_tests), by='Date') %>%
    mutate(Country=country) %>% select(Date, Country, everything())

  save(data, file='data/'%.%country%.%'_historical_'%.%t1(data$Date)%.%'.rda')


}

####################################################

load_data_mzcr = function(CountryCZ) {

  country = CountryCZ$name
  rda_file_country = 'data/'%.%country%.%'_historical_'%.%c(utilbox::t_day(),y_day())%.%'.rda' %>% `[`(file.exists(.)) %>% h1()
  rda_file_latest = list.files('data', country%.%'_latest_', full.names=TRUE) %>% file_sort_time() %>% h1()

  if(is_empty(rda_file_country)) stop('There is no file with the historical data for ', country,'.')
  if(is_empty(rda_file_country)) stop('There is no file with the latest data for ', country,'.')

  catn("Loading data for ",country,"...")
  load(rda_file_country)
  load(rda_file_latest)

  nlist(data, latest)

}

####################################################

load_data_individual = function(countries) {

  if(missing(countries)) countries = download_country_list(url_wom)

  Data = Latest = NULL
  for(country in split_rows(countries)) {

    rda_file_country = 'data/'%.%country$name%.%'_historical_'%.%c(y_day(lag=0:-1))%.%'.rda' %>%
      `[`(file.exists(.)) %>% h1()
    rda_file_latest = list.files('data', country$name%.%'_latest_', full.names=TRUE) %>%
      file_sort_time() %>% h1()

    if(is_empty(rda_file_country))
      error('No file with the historical data found for ', country$name,'.')
    if(is_empty(rda_file_latest))
      error('No file with the latest data found for ', country$name,'.')

    catn("Loading data for ",country$name,"...")
    load(rda_file_country)
    load(rda_file_latest)

    data %<>%  add_col(CurrentlyInfected=NA) %>%
      rename(Infected=CurrentlyInfected)

    Data %<>% bind_rows(data)
    Latest %<>% bind_rows(latest)

  }

  catn("Data loaded.")

  nlist(Data, Latest)


}

####################################################

process_lag = function(Data, Lag_by='Cases', lag_start=-10, lag_end=40, Country0='Italy', return_lagged_data=TRUE) {

  catn("Processing lag by ",Lag_by," ...")

  Countries = unique(Data$Country)
  if(Country0 %notin% Countries) stop('Missing data for Country0 (',Country0,').')

  lead2 = function(x, n, ...)
    if(n>=0) lead(x, n, ...) else lag(x, -n, ...)

  matchup = function(k, D_A, D_B, by='Cases') {
    matchup1 = function(k, D_A, D_B) mean2(abs(D_A[[by]]- lead2(D_B[[by]], k)))
    sapply(k, matchup1, D_A, D_B) #%>% setNames('k='%.%k)
  }

  Empty = tibble(Date=y_day(lag=-(as.Date(t_day())-as.Date('2020-01-01')):0))

  D0 = Data %>% filter(Country==Country0) %>%
    select(Country, Date, Cases, Deaths) %>%
    full_join(mutate(Empty, Country=.$Country[1]), ., by=c('Country','Date')) %>%
    select(Country, everything())

  ks = lag_start:lag_end
  K = tibble(Country=Countries, Lag=NA)
  Data_Lag = NULL
  for(C in Countries) {

    D = Data %>% filter(Country==C) %>%
      select(Country, Date, Cases, Deaths) %>%
      full_join(mutate(Empty, Country=.$Country[1]), ., by=c('Country','Date')) %>%
      select(Country, everything())

    y = matchup(ks, D0, D, Lag_by)
    k = t1(ks[which(abs(y-min(y,na.rm=TRUE))<1e-12)])
    K[K$Country==C,]$Lag = k

    if(return_lagged_data) {

      D0_Lagged = D0 %>%
        mutate(Date_lagged=as.character(as.Date(Date)+k)) %>%
        rename(Date0=Date)

      data = D %>% full_join(D0_Lagged, by=c('Date'='Date_lagged')) %>%
        mutate(Country.x=C, Country.y=Country0) %>%
        rename(Country=Country.x, Country0=Country.y) %>%
        setNames(sub('[.]y','0',sub('[.]x','',names(.))))

      Data_Lag %<>% bind_rows(data)

    }

  }

  if(return_lagged_data) {
    Data_Lag %<>% filter(!is.na(Cases) | !is.na(Cases0)) %>%
      filter(ifelse(is.na(Cases),0,Cases)>0 | ifelse(is.na(Cases0),0,Cases0)>0)
  }

  Lag_by = 'Lag_'%.%Lag_by
  K %<>% rename(!!Lag_by:=Lag)
  nlist(K, Data_Lag) %>% list_clean()

}

##########################################################

CountryCZ = tibble(name='Czechia', url=url_mzcz)
available_countries_wom = download_country_list(url_wom)

#some_fresh_data_present = FALSE

if(!do_download_data) do_download_data = length(list.files('data', t_day()))==0

if(do_download_data) {

  download_data_ocdc(url_ocdc, do_force_fresh_data)
  download_data_mzcr(CountryCZ)
  download_data_wom(available_countries_wom, url_wom)

}

if(do_load_data || !exists('Data') || !exists('Latest')) {

  Data_ocdc = read_data_ocdc(url_ocdc)
  Latest_ocdc = Data_ocdc %>% group_by(Country) %>% slice(n()) %>% ungroup()

  data_cz = load_data_individual(CountryCZ)
  DataCZ = data_cz$Data
  LatestCZ = data_cz$Latest

  data = load_data_individual(available_countries_wom)
  Data = data$Data
  Latest = data$Latest

  Data %<>% bind_rows(DataCZ, .)
  Latest %<>% bind_rows(LatestCZ, .)

  PopData = Data_ocdc %>% mutate(Country=recode(Country, 'Czech_Republic'='Czechia',
      'United_States_of_America'='USA', 'United_Kingdom'='UK', 'South_Korea'='S. Korea')) %>%
    group_by(Country) %>% slice(1) %>% ungroup() %>% select(Country, Population) %>%
    add_row(Country='Hong Kong', Population=7500000)

  # Check for some country's population missing
  PopData %>% anti_join(Data, ., by='Country') %>% group_by(Country) %>% slice(1) %>% ungroup() %>% nrow() %>% {stopifnot(.==0)}

  Data %<>% left_join(PopData, by='Country') %>%
    mutate_at(vars(Cases,Deaths,DailyCases,DailyDeaths), ~ifelse(.x==0, NA, .x)) %>%
    mutate(CasesPop=Cases, DeathsPop=Deaths, TestedPop=Tested, InfectedPop=Infected) %>%
    mutate_at(vars(ends_with("Pop")), ~.x/Population*PerPopulation)
    #mutate(CasesPop=Cases/Population*PerPopulation, DeathsPop=Deaths/Population*PerPopulation,
    #       TestedPop=Tested/Population*PerPopulation, InfectedPop=Infected/Population*PerPopulation)

}

#if(do_process_lag && (do_force_lag_calculation || some_fresh_data_present || !exists('K'))) {

K_Cases = process_lag(Data, 'Cases', Country0=Country0)
Data_Lag_Cases = K_Cases$Data_Lag

K_Deaths = process_lag(Data, 'Deaths', Country0=Country0, return_lagged_data=FALSE)
K = full_join(K_Cases$K, K_Deaths$K, by='Country') %>%
  mutate(Lag_diff=Lag_Deaths-Lag_Cases)

KK = bind_cols(K %>% arrange(Lag_Cases), K %>% arrange(Lag_Deaths), K %>% arrange(Lag_diff)) %T>% print(n=100)

Data4 = Data_Lag_Cases %>% filter(Country %in% Countries_focus_narrow)
DataCZ2 = Data_Lag_Cases %>% filter(Country=='Czechia')

### PRODUCE COMPARISON PLOTS ###
catn("Plotting comparisons with Italy ...")
descriptions = c()
plots1 = plots2 = list()
for(C in Countries_focus_narrow) {

  D2 = Data4 %>% filter(Country==C)

  write.table(D2, file='tables/'%.%C%.%'_vs_'%.%Country0%.%'_'%.%t_day()%.%'.txt', row.names=FALSE, quote=FALSE)

  D3 = bind_rows(D2 %>% select(-ends_with('0')),
                 D2 %>% mutate(Date0=Date) %>% select(ends_with('0')) %>% rename_all(~sub('0$','',.x))) %>%
    mutate(Date==sub('[0-9]+-','',Date))

  annot = list(yref="paper", xref="x", y=1, x=0.5, xanchor='left', showarrow=FALSE)

  plot1 = plot_ly(D3, x=~Date, y=~Cases, color=~Country, name=~Country, colors=Colors(reverse=C<Country0), type='scatter', mode='lines') %>%
    layout(annotations=annot %append% list(text='<b>'%.%C%.%'</b> '%.%Country0%.%' (cases)'), xaxis=list(title="", showticklabels = FALSE))

  plot2 = plot_ly(D3, x=~Date, y=~Deaths, color=~Country, name=~Country, colors=Colors(reverse=C<Country0), type='scatter', mode='lines') %>%
    layout(annotations=annot %append% list(text='<b>'%.%C%.%'</b> '%.%Country0%.%' (deaths)'), xaxis=list(title="", showticklabels = FALSE))

  plots1 %<>% append(list(plot1))
  plots2 %<>% append(list(plot2))

}

descriptions %<>% c(plot_lag="Comparison of the lag (in number of days) behind "%.%Country0)
title = "Total number of cases and deaths: comparison with "%.%Country0%.%"\n("%.%data_source%.%")"
plot_lag = do.call(subplot, plots1 %append% plots2 %append% list(nrows=2, shareY = FALSE, titleY=TRUE, titleX = TRUE)) %>%
  layout(title = title, showlegend = FALSE)
rm(annot, plot1, plot2, plots1, plots2)

### PRODUCE PLOTS FOR ALL COUNTRIES ###
catn("Plotting ...")

Latest %<>% arrange(desc(Cases))

#plot_infect_bar = plot_ly(data=Latest, x=~Country, y=~Infected, color=~Country, name=~Country, colors=Colors(), type='bar') %>%
#  layout(title = "Number of active infections", xaxis = list(title = "Country", categoryorder = "array", categoryarray = ~Country))
#descriptions %<>% c(plot_infect_bar="Barplot of the <b>number of active infections</b> for various countries")

descriptions %<>% c(plot_cases_bar="Barplot of the <b>total number of cases</b> for various countries")
title = "Total number of cases (as of "%.%t_day()%.%")\n("%.%data_source%.%")"
plot_cases_bar = plot_ly(data=Latest, x=~Country, y=~Cases, color=~Country, name=~Country, colors=Colors(), type='bar') %>%
  layout(title = title, xaxis = list(title = "Country", categoryorder = "array", categoryarray = ~Country))

descriptions %<>% c(plot_deaths_bar="Barplot of the <b>total number of deaths</b> for various countries")
title = "Total number of deaths (as of "%.%t_day()%.%")\n("%.%data_source%.%")"
plot_deaths_bar = plot_ly(data=Latest, x=~Country, y=~Deaths, color=~Country, name=~Country, colors=Colors(), type='bar') %>%
  layout(title = title, xaxis = list(title = "Country", categoryorder = "array", categoryarray = ~Country))

descriptions %<>% c(plot_recovered_bar="Barplot of the <b>total number of recovered patients</b> for various countries")
title = "Number of recovered patients (as of "%.%t_day()%.%")\n("%.%data_source%.%")"
plot_recovered_bar = plot_ly(data=Latest, x=~Country, y=~Recovered, color=~Country, name=~Country, colors=Colors(), type='bar') %>%
  layout(title = title, xaxis = list(title = "Country", categoryorder = "array", categoryarray = ~Country))


Data4 = Data %>% mutate(Date=sub('[0-9]+-','',Date))
if(countries_focus=="broad") Data4 %<>% filter(Country %in% Countries_focus_broad)
if(countries_focus=="narrow") Data4 %<>% filter(Country %in% Countries_focus_narrow)


descriptions %<>% c(plot_infect="Evolution of the <b>number of active infections</b> over time for various countries")
title = "Number of active infections (as of "%.%t_day()%.%")\n("%.%data_source%.%")"
plot_infect = plot_ly(data=Data4, x=~Date, y=~Infected, color=~Country, name=~Country, colors=Colors(), type='scatter', mode='lines') %>%
  layout(title = title, yaxis = list(title = "Active infections"))

descriptions %<>% c(plot_cases="Evolution of the <b>number of cases</b> over time for various countries")
title = "Total number of cases (as of "%.%t_day()%.%")\n("%.%data_source%.%")"
plot_cases = plot_ly(data=Data4, x=~Date, y=~Cases, color=~Country, name=~Country, colors=Colors(), type='scatter', mode='lines') %>%
  layout(title = title, yaxis = list(title = "Cases"))

descriptions %<>% c(plot_deaths="Evolution of the <b>number of deaths</b> over time for various countries")
title = "Total number of deaths (as of "%.%t_day()%.%")\n("%.%data_source%.%")"
plot_deaths = plot_ly(data=Data4, x=~Date, y=~Deaths, color=~Country, name=~Country, colors=Colors(), type='scatter', mode='lines') %>%
  layout(title = title, yaxis = list(title = "Deaths"))

descriptions %<>% c(plot_tests="Evolution of the <b>number of diagnostic tests</b> for COVID-19 (currently only Czechia)")
title = "Total number of diagnostic tests for COVID-19 (as of "%.%t_day()%.%")\n("%.%data_source%.%")"
plot_tests = plot_ly(data=Data4, x=~Date, y=~Tested, color=~Country, name=~Country, colors=Colors(), type='scatter', mode='lines') %>%
  layout(title = title, yaxis = list(title = "Tested"))


descriptions %<>% c(plot_infectpop="Evolution of the <b>number of active infections per population</b> over time for various countries")
title = "Number of currently infected per "%.%PerPopulation%.%" citizens\n("%.%data_source%.%")"
plot_infectpop = plot_ly(data=Data4, x=~Date, y=~InfectedPop, color=~Country, name=~Country, colors=Colors(), type='scatter', mode='lines') %>%
  layout(title = title, yaxis = list(title = "Active infections"))

descriptions %<>% c(plot_casespop="Evolution of the <b>number of cases per population</b> over time for various countries")
title = "Total number of cases per "%.%PerPopulation%.%" citizens\n("%.%data_source%.%")"
plot_casespop = plot_ly(data=Data4, x=~Date, y=~CasesPop, color=~Country, name=~Country, colors=Colors(), type='scatter', mode='lines') %>%
  layout(title = title, yaxis = list(title = "Cases"))

descriptions %<>% c(plot_deathspop="Evolution of the <b>number of deaths per population</b> over time for various countries")
title = "Total number of deaths per "%.%PerPopulation%.%" citizens\n("%.%data_source%.%")"
plot_deathspop = plot_ly(data=Data4, x=~Date, y=~DeathsPop, color=~Country, name=~Country, colors=Colors(), type='scatter', mode='lines') %>%
  layout(title = title, yaxis = list(title = "Deaths"))

descriptions %<>% c(plot_testspop="Evolution of the <b>number of diagnostic tests per population</b> for COVID-19 (currently only Czechia)")
title = "Total number of diagnostic tests for COVID-19 per "%.%PerPopulation%.%" citizens\n("%.%data_source%.%")"
plot_testspop = plot_ly(data=Data4, x=~Date, y=~TestedPop, color=~Country, name=~Country, colors=Colors(), type='scatter', mode='lines') %>%
  layout(title = title, yaxis = list(title = "Tested"))

descriptions %<>% c(plot_caserate="Evolution of the <b>case rate</b> (i.e. DailyCases/Cases) over time for various countries")
title = "Daily case rate\n("%.%data_source%.%")"
plot_caserate = Data4 %>% mutate(DailyCaseRate=DailyCases/Cases) %>%
  plot_ly(x=~Date, y=~DailyCaseRate, color=~Country, name=~Country, colors=Colors(), type='scatter', mode='lines') %>%
  layout(title = title, yaxis = list(title = "Daily case rate"))

descriptions %<>% c(plot_deathrate="Evolution of the <b>death rate</b> (i.e. DailyDeaths/Deaths) over time for various countries")
title = "Case-fatality ratio (i.e. death rate)\n("%.%data_source%.%")"
plot_deathrate = Data4 %>% mutate(CaseFatalityRatio=Deaths/Cases) %>%
  plot_ly(x=~Date, y=~CaseFatalityRatio, color=~Country, name=~Country, colors=Colors(), type='scatter', mode='lines') %>%
  layout(title = title, yaxis = list(title = "Daily death rate"))

descriptions %<>% c(plot_testrate="Evolution of the <b>test rate</b> (i.e. DailyDeaths/Deaths) over time for various countries")
title = "Daily test rate\n("%.%data_source%.%")"
plot_testrate = Data4 %>% mutate(DailyTestRate=DailyTested/Tested) %>%
  plot_ly(x=~Date, y=~DailyTestRate, color=~Country, name=~Country, colors=Colors(), type='scatter', mode='lines') %>%
  layout(title = title, yaxis = list(title = "Daily test rate"))

plot_infect_log = plot_infect %>% layout(yaxis = list(type = "log"))
plot_cases_log = plot_cases %>% layout(yaxis = list(type = "log"))
plot_deaths_log = plot_deaths %>% layout(yaxis = list(type = "log"))
plot_tests_log = plot_tests %>% layout(yaxis = list(type = "log"))
plot_caserate_log = plot_caserate %>% layout(yaxis = list(type = "log"))
plot_deathrate_log = plot_deathrate %>% layout(yaxis = list(type = "log"))
plot_testrate_log = plot_testrate %>% layout(yaxis = list(type = "log"))
descriptions %<>% c(
    plot_infect_log=descriptions['plot_infect']%.%" (logarhitmic scale)",
    plot_cases_log=descriptions['plot_cases']%.%" (logarhitmic scale)",
    plot_deaths_log=descriptions['plot_deaths']%.%" (logarhitmic scale)",
    plot_tests_log=descriptions['plot_tests']%.%" (logarhitmic scale)",
    plot_caserate_log=descriptions['plot_caserate']%.%" (logarhitmic scale)",
    plot_deathrate_log=descriptions['plot_deathrate']%.%" (logarhitmic scale)",
    plot_testrate_log=descriptions['plot_testrate']%.%" (logarhitmic scale)")

### DO REGRESSION FOR CZECHIA ###
min_Cases = 250
DataCZ250 = DataCZ %>% filter(Date>'2020-03') %>% filter(Cases>=min_Cases)
m_total = lm(Cases~Tested, data=DataCZ250) %T>% summary()
m_daily = lm(DailyCases~DailyTested, data=DataCZ250) %T>% summary()

DataCZ250 %<>% mutate(Predicted_Cases=m_total$fitted.values,
                      Predicted_DailyCases=m_daily$fitted.values,
                      DateS=DataCZ250$Date %>% sub('[0-9]+-','',.))

title = '<b>Czech Republic</b>\n<i>Data</i>: all days after '%.%h1(DataCZ250$Date)%.%
        ' (i.e. days with at least '%.%min_Cases%.%' cases)'%.%
        ')\n<i>Model</i>: simple LRM (i.e. Cases~Tested), R<sup>2</sup>='
title_total = title%.%signif(summary(m_total)$r.squared,3)
title_daily = title%.%signif(summary(m_daily)$r.squared,3)

plot_lm_total = plot_ly(DataCZ250, x=~Tested, y=~Cases, name='observed', type='scatter', mode='line', colors=Colors()) %>%
  add_trace(x=~Tested, y=~Predicted_Cases, name='predicted', mode='lines+markers', color='red') %>%
  #layout(title=FALSE, yaxis=list(type='log'), xaxis=list(type='log')) %>%
  add_annotations(yref="paper", xref="paper", y=1.05, x=0, text=title_total, align='left', showarrow=FALSE, font=list(size=17)) %>%
  add_annotations(yref="y", xref="x", x=DataCZ250$Tested, y=DataCZ250$Cases, text=DataCZ250$DateS,
                  showarrow=TRUE, arrowhead = 4, arrowsize = .4, ax = -10, ay = -50)

plot_lm_daily = plot_ly(DataCZ250, x=~DailyTested, y=~DailyCases, name='observed', type='scatter', mode='markers', colors=Colors()) %>%
  add_trace(x=~DailyTested, y=~Predicted_DailyCases, name='predicted', mode='lines+markers', color='red') %>%
  #layout(title=FALSE, yaxis=list(type='log'), xaxis=list(type='log')) %>%
  add_annotations(yref="paper", xref="paper", y=1.05, x=0, text=title_daily, align='left', showarrow=FALSE, font=list(size=17)) %>%
  add_annotations(yref="y", xref="x", x=DataCZ250$DailyTested, y=DataCZ250$DailyCases, text=DataCZ250$DateS,
                  showarrow=TRUE, arrowhead = 4, arrowsize = .4, ax = -10, ay = -50)

descriptions %<>% c(
  plot_lm_daily="Czechia: Linear fit of the daily number of cases against the daily number of performed diagnostic tests for COVID-19",
  plot_lm_total="Czechia: Linear fit of the total number of cases against the total number of performed diagnostic tests for COVID-19")

### END OF REGRESSION FOR CZECHIA ###

ls(pattern='^plot') %>%
  split_into_groups(6) %>%
  lapply(paste, collapse='\t  ') %>%
  {do.call('paste', . %append% list(sep='\n\t'))} %>%
  catn("\nCreated plot objects:\n\n\t", .,'\n')

if(do_save_plotly_to_file) {
  catf = hijack(catn, file='index.html', append=TRUE)
  catf("<!DOCTYPE HTML>\n<html lang='en'>\n<head>\n<title>Pecanka Consulting: Coronavirus plots</title>", append=FALSE)
  catf("<link rel='stylesheet' type='text/css' href='style.css'></head>\n<body><div class='main'>")
  catf("<div class='head'>CORONOVIRUS: COVID-19</div><p>")
  catf("<div class='descriptions'>A collection of plots showing the current state and the historical evolutionof the COVID-19")
  catf(" pandemic around the world.<p>Author: <a href='https://www.pecanka.net'>Jakub Pecanka, PhD</a><p>"%.%data_source)
  catf("</div><div class='list'><ul>")
  ps = ls(pattern='^plot') %>% {.[order(descriptions[.])]}
  for(p in ps) {
    catn("Saving plot '"%.%p%.%"' to file ...")
    filename = '.~'%.%p%.%'.html'
    filename2 = 'plots_plotly/'%.%filename
    htmlwidgets::saveWidget(as_widget(get(p)), file=filename)
    file.rename(filename, filename2)
    catf("<div class='link'><li><a href='",filename2,"'>",descriptions[p],"</a></li></div>")
  }
  catf("</ul></div><p></div></body>\n</html>")
  rm(catf)
  catn("Finished.")
}
