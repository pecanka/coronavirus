#source('d:/Dropbox/Projects/CoronaVirus/get_corona_data.R')
options(scipen=5)

setwd2()

llib(rvest, stringr, tidyr, lubridate, plotly, rjson, readxl)

#########################################################
url_ocdc = 'https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-'%.%t_day()%.%'.xlsx'

url_wom = 'https://www.worldometers.info/coronavirus/'
url_mzcz = 'https://onemocneni-aktualne.mzcr.cz/covid-19'
url_mzcz_api = 'https://onemocneni-aktualne.mzcr.cz/api/v1/covid-19/'

do_download_data = !FALSE
do_load_data = FALSE

do_force_fresh_data = FALSE
do_use_any_existing_data = FALSE

do_process_lag = TRUE

do_plot = TRUE
do_plot_lag = TRUE
do_plot_bar = TRUE
do_plot_ts = TRUE
do_plot_lm = TRUE

do_save_plotly_to_file = TRUE

format_for_plots = 'html'
#format_for_plots = 'png'

countries_focus = TRUE

PerPopulation = 1000000
Country0 = 'Italy'
Countries_focus_narrow = c("Netherlands","Germany","USA","Czechia")

Countries_focus_broad = c('Czechia','Italy','Germany','Netherlands',
  'Spain','USA','S. Korea','Austria','UK','Canada','Belgium','Denmark',
  'Norway','Poland','Sweden','Switzerland','France','Portugal','Brazil',
  'Israel','Australia','Ireland','Chile','Romania','Japan','Luxembourg',
  'Finland','Mexico','Greece','Slovakia')
#Countries_focus_broad = c('Czechia','Italy')

### TEMPORARILY ADD THE CZECH DEATHS MANUALLY ###
#cz_daily_deaths = tribble(~Date, ~DailyDeaths,
#  '2020-03-22', 1, '2020-03-23', 0, '2020-03-24', 2,
#  '2020-03-25', 3, '2020-03-26', 3, '2020-03-27', 0,
#  '2020-03-28', 2, '2020-03-29', 5, '2020-03-30', 7)

#cz_deaths = cz_daily_deaths %>% mutate(Deaths=cumsum(DailyDeaths)) %>% select(-DailyDeaths)
### TEMPORARILY ADD THE CZECH DEATHS MANUALLY ###

data_source = "Based on data from <a href='"%.%url_wom%.%"'>Worldometers</a>, "%.%
  "<a href='"%.%url_ocdc%.%"'>OCDC</a>"%.%" and <a href='"%.%url_mzcz%.%"'>mzcr.cz</a>"

#########################################################

download_country_list = function(url, name_start='wom__list-of-countries_') {

  pattern = name_start%.%ifelse(do_use_any_existing_data,"",t_day())
  f = list.files('data',pattern, full.names=TRUE) %>% file_sort_time() %>% h1()

  if(!do_force_fresh_data && length(f)>0) {
    catn("Loading list of countries ...", flush_cycle=10)
    load(f)
  } else {
    catn("Getting list of countries from url '",url,"' ...")
    html = read_html(url)
    name = html %>% html_nodes('.mt_a') %>% html_text()
    surl = html %>% html_nodes('.mt_a') %>% html_attr('href')
    countries = tibble(name, prefix='wom_', url=url%.%surl) %>% unique()
    save(countries, file='data/'%.%name_start%.%t_day()%.%'.rda')
  }

  countries

}


html_extract_latest_mzcr = function(html, country) {
  Tested = html %>% html_nodes('#count-test') %>% html_text() %>% force_as_integer()
  Cases = html %>% html_nodes('#count-sick') %>% html_text() %>% force_as_integer()
  Deaths = html %>% html_nodes('#count-dead') %>% html_text() %>% force_as_integer()
  Recovered = html %>% html_nodes('#count-recover') %>% html_text() %>% force_as_integer()
  tibble(Tested, Cases, Deaths, Recovered) %>%
  setNames(c('Tested','Cases','Deaths','Recovered')) %>%
  mutate(Country=country, Date=t_day()) %>%
  select(Country, Date, everything())
}

html_extract_latest_wom = function(html, country) {
  html %>% html_nodes('div .maincounter-number span') %>%
    html_text() %>%
    force_as_integer() %>%
    setNames(c('Cases','Deaths','Recovered')) %>%
    t() %>%
    as_tibble() %>%
    mutate(Country=country, Date=t_day(), Tested=NA) %>%
    select(Country, Date, Tested, everything())
}

html_extract = function(x, pattern, names) {
  x %>% str_extract(pattern) %>% str_extract("[\\[].+[\\]]") %>%
    gsub('\"nan\"|null','NA',.) %>% sub('[\\[]','c(',.) %>% sub(']',')',.) %>%
    lapply(function(x) eval(parse(text=x))) %>%
    setNames(names)
}

####################################################

download_latest_all_wom = function(url='https://www.worldometers.info/coronavirus/') {

  catn("Reading latest counts for all countries from WOM ...")

  html = try(read_html(url))
  if('try-error' %in% class(html)) error("Something went wrong when reading '",url,"'.")

  latest_all = html %>% html_node('table') %>% html_table() %>% as_tibble()

  names(latest_all) = names(latest_all) %>% gsub('[,/ ]','_',.) %>% gsub('Tot.*Cases','TotCases',.)

  latest_all %<>% select(-starts_with("Country")) %>%
    mutate_if(~is.character(.x), force_as_integer) %>%
    mutate(Country=latest_all$Country_Other, Date=t_day(), PerPopulation=1e6) %>%
    rename(CriticalCases=Serious_Critical,
           TotalCasesPop=TotCases_1M_pop,
           TotalDeathsPop=Deaths_1M_pop,
           TotalTestsPop=Tests_1M_pop) %>%
    select(Country, Date, everything())

  out_file_latest_all = 'data/wom_latest_all_countries_@date.rda'
  save(latest_all, file=out_file_latest_all %>% sub('@date',t_day('%Y-%m-%d-%H%M%S'),.))

  invisible(latest_all)
}

download_data_ocdc = function(url, force=FALSE) {
  file = 'data/'%.%separate_path(url)$filename
  if(force || !file.exists(file)) {
    if(!url_exists(url)) {
      warn("File '",url,"' does not seem to exist.")
    } else {
      download.file(url, file, mode="wb")
    }
  }
}

####################################################

read_data_ocdc = function(url, max_lag=3, lag=1) {

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
      'Czech_Republic'='Czechia', 'United_States_of_America'='USA',
      'United_Kingdom'='UK', 'South_Korea'='S. Korea'))

}

####################################################

download_data_wom = function(available_countries, url) {

  if(missing(available_countries)) available_countries = download_country_list(url)

  Data = Latest = NULL
  for(country in split_rows(available_countries)) {

    country_url = country %>% pull(url)
    country_prefix = country %>% pull(prefix)
    country_name = country %>% pull(name)

    out_file = 'data/'%.%country_prefix%.%country_name%.%'_@type_@date.rda'

    catn("Scrapping data for ",country_name," from WOM ...")
    html = try(read_html(country_url))
    if('try-error' %in% class(html)) next
    #some_fresh_data_present = TRUE

    latest = html_extract_latest_wom(html, country_name)
    save(latest, file=out_file %>% sub('@type','latest',.) %>% sub('@date',t_day('%Y-%m-%d-%H%M%S'),.))
      #'data/'%.%country_prefix%.%country_name%.%'_latest_'%.%t_day('%Y-%m-%d-%H%M%S')%.%'.rda')

    series = html %>% as.character() %>% regmatches(gregexpr('series:[^}]+[}]',.)) %>% unlist()
    series_names = series %>% str_extract("name: '[^,]+',") %>% str_extract("'.+'") %>% gsub("[' ]","",.)
    series_dates = html %>% as.character() %>% regmatches(gregexpr('xAxis:[^}]+[}]',.)) %>% unlist() %>%
      html_extract("categories: [\\[][^\\]]+[\\]]", series_names)
    series_data = series %>% html_extract("data: [\\[][^\\]]+[\\]]", series_names)

    data = lapply(1:length(series_names),
                  function(i) do.call(bind_cols, list(series_dates[i], series_data[i])) %>%
                                setNames(c('Date',series_names[i]))) %>%
      setNames(series_names) %>%
      `[`(!duplicated(series_names)) %>%
      join_recurse(by='Date', join='full_join') %>%
      mutate(Country=country_name, Date=parse_date_time(Date, orders='md', tz=NULL) %>% format()) %>%
      select(Country, Date, everything())

    save(data, file=out_file %>% sub('@type','historical',.) %>% sub('@date',t1(data$Date),.))
     #'data/'%.%country_prefix%.%country_name%.%'_historical_'%.%t1(data$Date)%.%'.rda')

  }

  catn("Data for all countries scrapped.")

}

####################################################

download_data_mzcr = function(CountryCZ) {

  country_url = CountryCZ$url
  country_prefix = CountryCZ$prefix
  country_name = CountryCZ$name
  out_file = 'data/'%.%country_prefix%.%country_name%.%'_@type_@date.rda'

  catn("Getting latest data for ",country_name," from MZCR ...")
  html = try(read_html(country_url))
  if('try-error' %in% class(html)) error("Data not found.")

  latest = html_extract_latest_mzcr(html, country_name)
  save(latest, file=out_file %>% sub('@type','latest',.) %>% sub('@date',t_day('%Y-%m-%d-%H%M%S'),.))

  catn("Processing ",country_name,"...")

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

  data = full_join_recurse(list(cz_cases, cz_daily_cases, cz_tests, cz_daily_tests), by='Date') %>%
    mutate(Country=country_name) %>% select(Date, Country, everything())

  save(data, file=out_file %>% sub('@type','historical',.) %>% sub('@date',t1(data$Date),.))


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

load_data_individual = function(countries) {

  if(missing(countries)) countries = download_country_list(url_wom)

  Data = Latest = NULL
  for(country in split_rows(countries)) {

    rda_file_country = 'data/'%.%country$prefix%.%country$name%.%'_historical_'%.%c(y_day(lag=0:-2))%.%'.rda' %>%
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

  if(is_empty(rda_file_latest)) stop('There is no file with the latest data for ', country_name,'.')

  catn("Loading latest data for all countries from file '",rda_file_latest,"'...")
  load(rda_file_latest)

  latest_all


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

CountryCZ = tibble(name='Czechia', prefix='mzcr_', url=url_mzcz)
available_countries_wom = download_country_list(url_wom) %>%
  `if`(countries_focus, filter(., name %in% Countries_focus_broad), .)

#some_fresh_data_present = FALSE

if(!do_download_data) do_download_data = length(list.files('data', t_day()))==0

if(do_download_data) {

  download_latest_all_wom()
  download_data_ocdc(url_ocdc, do_force_fresh_data)
  download_data_mzcr(CountryCZ)
  download_data_wom(available_countries_wom, url_wom)

}

if(do_load_data || do_download_data || !exists('Data') || !exists('Latest')) {

  Latest_all = load_latest_all_wom()

  ## Read OCDC data from a file
  DataOCDC = read_data_ocdc(url_ocdc)
  LatestOCDC = DataOCDC %>% group_by(Country) %>% slice(n()) %>% ungroup()

  ## Read MZCR data from a file
  dataCZ = load_data_individual(CountryCZ)
  LatestCZ = dataCZ$Latest
  DataCZ = dataCZ$Data

  ## Add the OCDC data for Czechia into the MZCR data (done because (Daily)Deaths
  ## are missing from MZCR data. Columns Population, ID, Code are dropped because
  ## they are appended below)
  DataCZ = DataOCDC %>% filter(Country=='Czechia') %>%
    select(-Country, -Population, -ID, -Code) %>%
    rename_all(~.x%.%'OCDC') %>%
    select(Date=DateOCDC, Deaths=DeathsOCDC, DailyDeaths=DailyDeathsOCDC, everything()) %>%
    left_join(DataCZ, ., by='Date') %>% arrange(Date) %T>% print(n=100)

  ## Read WOM data from files
  dataWOM = load_data_individual(available_countries_wom)
  Data = dataWOM$Data
  Latest = dataWOM$Latest

  ## Merge the WOM data with the MZCR data
  DataCZ1 = DataCZ %>%
    select(Date, Country, DeathsCZ=Deaths, DailyDeathsCZ=DailyDeaths,
           one_of(setdiff(names(.), names(Data))))

  Data %<>% full_join(DataCZ1) %>%
    mutate(Deaths=coalesce(Deaths, DeathsCZ), DailyDeaths=coalesce(DailyDeaths,DailyDeathsCZ)) %>%
    select(-ends_with('CZ')) %>%
    arrange(Country, Date) #%>% filter(Country=='Czechia')

  Latest %<>% full_join(LatestCZ) %>% arrange(Country)

  ## Extract the population and country codes from the OCDC data
  PopData = DataOCDC %>% group_by(Country) %>% slice(1) %>% ungroup() %>%
    select(Country, Population, ID, Code) %>%
    add_row(Country='Hong Kong', Population=7500000, ID='HK', Code='HKG')

  # Check for duplicated records
  Data %>% select(Date, Country) %>% anyDuplicated() %>% equals(0) %>% stopifnot()
  Latest %>% select(Country) %>% anyDuplicated() %>% equals(0) %>% stopifnot()

  # Check for some country's population missing
  PopData %>% anti_join(Data, ., by='Country') %>%
    group_by(Country) %>% slice(1) %>% ungroup() %>% nrow() %>% equals(0) %>%
    stopifnot()

  ## Add the population and country codes to the WOM data

  call_Pop = . %>% mutate_at(vars(ends_with("Pop_")), ~.x/Population*PerPopulation) %>%
    setNames(sub("_$",'',names(.)))

  Data %<>% left_join(PopData, by='Country') %>%
    mutate_at(vars(Cases,Deaths,DailyCases,DailyDeaths), ~ifelse(.x==0, NA, .x)) %>%
    mutate(CasesPop_=Cases, DeathsPop_=Deaths, TestedPop_=Tested, InfectedPop_=Infected) %>%
    call_Pop

  Latest %<>% left_join(PopData, by='Country') %>%
    mutate(CasesPop_=Cases, DeathsPop_=Deaths, TestedPop_=Tested, RecoveredPop_=Recovered) %>%
    call_Pop

  Latest_all %<>% left_join(PopData, by='Country') %>%
    mutate(ActiveCasesPop_=ActiveCases, CriticalCasesPop_=CriticalCases, NewDeathsPop_=NewDeaths) %>%
    call_Pop

  Latest %<>% left_join(Latest_all, by=c('Country','Date'), suffix=c('','_2'))

  if(!setequal(unique(Latest$Date), unique(Latest_all$Date)))
    error("Something seems to have gone wrong with the data, the dates in",
          "'Latest' and 'Latest_all' do not match. Try redownloading the data.")

  # Check for duplicated records
  Data %>% select(Date, Country) %>% anyDuplicated() %>% equals(0) %>% stopifnot()
  Latest %>% select(Country) %>% anyDuplicated() %>% equals(0) %>% stopifnot()

  Data4 = Data %>% mutate(Date=sub('[0-9]+-','',Date)) %>%
    mutate(DailyInfected=diff(c(NA,Infected)),
           DailyInfectedRatio=DailyInfected/Cases,
           DailyCasesRatio=DailyCases/Cases,
           DailyDeathsRatio=DailyDeaths/Cases,
           DailyTestedRatio=DailyTested/Tested)

  if(countries_focus) {
    call = . %>% filter(Country %in% Countries_focus_broad)
    Data4 %<>% call
    Latest %<>% call
  }

} # if(do_load_data ...)

#Data4 = Data_Lag_Cases %>% filter(Country %in% Countries_focus_narrow)
#DataCZ2 = Data_Lag_Cases %>% filter(Country=='Czechia')

###############################################################

descriptions = ylabs = c()

set_axis_lab = function(d, pattern1='.+<[bB]>', pattern2='<.+') {
  d %>% sub(pattern1,'',.) %>% sub(pattern2,'',.) %>% str_trim() %>% tolower %>% setNames(names(d))
}

set_titles = function(descriptions, what='Progression of the') {
  descriptions %>% set_axis_lab() %>%
    inset(seq_along(.), ifelse(regexpr('population',.)<0, ., sub("population",N2T(PerPopulation)%.%" inhabitants",.))) %>%
    gsub('italy','Italy',.) %>%
    paste(toupperfirst(what),.,"(as of "%.%t_day()%.%")") %>%
    setNames(names(descriptions))
}

#########################
### DO LAG COMPARISON ###
#########################

if(do_process_lag || !exists("Data_Lag_Cases")) {

  K_Cases = process_lag(Data, 'Cases', Country0=Country0)
  Data_Lag_Cases = K_Cases$Data_Lag

  K_Deaths = process_lag(Data, 'Deaths', Country0=Country0, return_lagged_data=FALSE)
  K = full_join(K_Cases$K, K_Deaths$K, by='Country') %>%
    mutate(Lag_diff=Lag_Deaths-Lag_Cases)

  KK = bind_cols(K %>% arrange(Lag_Cases), K %>% arrange(Lag_Deaths), K %>% arrange(Lag_diff)) %T>% print(n=100)

}

text_source = "<span style='font-size: 90%;'><i>Source: "%.%data_source%.%"</i></span>"
text_author = "<span style='font-size: 90%;'><i>Author: Jakub Pecanka (Pecanka Consulting,"%.%
              " <a href='https://www.pecanka.net'>www.pecanka.net</a>)</i></span>"

annot_bottom = list(yref="paper", xref="paper", align='left', xanchor='center', showarrow=FALSE)
annot_source = list(text=text_source, y=-0.19, x=-0.04, yref="paper", xref="paper", xanchor='left', showarrow=FALSE)
annot_author = list(text=text_author, y=-0.19, x=1.12, yref="paper", xref="paper", xanchor='right', showarrow=FALSE)

if(do_plot_lag) {

  catn("Plotting comparisons with Italy ...")
  plots_Cases = plots_Deaths = list()
  for(C in Countries_focus_narrow) {

    D3 = Data_Lag_Cases %>% filter(Country==C) %T>%
      write.table(file='tables/'%.%C%.%'_vs_'%.%Country0%.%'_'%.%t_day()%.%'.txt', row.names=FALSE, quote=FALSE) %>%
      {bind_rows(select(., -ends_with('0')),
                 mutate(., Date0=Date) %>% select(ends_with('0')) %>% rename_all(~sub('0$','',.x)))} %>%
      mutate(Date=sub('[0-9]+-','',Date))

    #Col = c(Colors(12, skip=11), Colors(1, skip=0)) %>% pif(C<Country0, rev)
    Col = c("#cc4125","#64dd60") %>% setNames(c(Country0, C)) %>% pif(C<Country0, rev)

    for(v in c('Cases','Deaths')) {
      D3 %>% mutate(y=!!sym(v), col=Col[Country]) %T>% assign('D3b', ., envir=.GlobalEnv) %>%
        plot_ly(x=~Date, y=~y, color=~Country, name=~Country, colors=Col,
                type='scatter', mode='lines', showlegend=TRUE) %>%
        layout(legend=list(x = 0.1, y = 0.5),
               yaxis=list(title=ifelse(C==Countries_focus_narrow[1], v, "")),
               xaxis=list(title="", showticklabels=FALSE)) %>%
        add_trace(x=~Date, y=~y, data=D3b %>% filter(Country==C), colors=Col[C]) %>%
        add_annotations(text = C,        x = 0.1, y = 0.9, xref='paper', yref='paper', showarrow = FALSE,
                        xanchor='left', font = list(color = Col[2-as.numeric(C<Country0)], size = 14)) %>%
        add_annotations(text = Country0, x = 0.1, y = 0.8, xref='paper', yref='paper', showarrow = FALSE,
                        xanchor='left', font = list(color = Col[1+as.numeric(C<Country0)], size = 14)) %>%
        list() %>%
        append(get('plots_'%.%v, envir=.GlobalEnv), .) %>%
        assign('plots_'%.%v, ., envir=.GlobalEnv)
    }

  }

  descriptions %<>% c(plot_lagItaly="Country comparison: <b>LAGs BEHIND "%.%toupper(Country0)%.%"</b> (in number of days)")

  title = "Progression of the number of cases and deaths: a comparison with "%.%Country0
  explain_lag = "Each country's progression of case counts is matched up against "%.%Country0%.%
    "'s case count, which determines the number of days the given country lags behind "%.%Country0%.%".\n"%.%
    "The resulting matching is shown on the top row, while the bottom row shows the death count progression"%.%
    " with "%.%Country0%.%"'s counts shifted by the determined lag."

  plot_lagItaly = append(plots_Cases, plots_Deaths) %>%
    append(list(nrows=2, shareY = FALSE, titleY=TRUE, titleX = TRUE)) %>%
    do.call(subplot, .) %>%
    layout(title=list(text=title, y=1.35), showlegend = FALSE) %>%
    layout(margin=list(l=50, r=50, b=90, t=50, pad=1)) %>%
    layout(plot_bgcolor='black', paper_bgcolor='black', font=list(color='white')) %>%
    layout(annotations=annot_bottom %modify% list(text=explain_lag, y=-0.12, x=0.48)) %>%
    layout(annotations=annot_source) %>%
    layout(annotations=annot_author %modify% list(x=1))

} # if(do_plot_lag)

####################
### DO BAR PLOTS ###
####################

if(do_plot_bar) {

  catn("Plotting barplots...")

  descriptions %<>% c(plot_bar_Cases="Barplot: <b>NUMBER OF CASES</b>")
  descriptions %<>% c(plot_bar_ActiveCases="Barplot: <b>NUMBER OF ACTIVE CASES</b>")
  descriptions %<>% c(plot_bar_CriticalCases="Barplot: <b>NUMBER OF CRITICAL/SERIOUS CASES</b>")
  descriptions %<>% c(plot_bar_Deaths="Barplot: <b>NUMBER OF DEATHS</b>")
  descriptions %<>% c(plot_bar_Recovered="Barplot: <b>NUMBER OF RECOVERED PATIENTS</b>")
  descriptions %<>% c(plot_bar_TotalTests="Barplot: <b>NUMBER OF DIAGNOSTIC TESTS</b>")
  descriptions %<>% c(plot_bar_NewDeaths="Barplot: <b>NUMBER OF NEW DEATHS</b>")
  descriptions %<>% c(plot_barpop_CasesPop="Barplot: <b>NUMBER OF CASES PER POPULATION</b>")
  descriptions %<>% c(plot_barpop_ActiveCasesPop="Barplot: <b>NUMBER OF ACTIVE CASES PER POPULATION</b>")
  descriptions %<>% c(plot_barpop_CriticalCasesPop="Barplot: <b>NUMBER OF CRITICAL/SERIOUS CASES PER POPULATION</b>")
  descriptions %<>% c(plot_barpop_DeathsPop="Barplot: <b>NUMBER OF DEATHS PER POPULATION</b>")
  descriptions %<>% c(plot_barpop_Recovered="Barplot: <b>NUMBER OF RECOVERED PATIENTS PER POPULATION</b>")
  descriptions %<>% c(plot_barpop_NewDeathsPop="Barplot: <b>NUMBER OF NEW DEATHS PER POPULATION</b>")
  descriptions %<>% c(plot_barpop_TotalTestsPop="Barplot: <b>NUMBER OF DIAGNOSTIC TESTS PER POPULATION</b>")

  for(p in names(descriptions)) {

    if(regexpr('plot_bar',p)<0) next

    v = p %>% sub('.*_','',.)

    #Latest %>% arrange(Country) %>% mutate(y=!!sym(v), XCol=Colors(Country)) %>% arrange(desc(y)) %>% select(Country, y, XCol) %>% mutate()

    ## This is carefully set up so that the colors are based on 'Country' even
    ## after the reordering of categories according to value. Plotly requires
    ## the use of factors as the basis for the x-axis and they must be made from
    ## numerical value (which gives the order) and given the desired labels
    ## (i.e. Country, in this case)
    D = Latest %>% arrange(Country) %>% mutate(y=!!sym(v), XCol=Colors(Country)) %>%
      select(Country, y, XCol) %>% filter(complete.cases(.)) %>% arrange(desc(y)) %>%
      mutate(Order=factor(1:n(), labels=Country))

    plot_ly(D, x=~Order, y=~y, color=~Order, name=~Country, colors=~XCol, type='bar') %>%
      layout(title = set_titles(descriptions[p], 'Current'),
             yaxis = list(title=set_axis_lab(descriptions[p])),
             xaxis = list(title="")) %>%#, type="category", categoryorder="array", categoryarray=~Order)) %>%
      layout(plot_bgcolor='black', paper_bgcolor='black', font=list(color='white')) %>%
      layout(margin=list(l=50, r=50, b=55, t=50, pad=1)) %>%
      layout(annotations=annot_source) %>%
      layout(annotations=annot_author) %>%
      assign(p, ., envir=.GlobalEnv)

    #if(p=='plot_barpop_CasesPop') {print(get(p)); stop()}

  }

} # if(do_plot_bar)

############################
### DO TIME SERIES PLOTS ###
############################

if(do_plot_ts) {

  catn("Plotting progression plots...")

  descriptions %<>% c(plot_ts_Infected="Time series: <b>NUMBER OF ACTIVE INFECTIONS</b>")
  descriptions %<>% c(plot_ts_Cases="Time series: <b>NUMBER OF CASES</b>")
  descriptions %<>% c(plot_ts_Deaths="Time series: <b>NUMBER OF DEATHS</b>")
  descriptions %<>% c(plot_ts_Tested="Time series: <b>NUMBER OF TESTS</b>")

  descriptions %<>% c(plot_tspop_InfectedPop="Time series: <b>NUMBER OF ACTIVE INFECTIONS PER POPULATION</b>")
  descriptions %<>% c(plot_tspop_CasesPop="Time series: <b>NUMBER OF CASES PER POPULATION</b>")
  descriptions %<>% c(plot_tspop_DeathsPop="Time series: <b>NUMBER OF DEATHS PER POPULATION</b>")
  descriptions %<>% c(plot_tspop_TestedPop="Time series: <b>NUMBER OF TESTS PER POPULATION</b>")

  descriptions %<>% c(plot_tsrate_DailyInfectedRatio="Time series: <b>RATE OF DAILY ACTIVE INFECTIONS</b>")
  descriptions %<>% c(plot_tsrate_DailyCasesRatio="Time series: <b>RATE OF DAILY CASES</b>")
  descriptions %<>% c(plot_tsrate_DailyDeathsRatio="Time series: <b>RATE OF DAILY DEATHS</b>")
  descriptions %<>% c(plot_tsrate_DailyTestedRatio="Time series: <b>RATE OF DAILY TESTS</b>")

  for(p in names(descriptions)) {

    if(regexpr('plot_ts',p)<0) next

    v = p %>% sub('.*_','',.)
    p_log = sub('_ts','_xtslog',p)
    title = set_titles(descriptions, 'Progression of the')[p]

    #D = Data4 %>% mutate(y=!!sym(v)) %>% group_by(Country) %>% mutate(LV=t1(y)) %>% ungroup() %>% arrange(desc(LV)) %>% mutate(XCol=Colors(Country)) %>%

    D = Data4 %>% arrange(Country) %>%
      mutate(y=!!sym(v), XCol=Colors(Country)) %>%
      select(Country, Date, y, XCol) %>% filter(complete.cases(.)) %>%
      #mutate(Order=factor(1:n(), labels=Country)
      mutate(Order=as_factor(Country))
      #if(p=='plot_ts_Cases') {ddd = D;stop()}

    plot_ly(D, x=~Date, y=~y, color=~Order, name=~Country, colors=~XCol, type='scatter', mode='lines',
            line=list(color=~XCol)) %>%
      layout(title=title,
             yaxis=list(title=set_axis_lab(descriptions[p])),
             xaxis=list(title="")) %>%
      layout(plot_bgcolor='black', paper_bgcolor='black', font=list(color='white')) %>%
      layout(margin=list(l=50, r=50, b=90, t=35, pad=1)) %>%
      layout(annotations=annot_source) %>%
      layout(annotations=annot_author) %>%
      assign(p, ., envir=.GlobalEnv) %>%
      layout(title=title %>% paste0('\n(logarhitmic scale)'),
             yaxis=list(type="log", range=c(0,log10(max(D$y))))) %>%
             #yaxis=list(type="log", range=c(0,log10(max(D$y))))) %>%
      assign(p_log, ., envir=.GlobalEnv)

      #if(p=='plot_tspop_TestedPop') stop()

    descriptions %<>% c(descriptions[p]%.%" (logarhitmic scale)" %>% setNames(p_log))

    #get(p) %>% print()
  }

} # if(do_plot_ts)

#################################
### DO REGRESSION FOR CZECHIA ###
#################################

if(do_plot_lm) {

  catn("Plotting linear model plots...")
  min_Cases = 250
  day0 = DataCZ %>% filter(Cases>=min_Cases) %>% slice(1) %>% pull(Date) # '2020-03-15' for min_Cases=250
  splits = '2020-03-28'
  #DataCZ250 = DataCZ %>% filter(Date>'2020-03') %>% filter(Cases>=min_Cases) %>%
  DataCZ250 = DataCZ %>% mutate(DateS=Date %>% sub('[0-9]+-','',.)) %>%
    filter(Date>=day0)


  descriptions %<>% c(plot_z_lm_daily="Linear model: <b>DAILY CASES</b> vs <b>DAILY TESTS</b> (Czechia)",
                      plot_z_lm_total="Linear model: <b>TOTAL CASES</b> vs <b>TOTAL TESTS</b> (Czechia)")

  title = '<b>Czech Republic</b>: dependence of discovered cases on performed tests\n'%.%
          "<span style='font-size: 75%; border: 1px solid silver'>"%.%
          '<b>Model 0: </b>simple LRM (i.e. @y~@x, R<sup>2</sup>=@r0)'%.%
          ' based on all days after '%.%day0%.%'\n'%.%
          '<b>Model 1: </b>simple LRM (i.e. @y~@x) based on all days from '%.%
          day0%.%' (the first day with at least '%.%min_Cases%.%
          ' cases)\nand up to '%.%splits[1]%.%' (the day where a qualitative shift started to appear, see the note below)\n'%.%
          '<b>Model 2: </b>simple LRM (i.e. @y~@x)'%.%
          ' based on the days after '%.%splits[1]%.%
          '</span>'

  explain_model = 'Note: Model 0 works with all data, while Models 1 and 2 work with two disjoint'%.%
    ' periods (split by '%.%day0%.%'). The observed differences among the 3 models'%.%
    ' illustrate the apparent\nchanges in the relationship between the number of discovered cases and '%.%
    'the number of performed tests, which initially appeared to be stronger than in the more'%.%
    ' recent period.'

  for(p in names(descriptions)) {
    if(regexpr('plot_z_lm',p)<0) next

    u = ifelse(regexpr('daily',p)>0,'Daily','') %.% 'Tested'
    v = ifelse(regexpr('daily',p)>0,'Daily','') %.% 'Cases'

    DCZ0 = DataCZ250 %>%
      mutate(x=!!sym(u), y=!!sym(v),
             DateS=Date %>% sub('[0-9]+-','',.)) %>%
      drop_na(x, y)

    DCZ1 = DCZ0 %>% filter(Date<=splits[1])
    DCZ2 = DCZ0 %>% filter(Date>splits[1])

    m0 = with(DCZ0, lm(y~x))
    m1 = with(DCZ1, lm(y~x))
    m2 = with(DCZ2, lm(y~x))

    DCZ0 %<>% structure('m0'=m0, 'm1'=m1, 'm2'=m2, 'u'=u, 'v'=v)

    Col = Colors(3)
    arrow_n1x = 9
    arrow_n1y = 9

    DCZ0 %>%
      mutate(Predicted0=predict(m0),
             Predicted1=predict(m1, list(x=DCZ0$x)),
             Predicted2=predict(m2, list(x=DCZ0$x))) %>%
      plot_ly(type='scatter', mode='markers', colors=Col) %>%
      add_trace(x=~x, y=~y, name='observed', marker=list(size = 17)) %>%
      add_trace(x=~x, y=~Predicted0, name='Model 0', mode='lines') %>%
      add_trace(x=~x, y=~Predicted1, name='Model 1', mode='lines') %>%
      add_trace(x=~x, y=~Predicted2, name='Model 2', mode='lines') %>%
      add_annotations(yref="paper", xref="paper", y=1.05, x=0.035,
                      text=title %>% sub('@r0',signif(summary(attr(DCZ0,'m0'))$r.squared,3),.) %>%
                             gsub('@y',attr(DCZ0,'v'),.) %>% gsub('@x',attr(DCZ0,'u'),.),
                      align='left', showarrow=FALSE, font=list(size=15)) %>%
      add_annotations(yref="y", xref="x", x=~x, y=~y, text=DCZ0$DateS, font = list(color = Col[1]),
                      showarrow=TRUE, arrowhead=4, arrowsize=0.6, arrowcolor=Col[1], ax=20, ay=60) %>%
      layout(plot_bgcolor='black', paper_bgcolor='black', font=list(color='white'),
             legend = list(x = 0.9, y = 0.16),
             yaxis=list(title=set_axis_lab(descriptions[p], '[^<>]+<[bB]>')),
             xaxis=list(title='')) %>%
      layout(annotations=annot_bottom %modify% list(text=set_axis_lab(descriptions[p]), y=-0.085)) %>%
      layout(margin=list(l=50, r=50, b=125, t=50, pad=1)) %>%
      layout(annotations=annot_bottom %modify% list(text=explain_model, y=-0.225, x=0.48)) %>%
      layout(annotations=annot_source %modify% list(y=-0.3)) %>%
      layout(annotations=annot_author %modify% list(y=-0.3, x=1)) %>%
      assign(p, ., envir=.GlobalEnv)

  }

} # if(do_plot_lm)

### END OF REGRESSION FOR CZECHIA ###

## Find and announce all plot objects (produced by plotly)
ls(pattern='^plot') %>%
  split_into_groups(6) %>%
  lapply(paste, collapse='\t  ') %>%
  {do.call('paste', . %append% list(sep='\n\t'))} %>%
  catn("\nCreated plot objects:\n\n\t", .,'\n')

if(do_save_plotly_to_file) {
  setwd('plots_plotly')
  ds = descriptions
  catf = hijack(catn, file='../index.html', append=TRUE)
  catf("<!DOCTYPE HTML>\n<html lang='en'>\n<head>", append=FALSE)
  catf("<title>Pecanka Consulting: Coronavirus plots</title>")
  catf("<link rel='stylesheet' type='text/css' href='style.css'>")
  catf("</head>\n<body><div class='main'>")
  catf("<div class='head'>CORONOVIRUS: COVID-19</div><p>")
  catf("<center>Jakub Pecanka, PhD (Pecanka Consulting, <a href='https://www.pecanka.net'>www.pecanka.net</a>)</center><p>")
  catf("<div class='descriptions'>A collection of plots showing the current state")
  catf("and the historical progression of the COVID-19 pandemic in select countries around the world.<p>")
  catf("<span style='font-size: 80%'>"%.%data_source%.%"</span>")
  catf("</div><div class='list'><div class='note2'>linear scale</div><ul>")

  ps = ls(pattern='^plot_') %>% sort()
  ds %<>% .[ps]

  is_gap = ps %>% sub('_[^_]+$','',.) %>% sub('plot_','',.) %>% c(.[1],.) %>% str_diff() %>% setNames(ps)
  is_log = ps %>% sub('_[^_]+$','',.) %>% sub('plot_','',.) %>% {regexpr('log',.)>0} %>% {. & !dplyr::lag(.)} %>% setNames(ps)

  for(p in ps) {
    filename = p%.%'.'%.%format_for_plots
    catn("Saving plot '"%.%p%.%"' to file ...")
    if(p==ps[which.max(regexpr("Czechia",ds)>0)]) {
      catf("</ul><div class='note'>Plots based on data available for CZECHIA only</div>")
      catf("<div class='note2'>linear scale</div><ul>")
    }
    if(is_gap[p]) catf("<p>")
    if(is_log[p]) catf("</ul><div class='note2'>logarhitmic scale</div><ul>")
    add_goback = FALSE

    #if(regexpr('plot_z_lm_',p)>0) {
    #pattern = 'plot_lag|plot_bar_Cases|plot_ts_Cases|plot_z_lm_total'
    pattern = 'plot_lag'
    #pattern = 'plot_bar_Cases'
    #pattern = 'plot_ts_Cases'
    #pattern = 'plot_z_lm_total|plot_z_lm_daily'
    #pattern = 'plot_z_lm_total'
    #pattern = 'plot_z_lm_daily'
    #pattern = 'plot_.*Ratio'
    #pattern = '---'
    pattern = ''
    #pattern = 'plot_barpop_ActiveCasesPop'
    if(regexpr(pattern,p)>0) {
    if(format_for_plots=='html') {
      html_title = "Coronavirus: "%.%gsub("<[/]?b>","",ds[p])%.%" (by Pecanka Consulting)"
      if(TRUE || !file.exists(filename)) {
        htmlwidgets::saveWidget(as_widget(get(p)), file=filename, background='#000000', title=html_title); add_goback = TRUE
      }
    } else if(format_for_plots=='png') {
      orca(get(p), file=filename)
      stop()
    } else error("Unknown format for plots '",format_for_plots,"'.")
    }

    if(add_goback) {
      content = readLines(filename)
      w = which(substr(content,1,32)=='<div id=\"htmlwidget_container\">')
      if(length(w)>0) {
        catn("Placing the 'Go back' code on line ",w," of the plotly file ...")
        content[w] %<>% paste0("<a href='../index.html'><div style='border: 1px solid #101010;"%.%
          " background-color: #505050; color: white; padding: 6px; position: absolute; z-index:10;"%.%
          " top: 0; left: 0; font-family: Calibri, Arial, Sans Serif;'>&larr;&nbsp;"%.%
          "Back to the overview of plots</div></a>", .)
        writeLines(content, filename)
      } else note("The 'Go back' code was not placed.")
    }
    catf("<div class='link'><li><a href='plots_plotly/"%.%filename%.%"'>",ds[p],"</a></li></div>")
    #break
  }
  catf("</ul></div><p></div></body>\n</html>")
  rm(catf)
  setwd('..')
  catn("Finished.")
}
