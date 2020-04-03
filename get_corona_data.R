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

PerPopulation = "100000"
Country0 = 'Italy'
Countries_focus_narrow = c("Netherlands","Germany","USA","Czechia")

Countries_focus_broad = c('Czechia','Italy','Germany','Netherlands',
  'Spain','USA','S. Korea','Austria','UK','Canada','Belgium','Denmark',
  'Norway','Poland','Sweden','Switzerland','France')

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
    mutate(Country=country, Date=t_day(), Tested=NA) %>%
    select(Country, Date, Tested, everything())
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

read_data_ocdc = function(url, max_lag=2, lag=1) {

  file = 'data/'%.%separate_path(url)$filename

  data = if(!file.exists(file)) {
    warn("File '",file,"' does not exist.")
    if(lag<=max_lag) {
      catn("Trying previous day's data ...")
      data = read_data_ocdc(sub(t_day(), y_day(lag=-lag), url), lag=lag+1)
    } else {
      NULL
    }
  } else {
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
      select(Date, Country, Cases, Deaths, DailyCases, DailyDeaths, Population)
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

  data = full_join_recurse(list(cz_cases, cz_daily_cases, cz_tests, cz_daily_tests), by='Date') %>%
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

    rda_file_country = 'data/'%.%country$name%.%'_historical_'%.%c(y_day(lag=0:-2))%.%'.rda' %>%
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

if(do_load_data || do_download_data || !exists('Data') || !exists('Latest')) {

  DataOCDC = read_data_ocdc(url_ocdc)
  LatestOCDC = DataOCDC %>% group_by(Country) %>% slice(n()) %>% ungroup()

  DataCZ = load_data_individual(CountryCZ)
  LatestCZ = DataCZ$Latest
  DataCZ %<>% .$Data

  DataCZ = DataOCDC %>% filter(Country=='Czechia') %>%
    select(Date, Deaths, DailyDeaths, CasesOCDC=Cases, DailyCasesOCDC=DailyCases) %>%
    left_join(DataCZ, ., by='Date')

  data = load_data_individual(available_countries_wom)
  Data = data$Data
  Latest = data$Latest

  Data %<>% bind_rows(DataCZ, .)
  Latest %<>% bind_rows(LatestCZ, .)

  PopData = DataOCDC %>% group_by(Country) %>% slice(1) %>% ungroup() %>%
    select(Country, Population) %>% add_row(Country='Hong Kong', Population=7500000)

  # Check for some country's population missing
  PopData %>% anti_join(Data, ., by='Country') %>% group_by(Country) %>% slice(1) %>% ungroup() %>% nrow() %>% {stopifnot(.==0)}

  Data %<>% left_join(PopData, by='Country') %>%
    mutate_at(vars(Cases,Deaths,DailyCases,DailyDeaths), ~ifelse(.x==0, NA, .x)) %>%
    mutate(CasesPop=Cases, DeathsPop=Deaths, TestedPop=Tested, InfectedPop=Infected) %>%
    mutate_at(vars(ends_with("Pop")), ~.x/Population*as.numeric(PerPopulation))

  Latest %<>% left_join(PopData, by='Country') %>%
    mutate(CasesPop=Cases, DeathsPop=Deaths, TestedPop=Tested, RecoveredPop=Recovered) %>%
    mutate_at(vars(ends_with("Pop")), ~.x/Population*as.numeric(PerPopulation))

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
descriptions = ylabs = c()
plots_Cases = plots_Deaths = list()
for(C in Countries_focus_narrow) {

  D3 = Data4 %>% filter(Country==C) %T>%
    write.table(file='tables/'%.%C%.%'_vs_'%.%Country0%.%'_'%.%t_day()%.%'.txt', row.names=FALSE, quote=FALSE) %>%
    {bind_rows(select(., -ends_with('0')),
               mutate(., Date0=Date) %>% select(ends_with('0')) %>% rename_all(~sub('0$','',.x)))} %>%
    mutate(Date=sub('[0-9]+-','',Date))

  Col = Colors(12, skip=9) %>% {c(h1(.),t1(.))} %>% pif(C<Country0, rev)

  for(v in c('Cases','Deaths')) {
    D3 %>% mutate(y=!!sym(v)) %>%
      plot_ly(x=~Date, y=~y, color=~Country, name=~Country,
                   colors=Col, type='scatter', mode='lines', showlegend=TRUE) %>%
      layout(legend=list(x = 0.1, y = 0.5)) %>%
      layout(yaxis=list(title=v)) %>%
      layout(xaxis=list(title="", showticklabels=FALSE)) %>%
      #layout(annotations=list(text='<b>'%.%C%.%'</b>', y=0.9, x=0.5, yref="paper", xref="x",
      #                        align='left', xanchor='left', showarrow=FALSE)) %>%
      #layout(annotations=list(text=c('<b>'%.%C%.%'</b>',Country0), x=rep(0.2,2), y=c(0.7,0.6), yref="paper",
      #                        xref='paper', align='left', xanchor='left', showarrow=TRUE, arrowcolor=Col,
      #                        arrowhead = 1, ax = c(0,0), ay=c(0.7,0.6), axref="paper", ayref="paper",
      #                        font = list(color = Col, size = 14))) %>%
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

annot_bottom = list(yref="paper", xref="paper", align='left', xanchor='left', showarrow=FALSE)

text_source = "<span style='font-size: 90%;'><i>Source: "%.%data_source%.%"</i></span>"

title = "Total number of cases and deaths: a comparison with "%.%Country0
lag_explanation = c("Explanation: Each country's progression of case counts is matched up against "%.%Country0%.%
  "'s case count, which determines the number of days the given country lags behind "%.%Country0%.%
  ". The resulting", "matching is shown on the top row. The bottom row shows the death count progression"%.%
  " with "%.%Country0%.%"'s counts shifted by the lag determined based on the reported case counts.")

plot_lagItaly = append(plots_Cases, plots_Deaths) %>%
  append(list(nrows=2, shareY = FALSE, titleY=TRUE, titleX = TRUE)) %>%
  do.call(subplot, .) %>%
  layout(title=list(text=title, y=1.35), showlegend = FALSE) %>%
  layout(margin=list(l=50, r=50, b=90, t=50, pad=1)) %>%
  layout(plot_bgcolor='black', paper_bgcolor='black', font=list(color='white')) %>%
  layout(annotations=annot_bottom %append% list(text=lag_explanation[1], y=-0.10, x=-0.025)) %>%
  layout(annotations=annot_bottom %append% list(text=lag_explanation[2], y=-0.14, x=0.01)) %>%
  layout(annotations=annot_bottom %append% list(text=text_source, y=-0.20, x=-0.03))

### PRODUCE PLOTS FOR ALL COUNTRIES ###
catn("Plotting ...")

set_axis_lab = function(d, pattern1='.+<[bB]>', pattern2='[(<].+') {
  d %>% sub(pattern1,'',.) %>% sub(pattern2,'',.) %>% str_trim() %>% setNames(names(d))
}

set_titles = function(descriptions) {
  descriptions %>% set_axis_lab() %>%
    paste0(rep(c(""," per "%.%PerPopulation%.%" citizens"), e=length(descriptions)/2)) %>%
    paste0("Progression of the",.," (as of "%.%t_day()%.%")") %>%
    setNames(names(descriptions))
}

Data4 = Data %>% mutate(Date=sub('[0-9]+-','',Date)) %>%
  mutate(DailyInfected=diff(c(NA,Infected)),
         DailyInfectedRate=DailyInfected/Cases,
         DailyCasesRate=DailyCases/Cases,
         DailyDeathsRatio=DailyDeaths/Cases,
         DailyTestedRate=DailyTested/Tested)

if(countries_focus=="broad") Data4 %<>% filter(Country %in% Countries_focus_broad)
if(countries_focus=="narrow") Data4 %<>% filter(Country %in% Countries_focus_narrow)

if(countries_focus=="broad") Latest %<>% filter(Country %in% Countries_focus_broad)
if(countries_focus=="narrow") Latest %<>% filter(Country %in% Countries_focus_narrow)

n_old = length(descriptions)
descriptions %<>% c(plot_bar_Cases="Barplot: <b>NUMBER OF CASES</b>")
descriptions %<>% c(plot_bar_Deaths="Barplot: <b>NUMBER OF DEATHS</b>")
descriptions %<>% c(plot_bar_Recovered="Barplot: <b>NUMBER OF RECOVERED PATIENTS</b>")
descriptions %<>% c(plot_barpop_Cases="Barplot: <b>NUMBER OF CASES (PER POPULATION)</b>")
descriptions %<>% c(plot_barpop_Deaths="Barplot: <b>NUMBER OF DEATHS (PER POPULATION)</b>")
descriptions %<>% c(plot_barpop_Recovered="Barplot: <b>NUMBER OF RECOVERED PATIENTS (PER POPULATION)</b>")


for(p in names(tail(descriptions,-n_old))) {
  v = p %>% sub('plot_[a-z]+_','',.)
  Latest %>% arrange(desc(!!sym(v))) %>%
    plot_ly(x=~Country, y=~get(v), color=~Country, name=~Country, colors=Colors(), type='bar') %>%
    layout(title = set_titles(tail(descriptions, -n_old))[p], yaxis = list(title=set_axis_lab(descriptions[p])),
           xaxis = list(title = "Country", categoryorder = "array", categoryarray = ~Country)) %>%
    layout(plot_bgcolor='black', paper_bgcolor='black', font=list(color='white')) %>%
    layout(margin=list(l=50, r=50, b=50, t=50, pad=1)) %>%
    layout(annotations=annot_bottom %append% list(text=text_source, y=-0.18, x=-0.04)) %>%
    assign(p, ., envir=.GlobalEnv)
}

n_old = length(descriptions)
descriptions %<>% c(plot_ts_Infected="Time series: <b>NUMBER OF ACTIVE INFECTIONS</b>")
descriptions %<>% c(plot_ts_Cases="Time series: <b>NUMBER OF CASES</b>")
descriptions %<>% c(plot_ts_Deaths="Time series: <b>NUMBER OF DEATHS</b>")
descriptions %<>% c(plot_ts_Tested="Time series: <b>NUMBER OF TESTS</b>")

descriptions %<>% c(plot_tspop_Infected="Time series: <b>NUMBER OF ACTIVE INFECTIONS (PER POPULATION)</b>")
descriptions %<>% c(plot_tspop_Cases="Time series: <b>NUMBER OF CASES (PER POPULATION)</b>")
descriptions %<>% c(plot_tspop_Deaths="Time series: <b>NUMBER OF DEATHS (PER POPULATION)</b>")
descriptions %<>% c(plot_tspop_Tested="Time series: <b>NUMBER OF TESTS (PER POPULATION)</b>")

descriptions %<>% c(plot_tsrate_DailyInfectedRate="Time series: <b>RATE OF DAILY ACTIVE INFECTIONS</b>")
descriptions %<>% c(plot_tsrate_DailyCasesRate="Time series: <b>RATE OF DAILY CASES</b>")
descriptions %<>% c(plot_tsrate_DailyDeathsRate="Time series: <b>RATE OF DAILY DEATHS</b>")
descriptions %<>% c(plot_tsrate_DailyTestedRate="Time series: <b>RATE OF DAILY TESTS</b>")

for(p in names(tail(descriptions,-n_old))) {
  v = p %>% sub('plot_ts_','',.)
  p_log = sub('_ts','_xtslog',p)
  Data4 %>%
    plot_ly(x=~Date, y=~get(v), color=~Country, name=~Country, colors=Colors(), type='scatter', mode='lines') %>%
    layout(title = set_titles(tail(descriptions, -n_old))[p], yaxis = list(title=set_axis_lab(descriptions[p]))) %>%
    layout(plot_bgcolor='black', paper_bgcolor='black', font=list(color='white')) %>%
    layout(margin=list(l=50, r=50, b=90, t=35, pad=1)) %>%
    layout(annotations=annot_bottom %append% list(text=text_source, y=-0.18, x=-0.04)) %T>%
    assign(p, ., envir=.GlobalEnv) %>%
    layout(yaxis = list(type = "log")) %>%
    assign(p_log, ., envir=.GlobalEnv)

  descriptions %<>% c(descriptions[p]%.%" (logarhitmic scale)" %>% setNames(p_log))

  #get(p) %>% print()
}


### DO REGRESSION FOR CZECHIA ###
min_Cases = 250
DataCZ250 = DataCZ %>% filter(Date>'2020-03') %>% filter(Cases>=min_Cases)

n_old = length(descriptions)
descriptions %<>% c(plot_z_lm_daily = "Linear model: <b>DAILY CASES</b> vs <b>DAILY TESTS</b> (Czechia)",
                    plot_z_lm_total = "Linear model: <b>TOTAL CASES</b> vs <b>TOTAL TESTS</b> (Czechia)")

title = '<b>Czech Republic</b>\n<i>Data</i>: all days after '%.%h1(DataCZ250$Date)%.%
        ' (i.e. days with at least '%.%min_Cases%.%' cases)'%.%
        ')\n<i>Model</i>: simple LRM (i.e. Cases~Tested), R<sup>2</sup>='

for(p in tail(names(descriptions), -n_old)) {

  u = ifelse(regexpr('daily',p)>0,'Daily','') %.% 'Tested'
  v = ifelse(regexpr('daily',p)>0,'Daily','') %.% 'Cases'
  m = with(DataCZ250, lm(I(get(v))~I(get(u))))
  date_short = DataCZ250$Date %>% sub('[0-9]+-','',.)

  DataCZ250 %>% mutate(Predicted=m$fitted.values,
                       DateS=Date %>% sub('[0-9]+-','',.)) %>%
    plot_ly(x=~get(u), y=~get(v), name='observed', type='scatter', mode='line', colors=Colors()) %>%
    add_trace(x=~Tested, y=~Predicted, name='predicted', mode='lines+markers', color='red') %>%
    add_annotations(yref="paper", xref="paper", y=1.05, x=0, text=title%.%signif(summary(m)$r.squared,3),
                    align='left', showarrow=FALSE, font=list(size=17)) %>%
    add_annotations(yref="y", xref="x", x=~get(u), y=~get(v), text=date_short,
                    showarrow=TRUE, arrowhead = 4, arrowsize = .4, ax = -10, ay = -50) %>%
    layout(plot_bgcolor='black', paper_bgcolor='black', font=list(color='white'),
           yaxis = list(title=set_axis_lab(descriptions[p], '[^<>]+<[bB]>')),
           xaxis = list(title=set_axis_lab(descriptions[p]))) %>%
    assign(p, ., envir=.GlobalEnv)

}

### END OF REGRESSION FOR CZECHIA ###

## Find all plot objects (produced by plotly)
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
  catf("<div class='descriptions'>A collection of plots showing the current state")
  catf("and the historical progression of the COVID-19 pandemic around the world.<p>")
  catf("Author: <a href='https://www.pecanka.net'>Jakub Pecanka, PhD</a><p>")
  catf("<span style='font-size: 75%'>"%.%data_source%.%"</span>")
  catf("</div><div class='list'><div class='note2'>linear scale</div><ul>")

  ps = ls(pattern='^plot_') %>% sort()
  ds %<>% .[ps]

  #vals = rep(1, length(ds))
  #vals %<>% {. + as.numeric(regexpr("logarhitmic",ds)>0)}
  #vals %<>% {. + as.numeric(regexpr("Czechia",ds)>0)}
  #vals %<>% {. + as.numeric(regexpr("logarhitmic.*Czechia|Czechia.*logarhitmic",ds)>0)}
  #ord = order(vals %.% ds)
  #ps %<>% .[ord]
  #vals %<>% .[ord] %>% setNames(ps)
str_diff = function(x) {
  tail(x,-1) != head(x,-1)
}

  is_gap = ps %>% sub('_[^_]+$','',.) %>% sub('plot_','',.) %>% c(.[1],.) %>% str_diff() %>% setNames(ps)
  is_log = ps %>% sub('_[^_]+$','',.) %>% sub('plot_','',.) %>% {regexpr('log',.)>0} %>% {. & !dplyr::lag(.)} %>% setNames(ps)

  #ps = ls(pattern='^plot') %>% {.[order(ds[.])]} %>%
  #  { c(.[regexpr("Czechia",ds[.])<0], .[regexpr("Czechia",descriptions[.])>0]) }
  for(p in ps) {
    catn("Saving plot '"%.%p%.%"' to file ...")
    if(p==ps[which.max(regexpr("Czechia",ds)>0)]) {
      catf("</ul><div class='note'>Plots based on data available for CZECHIA only</div>")
      catf("<div class='note2'>linear scale</div><ul>")
    }
    if(is_gap[p]) catf("<p>")
    if(is_log[p]) catf("</ul><div class='note2'>logarhitmic scale</div><ul>")
    filename = p%.%'.html'
    add_goback = FALSE
    if(regexpr('plot_z_lm_',p)>0) {
      title = "Coronavirus: "%.%gsub("<[/]?b>","",ds[p])%.%" (by Pecanka Consulting)"
      htmlwidgets::saveWidget(as_widget(get(p)), file=filename, background='#000000', title=title); add_goback = TRUE
    }
    #filename = p%.%'.png'; orca(get(p), file=filename)

    if(add_goback) {
      content = readLines(filename)
      w = which(substr(content,1,32)=='<div id=\"htmlwidget_container\">')
      if(length(w)>0) {
        catn("Placing the 'Go back' code on line ",w," of the plotly file ...")
        content[w] %<>% paste0("<a href='../index.html'><div style='border: 1px solid #101010;"%.%
          " background-color: #505050; color: white; padding: 10px; position: absolute; z-index:10;"%.%
          " top: 0; left: 0; font-family: Calibri, Arial, Sans Serif;'>&larr;&nbsp;"%.%
          "Back to the overview of plots...</div></a>", .)
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
