download_country_list_wom = function(url, name_start='wom__list-of-countries_') {

  pattern = name_start%p%ifelse(do_use_any_existing_data,"",t_day())
  f = list.files('data',pattern, full.names=TRUE) %>% file_sort_time() %>% h1()

  if(!do_force_fresh_data && length(f)>0) {
    catn("Loading list of countries ...", flush_cycle=10)
    load(f)
  } else {
    catn("Getting list of countries from url '",url,"' ...")
    html = try(read_html(url))
    if(is_error(html)) {
      html = try(read_url_via_download(url, read_html))
    }
    if('try-error' %in% class(html)) error("Something went wrong when reading '",url,"'.")

    name = html %>% html_nodes('.mt_a') %>% html_text()
    surl = html %>% html_nodes('.mt_a') %>% html_attr('href')
    countries = tibble(name, prefix='wom_', url=url%p%surl) %>% unique()
    save(countries, file='data/'%p%name_start%p%t_day()%p%'.rda')
  }

  countries

}

####################################################

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
  if(is_error(html)) {
    html = try(read_url_via_download(url, read_html))
  }
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

####################################################

download_data_wom = function(available_countries, url) {

  if(missing(available_countries)) 
    available_countries = download_country_list_wom(url)

  Data = Latest = NULL
  for(country in split_rows(available_countries)) {

    country_url = country %>% pull(url)
    country_prefix = country %>% pull(prefix)
    country_name = country %>% pull(name)

    out_file = 'data/'%p%country_prefix%p%country_name%p%'_@type_@date.rda'

    catn("Scrapping data for ",country_name," from WOM ...")
    html = try(read_html(url))
    if(is_error(html)) {
      html = try(read_url_via_download(url, read_html))
    }
    if('try-error' %in% class(html)) next
    #some_fresh_data_present = TRUE

    latest = html_extract_latest_wom(html, country_name)
    save(latest, file=out_file %>% sub('@type','latest',.) %>% sub('@date',t_day('%Y-%m-%d-%H%M%S'),.))
      #'data/'%p%country_prefix%p%country_name%p%'_latest_'%p%t_day('%Y-%m-%d-%H%M%S')%p%'.rda')

    series = html %>% as.character() %>% regmatches(gregexpr('series:[ ][[][^}]+[}]',.)) %>% unlist()
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
     #'data/'%p%country_prefix%p%country_name%p%'_historical_'%p%t1(data$Date)%p%'.rda')
     
    #if(country$name=='Czechia') browser()

  }

  catn("Data for all countries scrapped.")

}

####################################################
