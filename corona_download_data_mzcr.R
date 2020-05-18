download_data_mzcr = function(CountryCZ) {

  country_url = CountryCZ$url
  country_prefix = CountryCZ$prefix
  country_name = CountryCZ$name
  out_file = 'data/'%p%country_prefix%p%country_name%p%'_@type_@date.rda'

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
