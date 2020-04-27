process_lag_cases_deaths = function(Data) {

  K_Cases = process_lag(Data, 'Cases', Country0=Country0)

  Data_Lag_Cases = K_Cases$Data_Lag

  K_Deaths = process_lag(Data, 'Deaths', Country0=Country0, return_lagged_data=FALSE)

  K = full_join(K_Cases$K, K_Deaths$K, by='Country') %>%
    mutate(Lag_diff=Lag_Deaths-Lag_Cases)

  bind_cols(K %>% arrange(Lag_Cases),
            K %>% arrange(Lag_Deaths),
            K %>% arrange(Lag_diff)) %T>%
    print(n=100)

  Data_Lag_Cases

}

process_lag = function(Data, Lag_by='Cases', lag_start=-10, lag_end=40, Country0='Italy', return_lagged_data=TRUE) {

  catn("Processing lag by ",Lag_by," ...")

  Countries = Data %>% group_by(Country) %>% filter(!all(is.na(Cases))) %>% pull(Country) %>% unique()
  
  if(Country0 %notin% Countries) stop('Missing data for Country0 (',Country0,').')

  lead2 = function(x, n, ...)
    if(n>=0) lead(x, n, ...) else lag(x, -n, ...)

  matchup = function(k, D_A, D_B, by='Cases') {
    matchup1 = function(k, D_A, D_B) amean(abs(D_A[[by]]- lead2(D_B[[by]], k)))
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

  list_clean(nlist(K, Data_Lag))

}
