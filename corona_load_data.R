load_data = function(envir=.GlobalEnv) {

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
    rename_all(~.x%p%'OCDC') %>%
    select(Date=DateOCDC, Deaths=DeathsOCDC, DailyDeaths=DailyDeathsOCDC, everything()) %>%
    left_join(DataCZ, ., by='Date') %>% arrange(Date) %T>% print(n=100)

  ## Read WOM data from files
  dataWOM = load_data_individual(available_countries_wom)
  Data = dataWOM$Data
  Latest = dataWOM$Latest
  
  change_Korea = . %>% mutate(Country=ifelse(Country=="S. Korea", "SouthKorea", Country))
  Data %<>% change_Korea
  Latest %<>% change_Korea

  ## Merge the WOM data with the MZCR data
  DataCZ1 = DataCZ %>%
    select(Date, Country, DeathsCZ=Deaths, DailyDeathsCZ=DailyDeaths,
           one_of(setdiff(names(.), names(Data))))

  Data %<>% full_join(DataCZ1) %>%
    mutate(Deaths=coalesce(Deaths, DeathsCZ), 
           DailyDeaths=coalesce(DailyDeaths,DailyDeathsCZ)) %>%
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

  call_Pop = . %>% mutate_at(vars(ends_with('Pop_')), ~.x/Population*PerPopulation) %>%
    setNames(sub("_$",'',names(.)))

  Data %<>% left_join(PopData, by='Country') %>%
    group_by(Country) %>%
    mutate(DailyInfected=diff(c(0,Infected))) %>%
    ungroup() %>%
    mutate_at(vars(Cases,Deaths,Infected,Tested,DailyCases,DailyDeaths,DailyInfected,DailyTested), ~ifelse(.x==0, NA, .x)) %>%
    mutate(CasesPop_=Cases, 
           DeathsPop_=Deaths, 
           TestedPop_=Tested, 
           InfectedPop_=Infected,
           DailyCasesPop_=DailyCases, 
           DailyDeathsPop_=DailyDeaths, 
           DailyTestedPop_=DailyTested, 
           DailyInfectedPop_=DailyInfected) %>%
    call_Pop
    
  Latest %<>% left_join(PopData, by='Country') %>%
    mutate(CasesPop_=Cases, 
           DeathsPop_=Deaths, 
           TestedPop_=Tested, 
           RecoveredPop_=Recovered) %>%#,
           #DailyCasesPop_=DailyCases, 
           #DailyDeathsPop_=DailyDeaths, 
           #DailyTestedPop_=DailyTested, 
           #DailyRecoveredPop_=DailyRecovered) %>%
    call_Pop

  
  Latest_all = load_latest_all_wom()

  if('Population' %nin% names(Latest_all))
    Latest_all %<>% left_join(PopData, by='Country') 
    
  Latest_all %<>%
    mutate(ActiveCasesPop_=ActiveCases, CriticalCasesPop_=CriticalCases, NewDeathsPop_=NewDeaths) %>%
    call_Pop

  browser()
  Latest %<>% left_join(Latest_all, by=c('Country','Date'), suffix=c('','_2'))

  if(!setequal(unique(Latest$Date), unique(Latest_all$Date)))
    error("Something seems to have gone wrong with the data, the dates in",
          "'Latest' and 'Latest_all' do not match. Try redownloading the data.")

  # Check for duplicated records
  Data %>% select(Date, Country) %>% anyDuplicated() %>% equals(0) %>% stopifnot()
  Latest %>% select(Country) %>% anyDuplicated() %>% equals(0) %>% stopifnot()

  Data4 = Data %>% 
    mutate(Date=sub('[0-9]+-','',Date)) %>%
    group_by(Country) %>%
    mutate(DailyInfectedRatio=DailyInfected/c(0,head(Infected,-1)),
           DailyCasesRatio=DailyCases/c(0,head(Cases,-1)),
           DailyDeathsRatio=DailyDeaths/c(0,head(Deaths,-1)),
           DailyTestedRatio=DailyTested/c(0,head(Tested,-1))) %>%
    ungroup()

  if(countries_focus) {
    call_focus = . %>% filter(Country %in% Countries_focus_broad)
    Data4 %<>% call_focus
    Latest %<>% call_focus
  }

  # Add plotting colors to the data frames
  Data4 %<>% arrange(Country) %>% mutate(XCol=Colors(Country))
  Latest %<>% arrange(Country) %>% mutate(XCol=Colors(Country))

  assign('Data', Data, envir=envir)
  assign('Data4', Data4, envir=envir)
  assign('DataCZ', DataCZ, envir=envir)
  assign('Latest', Latest, envir=envir)

}
