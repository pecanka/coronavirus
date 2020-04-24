plot_ts = function(Data, envir=.GlobalEnv) {

  catn("Plotting time series plots...")

  descr = get('descriptions', envir=envir)
  
  # Note: The naming of elements after 'plotly_' is tripped decides the final order on the html page.
  
  descr %<>% list_update(list(plotly_ts_Cases="Time series: <b>NUMBER of TOTAL CASES</b>"))
  descr %<>% list_update(list(plotly_ts_Deaths="Time series: <b>NUMBER of TOTAL DEATHS</b>"))
  descr %<>% list_update(list(plotly_ts_Infected="Time series: <b>NUMBER of TOTAL ACTIVE INFECTIONS</b>"))
  descr %<>% list_update(list(plotly_ts_Tested="Time series: <b>NUMBER of TOTAL TESTS</b>"))

  descr %<>% list_update(list(plotly_tsd_DailyCases="Time series: <b>NUMBER of DAILY CASES</b>"))
  descr %<>% list_update(list(plotly_tsd_DailyDeaths="Time series: <b>NUMBER of DAILY DEATHS</b>"))
  descr %<>% list_update(list(plotly_tsd_DailyInfected="Time series: <b>NUMBER of DAILY ACTIVE INFECTIONS</b>"))
  descr %<>% list_update(list(plotly_tsd_DailyTested="Time series: <b>NUMBER of DAILY TESTS</b>"))

  descr %<>% list_update(list(plotly_tspop_CasesPop="Time series: <b>NUMBER of TOTAL CASES PER POPULATION</b>"))
  descr %<>% list_update(list(plotly_tspop_DeathsPop="Time series: <b>NUMBER of TOTAL DEATHS PER POPULATION</b>"))
  descr %<>% list_update(list(plotly_tspop_InfectedPop="Time series: <b>NUMBER of TOTAL ACTIVE INFECTIONS PER POPULATION</b>"))
  descr %<>% list_update(list(plotly_tspop_TestedPop="Time series: <b>NUMBER of TOTAL TESTS PER POPULATION</b>"))
  
  descr %<>% list_update(list(plotly_tspopd_DailyCasesPop="Time series: <b>NUMBER of DAILY CASES PER POPULATION</b>"))
  descr %<>% list_update(list(plotly_tspopd_DailyDeathsPop="Time series: <b>NUMBER of DAILY DEATHS PER POPULATION</b>"))
  descr %<>% list_update(list(plotly_tspopd_DailyInfectedPop="Time series: <b>NUMBER of DAILY ACTIVE INFECTIONS PER POPULATION</b>"))
  descr %<>% list_update(list(plotly_tspopd_DailyTestedPop="Time series: <b>NUMBER of DAILY TESTS PER POPULATION</b>"))

  descr %<>% list_update(list(plotly_tsrate_DailyCasesRatio="Time series: <b>RATE of DAILY CASES</b>"))
  descr %<>% list_update(list(plotly_tsrate_DailyDeathsRatio="Time series: <b>RATE of DAILY DEATHS</b>"))
  descr %<>% list_update(list(plotly_tsrate_DailyInfectedRatio="Time series: <b>RATE of DAILY ACTIVE INFECTIONS</b>"))
  descr %<>% list_update(list(plotly_tsrate_DailyTestedRatio="Time series: <b>RATE of DAILY TESTS</b>"))

  CountryCollections = list(NA) %append% as.list(unique(Data$Country))

  for(p in names(descr)) {

    if(regexpr('plotly_ts',p)<0) next

    for(Collection in CountryCollections) {
    
      v = p %>% sub('.*_','',.)
      title = set_titles(descr[[p]], 'Progression of the')
      
      p2 = if(is.na(Collection)) p else p %.% '_Only_' %.% collapse0(Collection, sep='_')
      
      p_log = sub('_ts','_xtslog',p2)

      DataF = if(is.na(Collection)) {
        Data %>% structure('plotly_type'='scatter', 'plotly_mode'='lines', 'plotly_line'=list(color=~XCol)) 
      } else {
        Data %>% filter(Country%in%Collection) %>% structure('plotly_type'='bar')
      }

      D = DataF %>% arrange(Country) %>% mutate(y=!!sym(v)) %>%
        select(Country, Date, y, XCol) %>% filter(complete.cases(.))

      if(is_all_same(D$Country)) {
        D %<>% bind_rows(slice(.,1) %>% 
          mutate(Country='&nbsp;', fCountry=to_factor(Country), XCol='black'), .)
      }

      D %<>% mutate(fCountry=to_factor(Country)) %>%
        copy_attributes(DataF, 'plotly_', pattern=TRUE) %>%
        structure('plotly_title'=title)

      plot_ly(D, x=~Date, y=~y, color=~fCountry, name=~Country, colors=~XCol, showlegend=TRUE, 
              type=attr(D,'plotly_type'), mode=attr(D,'plotly_mode'), line=attr(D,'plotly_line')) %>%
              #line=list(color=~XCol), showlegend=TRUE) %>%
        layout(title=attr(D,'plotly_title'), 
               xaxis=list(title=""),
               yaxis=list(title=set_axis_lab(descr[[p]]),
                          tickformat=ifelse('Ratio'%match%p,'%',''))) %>%
        layout(plot_bgcolor='black', paper_bgcolor='black', font=list(color='white')) %>%
        layout(margin=list(l=50, r=50, b=90, t=35, pad=1)) %>%
        layout(annotations=annot_source) %>%
        layout(annotations=annot_author) %>%
        assign(p2, ., envir=envir) %>%
        layout(title=title %>% paste0('\n(logarhitmic scale)'),
               yaxis=list(type="log", range=c(0,log10(max(1,D$y))))) %>%
        assign(p_log, ., envir=envir)

      #if(p=='plotly_ts_Tested') stop()

      descr %<>% list_update(list(descr[[p]]%.%" (logarhitmic scale)") %>% setNames(p_log))
      
    }

  }

  assign('descriptions', descr, envir=envir) 

}
