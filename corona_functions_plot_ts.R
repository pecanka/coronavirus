plot_ts = function(Data, envir=.GlobalEnv) {

  catn("Plotting progression plots...")

  descr = get('descriptions', envir=envir)
  
  descr %<>% c(plotly_ts_Cases="Time series: <b>NUMBER OF CASES</b>")
  descr %<>% c(plotly_ts_Deaths="Time series: <b>NUMBER OF DEATHS</b>")
  descr %<>% c(plotly_ts_Infected="Time series: <b>NUMBER OF ACTIVE INFECTIONS</b>")
  descr %<>% c(plotly_ts_Tested="Time series: <b>NUMBER OF TESTS</b>")

  descr %<>% c(plotly_ts_DailyCases="Time series: <b>NUMBER OF DAILY CASES</b>")
  descr %<>% c(plotly_ts_DailyDeaths="Time series: <b>NUMBER OF DAILY DEATHS</b>")
  descr %<>% c(plotly_ts_DailyInfected="Time series: <b>NUMBER OF DAILY ACTIVE INFECTIONS</b>")
  descr %<>% c(plotly_ts_DailyTested="Time series: <b>NUMBER OF DAILY TESTS</b>")

  descr %<>% c(plotly_tspop_CasesPop="Time series: <b>NUMBER OF CASES PER POPULATION</b>")
  descr %<>% c(plotly_tspop_DeathsPop="Time series: <b>NUMBER OF DEATHS PER POPULATION</b>")
  descr %<>% c(plotly_tspop_InfectedPop="Time series: <b>NUMBER OF ACTIVE INFECTIONS PER POPULATION</b>")
  descr %<>% c(plotly_tspop_TestedPop="Time series: <b>NUMBER OF TESTS PER POPULATION</b>")

  descr %<>% c(plotly_tsrate_DailyCasesRatio="Time series: <b>RATE OF DAILY CASES</b>")
  descr %<>% c(plotly_tsrate_DailyDeathsRatio="Time series: <b>RATE OF DAILY DEATHS</b>")
  descr %<>% c(plotly_tsrate_DailyInfectedRatio="Time series: <b>RATE OF DAILY ACTIVE INFECTIONS</b>")
  descr %<>% c(plotly_tsrate_DailyTestedRatio="Time series: <b>RATE OF DAILY TESTS</b>")

  for(p in names(descriptions)) {

    if(regexpr('plotly_ts',p)<0) next

    v = p %>% sub('.*_','',.)
    p_log = sub('_ts','_xtslog',p)
    title = set_titles(descriptions, 'Progression of the')[p]

    D = Data %>% arrange(Country) %>% mutate(y=!!sym(v)) %>%
      select(Country, Date, y, XCol) %>% filter(complete.cases(.))

    if(is_unique(D$Country))
      D %<>% bind_rows(slice(.,1) %>% mutate(Country='&nbsp;', fCountry=as_factor(Country), XCol='black'), .)

    D %<>% mutate(fCountry=as_factor(Country))

    plot_ly(D, x=~Date, y=~y, color=~fCountry, name=~Country, colors=~XCol, type='scatter', mode='lines',
            line=list(color=~XCol), showlegend=TRUE) %>%
      layout(title=title, xaxis=list(title=""),
             yaxis=list(title=set_axis_lab(descriptions[p]),
                        tickformat=ifelse('Ratio'%match%p,'%',''))) %>%
      layout(plot_bgcolor='black', paper_bgcolor='black', font=list(color='white')) %>%
      layout(margin=list(l=50, r=50, b=90, t=35, pad=1)) %>%
      layout(annotations=annot_source) %>%
      layout(annotations=annot_author) %>%
      assign(p, ., envir=envir) %>%
      layout(title=title %>% paste0('\n(logarhitmic scale)'),
             yaxis=list(type="log", range=c(0,log10(max(1,D$y))))) %>%
      assign(p_log, ., envir=envir)

    #if(p=='plotly_ts_Tested') stop()

    descr %<>% c(descr[p]%.%" (logarhitmic scale)" %>% setNames(p_log))

  }

  assign('descriptions', descr, envir=envir) 

}
