plot_bar = function(Latest, envir=.GlobalEnv) {

  catn("Plotting barplots...")

  descr = get('descriptions', envir=envir)

  descr %<>% list_update(list(plotly_bar_Cases="Barplot: <b>NUMBER OF CASES</b>"))
  descr %<>% list_update(list(plotly_bar_ActiveCases="Barplot: <b>NUMBER OF ACTIVE CASES</b>"))
  descr %<>% list_update(list(plotly_bar_CriticalCases="Barplot: <b>NUMBER OF CRITICAL/SERIOUS CASES</b>"))
  descr %<>% list_update(list(plotly_bar_Deaths="Barplot: <b>NUMBER OF DEATHS</b>"))
  descr %<>% list_update(list(plotly_bar_Recovered="Barplot: <b>NUMBER OF RECOVERED PATIENTS</b>"))
  descr %<>% list_update(list(plotly_bar_TotalTests="Barplot: <b>NUMBER OF DIAGNOSTIC TESTS</b>"))
  descr %<>% list_update(list(plotly_bar_NewDeaths="Barplot: <b>NUMBER OF NEW DEATHS</b>"))
  descr %<>% list_update(list(plotly_barpop_CasesPop="Barplot: <b>NUMBER OF CASES PER POPULATION</b>"))
  descr %<>% list_update(list(plotly_barpop_ActiveCasesPop="Barplot: <b>NUMBER OF ACTIVE CASES PER POPULATION</b>"))
  descr %<>% list_update(list(plotly_barpop_CriticalCasesPop="Barplot: <b>NUMBER OF CRITICAL/SERIOUS CASES PER POPULATION</b>"))
  descr %<>% list_update(list(plotly_barpop_DeathsPop="Barplot: <b>NUMBER OF DEATHS PER POPULATION</b>"))
  descr %<>% list_update(list(plotly_barpop_Recovered="Barplot: <b>NUMBER OF RECOVERED PATIENTS PER POPULATION</b>"))
  descr %<>% list_update(list(plotly_barpop_NewDeathsPop="Barplot: <b>NUMBER OF NEW DEATHS PER POPULATION</b>"))
  descr %<>% list_update(list(plotly_barpop_TotalTestsPop="Barplot: <b>NUMBER OF DIAGNOSTIC TESTS PER POPULATION</b>"))

  for(p in names(descr)) {

    if(regexpr('plotly_bar',p)<0) next

    v = p %>% sub('.*_','',.)

    #Latest %>% arrange(Country) %>% mutate(y=!!sym(v)) %>% arrange(desc(y)) %>% select(Country, y, XCol) %>% mutate()

    ## This is carefully set up so that the colors are based on 'Country' even
    ## after the reordering of categories according to value. Plotly requires
    ## the use of factors as the basis for the x-axis and they must be made from
    ## numerical value (which gives the order) and given the desired labels
    ## (i.e. Country, in this case)
    D = Latest %>% arrange(Country) %>% mutate(y=!!sym(v)) %>%
      select(Country, y, XCol) %>% filter(complete.cases(.)) %>% arrange(desc(y)) %>%
      mutate(fCountry=as_factor(Country))
      #mutate(fCountry=factor(1:n(), labels=Country))

    plot_ly(D, x=~fCountry, y=~y, color=~fCountry, name=~Country, colors=~XCol, type='bar') %>%
      layout(title = set_titles(descr[[p]], 'Current'),
             yaxis = list(title=set_axis_lab(descriptions[[p]])),
             xaxis = list(title="")) %>%#, type="category", categoryorder="array", categoryarray=~fCountry)) %>%
      layout(plot_bgcolor='black', paper_bgcolor='black', font=list(color='white')) %>%
      layout(margin=list(l=50, r=50, b=55, t=50, pad=1)) %>%
      layout(annotations=annot_source) %>%
      layout(annotations=annot_author) %>%
      assign(p, ., envir=envir)

  }

  assign('descriptions', descr, envir=envir) 

}
