plot_lm = function(Data, envir=.GlobalEnv) {

  catn("Plotting linear model plots...")

  min_Cases = 250
  day0 = Data %>% filter(Cases>=min_Cases) %>% slice(1) %>% pull(Date) # '2020-03-15' for min_Cases=250
  splits = '2020-03-28'

  descr = get('descriptions', envir=envir)
  
  descr %<>% list_update(list(plotly_z_lm_daily="Linear model: <b>DAILY CASES</b> vs <b>DAILY TESTS</b> (Czechia)"))
  descr %<>% list_update(list(plotly_z_lm_total="Linear model: <b>TOTAL CASES</b> vs <b>TOTAL TESTS</b> (Czechia)"))
  
  assign('descriptions', descr, envir=envir) 

  title = '<b>Czech Republic</b>: dependence of discovered cases on performed tests\n'%.%
          "<span style='font-size: 75%; border: 1px solid silver'>"%.%
          '<b>Model 0: </b>simple LRM (i.e. @y~@x, R<sup>2</sup>=@r0)'%.%
          ' based on all days after '%.%day0%.%'\n'%.%
          '<b>Model 1: </b>simple LRM (i.e. @y~@x) based on all days from '%.%
          day0%.%' (the first day with at least '%.%min_Cases%.%
          ' cases)\nand up to '%.%splits[1]%.%' (the day where a qualitative'%.%
          ' shift started to appear, see the note below)\n'%.%
          '<b>Model 2: </b>simple LRM (i.e. @y~@x)'%.%
          ' based on the days after '%.%splits[1]%.%
          '</span>'

  explain_model = 'Note: Model 0 works with all data, while Models 1 and 2 work'%.%
    ' with two disjoint periods (split by '%.%day0%.%'). The observed differences'%.%
    ' among the 3 models illustrate the apparent\nchanges in the relationship'%.%
    ' between the number of discovered cases and the number of performed tests,'%.%
    ' which initially appeared to be stronger than in the more recent period.'

  DataCZ250 = DataCZ %>% mutate(DateS=Date %>% sub('[0-9]+-','',.)) %>%
    filter(Date>=day0)
    
  for(p in names(descr)) {
  
    if(regexpr('plotly_z_lm',p)<0) next

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
             yaxis=list(title=set_axis_lab(descr[[p]], '[^<>]+<[bB]>')),
             xaxis=list(title='')) %>%
      layout(annotations=annot_bottom %modify% list(text=set_axis_lab(descr[[p]]), y=-0.085)) %>%
      layout(margin=list(l=50, r=50, b=125, t=50, pad=1)) %>%
      layout(annotations=annot_bottom %modify% list(text=explain_model, y=-0.225, x=0.48)) %>%
      layout(annotations=annot_source %modify% list(y=-0.3)) %>%
      layout(annotations=annot_author %modify% list(y=-0.3, x=1)) %>%
      assign(p, ., envir=envir)

  }

}
