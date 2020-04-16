plot_lag = function(Data, envir=.GlobalEnv, envir1=environment()) {

  catn("Plotting lag comparisons with ",Country0," ...")

  descr = get('descriptions', envir=envir)
  
  descr %<>% c(structure("Country comparison: <b>LAGs BEHIND "%.%toupper(Country0)%.%
                         "</b> (in number of days)", names='plotly_lag'%.%Country0))
  
  assign('descriptions', descr, envir=envir) 

  title = "Progression of the number of cases and deaths: a comparison with "%.%Country0
  explain_lag = "Each country's progression of case counts is matched up against "%.%Country0%.%
    "'s case count, which determines the number of days the given country lags behind "%.%Country0%.%".\n"%.%
    "The resulting matching is shown on the top row, while the bottom row shows the death count progression"%.%
    " with "%.%Country0%.%"'s counts shifted by the determined lag."

  # Produce plots individual for individual countries
  plots_Cases = plots_Deaths = list()
  for(C in Countries_focus_narrow) {

    D3 = Data %>% filter(Country==C) %T>%
      write.table(file='tables/'%.%C%.%'_vs_'%.%Country0%.%'_'%.%t_day()%.%'.txt', row.names=FALSE, quote=FALSE) %>%
      {bind_rows(select(., -ends_with('0')),
                 mutate(., Date0=Date) %>% select(ends_with('0')) %>% rename_all(~sub('0$','',.x)))} %>%
      mutate(Date=sub('[0-9]+-','',Date))

    Col = c("#cc4125","#64dd60") %>% setNames(c(Country0, C)) %>% pif(C<Country0, rev)

    for(v in c('Cases','Deaths')) {

      p = 'plots_'%.%v

      D3 %<>% mutate(y=!!sym(v), col=Col[Country])

      plot_ly(D3, x=~Date, y=~y, color=~Country, name=~Country, colors=Col,
              type='scatter', mode='lines', showlegend=TRUE) %>%
        layout(legend=list(x = 0.1, y = 0.5),
               yaxis=list(title=ifelse(C==Countries_focus_narrow[1], v, "")),
               xaxis=list(title="", showticklabels=FALSE)) %>%
        add_trace(x=~Date, y=~y, data=filter(D3,Country==C), colors=Col[C]) %>%
        add_annotations(text = C,        x = 0.1, y = 0.9, xref='paper', yref='paper', showarrow = FALSE,
                        xanchor='left', font = list(color = Col[2-as.numeric(C<Country0)], size = 14)) %>%
        add_annotations(text = Country0, x = 0.1, y = 0.8, xref='paper', yref='paper', showarrow = FALSE,
                        xanchor='left', font = list(color = Col[1+as.numeric(C<Country0)], size = 14)) %>%
        list() %>%
        append(get(p, envir=envir1), .) %>%
        assign(p, ., envir=envir1)

    }

  }

  # Put the plots into a plot grid
  append(plots_Cases, plots_Deaths) %>%
    append(list(nrows=2, shareY = FALSE, titleY=TRUE, titleX = TRUE)) %>%
    do.call(subplot, .) %>%
    layout(title=list(text=title, y=1.35), showlegend=FALSE) %>%
    layout(margin=list(l=50, r=50, b=90, t=50, pad=1)) %>%
    layout(plot_bgcolor='black', paper_bgcolor='black', font=list(color='white')) %>%
    layout(annotations=annot_bottom %modify% list(text=explain_lag, y=-0.12, x=0.48)) %>%
    layout(annotations=annot_source) %>%
    layout(annotations=annot_author %modify% list(x=1)) %>%
    assign('plotly_lag'%.%Country0, ., envir=envir)

}
