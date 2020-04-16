
descriptions = ylabs = c()

data_source = "Based on data from <a href='"%.%url_wom%.%"'>Worldometers</a>, "%.%
  "<a href='"%.%url_ocdc%.%"'>OCDC</a>"%.%" and <a href='"%.%url_mzcz%.%"'>mzcr.cz</a>"

text_source = "<span style='font-size: 90%;'><i>Source: "%.%data_source%.%"</i></span>"
text_author = "<span style='font-size: 90%;'><i>Author: Jakub Pecanka (Pecanka Consulting,"%.%
              " <a href='https://www.pecanka.net'>www.pecanka.net</a>)</i></span>"

annot_bottom = list(yref="paper", xref="paper", align='left',
                    xanchor='center', showarrow=FALSE)
annot_source = list(text=text_source, y=-0.19, x=-0.04, yref="paper",
                    xref="paper", xanchor='left', showarrow=FALSE)
annot_author = list(text=text_author, y=-0.19, x=1.12, yref="paper",
                    xref="paper", xanchor='right', showarrow=FALSE)


set_axis_lab = function(d, pattern1='.+<[bB]>', pattern2='<.+') {
  d %>% sub(pattern1,'',.) %>% sub(pattern2,'',.) %>%
    str_trim() %>% tolower %>% setNames(names(d))
}

set_titles = function(descriptions, what='Progression of the') {
  descriptions %>% set_axis_lab() %>%
    inset(seq_along(.), ifelse(regexpr('population',.)<0,
                               ., sub("population",N2T(PerPopulation)%.%" inhabitants",.))) %>%
    gsub('italy','Italy',.) %>%
    paste(toupperfirst(what),.,"(as of "%.%t_day()%.%")") %>%
    setNames(names(descriptions))
}

announce_plots = function() {
  ls(pattern='^plotly_', envir=.GlobalEnv) %>%   
    str_lengthen() %>%
    split_into_groups(floor(options()$width/max(1,nchar(.)))) %>%
    lapply(paste, collapse='\t  ') %>%
    {do.call('paste', . %append% list(sep='\n\t'))} %>%
    catn("\nCreated plot objects:\n\n\t", .,'\n')
}
