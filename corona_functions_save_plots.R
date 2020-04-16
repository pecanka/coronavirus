save_plots_to_file = function(envir=.GlobalEnv) {

  setwd('plots_plotly')
  ds = descriptions
  catf = hijack(catn, file='../index.html', append=TRUE)
  catf("<!DOCTYPE HTML>\n<html lang='en'>\n<head>", append=FALSE)
  catf("<title>Pecanka Consulting: Coronavirus plots</title>")
  catf("<link rel='stylesheet' type='text/css' href='style.css'>")
  catf("</head>\n<body><div class='main'>")
  catf("<div class='head'>CORONOVIRUS: COVID-19</div><p>")
  catf("<center>Jakub Pecanka, PhD (Pecanka Consulting, <a href='https://www.pecanka.net'>www.pecanka.net</a>)</center><p>")
  catf("<div class='descriptions'>A collection of plots showing the current state")
  catf("and the historical progression of the COVID-19 pandemic in select countries around the world.<p>")
  catf("<span style='font-size: 80%'>"%.%data_source%.%"</span>")
  catf("</div><div class='list'><div class='note2'>linear scale</div><ul>")

  ps = ls(pattern='^plotly_', envir=envir) %>% sort()
  ds %<>% .[ps]

  is_gap = ps %>% sub('_[^_]+$','',.) %>% sub('plot_','',.) %>% c(.[1],.) %>% str_diff() %>% setNames(ps)
  is_log = ps %>% sub('_[^_]+$','',.) %>% sub('plot_','',.) %>% {regexpr('log',.)>0} %>% {. & !dplyr::lag(.)} %>% setNames(ps)

  
  for(p in ps) {
  
    filename = p%.%'.'%.%format_for_plots
    catn("Saving plot '"%.%p%.%"' to file ...")
    
    if(p==ps[which.max(regexpr("Czechia",ds)>0)]) {
      catf("</ul><div class='note'>Plots based on data available for CZECHIA only</div>")
      catf("<div class='note2'>linear scale</div><ul>")
    }
    
    if(is_gap[p]) catf("<p>")
    if(is_log[p]) catf("</ul><div class='note2'>logarhitmic scale</div><ul>")
    add_goback = FALSE

    pattern = ''
    if(regexpr(pattern,p)>0) {

      if(format_for_plots=='html') {
        html_title = "Coronavirus: "%.%gsub("<[/]?b>","",ds[p])%.%" (by Pecanka Consulting)"
        if(TRUE || !file.exists(filename)) {
          htmlwidgets::saveWidget(as_widget(get(p)), file=filename, background='#000000', title=html_title); add_goback = TRUE
        }
      } else if(format_for_plots=='png') {
        orca(get(p), file=filename)
        stop()
      } else error("Unknown format for plots '",format_for_plots,"'.")

    }

    if(add_goback) {
      content = readLines(filename)
      w = which(substr(content,1,32)=='<div id=\"htmlwidget_container\">')
      if(length(w)>0) {
        catn("Placing the 'Go back' code on line ",w," of the plotly file ...")
        content[w] %<>% paste0("<a href='../index.html'><div style='border: 1px solid #101010;"%.%
          " background-color: #505050; color: white; padding: 6px; position: absolute; z-index:10;"%.%
          " top: 0; left: 0; font-family: Calibri, Arial, Sans Serif;'>&larr;&nbsp;"%.%
          "Back to the overview of plots</div></a>", .)
        writeLines(content, filename)
      } else note("The 'Go back' code was not placed.")
    }

    catf("<div class='link'><li><a href='plots_plotly/"%.%filename%.%"'>",ds[p],"</a></li></div>")

  }
  catf("</ul></div><p></div></body>\n</html>")
  rm(catf)
  setwd('..')

}
