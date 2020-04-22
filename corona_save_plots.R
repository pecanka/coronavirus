save_plots_to_file = function(envir=.GlobalEnv, add_goback=TRUE, only_nonexistent=TRUE) {

  ps = ls(pattern='^plotly_', envir=envir) %>% sort()
  
  catn("Total number of plots: ", length(ps))
  
  ds = descriptions[sub("_Only_.*$",'', ps)]

  on.exit(setwd('..'))
  setwd('plots_plotly')
  
  deps = NULL
  if(file.exists('plotly_deps.rda')) load('plotly_deps.rda')
  
  for(p in ps) {
  
    filename = p%.%'.'%.%format_for_plots
    
    if(only_nonexistent && file.exists(filename)) next
    
    catn("Saving plot '"%.%p%.%"' to file '",filename,"' ...")
    
    if(format_for_plots=='html') {
      
      html_title = "Coronavirus: "%.%gsub("<[/]?b>","",ds[p])%.%" (by Pecanka Consulting)"
      args = list(as_widget(get(p)), file=filename, background='#000000', 
                  title=html_title, selfcontained=FALSE, libdir='plotly_files', 
                  deps=deps)
      deps = do.call(saveWidget2, args)
      
    } else if(format_for_plots=='png') {
    
      orca(get(p), file=filename)
      stop()
      
    } else error("Unknown format for plots '",format_for_plots,"'.")

    if(add_goback) {
      catn("Looking for where to place the 'Go back' code in the plotly file ...")
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

  }

}

make_index_html = function(envir=.GlobalEnv) {
  
  ps = ls(pattern='^plotly_', envir=envir) %>% sort()
  
  if(is_empty(ps)) {
    catn("No plotly plots found. HTML file not updated.")
    return()
  }
  
  catn("Updating index.html file ...")
  
  #catf = hijack(catn, file='index.html', append=TRUE)

  con = file('index.html', open='w')
  catf = hijack(writeLines, con=con)
  
  filename = function(p) 'plots_plotly/'%.%p%.%'.'%.%format_for_plots
  title = function(p) collapse0(c(toupper(countries[p]) %|||||% NULL, ds[[sub("_Only_.*$",'', p)]]), sep=": ")

  html_close = function()     catf("</body>\n</html>")
  main_close = function()     catf("</div>\n")
  main_open = function(hidden=FALSE, id='', class='main')
    catf("<div class='"%.%class%.%"' id='"%.%id%.%"' style='"%.%
         ifelse(hidden, 
                'display: none;',
                'display: block;')%.%
                #'display: none; height: 0px; overflow: hidden; border: 0px solid white; margin-top: 0px; margin-bottom: 0px; padding: 0px;',
                #'display: block; height: auto; overflow: visible; border: 1px solid silver; margin-top: 30px; margin-bottom: 30px; padding: 20px;')%.%
         "'>")
  list_open = function()      catf("  <div class='list'>")
  list_close = function()     catf("  </div>")
  ul_open = function()        catf("    <ul>")
  ul_close = function()       catf("    </ul>")  
  gap = function()            catf("    <p>")

  html_open = function()  catf("<!DOCTYPE HTML>\n<html lang='en'>")#, append=FALSE)
  html_head = function() {
  catf("<head>\n<title>Pecanka Consulting: Coronavirus plots</title>")
  catf("<link rel='stylesheet' type='text/css' href='style.css'>")
  catf("<script type='text/javascript' src='toggle.js' ></script>")
  catf("</head>\n<body>")
  }
  
  head_main = function() {
    catf("  <div class='head'>CORONOVIRUS: COVID-19</div><p>")
    catf("    <center>Jakub Pecanka, PhD (Pecanka Consulting,")
    catf("    <a href='https://www.pecanka.net'>www.pecanka.net</a>)</center><p>")
    catf("    <div class='descriptions'>A collection of plots showing the")
    catf("    current state and the historical progression of the COVID-19")
    catf("    pandemic in select countries around the world.<p>")
    catf("    <span style='font-size: 80%'>"%.%data_source%.%"</span>")
    catf("  </div>")
  }

  head_big = function(x)      catf("    <div class='note'>"%.%x%.%"</div>")
  head_small = function(x)    catf("    <div class='note2'>"%.%x%.%"</div>")
  head_small_ul = function(x) {
    ul_close()
    head_small(x)
    ul_open()
  }
  
  box_close = function(ul=TRUE) {
    if(ul) ul_close()
    list_close()
    main_close()
  }
  box_open = function(x, hidden=FALSE, id='', ul=TRUE, class='main') {
    main_open(hidden, id, class)
    list_open()
    head_big(x)
    if(ul) ul_open()
  }
  
  box_reopen = function(x, hidden=FALSE, id='', ul_close=TRUE, ul_open=TRUE, class='main') {
    box_close(ul_close)
    box_open(x, hidden, id, ul_open, class)
  }
  
  link = function(p)          catf("    <div class='link'><li><a href='"%.%filename(p)%.%"'>"%.%title(p)%.%"</a></li></div>")
  link_country = function(p)  {
    catf("    <a id='"%.%countries[p]%.%"'></a>")
    catf("    <div class='linkcountry' onclick=\"toggle_country('box_"%.%countries[p]%.%"', '"%.% country_colors[countries[p]] %.%"')\">")
    catf("    <a href='#"%.%countries[p]%.%"'>"%.%countries[p]%.%"</a>")
    catf("    </div>")
  }
  
  ## ==========================================
  
  first = which(!grepl('(Only)', ps))
  ord = ps[-first] %>% {order(sub('.*_Only_','',.)%.%.)}
  ps = c(ps[first], ps[-first][ord])
   
  ds = descriptions[sub("_Only_.*$",'', ps)]

  is_all = ps %>% grepl('.*_Only_', .) %>% not() %>% setNames(ps)
  is_new_country = ps %>% sub('.*_Only_','', .) %>% is_start_of_run() %>% setNames(ps)
  #browser()
  is_first_only_country = equals(ps, h1(ps[grepl('_Only_', ps)])) %>% setNames(ps)
  
  countries = sub('.*_','',ps) %>% setNames(ps)
  countries[is_all] = ''
  
  country_colors = Data4 %>% group_by(Country) %>% filter(row_number()==1) %>% select(Country, XCol) %>% {structure(.$XCol, names=.$Country)}
  
  #countries[!is_all] = countries[!is_all] %.% ": "
  
  is_gap = ps[is_all] %>% sub('_[^_]+$','',.) %>% sub('plot_','',.) %>% c(.[1],.) %>% str_diff() %>% setNames(ps[is_all])
  is_log = ps[is_all] %>% sub('_[^_]+$','',.) %>% sub('plot_','',.) %>% {regexpr('log',.)>0} %>% {. & !dplyr::lag(.)} %>% setNames(ps[is_all])

  html_open()
  html_head()
  main_open(FALSE)
  head_main()
  list_open()
  head_small('linear scale')
  ul_open()

  for(p in ps[is_all]) {
  
    if(p==ps[which.max(regexpr("Czechia",ds)>0)]) {
      box_reopen('Plots based on data available for CZECHIA only')
    }
    
    if(is_gap[p]) gap()
    if(is_log[p]) head_small_ul('logarhitmic scale')

    link(p)

  }
  
  box_reopen("INDIVIDUAL COUNTRIES")
  
  for(p in unique(ps[!is_all])) {
    if(is_new_country[p]) link_country(p)
  }
  
  for(p in ps[!is_all]) {
  
    if(is_new_country[p]) {
      box_reopen(countries[p], hidden=TRUE, id='box_'%.%countries[p], class='main2')
    }
    
    link(p)

  }
  
  box_close()
  
  html_close()

  close(con)
  
}
