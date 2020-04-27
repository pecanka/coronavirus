save_plots_to_file = function(envir=.GlobalEnv, add_tracer=TRUE, add_goback=TRUE, only_nonexistent=FALSE) {

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

    if(add_tracer) {
      catn("Adding tracer in the plotly file ...")
      content = readLines(filename)
      content[3] %<>% paste0("\n"%.%html_tracer_code())
      writeLines(content, filename)
    }
    
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

  # open connection
  con = file('index.html', open='w')
  wL = hijack(writeLines, con=con)

  filename = function(p)
    'plots_plotly/'%.%p%.%'.'%.%format_for_plots
  title = function(p)
    collapse0(c(toupper(countries[p]) %|||||% NULL, ds[[sub("_Only_.*$",'', p)]]), sep=": ")

  html_close = function()
    wL("</body>\n</html>")
  main_close = function()
    wL("</div>\n")
  main_open = function(hidden=FALSE, id='', class='main')
    wL("<div class='"%.%class%.%"' id='"%.%id%.%"' style='"%.%
         ifelse(hidden,
                'display: none;',
                'display: block;')%.%
                #'display: none; height: 0px; overflow: hidden; border: 0px solid white; margin-top: 0px; margin-bottom: 0px; padding: 0px;',
                #'display: block; height: auto; overflow: visible; border: 1px solid silver; margin-top: 30px; margin-bottom: 30px; padding: 20px;')%.%
         "'>")
  list_open = function()
    wL("  <div class='list'>")
  list_close = function()
    wL("  </div>")
  ul_open = function() {}
    #wL("    <ul>")
  ul_close = function() {}
    #wL("    </ul>")
  gap = function()
    wL("    <p>")

  html_open = function()  
    wL("<!DOCTYPE HTML>\n<html lang='en'>\n<head>")
    
  html_tracer = function()
    wL(html_tracer_code())

  html_head = function() {
    wL("  <title>Pecanka Consulting: Coronavirus plots</title>")
    wL("  <link rel='stylesheet' type='text/css' href='style.css'>")
    wL("  <script type='text/javascript' src='toggle.js' ></script>")
    wL("</head>\n<body>")
  }

  head_main = function() {
    wL("  <div class='head'>CORONOVIRUS: COVID-19</div><p>")
    wL("    <center>Jakub Pecanka, PhD (Pecanka Consulting,")
    wL("    <a href='https://www.pecanka.net'>www.pecanka.net</a>)</center><p>")
    wL("    <div class='descriptions'>A collection of plots showing the")
    wL("    current state and the historical progression of the COVID-19")
    wL("    pandemic in select countries around the world.<p>")
    wL("    <span style='font-size: 80%'>"%.%data_source%.%"</span>")
    wL("  </div>")
  }

  head_big = function(x)
    wL("    <div class='note'>"%.%x%.%"</div>")
  head_small = function(x)
    wL("    <div class='note2'>"%.%x%.%"</div>")
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

  link_open = function(class='link')
    wL("    <div class='"%.%class%.%"'>")
  link_close = function()
    wL("    </div>")

  add_link = function(p, open_link=TRUE, close_link=TRUE) {
    if(open_link) link_open()
    wL("      <li><a href='"%.%filename(p)%.%"'>"%.%title(p)%.%"</a></li>")
    if(close_link) link_close()
  }
    
  add_link_country = function(p)  {
    wL("    <a id='"%.%countries[p]%.%"'></a>")
    wL("    <div class='linkcountry' onclick=\"toggle_country('box_"%.%countries[p]%.%"', '"%.% country_colors[countries[p]] %.%"')\">")
    wL("    <a href='#"%.%countries[p]%.%"'>"%.%countries[p]%.%"</a>")
    wL("    </div>")
  }

  ## ==========================================

  first = which(!grepl('(Only)', ps))
  ord = ps[-first] %>% {order(sub('.*_Only_','',.)%.%.)}
  ps = c(ps[first], ps[-first][ord])

  ds = descriptions[sub("_Only_.*$",'', ps)]

  is_all = ps %>% grepl('.*_Only_', .) %>% not() %>% setNames(ps)
  is_new_country = ps %>% sub('.*_Only_','', .) %>% is_start_of_run() %>% setNames(ps)
  is_first_only_country = equals(ps, h1(ps[grepl('_Only_', ps)])) %>% setNames(ps)

  countries = sub('.*_','',ps) %>% setNames(ps)
  countries[is_all] = ''

  country_colors = Data4 %>% group_by(Country) %>% filter(row_number()==1) %>% select(Country, XCol) %>% {structure(.$XCol, names=.$Country)}

  plot_types = ps %>% sub('plotly_','',.) %>% sub('_.*$','',.)
  is_gap = plot_types %>% c(.[1],.) %>% str_diff() %>% setNames(ps)
  #is_log = plot_types %>% grepl('log',.) %>% {. & !dplyr::lag(.)} %>% setNames(ps)
  is_log = plot_types %>% grepl('log',.) %>% {. & is_start_of_run(.)} %>% setNames(ps)

  html_open()
  html_tracer()
  html_head()
  main_open(FALSE)
  head_main()
  list_open()
  head_small('linear scale')
  ul_open()

  for(p in ps[is_all]) {

    if(p %in% ps[w_first_positive(grepl("Czechia",ds))]) {
      box_reopen('Plots based on data available for CZECHIA only')
    }

    if(is_gap[p]) gap()
    if(is_log[p]) head_small_ul('logarhitmic scale')

    add_link(p)

  }

  box_reopen("INDIVIDUAL COUNTRIES")

  link_open('link2')
  for(p in unique(ps[!is_all])) {
    if(is_new_country[p]) add_link_country(p)
  }
  link_close()

  for(p in ps[!is_all]) {

    if(is_new_country[p]) {
      link_close()
      box_reopen(countries[p], hidden=TRUE, id='box_'%.%countries[p], class='main2')
      head_small('linear scale')
      link_open()
    }

    if(is_gap[p] && !is_new_country[p]) {
      link_close()
      gap()
    }
    if(is_log[p]) {
      head_small_ul('logarhitmic scale')
    }
    if(is_gap[p] && !is_new_country[p]) {
      link_open('link2')
    }

    add_link(p, FALSE, FALSE)

  }

  link_close()
  box_close()

  html_close()

  close(con)

}

html_tracer_code = function() {
  "<!-- Global site tag (gtag.js) - Google Analytics -->\n"%.%
  "  <script async src='https://www.googletagmanager.com/gtag/js?id=UA-162945949-1'></script>\n"%.%
  "  <script>\n"%.%
  "    window.dataLayer = window.dataLayer || [];\n"%.%
  "    function gtag(){dataLayer.push(arguments);}\n"%.%
  "    gtag('js', new Date());\n"%.%
  "    gtag('config', 'UA-162945949-2');\n"%.%
  "  </script>\n"
  "<!-- End of Global site tag (gtag.js) - Google Analytics -->"
}
