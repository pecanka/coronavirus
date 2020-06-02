#source('d:/Dropbox/Projects/CoronaVirus/run_corona.R')
options(scipen=5)

setwd2()

wait("Running in the path '",getwd(),"'. Is this OK?")

require(utilbox)
llib(magrittr, dplyr, rvest, stringr, tidyr, lubridate, plotly, rjson, readxl)

#########################################################

url_ocdc = 'https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-'%p%t_day()%p%'.xlsx'

url_wom = 'https://www.worldometers.info/coronavirus/'
url_mzcz = 'https://onemocneni-aktualne.mzcr.cz/covid-19'
url_mzcz_api = 'https://onemocneni-aktualne.mzcr.cz/api/v1/covid-19/'

do_regular_update = !FALSE

do_download_data = FALSE
do_load_data = FALSE

do_force_fresh_data = FALSE
do_use_any_existing_data = FALSE

do_process_lag = FALSE
do_plot = FALSE
do_plot_lag = FALSE
do_plot_bar = FALSE
do_plot_ts = FALSE
do_plot_lm = FALSE

do_process_lag = do_plot = do_plot_lag = do_plot_bar = do_plot_ts = do_plot_lm = !FALSE

do_save_plotly_to_file = !FALSE
do_save_plotly_to_file_only_nonexistent = FALSE
do_make_index_html = TRUE

if(do_regular_update) {
  do_download_data = TRUE
  do_process_lag = do_plot = do_plot_lag = do_plot_bar = do_plot_ts = do_plot_lm = TRUE
  do_save_plotly_to_file = TRUE
  do_save_plotly_to_file_only_nonexistent = FALSE
  do_make_index_html = TRUE
}

format_for_plots = 'html'
#format_for_plots = 'png'

countries_focus = TRUE

PerPopulation = 1000000
Country0 = "Italy"
Countries_focus_narrow = c("Netherlands","Germany","USA","UK","Czechia")

Countries_focus_broad = c('Czechia','Italy','Germany','Netherlands',
  'Spain','USA','S. Korea','Austria','UK','Canada','Belgium','Denmark',
  'Norway','Poland','Sweden','Switzerland','France','Portugal','Brazil',
  'Israel','Australia','Ireland','Chile','Romania','Japan','Luxembourg',
  'Finland','Mexico','Greece','Slovakia')

CountryCZ = tibble(name='Czechia', prefix='mzcr_', url=url_mzcz)

#########################################################

source_from_web = grepl('^http[s]?://', script_dir())

if(source_from_web) {

  path = script_dir()
  path = 'https://raw.githubusercontent.com/pecanka/coronavirus/master/'
  
  warn("Sourcing R files from '",path,"' ...")
  source(path%p%'corona_download_data.R')
  source(path%p%'corona_download_data_ocdc.R')
  source(path%p%'corona_download_data_mzcr.R')
  source(path%p%'corona_lag.R')
  source(path%p%'corona_load_data.R')
  source(path%p%'corona_modify_saveWidget.R')
  source(path%p%'corona_plot_bar.R')
  source(path%p%'corona_plot_lag.R')
  source(path%p%'corona_plot_lm.R')
  source(path%p%'corona_plot_ts.R')
  source(path%p%'corona_read_data.R')
  source(path%p%'corona_save_plots.R')
  source(path%p%'corona_setup_plots.R')
  
  if(!file.exists('style.css')) download.file(path%p%'style.css', 'style.css')
  if(!file.exists('toggle.js')) download.file(path%p%'toggle.js', 'toggle.js')
  
  # re-source the modified files locally
  #source('d:/Jakub/covid/corona_download_data.R')
  #source('d:/Jakub/covid/corona_download_data_ocdc.R')
  #source('d:/Jakub/covid/corona_download_data_mzcr.R')

} else {

  source_pattern('corona_', announce=FALSE)

}

dir_create(c('data','plots_plotly','tables'))

available_countries_wom = download_country_list_wom(url_wom) %>%
  `if`(countries_focus, filter(., name %in% Countries_focus_broad), .)

if(!do_download_data)
  do_download_data = length(list.files('data', t_day()))==0

if(!do_load_data)
  do_load_data = !exists('Data') || !exists('Latest')

if(!do_process_lag)
  do_process_lag = !exists("Data_Lag_Cases")

if(do_download_data) {

  download_latest_all_wom()
  download_data_ocdc(url_ocdc, do_force_fresh_data)
  download_data_mzcr(CountryCZ)
  download_data_wom(available_countries_wom, url_wom)
  do_load_data = TRUE

}

if(do_load_data)
  load_data()       # side effect loads: Data, Data4, DataCZ, Latest

if(do_process_lag)
  Data_Lag_Cases = process_lag_cases_deaths(Data)

if(do_plot_lag)
  plot_lag(Data_Lag_Cases)

if(do_plot_bar)
  plot_bar(Latest)

if(do_plot_ts)
  plot_ts(Data4)

if(do_plot_lm)
  plot_lm(DataCZ)

announce_plots()

if(do_save_plotly_to_file)
  save_plots_to_file(only_nonexistent=do_save_plotly_to_file_only_nonexistent)
  
if(do_make_index_html)
  make_index_html()
  
catn("Finished.")
