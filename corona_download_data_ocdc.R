download_data_ocdc = function(url, force=FALSE) {

  file = 'data/'%p%separate_path(url)$filename

  if(force || !file.exists(file)) {

    if(F && !url_exists(url)) {
      warn("File '",url,"' does not seem to exist.")
    }
    
    download.file(url, file, mode="wb")

  }
  
}
