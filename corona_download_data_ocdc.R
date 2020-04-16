download_data_ocdc = function(url, force=FALSE) {
  file = 'data/'%.%separate_path(url)$filename
  if(force || !file.exists(file)) {
    if(!url_exists(url)) {
      warn("File '",url,"' does not seem to exist.")
    } else {
      download.file(url, file, mode="wb")
    }
  }
}
