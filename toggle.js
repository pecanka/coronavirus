function toggle_country(country, color) {

  var height = document.getElementById(country).style.height;
  var overflow = document.getElementById(country).style.overflow;
  var display = document.getElementById(country).style.display;
  var divs = document.querySelectorAll('*[id^="box_"]');
  
  for(var i=0;i<divs.length;i++) {
    
    divs[i].style.display="none" ;
    divs[i].style.overflow="hidden";
    divs[i].style.marginTop="0px" ;
    
  }
  
  if (display === "none") {
    document.getElementById(country).style.display="block" ;
    document.getElementById(country).style.overflow="visible" ;
    document.getElementById(country).style.background=color ;
    document.getElementById(country).style.marginTop="30px" ;
  } else {
    document.getElementById(country).style.display="none" ;
    document.getElementById(country).style.overflow="hidden";
    document.getElementById(country).style.marginTop="0px" ;
  }
}
