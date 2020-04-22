save_html2 = hijack(htmltools::save_html, deps=NULL)
save_html2 = sub_lang(save_html2, "deps <-", "if(is.null(deps)) deps <-")
save_html2 = append_body(save_html2, "deps", where='last')

saveWidget2 = hijack(htmlwidgets::saveWidget, deps=NULL)
saveWidget2 = sub_lang(saveWidget2, 'htmltools::save_html', 'deps <- save_html2')
saveWidget2 = sub_lang(saveWidget2, 'background)', 'background, deps = deps)')
saveWidget2 = append_body(saveWidget2, "invisible(deps)", where='last')

