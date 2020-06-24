# render_report.r file (note the use of file.path() to avoid any dependency on the operative system :wink: )
# based on this example: https://community.rstudio.com/t/is-it-possible-to-save-the-html-output-in-a-directory-which-is-not-the-one-where-the-rmd-file-resides/3588/12
library(rmarkdown)
code_dir <- "code/2020-06-07-test-post-two"
county = "New Haven"
report_filename <- paste0("county", "_summary.Rmd")
report_filename <- file.path(code_dir, report_filename)
output_dir <- path_to_static_june
output <- file.path("..", output_dir)
render(report_filename,
       output_dir = output_dir,
       output_file = paste0(county %>% tolower() %>% str_replace(" ", "_"), "_summary.html"),
       params = list(output_dir = output, county = county))

#myreport.Rmd file

render(report_filename,
       output_dir = output_dir,
       output_file = paste0(county %>% tolower() %>% str_replace(" ", "_"), "_summary.html"),
       params = list(output_dir = output, county = county))

