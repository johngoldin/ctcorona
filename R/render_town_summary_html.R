# render_report.r file (note the use of file.path() to avoid any dependency on the operative system :wink: )
# based on this example: https://community.rstudio.com/t/is-it-possible-to-save-the-html-output-in-a-directory-which-is-not-the-one-where-the-rmd-file-resides/3588/12
library(rmarkdown)
library(future)
start_rendering_time <- Sys.time()
code_dir <- "code"
county = "New Haven"
report_filename <- paste0(file.path(getwd(), code_dir, "county_summary.Rmd"))
output_dir <- paste0(path_to_static_june, "index.en_files/")

the_counties <- c("Fairfield",  "Hartford",   "Litchfield",
                  "Middlesex",  "New Haven",  "New London",
                  "Tolland",    "Windham" )

render_county <- function(acounty) {
  print(paste("About to create html file for", acounty, Sys.time()))
  plan(multicore)
  f <- future({
    render(report_filename,
           output_dir = output_dir,
           output_file = paste0(acounty %>% tolower() %>% str_replace(" ", "_"), "_summary.html"),
           params = list(output_dir = output_dir, county = acounty))
  })
  print(paste("Created html file for", acounty, Sys.time()))

}
original_render_county <- function(acounty) {
  print(paste("About to create html file for", acounty, Sys.time()))
  render(report_filename,
         output_dir = output_dir,
         output_file = paste0(acounty %>% tolower() %>% str_replace(" ", "_"), "_summary.html"),
         params = list(output_dir = output_dir, county = acounty))
  print(paste("Created html file for", acounty, Sys.time()))
}

# library(furrr)


walk(the_counties, render_county)
# walk(the_counties, original_render_county)

# future_walk(the_counties, original_render_county)

total_rendering_time <- Sys.time()



