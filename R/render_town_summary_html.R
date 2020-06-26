# render_report.r file (note the use of file.path() to avoid any dependency on the operative system :wink: )
# based on this example: https://community.rstudio.com/t/is-it-possible-to-save-the-html-output-in-a-directory-which-is-not-the-one-where-the-rmd-file-resides/3588/12
library(rmarkdown)
code_dir <- "code"
county = "New Haven"
report_filename <- paste0(file.path(getwd(), code_dir, "county_summary.Rmd"))
output_dir <- paste0(path_to_static_june, "index.en_files/")

the_counties <- c("Fairfield",  "Hartford",   "Litchfield",
                  "Middlesex",  "New Haven",  "New London",
                  "Tolland",    "Windham" )

render_county <- function(acounty) {
        render(report_filename,
               output_dir = output_dir,
               output_file = paste0(acounty %>% tolower() %>% str_replace(" ", "_"), "_summary.html"),
               params = list(output_dir = output_dir, county = acounty))
        print(paste("Created html file for", acounty))
}


walk(the_counties, render_county)



