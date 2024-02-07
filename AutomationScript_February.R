first_week_start <- "2020-02-01"
first_week_end <- "2020-02-07"

start_date <- "2020-02-01"
end_date <- "2020-02-29"

report.name <- "February 2020 Monthly Report"

params <- list(first_week_start = first_week_start,
               first_week_end = first_week_end,
               start_date = start_date, 
               end_date = end_date,
               report.name = report.name)

the.year <- lubridate::year(end_date)
the.month <- lubridate::month(end_date)
out.path <- sprintf("../Reports/Monthly Reports/%s/%s", the.year, the.month)
print(out.path)
dir.create(path = out.path, showWarnings = F, recursive = T)

rmarkdown::render(input = "Monthly Report.Rmd", output_file = sprintf("%s/%s.html",
                                                                      out.path, report.name), params = params)
