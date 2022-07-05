library(rmarkdown)
source("./wrangle/functions.R")
source("./wrangle/load.R")

country_codes = c("AGO", "BEN", "RWA")
country_labels = c("Angola", "Benin", "Rwanda")
city_labels = c("Luanda", "Porto-Novo","Kigali")

render_report = function(code, country, city) {
  rmarkdown::render(
    "input.Rmd", params = list(
      code = code,
      country = country,
      city = city
    ),
    output_file = paste0(country, ".pdf"),
    output_dir = "./country-reports"
  )
}

for (i in seq_along(country_codes)) {
  load()
  code <- country_codes[i]
  name <- country_labels[i]
  city <- city_labels[i]
  print(code)
  print(name)
  print(city)

  load_country(code, name, city)
  bounds(city)
  render_report(code, name, city)
}

