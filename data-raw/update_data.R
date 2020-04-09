
download.file("https://analisi.transparenciacatalunya.cat/api/views/xuwf-dxjd/rows.csv?accessType=DOWNLOAD&sorting=true", "covid_tests_catalonia.csv") 
covid_tests_catalonia = read.csv("covid_tests_catalonia.csv")
usethis::use_data("covid_tests_catalonia")


