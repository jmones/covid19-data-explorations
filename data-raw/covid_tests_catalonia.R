# Tests of COVID-19 in Catalonia
#
# Test of COVID-19 performed in Catalonia. Segregated by genre and municipality.
# Data retrieved from the open data portal of the Catalan Government
# https://analisi.transparenciacatalunya.cat/Salut/Registre-de-test-de-COVID-19-realitzats-a-Cataluny/jj6z-iyrp
#
# License
# From http://governobert.gencat.cat/ca/dades_obertes/dades-obertes-covid-19/:
# Avís legal: D’acord amb l’article 17.1 de la Llei 19/2014, la ©Generalitat de Catalunya permet la reutilització dels continguts i de les dades sempre que se'n citi la font i la data d'actualització i que no es desnaturalitzi la informació (article 8 de la Llei 37/2007) i també que no es contradigui amb una llicència específica.

library(drake)

covid_tests_catalonia_plan <- drake_plan(
  download.file("https://analisi.transparenciacatalunya.cat/api/views/jj6z-iyrp/rows.csv?accessType=DOWNLOAD&sorting=true", file_out("data-raw/covid_tests_catalonia.csv")),
  covid_tests_catalonia = read.csv(file_in("data-raw/covid_tests_catalonia.csv")),
  covid_tests_catalonia_write_to_data = save(covid_tests_catalonia, file=file_out("data/covid_tests_catalonia.rda"))
)