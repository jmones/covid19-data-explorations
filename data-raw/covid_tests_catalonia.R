# Tests of COVID-19 in Catalonia
#
# Test of COVID-19 performed in Catalonia. Segregated by genre and basic health area (àrea bàsica de salut, ABS).
# Data retrieved from the open data portal of the Catalan Government
# https://analisi.transparenciacatalunya.cat/ca/Salut/Registre-de-test-de-COVID-19-realitzats-a-Cataluny/xuwf-dxjd
#
# License
# From http://governobert.gencat.cat/ca/dades_obertes/dades-obertes-covid-19/:
# Avís legal: D’acord amb l’article 17.1 de la Llei 19/2014, la ©Generalitat de Catalunya permet la reutilització dels continguts i de les dades sempre que se'n citi la font i la data d'actualització i que no es desnaturalitzi la informació (article 8 de la Llei 37/2007) i també que no es contradigui amb una llicència específica.

download.file("https://analisi.transparenciacatalunya.cat/api/views/xuwf-dxjd/rows.csv?accessType=DOWNLOAD&sorting=true", "covid_tests_catalonia.csv") 
covid_tests_catalonia = read.csv("covid_tests_catalonia.csv")
usethis::use_data(covid_tests_catalonia, overwrite=TRUE)
