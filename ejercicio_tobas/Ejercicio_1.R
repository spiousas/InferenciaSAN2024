# descargar los datos de OSF #
pacman::p_load(osfr)

# obtengo info del repo de OSF:
toba_repo <- osf_retrieve_node("https://osf.io/nxtvk/")

# obtengo la lista de archivos disponibles
toba_files <- osf_ls_files(toba_paper)
toba_files

# descargo dos archivos con los datos que me importan y otro con la metadata
osf_download(toba_files[c(4:6),], path = "./")

# Una vez descargados, pueden abrir el archivo de metadata para entender
# el contenido de los archivos de datos