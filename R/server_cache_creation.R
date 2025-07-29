server_cache_creation <- function() {
  cache_dir <- user_data_dir("rdcAVS") # OS-specific user data dir
  cli::cli_alert(paste0("Cache dir: ", cache_dir))

  if (!dir.exists(cache_dir)) {
    # First-time user: prompt for setup
    showModal(modalDialog(
      title = "Initialisation des Donn\u00e9es",
      paste0(
        "Ceci est la premi\u00e8re fois que vous ex\u00e9cutez l'application. Les donn\u00e9es g\u00e9ographiques et les autorisations seront initialis\u00e9es.",
        "\n Vous pouvez trouver les dossiers ici: ", cache_dir
      ),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))

    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  }
}
