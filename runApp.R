setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
devtools::load_all()
campagneApp()
