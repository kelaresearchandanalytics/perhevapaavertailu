# sass::sass(
#   sass::sass_file("inst/app/www/custom.scss"),
#   output = "inst/app/www/custom.css"
# )

# Set options here
options(golem.app.prod = TRUE) # TRUE = production mode, FALSE = development mode
options(shiny.useragg = TRUE)
options(sass.cache = FALSE)
options(OutDec = ",")
options(shiny.autoreload = TRUE)
# options(big.mark=" ")

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
# run_app(enableBookmarking = "server")
run_app()
# rstudioapi::restartSession()

