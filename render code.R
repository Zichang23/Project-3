utils::download.file("https://github.com/Zichang23/Project-3/archive/master.tar.gz", method = "libcurl")
shiny::runGitHub("Project-3", "Zichang23", subdir = "Project-3", ref = "main")

shiny::runGitHub('shiny_example', 'rstudio')

shiny::runGitHub("shiny-examples", "rstudio", subdir = "001-hello")
