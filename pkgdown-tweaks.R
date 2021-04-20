
tweak_site <- function(write = FALSE) {

  system("git checkout gh-pages")

  gitignore <- c(
    ".Rproj.user",
    ".Rhistory",
    ".Rdata",
    ".DS_Store",
    ".httr-oauth"
  )
  if(write) writeLines(gitignore, ".gitignore")


  # lines to be inserted into header
  new_lines <- c(
    ' ',
    '<!-- adds plausible.io -->',
    '<script async defer data-domain="flametree.djnavarro.net" src="https://plausible.io/js/plausible.js"></script>',
    ' '
  )

  # files into which lines need insertion
  html_files <- list.files(
    pattern = "html$",
    recursive = TRUE,
    full.names = TRUE
  )

  # function to insert lines
  insert_lines <- function(file, new_lines) {
    lines <- readLines(file)
    ind <- which(grepl(x = lines, pattern = "</head>"))

    # assume file doesn't begin or end with </head>
    lines <- c(
      lines[1:(ind[1]-1)],
      new_lines,
      lines[ind[1]:length(lines)]
    )
    if(write) {
      writeLines(lines, file)
      cat("updating header: ", file, "\n")
    }
  }

  # insert for all files
  for(f in html_files) {
    insert_lines(f, new_lines)
  }


  system("git add .")
  system("git commit -m 'tweaks header'")
  system("git checkout master")

}

tweak_site(write = FALSE)
