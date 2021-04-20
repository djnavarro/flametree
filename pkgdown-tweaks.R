
system("git checkout gh-pages")

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
  lines <- read.lines(file)
  ind <- which(grepl(x = lines, pattern = "</head>"))
  #if(is.null(ind)) rlang::warn(paste0("no </head> line found in: ", file))
  #if(length(ind) > 1) rlang::warn(paste0("multiple </head> lines found in: ", file))

  # assume file doesn't begin or end with </head>
  lines <- c(
    lines[1:(ind[1]-1)],
    new_lines,
    lines[ind[1]:length(lines)]
  )
  write.lines(lines, file)
}

# insert for all files
for(file in html_files) {
  insert_lines(file, new_lines)
}

system("git add .")
system("git commit -m 'tweaks header'")
system("git checkout master")
