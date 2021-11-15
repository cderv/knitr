id <- "944b7c92-758a-4f92-89ca-55e2ece9753a"

id <- "e2de4e88-f37f-4dc5-b1dd-84451f777660" # clustermq special run by josh

revdepcheck::cloud_summary(id)
revdepcheck::cloud_report(id)

files <- fs::dir_ls(glue::glue("revdep/cloud/{id}/"), glob = "*/dependency_install.log", recurse = TRUE)

# Error 500
res <- purrr::map(files, ~ {
  content <- xfun::read_utf8(.x)
  error <- grep("ERROR 50\\d", content, value = TRUE)
  gsub("^.*(ERROR 50\\d.*)$", "\\1", error)
})

length(res)

# After retries
res <- purrr::map(files, ~ {
  content <- xfun::read_utf8(.x)
  error <- grep("'wget' call had nonzero exit status", content, value = TRUE)
  stringr::str_trim(error)
})

length(res)
res_issue <- purrr::compact(res)
if (length(res_issue)) range(purrr::map_int(, length))

