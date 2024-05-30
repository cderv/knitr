id <- "eb815a4a-ac2e-42fe-ae6c-b4708f7314b9"

revdepcheck::cloud_summary(id)
revdepcheck::cloud_report(id)

cloud_res <- revdepcheck::cloud_results(id)

files <- fs::dir_ls(glue::glue("revdep/cloud/{id}/"), glob = "*/dependency_install.log", recurse = TRUE)

# Error 500
res <- purrr::map(files, ~ {
  content <- xfun::read_utf8(.x)
  error <- grep("ERROR 50\\d", content, value = TRUE)
  gsub("^.*(ERROR 50\\d.*)$", "\\1", error)
}, .progress = TRUE)

length(res)
res_issue <- purrr::compact(res)
if (length(res_issue)) range(purrr::map_int(res_issue, length))

# After retries
res <- purrr::map(files, ~ {
  content <- xfun::read_utf8(.x)
  error <- grep("'wget' call had nonzero exit status", content, value = TRUE)
  stringr::str_trim(error)
})

length(res)
res_issue <- purrr::compact(res)
if (length(res_issue)) range(purrr::map_int(res_issue, length))

