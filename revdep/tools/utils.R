library(httr)

cloud_url <- "https://xgyefaepu5.execute-api.us-east-1.amazonaws.com/"
id <- "e0ee9238-3d5f-4db6-ace4-8533735257ef"
id <- "0cf1ed96-d919-4d4b-b03e-79a0e29e63a1" # after greater number of jobs
id <- "2a75e7c6-422f-4077-9ee2-9da89fa2a5af" # after last tweaks on retry
id <- "944b7c92-758a-4f92-89ca-55e2ece9753a" # after higher rate limit
id <- "ffb83a12-16e1-484a-949d-58921a8d723f" # Rereun on 02/11/2021
id <- "18398b43-9ada-481c-8f37-c85f03096654" # Rereun on 03/11/2021
id <- "6c27cf90-6c69-4d82-a9a5-2d42accb51f8" # Rerun after just above for 3 failed to start
id <- "bb9dafa9-9a97-4c5d-b38e-95cf146311dd" # after retry update
id <- "e2de4e88-f37f-4dc5-b1dd-84451f777660" # 08/11/2021
id <- "0c961f35-f5a8-423f-91ef-446a22cd4aa6" # Re running only the 5 that are timeout
id <- "fe5698a2-595d-40f3-9d1d-ec64d25b334f" # 15/11/2021 running with different spot instance
id <- "5c39cf12-5dc4-4184-9a67-e5ca6c8ad422" # 15/11/2021 with updated knitr
id <- "eb815a4a-ac2e-42fe-ae6c-b4708f7314b9" # 28/05/2024 knitr 1.47
cloud_url <- modify_url(cloud_url, path = glue::glue("staging/check/{id}"))
auth_header <- add_headers('x-api-key' = Sys.getenv("RSTUDIO_CLOUD_REVDEP_KEY"))

# Get time ----------------------------------------------------------------
res <- GET(cloud_url, auth_header)
res <- content(res, as = "text")
jqr::jq(res, ".r_version")
res_time <- jqr::jq(res, '. | with_entries(select(.key | endswith("stamp")))')
res_time <- jsonlite::parse_json(res_time)

library(lubridate)
# run duration
as.period(ymd_hms(res_time$finished_timestamp) - ymd_hms(res_time$started_timestamp))

# Total duration
as.period(ymd_hms(res_time$finished_timestamp) - ymd_hms(res_time$created_timestamp))

# Get packages in failed job

res <- GET(glue::glue("{cloud_url}/status"), auth_header)
res <- content(res, as = "text")
jqr::jq(jqr::index(res))

res <- GET(glue::glue("{cloud_url}/status/SUCCEEDED"), auth_header)
res <- content(res, as = "text")
res <- jqr::jq(res, '.packages')
res

res <- GET(glue::glue("{cloud_url}/status/FAILED"), auth_header)
res <- content(res, as = "text")
res <- jqr::jq(res, '.packages')
pkgs_failed <- jsonlite::parse_json(res, simplifyVector = TRUE)
pkgs_failed

dput(pkgs_failed)

reasons <- purrr::map(purrr::set_names(pkgs_failed), ~ {
  res <- GET(glue::glue("{cloud_url}/packages/{.x}"), auth_header)
  res <- content(res, as = "text")
  res <- jqr::jq(res, '.statusReason')
  jsonlite::parse_json(res, simplifyVector = TRUE)
})

reasons
print(tibble::enframe(purrr::simplify(reasons)), n = Inf)

library(dplyr)

tibble::enframe(purrr::simplify(reasons)) %>%
  mutate(value = gsub("(Host EC2).*( terminated)", "\\1\\2", value)) %>%
  count(value)

# Result of a package -----------------------------------------------------

pkg <- pkgs_failed[1]
pkg <- "clustermq"
res <- GET(glue::glue("{cloud_url}/packages/{pkg}"), auth_header)
res <- content(res, as = "text")
res <- jqr::jq(res, ".")
res

# Download ----------------------------------------------------------------
out  <- paste0(pkg, ".tar.gz")
res <- GET(glue::glue("{cloud_url}/packages/{pkg}/results.tar.gz"), auth_header, write_disk(out))
archive::archive(out)
res <- jqr::jq(res)

# Build report ------------------------------------------------------------
rmarkdown::render("revdep/failures.md", rmarkdown::html_document(toc = TRUE, toc_depth = 1, toc_float = TRUE))



