# pull_dma_population.R
#
# Source-safe helper for estimating DMA-level population from Census county
# population and an unofficial open county-to-DMA mapping. No network request is
# made when this file is sourced; call pull_dma_population() explicitly.

`%||%` <- function(x, y) if (is.null(x)) y else x

dma_clean_county <- function(x) {
  x <- toupper(as.character(x))
  x <- gsub("\\.", "", x)
  x <- gsub("&", "AND", x)
  x <- gsub("^ST ", "SAINT ", x)
  x <- gsub("^STE ", "SAINTE ", x)
  x <- gsub("[^A-Z0-9 ]", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

dma_clean_census_county <- function(x) {
  x <- toupper(as.character(x))
  x <- gsub(" COUNTY$", "", x)
  x <- gsub(" PARISH$", "", x)
  x <- gsub(" BOROUGH$", "", x)
  x <- gsub(" CENSUS AREA$", "", x)
  x <- gsub(" CITY AND BOROUGH$", "", x)
  x <- gsub(" MUNICIPALITY$", "", x)
  dma_clean_county(x)
}

dma_make_fips <- function(state, county) {
  sprintf("%02d%03d", as.integer(state), as.integer(county))
}

dma_state_lookup <- function() {
  data.table::data.table(
    state_fips = sprintf("%02d", c(
      1, 2, 4, 5, 6, 8, 9, 10, 11, 12,
      13, 15, 16, 17, 18, 19, 20, 21, 22, 23,
      24, 25, 26, 27, 28, 29, 30, 31, 32, 33,
      34, 35, 36, 37, 38, 39, 40, 41, 42, 44,
      45, 46, 47, 48, 49, 50, 51, 53, 54, 55,
      56, 72
    )),
    STATE_AB = c(
      "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
      "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
      "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
      "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
      "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI",
      "WY", "PR"
    )
  )
}

dma_download_with_retries <- function(url,
                                      destfile,
                                      timeout = 60,
                                      retries = 2L,
                                      quiet = TRUE) {
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(as.numeric(timeout)[1], old_timeout %||% 60))
  last_error <- NULL
  for (attempt in seq_len(max(1L, as.integer(retries) + 1L))) {
    ok <- tryCatch({
      utils::download.file(url, destfile, mode = "wb", quiet = quiet)
      TRUE
    }, error = function(e) {
      last_error <<- conditionMessage(e)
      FALSE
    }, warning = function(w) {
      last_error <<- conditionMessage(w)
      FALSE
    })
    if (isTRUE(ok) && file.exists(destfile) && file.info(destfile)$size > 0) return(destfile)
    if (attempt <= retries) Sys.sleep(min(2 ^ attempt, 8))
  }
  stop("Download failed after retries. URL: ", url, if (!is.null(last_error)) paste0(" Error: ", last_error) else "")
}

dma_validate_columns <- function(dt, required_cols, label) {
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols)) stop(label, " missing columns: ", paste(missing_cols, collapse = ", "))
  invisible(TRUE)
}

pull_dma_population <- function(acs_year = 2024,
                                census_key = Sys.getenv("CENSUS_API_KEY"),
                                dma_url = "https://raw.githubusercontent.com/alex-patton/US-TVDMA-BY-COUNTY/master/usa-tvdma-county.csv",
                                cache_dir = NULL,
                                timeout = 60,
                                retries = 2L,
                                output_file = NULL,
                                verbose = FALSE) {
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Package 'data.table' is required.")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' is required to call the Census API.")
  acs_year <- as.integer(acs_year)[1]
  if (!is.finite(acs_year) || acs_year < 2009) stop("acs_year must be a valid ACS 5-year API year.")

  if (!is.null(cache_dir) && nzchar(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_file <- function(name) if (!is.null(cache_dir) && nzchar(cache_dir)) file.path(cache_dir, name) else NULL

  census_cache <- cache_file(sprintf("census_county_population_%s.rds", acs_year))
  if (!is.null(census_cache) && file.exists(census_cache)) {
    county_pop <- readRDS(census_cache)
  } else {
    query_url <- paste0(
      sprintf("https://api.census.gov/data/%s/acs/acs5", acs_year),
      "?get=NAME,B01003_001E",
      "&for=county:*",
      "&in=state:*",
      if (nzchar(census_key)) paste0("&key=", utils::URLencode(census_key, reserved = TRUE)) else ""
    )
    census_raw <- jsonlite::fromJSON(query_url)
    county_pop <- data.table::as.data.table(census_raw[-1, , drop = FALSE])
    data.table::setnames(county_pop, census_raw[1, ])
    dma_validate_columns(county_pop, c("NAME", "B01003_001E", "state", "county"), "Census response")
    county_pop[, population := suppressWarnings(as.numeric(B01003_001E))]
    county_pop[, county_fips := dma_make_fips(state, county)]
    county_pop[, state_fips := sprintf("%02d", as.integer(state))]
    county_pop <- merge(county_pop, dma_state_lookup(), by = "state_fips", all.x = TRUE)
    county_pop[, county_name := sub(",.*$", "", NAME)]
    county_pop[, county_clean := dma_clean_census_county(county_name)]
    county_pop <- county_pop[, list(county_fips, STATE_AB, county_name, county_clean, population)]
    if (!is.null(census_cache)) saveRDS(county_pop, census_cache)
  }

  dma_cache <- cache_file("county_dma_map.rds")
  if (!is.null(dma_cache) && file.exists(dma_cache)) {
    dma_map <- readRDS(dma_cache)
  } else {
    dma_tmp <- tempfile(fileext = ".csv")
    dma_download_with_retries(dma_url, dma_tmp, timeout = timeout, retries = retries, quiet = !isTRUE(verbose))
    raw_txt <- readChar(dma_tmp, file.info(dma_tmp)$size, useBytes = TRUE)
    raw_txt <- gsub("\r", "\n", raw_txt, fixed = TRUE)
    dma_map <- data.table::fread(text = raw_txt, fill = TRUE)
    data.table::setnames(dma_map, names(dma_map), trimws(names(dma_map)))
    dma_validate_columns(dma_map, c("STATE_AB", "COUNTY", "TVDMA"), "DMA map")
    dma_map[, STATE_AB := toupper(trimws(STATE_AB))]
    dma_map[, county_clean := dma_clean_county(COUNTY)]
    dma_map[, dma_name := trimws(TVDMA)]
    dma_map[, dma_name := sub("\\s+DMA$", "", dma_name, ignore.case = TRUE)]
    dma_map <- unique(dma_map[, list(STATE_AB, county_clean, dma_name)])
    if (!is.null(dma_cache)) saveRDS(dma_map, dma_cache)
  }

  county_dma_population <- merge(county_pop, dma_map, by = c("STATE_AB", "county_clean"), all.x = TRUE)
  unmatched_counties <- county_dma_population[is.na(dma_name)][order(STATE_AB, county_name)]
  dma_population <- county_dma_population[
    !is.na(dma_name),
    list(
      estimated_population = sum(population, na.rm = TRUE),
      county_count = data.table::uniqueN(county_fips),
      source_population = paste0("Census ACS ", acs_year, " 5-year B01003_001E"),
      source_dma_map = "alex-patton/US-TVDMA-BY-COUNTY",
      official_nielsen = FALSE
    ),
    by = dma_name
  ][order(-estimated_population)]

  metadata <- list(
    acs_year = acs_year,
    census_source = paste0("Census ACS ", acs_year, " 5-year B01003_001E"),
    dma_source = dma_url,
    official_nielsen = FALSE,
    warning = "DMA mapping is unofficial/open-source and is not an official Nielsen DMA population file.",
    unmatched_county_n = nrow(unmatched_counties)
  )
  if (!is.null(output_file) && nzchar(output_file)) data.table::fwrite(dma_population, output_file)
  if (isTRUE(verbose)) {
    print(utils::head(dma_population, 20))
    if (nrow(unmatched_counties)) print(unmatched_counties)
  }
  list(
    dma_population = dma_population[],
    county_dma_population = county_dma_population[],
    unmatched_counties = unmatched_counties[],
    metadata = metadata
  )
}
