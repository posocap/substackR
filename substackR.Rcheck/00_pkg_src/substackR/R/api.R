#' @keywords internal
#' @import httr2
#' @importFrom cli cli_abort
#' @importFrom rlang %||%
NULL

#' Fallback operator
#'
#' A simple “null coalesce” operator. Returns `y` if `x` is `NULL`, otherwise `x`.
#'
#' @param x Any R object.
#' @param y Any R object.
#' @return `x` if not `NULL`, otherwise `y`.
#' @noRd
`%||%` <- rlang::`%||%`

# internal env to hold your API key
.substackr_env <- new.env(parent = emptyenv())

# internal helper: safe wrapper around make_substack_df()
#' @noRd
.safe_df <- function(x) {
  tryCatch(
    make_substack_df(x),
    error = function(e) {
      cli::cli_warn(c(
        "!" = "Failed to parse API response, returning empty data.frame.",
        "i" = "Underlying error: {.val {e$message}}"
      ))
      make_substack_df(NULL)
    }
  )
}

#' Set your Substack API Key
#'
#' Store your key once per session. Subsequent calls to other
#' functions will use this key automatically.
#'
#' @param key A single string: your Substack API key.
#' @return Invisibly `TRUE` if successful.
#' @export
set_substack_key <- function(key) {
  if (!is.character(key) || length(key) != 1L) {
    cli::cli_abort("`key` must be a single string.")
  }
  .substackr_env$api_key <- key
  invisible(TRUE)
}

#' @noRd
get_substack_key <- function() {
  key <- .substackr_env$api_key %||% ""
  if (!nzchar(key)) {
    cli::cli_abort(
      "No Substack API key set. Call `set_substack_key(\"YOUR_KEY\")` first."
    )
  }
  key
}

#' @noRd
substack_api_call <- function(path, params = list()) {
  req <- httr2::request("https://api.substackapi.dev") |>
    httr2::req_url_path_append(path) |>
    httr2::req_headers(`X-API-Key` = get_substack_key()) |>
    httr2::req_url_query(!!!params)

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      cli::cli_abort("Request failed: {e$message}")
    }
  )

  status <- httr2::resp_status(resp)

  if (status == 429) {
    cli::cli_abort("Rate limit exceeded (HTTP 429 Too Many Requests).")
  }
  if (status >= 400) {
    # try to extract any short message from the body
    msg <- tryCatch(
      httr2::resp_body_json(resp, simplifyVector = TRUE)$message,
      error = function(e) {
        print(paste("Error occurred: ", e$message))
        NULL
      }
    )
    cli::cli_abort(
      "HTTP {status} error{?s}: {if (!is.null(msg)) msg else ''}"
    )
  }

  httr2::resp_body_json(resp, simplifyVector = TRUE)
}


#' @noRd
make_substack_df <- function(x) {
  cols <- c(
    "slug", "url", "title", "description", "excerpt", "body_html",
    "reading_time_minutes", "audio_url", "date", "likes", "paywall",
    "cover_image", "cover_image_color_palette", "author", "author_image"
  )
  if (is.null(x) || length(x) == 0L) {
    return(as.data.frame(
      matrix(nrow = 0L, ncol = length(cols)),
      stringsAsFactors = FALSE,
      col.names = cols
    ))
  }
  as.data.frame(x, stringsAsFactors = FALSE)
}

#' Latest Posts
#'
#' Fetch the latest posts from a Substack publication.
#'
#' @param publication_url Character, e.g. `"example.substack.com"`.
#' @param limit Integer `[1–50]`, default `10`.
#' @param offset Integer `>= 0`, default `0`.
#' @return A `data.frame` (possibly zero‐row) of post records.
#' @export
get_substack_latest <- function(publication_url,
                                limit = 10L,
                                offset = 0L) {
  params <- list(
    publication_url = publication_url,
    limit           = as.integer(limit),
    offset          = as.integer(offset)
  )
  out <- substack_api_call("/posts/latest", params)
  .safe_df(out)
}

#' Top Posts
#'
#' Fetch the most popular posts from a Substack publication.
#'
#' @inheritParams get_substack_latest
#' @return A `data.frame` (possibly zero‐row) of post records.
#' @export
get_substack_top <- function(publication_url,
                             limit = 10L,
                             offset = 0L) {
  params <- list(
    publication_url = publication_url,
    limit           = as.integer(limit),
    offset          = as.integer(offset)
  )
  out <- substack_api_call("/posts/top", params)
  .safe_df(out)
}

#' Search Posts
#'
#' Search posts in a Substack publication. If zero results, returns an
#' empty data.frame.
#'
#' @param publication_url Character, e.g. `"example.substack.com"`.
#' @param query Character. Search term.
#' @param limit Integer `[1–50]`, default `10`.
#' @param offset Integer `>= 0`, default `0`.
#' @return A `data.frame` (possibly zero‐row).
#' @export
get_substack_search <- function(publication_url,
                                query,
                                limit = 10L,
                                offset = 0L) {
  params <- list(
    publication_url = publication_url,
    query           = query,
    limit           = as.integer(limit),
    offset          = as.integer(offset)
  )
  out <- substack_api_call("/posts/search", params)
  .safe_df(out)
}

#' Get Single Post
#'
#' Fetch a single post by its slug. Returns a one‐row `data.frame`, or
#' zero‐row if not found.
#'
#' @param publication_url Character.
#' @param slug Character. Post slug.
#' @return A `data.frame` with one row (or zero rows if missing).
#' @export
get_substack_post <- function(publication_url,
                              slug) {
  params <- list(
    publication_url = publication_url,
    slug            = slug
  )
  out <- substack_api_call("/post", params)

  # If nothing returned, build empty df
  if (is.null(out) || length(out) == 0L) {
    return(make_substack_df(NULL))
  }

  # Wrap in tryCatch in case structure is unexpected
  tryCatch(
    as.data.frame(t(unlist(out)), stringsAsFactors = FALSE),
    error = function(e) {
      cli::cli_warn(c(
        "!" = "Failed to coerce single post to data.frame, returning empty.",
        "i" = "Error was {.val {e$message}}"
      ))
      make_substack_df(NULL)
    }
  )
}
