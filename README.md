```yaml
---
output:
  github_document:
    toc: true
    toc_depth: 2
    number_sections: true
    fig_caption: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%"
)
```

# substackR

[![R-CMD-check](https://github.com/posocap/substackR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/posocap/substackR/actions/workflows/R-CMD-check.yaml)

A lightweight R client for the Substack API. All functions return data frames, handle errors with informative messages, and work out of the box once you’ve set your API key.

## Installation

From CRAN:

```r
install.packages("substackR")
```

Development version from GitHub:

```r
# install.packages("pak")   # if needed
pak::pak("posocap/substackR")
```

## Authentication

Provide your API key once per session with:

```r
library(substackR)
set_substack_key("your-substack-api-key")
```

If you haven’t set the key, any API call will prompt you to call `set_substack_key()` first.

## Main Functions

### get_substack_latest()

Fetches the latest posts.

```r
latest_posts <- get_substack_latest(
  publication_url = "posocap.substack.com",
  limit           = 10,
  offset          = 0
)
```

Returns a data frame with columns:

- slug
- url
- title
- description
- excerpt
- body_html
- reading_time_minutes
- audio_url
- date
- likes
- paywall
- cover_image
- cover_image_color_palette
- author
- author_image

### get_substack_top()

Fetches the top (most liked) posts.

```r
top_posts <- get_substack_top("posocap.substack.com", limit = 5)
```

### get_substack_search()

Searches posts by keyword.

```r
search_results <- get_substack_search(
  publication_url = "posocap.substack.com",
  query           = "data science",
  limit           = 20
)
```

### get_substack_post()

Retrieves a single post by slug.

```r
single_post <- get_substack_post(
  publication_url = "posocap.substack.com",
  slug            = "your-post-slug"
)
```

## Error Handling

- Missing API key → error asking you to run `set_substack_key()`.
- HTTP errors (4xx, 5xx, rate limits) → `cli::cli_abort()` with status code and message.
- JSON parsing issues → warning + empty data frame.

## Contributing

1. Fork the repo
2. Create a branch (e.g. `feature/xyz`)
3. Install dependencies:
   ```r
   devtools::install_deps(dependencies = TRUE)
   ```
4. Run tests:
   ```r
   devtools::test()
   ```
5. Submit a pull request.

## License

MIT © Posocap.com  
See `LICENSE` for details.
