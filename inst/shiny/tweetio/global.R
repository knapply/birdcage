library(shiny)
library(tweetio)
library(purrr)




build_DT <- function(tweet_df) {

  
  dttm_cols <- which(map_lgl(tweet_df, inherits, "POSIXct"))
  
  tweet_df %>% 
    DT::datatable(
      rownames = FALSE,
      escape = FALSE,
      filter = "top",
      extensions = "Buttons",
      # selection = .selection,
      options = list(
        dom = 'Bfrtip',
        # pageLength = .page_length,
        # autoWidth = TRUE,
        # columnDefs = list(
        #   list(targets = js_index_invis_cols, visible = !has_invis_cols)
        # ),
        # scrollY = height,
        # scrollX = TRUE,
        # scrollY = TRUE,
        searchHighlight = TRUE
        # buttons = list(
        #   list(extend = "colvis",
        #        columns = js_index_all_cols,
        #        collectionLayout = "fixed four-column")
        # )
      )
    ) %>%
  DT::formatDate(columns = dttm_cols, method = "toUTCString")
}