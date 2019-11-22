server <- shinyServer(function(input, output) {
  options(shiny.maxRequestSize = 300 * 1024^2)
  
  DATA <- reactiveValues()
  
  
  observeEvent(input$file1$datapath, {
    withCallingHandlers({
      shinyjs::html("text", "")
      
      message("Parsing Tweets...")
      DATA$tweet_df <<- tweetio::read_tweets(input$file1$datapath)
      
      message("Finished!")
    },
    message = function(m) shinyjs::html(id = "text", html = m$message, add = TRUE)
    )})
  

  output$tweet_DT <- DT::renderDT({
    req(DATA$tweet_df)
    
    build_DT(DATA$tweet_df)
  })
  
  test_url <- "https://publish.twitter.com/oembed?url=https://twitter.com/knapply_/status/1196304860653637632&omit_script=1&dnt=1&theme=dark&hide_media=2"
  bq <- httr::GET(test_url)
  res <- httr::content(bq, "parsed")$html
  
  output$bq <- renderText({
    req(res)
    res
  })
  
  # observeEvent(input$done, {
  #   req(out)
  #   stopApp(returnValue = out)
  # })
  
})


# httr::content('{"url":"https://twitter.com/knapply_/status/1196304860653637632","author_name":"Brendan Knapp","author_url":"https://twitter.com/knapply_","html":"u003Cblockquote class="twitter-tweet" data-dnt="true"u003Eu003Cp lang="en" dir="ltr"u003EI guess u003Ca href="https://twitter.com/hashtag/rstats?src=hash&amp;ref_src=twsrc%5Etfw"u003E#rstatsu003C/au003E can be pretty fast.u003Ca href="https://t.co/xIPqU1xLWv"u003Ehttps://t.co/xIPqU1xLWvu003C/au003E u003Ca href="https://t.co/8A3zsmSBOZ"u003Epic.twitter.com/8A3zsmSBOZu003C/au003Eu003C/pu003E&mdash; Brendan Knapp (@knapply_) u003Ca href="https://twitter.com/knapply_/status/1196304860653637632?ref_src=twsrc%5Etfw"u003ENovember 18, 2019u003C/au003Eu003C/blockquoteu003En","width":550,"height":null,"type":"rich","cache_age":"3153600000","provider_name":"Twitter","provider_url":"https://twitter.com","version":"1.0"}')
