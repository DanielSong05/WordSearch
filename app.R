library(shiny)
# Define a word reference class
Word <- setRefClass(
  "Word",
  fields = list(
    word = "character",
    definition = "character"
  ),
  methods = list(
    getWord = function() {
      return(word)
    },
    getDefinition = function (){
      return(definition)
    }
  )
)

# Create a collection of stats vocab
vocab <- list(
  Word$new(word = "MEAN", definition = "The average of a set of numbers, calculated as the sum divided by the count."),
  Word$new(word = "MEDIAN", definition = "The middle value in an ordered dataset."),
  Word$new(word = "MODE", definition = "The value that appears most frequently in a dataset."),
  Word$new(word = "VARIANCE", definition = "A measure of how much values in a dataset differ from the mean."),
  Word$new(word = "REGRESSION", definition = "A statistical method to model the relationship between variables."),
  Word$new(word = "STANDARD", definition = "Referring to standard deviation, a measure of data dispersion."),
  Word$new(word = "CORRELATION", definition = "A measure of the relationship between two variables."),
  Word$new(word = "HYPOTHESIS", definition = "A proposed explanation tested by statistical methods."),
  Word$new(word = "SIGNIFICANCE", definition = "The likelihood that a result is not due to chance."),
  Word$new(word = "DISTRIBUTION", definition = "The way values are spread in a dataset."),
  Word$new(word = "PROBABILITY", definition = "The measure of the likelihood of an event occurring."),
  Word$new(word = "STATISTICS", definition = "The science of collecting and analyzing data."),
  Word$new(word = "SAMPLE", definition = "A subset of data selected from a population for analysis."),
  Word$new(word = "POPULATION", definition = "The entire group of items or individuals being studied."),
  Word$new(word = "DEVIATION", definition = "The difference between a data point and the mean."),
  Word$new(word = "QUARTILE", definition = "Values dividing a dataset into four equal parts."),
  Word$new(word = "OUTLIER", definition = "A data point significantly different from others in a dataset."),
  Word$new(word = "SKEWNESS", definition = "A measure of the asymmetry of a distribution."),
  Word$new(word = "PVALUE", definition = "Describes how likely you are to have found a particular set of observations if the null hypothesis were true."),
  Word$new(word = "TTEST", definition = "A statistical test to compare the means of two groups."),
  Word$new(word = "ANOVA", definition = "A statistical method to compare means across multiple groups.")
)

# Define grid generation
generate_grid <- function(vocab, grid_size = 20, num_word = 10) {
  # initialization
  grid <- matrix("", nrow = grid_size, ncol = grid_size)
  selected_words <- list() # to store the words that were successfully placed into the matrix
  word_placement <- list()
  available_indices <- sample(1:length(vocab)) # shuffle all indexes
  
  # try to place 10 words, while there are more words available
  while(length(selected_words) < num_word && length(available_indices) > 0) {
    # Select the next word
    word_index <- available_indices[1] #choose the first word available
    word_obj <- vocab[[word_index]] # extract the word class object
    available_indices <- available_indices[-1] # remove used word
    word <- word_obj$getWord() # the word to place
    word_len <- nchar(word)
    placed <- FALSE
    attempts <- 0
    max_attempts <- 500
    
    # while the word is not placed & we have not reached maximum times of attempts
    while(!placed && attempts < max_attempts) {
      direction <- sample(0:1, 1) # 0 for horizontal, 1 for vertical
      
      # horizontal
      if(direction == 0) {
        # randomly select position
        row <- sample(1:grid_size,1)
        col <- sample(1:(grid_size - word_len + 1), 1)
        
        # validate position
        valid_pos <- TRUE
        for(i in 1:word_len) {
          # if the new word interfere with a placed word, and overlapping letter does not match
          if(grid[row, col + i - 1] != "" && grid[row, col + i - 1] != substr(word, i, i)) {
            valid_pos <- FALSE
            break
          }
        }
        
        # if the position is valid, place the word
        if(valid_pos) {
          for (i in 1:word_len) {
            grid[row, col + i - 1] <- substr(word, i, i)
          }
          placed <- TRUE
          word_placement <- c(word_placement, list(list(word = word, row = row, col = col, direction = "horizontal")))
        }
      } else { # vertical
        # randomly select position
        row <- sample(1:(grid_size - word_len + 1), 1)
        col <- sample(1:grid_size,1)
        
        # validate position
        valid_pos <- TRUE
        for(i in 1:word_len) {
          # if the new word interfere with a placed word, and overlapping letter does not match
          if(grid[row + i - 1, col] != "" && grid[row + i - 1, col] != substr(word, i, i)) {
            valid_pos <- FALSE
            break
          }
        }
        
        # if the position is valid, place the word
        if(valid_pos) {
          for (i in 1:word_len) {
            grid[row + i - 1, col] <- substr(word, i, i)
          }
          placed <- TRUE
          word_placement <- c(word_placement, list(list(word = word, row = row, col = col, direction = "vertical")))
        }
      }
      attempts <- attempts + 1
    }
    
    if(placed) {
      selected_words <- c(selected_words, list(word_obj))
    } else {
      cat("Unable to place word", word, "\n")
    }
  }
  
  # fill unused grid with random letters
  grid[grid == ""] <- sample(LETTERS[1:26], sum(grid == ""), replace = TRUE)
  
  return(list(grid = grid, selected_words = selected_words, word_placement = word_placement))
}

# grid generation test
#grid <- generate_grid(vocab, grid_size = 20, num_word = 10)
#grid[["grid"]]
#grid[["selected_words"]]

ui <- fluidPage(
  titlePanel("How many stats words can you find?"),
  sidebarLayout(
    sidebarPanel(
      actionButton("newGameBtn", "New Game"),
      tags$hr(),
      textInput("userInputWord", "Enter found word:"),
      fluidRow(
        column(6, textInput("userInputRow", "Enter word row")),
        column(6, textInput("userInputCol", "Enter word col"))
      ),
      selectInput("userInputDirection", "Choose word direction", choices = c("Horizontal", "Vertical")),
      actionButton("submitBtn", "Submit Word"),
      tags$hr(),
      actionButton("hintBtn", "Get Hint"),
      br(),
      br(),
      textOutput("hintDefinition"),
      tags$hr(),
      h4("Found Words"),
      textOutput("foundWords"),
      tags$hr(),
      h4("Score"),
      textOutput("scoreDisplay")
    ),
    mainPanel(
      h4("Word Search Grid"),
      uiOutput("puzzleGrid"),
    )
  ),
  div(id = "footer", "This is a project by STATS 20 S25 Group 9: Eleanore Zhu, Gabriella Aufdermaur, Yinbo Zhang, Daniel Song, Yubei Li")
)

server <- function(input, output, session) {
  puzzle_trigger <- reactiveVal(0)
  
  # Redefine puzzle to depend on trigger
  puzzle <- reactive({
    puzzle_trigger() # Depend on trigger
    generate_grid(vocab, grid_size = 20, num_word = 10)
  })
  # Render the puzzle
  output$puzzleGrid <- renderUI({
    grid <- puzzle()$grid
    grid_size <- nrow(grid)
    found_word_list <- found_words()
    placements <- puzzle()$word_placement
    is_cell_found <- function(r, c, found_word_list, placements){
      for(w in placements){
        if(w$word %in% found_word_list){
          len <- nchar(w$word)
          if(w$direction == "horizontal"){
            if(r == w$row && c >= w$col && c< w$col + len){
              return(TRUE)
            }
          } else if(w$direction == "vertical"){
            if(c == w$col && r >= w$row && r < w$row + len){
              return(TRUE)
            }
          }
        }
      }
      return(FALSE)
    }
    tags$table(
      
      # 1) Column headers
      tags$tr(
        tags$th("", style="width:30px; border:none;"), # so that label doesn't start at corner
        # column header
        lapply(seq_len(grid_size), function(c) {
          tags$th(c, style = "text-align:center;")
        })
      ),
      
      # 2) Data rows
      lapply(seq_len(grid_size), function(r) {
        tags$tr(
          # row header
          tags$th(r),
          # the letter cells
          lapply(seq_len(grid_size), function(c) {
            tags$td(
              grid[r, c],
              style = paste(
                "width:25px; height:25px; text-align:center; ",
                "border:1px solid #999; ",
                if (is_cell_found(r, c, found_word_list, placements)){
                  "background-color: yellow;" 
                }else {
                  ""
                }
              )
            )
          })
        )
      })
    )
  })
  
  # Store found words
  found_words <- reactiveVal(character(0))
  
  # Handles score & submitted answer
  score <- reactiveVal(0)
  output$scoreDisplay <- renderText({
    paste(score())
  })
  
  observeEvent(input$submitBtn, {
    word_input <- toupper(trimws(input$userInputWord))
    row_input <- as.numeric(input$userInputRow)
    col_input <- as.numeric(input$userInputCol)
    dir_input <- tolower(input$userInputDirection)
    
    match_found <- FALSE
    
    for (w in puzzle()$word_placement) {
      if (word_input == w$word &&
          row_input == w$row &&
          col_input == w$col &&
          dir_input == w$direction) {
        
        if (!(word_input %in% found_words())) {
          found_words(c(found_words(), word_input))
          score(score() + 10)  # b Add points only if word wasn't already found
          showNotification("Correct!", type = "message")
        } else {
          showNotification("Already found!", type = "warning")
        }
        match_found <- TRUE
        break
      }
    }
    
    if (!match_found) {
      showNotification("Incorrect guess!", type = "error")
    }
  })
  
  # Handle hint
  hint_text <- reactiveVal("Click 'Get Hint' to see a hint")
  output$hintDefinition <- renderText({
    hint_text()
  })
  
  observeEvent(input$hintBtn, {
    unfound_words <- puzzle()$selected_words[!sapply(puzzle()$selected_words, function(w) w$getWord() %in% found_words())]
    if(length(unfound_words) > 0) {
      random_hint <- sample(unfound_words, 1)[[1]]
      hint <- paste("Hint:", random_hint$getDefinition())
      cat("Hint:", random_hint$getDefinition())
      cat("Hint generated!\n")
      hint_text(hint)
      score(max(0, score() - 5))
    } else { # if all words are found
      paste0("You have found all words! Congratulations!") 
    }
  })
  
  # Found words
  output$foundWords <- renderText({
    paste(found_words(), collapse = ", ")
  })
  
  observe({
    found_words <- found_words()
    all_words <- unlist(lapply(puzzle()$selected_words, function(w) w$getWord()))
    if (length(found_words) == length(all_words) && length(all_words) > 0) {
      showNotification("You found all the words!", type = "message")
    }
  })
  
  observeEvent(input$newGameBtn, {
    puzzle_trigger(puzzle_trigger() + 1) # Increment to trigger new puzzle
    found_words(character(0)) # Reset found words
    score(0)
    hint_text("Click 'Get Hint' to see a hint") # Reset hint
  })
}

shinyApp(ui = ui, server = server)