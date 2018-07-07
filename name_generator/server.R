
names <- read.csv("baby-names.csv", stringsAsFactors = FALSE)

substrRight <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}

pickRandomRows = function(df, numberOfRows = 10) {
  df %>% slice(runif(numberOfRows,0, length(df[,1])))
}


shinyServer(function(input, output, session) {
  out.reac <- reactive({
    #scrub inputs
    name_end_lower <- input$name_end %>% tolower()
    name_start_lower <- input$name_start %>% tolower()
    
    len_start <- length(strsplit(input$name_start, "")[[1]])
    len_end <- length(strsplit(input$name_end, "")[[1]])
    
    yearmin <- input$year[1]
    yearmax <- input$year[2]
    vowelmin <- input$vowels[1]
    vowelmax <- input$vowels[2]
    consonantmin <- input$consonants[1]
    consonantmax <- input$consonants[2]
    percentmin <- input$rarity[1]
    percentmax <- input$rarity[2]
    lengthmin <- input$length[1]
    lengthmax <- input$length[2]
    
    
    list.out <- names %>%
      filter(
        year >= yearmin,
        year <= yearmax,
        vowels <= vowelmax,
        vowels >= vowelmin,
        consonants <= consonantmax,
        consonants >= consonantmin,
        percent <= percentmax,
        percent >= percentmin,
        length <= lengthmax,
        length >= lengthmin
      )
    
    
    if (input$name_end != "") {
      list.out %<>%
        filter(length >= len_end) %>%
        mutate(last = substrRight(lname, len_end)) %>%
        filter(last == name_end_lower)
    }
    
    if (input$name_start != "") {
      list.out %<>%
        filter(length >= len_start) %>%
        mutate(first = substr(lname, 1, len_start)) %>%
        filter(first == name_start_lower)
    }
    
    if (input$gender != "any") {
      list.out %<>% filter(sex == input$gender)
    }
    
    if (input$name_cont != "") {
      list.out %<>% filter(grepl(tolower(input$name_cont), lname))
    }
    
    if (input$name_letters != "") {
      list.out <-
        list.out[sapply(list.out$lname, function(x)
          all(strsplit(tolower(
            input$name_letters
          ), "")[[1]] %in% strsplit(x, "")[[1]])) %>% as.vector(), ]
    }
    
    list.out
    
  })
  
  output$table_boy <- DT::renderDataTable({
    out.reac() %>% filter(sex == "boy", name != "") %>% group_by(name) %>%
      summarise(
        permille = round(mean(percent) * 10, 4),
        vowels = unique(vowels),
        consonants = unique(consonants),
        length = unique(length),
        years_obs = n()
      )
  })
  
  output$table_girl <- DT::renderDataTable({
    out.reac() %>% filter(sex == "girl", name != "") %>% group_by(name) %>%
      summarise(
        permille = round(mean(percent) * 10, 4),
        vowels = unique(vowels),
        consonants = unique(consonants),
        length = unique(length),
        years_obs = n()
      )
  })
  
  output$plot1 <- renderPlot({
    names$lname <- tolower(names$name)
    invec <- tolower(c(
      input$name1,
      input$name2,
      input$name3,
      input$name4,
      input$name5,
      input$name6
    ))
    if (!all(invec == "") &
        any(invec[invec != ""] %in% tolower(names$name))) {
      a <- names %>%
        filter(lname %in% invec, name != "",
               sex == "boy") %>%
        ggplot(aes(year, percent * 10, color = name)) +
        geom_line() +
        ggtitle("Boys") +
        theme_classic() +
        ylab("permille")
      
      b <- names %>%
        filter(lname %in% invec,
               name != "",
               sex == "girl") %>%
        ggplot(aes(year, percent * 10, color = name)) +
        geom_line() +
        ggtitle("Girls") +
        theme_classic() +
        ylab("permille")
      
      
      if(input$log_plot) {
        a <- a + scale_y_log10()
        b <- b + scale_y_log10()
      }
      
      grid.arrange(a, b, ncol = 1)
    }
    
    if (all(invec == ""))
    {
      text(
        x = 0.5,
        y = 0.5,
        paste("Enter one or more names.\n"),
        cex = 1.6,
        col = "black"
      )
    }
    
    if (!all(invec == "") &
        !any(invec[invec != ""] %in% names$lname))
    {
      text(
        x = 0.5,
        y = 0.5,
        paste("None of the entered names were found.\n"),
        cex = 1.6,
        col = "black"
      )
    }
    
  })
  
  output$plot2 <- renderPlot({
    #scrub inputs
    
    names$lname <- tolower(names$name)
    name_end_lower <- input$name_end_plot %>% tolower()
    name_start_lower <- input$name_start_plot %>% tolower()
    
    len_start <- length(strsplit(input$name_start_plot, "")[[1]])
    len_end <- length(strsplit(input$name_end_plot, "")[[1]])
    
    list.out <- names
    
    
    if (input$name_end_plot != "") {
      list.out %<>%
        filter(length >= len_end) %>%
        mutate(last = substrRight(lname, len_end)) %>%
        filter(last == name_end_lower)
    }
    
    if (input$name_start_plot != "") {
      list.out %<>%
        filter(length >= len_start) %>%
        mutate(first = substr(lname, 1, len_start)) %>%
        filter(first == name_start_lower)
    }
    
    if (input$name_cont_plot != "") {
      list.out %<>% filter(grepl(tolower(input$name_cont_plot), lname))
    }
    
    if (input$name_letters_plot != "") {
      list.out <-
        list.out[sapply(list.out$lname, function(x)
          all(strsplit(
            tolower(input$name_letters_plot), ""
          )[[1]] %in% strsplit(x, "")[[1]])) %>% as.vector(), ]
    }
    
    if (input$sort_plot == "Top10") {
      a_names <- list.out %>%
        filter(name != "",
               sex == "boy") %>%
        group_by(name) %>%
        summarise(mean = mean(percent), length = n()) %>%
        filter(length > 50) %>%
        arrange(desc(mean)) %>%
        head(10) %>%
        pull(name)
      
      b_names <- list.out %>%
        filter(name != "",
               sex == "girl") %>%
        group_by(name) %>%
        summarise(mean = mean(percent), length = n()) %>%
        filter(length > 50) %>%
        arrange(desc(mean)) %>%
        head(10) %>%
        pull(name)
      
    }
    
    if (input$sort_plot == "Bottom10") {
      a_names <- list.out %>%
        filter(name != "",
               sex == "boy") %>%
        group_by(name) %>%
        summarise(mean = mean(percent), length = n()) %>%
        filter(length > 50) %>%
        arrange(mean) %>%
        head(10) %>%
        pull(name)
      
      
      b_names <- list.out %>%
        filter(name != "",
               sex == "girl") %>%
        group_by(name) %>%
        summarise(mean = mean(percent), length = n()) %>%
        filter(length > 50) %>%
        arrange(mean) %>%
        head(10) %>%
        pull(name)
    }
    
    if (input$sort_plot == "Random10") {
      a_names <- list.out %>%
        filter(name != "",
               sex == "boy") %>%
        group_by(name) %>%
        summarise(mean = mean(percent), length = n()) %>%
        filter(length > 50) %>%
        pickRandomRows() %>%
        pull(name)
      
      
      b_names <- list.out %>%
        filter(name != "",
               sex == "girl") %>%
        group_by(name) %>%
        summarise(mean = mean(percent), length = n()) %>%
        filter(length > 50) %>%
        pickRandomRows() %>%
        pull(name)
    }
    
    a <- list.out %>%
      filter(name != "",
             sex == "boy",
             name %in% a_names) %>%
      ggplot(aes(year, percent * 10, color = name)) +
      geom_line() +
      ggtitle("Boys") +
      theme_classic() +
      ylab("permille")
    
    b <- list.out %>%
      filter(name != "",
             sex == "girl",
             name %in% b_names) %>%
      ggplot(aes(year, percent * 10, color = name)) +
      geom_line() +
      ggtitle("Girls") +
      theme_classic() +
      ylab("permille")
    
    if(input$log_splot) {
      a <- a + scale_y_log10()
      b <- b + scale_y_log10()
    }
    
    grid.arrange(a, b, ncol = 1)
  })
  
  observeEvent(input$reset_input, {
    updateSliderInput(session, "year", value = c(1880, 2016))
    updateSliderInput(session, "vowels", value = c(0, 6))
    updateSliderInput(session, "consonants", value = c(0, 16))
    updateSliderInput(session, "rarity", value = c(.00001, .1))
    updateTextInput(session, "name_cont", value = "")
    updateTextInput(session, "name_end", value = "")
    updateTextInput(session, "name_start", value = "")
    updateTextInput(session, "name_letters", value = "")
  })
  
  observeEvent(input$reset_input_plot, {
    updateTextInput(session, "name_cont_plot", value = "")
    updateTextInput(session, "name_end_plot", value = "")
    updateTextInput(session, "name_start_plot", value = "")
    updateTextInput(session, "name_letters_plot", value = "")
  })
  
  observeEvent(input$reset_plot, {
    updateTextInput(session, "name1", value = "")
    updateTextInput(session, "name2", value = "")
    updateTextInput(session, "name3", value = "")
    updateTextInput(session, "name4", value = "")
    updateTextInput(session, "name5", value = "")
    updateTextInput(session, "name6", value = "")
  })
})
