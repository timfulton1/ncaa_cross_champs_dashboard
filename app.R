# Source the helper_function script
source("helper_functions.R")


# Define UI ----
ui <- page_navbar(
  tags$head(includeHTML("google_analytics.html")),
  title = "NCAA Division I Cross Country: Modern Era Results",
  bg = "#DCE3D9",
  nav_spacer(),
  tabPanel(
    "Men",
    ## Nav Panel Team Men ----
    navset_card_underline(
      nav_panel(
        fill = FALSE,
        card_header("Men's Teams"),
        layout_columns(
          fill = FALSE,
          layout_column_wrap(
            width = 1,
            div(selectInput(
              inputId = "team_selection", 
              label = NULL,
              width = "200px",
              choices = c("Teams", teams),
              selected = ""),
              tags$style(type='text/css', ".selectize-dropdown-content {max-height: 400px;}")
            ),
            actionButton("show_table", "All Men's Team Data", icon = icon("table"), width = "200px")
          ),
          value_box(
            title = "Championships",
            value = textOutput("team_champs_count"),
            theme = value_box_theme(bg = "#FFB500"),
            max_height = "170px",
            showcase = icon("trophy", lib = "font-awesome"),
            showcase_layout = "top right",
            p(textOutput("team_champs_rank"))
          ),
          value_box(
            title = "Podium Finishes",
            value = textOutput("team_podiums_count"),
            theme = value_box_theme(bg = "#6095A4", fg = "black"),
            max_height = "170px",
            showcase = bs_icon("award-fill"),
            showcase_layout = "top right",
            p(textOutput("team_podiums_rank"))
          ),
          value_box(
            title = "Top 10 Finishes",
            value = textOutput("team_tens_count"),
            theme = value_box_theme(bg = "#D1DBBD"),
            max_height = "170px",
            showcase = icon("bookmark", lib = "font-awesome"),
            showcase_layout = "top right",
            p(textOutput("team_tens_rank"))
          ),
          value_box(
            title = "Individual Winners",
            value = textOutput("ind_wins_count"),
            max_height = "170px",
            theme = value_box_theme(bg = "#969CA3", fg = "black"),
            showcase = icon("person-running", lib = "font-awesome"),
            showcase_layout = "top right",
            p(textOutput("ind_wins_rank"))
          )
        ),
        card(
          fill = FALSE,
          min_height = 300,
          max_height = 360,
          plotOutput("team_plot")
        )
      ),
      ## Nav Panel Individual Men ----
      nav_panel(
        card_header("Men's Individuals"),
        div(selectInput(
          inputId = "ind_selection", 
          label = "Type a name to search",
          choices = c("", athletes),
          multiple = FALSE,
          selected = "",
          selectize = TRUE)
        ),
        layout_columns(
          col_widths = c(8, 4),
          card(
            fill = FALSE,
            layout_columns(
              value_box(
                title = "Wins",
                value = textOutput("ind_winners_count"),
                theme = value_box_theme(bg = "#FFB500"),
                showcase = icon("trophy", lib = "font-awesome"),
              ),
              value_box(
                title = "Top 10 Finishes",
                value = textOutput("ind_top10_finish"),
                theme = value_box_theme(bg = "#969CA3", fg = "black"),
                showcase = icon("award", lib = "font-awesome"),
              )
            ),
            layout_columns(
              value_box(
                title = "Fastest Time",
                value = textOutput("fastest_time"),
                theme = value_box_theme(bg = "#D1DBBD", fg = "black"),
                showcase = icon("stopwatch", lib = "font-awesome")
              ),
              value_box(
                title = "Highest Finish",
                value = textOutput("highest_finish"),
                theme = value_box_theme(bg = "#6095A4", fg = "black"), 
                showcase = icon("person-running", lib = "font-awesome")
              )
            )
          ),
          card(
            fill = FALSE,
            tableOutput("ind_results_table")
          )
        )
      ),
      ## Nav Panel Yearly Men ----
      nav_panel(
        card_header("Men's Yearly"),
        layout_columns(
          col_widths = c(3, 4, 4),
          fill = FALSE,
          div(selectInput(
            inputId = "year_selection", 
            label = "Select a year",
            choices = c("Years", years),
            multiple = FALSE,
            selected = "")
          ),
          value_box(
            title = "Individual Title Winning Margin",
            value = textOutput("ind_winning_margin"),
            theme = value_box_theme(bg = "#D1DBBD", fg = "black"),
            max_height = "100px",
            fill = FALSE
          ),
          value_box(
            title = "Team Title Winning Margin",
            value = textOutput("team_winning_margin"),
            theme = value_box_theme(bg = "#6095A4", fg = "black"),
            max_height = "100px",
            fill = FALSE
          )
        ),
        layout_columns(
          col_widths = c(7, 5),
          card(
            fill = FALSE,
            tableOutput("ind_results_table_year")
          ),
          card(
            fill = FALSE,
            tableOutput("team_results_table_year")
          )
        )
      ),
      ## Nav Panel Overall Men ----
      nav_panel(
        card_header("Men's Stats"),
        layout_columns(
          fill = FALSE,
          col_widths = c(6, 6),
          card(
            card_header("Individual Stats"),
            layout_columns(
              col_widths = c(4, 4, 4),
              fill = FALSE,
              value_box(
                title = "Average Winning Time",
                value = textOutput("overall_ind_time_win"),
                theme = value_box_theme(bg = "#D1DBBD", fg = "black"),
                fill = FALSE,
              ),
              value_box(
                title = "Fastest Winning Time",
                value = textOutput("overall_ind_fastest_win_time"),
                theme = value_box_theme(bg = "#D1DBBD", fg = "black"),
                fill = FALSE,
                p("Henry Rono, 1976"),
              ),
              value_box(
                title = "Slowest Winning Time",
                value = textOutput("overall_ind_slowest_win_time"),
                theme = value_box_theme(bg = "#D1DBBD", fg = "black"),
                fill = FALSE,
                p("John Rohatinsky, 2006"),
              )
            ), 
            layout_columns(
              col_widths = c(4, 4, 4),
              value_box(
                title = "Average 2nd Place Time",
                value = textOutput("overall_ind_time_top2"),
                theme = value_box_theme(bg = "#969CA3", fg = "black"),
                fill = FALSE
              ),
              value_box(
                title = "Average 5th Place Time",
                value = textOutput("overall_ind_time_top5"),
                theme = value_box_theme(bg = "#969CA3", fg = "black"),
                fill = FALSE
              ),
              value_box(
                title = "Average 10th Place Time",
                value = textOutput("overall_ind_time_top10"),
                theme = value_box_theme(bg = "#969CA3", fg = "black"),
                fill = FALSE
              )
            ),
            layout_columns(
              col_widths = c(4, 4, 4),
              fill = FALSE,
              value_box(
                title = "Average Winning Margin",
                value = textOutput("overall_ind_margin_avg"),
                theme = value_box_theme(bg = "#6095A4", fg = "black"),
                fill = FALSE,
              ),
              value_box(
                title = "Smallest Winning Margin",
                value = textOutput("overall_ind_margin_min"),
                theme = value_box_theme(bg = "#6095A4", fg = "black"),
                fill = FALSE,
                p("2018")
              ),
              value_box(
                title = "Largest Winning Margin",
                value = textOutput("overall_ind_margin_max"),
                theme = value_box_theme(bg = "#6095A4", fg = "black"),
                fill = FALSE,
                p("1995")
              )
            )
          ),
          card(
            card_header("Team Stats"),
            layout_columns(
              col_widths = c(4, 4, 4),
              fill = FALSE,
              value_box(
                title = "Average Winning Score",
                value = textOutput("overall_team_points_win"),
                theme = value_box_theme(bg = "#D1DBBD", fg = "black"),
                fill = FALSE
              ),
              value_box(
                title = "Lowest Winning Score",
                value = textOutput("overall_team_lowest_points"),
                theme = value_box_theme(bg = "#D1DBBD", fg = "black"),
                fill = FALSE,
                p("UTEP, 1981")
              ),
              value_box(
                title = "Highest Winning Score",
                value = textOutput("overall_team_highest_points"),
                theme = value_box_theme(bg = "#D1DBBD", fg = "black"),
                fill = FALSE,
                p("Colorado, 2013")
              )
            ), 
            layout_columns(
              col_widths = c(4, 4, 4),
              value_box(
                title = "Average 2nd Place Score",
                value = textOutput("overall_team_points_top2"),
                theme = value_box_theme(bg = "#969CA3", fg = "black"),
                fill = FALSE
              ),
              value_box(
                title = "Average Podium Score",
                value = textOutput("overall_team_points_top4"),
                theme = value_box_theme(bg = "#969CA3", fg = "black"),
                fill = FALSE
              ),
              value_box(
                title = "Average 10th Place Score",
                value = textOutput("overall_team_points_top10"),
                theme = value_box_theme(bg = "#969CA3", fg = "black"),
                fill = FALSE
              )
            ),
            layout_columns(
              col_widths = c(4, 4, 4),
              fill = FALSE,
              value_box(
                title = "Average Winning Margin",
                value = textOutput("overall_team_margin_avg"),
                theme = value_box_theme(bg = "#6095A4", fg = "black"),
                fill = FALSE,
              ),
              value_box(
                title = "Smallest Winning Margin",
                value = textOutput("overall_team_margin_min"),
                theme = value_box_theme(bg = "#6095A4", fg = "black"),
                fill = FALSE,
                p("2022 (winner was based on better 6th place finisher)")
              ),
              value_box(
                title = "Largest Winning Margin",
                value = textOutput("overall_team_margin_max"),
                theme = value_box_theme(bg = "#6095A4", fg = "black"),
                fill = FALSE,
                p("2003")
              )
            )
          )
        )
      )
    )
  ),
  ## Nav Panel Team Women ----
  nav_panel(
    "Women",
    navset_card_underline(
      nav_panel(
        card_header("Women's Teams"),
        layout_columns(
          fill = FALSE,
          layout_column_wrap(
            width = 1,
            div(selectInput(
              inputId = "team_selection_women", 
              label = NULL,
              width = "200px",
              choices = c("Teams", teams_women),
              selected = ""),
              tags$style(type='text/css', ".selectize-dropdown-content {max-height: 400px; }")
            ),
            actionButton("show_table_women", "All Women's Team Data", icon = icon("table"),width = "200px")
          ),
        value_box(
          title = "Championships",
          value = textOutput("team_champs_count_women"),
          theme = value_box_theme(bg = "#FFB500"),
          max_height = "170px",
          showcase = icon("trophy", lib = "font-awesome"),
          showcase_layout = "top right",
          p(textOutput("team_champs_rank_women"))
        ),
        value_box(
          title = "Podium Finishes",
          value = textOutput("team_podiums_count_women"),
          theme = value_box_theme(bg = "#6095A4", fg = "black"),
          max_height = "170px",
          showcase = bs_icon("award-fill"),
          showcase_layout = "top right",
          p(textOutput("team_podiums_rank_women"))
        ),
        value_box(
          title = "Top 10 Finishes",
          value = textOutput("team_tens_count_women"),
          theme = value_box_theme(bg = "#D1DBBD"),
          max_height = "170px",
          showcase = icon("bookmark", lib = "font-awesome"),
          showcase_layout = "top right",
          p(textOutput("team_tens_rank_women"))
        ),
        value_box(
          title = "Individual Winners",
          value = textOutput("ind_wins_count_women"),
          theme = value_box_theme(bg = "#969CA3", fg = "black"),
          max_height = "170px",
          showcase = icon("person-running", lib = "font-awesome"),
          showcase_layout = "top right",
          p(textOutput("ind_wins_rank_women"))
        )
      ),
      card(
        fill = FALSE,
        min_height = 300,
        max_height = 360,
        plotOutput("team_plot_women")
      )
    ),
    ## Nav Panel Individual Women ----
    nav_panel(
      card_header("Women's Individuals"),
      div(selectInput(
        inputId = "ind_selection_women", 
        label = "Type a name to search",
        choices = c("", athletes_women),
        multiple = FALSE,
        selected = "",
        selectize = TRUE)
      ),
      layout_columns(
        col_widths = c(8, 4),
        card(
          fill = FALSE,
          layout_columns(
            value_box(
              title = "Wins",
              value = textOutput("ind_winners_count_women"),
              theme = value_box_theme(bg = "#FFB500"),
              showcase = icon("trophy", lib = "font-awesome"),
            ),
            value_box(
              title = "Top 10 Finishes",
              value = textOutput("ind_top10_finish_women"),
              theme = value_box_theme(bg = "#969CA3", fg = "black"),
              showcase = icon("award", lib = "font-awesome"),
            )
          ),
          layout_columns(
            value_box(
              title = "Fastest Time",
              value = textOutput("fastest_time_women"),
              theme = value_box_theme(bg = "#D1DBBD", fg = "black"),
              showcase = icon("stopwatch", lib = "font-awesome")
            ),
            value_box(
              title = "Highest Finish",
              value = textOutput("highest_finish_women"),
              theme = value_box_theme(bg = "#6095A4", fg = "black"), 
              showcase = icon("person-running", lib = "font-awesome")
            )
          )
        ),
        card(
          fill = FALSE,
          tableOutput("ind_results_table_women")
        )
      )
    ),
    ## Nav Panel Yearly Women ----
    nav_panel(
      card_header("Women's Yearly"),
      layout_columns(
        col_widths = c(3, 4, 4),
        fill = FALSE,
        div(selectInput(
          inputId = "year_selection_women", 
          label = "Select a year",
          choices = c("Years", years_women),
          multiple = FALSE,
          selected = "")
        ),
        value_box(
          title = "Individual Title Winning Margin",
          value = textOutput("ind_winning_margin_women"),
          theme = value_box_theme(bg = "#D1DBBD", fg = "black"),
          max_height = "100px",
          fill = FALSE
        ),
        value_box(
          title = "Team Title Winning Margin",
          value = textOutput("team_winning_margin_women"),
          theme = value_box_theme(bg = "#6095A4", fg = "black"),
          max_height = "100px",
          fill = FALSE
        )
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(
          fill = FALSE,
          tableOutput("ind_results_table_year_women")
        ),
        card(
          fill = FALSE,
          tableOutput("team_results_table_year_women")
        )
      )
    ),
    ## Nav Panel Overall Women ----
    nav_panel(
      card_header("Women's Stats"),
      layout_columns(
        fill = FALSE,
        col_widths = c(6, 6),
        card(
          card_header("Individual Stats"),
          layout_columns(
            col_widths = c(4, 4, 4),
            fill = FALSE,
            value_box(
              title = "Average Winning Time",
              value = textOutput("overall_ind_time_win_women"),
              theme = value_box_theme(bg = "#D1DBBD", fg = "black"),
              fill = FALSE,
            ),
            value_box(
              title = "Fastest Winning Time",
              value = textOutput("overall_ind_fastest_win_time_women"),
              theme = value_box_theme(bg = "#D1DBBD", fg = "black"),
              fill = FALSE,
              p("Vicki Huber, 1989"),
            ),
            value_box(
              title = "Slowest Winning Time",
              value = textOutput("overall_ind_slowest_win_time_women"),
              theme = value_box_theme(bg = "#D1DBBD", fg = "black"),
              fill = FALSE,
              p("Kara Wheeler, 2000"),
            )
          ), 
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(
              title = "Average 2nd Place Time",
              value = textOutput("overall_ind_time_top2_women"),
              theme = value_box_theme(bg = "#969CA3", fg = "black"),
              fill = FALSE
            ),
            value_box(
              title = "Average 5th Place Time",
              value = textOutput("overall_ind_time_top5_women"),
              theme = value_box_theme(bg = "#969CA3", fg = "black"),
              fill = FALSE
            ),
            value_box(
              title = "Average 10th Place Time",
              value = textOutput("overall_ind_time_top10_women"),
              theme = value_box_theme(bg = "#969CA3", fg = "black"),
              fill = FALSE
            )
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            fill = FALSE,
            value_box(
              title = "Average Winning Margin",
              value = textOutput("overall_ind_margin_avg_women"),
              theme = value_box_theme(bg = "#6095A4", fg = "black"),
              fill = FALSE,
            ),
            value_box(
              title = "Smallest Winning Margin",
              value = textOutput("overall_ind_margin_min_women"),
              theme = value_box_theme(bg = "#6095A4", fg = "black"),
              fill = FALSE,
              p("2018")
            ),
            value_box(
              title = "Largest Winning Margin",
              value = textOutput("overall_ind_margin_max_women"),
              theme = value_box_theme(bg = "#6095A4", fg = "black"),
              fill = FALSE,
              p("1995")
            )
          )
        ),
        card(
          card_header("Team Stats"),
          layout_columns(
            col_widths = c(4, 4, 4),
            fill = FALSE,
            value_box(
              title = "Average Winning Score",
              value = textOutput("overall_team_points_win_women"),
              theme = value_box_theme(bg = "#D1DBBD", fg = "black"),
              fill = FALSE
            ),
            value_box(
              title = "Lowest Winning Score",
              value = textOutput("overall_team_lowest_points_women"),
              theme = value_box_theme(bg = "#D1DBBD", fg = "black"),
              fill = FALSE,
              p("Virginia, 1981")
            ),
            value_box(
              title = "Highest Winning Score",
              value = textOutput("overall_team_highest_points_women"),
              theme = value_box_theme(bg = "#D1DBBD", fg = "black"),
              fill = FALSE,
              p("Stanford, 2006")
            )
          ), 
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(
              title = "Average 2nd Place Score",
              value = textOutput("overall_team_points_top2_women"),
              theme = value_box_theme(bg = "#969CA3", fg = "black"),
              fill = FALSE
            ),
            value_box(
              title = "Average Podium Score",
              value = textOutput("overall_team_points_top4_women"),
              theme = value_box_theme(bg = "#969CA3", fg = "black"),
              fill = FALSE
            ),
            value_box(
              title = "Average 10th Place Score",
              value = textOutput("overall_team_points_top10_women"),
              theme = value_box_theme(bg = "#969CA3", fg = "black"),
              fill = FALSE
            )
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            fill = FALSE,
            value_box(
              title = "Average Winning Margin",
              value = textOutput("overall_team_margin_avg_women"),
              theme = value_box_theme(bg = "#6095A4", fg = "black"),
              fill = FALSE,
            ),
            value_box(
              title = "Smallest Winning Margin",
              value = textOutput("overall_team_margin_min_women"),
              theme = value_box_theme(bg = "#6095A4", fg = "black"),
              fill = FALSE,
              p("2016 & 2023")
            ),
            value_box(
              title = "Largest Winning Margin",
              value = textOutput("overall_team_margin_max_women"),
              theme = value_box_theme(bg = "#6095A4", fg = "black"),
              fill = FALSE,
              p("1990")
            )
          )
        )
      )
    )
  )
),
## Nav Panel Other Info ----
nav_panel(
  card_header(bs_icon("info-circle")),
  max_height = "300px",
  div(
    style = "font-size:16px; line-height: 1.5;", 
    HTML(
      "<p>Thank you for viewing this dashboard. If you have suggestions for other interesting data/stats, or notice any errors, please email <b>ncaa.cross.dashboard@gmail.com</b>.</p>
           <br>
           <p><b>Men's data notes:</b>
           The 'modern era' was defined as 1965 and later because the men's race distance changed from 4 miles to 6 miles in 1965.
           The men's race time statistics shown on the 'Other Stats' page only inlcude the years of 1976 and later because the distance was changed to 10 kilometers in 1976.</p>
          <br>
          <p><b>Women's data notes:</b> 
           Data for the women start in 1981, as that was the first year there was a championship. The race distance was 5 kilometers from 1981 to 1999,
           and changed to 6 kilometers in 2000. The women's race time statistics shown on the 'Other Stats' page only include the years of 2000 and later (6K distance).</p>
          <br>
          <b>Other notes:</b>
          <li> For both men and women, only the top 10 teams and individuals are included as the data for places beyond that is not readily availble for years prior to 2000. 
          Please email if you have access to full data sets dating back to 1981 for women and 1965 for men.</li>
          <li> Podium finishes in NCAA Cross Country include the top 4 teams.</li>
          <br>
          <p>Last updated May 2024</p>"
    )
  )  
)
)
## End Nav Card ----



# Define Server  ----
server <- function(input, output) {
  
  ## Render the plot ----
  output$team_plot <- renderPlot({
    
    # Ask user to select the inputs and don't display a plot without the inputs
    validate(
      need(input$team_selection != "Teams", "Select a team to see results"),
    )
    
    # Create the plot
    selected_team_plot(input$team_selection)   
    
  })
  
  # Create the data table of team results
  output$men_team_table <- render_gt({
    
    men_team_table <- team_ranks %>% 
      select(team, top1_count, top4_count, top10_count, ind_winner_count) %>% 
      rename("Team" = "team",
             "Championships" = "top1_count",
             "Podiums" = "top4_count",
             "Top 10s" = "top10_count",
             "Individual Winners" = "ind_winner_count") %>% 
      gt() %>% 
      opt_interactive()
    
    men_team_table
    
  })
  
  # Show the modal dialog when the table button is clicked
  observeEvent(input$show_table, {
    showModal(
      modalDialog(
        title = "Men's Team Data",
        gt_output("men_team_table"),  
        easyClose = FALSE,
        footer = tagList(modalButton("Close")),
        size = "xl"
      )
    )
  })
  
  ## Render text for Team Top 1, 4, and 10 counts ----
  output$team_champs_count <- renderText(selected_team_ranks(input$team_selection)[[1]])
  
  output$team_champs_rank <- renderText(glue("Rank: {selected_team_ranks(input$team_selection)[[4]]}"))
  
  output$team_podiums_count <- renderText(selected_team_ranks(input$team_selection)[[2]])
  
  output$team_podiums_rank <- renderText(glue("Rank: {selected_team_ranks(input$team_selection)[[5]]}"))
  
  output$team_tens_count <- renderText(selected_team_ranks(input$team_selection)[[3]])
  
  output$team_tens_rank <- renderText(glue("Rank: {selected_team_ranks(input$team_selection)[[6]]}"))
  
  output$ind_wins_count <- renderText(selected_team_ranks(input$team_selection)[[7]])
  
  output$ind_wins_rank <- renderText(glue("Rank: {selected_team_ranks(input$team_selection)[[8]]}"))
  
  ## Render text for Individual wins, fastest time, and highest finish
  output$ind_winners_count <- renderText(selected_ind_rank_df(input$ind_selection)[[1]])
  
  output$ind_top10_finish <- renderText(selected_ind_rank_df(input$ind_selection)[[2]])
  
  output$fastest_time <- renderText(selected_ind_rank_df(input$ind_selection)[[3]])
  
  output$highest_finish <- renderText(selected_ind_rank_df(input$ind_selection)[[4]])
  
  ## Render table for Individual results
  output$ind_results_table <- renderTable(
    selected_ind_results_df(input$ind_selection),
    hover = TRUE,
    align = c("ccll"),
  )
  
  ## Render table for Ind results by Year
  output$ind_results_table_year <- renderTable(
    selected_year_ind_results(input$year_selection),
    hover = TRUE,
    striped = TRUE,
    align = c("clll"),
  )
  
  ## Render table for Team results by Year
  output$team_results_table_year <- renderTable(
    selected_year_team_results(input$year_selection),
    hover = TRUE,
    striped = TRUE,
    align = c("clc"),
  )
  
  ## Render text for Yearly tab metrics ----
  output$team_winning_margin <- renderText({
    
    pt_margin <- selected_year_team_win_margin(input$year_selection)
    pt_label <- ifelse(pt_margin == 1, "point", "points")
    
    ifelse(input$year_selection == "Years", "", glue("{selected_year_team_win_margin(input$year_selection)} {pt_label}"))
  })
  
  output$ind_winning_margin <- renderText({
    
    ind_margin <- round(selected_year_ind_win_margin(input$year_selection), 1)
    
    sec_label <- ifelse(ind_margin == 1, "second", "seconds")
    
    ifelse(input$year_selection == "Years", "", glue("{ind_margin} {sec_label}"))
    
  })
  
  ## Render text for Overall Individual Metrics  ----
  output$overall_ind_margin_min <- renderText(glue("{overall_ind_metrics_df[[1,2]]} seconds"))
  
  output$overall_ind_margin_max <- renderText(glue("{overall_ind_metrics_df[[2,2]]} seconds"))
  
  output$overall_ind_margin_avg <- renderText(glue("{overall_ind_metrics_df[[3,2]]} seconds\n"))
  
  output$overall_ind_time_win <- renderText(overall_ind_metrics_df[[4,2]])
  
  output$overall_ind_time_top5 <- renderText(overall_ind_metrics_df[[5,2]])
  
  output$overall_ind_time_top10 <- renderText(overall_ind_metrics_df[[6,2]])
  
  output$overall_ind_fastest_win_time <- renderText(overall_ind_metrics_df[[7,2]])
  
  output$overall_ind_slowest_win_time <- renderText(overall_ind_metrics_df[[8,2]])
  
  output$overall_ind_time_top2 <- renderText(overall_ind_metrics_df[[9,2]])
  
  ## Render text for Overall Team Metrics ----
  output$overall_team_margin_min <- renderText(overall_team_metrics_df[[1,2]])
  
  output$overall_team_margin_max <- renderText(overall_team_metrics_df[[2,2]])
  
  output$overall_team_margin_avg <- renderText(overall_team_metrics_df[[3,2]])
  
  output$overall_team_points_win <- renderText(overall_team_metrics_df[[4,2]])
  
  output$overall_team_points_top4 <- renderText(overall_team_metrics_df[[5,2]])
  
  output$overall_team_points_top10 <- renderText(overall_team_metrics_df[[6,2]])
  
  output$overall_team_lowest_points <- renderText(overall_team_metrics_df[[7,2]])
  
  output$overall_team_highest_points <- renderText(overall_team_metrics_df[[8,2]])
  
  output$overall_team_points_top2 <- renderText(overall_team_metrics_df[[9,2]])
  
  
  ### WOMEN ###
  ## Render the plot ----
  output$team_plot_women <- renderPlot({
    
    # Ask user to select the inputs and don't display a plot without the inputs
    validate(
      need(input$team_selection_women != "Teams", "Select a team to see results"),
    )
    
    # Create the plot
    selected_team_plot_women(input$team_selection_women)   
    
  })
  
  # Create the data table of team results
  output$women_team_table <- render_gt({
    
    women_team_table <- team_ranks_women %>% 
      select(team, top1_count, top4_count, top10_count, ind_winner_count) %>% 
      rename("Team" = "team",
             "Championships" = "top1_count",
             "Podiums" = "top4_count",
             "Top 10s" = "top10_count",
             "Individual Winners" = "ind_winner_count") %>% 
      gt() %>% 
      opt_interactive()
    
    women_team_table
    
  })
  
  # Show the modal dialog when the table button is clicked
  observeEvent(input$show_table_women, {
    showModal(
      modalDialog(
        title = "Women's Team Data",
        gt_output("women_team_table"),  
        easyClose = FALSE,
        footer = tagList(modalButton("Close")),
        size = "xl"
      )
    )
  })
  
  ## Render text for Team Top 1, 4, and 10 counts ----
  output$team_champs_count_women <- renderText(selected_team_ranks_women(input$team_selection_women)[[1]])
  
  output$team_champs_rank_women <- renderText(glue("Rank: {selected_team_ranks_women(input$team_selection_women)[[4]]}"))
  
  output$team_podiums_count_women <- renderText(selected_team_ranks_women(input$team_selection_women)[[2]])
  
  output$team_podiums_rank_women <- renderText(glue("Rank: {selected_team_ranks_women(input$team_selection_women)[[5]]}"))
  
  output$team_tens_count_women <- renderText(selected_team_ranks_women(input$team_selection_women)[[3]])
  
  output$team_tens_rank_women <- renderText(glue("Rank: {selected_team_ranks_women(input$team_selection_women)[[6]]}"))
  
  output$ind_wins_count_women <- renderText(selected_team_ranks_women(input$team_selection_women)[[7]])
  
  output$ind_wins_rank_women <- renderText(glue("Rank: {selected_team_ranks_women(input$team_selection_women)[[8]]}"))
  
  ## Render text for Individual wins, fastest time, and highest finish
  output$ind_winners_count_women <- renderText(selected_ind_rank_df_women(input$ind_selection_women)[[1]])
  
  output$ind_top10_finish_women <- renderText(selected_ind_rank_df_women(input$ind_selection_women)[[2]])
  
  output$fastest_time_women <- renderText(selected_ind_rank_df_women(input$ind_selection_women)[[3]])
  
  output$highest_finish_women <- renderText(selected_ind_rank_df_women(input$ind_selection_women)[[4]])
  
  ## Render table for Individual results
  output$ind_results_table_women <- renderTable(
    selected_ind_results_df_women(input$ind_selection_women),
    hover = TRUE,
    align = c("ccll"),
  )
  
  ## Render table for Ind results by Year
  output$ind_results_table_year_women <- renderTable(
    selected_year_ind_results_women(input$year_selection_women),
    hover = TRUE,
    striped = TRUE,
    align = c("clll"),
  )
  
  ## Render table for Team results by Year
  output$team_results_table_year_women <- renderTable(
    selected_year_team_results_women(input$year_selection_women),
    hover = TRUE,
    striped = TRUE,
    align = c("clc"),
  )
  
  ## Render text for Yearly tab metrics ----
  output$team_winning_margin_women <- renderText({
    
    pt_margin <- selected_year_team_win_margin_women(input$year_selection_women)
    pt_label <- ifelse(pt_margin == 1, "point", "points")
    
    ifelse(input$year_selection_women == "Years", "", glue("{selected_year_team_win_margin_women(input$year_selection_women)} {pt_label}"))
  })
  
  output$ind_winning_margin_women <- renderText({
    
    ind_margin <- round(selected_year_ind_win_margin_women(input$year_selection_women), 1)
    
    sec_label <- ifelse(ind_margin == 1, "second", "seconds")
    
    ifelse(input$year_selection_women == "Years", "", glue("{ind_margin} {sec_label}"))
    
  })
  
  ## Render text for Overall Individual Metrics  ----
  output$overall_ind_margin_min_women <- renderText(glue("{overall_ind_metrics_df_women[[1,2]]} seconds"))
  
  output$overall_ind_margin_max_women <- renderText(glue("{overall_ind_metrics_df_women[[2,2]]} seconds"))
  
  output$overall_ind_margin_avg_women <- renderText(glue("{overall_ind_metrics_df_women[[3,2]]} seconds\n"))
  
  output$overall_ind_time_win_women <- renderText(overall_ind_metrics_df_women[[4,2]])
  
  output$overall_ind_time_top5_women <- renderText(overall_ind_metrics_df_women[[5,2]])
  
  output$overall_ind_time_top10_women <- renderText(overall_ind_metrics_df_women[[6,2]])
  
  output$overall_ind_fastest_win_time_women <- renderText(overall_ind_metrics_df_women[[7,2]])
  
  output$overall_ind_slowest_win_time_women <- renderText(overall_ind_metrics_df_women[[8,2]])
  
  output$overall_ind_time_top2_women <- renderText(overall_ind_metrics_df_women[[9,2]])
  
  ## Render text for Overall Team Metrics ----
  output$overall_team_margin_min_women <- renderText(overall_team_metrics_df_women[[1,2]])
  
  output$overall_team_margin_max_women <- renderText(overall_team_metrics_df_women[[2,2]])
  
  output$overall_team_margin_avg_women <- renderText(overall_team_metrics_df_women[[3,2]])
  
  output$overall_team_points_win_women <- renderText(overall_team_metrics_df_women[[4,2]])
  
  output$overall_team_points_top4_women <- renderText(overall_team_metrics_df_women[[5,2]])
  
  output$overall_team_points_top10_women <- renderText(overall_team_metrics_df_women[[6,2]])
  
  output$overall_team_lowest_points_women <- renderText(overall_team_metrics_df_women[[7,2]])
  
  output$overall_team_highest_points_women <- renderText(overall_team_metrics_df_women[[8,2]])
  
  output$overall_team_points_top2_women <- renderText(overall_team_metrics_df_women[[9,2]])
  
  
}


# Run the app ----
shinyApp(ui = ui, server = server)
