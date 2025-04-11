# hente allevents og filter
# 1.1
library(readr)
library(dplyr)
library(caret)
library(rpart)
library(randomForest)
library(pROC)
library(ggsoccer)
library(metR)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggpubr)
library(DT)
library(lattice)

# Indlæs alle RDS-filer
afstand_analyse <- readRDS("afstand_analyse.RDS")
conf_df <- readRDS("conf_df.RDS")
dribble_to_shot_direct <- readRDS("dribble_to_shot_direct.RDS")
glm_df <- readRDS("glm_df.RDS")
opg_1_6 <- readRDS("opg.1.6.RDS")
opg_1 <- readRDS("opg.1.RDS")
pitch_grid <- readRDS("pitch_grid.RDS")
season_2324 <- readRDS("season_2324.RDS")
season_2325 <- readRDS("season_2325.RDS")
test <- readRDS("test.RDS")
training <- readRDS("training.RDS")
rf <- readRDS("rf.rds")
glm <- readRDS("glm.rds")
rf_matrix <- readRDS("rf_matrix.rds")
glm_matrix <- readRDS("glm_matrix.rds")

##############################################################################
# OPGAVE 1.5


# 2 tabeller - GLM + Random forrest

ui <- dashboardPage(
  dashboardHeader(title = "Konklusioner på xG"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Random Forest", tabName = "randomF", icon = icon("tree")),
      menuItem("GLM", tabName = "glm", icon = icon("chart-line")),
      menuItem("Chancen for at score", tabName = "målrate", icon = icon("futbol"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "randomF",
              h2("Random Forest Results"),
              fluidRow(
                box(plotOutput("plot1", height = 400), width = 6),
                box(plotOutput("plot2", height = 400), width = 6),
                box(plotOutput("plot3", height = 400), width = 8)
              )
      ),
      
      tabItem(tabName = "glm",
              h2("GLM Results"),
              fluidRow(
                box(plotOutput("plot4", height = 400), width = 6),
                box(plotOutput("plot5", height = 400), width = 6)
              ),
              box(
                title = "GLM koefficenter",
                width = 12,
                DT::dataTableOutput("dataTable")
              )
      ),
      
      tabItem(tabName = "målrate",
              h2("Chancen for at score ud fra afstand til målet"),
              fluidRow(
                box(plotOutput("plot8", height = 600), width = 12)
              )
      )
    )
  )
)

server <- function(input, output) {
  
  # Random Forest - Plot 1 (Importance Plot)
  predict_rf <- predict(rf, newdata=test)
  rf_probs <- predict(rf, type = "prob")
  imp_df <- as.data.frame(importance(rf))
  imp_df$Variable <- rownames(imp_df)
  accuracy <- sum(diag(rf_matrix)) / sum(rf_matrix)
  precision <- rf_matrix[2,2] / sum(rf_matrix[2,])
  
  # For klassifikationsmodeller (brug den relevante kolonne for din model)
  # For regression, brug '%IncMSE' i stedet for 'MeanDecreaseGini'
  imp_df <- imp_df %>%
    arrange(MeanDecreaseGini) %>%
    mutate(Variable = factor(Variable, levels = Variable))
  
  # GLM
  predict_glm <- predict(glm, type = "response")
  predict_glm_test <- predict(glm, newdata = test,  type = "response")
  preds_glm <- ifelse(predict_glm_test > 0.3, 1, 0)
  glm_accuracy <- sum(diag(glm_matrix)) / sum(glm_matrix)
  glm_precision <- glm_matrix[2,2] / sum(glm_matrix[2,])
  
  # 1.6 grid plot
  pitch_grid <- expand.grid(
    x = seq(0, 100, length.out = 100),
    y = seq(0, 100, length.out = 50)
  )
  pitch_grid <- pitch_grid %>%
    mutate(
      distance_to_goal = sqrt((x - 100)^2 + (y - 50)^2),
      afstand_gruppe = cut(distance_to_goal, breaks = seq(0, 70, by = 5))
    )
  pitch_grid <- pitch_grid %>%
    left_join(afstand_analyse, by = "afstand_gruppe") %>%
    mutate(faktisk_målrate = ifelse(is.na(faktisk_målrate), 0, faktisk_målrate))
  
  # Plot 1 - Random Forest Variable Importance
  output$plot1 <- renderPlot({
    roc_curve_rf <- roc(training$SHOTISGOAL, rf_probs[,2])
    
    # Plot ROC-kurven
    plot(roc_curve_rf, 
         main = "ROC-kurve for Random Forrest model", 
         col = "blue", 
         lwd = 2, 
         print.auc = TRUE,  # AUC vises på grafen
         print.auc.x = 0.6,  # Placer AUC-tallet på x-aksen
         print.auc.y = 0.4)  # Placer AUC-tallet på y-aksen
    
  })
  
  # Random Forest - Plot 2 (Confusion Matrix)
  output$plot2 <- renderPlot({
    ggplot(data = conf_df, aes(x = Reference, y = Prediction, fill = Frequency)) +
      geom_tile() +
      geom_text(aes(label = Frequency), color = "white", size = 5) +
      scale_fill_gradient(low = "darkblue", high = "lightblue") +
      labs(title = "Random Forest Confusion Matrix",
           subtitle = paste("Accuracy:", round(accuracy, 4), 
                            "Precision:", round(precision, 4)
           ),
           x = "Actual Class",
           y = "Predicted Class") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
  })
  
  # Plot 3 - Random Forest Variable Importance
  output$plot3 <- renderPlot({
    ggplot(imp_df, aes(x = Variable, y = MeanDecreaseGini)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label = round(MeanDecreaseGini, 2)), 
                hjust = -0.2, vjust = -0.3, size = 3) +
      labs(title = "Variable Importance",
           y = "Mean Decrease Gini",
           x = "") +
      theme_minimal() +
      coord_flip()
  })
  
  # GLM - Plot 4 (ROC-kurve)
  output$plot4 <- renderPlot({
    
    # Beregn ROC-kurven
    roc_curve_glm <- roc(test$SHOTISGOAL, predict_glm_test)
    
    # Plot ROC-kurven
    plot(roc_curve_glm, 
         main = "ROC-kurve for GLM-model", 
         col = "blue", 
         lwd = 2, 
         print.auc = TRUE,  # AUC vises på grafen
         print.auc.x = 0.6,  # Placer AUC-tallet på x-aksen
         print.auc.y = 0.4)  # Placer AUC-tallet på y-aksen
  })
  
  # GLM - Plot 5 (Confusion Matrix)
  output$plot5 <- renderPlot({
    ggplot(data = glm_df, aes(x = Reference, y = Prediction, fill = Frequency)) +
      geom_tile() +
      geom_text(aes(label = Frequency), color = "white", size = 5) +
      scale_fill_gradient(low = "darkblue", high = "lightblue") +
      labs(title = "GLM Confusion Matrix",
           subtitle = paste("Accuracy:", round(glm_accuracy, 4), 
                            "Precision:", round(glm_precision, 4)
           ),
           x = "Actual Class",
           y = "Predicted Class") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
    
  })
  
  # GLM koefficenter
  output$dataTable <- DT::renderDataTable({
    glm_summary <- summary(glm)
    glm_coef <- round(as.data.frame(coef(glm_summary)), digits = 2)
    
    # Tilføj variabelnavnene som en kolonne
    glm_coef$Variable <- rownames(glm_coef)
    
    # Omarranger kolonnerne (så variabelnavnet kommer først)
    glm_coef <- glm_coef[, c("Variable", "Estimate", "Std. Error", "z value", "Pr(>|z|)")]
    
    # Vis tabellen uden paging og uden rækkenumre
    DT::datatable(glm_coef,
                  options = list(
                    paging = FALSE,  # Deaktiverer paging
                    searching = FALSE  # Fjerner søgefeltet
                  ),
                  rownames = FALSE)
  })
  
  # plot til chancen for mål
  output$plot8 <- renderPlot({
    ggplot(pitch_grid, aes(x = x, y = y, fill = faktisk_målrate)) +
      geom_tile(alpha = 0.7) +
      # Banen
      geom_segment(aes(x = 0, y = 0, xend = 100, yend = 0), color = "white", size = 1) +
      geom_segment(aes(x = 100, y = 0, xend = 100, yend = 100), color = "white", size = 1) +
      geom_segment(aes(x = 100, y = 100, xend = 0, yend = 100), color = "white", size = 1) +
      geom_segment(aes(x = 0, y = 100, xend = 0, yend = 0), color = "white", size = 1) +
      # Straffesparksfeltet
      geom_segment(aes(x = 83, y = 21.5, xend = 100, yend = 21.5), color = "white", size = 1) +
      geom_segment(aes(x = 83, y = 78.5, xend = 100, yend = 78.5), color = "white", size = 1) +
      geom_segment(aes(x = 83, y = 21.5, xend = 83, yend = 78.5), color = "white", size = 1) +
      # Målet
      geom_segment(aes(x = 100, y = 45, xend = 100, yend = 55), color = "white", size = 2) +
      # Opdateret farveskala med højere maksimumværdi
      scale_fill_gradientn(
        colors = c("darkgreen", "green", "yellow", "orange", "red", "darkred"),
        values = scales::rescale(c(0, 0.03, 0.05, 0.1, 0.2, 0.3, 0.6)),
        limits = c(0, 0.6),  # Nu op til 60%
        name = "Målrate"
      ) +
      # Tilføj tekst for forskellige sandsynligheder
      annotate("text", x = 93, y = 50, label = "30%", color = "white", size = 4, fontface = "bold") +
      annotate("text", x = 85, y = 50, label = "20%", color = "white", size = 4, fontface = "bold") +
      annotate("text", x = 75, y = 50, label = "10%", color = "white", size = 4, fontface = "bold") +
      annotate("text", x = 65, y = 50, label = "5%", color = "white", size = 4, fontface = "bold") +
      annotate("text", x = 55, y = 50, label = "3%", color = "white", size = 4, fontface = "bold") +
      annotate("text", x = 40, y = 50, label = "<3%", color = "white", size = 4, fontface = "bold") +
      # Tilføj label for højere målrater tæt på målet
      annotate("text", x = 98, y = 50, label = "50%+", color = "white", size = 4, fontface = "bold") +
      # Tilpas koordinatsystem
      coord_fixed() +
      theme_void() +
      theme(
        legend.position = "bottom",
        plot.background = element_rect(fill = "darkgreen")
      ) +
      labs(
        title = "Expected Goals (xG) baseret på afstand til mål",
        subtitle = "Superligaen 2023/2024") +
      theme(
        plot.title = element_text(color = "white", size = 16),
        plot.subtitle = element_text(color = "white", size = 14)  )
  })
  
  
}

shinyApp(ui, server)
