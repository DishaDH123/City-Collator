library(dash)
library(dashHtmlComponents)
library(devtools)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(tidyverse)
library(reshape)
library(dashHtmlComponents)

data <- read.csv("data.csv")
radio_button = dccRadioItems(
  options=list(
    list(label="Cost of living index", value = 1),
    list(label="Overall expenditure", value = 6),
    list(label="Average Salary", value = 7)
  ),
  id="radio",
  value=1,
  labelStyle = list("display" = "block"),
  style=list(color="#61A4BC")
)

mdata <- select(data,-cost_of_living_and_rent,-estimated_monthly_cost,-average_salary)

mdata <- melt(mdata, id=c("City"))

plot2 <- mdata %>%
        mutate(City = fct_reorder(City, value))  %>%
        ggplot() + aes(x=City, y=value, group=variable, color=variable) +
        geom_point() + geom_line()+ theme_bw() + 
        theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12)) + 
        scale_color_manual(values = c("#1572A1", "#6D8299", "#79B4B7","#424874"))

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP,suppress_callback_exceptions = TRUE)
app$layout(
    dbcContainer(
        list(
            dbcRow("Welcome to the city collator dashboard!",style=list('font-size'='30px')),
            dbcLabel("Every year we see a wide range and variety of immigrants to the vast country of Canada. Have you ever wondered whcih city would be the most cost efficient for you to live in? Has it occured to you ever which is the city which would give you the highest wage? If yes, you are in the right place! Just select the catrgory you would want to compare across and the dashboard will display cities and compare across the same. What are you waiting for? Go give it a try!"), # nolint
            br(),
            dbcLabel("Compare the various cities in Canada with the following indicators",style=list('font-size'='20px')),
            br(),
            dbcRow(radio_button),
            br(),


            dbcRow(
                list(
                    dbcCol(
                        div(
                            dccGraph(id = "plot"),
                            style = list(width = "80 %", padding = "10px 5px", backgroundColor = "#1572A1") # nolint
                            ),
                            md = 6
                            ),
                    dbcCol(
                            div(
                            dccGraph(figure=ggplotly(plot2)),
                            style = list(width = "100%", padding = "10px 5px", backgroundColor = "#1572A1") # nolint
                                    ),
                            md = 6
                            )
                                    )
                                    )
            
)
)
)

app %>% add_callback(

    output("plot", "figure"),
    list(input("radio", "value")), # nolint
    function(category) {
        if (category == 1) {
        plot <- data %>%
        arrange(cost_of_living_and_rent) %>%   
        mutate(City=factor(City, levels=City)) %>% 
        ggplot() + aes(x=City, y=cost_of_living_and_rent, group=1) +
        geom_point(color="#1572A1") + geom_line(color="#589cbe")+ theme_bw() + 
        theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12)) 
        ggplotly(plot)
        }
       
        else if (category == 6) {
        plot <- data %>%
        arrange(estimated_monthly_cost) %>%   
        mutate(City=factor(City, levels=City)) %>% 
        ggplot() + aes(x=City, y=estimated_monthly_cost, group=1) +
        geom_point(color="#1572A1") + geom_line(color="#589cbe")+ theme_bw() + 
        theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12)) 
        ggplotly(plot)
        }
        else if (category == 7) {
        plot <- data %>%
         arrange(average_salary) %>%   
         mutate(City=factor(City, levels=City)) %>% 
        ggplot() + aes(x=City, y=average_salary, group=1) +
        geom_point(color="#1572A1") + geom_line(color="#589cbe")+ theme_bw() + 
        theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12)) 
        ggplotly(plot)
         }
    }
)

app$run_server(debug = T)