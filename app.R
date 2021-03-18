# STA141B Final Project
# Yi Fu
library(shiny)
library(jsonlite)
library(DT)
library(dplyr)
library(curl)

getData <- function(brand, price_range = c(0, 80), rating_range = c(0, 5),
                 tag = NA, type = NA, category = NA){
  brand <- tolower(brand)
  tag <- tolower(tag)

  url <- "http://makeup-api.herokuapp.com/api/v1/products.json?"
  url <- paste0(url, "brand=", brand)
  url <- paste0(url, "&price_greater_than=", price_range[1])
  url <- paste0(url, "&price_less_than=", price_range[2])
  url <- paste0(url, "&rating_greater_than=", rating_range[1])
  url <- paste0(url, "&rating_less_than=", rating_range[2])

  if(!is.na(tag) & tag!= "-"){
    url <- paste0(url, "&product_tag=", tag)
  }
  if(!is.na(type) & type !="-"){
    url <- paste0(url, "&product_type=", type)
  }

  url <- URLencode(url)
  print(url)
  fromJSON(url)
}


tags <- c("Canadian",
         "CertClean",
         "Chemical Free",
         "Dairy Free",
         "EWG Verified",
         "EcoCert",
         "Fair Trade",
         "Gluten Free",
         "Hypoallergenic",
         "Natural",
         "No Talc",
         "Non GMO",
         "Organic",
         "Peanut Free Product",
         "Sugar Free",
         "USDA Organic",
         "Vegan",
         "alcohol free",
         "cruelty free",
         "oil free",
         "purpicks",
         "silicone free",
         "water free")

brands <- c("almay",
            "alva",
            "anna sui",
            "annabelle",
            "benefit",
            "boosh",
            "burt's bees",
            "butter london",
            "c'est moi",
            "cargo cosmetics",
            "china glaze",
            "clinique",
            "coastal classic creation",
            "colourpop",
            "covergirl",
            "dalish",
            "deciem",
            "dior",
            "dr. hauschka",
            "e.l.f.",
            "essie",
            "fenty",
            "glossier",
            "green people",
            "iman",
            "l'oreal",
            "lotus cosmetics usa",
            "maia's mineral galaxy",
            "marcelle",
            "marienatie",
            "maybelline",
            "milani",
            "mineral fusion",
            "misa",
            "mistura",
            "moov",
            "nudus",
            "nyx",
            "orly",
            "pacifica",
            "penny lane organics",
            "physicians formula",
            "piggy paint",
            "pure anada",
            "rejuva minerals",
            "revlon",
            "sally b's skin yummies",
            "salon perfect",
            "sante",
            "sinful colours",
            "smashbox",
            "stila",
            "suncoat",
            "w3llpeople",
            "wet n wild",
            "zorah",
            "zorah biocosmetiques")

types <- c("blush", "bronzer", "eyebrow", "eyeliner", "eyeshadow", "foundation",
           "lip_liner", "lipstick", "mascara", "nail_polish", "")

#####ui##############################
ui <- fluidPage(
  titlePanel("Makeup search up"),

  sidebarLayout(

    sidebarPanel(
      selectInput("brand", "Brand", choices = brands),
      selectInput("tag", "Tag", choices = c("-", tags), selected = "-"),
      selectInput("type", "Type", choices = c("-", types), selected = "-"),
      sliderInput("price", "Price", min = 0, max = 80, value = c(0, 80), step = 0.1),
      sliderInput("rating", "Rating", min = 0, max = 5, value = c(0, 5), step = 0.1)
    ),

    mainPanel(
          DTOutput(outputId = "table")

    )
  )
)

##########server###############################
server <- function(input, output) {

  output$table <- renderDataTable({
    df <- getData(input$brand, input$price, input$rating, input$tag, input$type)
    if(!is.data.frame(df)){
      data.frame(id="no products found for this brand")
    }else{
      df %>% select(id, brand, name, price, rating, product_type)
    }
  })
}

shinyApp(ui, server)
