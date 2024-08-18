# This Shiny App simulates the environmental impacts of individuals derived from their food consumption
# It is based on data representative of the French population (ANSES, INCA3)
# The simulator is valid for adults, focusing on age group 18-75
# This tool was developed in the context of a project for the Dismal Science website (http://dismal-science.eu)
#


# Note: the person using the simulator is refered to as the "individual". Comparison groups (e.g., men or women, typically of the same age group as the individual) are called "demographic groups".


# 1. Loading libraries -------------------------------------------------------

library(tidyverse)

library(shiny)
library(shiny.fluent)
library(shiny.router)
library(shinyjs)

library(gridlayout)
library(glue)

library(highcharter)
library(ggplot2)
library(ggpubr)
library(treemapify)

library(rmarkdown)
library(knitr)




# 2. Loading relevant data files ------------------------------------------


# Loading benchmark data (useful to compare the results of the individual using the simulator to other people), data used to derive the estimates for the individual, and various global variables 


## 2.1 Benchmark data for comparisons --------------------------------------


### a. Consumption data -----------------------------------------------------


yearly_conso_demo_groups <- read.csv("data/benchmark/yearly_conso_demo_groups.csv") # Consumption data of demographic groups at the level of specific and macro groups of food


### b. Impacts data -----------------------------------------------------


yearly_impacts_demo_groups <- read.csv("data/benchmark/yearly_impacts_demo_groups.csv") # Detailed yearly impacts by demographic groups and types of impacts (aggregated)
yearly_impacts_demo_groups_by_food_group <- read.csv("data/benchmark/yearly_impacts_demo_groups_by_food_group.csv") # Detailed yearly impacts by demographic groups and types of impacts (breakdown with the impacts of specific groups of food)
aggregated_yearly_vars_dem_groups <- read.csv("data/benchmark/aggregated_yearly_vars_dem_groups.csv") # Same as yearly_impacts_demo_groups but in a wider format 
percentiles_impacts_all <- read.csv("data/benchmark/percentiles_impacts_all.csv") # Percentiles by demographic group and types of impacts


### c. Demographic group background data -----------------------------------------------------


anthropometrics <- read.csv("data/benchmark/anthropometrics.csv") # Anthropometric data (e.g., height, weight, BMI, BMR) by demographic group, and related environmental impacts at the individual level


## 2.2. Data for estimates of individual consumption --------------------------------------


### a. Beverage data -----------------------------------------------------

beverage_composition <- read.csv("data/estimates/beverages/beverage_composition.csv") # Proportion of the diet (0 to 1) for the different types of beverages, sorted out by typology of drinkers
beverage_intensities <- read.csv("data/estimates/beverages/beverage_intensities.csv") # Impact of the different beverages by unit of mass, for all the types of environmental impacts. Sorted out by typology of drinkers. 



### b. Food data -----------------------------------------------------


food_composition <- read.csv("data/estimates/food/food_composition.csv") # Proportion of the diet (0 to 1) for the different specific groups of food, sorted out by typology of eaters (based on number of meals)
food_intensities <- read.csv("data/estimates/food/food_intensities.csv") # Impact of the different specific groups of food by unit of mass, for all the types of environmental impacts. Sorted out by typology of eaters 


## 2.3. Data for global variables --------------------------------------


### a. Correspondence tables -----------------------------------------------------


match_macro_specific <- read.csv("data/global/match_macro_specific.csv") # Correspondence table linking macro groups of food to each specific group of food


### b. Description of Impacts -----------------------------------------------------


description_impacts <- read.csv("data/global/description_impacts.csv") # Summary table for the 15 different types of environmental impacts

list_impacts <- as.vector(description_impacts$type_impacts)
list_names_impacts <- as.vector(description_impacts$name)
list_units_impacts <- as.vector(description_impacts$unit)

### c. Colors for Graphs -----------------------------------------------------

data_colors_beverages <- read.csv("style/colors/data_colors_beverages.csv")
data_colors_macrofood <- read.csv("style/colors/data_colors_macrofood.csv")
data_colors_sex <- read.csv("style/colors/data_colors_sex.csv")
data_colors_impacts <- read.csv("style/colors/data_colors_impacts.csv")
data_colors_ramp <- read.csv("style/colors/data_colors_ramp.csv")




# 3. UI and information collection from the user -------------------------------------------------------

## 3.1. Helper functions and theming  --------------------------------------

### a. Helper functions for the UI -----------------------------------------------------


DropdownMenuItemType <- function(type) { # Rendering headers and dividers inside dropdowns
  
  JS(paste0("jsmodule['@fluentui/react'].DropdownMenuItemType."), type)
  
}


makeCard <- function(title, content, size = 12, style = "") { # creation of a basic card to be used to construct pages. Can control title, content, size of the card, and style
  
  div(
    
    class = glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = style,
    
    Stack(
      tokens = list(childrenGap = 5),
      Text(variant = "xLarge", title, block = TRUE),
      content
    )
    
  )
}




makeValueBox <- function(title, value, subtitle, text_color = "white", fontsize_core = "mega", background_color = "#0078d4", image = "https://via.placeholder.com/40", size = 12) { # creation of a value box to showcase a specific figure, with an associated image
  # Can define the title of the box, the value, a potential subtitle, text color, text size of the value, background color, image, and size of the box (12 = full size)
  
  div(
    
    class = glue("value-box ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = paste0("background-color: ", background_color,"; color: white; padding: 20px; border-radius: 5px; position: relative; overflow: hidden; display: flex; align-items: center;   margin-bottom: 28px;"),
    
    div(
      style = "display: flex; flex-direction: column; flex-grow: 1; align-items: flex-start;", # Adjusted align-items
      Text(variant = "xLarge", style = list(color = text_color),  title),
      Text(variant = fontsize_core, style = list(color = text_color), value),
      Text(variant = "xLarge", style = list(color = text_color), subtitle)
    ),
    
    div(
      style = "max-width: 30%; max-height: 100%; flex-shrink: 0; display: flex; justify-content: flex-end;", # Adjusted max-width and added display: flex and justify-content
      
      img(
        src = image,
        style = "width: auto; height: auto; max-height: 100%; max-width: 100%; object-fit: contain; align-self: center;" # Adjusted width and height
      )
      
    )
    
  )
}



makePage <- function (title, subtitle, contents) { # creation of a function to build each page, based on a title, subtitle and the content
  
  tagList(div(
    class = "page-title",
    
    span(title, class = "ms-fontSize-32 ms-fontWeight-semibold", style =
           "color: #323130"),
    
    span(subtitle, class = "ms-fontSize-18 ms-fontWeight-regular", style =
           "color: #605E5C; margin: 14px;")
  ),
  
  contents)
  
}



### b. Graph theming (highcharter) -----------------------------------------------------

chosen_theme <- hc_theme_google() # set the theme from highcharter

## Graph Font Sizes

title_size <- 18
axis_size <- 14
caption_size <- 14


## Graph Colors 

colors_beverages <- data_colors_beverages$color
colors_macrofood <- data_colors_macrofood$color
colors_sexyou <- data_colors_sex$solid_color
colors_sexyou_faded <- data_colors_sex$faded_color
colors_impacts <- data_colors_impacts$color
colors_box_neg_pos <- data_colors_ramp$color
colors_intermediate_waterfall <- data_colors_beverages$color[1]
colors_negative_positive <- c("#BE0032", "#008856")


### c. Other theming -----------------------------------------------------


# Value Boxes styling

impacts_vb_size <- 3
impacts_vb_font_color <- "white"

understand_vb_size <- 11.5
understand_vb_font_color <- "white"

compare_vb_size <- 11.5
compare_vb_font_color <- "white"

## 3.2. Build input controllers  --------------------------------------


### a. Inputs for the profile page (i.e., demographic profile, food consumption, beverage consumption) -----------------------------------------------------


profile_inputs <- tagList( # list of inputs to determine the demographic profile
  
  Dropdown.shinyInput("sex", value = "male", 
  options = list(
    
    list(key = "male", text = "Male"),
    list(key = "female", text = "Female")), 
  
  label = "Sex/Gender"),
  
  SpinButton.shinyInput("age", value = 18, min = 18, max = 79, step = 1, label = "Age"),
  
  SpinButton.shinyInput("height", value = 175, min = 1, step = 1, label = "Height (cm)"),
  
  SpinButton.shinyInput("weight", value = 80, min = 1, step = 1, label = "Weight (kg)")
  
  
  )


food_inputs <- tagList( # list of inputs to determine the food consumption patterns 
  tags$p(""),
  
  Toggle.shinyInput("food_expert", value = FALSE, label="Expert mode", inlineLabel = TRUE),
  
  conditionalPanel("input.food_expert == '0'", Dropdown.shinyInput("simple_food_quantity", value =  1250, 
    options = list(
      
    list(key = 500, text = "Very Low"), # D1
    list(key = 900, text = "Low"), #Q25 for Women
    list(key = 1000, text = "Moderate"), 
    list(key = 1100, text = "Average - Low"), # Q25 for Men
    list(key = 1250, text = "Average"),
    list(key = 1400, text = "Average - High"), # Q75 for Women
    list(key = 1500, text = "Substantial"),
    list(key = 1600, text = "High"), # Q75 for Men
    list(key = 1800, text = "Very High")), label = "Food quantities eaten")), # D10
  
  
  conditionalPanel("input.food_expert == '1'", Slider.shinyInput("food_quantity", value = 1250, min = 150, max = 3500, step = 50, label = "Daily food consumption (g)") ), # expert slider for quantities
  
  tags$p(""),
  
  tags$h3("Please describe your diet:"),
  
  textOutput("red_meat_text"), 
  Slider.shinyInput("red_meat_meals", min = 0, max = 14, step = 1, value = 0, showValue = FALSE),
  div(style = "display: flex; justify-content: space-between;", span("Less"), span("More") ),
  tags$p(""),
  
  textOutput("white_meat_text"), 
  Slider.shinyInput("white_meat_meals", min = 0, max = 14, step = 1, value = 0, showValue = FALSE),
  div(style = "display: flex; justify-content: space-between;", span("Less"), span("More") ),
  tags$p(""),
  
  textOutput("fish_text"), 
  Slider.shinyInput("fish_meals", min = 0, max = 14, step = 1, value = 0, showValue = FALSE),
  div(style = "display: flex; justify-content: space-between;", span("Less"), span("More") ),
  tags$p(""),
  
  textOutput("vegetarian_text"), 
  Slider.shinyInput("vegetarian_meals", min = 0, max = 14, step = 1, value = 0, showValue = FALSE),
  div(style = "display: flex; justify-content: space-between;", span("Less"), span("More") ),
  tags$p(""),
  
  textOutput("vegan_text"), 
  Slider.shinyInput("vegan_meals", min = 0, max = 14, step = 1, value = 0, showValue = FALSE),
  div(style = "display: flex; justify-content: space-between;", span("Less"), span("More") ),
  tags$p("")
  
  
)

beverage_inputs <- tagList( # list of inputs to determine the beverage consumption patterns 
  
  tags$p(""),
  
  Toggle.shinyInput("beverage_expert", value = FALSE, label="Expert mode", inlineLabel = TRUE),
  
  conditionalPanel("input.beverage_expert == '0'", Dropdown.shinyInput("simple_beverage_quantity", value = 1650, 
   options = list(
    
    list(key = 900, text = "Very Low"), # D1
    list(key = 1100, text = "Low"), #Q25 for Women 
    list(key = 1200, text = "Moderate"),
    list(key = 1300, text = "Average - Low"),#Q25 for Men
    list(key = 1650, text = "Average"),
    list(key = 2000, text = "Average - High"), #Q75 for Women
    list(key = 2100, text = "Substantial"),
    list(key = 2200, text = "High"), #725 for Men
    list(key = 2600, text = "Very High")), label = "Beverage quantities drunk")), # D10
  
  conditionalPanel("input.beverage_expert == '1'", Slider.shinyInput("beverage_quantity", value = 1650, min = 150, max = 6500, step = 50, label = "Daily beverage consumption (mL)") ),
  
  tags$p(""),
  tags$h3("Please select how much you drink these beverages"),
  
  Dropdown.shinyInput("water", value = 0, options = list(
    list(key = 0, text = "None"),
    list(key = 1, text = "Limited"),
    list(key = 2, text = "Moderate"),
    list(key = 3, text = "Average"),
    list(key = 4, text = "High"),
    list(key = 5, text = "Very High")), label = "Water consumption"),
    htmlOutput("water_text"),
  
  
  Dropdown.shinyInput("alcohol", value = 0, options = list(
    list(key = 0, text = "None"),
    list(key = 1, text = "Limited"),
    list(key = 2, text = "Moderate"),
    list(key = 3, text = "Average"),
    list(key = 4, text = "High"),
    list(key = 5, text = "Very High")), label = "Alcohol consumption"),
    htmlOutput("alcohol_text"),
  
  
  Dropdown.shinyInput("other_drink", value = 0, options = list(
    list(key = 0, text = "None"),
    list(key = 1, text = "Limited"),
    list(key = 2, text = "Moderate"),
    list(key = 3, text = "Average"),
    list(key = 4, text = "High"),
    list(key = 5, text = "Very High")), label = "Other drinks (milk, juices...) consumption"),
    htmlOutput("other_drink_text"),
  
  Dropdown.shinyInput("coffee", value = 0, options = list(
    list(key = 0, text = "None"),
    list(key = 1, text = "Limited"),
    list(key = 2, text = "Moderate"),
    list(key = 3, text = "Average"),
    list(key = 4, text = "High"),
    list(key = 5, text = "Very High")), label = "Coffee consumption"),
    htmlOutput("coffee_text"),
  
  
  Dropdown.shinyInput("tea", value = 0, options = list(
    list(key = 0, text = "None"),
    list(key = 1, text = "Limited"),
    list(key = 2, text = "Moderate"),
    list(key = 3, text = "Average"),
    list(key = 4, text = "High"),
    list(key = 5, text = "Very High")), label = "Tea consumption"),
    htmlOutput("tea_text")
  
)




### b. Inputs for the comparison page (i.e., selection of the impacts) -----------------------------------------------------


compare_inputs <- tagList( # list of inputs to determine the impacts to analyse in the comparison page
  
  tags$p(""),
  
  
  Dropdown.shinyInput("selected_impact_compare", value = 1, 
    
                      options = list(
                        
    list(
      key = "majorimpactsHeader",
      text = "Major Impacts",
      itemType = DropdownMenuItemType("Header")
    ),
    
    list(key = 1, text = "Environmental Footprint Score"),
    list(key = 2, text = "Anthropogenic Climate Change"),
    list(key = 11, text = "Land Use"),
    
    list(
      key = "divider_1",
      text = "-",
      itemType = DropdownMenuItemType("Divider")
    ),
    
    list(
      key = "healthimpactsHeader",
      text = "Health-related Impacts",
      itemType = DropdownMenuItemType("Header")
    ),
    
    list(key = 3, text = "Ozone Layer Depletion"),
    list(key = 5, text = "Photochemical Ozone Formation"),
    list(key = 4, text = "Ionizing Radiation"),
    list(key = 6, text = "Particulate Matter"),
    
    list(
      key = "divider_2",
      text = "-",
      itemType = DropdownMenuItemType("Divider")
    ),
    
    list(
      key = "watersoilHeader",
      text = "Water & Soil Impacts",
      itemType = DropdownMenuItemType("Header")
    ),
    
    list(key = 7, text = "Soil & Water Acidification"),
    list(key = 8, text = "Terrestrial Eutrophication"),
    list(key = 9, text = "Freshwater Eutrophication"),
    list(key = 10, text = "Marine Eutrophication"),
    list(key = 12, text = "Freshwater Ecotoxicity"),
    list(key = 13, text = "Water Depletion"),
    
    list(
      key = "divider_3",
      text = "-",
      itemType = DropdownMenuItemType("Divider")
    ),
    
    list(
      key = "otherHeader",
      text = "Other Depletion Impacts",
      itemType = DropdownMenuItemType("Header")
    ),
    
    list(key = 14, text = "Energy Depletion"),
    list(key = 15, text = "Metals/Minerals Depletion")), label = "")
  
)



### c. Inputs for the understand page (i.e., selection of the impacts) -----------------------------------------------------



understand_inputs <- tagList( # list of inputs to determine the impacts to analyse in the understand page
  
  tags$p(""),
  
  Dropdown.shinyInput("selected_impact_understand", value = 1,
  options = list(
    
    list(
      key = "majorimpactsuHeader",
      text = "Major Impacts",
      itemType = DropdownMenuItemType("Header")
    ),
    
    list(key = 1, text = "Environmental Footprint Score"),
    list(key = 2, text = "Anthropogenic Climate Change"),
    list(key = 11, text = "Land Use"),
    
    list(
      key = "divider_1u",
      text = "-",
      itemType = DropdownMenuItemType("Divider")
    ),
    
    list(
      key = "healthimpactsuHeader",
      text = "Health-related Impacts",
      itemType = DropdownMenuItemType("Header")
    ),
    
    list(key = 3, text = "Ozone Layer Depletion"),
    list(key = 5, text = "Photochemical Ozone Formation"),
    list(key = 4, text = "Ionizing Radiation"),
    list(key = 6, text = "Particulate Matter"),
    
    list(
      key = "divider_2u",
      text = "-",
      itemType = DropdownMenuItemType("Divider")
    ),
    
    list(
      key = "watersoiluHeader",
      text = "Water & Soil Impacts",
      itemType = DropdownMenuItemType("Header")
    ),
    
    list(key = 7, text = "Soil & Water Acidification"),
    list(key = 8, text = "Terrestrial Eutrophication"),
    list(key = 9, text = "Freshwater Eutrophication"),
    list(key = 10, text = "Marine Eutrophication"),
    list(key = 12, text = "Freshwater Ecotoxicity"),
    list(key = 13, text = "Water Depletion"),
    
    list(
      key = "divider_3u",
      text = "-",
      itemType = DropdownMenuItemType("Divider")
    ),
    
    list(
      key = "otheruHeader",
      text = "Other Depletion Impacts",
      itemType = DropdownMenuItemType("Header")
    ),
    
    list(key = 14, text = "Energy Depletion"),
    list(key = 15, text = "Metals/Minerals Depletion")), label = "")
  
  
)



### d. Inputs for the anthropometrics page (i.e., selection of the impacts) -----------------------------------------------------


anthropometrics_inputs <- tagList( # list of inputs to determine the impacts to analyse in the anthropometrics page
  
  tags$p(""),
  
  Dropdown.shinyInput("selected_impact_anthropometrics", value = 1, 
    options = list(
      
    list(
      key = "majorimpactsbHeader",
      text = "Major Impacts",
      itemType = DropdownMenuItemType("Header")
    ),
    
    list(key = 1, text = "Environmental Footprint Score"),
    list(key = 2, text = "Anthropogenic Climate Change"),
    list(key = 11, text = "Land Use"),
    
    list(
      key = "divider_1b",
      text = "-",
      itemType = DropdownMenuItemType("Divider")
    ),
    
    list(
      key = "healthimpactsbHeader",
      text = "Health-related Impacts",
      itemType = DropdownMenuItemType("Header")
    ),
    
    list(key = 3, text = "Ozone Layer Depletion"),
    list(key = 5, text = "Photochemical Ozone Formation"),
    list(key = 4, text = "Ionizing Radiation"),
    list(key = 6, text = "Particulate Matter"),
    
    list(
      key = "divider_2b",
      text = "-",
      itemType = DropdownMenuItemType("Divider")
    ),
    
    list(
      key = "watersoilbHeader",
      text = "Water & Soil Impacts",
      itemType = DropdownMenuItemType("Header")
    ),
    
    list(key = 7, text = "Soil & Water Acidification"),
    list(key = 8, text = "Terrestrial Eutrophication"),
    list(key = 9, text = "Freshwater Eutrophication"),
    list(key = 10, text = "Marine Eutrophication"),
    list(key = 12, text = "Freshwater Ecotoxicity"),
    list(key = 13, text = "Water Depletion"),
    
    list(
      key = "divider_3b",
      text = "-",
      itemType = DropdownMenuItemType("Divider")
    ),
    
    list(
      key = "otherbHeader",
      text = "Other Depletion Impacts",
      itemType = DropdownMenuItemType("Header")
    ),
    
    list(key = 14, text = "Energy Depletion"),
    list(key = 15, text = "Metals/Minerals Depletion")), label = "")

  
)



### e. Inputs for the download report page (i.e., name customisation) -----------------------------------------------------


download_inputs <- tagList( # list of inputs for the download page

TextField.shinyInput(
  label = "Enter your name:",
  placeholder = "Anonymous",
  inputId = "your_name"),

tags$p(""),


PrimaryButton.shinyInput( # Note: it necessitates a hidden download button to circumvent issues with fluent 
  "downloadButton",
  text = "Download",
  iconProps = list(iconName = "Download")
),

tags$p(""),

uiOutput("spinner")


)







## 3.3. Build cards  --------------------------------------


### a. Cards for the Home Page  -----------------------------------------------------

home_context_card <- makeCard(
  
  "Context",
  
  div(
    
    Text("This application was developed in the context of a project evaluating the environmental impacts from food consumption of French adults. It is based on reputable sources, including the INCA3 study from the ANSES (food habits) and AGRIBALYSE database of ADEME (environmental impacts).", variant = "large"),
    
    br(),
    br(),
    
    Text("This simulator allows the user to estimate his/her own environmental impacts and to compare them to the rest of the population. It should be understood as providing orders of magnitude, rather than exact and fully individualized values.", variant = "large")
    
    ), size = 11.5)




home_instructions_card <- makeCard(
  
  "How to Use It?",
  
  div(
    
    Text("To use the simulator, select the 'Your Profile' tab above and compile the information with the highest possible level of precision. Then, see the plots and analyses in the other tabs.", variant = "large"),
    
    br(),
    br(),
    
    Text("It will give you some insigths to improve your environmental impacts, e.g., by identifying where you could reduce or substitute elements.", variant = "large"),
    
  ), size = 11.5)



home_scope_card <- makeCard(
  
  "Scope",
  
  div(
    
    Text("This tool covers 15 different environmental impact indicators.", variant = "large"),
    
    br(),
    br(),
    
    Text("A short description of the different impacts is provided here:", variant = "large"),
    
    br(),
    
    tags$ul(
      
      tags$li(Text("Environmental footprint score: synthetic indicator on the environmental and health impacts", variant = "large")), 
      
      tags$li(Text("Climate change: increase in carbon emissions and related consequences (e.g., rise in global temperature)", variant = "large")), 
      
      tags$li(Text("Ionizing radiation: radioactivity from nuclear waste", variant = "large")), 
      
      tags$li(Text("Ozone layer depletion: damage to the ozone layer (a natural shield against harmful solar UV radiations)", variant = "large")), 
      
      tags$li(Text("Photochemical Ozone formation: degradation of air quality", variant = "large")), 
      
      tags$li(Text("Particulate matter: air pollution with adverse health effects", variant = "large")), 
      
      tags$li(Text("Land use: consumption of land, leading to biodiversity pressures", variant = "large")), 
      
      tags$li(Text("Soil and water acidification: chemical emissions and associated phenomena such as acid rains", variant = "large")), 
      
      tags$li(Text("Terrestrial eutrophication: excess nitrogen leading to disequilibria and to the impoverishment of the ecosystems", variant = "large")), 
      
      tags$li(Text("Marine eutrophication: excess of nutrients in the sea, leading to problems of proliferation and asphyxia especially though algae", variant = "large")), 
      
      tags$li(Text("Freshwater eutrophication: similar to marine eutrophication but in freshwater milieus, such as rivers or lakes", variant = "large")), 
      
      tags$li(Text("Freshwater ecotoxicity: contamination of the water environment", variant = "large")), 
      
      tags$li(Text("Water resources depletion: water consumption", variant = "large")), 
      
      tags$li(Text("Energy resources depletion: consumption of non-renewable energy. It mostly includes fossil fuels (e.g., coal, gas, oil…) and uranium", variant = "large")), 
      
      tags$li(Text("Mineral resources depletion: consumption of non-renewable mineral resources (including metals), such as copper, rare earth metals, sand...", variant = "large")), 
    
      
    ),), size = 11.5)


home_methodology_card <- makeCard(
  
  "Methodology",
  
  div(
    
    Text("This app uses real-world data from the French population to simulate your environmental impacts from food consumption. It proceeds in three major steps:",  variant = "large"),
    
    br(),
    br(),
    
    Text("First it defines the quantities of food and beverage that you eat.", variant = "large"),
    
    br(),
    br(),
    
    Text("Then, it estimates the composition of your diet, classified into 10s of granular groups (e.g., beef, eggs...). In order to avoid user fatigue, the shares of these groups in your diet is estimated based on the types of meal you consume in a typical week (e.g., 2 meals with red meat, 1 with white meat, and 11 vegetearian meals). Quantities are then multiplied by these shares to get the consumed quantities for each group. ", variant = "large"),
    
    br(),
    br(),
    
    Text("The quantities are themselves multiplied by environmental intensities for each type of impact, i.e., how much damage one kg of each food type does. The damages are then aggregated to get your full impacts", variant = "large"),
    
  ), size = 11.5)




### b. Cards for the Profile / Settings Page  -----------------------------------------------------



profile_demographic_card <- makeCard("Demographic information", profile_inputs, size = 4)


profile_instructions_card <- makeCard("Instructions", 
                            
                                      div("Fill in your demographic information as well as your habits in terms of diet (food and beverage).", 
                                                          
                                          br(), br(), 
                                          
                                          "Please also fill in your anthropometrical data (height and weight) to allow further analyses." , 
                                          
                                          br(), br(),
                                                          
                                          "The graphs on this page allow you to review the estimates of your consumption, and to compare it to the average individual from your sex and age group."),
                                      
                                      size = 8)


profile_food_inputs_card <- makeCard("Food Consumption", food_inputs, size = 4)


profile_beverage_inputs_card <- makeCard("Beverage Consumption", beverage_inputs, size = 4)


profile_graph_food_individual_card <- makeCard("", highchartOutput("graph_food_individual"), size = 4)


profile_graph_food_dem_card <- makeCard("", highchartOutput("graph_food_dem"), size = 4)


profile_graph_beverage_individual_card <- makeCard("", highchartOutput("graph_beverage_individual"), size = 4)


profile_graph_beverage_dem_card <- makeCard("", highchartOutput("graph_beverage_dem"), size = 4)




### c. Cards for the Impacts Page  -----------------------------------------------------

 
# Generate all the value boxes

for (i in 1:length(list_impacts)) {
  
  assign(paste0("impacts_", list_impacts[i],"_vb"),  makeValueBox(title = list_names_impacts[i], subtitle = list_units_impacts[i], value = textOutput(paste0("vb_", list_impacts[i])), background_color = colors_impacts[i], text_color = impacts_vb_font_color, image = paste0("impacts_logo/", list_impacts[i], ".png"), size = impacts_vb_size) ) 
  
}



### d. Cards for the Comparison Page  -----------------------------------------------------


comparison_instructions_card <- makeCard("Instructions", 
                                         
                                         div("In this tab, you can compare your impacts against other people from the same age group (this is the most adequate approach due to changes in dietary habits over the lifecycle):", 
                                             br(), br(), 
                                             
                                             "The comparisons all refer to the type of impact you select in the box on the right.", 
                                             
                                             br(), br(), 
                                             
                                             "First, you can compare your impacts to the average ones produced by both men and women of your age group. The boxes on the right summarize where you fall compared to them", 
                                             
                                             br(), br(), 
                                             
                                             "Then, the graph below compares your impacts to the entire distribution for both men and women of your age group. This is relevant since the distributions are often very skewed: some people have very high impacts that can push the average upwards. That way, you can get a complementary view of where you fall. The boxes on the right show the corresponding percentiles of the distributions where you end (meaning which percentage of people pollute more than you for this impact)")
                                         
                                         , size = 8)



comparison_select_card <- makeCard("Select an impact", compare_inputs, size = 4)


comparison_graph_individual_vs_groups_card <- makeCard("", highchartOutput("graph_individual_vs_groups"), size = 8)


comparison_male_perc_vb <- uiOutput("vb_male_perc")


comparison_female_perc_vb <-uiOutput("vb_female_perc")



comparison_graph_individual_percentile_card <- makeCard("", highchartOutput("graph_individual_percentile"), size = 8)


comparison_male_percentile_vb <- uiOutput("vb_male_percentile")


comparison_female_percentile_vb <- uiOutput("vb_female_percentile")



### e. Cards for the Understand Page  -----------------------------------------------------



understand_instructions_card <- makeCard("Instructions", 
                                         div(
                                           "In this tab, you can understand where your impacts come from;", 
                                           
                                           br(), br(), 
                                           
                                           "First, you can check which major groups of food group and specific types of food drive your impacts. You can click on any part of the treemap graph to zoom in. The most relevant types of food are highlighted on the right", 
                                           
                                           br(), br(), 
                                           
                                           "Then, you can compare yourself to males and females from your age group, and check whether your difference in impacts comes from the dietary choices you make or the fact that you are eating more or less")
                                         
                                         , size = 8)




understand_select_card <- makeCard("Select an impact", div(understand_inputs), size = 4)


understand_graph_treemap_card <- makeCard("", highchartOutput("graph_treemap_specific_impacts", height = 550), size = 8)


understand_burden_one_vb <- uiOutput("vb_burden_specific_1")


understand_burden_two_vb <- uiOutput("vb_burden_specific_2")


understand_burden_three_vb <- uiOutput("vb_burden_specific_3")


understand_graph_waterfall_males_card <- makeCard("", highchartOutput("graph_waterfall_males", height = 400), size = 6)


understand_graph_waterfall_females_card <- makeCard("", highchartOutput("graph_waterfall_females", height = 400), size = 6)



### f. Cards for the Anthropometrics Page  -----------------------------------------------------


anthropometrics_instructions_card <- makeCard(
                                  
                                    "Instructions", 
                                    
                                    div(
                                      
                                      "In this tab, you can review the linkages between several anthropometrical measurements and your environmental impacts. The measurements include your height, weight, Body Mass Index and Base Metabolic Rate (estimated).", 
                                      
                                      br(), br(), 
                                      
                                      "Please select an impact on the right to begin the analysis.", 
                                      
                                      br(), br(), 
                                      
                                      "Each graph shows you the relationship between one type of measurement and your impacts. Males and females of your age group are shown for you to be able to contextualize your own situation. Regression lines by sex are also shown. If you are below the lines, it implies you are doing 'better' than expected for the given sex. You are shown as a yellow point on each graph."
                                      
                                      ), size = 8)



anthropometrics_select_card <- makeCard("Select an impact", anthropometrics_inputs, size = 4)


anthropometrics_graph_height_card <- makeCard("", highchartOutput("graph_height"), size = 6)


anthropometrics_graph_weight_card <- makeCard("", highchartOutput("graph_weight"), size = 6)


anthropometrics_graph_bmi_card <- makeCard("", highchartOutput("graph_bmi"), size = 6)


anthropometrics_graph_bmr_card <- makeCard("", highchartOutput("graph_bmr"), size = 6)



### g. Cards for the Download Page  -----------------------------------------------------


download_name_card <- makeCard("Customise your report", download_inputs,  size = 4)




## 3.4. Build individual pages  --------------------------------------


### a. Home Page  -----------------------------------------------------

home_page <- makePage(
  
    "Food Environmental Impacts Simulator",
    "Developed by Dismal Science",
    
    div(home_context_card, home_instructions_card, home_scope_card, home_methodology_card)
    
    )


### b. Profile / Settings Page  -----------------------------------------------------


profile_page <- makePage(
  
    "Your Profile",
    "Describe yourself and your food consumption habits",
    
    div(
      
        Stack(
            horizontal = TRUE,
            tokens = list(childrenGap = 10),
            
            profile_demographic_card, 
            
            profile_instructions_card),
        
        Stack(
            horizontal = TRUE,
            tokens = list(childrenGap = 10),
            
            profile_food_inputs_card, 
            
            profile_graph_food_individual_card,
            profile_graph_food_dem_card,
            
        ),
        
        Stack(
            horizontal = TRUE,
            tokens = list(childrenGap = 10),
            
            profile_beverage_inputs_card,
            
            profile_graph_beverage_individual_card,
            profile_graph_beverage_dem_card
            
        )
    )
    
    )


### c. Impacts Page  -----------------------------------------------------


impacts_page <- makePage(
  
    "Your Impacts",
    "Overview of the yearly environmental impacts derived from your diet",
    
    div(
        
        Stack( # Major impacts
          
            horizontal = TRUE,
            horizontalAlign = 'center',
            tokens = list(childrenGap = 10),
            
            impacts_environmental_footprint_score_vb,
            impacts_climate_change_vb,
            impacts_soil_use_vb,
            
            ),
        
        Stack( # Healh related impacts
          
            horizontal = TRUE,
            tokens = list(childrenGap = 10),
            
            impacts_ozone_depletion_vb,
            impacts_ozone_formation_vb,
            impacts_ion_radiation_vb,
            impacts_particles_vb,
            
            ),
        
        Stack( # Eutrophication
          
          horizontal = TRUE,
          tokens = list(childrenGap = 10),
          
          impacts_acidification_vb,
          impacts_terrestrial_eutrophisation_vb,
          impacts_fresh_water_eutrophisation_vb,
          impacts_marine_eutrophisation_vb,
          
          ),
        
        Stack( # Depletion and ecotox
          
          horizontal = TRUE,
          tokens = list(childrenGap = 10),
          
          impacts_fresh_water_ecotoxicity_vb,
          impacts_water_depletion_vb,
          impacts_energy_depletion_vb,
          impacts_mineral_depletion_vb,
          
          )
    
        )
    )




### d. Comparison Page  -----------------------------------------------------


compare_page <- makePage(
  
    "Comparative analysis",
    "Check out where you stand compared to men and women of your age group",
    
    div(Stack(
      
        horizontal = TRUE,
        tokens = list(childrenGap = 10),
        
        comparison_instructions_card, 
        
        comparison_select_card,            

    ),
    
    Stack(
      
      
        horizontal = TRUE,
        tokens = list(childrenGap = 10),
        
        comparison_graph_individual_vs_groups_card,
        
        
        Stack(
          
          horizontal = FALSE,
          
          comparison_male_perc_vb,
          
          comparison_female_perc_vb
        
        )
        
        ),
    
    
    Stack(
      
      horizontal = TRUE,
      tokens = list(childrenGap = 10),
      
      comparison_graph_individual_percentile_card,
      
      
      Stack(
        
        horizontal = FALSE,
        
        comparison_male_percentile_vb,
        
        comparison_female_percentile_vb
        
        )
      
      )
    
    )
    
    )



### d. Understand Page  -----------------------------------------------------



understand_page <- makePage(
  
  "Understand your impacts",
  "Analyse where your impacts come from for the different types of environmental damages",
  
  div(
    
    Stack(
    
    horizontal = TRUE,
    tokens = list(childrenGap = 10),
    
    understand_instructions_card,
    
    understand_select_card
    
    ), 
  
  Stack(
    
    horizontal = TRUE,
    tokens = list(childrenGap = 10),
    
    understand_graph_treemap_card,
    
    Stack(
      
      horizontal = FALSE,
      
      understand_burden_one_vb,
      
      understand_burden_two_vb,
      
      understand_burden_three_vb
      
      )
    
    ),
  
  Stack(
    
    horizontal = TRUE,
    tokens = list(childrenGap = 10),
    
    understand_graph_waterfall_males_card,
    
    understand_graph_waterfall_females_card
    
    )
  
  )
  
  )


### e. Anthropometrics Page  -----------------------------------------------------


anthropometrics_page <- makePage(
  
  "Your Body",
  
  "Analyse the linkages between your impacts and your body characteristics",
  
  div(
    Stack(
      
    horizontal = TRUE,
    tokens = list(childrenGap = 10),
    
    anthropometrics_instructions_card,
    
    anthropometrics_select_card
    
  ),
  
  Stack(
    
    horizontal = TRUE,
    tokens = list(childrenGap = 10),
    
    anthropometrics_graph_height_card,
    
    anthropometrics_graph_weight_card
    
  ),
  
  Stack(
    
    horizontal = TRUE,
    tokens = list(childrenGap = 10),
    
    anthropometrics_graph_bmi_card,
    
    anthropometrics_graph_bmr_card
    
    )
  
  )
  
  )








### f. Download Page  -----------------------------------------------------



download_page <- makePage(
  
  "Your Full Report",
  
  "Download a PDF file with your simulation",
  
  div(
    
    download_name_card

  )
)



## 3.5. Build navigation and layout  --------------------------------------

### a. Header  -----------------------------------------------------

header <- tagList(
  
  img(src = "logo/dismalsci.png", class = "logo"), # Logo
  
  div(Text(variant = "xLarge", "Food Env Impacts"), class = "title"), # App Title
  
  CommandBar(
    
    items = list(
      CommandBarItem("Contact the developer", "Mail", key = "emailMessage", href = "mailto:dismalscience@protonmail.com")
    ),
    
    farItems = list(
      CommandBarItem( "App v.1 / release June 2024", "Info", iconOnly = TRUE)
      
    ),
    
    style = list(width = "100%")))


### b. Navigation Bar  -----------------------------------------------------

navigation <- Nav(
  
  groups = list(
    
    list(links = list(
      
      list(name = 'Home', url = '#!/', key = 'home_page', icon = 'Home'),
      list(name = 'Your profile', url = '#!/profile', key = 'profile_page', icon = 'ProfileSearch'),
      list(name = 'Your Impacts', url = '#!/impacts', key = 'impacts_page', icon = 'StackedLineChart'),
      list(name = 'Comparison with Others', url = '#!/compare', key = 'compare_page', icon = 'Compare'),
      list(name = 'Your Body', url = '#!/anthropometrics', key = 'anthropometrics_page', icon = 'ReminderPerson'),
      list(name = 'Causes of your Impacts', url = '#!/understand', key = 'understand_page', icon = 'ReceiptTentative'),
      list(name = 'Your Full Report', url = '#!/download', key = 'download_page', icon = 'DownloadDocument')
      
    ))
    
  ),
  
  initialSelectedKey = 'home',
  
  styles = list(
    root = list(
      height = '100%',
      boxSizing = 'border-box',
      overflowY = 'auto'
    )
    
  )
)



### c. Footer  -----------------------------------------------------


footer <- Stack(
  
  horizontal = TRUE,
  horizontalAlign = 'space-between',
  tokens = list(childrenGap = 20),
  
  Text(variant = "medium", "Designed by Dismal Science", block=TRUE),
  Text(variant = "medium", nowrap = FALSE, "Estimates based on French INCA 3 and Agribalyse data"),
  
  )


### d. Overarching Layout of the App  -----------------------------------------------------


layout <- function(mainUI){
  
  div(class = "grid-container",
      
      div(class = "header", header),
      div(class = "sidenav", navigation),
      div(class = "main", mainUI),
      div(class = "footer", footer)
      
  )
  
  }



## 3.6. Launch the UI  --------------------------------------



### a. Shiny Router  -----------------------------------------------------



router <- router_ui( # Router, should point to the different pages developed in section 3.4
  
    route("/", home_page),
    route("profile", profile_page),
    route("impacts", impacts_page),
    route("compare", compare_page),
    route("anthropometrics", anthropometrics_page),
    route("understand", understand_page),
    route("download", download_page)
    
)


### b. UI Page  -----------------------------------------------------



ui <- fluentPage( # Consolidate the entire user interface, define the css style 
  useShinyjs(),
  div( # hidden download button
    style = "visibility: hidden;",
    downloadButton("download", label = "")
  ),
  uiOutput("messageBarUI"),
    layout(router),
    tags$head(
        tags$link(href = "style.css", rel = "stylesheet", type = "text/css"), # use the CSS style
        
    ))





# 4. Helper functions for data processing -------------------------------------------------------


## 4.1. Demographic functions  --------------------------------------


### a. Definition of demographic group -----------------------------------------------------


find_age_group <- function(age) { # function taking the age of the individual in input and returning the corresponding age group
  
  round_age <- round(age, digits = 0)
  
  if(round_age < 18) {
    
    paste0("0-17")
    
  } else if (17 < round_age & round_age < 45) {
    
    paste0("18-44")
    
  } else if (44 < round_age & round_age < 65) {
    
    paste0("45-64")
    
  } else if (64 < round_age & round_age < 80) {
    
    paste0("65-79")
    
  } else if (round_age > 79) {
    
    paste0("80+")
    
  }
  
}


find_dem_group <- function(sex, age) { # function taking the age and sex of the individual in input and returning the corresponding demographic group
  
  paste0(ifelse(sex == "male", "Males ", "Females "), find_age_group(age))
  
}





### b. BMI and BMR -----------------------------------------------------


compute_bmi <- function(weight, height) { # function to compute the BMI based on individual weight and height
  
  round(weight / (height / 100)^2, digits = 2)
  
}



compute_bmr  <- function(sex, weight, height, age) { # function to estimate the basal metabolic rate of the person. Using Revised Harris-Benedict Equation from https://www.calculator.net/bmr-calculator.html
  
  # requires sex (formatted as character string "female" or "male"), weight in kg, height in cm, and age in years
  
  if(sex == "male") {
    
    round((13.397*weight) + (4.799*height) - (5.677*age) +88.362, digits = 0)
    
  } else if (sex == "female") {
    
    round((9.247*weight) + (3.098*height) - (4.33*age) +447.593, digits = 0)
    
    
  }
  
}



## 4.2. Other processing functions  --------------------------------------


rescale_values <- function(slider_values) { # function taking a vector as input and rescaling it so that it sums up to 14 (used for the typology of food eater)
  
  # Ensure the sum of the input values is not zero
  if (sum(slider_values) == 0) {
    return(rep(0, length(slider_values)))
  }
  
  # Normalize the values to sum up to 14
  total <- sum(slider_values)
  normalized_values <- slider_values / total * 14
  
  # Get integer parts of the normalized values
  integer_parts <- floor(normalized_values)
  integer_sum <- sum(integer_parts)
  
  # Calculate the remainders
  remainders <- normalized_values - integer_parts
  
  # Calculate the difference needed to reach 14
  difference <- 14 - integer_sum
  
  # Order the remainders in descending order
  remainder_order <- order(remainders, decreasing = TRUE)
  
  # Distribute the difference
  for (i in seq_len(difference)) {
    integer_parts[remainder_order[i]] <- integer_parts[remainder_order[i]] + 1
  }
  
  return(integer_parts)
}



convert_to_glass  <- function(volume, timeframe = "year", liquid = "water") { # takes a volume in L and convert it to number of glasses per day. Optional arguments for the timeframe of the provided volume (e.g., year or day) and the type of glasses (default: water)
  
  # Volume of glasses in L
  water_glass <- 0.25
  wine_glass <- 0.15
  beer_glass <- 0.5 # (pint)
  tea_glass <- 0.35
  coffee_glass <- 0.2
  
  # Attribute selected volume
  
  chosen_glass <- get(paste0(liquid, "_glass"))
  
  # Standardisation for timeframe
  
  year_length <- 365.25
  month_length <- 365.25 / 12
  week_length <- 7
  day_length <- 1
  
  length <- get(paste0(timeframe, "_length"))
  
  # Computation of the number of glasses
  
  round(volume / length / chosen_glass, digits = 2)
  
}



get_impact_type <- function(selected_impact) { # get the type of impact
  
  list_impacts[as.numeric(selected_impact)]
  
}


get_impact_name <- function(selected_impact) { # get the name of impact
  
  list_names_impacts[as.numeric(selected_impact)]
  
}


get_impact_unit <- function(selected_impact) { # get the unit of impact
  
  list_units_impacts[as.numeric(selected_impact)]
  
}




get_percentile  <- function(data, value) { # function to estimate the percentile of a given value within a distribution of data
  
  100 * ecdf(unlist(data))(value)
  
}





# 5. Server logic, including outputs -------------------------------------------------------


server <- function(input, output, session) {
    
  
  ## 5.1. Sever Router and UI  --------------------------------------
  
    
    router_server() # router for navigation purposes
  
  
# Text for the sliders in the UI
  
  output$red_meat_text <- renderText({
    
    paste0("Red meat - Basis of ", ifelse( total_meals() == 0, 0,  round(100 * (rescale_values(c(input$red_meat_meals, input$white_meat_meals, input$fish_meals, input$vegetarian_meals, input$vegan_meals))[1]) / 14 , digits = 0)), "% of your meals")
    
  })
  
  output$white_meat_text <- renderText({
    
    paste0("White meat - Basis of ", ifelse(total_meals() == 0, 0,  round(100 * (rescale_values(c(input$red_meat_meals, input$white_meat_meals, input$fish_meals, input$vegetarian_meals, input$vegan_meals))[2]) / 14 , digits = 0)), "% of your meals")
    
  })
  
  output$fish_text <- renderText({
    
    paste0("Fish and Seafood - Basis of ", ifelse(total_meals() == 0, 0,  round(100 * (rescale_values(c(input$red_meat_meals, input$white_meat_meals, input$fish_meals, input$vegetarian_meals, input$vegan_meals))[3]) / 14  , digits = 0)), "% of your meals")
    
  })
  
  output$vegetarian_text <- renderText({
    
    paste0("Vegetarian dishes - Basis of ", ifelse(total_meals() == 0, 0,  round(100 * (rescale_values(c(input$red_meat_meals, input$white_meat_meals, input$fish_meals, input$vegetarian_meals, input$vegan_meals))[4]) / 14  , digits = 0)), "% of your meals")
    
  })
  
  output$vegan_text <- renderText({
    
    paste0("Vegan dishes - Basis of ", ifelse(total_meals() == 0, 0,  round(100 * (rescale_values(c(input$red_meat_meals, input$white_meat_meals, input$fish_meals, input$vegetarian_meals, input$vegan_meals))[5]) / 14  , digits = 0)), "% of your meals")
    
  })

  
  
  output$water_text <- renderText({
    
    paste0("<p>",
           ifelse( beverage_proportions() == 0, 0,  
                   round( beverage_quantity_individual() %>% filter(specific_group == "Water") %>% select(share_conso) , digits = 0)), "% of your beverages", 
           
           ifelse( beverage_proportions() == 0, "", 
                   paste0(", i.e., about ", convert_to_glass(beverage_total_yearly_quantity() * beverage_quantity_individual() %>% filter(specific_group == "Water") %>% select(share_conso) / 100, "year", "water") ,  " glasses per day" ) ), "</p>" )
    
  })
  
  
  
  output$alcohol_text <- renderText({
    
    paste0("<p>",
           ifelse( beverage_proportions() == 0, 0,  
                   round( beverage_quantity_individual() %>% filter(specific_group == "Alcoholic beverages") %>% select(share_conso) , digits = 0)), "% of your beverages", 
           
           ifelse( beverage_proportions() == 0, "", 
                   paste0(", i.e., about ", convert_to_glass(beverage_total_yearly_quantity() * beverage_quantity_individual() %>% filter(specific_group == "Alcoholic beverages") %>% select(share_conso) / 100, "year", "wine") ,  " glasses of wine per day" ) ), "</p>" )
    
  })
  
  
  output$other_drink_text <- renderText({
    
    paste0("<p>",
           ifelse( beverage_proportions() == 0, 0,  
                   round( beverage_quantity_individual() %>% filter(specific_group == "Non alcoholic beverages") %>% select(share_conso) , digits = 0)), "% of your beverages", 
           
           ifelse( beverage_proportions() == 0, "", 
                   paste0(", i.e., about ", convert_to_glass(beverage_total_yearly_quantity() * beverage_quantity_individual() %>% filter(specific_group == "Non alcoholic beverages") %>% select(share_conso) / 100, "year", "water") ,  " glasses per day" ) ), "</p>" )
    
  })
  
  
  output$coffee_text <- renderText({
    
    paste0("<p>",
           ifelse( beverage_proportions() == 0, 0,  
                   round( beverage_quantity_individual() %>% filter(specific_group == "Coffee") %>% select(share_conso) , digits = 0)), "% of your beverages", 
           
           ifelse( beverage_proportions() == 0, "", 
                   paste0(", i.e., about ", convert_to_glass(beverage_total_yearly_quantity() * beverage_quantity_individual() %>% filter(specific_group == "Coffee") %>% select(share_conso) / 100, "year", "coffee") ,  " coffee cups per day" ) ), "</p>" )
    
  })
  
  
  output$tea_text <- renderText({
    
    paste0("<p>",
           ifelse( beverage_proportions() == 0, 0,  
                   round( beverage_quantity_individual() %>% filter(specific_group == "Tea") %>% select(share_conso) , digits = 0)), "% of your beverages", 
           
           ifelse( beverage_proportions() == 0, "", 
                   paste0(", i.e., about ", convert_to_glass(beverage_total_yearly_quantity() * beverage_quantity_individual() %>% filter(specific_group == "Tea") %>% select(share_conso) / 100, "year", "tea") ,  " tea cups per day" ) ), "</p>" )
    
  })
  
  
  
  
  message_profile_inputs <- reactive({ # dynamic message for wrong selection of inputs
    
    if (total_meals() == 0 & beverage_proportions() == 0) {
     
      paste0("Please select some food AND beverages in your Profile")
       
    } else if (total_meals() > 0 & beverage_proportions() == 0) {
      
      paste0("Please select some beverages in your Profile")
      
    } else if (total_meals() == 0 & beverage_proportions() > 0) {
      
      paste0("Please select some food in your Profile")
      
    }
    
  })
  
  

  # Generate the message bar UI if total_meals is  equal to 0
  
  output$messageBarUI <- renderUI({
    
    if (total_meals() == 0 | beverage_proportions() == 0) {
      
      MessageBar(
        
        message_profile_inputs()

      )
      
    }
  })
  
  


  
  ## 5.2. Dynamic variables  --------------------------------------
  
  
  ### a. Individual demographic variables  -----------------------------------------------------
  
  
  age_group_individual <- reactive({ # age group of the user
    
    find_age_group(input$age)
    
  })
  
  
  dem_profile_individual <- reactive({ # demographic profile of the user
    
    find_dem_group(input$sex, input$age)
    
  })
  
  
  bmi_individual <- reactive({ # body mass index of the person
    
    compute_bmi(input$weight, input$height)

  })
  
  
  bmr_individual  <- reactive({ # estimated base metabolic rate of the person.
    
    compute_bmr(input$sex, input$weight, input$height, input$age)
    
  })
  
  
    
  ### b. Consumption variables  -----------------------------------------------------
  
    
  #### Overarching  -----------------------------------------------------
  
  
    total_meals <- reactive({ # number of meals provided by the user
      
      sum(
        input$red_meat_meals, 
          input$white_meat_meals,
          input$fish_meals,
          input$vegetarian_meals,
          input$vegan_meals
          )
        
    })
    
  
  
  #### Food consumption  -----------------------------------------------------
  
  
  food_quantity_actual <- reactive({ # actual value for food quantity (based on expert mode or not). Output in g / day
    
    ifelse(input$food_expert == 1, input$food_quantity, input$simple_food_quantity)
    
  })
  
  
  food_typology <- reactive({ # typology of eater by weekly meal composition
  
    
    paste0(rescale_values(c(input$red_meat_meals, input$white_meat_meals, input$fish_meals, input$vegetarian_meals, input$vegan_meals)),c("R","W", "F", "VT", "VN"), collapse = "")
    
  })
  
  
  debounced_typology <- debounce(food_typology, 500) # deboucing the typology to avoid too many updates 
  
  
  
  #### Beverage consumption  -----------------------------------------------------
  
  
  beverage_quantity_actual <- reactive({ # actual value for daily beverage quantity (based on expert mode or not). Output in g / day
    
    ifelse(input$beverage_expert == 1, input$beverage_quantity, input$simple_beverage_quantity)
    
  })
  
  
  beverage_proportions <- reactive({ # sum of proportions inputed by the user, used to rescale
    
    sum(input$water, input$tea, input$coffee, input$alcohol, input$other_drink)
    
  })
  

  beverage_typology <- reactive({ # typology of drinkers
    
    paste0(input$water, "W", input$tea, "T", input$coffee, "C", input$alcohol, "A", input$other_drink, "O")
    
  })
  
  
    
    ## 5.3. Data processing  --------------------------------------
  
  
  
  #### Food data  -----------------------------------------------------
  
  
  food_quantity_individual <- reactive({ # Dataframe with the yearly food quantities consumed by the individual, based on his/her inputs for consumption
    
    if (total_meals() > 0) {
      
      
      food_composition %>% 
        
        filter(id_typology == debounced_typology()) %>% # filter based on typology of eater
        mutate_at(vars(-red_meat, -white_meat, -fish, -vegets, -vegan, -id_typology), 
                  ~ . * 365.25 * food_quantity_actual() / 1000) %>%  # convert values to yearly kg
        
        select(-red_meat, -white_meat, -fish, -vegets, -vegan) %>% 
        pivot_longer(!id_typology, names_to = "specific_group", values_to = "conso_yearly_kg") %>% 
        
        mutate(specific_group = str_trim(str_replace_all(specific_group, "[.]", " "))) %>% # clean names of specific groups
        
        mutate(share_conso = conso_yearly_kg / sum(conso_yearly_kg) * 100)   # estimate yearly consumption in kg
      
    }
    
 
    })
  
  
  
  food_total_quantity <- reactive({ # Total yearly food quantities consumed by the individual, in kg
    
    round(food_quantity_actual() * 365.25 / 1000, digits = 0)
    
  })
  
  
  
  food_impacts_individual <- reactive({ # Dataframe with the yearly impacts from food consumption, based on the estimated quantities and the corresponding intensities. Breakdown by specific groups of food
    
    
    if (total_meals() > 0) {
      
      food_intensities %>% # start with intensities by specific food group
        
        filter(id_typology == debounced_typology()) %>% 
        select(-red_meat, -white_meat, -fish, -vegets, -vegan, -total) %>% 
        
        pivot_longer(!c(id_typology, type_impacts), names_to = "specific_group", values_to = "intensity") %>% 
        mutate(specific_group = str_trim(str_replace_all(specific_group, "[.]", " "))) %>% 
        
        left_join(food_quantity_individual()) %>% # join with quantities
        mutate(yearly_impacts = intensity * conso_yearly_kg)  # etimate impacts
      
      
    }
    

  })
  
  
  
  #### Beverage data  -----------------------------------------------------
  
    beverage_quantity_individual <- reactive({ # Dataframe with the yearly beverage quantities consumed by the individual, based on his/her inputs for consumption
      
      
      if (beverage_proportions() > 0) {
        
        beverage_composition %>% 
          
          filter(id_typology == beverage_typology()) %>% # typology of beverage drinker
          
          mutate_at(vars(-water, -tea, -coffee, -alcohol, -other, -id_typology), 
                    ~ . * 365.25 * beverage_quantity_actual() / 1000) %>% # convert to yearly consumption in kg / L. 
          
          select(-water, -tea, -coffee, -alcohol, -other) %>% 
          
          pivot_longer(!id_typology, names_to = "specific_group", values_to = "conso_yearly_kg") %>% 
          
          mutate(specific_group = str_trim(str_replace_all(specific_group, "[.]", " "))) %>% # clean names specific groups
          
          mutate(share_conso = conso_yearly_kg / sum(conso_yearly_kg) * 100)  # estimate yearly consumption in kg and related shares
        
        
        
      }
        
       
        
    })
    
    
    beverage_total_yearly_quantity <- reactive({ # Total yearly beverage quantities consumed by the individual, in L. / kg
        
      round(beverage_quantity_actual() * 365.25 / 1000, digits = 0)
        
    })
    
    
    
    beverage_impacts_individual <- reactive({ # Dataframe with the yearly impacts from beverage consumption, based on the estimated quantities and the corresponding intensities. Breakdown by specific groups of beverages
      
      if (beverage_proportions() > 0) {
        
      
        beverage_intensities %>% 
          
          filter(id_typology == beverage_typology()) %>% # filter based on typology of drinker
          
          select(-water, -tea, -coffee, -alcohol, -other) %>% 
          pivot_longer(!c(id_typology, type_impacts), names_to = "specific_group", values_to = "intensity") %>% 
          mutate(specific_group = str_trim(str_replace_all(specific_group, "[.]", " "))) %>% 
          
          left_join(beverage_quantity_individual()) %>% # joining with estimates of quantities
          mutate(yearly_impacts = intensity * conso_yearly_kg) # estimating yearly impacts
        
      }

    })
    
    
    #### Overarching consumption data  -----------------------------------------------------
    
    
    gross_total_yearly_quantity <- reactive({ # Total yearly quantity of beverage AND food combined (in kg)
      
      beverage_total_yearly_quantity() + food_total_quantity()

    })
    
    
    
    #### Overarching impacts data  -----------------------------------------------------
    
    
    # Logic to reduce frequent updates reducing performance
    
    trigger_impacts_processing <- reactiveVal(FALSE)
    

    # Track the last time an input was changed
    lastInputChange <- reactiveVal(Sys.time())
    
    # Observe changes to all inputs
    observeEvent({
      input$red_meat_meals
      input$white_meat_meals
      input$fish_meals
      input$vegetarian_meals
      input$vegan_meals
      input$water
      input$alcohol
      input$other_drink
      input$tea
      input$coffee
    }, {
      lastInputChange(Sys.time())
      trigger_impacts_processing(FALSE)  # Reset the trigger when any input changes
      
    })
    
    # Reactive timer that checks every 2 seconds
    timer <- reactiveTimer(2000)
    
    observe({
      timer()
      
      # Check if more than 1 seconds have passed since the last input change
      if (difftime(Sys.time(), lastInputChange(), units = "secs") > 2) {
        trigger_impacts_processing(TRUE)
      } else {
        trigger_impacts_processing(FALSE)
      }
    })
    
    
    all_impacts_individual <- reactive({ # Dataframe with the full estimates of impacts by specific groups (both food and beverages included)
        
      req(trigger_impacts_processing())
      
      
      if (total_meals() > 0 & beverage_proportions() > 0) {
        
      
        
        left_join(
          
          rbind(beverage_impacts_individual(), food_impacts_individual()) %>% mutate(specific_group = str_trim(str_replace_all(specific_group, "[.]", " "))), # combining the dataframes for beverage and food impacts
          match_macro_specific) %>% # combining with the references to macro_groups
          
          select(-share_conso) %>% # remove shares of consumptions as they were computed for beverages and food separately 
          
          group_by(type_impacts) %>% # Compute shares of impacts by specific group
          mutate(share_impacts = round(yearly_impacts / sum(yearly_impacts) * 100, digits = 0)) %>%
          ungroup()
        
        
        
      }
      
     
        
    })
    
    
    
    summary_impacts_individual <- reactive({ # Dataframe with aggregated yearly impacts for the individual (i.e., without breakdown by specific group of food) and all demographic groups
      
      if (total_meals() > 0 & beverage_proportions() > 0) {
        
        rbind(
          all_impacts_individual() %>% group_by(type_impacts) %>% 
            summarise(yearly_impacts = sum(yearly_impacts)) %>% 
            mutate(demographic_group = "You"), 
          
          yearly_impacts_demo_groups) # yearly impacts of other demographic groups
        
      }
    
        
    })
    
    
    
    #### Data for comparison between individual and males/females of the same age group  -----------------------------------------------------
    
    
    comparison_individual_impacts_age_groups <- reactive({ # Dataframe comparing individual impact for all the types of impacts with males and females of the same age group
        
      if (total_meals() > 0 & beverage_proportions() > 0) {
        
        summary_impacts_individual() %>% 
        
        filter(demographic_group %in% c("You", 
                                        paste0("Males ", age_group_individual() ), 
                                        paste0("Females ", age_group_individual() ) 
                                        
                                        )
               
               ) 
      }
        
      })
    
    
    
    comparison_individual_impacts_age_groups_percent <- reactive({ # Dataframe with summary stats with percent difference between you and the males/females groups
      
      if (total_meals() > 0 & beverage_proportions() > 0) {
        
      
      comparison_individual_impacts_age_groups() %>%
        
        group_by(demographic_group, type_impacts) %>%
        
        summarize(yearly_impacts = mean(yearly_impacts), .groups = 'drop') %>%
        group_by(type_impacts) %>%
        
        mutate(
          change_vs_males = 100 * (yearly_impacts - yearly_impacts[demographic_group == paste0("Males ", age_group_individual())]) / yearly_impacts[demographic_group == paste0("Males ", age_group_individual())],
          
          change_vs_females = 100 * (yearly_impacts - yearly_impacts[demographic_group == paste0("Females ", age_group_individual())]) / yearly_impacts[demographic_group == paste0("Females ", age_group_individual())]
        ) %>%
        
        ungroup() %>%
        
        mutate(mean_change = (change_vs_females + change_vs_males) / 2) %>% 
        
        
        filter(demographic_group == "You")
        
      }
      
    })
    
    
    
    #### Data for decomposition of individual impacts' against demographic groups of comparison (i.e., waterfall charts data)  -----------------------------------------------------
    
    
    individual_waterfall_data <- reactive({ # Dataframe for the individual with both yearly quantities and impacts, in wide format
      
      if (total_meals() > 0 & beverage_proportions() > 0) {
        
      
        all_impacts_individual() %>% 
        
        group_by(type_impacts) %>% 
        summarise(yearly_impacts = sum(yearly_impacts)) %>% 

        pivot_wider(names_from = type_impacts, values_from = yearly_impacts) %>% 
        
        mutate("conso_yearly_kg" = gross_total_yearly_quantity() ) %>% 
        
        select(get_impact_type(input$selected_impact_understand), conso_yearly_kg)
      
      }
      
    })
    
    
    
    individual_waterfall_males <- reactive({ # Dataframe with the data to create the waterfall chart comparing the individual against males of the same age group 
      
      if (total_meals() > 0 & beverage_proportions() > 0) {
        
      
      target_comp_dem_group <- paste0("Males ", age_group_individual() ) # name of the demographic group to compare individual against (i.e., reference group)
      impact_name <- get_impact_name(input$selected_impact_understand) # name of the selected impact (understand page)
      impact_type <- get_impact_type(input$selected_impact_understand) # type of the selected impact (understand page)
      
      conso_yearly_kg <- "conso_yearly_kg"
      
      data_dem_group <- aggregated_yearly_vars_dem_groups %>% # data for the targeted demographic group
        filter(demographic_group == target_comp_dem_group) %>%
        select(all_of(impact_type), conso_yearly_kg) # select the considered impact and the quantity in the table 
    
      individual_data <- individual_waterfall_data()
      
      
      impact_dem <- as.numeric(data_dem_group[[impact_type]])
      impact_individual <- as.numeric(individual_data[[impact_type]])
      
      quant_dem <- as.numeric(data_dem_group[[conso_yearly_kg]])
      quant_individual <-  as.numeric(individual_data[[conso_yearly_kg]])
      
      intens_dem <- impact_dem / quant_dem
      intens_individual <- impact_individual / quant_individual
      
      data.frame(
        "Variable" = c(paste0(target_comp_dem_group, " - ", impact_name),
                       "Dietary choices", 
                       "Consumed quantities", 
                       "Interactions", 
                       paste0("You - ", impact_name)), 
                 
                 "Value" = c(impact_dem, 
                             
                             ( intens_individual - intens_dem ) * quant_dem, # Difference due to dietary choices 
                             
                             (quant_individual - quant_dem) * intens_dem, # Difference due to consumed quantities
                             
                             (intens_individual - intens_dem) * (quant_individual - quant_dem),  # Interactions
                             
                             - impact_individual), # your environmental impact
                 
                 "Type" = c("Male Impact", "Causes of differences", "Causes of differences", "Causes of differences", "Your Impact")) %>% 
        
        mutate(shade = colorize(Type, c(colors_intermediate_waterfall[1], colors_sexyou[2],colors_sexyou[3])))
      
      }
      
    })
    
    
    
    individual_waterfall_females <- reactive({ # data table with the graph ready information for comparison of you vs females of your age group
      
      if (total_meals() > 0 & beverage_proportions() > 0) {
        
      
      
      target_comp_dem_group <- paste0("Females ", age_group_individual() ) # name of the demographic group to compare individual against (i.e., reference group)
      impact_name <- get_impact_name(input$selected_impact_understand) # name of the selected impact (understand page)
      impact_type <- get_impact_type(input$selected_impact_understand) # type of the selected impact (understand page)
      
      conso_yearly_kg <- "conso_yearly_kg"
      
      data_dem_group <- aggregated_yearly_vars_dem_groups %>% # data for the targeted demographic group
        filter(demographic_group == target_comp_dem_group) %>%
        select(all_of(impact_type), conso_yearly_kg) # select the considered impact and the quantity in the table 
      
      individual_data <- individual_waterfall_data()
      
      
      impact_dem <- as.numeric(data_dem_group[[impact_type]])
      impact_individual <- as.numeric(individual_data[[impact_type]])
      
      quant_dem <- as.numeric(data_dem_group[[conso_yearly_kg]])
      quant_individual <-  as.numeric(individual_data[[conso_yearly_kg]])
      
      intens_dem <- impact_dem / quant_dem
      intens_individual <- impact_individual / quant_individual
      
      data.frame(
        "Variable" = c(paste0(target_comp_dem_group, " - ", impact_name),
                       "Dietary choices", 
                       "Consumed quantities", 
                       "Interactions", 
                       paste0("You - ", impact_name)), 
        
        "Value" = c(impact_dem, 
                    
                    ( intens_individual - intens_dem ) * quant_dem, # Difference due to dietary choices 
                    
                    (quant_individual - quant_dem) * intens_dem, # Difference due to consumed quantities
                    
                    (intens_individual - intens_dem) * (quant_individual - quant_dem),  # Interactions
                    
                    - impact_individual), # your environmental impact
        
        "Type" = c("Male Impact", "Causes of differences", "Causes of differences", "Causes of differences", "Your Impact")) %>% 
        
        mutate(shade = colorize(Type, c(colors_intermediate_waterfall[1], colors_sexyou[1],colors_sexyou[3])))
      
      }
      
    })
    
    
  
    
    #### Data for the anthropometrics analysis  -----------------------------------------------------
    
    
    anthropo_dem_groups <- reactive({ # Dataframe with the anthropometrics data for the considered demographic groups. Ready to be used for the graphs
    
    anthropometrics %>% 
        
        select(-noind, -sex, -age) %>% 
        
        filter(demographic_group %in% c(paste0("Females ", age_group_individual() ), paste0("Males ", age_group_individual() )
                                        ))  
      
      })
    
    
    
    
    #### Selected individual impact value for each tab  -----------------------------------------------------
    
    
    individual_selected_impact_value_compare <- reactive({ # value of the selected yearly impact for the individual (Compare page)
        
      if (total_meals() > 0 & beverage_proportions() > 0) {
        
      
        (summary_impacts_individual() %>% 
           filter(type_impacts == get_impact_type(input$selected_impact_compare)) %>% 
           filter(demographic_group == "You"))$yearly_impacts
        
      }
    })
    
    
    
    individual_selected_impact_value_understand <- reactive({ # value of the selected yearly impact for the individual (Understand page)
      
      if (total_meals() > 0 & beverage_proportions() > 0) {
        
      
      (summary_impacts_individual() %>% 
         filter(type_impacts == get_impact_type(input$selected_impact_understand)) %>% 
         filter(demographic_group == "You"))$yearly_impacts
        
      }
      
    })
    
    
    individual_selected_impact_value_anthropometrics <- reactive({ # value of the selected yearly impact for the individual (Anthropometrics page)
      
      if (total_meals() > 0 & beverage_proportions() > 0) {
        
      
      (summary_impacts_individual() %>% 
         filter(type_impacts == get_impact_type(input$selected_impact_anthropometrics)) %>% 
         filter(demographic_group == "You"))$yearly_impacts
        
      }
      
    })
    
    
    
    #### Data on the most impactful types of food (Understand Page)  -----------------------------------------------------
    
    
    individual_most_burden_specific_groups_impacts <- reactive({ # Ranking of specific groups by level of damage caused by the individual, for a given type of impact (Understand page)
      
      if (total_meals() > 0 & beverage_proportions() > 0) {
        
      
      all_impacts_individual() %>% 
        
        filter(type_impacts == get_impact_type(input$selected_impact_understand)) %>% 
        
        mutate(share_impacts = round(yearly_impacts / sum(yearly_impacts) * 100, digits = 0)) %>% 
        
        arrange(desc(share_impacts)) # most burdensome groups sit at the top of the dataframe (NB: specific groups, not macro ones )
      
      }
        
    })
    
    
  
   

    ## 5.4. Graphs  --------------------------------------
    
    
    
    ### a. Profile graphs  -----------------------------------------------------
    
    
    ####  Food consumption graphs -----------------------------------------------------
    
    
    output$graph_food_individual <- renderHighchart({ # Graph about the estimated beverage yearly consumption of the individual  individual
      
      
      if(total_meals() > 0) {
        
        hchart(left_join(food_quantity_individual(), match_macro_specific) %>% 
                 filter(macro_group != "Beverages") %>% 
                 group_by(macro_group) %>% 
                 summarise(share_conso = sum(share_conso)), 
               
               "pie", hcaes(x = macro_group, y = round(share_conso, digits = 0)), 
               innerSize="70%", size = "80%", name = "Yearly consumption") %>% 
          
          hc_title(text = paste0("Your estimated yearly food consumption: ", food_total_quantity(), " kg"), style = list(useHTML = TRUE, fontSize = title_size)) %>% 
          
          hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), formatter = JS("function(){ return (this.point.name + ' : ' + this.point.y + ' %' )}")) %>%
          
          hc_add_theme(chosen_theme) %>% 
          hc_colors(colors_macrofood[2:12]) %>% 
          hc_plotOptions(pie =list(dataLabels = list(enabled = TRUE, style = list(useHTML = TRUE, fontSize = axis_size)))) 
        
        
      } else { # for cases where the number of meals is wrong
        
        hchart(data.frame("macro_group" = "Error", "share_conso" = NA), "area",             
               hcaes(x = macro_group, y = share_conso)) %>%               
          
          hc_title(text = paste0("You MUST select some food in your PROFILE"), 
                   style = list(useHTML = TRUE, color = "red", fontWeight = "bold",  fontSize = title_size)) %>%               
          
          hc_xAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size))) %>%       
          hc_xAxis(labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>%               
          hc_yAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size))) %>%       
          hc_yAxis(labels = list(format = paste0("{value} ", get_impact_unit(input$selected_impact_compare)), style = list(useHTML = TRUE, fontSize = axis_size))) %>%              
          
          hc_add_theme(chosen_theme)          
        
      }
      
    })
    
    
    output$graph_food_dem <- renderHighchart({ # Graph about the average food yearly consumption of the demographic group of the individual
      
      hchart(
        yearly_conso_demo_groups %>% 
          filter(macro_group != "Beverages") %>% 
          filter(demographic_group == dem_profile_individual()) %>% 
          group_by(macro_group) %>% 
          summarise(share_conso = sum(share_conso)), 
        
        "pie", hcaes(x = macro_group, y = round(share_conso, digits = 0)), 
             innerSize="70%", size = "80%", name = "Yearly consumption") %>% 
        
        hc_title(text = paste0(dem_profile_individual(), " average yearly food consumption: ", round( sum( (yearly_conso_demo_groups %>% filter(macro_group != "Beverages") %>% filter(demographic_group == dem_profile_individual() ) )$value ), digits = 0), " kg"), 
                 style = list(useHTML = TRUE, fontSize = title_size)) %>% 
        
        hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), formatter = JS("function(){ return (this.point.name + ' : ' + this.point.y + ' %' )}")) %>%
        
        hc_add_theme(chosen_theme) %>% 
        hc_colors(colors_macrofood[2:12]) %>% 
        hc_plotOptions(pie =list(dataLabels = list(enabled = TRUE, style = list(useHTML = TRUE, fontSize = axis_size))))
      
    })
    
    
    ####  Beverage consumption graphs -----------------------------------------------------
    

    output$graph_beverage_individual <- renderHighchart({ # Graph about the estimated beverage yearly consumption of the individual 
        
        
        if(beverage_proportions() > 0) {
            
        
        hchart(beverage_quantity_individual(), "pie", 
               hcaes(x = specific_group, y = round(share_conso, digits = 0)), 
               innerSize="70%", size = "80%", name = "Yearly consumption") %>% 
            
            hc_title(text = paste0("Your estimated yearly beverage consumption: ", beverage_total_yearly_quantity(), " L<br><br>(about ", convert_to_glass(beverage_total_yearly_quantity(), "year", "water")  , " glasses per day)"), style = list(useHTML = TRUE, fontSize = title_size)) %>% 
            
            hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), formatter = JS("function(){ return (this.point.name + ' : ' + this.point.y + ' %' )}")) %>%
            
            hc_add_theme(chosen_theme) %>% 
            hc_colors(colors_beverages) %>% 
            
            hc_plotOptions(pie =list(dataLabels = list(enabled = TRUE, style = list(useHTML = TRUE, fontSize = axis_size))))
            
          
        } else { # for cases where the number of meals is wrong
            
            
          hchart(data.frame("macro_group" = "Error", "share_conso" = NA), "area",             
                 hcaes(x = macro_group, y = share_conso)) %>%               
            
            hc_title(text = paste0("You MUST select some beverages in your PROFILE"), 
                     style = list(useHTML = TRUE, color = "red", fontWeight = "bold",  fontSize = title_size)) %>%               
            
            hc_xAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size))) %>%       
            hc_xAxis(labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>%               
            hc_yAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size))) %>%       
            hc_yAxis(labels = list(format = paste0("{value} ", get_impact_unit(input$selected_impact_compare)), style = list(useHTML = TRUE, fontSize = axis_size))) %>%              
            
            hc_add_theme(chosen_theme)                  
          
          
        }
        
    })
    
    
    
    output$graph_beverage_dem <- renderHighchart({ # Graph about the average beverage yearly consumption of the demographic group of the individual
        
        hchart(yearly_conso_demo_groups %>% 
                 
                 filter(macro_group == "Beverages") %>% 
                 filter(demographic_group == dem_profile_individual()), 
               
               "pie", hcaes(x = specific_group, y = round(share_conso, digits = 0)), 
               innerSize="70%", size = "80%", name = "Yearly consumption") %>% 
            
            hc_title(text = paste0(dem_profile_individual(), " average yearly beverage consumption: ", 
                                   round(sum((yearly_conso_demo_groups %>% 
                                                filter(macro_group == "Beverages") %>% 
                                                filter(demographic_group == dem_profile_individual()))$value ), digits = 0), 
                                   " L<br><br>(about ", convert_to_glass( sum((yearly_conso_demo_groups %>% filter(macro_group == "Beverages") %>% filter(demographic_group == dem_profile_individual()))$value ), "year", "water"), " glasses per day)"), 
                     style = list(useHTML = TRUE, fontSize = title_size)) %>% 
        
            hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), formatter = JS("function(){ return (this.point.name + ' : ' + this.point.y + ' %' )}")) %>%
            
            hc_add_theme(chosen_theme) %>% 
            hc_colors(colors_beverages) %>% 
            hc_plotOptions(pie =list(dataLabels = list(enabled = TRUE, style = list(useHTML = TRUE, fontSize = axis_size))))
        
    })
    
    

    
    ### b. Comparison graphs  -----------------------------------------------------
    
    
    output$graph_individual_vs_groups <- renderHighchart({ # Graph comparing your impact for a given type of environmental damage to the one of women and men of your own age group
      
      if(total_meals() > 0 & beverage_proportions() > 0) {
        
        
        hchart(comparison_individual_impacts_age_groups() %>% 
                 filter(type_impacts == get_impact_type(input$selected_impact_compare)), 
               
               "column", hcaes(x = demographic_group, y = round(yearly_impacts, digits = 0 ), group = demographic_group)) %>%        
            
            hc_title(text = paste0(get_impact_name(input$selected_impact_compare), " - Average Impacts"), style = list(useHTML = TRUE, fontSize = title_size)) %>% 
            
            hc_xAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size))) %>%
            hc_xAxis(labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>%
            
            hc_yAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size))) %>%
            hc_yAxis(labels = list(format = paste0("{value} ", get_impact_unit(input$selected_impact_compare)), style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
            
            hc_legend(align = "center", verticalAlign = "top",  itemStyle = list(useHTML = TRUE, fontSize = axis_size)) %>%
            hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), valueSuffix = paste0(" ", get_impact_unit(input$selected_impact_compare)) ) %>%
            
            hc_colors(colors_sexyou) %>% 
            hc_add_theme(chosen_theme)
        
      } else {
        
        hchart(data.frame("macro_group" = "Error", "share_conso" = NA), "area",             
               hcaes(x = macro_group, y = share_conso)) %>%               
          
          hc_title(text = message_profile_inputs(), 
                   style = list(useHTML = TRUE, color = "red", fontWeight = "bold",  fontSize = title_size)) %>%               
          
          hc_xAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size))) %>%       
          hc_xAxis(labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>%               
          hc_yAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size))) %>%       
          hc_yAxis(labels = list(format = paste0("{value} ", get_impact_unit(input$selected_impact_compare)), style = list(useHTML = TRUE, fontSize = axis_size))) %>%              
          
          hc_add_theme(chosen_theme)           
        
        
      }
        
    })
    
    
    
    output$graph_individual_percentile <- renderHighchart({ # Graph comparing your impact for a given type of environmental damage to the entire distribution of males and females impacts 
        
      if(total_meals() > 0 & beverage_proportions() > 0) {
        
      
        hchart(percentiles_impacts_all %>% 
                 filter(type_impacts == get_impact_type(input$selected_impact_compare)) %>% 
                 filter(demographic_group %in% c(paste0("Males ", age_group_individual() ), 
                                                 paste0("Females ", age_group_individual() ) ) ), 
               "area",  hcaes(x = percentile, y = value, group = demographic_group)) %>%
            
            hc_yAxis(plotLines = list(list(color = "black", width = 2, zIndex = 5,  dashStyle = "dash", 
                                           label = list(text = paste0("You (", round(individual_selected_impact_value_compare(), digits = 0), " ", get_impact_unit(input$selected_impact_compare), ")"), 
                                                        style = list(useHTML = TRUE, fontSize = axis_size)), 
                                           value = individual_selected_impact_value_compare() ) ) ) %>%        
            
            hc_title(text = paste0(get_impact_name(input$selected_impact_compare), " - Entire Distribution of Impacts"), style = list(useHTML = TRUE, fontSize = title_size)) %>% 
            
            hc_xAxis(title = list(text = "Percentile", style = list(useHTML = TRUE, fontSize = axis_size))) %>%
            hc_xAxis(labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>%
            
            hc_yAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size)), min = 0, 
                     
                     max = max( (percentiles_impacts_all %>% filter(type_impacts == get_impact_type(input$selected_impact_compare)) %>% filter(demographic_group %in% c(paste0("Males ", age_group_individual() ), paste0("Females ", age_group_individual() ) ) ) )$value,  individual_selected_impact_value_compare() ) ) %>%
          
            hc_yAxis(labels = list(format = paste0("{value} ", get_impact_unit(input$selected_impact_compare)), style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
            
            hc_legend(align = "center", verticalAlign = "top",  itemStyle = list(useHTML = TRUE, fontSize = axis_size)) %>%
            hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), formatter = JS(paste0("function(){ return (this.series.name + ' at the ' +  Highcharts.numberFormat(this.point.x,0) + '<sup>th</sup> Percentile<br>' + 'Yearly impacts: ' + Highcharts.numberFormat(this.point.y,0) )}"))) %>%
            
            hc_colors(colors_sexyou) %>% 
            hc_add_theme(chosen_theme)
        
      } else {
        
        
        hchart(data.frame("macro_group" = "Error", "share_conso" = NA), "area",             
               hcaes(x = macro_group, y = share_conso)) %>%               
          
          hc_title(text = message_profile_inputs(), 
                   style = list(useHTML = TRUE, color = "red", fontWeight = "bold",  fontSize = title_size)) %>%               
          
          hc_xAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size))) %>%       
          hc_xAxis(labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>%               
          hc_yAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size))) %>%       
          hc_yAxis(labels = list(format = paste0("{value} ", get_impact_unit(input$selected_impact_compare)), style = list(useHTML = TRUE, fontSize = axis_size))) %>%              
          
          hc_add_theme(chosen_theme)        
        
      }
        
    })
    
    
    
    ### c. Anthropometrics graphs  -----------------------------------------------------
    
    
    
    output$graph_height <- renderHighchart({ # scatter plot of height vs yearly impact, for both sexes of your age group and you 
      
      if(total_meals() > 0 & beverage_proportions() > 0) {
        
        
        hchart(anthropo_dem_groups() %>% 
                 filter(impacts == get_impact_type(input$selected_impact_anthropometrics) ), 
               
               "scatter", hcaes(x = taille, y = round(value, digits = 0), group = demographic_group), 
               
               regression = TRUE, regressionSettings = list(dashStyle = "ShortDash",  lineWidth = 2.5, name = "%eq | r2: %r", hideInLegend = FALSE), useAllSeries = TRUE, color = colors_sexyou_faded[1:2])  %>% 
          hc_add_dependency("plugins/highcharts-regression.js") %>%
          
          hc_colors(colors_sexyou[1:2]) %>%
          
          hc_add_series(data.frame(x = input$height, y = round(individual_selected_impact_value_anthropometrics(), digits = 0) ), type = "scatter", color = colors_sexyou[3],  zIndex = 99, name = "You", marker = list(radius = 13)) %>%
          
          hc_title(style = list(useHTML = TRUE, fontSize = title_size), text = paste0("Height versus " , get_impact_name(input$selected_impact_anthropometrics) ) ) %>%
          
          hc_yAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size))) %>%
          hc_xAxis(title = list(text = "Height"), labels = list(style = list(useHTML = TRUE, fontSize = axis_size) ) ) %>%
          hc_yAxis(labels = list(format = paste0("{value} ", get_impact_unit(input$selected_impact_anthropometrics)) , style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
          hc_xAxis(labels = list(format = paste0("{value} ", "cm") , style = list(useHTML = TRUE, fontSize = axis_size) ) ) %>% 
          hc_yAxis(type = "logarithmic") %>%
          
          hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), formatter = JS(paste0("function(){ return (this.series.name + ' <br>' + 'Height: ' + this.point.x + ' (cm) <br>' + 'Impact: ' + this.point.y + ' (unit - see LHS)' )}"))) %>%
          
          hc_legend(align = "center", verticalAlign = "top",  itemStyle = list(useHTML = TRUE, fontSize = axis_size)) %>%
          
          hc_add_theme(chosen_theme) 
        
        
      } else {
        
        
        hchart(data.frame("macro_group" = "Error", "share_conso" = NA), "area",             
               hcaes(x = macro_group, y = share_conso)) %>%               
          
          hc_title(text = message_profile_inputs(), 
                   style = list(useHTML = TRUE, color = "red", fontWeight = "bold",  fontSize = title_size)) %>%               
          
          hc_xAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size))) %>%       
          hc_xAxis(labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>%               
          hc_yAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size))) %>%       
          hc_yAxis(labels = list(format = paste0("{value} ", get_impact_unit(input$selected_impact_compare)), style = list(useHTML = TRUE, fontSize = axis_size))) %>%              
          
          hc_add_theme(chosen_theme)                  
        
      }
      
      
    })
    
    
    
  
    output$graph_weight <- renderHighchart({ # scatter plot of weight vs yearly impact, for both sexes of your age group and you 
      
      if(total_meals() > 0 & beverage_proportions() > 0) {
        
        
        hchart(anthropo_dem_groups() %>% 
                 filter(impacts == get_impact_type(input$selected_impact_anthropometrics) ), 
               
               "scatter", hcaes(x = round(poids, digits = 2), y = round(value, digits = 0), group = demographic_group), 
               
               regression = TRUE, regressionSettings = list(dashStyle = "ShortDash",  lineWidth = 2.5, name = "%eq | r2: %r", hideInLegend = FALSE), useAllSeries = TRUE, color = colors_sexyou_faded[1:2])  %>% 
          hc_add_dependency("plugins/highcharts-regression.js") %>%
          
          hc_colors(colors_sexyou[1:2]) %>%
          
          hc_add_series(data.frame(x = input$weight, y = round(individual_selected_impact_value_anthropometrics(), digits = 0) ), type = "scatter", color = colors_sexyou[3],  zIndex = 99, name = "You", marker = list(radius = 13)) %>%
          
          hc_title(style = list(useHTML = TRUE, fontSize = title_size), 
                   text = paste0("Weight versus " , get_impact_name(input$selected_impact_anthropometrics) ) ) %>%
          
          hc_yAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size) ) ) %>%
          hc_xAxis(title = list(text = "Weight"), labels = list(style = list(useHTML = TRUE, fontSize = axis_size) ) ) %>%
          hc_yAxis(labels = list(format = paste0("{value} ", get_impact_unit(input$selected_impact_anthropometrics) ) , style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
          hc_xAxis(labels = list(format = paste0("{value} ", "kg") , style = list(useHTML = TRUE, fontSize = axis_size) ) ) %>% 
          hc_yAxis(type = "logarithmic") %>%
          
          hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), formatter = JS(paste0("function(){ return (this.series.name + ' <br>' + 'Weight: ' + this.point.x + ' (kg) <br>' + 'Impact: ' + this.point.y + ' (unit - see LHS)' )}" ) ) ) %>%
          
          hc_legend(align = "center", verticalAlign = "top",  itemStyle = list(useHTML = TRUE, fontSize = axis_size) ) %>%
          
          hc_add_theme(chosen_theme) 
        
        
      } else {
        
        
        hchart(data.frame("macro_group" = "Error", "share_conso" = NA), "area",             
               hcaes(x = macro_group, y = share_conso)) %>%               
          
          hc_title(text = message_profile_inputs(), 
                   style = list(useHTML = TRUE, color = "red", fontWeight = "bold",  fontSize = title_size)) %>%               
          
          hc_xAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size))) %>%       
          hc_xAxis(labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>%               
          hc_yAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size))) %>%       
          hc_yAxis(labels = list(format = paste0("{value} ", get_impact_unit(input$selected_impact_compare)), style = list(useHTML = TRUE, fontSize = axis_size))) %>%              
          
          hc_add_theme(chosen_theme)             
        
      }
      
      
    })
    
    
    
    
    
    output$graph_bmi <- renderHighchart({ # scatter plot of BMI vs yearly impact, for both sexes of your age group and you 
      
      if(total_meals() > 0 & beverage_proportions() > 0) {
        
        
        hchart(anthropo_dem_groups() %>% 
                 filter(impacts == get_impact_type(input$selected_impact_anthropometrics) ),
               
               "scatter", hcaes(x = round(imc, digits = 2), y = round(value, digits = 0), group = demographic_group), 
               
               regression = TRUE, regressionSettings = list(dashStyle = "ShortDash",  lineWidth = 2.5, name = "%eq | r2: %r", hideInLegend = FALSE), useAllSeries = TRUE, color = colors_sexyou_faded[1:2])  %>% 
          hc_add_dependency("plugins/highcharts-regression.js") %>%
          
          hc_colors(colors_sexyou[1:2]) %>%
          
          hc_add_series(data.frame(x = bmi_individual(), y = round(individual_selected_impact_value_anthropometrics(), digits = 0)), type = "scatter", color = colors_sexyou[3],  zIndex = 99, name = "You", marker = list(radius = 13) ) %>%
          
          hc_title(style = list(useHTML = TRUE, fontSize = title_size), 
                   text = paste0("BMI versus " , get_impact_name(input$selected_impact_anthropometrics) ) ) %>%
          
          hc_yAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size) ) ) %>%
          hc_xAxis(title = list(text = "BMI"), labels = list(style = list(useHTML = TRUE, fontSize = axis_size) ) ) %>%
          hc_yAxis(labels = list(format = paste0("{value} ", get_impact_unit(input$selected_impact_anthropometrics) ) , style = list(useHTML = TRUE, fontSize = axis_size) ) ) %>% 
          hc_xAxis(labels = list(format = paste0("{value} ", "") , style = list(useHTML = TRUE, fontSize = axis_size) ) ) %>% 
          hc_yAxis(type = "logarithmic") %>%
          
          hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), formatter = JS(paste0("function(){ return (this.series.name + ' <br>' + 'BMI: ' + this.point.x + ' <br>' + 'Impact: ' + this.point.y + ' (unit - see LHS)' )}") ) ) %>%
          
          hc_legend(align = "center", verticalAlign = "top",  itemStyle = list(useHTML = TRUE, fontSize = axis_size) ) %>%
          
          hc_add_theme(chosen_theme) 
        
        
      } else {
        
        
        hchart(data.frame("macro_group" = "Error", "share_conso" = NA), "area",             
               hcaes(x = macro_group, y = share_conso)) %>%               
          
          hc_title(text = message_profile_inputs(), 
                   style = list(useHTML = TRUE, color = "red", fontWeight = "bold",  fontSize = title_size)) %>%               
          
          hc_xAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size))) %>%       
          hc_xAxis(labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>%               
          hc_yAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size))) %>%       
          hc_yAxis(labels = list(format = paste0("{value} ", get_impact_unit(input$selected_impact_compare)), style = list(useHTML = TRUE, fontSize = axis_size))) %>%              
          
          hc_add_theme(chosen_theme)            
        
      }
      
      
    })
    
    
    
    
    output$graph_bmr <- renderHighchart({ # scatter plot of BMR vs yearly impact, for both sexes of your age group and you 
      
      if(total_meals() > 0 & beverage_proportions() > 0) {
        
        
        hchart(anthropo_dem_groups() %>% 
                 filter(impacts == get_impact_type(input$selected_impact_anthropometrics) ), 
               
               "scatter", hcaes(x = round(bmr_kcal, digits = 2), y = round(value, digits = 0), group = demographic_group), 
               
               regression = TRUE, regressionSettings = list(dashStyle = "ShortDash",  lineWidth = 2.5, name = "%eq | r2: %r", hideInLegend = FALSE), useAllSeries = TRUE, color = colors_sexyou_faded[1:2])  %>% 
          hc_add_dependency("plugins/highcharts-regression.js") %>%
          
          hc_colors(colors_sexyou[1:2]) %>%
          
          hc_add_series(data.frame(x = bmr_individual(), y = round( individual_selected_impact_value_anthropometrics(), digits = 0) ), type = "scatter", color = colors_sexyou[3],  zIndex = 99, name = "You", marker = list(radius = 13) ) %>%
          
          hc_title(style = list(useHTML = TRUE, fontSize = title_size), text = paste0("BMR versus " , get_impact_name(input$selected_impact_anthropometrics) ) ) %>%
          
          hc_yAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size) ) ) %>%
          hc_xAxis(title = list(text = "BMR"), labels = list(style = list(useHTML = TRUE, fontSize = axis_size) ) ) %>%
          hc_yAxis(labels = list(format = paste0("{value} ", get_impact_unit(input$selected_impact_anthropometrics) ) , style = list(useHTML = TRUE, fontSize = axis_size) ) ) %>% 
          hc_xAxis(labels = list(format = paste0("{value} ", "kcal") , style = list(useHTML = TRUE, fontSize = axis_size) ) ) %>% 
          hc_yAxis(type = "logarithmic") %>%
          
          hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), formatter = JS(paste0("function(){ return (this.series.name + ' <br>' + 'BMR: ' + this.point.x + ' kcal<br>' + 'Impact: ' + this.point.y + ' (unit - see LHS)' )}") ) ) %>%
          
          hc_legend(align = "center", verticalAlign = "top",  itemStyle = list(useHTML = TRUE, fontSize = axis_size) ) %>%
          
          hc_add_theme(chosen_theme) 
        
        
      } else {
        
        
        hchart(data.frame("macro_group" = "Error", "share_conso" = NA), "area",             
               hcaes(x = macro_group, y = share_conso)) %>%               
          
          hc_title(text = message_profile_inputs(), 
                   style = list(useHTML = TRUE, color = "red", fontWeight = "bold",  fontSize = title_size)) %>%               
          
          hc_xAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size))) %>%       
          hc_xAxis(labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>%               
          hc_yAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size))) %>%       
          hc_yAxis(labels = list(format = paste0("{value} ", get_impact_unit(input$selected_impact_compare)), style = list(useHTML = TRUE, fontSize = axis_size))) %>%              
          
          hc_add_theme(chosen_theme)          
        
      }
      
      
    })
    
    
    
    
    ### d. Understand graphs  -----------------------------------------------------
    
    
    output$graph_treemap_specific_impacts <- renderHighchart({ # Graph describing your environmental impact by food group for a given type of impact
      
      
      # Function to get share_impacts for a specific group
      
      get_share_impacts <- function(group_name) {
        
        (all_impacts_individual() %>% 
           
           filter(type_impacts == get_impact_type(input$selected_impact_understand) ) %>%
           filter(specific_group == group_name))$share_impacts
      }
      
      
      if(total_meals() > 0 & beverage_proportions() > 0) {
        
        
        # Processing of the required data for the treemap list
        
        data_list <- list()
        
        # Level I data
        
        for (i in seq_along( unique(data_colors_macrofood$macro_group) ) ) {
          
          macro_group_selected <- unique(data_colors_macrofood$macro_group)[i]
          
          data_list <- append(data_list, list(
            list(
              id = macro_group_selected,
              name = macro_group_selected,
              color = colors_macrofood[i]
            )
          ))
        
          
          # Level II data for each macro group
          specific_groups <- correspondence_macro_specific %>% 
            filter(macro_group == macro_group_selected) %>%
            pull(specific_group)
          
          for (specific_group in specific_groups) {
            data_list <- append(data_list, list(
              list(
                name = specific_group,
                parent = macro_group_selected,
                value = get_share_impacts(specific_group)
                
              )
            ))
          }
        }
        
        
        highchart() %>% 
          hc_add_series(
            type = "treemap",
            allowTraversingTree = T,
            levelIsConstant = F,
            levels = list(
              list(
                level = 1,
                dataLabels = list(enabled = TRUE, 
                                  format = "{point.name}<br>
                                                   {point.value}%"), borderColor = "black", borderWidth = 2),         
              list(
                level = 2,
                dataLabels = list(
                  enabled = F
                ),
                borderWidth = 2
              )
            ),
            data = data_list
          ) %>%
          
          hc_title(text = paste0("Causes of your ", get_impact_name(input$selected_impact_understand), " (", round(individual_selected_impact_value_understand(), digits = 0), " ", get_impact_unit(input$selected_impact_understand),  ")"), style = list(useHTML = TRUE, fontSize = title_size)) %>%
          
          hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), valueSuffix ="%") %>% 
          hc_caption(style = list(useHTML = TRUE, fontSize = caption_size), text = "Note: Due to rounding errors, the total may not add up to 100%") %>%
          
          hc_plotOptions(treemap =list(dataLabels = list(style = list(useHTML = TRUE, fontSize = title_size)))) %>%
          
          
          hc_add_theme(chosen_theme)
      
      } else {
        
        
        hchart(data.frame("macro_group" = "Error", "share_conso" = NA), "area",             
               hcaes(x = macro_group, y = share_conso)) %>%               
          
          hc_title(text = message_profile_inputs(), 
                   style = list(useHTML = TRUE, color = "red", fontWeight = "bold",  fontSize = title_size)) %>%               
          
          hc_xAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size))) %>%       
          hc_xAxis(labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>%               
          hc_yAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size))) %>%       
          hc_yAxis(labels = list(format = paste0("{value} ", get_impact_unit(input$selected_impact_compare)), style = list(useHTML = TRUE, fontSize = axis_size))) %>%              
          
          hc_add_theme(chosen_theme)        
        
      }     
        
    
      
    })
    
    
    output$graph_waterfall_males <- renderHighchart({ # graph comparing your impacts against males of your age group, with the decomposition between differences in quantities, intensities/dietary choices, and interaction term
    
      if(total_meals() > 0 & beverage_proportions() > 0) {
        
      
      hchart(individual_waterfall_males(), "waterfall")  %>% 
        
        hc_xAxis(type = "category") %>% 
        
        hc_add_series(individual_waterfall_males(), "waterfall", hcaes(x = Variable, y = round(Value, digits = 0), color = shade)) %>%
        
        hc_title(style = list(useHTML = TRUE, fontSize = title_size), text = paste0("You against Males ", age_group_individual(), " - " , get_impact_name(input$selected_impact_understand))) %>%
        
        hc_yAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size))) %>%
        hc_xAxis(title = list(text = ""), labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>%
        hc_yAxis(labels = list(format = paste0("{value} ", get_impact_unit(input$selected_impact_understand)) , style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
        
        hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), formatter = JS("function(){ return (this.point.name + '' + ': ' + Math.abs(this.point.y) )}")) %>%
      
      hc_add_theme(chosen_theme) 
      
      } else {
        
        
        hchart(data.frame("macro_group" = "Error", "share_conso" = NA), "area",             
               hcaes(x = macro_group, y = share_conso)) %>%               
          
          hc_title(text = message_profile_inputs(), 
                   style = list(useHTML = TRUE, color = "red", fontWeight = "bold",  fontSize = title_size)) %>%               
          
          hc_xAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size))) %>%       
          hc_xAxis(labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>%               
          hc_yAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size))) %>%       
          hc_yAxis(labels = list(format = paste0("{value} ", get_impact_unit(input$selected_impact_compare)), style = list(useHTML = TRUE, fontSize = axis_size))) %>%              
          
          hc_add_theme(chosen_theme)         
        
        
      }
    
    
    })
    
    
    
    
    
    output$graph_waterfall_females <- renderHighchart({ # graph comparing your impacts against females of your age group, with the decomposition between differences in quantities, intensities/dietary choices, and interaction term
      
      if(total_meals() > 0 & beverage_proportions() > 0) {
        
        
        hchart(individual_waterfall_females(), "waterfall")  %>% 
          
          hc_xAxis(type = "category") %>% 
          
          hc_add_series(individual_waterfall_females(), "waterfall", hcaes(x = Variable, y = round(Value, digits = 0), color = shade)) %>%
          
          hc_title(style = list(useHTML = TRUE, fontSize = title_size), text = paste0("You against Females ", age_group_individual(), " - " , get_impact_name(input$selected_impact_understand))) %>%
          
          hc_yAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size))) %>%
          hc_xAxis(title = list(text = ""), labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>%
          hc_yAxis(labels = list(format = paste0("{value} ", get_impact_unit(input$selected_impact_understand)) , style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
          
          hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), formatter = JS("function(){ return (this.point.name + '' + ': ' + Math.abs(this.point.y) )}")) %>%
          
          hc_add_theme(chosen_theme) 
        
      } else {
        
        
        hchart(data.frame("macro_group" = "Error", "share_conso" = NA), "area",             
               hcaes(x = macro_group, y = share_conso)) %>%               
          
          hc_title(text = message_profile_inputs(), 
                   style = list(useHTML = TRUE, color = "red", fontWeight = "bold",  fontSize = title_size)) %>%               
          
          hc_xAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size))) %>%       
          hc_xAxis(labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>%               
          hc_yAxis(title = list(text = "", style = list(useHTML = TRUE, fontSize = axis_size))) %>%       
          hc_yAxis(labels = list(format = paste0("{value} ", get_impact_unit(input$selected_impact_compare)), style = list(useHTML = TRUE, fontSize = axis_size))) %>%              
          
          hc_add_theme(chosen_theme)            
        
      }
      
      
    })
    
    
    
    
    
    
    ## 5.6. Value Boxes  --------------------------------------
    
    
    ### a. Impacts Value Boxes  -----------------------------------------------------
    
    
    lapply(1:length(list_impacts), function(i) { # Generate the text of the 15 value boxes for the Impacts Page
      
      outputId <- paste0("vb_", list_impacts[i])
      
      output[[outputId]] <- renderText({
        
        if(total_meals() > 0 & beverage_proportions() > 0) {
          
          paste0(round((summary_impacts_individual() %>% filter(type_impacts == list_impacts[i]) %>% filter(demographic_group == "You"))$yearly_impacts, digits = 0), "")
          
        } else {
          
          "N/A"
          
        }
        
        
      })
      
    })
    
    
    
    ### b. Compare Value Boxes  -----------------------------------------------------
    
    #### Percentage difference value boxes  -----------------------------------------------------
    
    
    list_vb_perc_difference <- c("male", "female") # Boxes regarding the percentage difference of your impacts vs males/females of your age group
    
    
    lapply(1:length(list_vb_perc_difference), function(k) { 
      
      outputId <- paste0("vb_", list_vb_perc_difference[k], "_perc")
      
      output[[outputId]] <- renderUI({
        
        makeValueBox(title = "You pollute", 
                     
                     subtitle =  if(total_meals() > 0 & beverage_proportions() > 0) {
          
          ifelse(  (comparison_individual_impacts_age_groups_percent() %>% filter(type_impacts == get_impact_type(input$selected_impact_compare) )
                    %>% select(paste0("change_vs_", list_vb_perc_difference[k], "s")) ) >= 0, 
                 
                 paste0("more than the average ",  list_vb_perc_difference[k],  " of your age group"), 
                 paste0("less than the average ", list_vb_perc_difference[k], " of your age group"))
          
        } else {
          
          ""
          
        }, 
        
        value =  if(total_meals() > 0 & beverage_proportions() > 0) {
          
          paste0( round(
            abs(
              comparison_individual_impacts_age_groups_percent() %>% 
                 filter(type_impacts == get_impact_type(input$selected_impact_compare)) %>% 
                select(paste0("change_vs_", list_vb_perc_difference[k], "s"))
            
              )
            , digits = 0), 
                  
                  "%")
          
        } else {
          
          "N/A"
          
        }, background_color = ifelse( total_meals() >0 & beverage_proportions() > 0,  ifelse( (comparison_individual_impacts_age_groups_percent() %>%
                                         filter(type_impacts == get_impact_type(input$selected_impact_compare) ) %>%
                                         select(paste0("change_vs_", list_vb_perc_difference[k], "s")) ) >= 0, 
                                      
                                      colors_negative_positive[1], 
                                      colors_negative_positive[2]), "grey"), 
        
        text_color = compare_vb_font_color, 
        image = paste0("sex/", list_vb_perc_difference[k], ".png"), 
        size = compare_vb_size)
        
      })
    
      
    })
    
    
    
    #### Percentiles value boxes  -----------------------------------------------------
    
    
    list_vb_percentiles <- c("male", "female") # Boxes regarding the percentiles of your impacts vs males/females of your age group
    vb_percentiles_genders <- c("Males", "Females")
    
    
    lapply(1:length(list_vb_percentiles), function(l) { 
      
      outputId <- paste0("vb_", list_vb_percentiles[l], "_percentile")
      
      
      
      
      output[[outputId]] <- renderUI({
        
        
        if (total_meals() > 0 & beverage_proportions() > 0) {
          
          considered_percentile <- get_percentile( percentiles_impacts_all %>% 
                                                     filter(type_impacts == get_impact_type(input$selected_impact_compare)) %>% 
                                                     filter(demographic_group %in% c(paste0(vb_percentiles_genders[l], " ", age_group_individual() )))
                                                   %>% select(value)
                                                   , individual_selected_impact_value_compare())
          
        } else {
          
          considered_percentile <- 100
          
        }
        
       
        
        
        makeValueBox(title = "You pollute more than", subtitle =  if(total_meals() > 0 & beverage_proportions() > 0) {
          
          paste0("of ", list_vb_percentiles[l], "s from your age group")
          
        } else {
          
          ""
          
        }, value =  if(total_meals() > 0 & beverage_proportions() > 0) {
          
          paste0(considered_percentile, "%")
          
        } else {
          
          "N/A"
          
        }, background_color = colors_box_neg_pos[min(101, 
        round(as.numeric(considered_percentile), digits = 0) + 1)],
        
        
        text_color = compare_vb_font_color, 
        image = paste0("sex/", list_vb_perc_difference[l], ".png"), 
        size = compare_vb_size)
        
      
    })
      
    })
    
    
    
    
    ### c. Understand Value Boxes (most budensome specific types of food)  -----------------------------------------------------
    
    
    # Generate the three dynamic value boxes with the most impactful three specific type of food, for a given impact type
    
    list_vb_understand <- c("first", "second", "third")
    
    
    lapply(1:length(list_vb_understand), function(j) { 
      
      outputId <- paste0("vb_burden_specific_", j)
      
      output[[outputId]] <- renderUI({ # Box showing the most burdensome specific groupw for this type of impact for the individual 
        
        
        makeValueBox(title = paste0("Your ", list_vb_understand[j], " most impactful type of food:"), subtitle =  if(total_meals() > 0 & beverage_proportions() > 0) {
          
          paste0("(", (individual_most_burden_specific_groups_impacts() %>% slice(j))$share_impacts, "% of your ", 
                 get_impact_name(input$selected_impact_understand), " impacts)")
          
          
        } else {
          
          ""
          
        }, value =  if(total_meals() > 0 & beverage_proportions() > 0) {
          
          paste0( (individual_most_burden_specific_groups_impacts() %>% slice(j))$specific_group )
          
        } else {
          
          "N/A"
          
        }, background_color = ifelse(total_meals() > 0 & beverage_proportions() > 0, case_when(
          
          match((individual_most_burden_specific_groups_impacts() %>% slice(j))$macro_group, data_colors_macrofood$macro_group) > 0 ~ colors_macrofood[match((individual_most_burden_specific_groups_impacts() %>% slice(j))$macro_group, data_colors_macrofood$macro_group)],
          
          TRUE ~ "grey"
          
        ), "grey"), text_color = understand_vb_font_color, 
        
        image = ifelse(total_meals() > 0 & beverage_proportions() > 0, paste0("specific_groups/", (individual_most_burden_specific_groups_impacts() %>% slice(j))$specific_group, ".png"), "nogroup.png"), 
        
        size = understand_vb_size, fontsize_core = "xxLarge")
        
        
      })
      
    })
    
    
    
    
    ## 5.7. Report Processing  --------------------------------------

    
    download_clicked <- reactiveVal(FALSE)
    
    
    output$spinner <- renderUI({
   
        if (download_clicked()) {

      Spinner(size = 3, label = "Generating report...")
          
        }
    })    

  
    
    observeEvent(input$downloadButton, { # Redirect logic to generate download
      
      if (total_meals() > 0 & beverage_proportions() > 0) {
        
        click("download")
        download_clicked(TRUE)
        
        
      }
  
    })
    

    
    output$download <- downloadHandler(
      filename = "SimEnvImpacts_customized_report.pdf",
      content = function(file) {
        
        withProgress(
          message = "Generating report...",
          detail = "This may take a moment.",
          value = 0,
          {
            # Render the report
            
            res <- rmarkdown::render("report.Rmd")
            
            # Move the rendered file to the download location
            file.rename(res, file)
          }
        )
        Sys.sleep(2)
        download_clicked(FALSE)
        

      }
    )
    
    
    
}



# 6. Running the App -------------------------------------------------------


shinyApp(ui, server)


