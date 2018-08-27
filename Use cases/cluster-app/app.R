library(shiny)
library(shinythemes)

### Once off computations getting data in and cleaning it
require(gdata)
df = read.xls (("AssignmentData.xlsx"), sheet = 1, header = TRUE)
df<- df[c(1:21)]
# 192 obs. of  21 variables:
names(df)[3:21]<- c("ForeignInvestment", "ElectricityAccess", "RenewableEnergy", "CO2Emission", "Inflation", "MobileSubscriptions", "InternetUse", "Exports", "Imports", "GDP", "MortalityMale", "MortalityFemale", "BirthRate", "DeathRate", "MortalityInfant", "LifeExpectancy", "FertilityRate", "PopulationGrowth", "UrbanPopulation")

countries_data<-df

df2 = read.xls (("AssignmentData.xlsx"), sheet = 2, header = TRUE)
df2<- df2[c(1:21)]
names(df2)[3:21]<- c("ForeignInvestment", "ElectricityAccess", "RenewableEnergy", "CO2Emission", "Inflation", "MobileSubscriptions", "InternetUse", "Exports", "Imports", "GDP", "MortalityMale", "MortalityFemale", "BirthRate", "DeathRate", "MortalityInfant", "LifeExpectancy", "FertilityRate", "PopulationGrowth", "UrbanPopulation")

countrygroup_data<-df2

mapping = read.xls (("CLASS.xls"), sheet = 2, header = TRUE)
index<- which(rowSums(is.na(df))>2)
df[index,]
df<-df[-index,]
#str(df)
#sum(is.na(df))

#The dank avergaing algorithm
for( i in 3:21){
  for( j in 1:183){
    if (is.na(df[j,i])) {
      index<- which(as.character(df$Country[j])==as.character(mapping$CountryCode))
      myvar<- mapping$GroupCode[index]
      #print(myvar)
      myvalue<-0
      trig<-0
      
      for(x in 1:(length(myvar)-1)){
        tempval<-df2[which(as.character(df2$Country)==as.character(myvar[x])),i]
        if(is.na(tempval)){
          trig<-trig+1
        }else{
          myvalue<-myvalue+tempval
        }  
      }
      df[j,i]<-myvalue/(length(myvar)-trig-1)
    }
  }
}


### Shiny app starts here


# Define UI ----
ui <- fluidPage( theme = shinytheme("superhero"),
  titlePanel("My Shiny App"),
  
  sidebarLayout(
    
    sidebarPanel(
      h1("Installation"),
      p("Shiny is available on Cran, so you can blah blah"),
      code("install.packages(shiny)"),
      selectInput("var", h3("Select box"), 
                  choices = list("yes boys" = "getduck", "hasd 2" = 2,
                                 "Choice 3" = 3), selected = 1),
      sliderInput("slider2", "fucks 2 give",
                  min = 0, max = 100, value = c(25, 75))
      ),
    mainPanel(
      h1("Introducing Shiny"),
      p("talk some shit here"),
      h1("another heading talking some shit"),
      textOutput("selected_var")
    )
  )
)
# Define server logic ----
server <- function(input, output) {
  
  output$selected_var <- renderText({ 
    paste("You have selected", input$var)
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)