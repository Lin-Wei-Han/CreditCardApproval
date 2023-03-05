check.packages <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE)
  }
  sapply(pkg, require, character.only = TRUE)
}

pkg <- c(
  "data.table", "dplyr", "plotly", "tidymodels"
)

check.packages(pkg)

setwd("C:\\2_DataScience\\04_Credit_Card_Approval")
#========================================================================
#                      Read Dataset
#========================================================================

application_record <- fread("data/application_record.csv")
credit_record <- fread("data/credit_record.csv")

dataset <- merge(application_record,credit_record,by="ID")

#========================================================================
#                      Clean Data
#========================================================================
#-------------------------------------
#          Drop NAs
#-------------------------------------

dataset <- dataset[complete.cases(dataset),]

#-------------------------------------
#          transform category
#-------------------------------------

dataset$STATUS <- case_when(
  dataset$STATUS == 'C'~'Good',
  dataset$STATUS == 'X'~'Good',
  dataset$STATUS == '0'~'Good',
  TRUE ~ 'Bad'
)

dataset%>%group_by(STATUS)%>%summarise(n=n())
#-------------------------------------
#         Good/Bad client
#-------------------------------------

Good_client <- dataset%>%filter(STATUS=="Good")
Bad_client <- dataset%>%filter(STATUS=="Bad")

#-------------------------------------
#         Male/Female client
#-------------------------------------

Male_client <- dataset%>%filter(CODE_GENDER=="M")
Female_client <- dataset%>%filter(CODE_GENDER=="F")

#========================================================================
#                      Plotly
#========================================================================

dataset%>%
  group_by(STATUS)%>%
  summarise(n=n(),"mean_income"=mean(AMT_INCOME_TOTAL),"median_income"=median(AMT_INCOME_TOTAL),
            "mean_birth"=mean(DAYS_BIRTH),"median_birth"=median(DAYS_BIRTH))

dataset%>%
  group_by(CODE_GENDER)%>%
  summarise(n=n(),"mean_income"=mean(AMT_INCOME_TOTAL),"median_income"=median(AMT_INCOME_TOTAL))
#-------------------------------------
#           status / AMT_INCOME_TOTAL
#-------------------------------------
fig <- plot_ly(y =Good_client$AMT_INCOME_TOTAL , type = "box",name="Good client")
fig <- fig %>% add_trace(y =Bad_client$AMT_INCOME_TOTAL,name="Bad client" )
fig

#-------------------------------------
#           status / DAYS_BIRTH
#-------------------------------------

fig <- plot_ly(y =Good_client$DAYS_BIRTH , type = "box",name="Good client")
fig <- fig %>% add_trace(y =Bad_client$DAYS_BIRTH,name="Bad client" )
fig

#-------------------------------------
#           Gender / income
#-------------------------------------
fig <- plot_ly(y =Male_client$AMT_INCOME_TOTAL , type = "box",name="Male client")
fig <- fig %>% add_trace(y =Female_client$AMT_INCOME_TOTAL,name="Female client" )
fig

#-------------------------------------
#           status / education
#-------------------------------------
fig_Good_data <- Good_client %>% count(NAME_EDUCATION_TYPE,STATUS)
fig_Bad_data <- Bad_client %>% count(NAME_EDUCATION_TYPE,STATUS)

fig <- plot_ly()
fig <- fig %>% add_pie(data =fig_Good_data, labels = ~NAME_EDUCATION_TYPE, values = ~n, type = 'pie',
                       name = "Cut", domain = list(row = 0, column = 0))
fig <- fig %>% add_pie(data =fig_Bad_data, labels = ~NAME_EDUCATION_TYPE, values = ~n, type = 'pie',
                       name = "Cut", domain = list(row = 0, column = 1))
fig <- fig %>% layout(title = "Pie Charts with Subplots", showlegend = F,
                      grid=list(rows=1, columns=2),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig

#-------------------------------------
#           status / INCOME_TYPE
#-------------------------------------
fig_Good_data <- Good_client %>% count(NAME_INCOME_TYPE,STATUS)
fig_Bad_data <- Bad_client %>% count(NAME_INCOME_TYPE,STATUS)

fig <- plot_ly()
fig <- fig %>% add_pie(data =fig_Good_data, labels = ~NAME_INCOME_TYPE, values = ~n, type = 'pie',
                       name = "Cut", domain = list(row = 0, column = 0))
fig <- fig %>% add_pie(data =fig_Bad_data, labels = ~NAME_INCOME_TYPE, values = ~n, type = 'pie',
                       name = "Cut", domain = list(row = 0, column = 1))
fig <- fig %>% layout(title = "Pie Charts with Subplots",
                      grid=list(rows=1, columns=2),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig

#-------------------------------------
#           status / FLAG_OWN_REALTY
#-------------------------------------
fig_Good_data <- Good_client %>% count(FLAG_OWN_REALTY,STATUS)
fig_Bad_data <- Bad_client %>% count(FLAG_OWN_REALTY,STATUS)

fig <- plot_ly()
fig <- fig %>% add_pie(data =fig_Good_data, labels = ~FLAG_OWN_REALTY, values = ~n, type = 'pie',
                       name = "Cut", domain = list(row = 0, column = 0))
fig <- fig %>% add_pie(data =fig_Bad_data, labels = ~FLAG_OWN_REALTY, values = ~n, type = 'pie',
                       name = "Cut", domain = list(row = 0, column = 1))
fig <- fig %>% layout(title = "Pie Charts with Subplots",
                      grid=list(rows=1, columns=2),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig


