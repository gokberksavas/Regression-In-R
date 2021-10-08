#install.packages("rJava")
#install.packages("xlsxjars")
#install.packages("xlsx")

library(xlsx)
setwd("C:/Users/cicek/Desktop/")
data <- read.xlsx("dersnotlari2.xlsx", sheetName = "datastructures")
circuit <- read.xlsx("dersnotlari2.xlsx", sheetName = "circuittheory")
logic <- read.xlsx("dersnotlari2.xlsx", sheetName = "logicdesign") #imported data from excel file

names <- c(colnames(data))

calculate_mean <- function(df) #average calculating function, df is dataframe of lecture
{
  sum_of_grades <- 0
  number_of_grades <- 0
  for(i in 1:10)
  {
    if(is.numeric(df[,i]) == TRUE)
    {
      sum_of_grades <- sum_of_grades + df[,i]
      number_of_grades <- number_of_grades + 1
    }
    else {}
  }
  return(sum_of_grades/number_of_grades)
}

std_deviation <- function(df) #standard deviation calculating function, df is dataframe of lecture
{
  mean_value <- calculate_mean(df)
  numerator <- 0
  denumerator <- 0
  for(i in 1:10)
  {
    if(is.numeric(df[,i]) == TRUE)
    {
      numerator <- numerator + (df[,i] - mean_value)**2
      denumerator <- denumerator + 1
    }
    else {}
  }
  return((numerator/(denumerator-1))**(1/2))
}

zvalue <- function(df,student) #function that calculates the Z score of each student
{
  if(is.numeric(df[,student]) == TRUE)
  {
    z = (df[,student] - calculate_mean(df))/std_deviation(df)
  }
  else {z = NA}
  return(z)
}

zvalue_comparison <- function(ds1,ds2,ds3,student) #function that compares the success rankings of all courses of each student
  
  result <- c(pnorm(zvalue(ds1,student)),pnorm(zvalue(ds2,student)),pnorm(zvalue(ds3,student)))
  result <- sort(result,decreasing = TRUE,na.last = TRUE)
    for(i in 1:length(result))
    {
      if(is.na(result[i]) == TRUE)
      {result[i] <- "-" }
    }
    for(i in 1:length(result))
    {
      if(result[i] != "-" && is.na(zvalue(data,student)) == FALSE && result[i] == pnorm(zvalue(data,student)))
      {
        result[i] <- "Data Structures"
      }
      else if(result[i] != "-" && is.na(zvalue(circuit,student)) == FALSE && result[i] == pnorm(zvalue(circuit,student)))
      {
        result[i] <- "Circuit Theory"
      }
      else if(result[i] != "-" && is.na(zvalue(logic,student)) == FALSE && result[i] == pnorm(zvalue(logic,student)))
      {
        result[i] <- "Logic Design"
      }
      
    }
  return(result)
}                                             

  if(pnorm(zvalue(data,"baran")) > pnorm(zvalue(logic,"bulent")))
  {
    print("Baran, who took Data Structures course, is more successful than Bülent who took Logic Design course.")
  }
  if(pnorm(zvalue(data,"baran")) < pnorm(zvalue(logic,"bulent")))
  {
    print("Bülent, who took Logic Design course, is more successful than Baran who took Data Structures course.")
  }


basari_tablo <- data.frame(v = character(3),
                           v = character(3),
                           v = character(3),
                           v = character(3),
                           v = character(3),
                           v = character(3),
                           v = character(3),
                           v = character(3),
                           v = character(3),
                           v = character(3))



colnames(basari_tablo) <- names

  for(i in 1:length(names))
  {
    column <- zvalue_comparison(data,circuit,logic,names[i])
    basari_tablo[,names[i]] <- column
  }

View(basari_tablo)


















