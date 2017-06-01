library(tidyverse)

# For those who've heard the turn of phrase, "month of Sundays", this project
# ...is aimed at identifying the period of the pattern behind which months of
# ...a year have 5 Sundays in them. More specifically, the goal of the script
# ...is to clearly illustrate which years have 4 5-Sunday months, and which
# ...have 5 5-Sunday months.

# Requires that the user have installed the "tidyverse" R library.

# Modify these figures to change which years are analyzed
startYear <- 1018
endYear <- 2018

yearDifference <- endYear - startYear
yearCounterArray <- integer(12 * yearDifference)
dateString <- paste(startYear, "1-1", sep = "-")
currentDate <- as.Date(dateString)


yearCount <- integer(yearDifference)
yearObserved <- integer(yearDifference)
isLeapYear <- integer(yearDifference)


while(as.integer(format(currentDate, '%Y')) < endYear) {
  currentDateMonthIndex <- as.integer(format(currentDate, '%m')) - 1
  currentDateYearIndex <- as.integer(format(currentDate, '%Y'))
  trueYearIndex <- ((currentDateYearIndex - startYear) * 12) + 1
  currentIterativeIndex <- trueYearIndex + currentDateMonthIndex
  if (format(currentDate, '%A') == 'Sunday') {
    yearCounterArray[currentIterativeIndex] <- yearCounterArray[currentIterativeIndex] + 1
  }

  currentDate <- currentDate + 1
}

# matrix(yearCounterArray, ncol = 12, byrow = TRUE)



for(yearIndex in 0:(yearDifference-1)) {
  trueYearIndex <- yearIndex+1
  currentYearAnalyzed <- startYear + yearIndex
  yearObserved[trueYearIndex] <- currentYearAnalyzed
  if (currentYearAnalyzed %% 4 == 0 && currentYearAnalyzed %% 100 != 0) {
    isLeapYear[trueYearIndex] <- 1
  }
  for(monthIndex in 0:11) {
    currentIndex <- ((yearIndex*12) + monthIndex) + 1
    if (yearCounterArray[currentIndex] == 5) {
      yearCount[trueYearIndex] <- yearCount[trueYearIndex] + 1
    }
  }
}

# This section is for identifying pattern periodicity
# yearCount <- integer(yearDifference)
# yearObserved <- integer(yearDifference)
# isLeapYear <- integer(yearDifference)

currentYearForObservation <- 1
patternStartIndex <- -1
yearsBeforeIdentifiedAsPattern <- 100

yearCountdown <- yearsBeforeIdentifiedAsPattern
for(yearPatternIndex in 2:yearDifference) {
  if (yearCount[yearPatternIndex] == yearCount[currentYearForObservation]
      # & isLeapYear[yearPatternIndex] == isLeapYear[currentYearForObservation]
      ) {
    if (yearCountdown == yearsBeforeIdentifiedAsPattern) {
      patternStartIndex <- yearPatternIndex
    } else if (yearCountdown == 0) {
      print("SUCCESS")
      yearPatternIndex <- yearDifference + 1;
    }
    yearCountdown <- yearCountdown - 1
    currentYearForObservation <- currentYearForObservation + 1
  } else {
    patternStartIndex <- -1
    currentYearForObservation <- 1
    yearCountdown <- yearsBeforeIdentifiedAsPattern
  }
}

print(patternStartIndex)

# breakdownByYear <- data.frame(yearCount, yearObserved, isLeapYear)
# ggplot(data = breakdownByYear) + geom_point(mapping = aes(x = yearObserved, y = yearCount, color = isLeapYear))
# matrix(yearCount, ncol = 10, byrow = TRUE)