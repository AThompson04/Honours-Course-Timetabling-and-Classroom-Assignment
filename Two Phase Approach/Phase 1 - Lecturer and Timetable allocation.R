## Set working directory here
# setwd()

### Set the memory limit
memory.limit(size=14000)

#### Prepping the data ####
### Load the packages
library(Rglpk)
library(readxl)
library(xlsx)

### Loading the data
## Lecturer preferences
# Making module names the column names
preferences <- read_excel("lecturer preference.xlsx")
pref <- as.data.frame(preferences)
rownames(pref) <- pref[,1]
pref <- pref[,-1]

# Converting data frame into a matrix
preferences <- as.matrix(pref)

## Teaching hours
teaching_hours <- read_excel("teaching_hours.xlsx")
th <- as.data.frame(teaching_hours)
rownames(th) <- th[,1]
th <- th[,-1]
teaching_hours <- th

## Contact hours
contact_hours <- read_excel("contact hours.xlsx")
ch <- as.data.frame(contact_hours)
rownames(ch) <- ch[,1]
ch <- ch[,-1]
contact_hours <- ch

contact_hours1 <- ch[ch$Semester==1,]
contact_hours2 <- ch[ch$Semester==2,]
contact_hours0 <- ch[ch$Semester==0,]

# Module names per semester
module0 <- rownames(ch[ch$Semester==0,])
module1 <- rownames(ch[ch$Semester==1,])
module2 <- rownames(ch[ch$Semester==2,])

s0 <- length(module0)
s1 <- length(module1)
s2 <- length(module2)

# What module a lecturer can teach
L <- pref
for(i in 1:nrow(pref)){
  for(j in 1:ncol(pref)){
    if(L[i,j]>=1){
      L[i,j] <- 1
    }
  }
}
L <- as.data.frame(L)
L1 <- L[,module1]
L2 <- L[,module2]

## Teaching Restrictions per course
teaching_restrictions <- read_excel("teaching_restrictions.xlsx")
tr <- as.data.frame(teaching_restrictions)
rownames(tr) <- tr[,1]
tr <- tr[,-1]
teaching_restrictions <- tr

n <- as.numeric(nrow(pref)) # Number of lecturers
c <- ncol(pref) # Number of courses

# Lecturer availability per semester
semester_1 <- "~/lecturer_availablityS1.xlsx"
semester_2 <- "~/lecturer_availablityS2.xlsx"

## Semester 1
S1 <- list() # Empty list to load the semester 1 availability of each lecturer
lecturers1 <- matrix(0, nrow = n, ncol = 50) # Lecturer Availability Semester 1

# Load the lecturer availability sheet by sheet
for (i in 1:n){
  S1[[i]] <- read_xlsx(semester_1, sheet = i, col_names = TRUE)
} 

# Converting the list into vectors for each lecturer - starting with the Monday time slots and then moving to the Tuesday time slots and so on
for (i in 1:n) {
  x <- as.matrix(S1[[i]]) # Making the sheet a matrix
  x <- x[,-1] # Removing the lecturer names from column 1
  x <- as.vector(t(x)) # Converting the matrix into a vector
  x <- as.numeric(x) # Convert 1 and 0s to num type
  lecturers1[i,] <- x # Add to lecturers availablity matrix
}

row.names(lecturers1) <- row.names(pref)

## Semester 2
S2 <- list() # Empty list to load the semester 2 availability of each lecturer
lecturers2 <- matrix(0, nrow = n, ncol = 50) # Lecturer Availability Semester 1

# Load the lecturer availability sheet by sheet
for (i in 1:n){
  S2[[i]] <- read_xlsx(semester_2, sheet = i, col_names = TRUE)
} 

# Converting the list into vectors for each lecturer - starting with the Monday time slots and then moving to the Tuesday time slots and so on
for (i in 1:n) {
  x <- as.matrix(S2[[i]]) # Making the sheet a matrix
  x <- x[,-1] # Removing the lecturer names from column 1
  x <- as.vector(t(x)) # Converting the matrix into a vector
  x <- as.numeric(x) # Convert 1 and 0s to num type
  lecturers2[i,] <- x # Add to lecturers availability matrix
}

row.names(lecturers2) <- row.names(pref)

## Morning Preference
morning <- read_excel("morning_preference-noFriday.xlsx")
morn <- as.data.frame(morning)
rownames(morn) <- morn[,1]
morn <- morn[,-1]
morning <- as.matrix(morn)

# S1
c_morning <- t(matrix(as.vector(morning)))
pref_available <- matrix(0, nrow = 1, ncol = ncol(lecturers1)) # Multiplying the preference times by when lecturers for each module are available
for(j in 1:n){
  for(i in 1:s1){
    rough <- c_morning*lecturers1[j,]*L1[j,i]
    pref_available <- rbind(pref_available,rough)
  }
}
z1_morning1 <- pref_available[-1,]

# S2
c_morning <- t(matrix(as.vector(morning)))
pref_available <- matrix(0, nrow = 1, ncol = ncol(lecturers2)) # Multiplying the preference times by when lecturers for each module are available
for(j in 1:n){
  for(i in 1:s2){
    rough <- c_morning*lecturers2[j,]*L2[j,i]
    pref_available <- rbind(pref_available,rough)
  }
}
z1_morning2 <- pref_available[-1,]

## Afternoon Preference
afternoon <- read_excel("afternoon_preference-noFriday.xlsx")
after <- as.data.frame(afternoon)
rownames(after) <- after[,1]
after <- after[,-1]
afternoon <- as.matrix(after)

# S1
c_afternoon <- t(matrix(as.vector(afternoon)))
pref_available <- matrix(0, nrow = 1, ncol = ncol(lecturers1)) # Multiplying the preference times by when lecturers for each module are available
for(j in 1:n){
  for(i in 1:s1){
    rough <- c_afternoon*lecturers1[j,]*L1[j,i]
    pref_available <- rbind(pref_available,rough)
  }
}
z1_afternoon1 <- pref_available[-1,]

# S2
c_afternoon <- t(matrix(as.vector(afternoon)))
pref_available <- matrix(0, nrow = 1, ncol = ncol(lecturers2)) # Multiplying the preference times by when lecturers for each module are available
for(j in 1:n){
  for(i in 1:s2){
    rough <- c_afternoon*lecturers2[j,]*L2[j,i]
    pref_available <- rbind(pref_available,rough)
  }
}
z1_afternoon2 <- pref_available[-1,]

## Z2
pref1 <- pref[,ch$Semester==1]
pref2 <- pref[,ch$Semester==2]
pref0 <- pref[,ch$Semester==0]
lecturer_pref <- c(as.vector(t(pref1)),as.vector(t(pref2)),as.vector(t(pref0)))

#### Model Function ####
## Creating the lecturer and timetable assignment model as a function
coef_func <- function(contact_hours, s1pref, s2pref, lecturer_pref, goals, weights, epsilon, lecturers1, lecturers2){
  # s1pref = z1 (when available, pref for time slot, change teach module)
  # s2pref = z1 (same as line above but for afternoons)
  # lecturer_pref = z2 (lecturer preferences for modules)
  # goal = the targets for z1 and z2
  # weights = the deviance weightings
  # epsilon = the penalty for the sum of the weighted deviances
  # Lecturers1 = availability of the lecturer in S1
  # Lecturers2 = availability of the lecturer in S2
  
  contact_hours1 <- ch[ch$Semester==1,]
  contact_hours2 <- ch[ch$Semester==2,]
  contact_hours0 <- ch[ch$Semester==0,]
  
  # Variables
  n <- dim(lecturers1)[1] # rows/number of lecturers
  C0 <- nrow(contact_hours[contact_hours$Semester==0,]) # columns/number of modules in S0
  C1 <- nrow(contact_hours[contact_hours$Semester==1,]) # columns/number of modules in S1
  C2 <- nrow(contact_hours[contact_hours$Semester==2,]) # columns/number of modules in S2
  C <- nrow(contact_hours) # The total number of modules
  Col <- dim(s1pref)[2] # columns/number of timeslots
  semester1 <- Col*C1*n # Number of Semester 2 decision variables
  semester2 <- Col*C2*n # Number of Semester 2 decision variables
  lambda <- (C1+C2)*Col # Number of lambda decision variables
  module <- n*C # Number of module - lecturer allocation auxiliary decision variables
  delta <- n* C # Number of delta linking variables
  ts <- (Col/5) - 1 # Number of timeslots a day minus 1
  g <- length(goals)
  
  coef_matrix <- matrix(0, C1+C2+Col*n+Col*n+C1*Col+C2*Col+C1+C2, semester1+semester2+lambda+module+delta+3)
  
  # (11) Each course will have the correct number of classes for semester 1
  for(i in 1:C1){
    for(j in 1:n){
      for(k in 1:Col){
        coef_matrix[i, C1*Col*(j-1) + Col*(i-1) + k] <- 1
      }
    }
  }
  
  # (11) Each course will have the correct number of classes for semester 2
  for(i in 1:C2){
    for(j in 1:n){
      for(k in 1:Col){
        coef_matrix[C1+i, semester1 + C2*Col*(j-1) + Col*(i-1) + k] <- 1
      }
    }
  }
  
  # (12) Unique constraint for each time slot S1
  for(i in 1:n){
    for(j in 1:Col){
      for(k in 1:C1){
        coef_matrix[C1+C2+Col*(i-1)+j, Col*C1*(i-1) + Col*(k-1) + j] <- 1
      }
    }
  }
  
  # (13) Unique constraint for each time slot S2
  for(i in 1:n){
    for(j in 1:Col){
      for(k in 1:C2){
        coef_matrix[C1+C2+n*Col+Col*(i-1)+j, semester1 + Col*C2*(i-1) + Col*(k-1) + j] <- 1
      }
    }
  }
  
  # (14) All modules must be taught in the same time slot S1
  for(i in 1:C1){
    for(j in 1:Col){
      for(k in 1:n){
        coef_matrix[C1+C2+2*Col*n+Col*(i-1)+j, C1*Col*(k-1) + Col*(i-1) + j] <- lecturers1[k,j]
        coef_matrix[C1+C2+2*Col*n+Col*(i-1)+j, semester1+semester2+Col*(i-1)+j] <- -1*contact_hours1$`Num Teachers`[i]
      }
    }
  } 
  
  # (14) All modules must be taught in the same time slot S2
  for(i in 1:C2){
    for(j in 1:Col){
      for(k in 1:n){
        coef_matrix[C1+C2+2*Col*n+C1*Col+Col*(i-1)+j, semester1+C2*Col*(k-1) + Col*(i-1) + j] <- lecturers2[k,j]
        coef_matrix[C1+C2+2*Col*n+C1*Col+Col*(i-1)+j, semester1+semester2+Col*C1+Col*(i-1)+j] <- -1*contact_hours2$`Num Teachers`[i]
      }
    }
  }
  
  # (15) The number of allocated hours a week are allocated S1
  for(i in 1:C1){
    for(j in 1:Col){
      coef_matrix[C1+C2+2*Col*n+C1*Col+C2*Col+i, semester1+semester2+ Col*(i-1) + j] <- 1
    }
  }
  
  # (15) The number of allocated hours a week are allocated S2
  for(i in 1:C2){
    for(j in 1:Col){
      coef_matrix[C1+C2+2*Col*n+C1*Col+C2*Col+C1+i, semester1+semester2+ Col*C1 + Col*(i-1) + j] <- 1
    }
  }
  
  return(coef_matrix)
}

lambda_class_coef <- function(L1,L2,contact_hours, s1pref, s2pref, lecturer_pref, goals, weights, epsilon, lecturers1, lecturers2){
  # s1pref = z1 (when available, pref for time slot, change teach module)
  # s2pref = z1 (same as line above but for afternoons)
  # lecturer_pref = z2 (lecturer preferences for modules)
  # goal = the targets for z1 and z2
  # weights = the deviance weightings
  # epsilon = the penalty for the sum of the weighted deviances
  # Lecturers1 = availability of the lecturer in S1
  # Lecturers2 = availability of the lecturer in S2
  
  contact_hours1 <- ch[ch$Semester==1,]
  contact_hours2 <- ch[ch$Semester==2,]
  contact_hours0 <- ch[ch$Semester==0,]
  
  # Variables
  n <- dim(lecturers1)[1] # rows/number of lecturers
  C0 <- nrow(contact_hours[contact_hours$Semester==0,]) # columns/number of modules in S0
  C1 <- nrow(contact_hours[contact_hours$Semester==1,]) # columns/number of modules in S1
  C2 <- nrow(contact_hours[contact_hours$Semester==2,]) # columns/number of modules in S2
  C <- nrow(contact_hours) # The total number of modules
  Col <- dim(s1pref)[2] # columns/number of timeslots
  semester1 <- Col*C1*n # Number of Semester 2 decision variables
  semester2 <- Col*C2*n # Number of Semester 2 decision variables
  lambda <- (C1+C2)*Col # Number of lambda decision variables
  module <- n*C # Number of module - lecturer allocation auxiliary decision variables
  delta <- n* C # Number of delta linking variables
  ts <- (Col/5) - 1 # Number of timeslots a day minus 1
  g <- length(goals)
  
  # (16) Double Period S1
  # Consecutive sessions and single sessions with a day break
  rough <- matrix(0, nrow = 1, ncol = semester1+semester2+lambda+module+delta+3)
  
  for(j in 1:C1){
      if(contact_hours1$`Classes a Week`[j]==1 & contact_hours1$`Contact Hours`[j]==2){
        rough1 <- matrix(0, nrow = ts*5, ncol = semester1+semester2+lambda+module+delta+3)
        for(i in 1:5){
          for(k in 1:ts){
            if(k==1){ # Consecutive classes in the beginning of the day (If number of classes a week = 1)
              rough1[(i-1)*ts+k, semester1 + semester2 + (j-1)*Col + (i-1)*10 + k] <- 1
              rough1[(i-1)*ts+k, semester1 + semester2 + (j-1)*Col + (i-1)*10 + k+1] <- -1
            }else if(k<ts){ # Consecutive classes during the day (If number of classes a week = 1)
              rough1[(i-1)*ts+k, semester1 + semester2 + (j-1)*Col + (i-1)*10 + k] <- -1
              rough1[(i-1)*ts+k, semester1 + semester2 + (j-1)*Col + (i-1)*10 + k+1] <- 1
              rough1[(i-1)*ts+k, semester1 + semester2 + (j-1)*Col + (i-1)*10 + k+2] <- -1
            }else{ # Consecutive classes at the end of the day (If number of classes a week = 1)
              rough1[(i-1)*ts+k, semester1 + semester2 + (j-1)*Col + (i-1)*10 + k] <- -1
              rough1[(i-1)*ts+k, semester1 + semester2 + (j-1)*Col + (i-1)*10 + k+1] <- 1
            }
          }
        }
        rough <- rbind(rough, rough1) # Joining this modules consecutive session constraint coefficients to final
        
      }else if(contact_hours1$`Classes a Week`[j]==2 & contact_hours1$`Contact Hours`[j]==1){ # 7: Not have single lectures more than a day apart (If number of classes a week = 2)
        rough2 <- matrix(0, nrow = 4, ncol = semester1+semester2+lambda+module+delta+3)
        ts1 <- Col/5 # Number of time slots in a 1 day period
        ts2 <- (Col/5)*2 # Number of time slots over a 2 day period
        for(i in 1:4){
          for(k in 1:ts2){
            rough2[i, semester1 + semester2 + (j-1)*Col + (i-1)*ts1 + k] <- 1
          }
        }
        rough <- rbind(rough, rough2) # Joining the modules have a day between session if single lessons constraint coefficients to final
      }else {
        rough1 <- matrix(0, nrow = ts*5, ncol = semester1+semester2+lambda+module+delta+3)
        for(i in 1:5){
          for(k in 1:ts){
            if(k==1){ # Consecutive classes in the beginning of the day (If number of classes a week = 1)
              rough1[(i-1)*ts+k, semester1 + semester2 + (j-1)*Col + (i-1)*10 + k] <- 1
              rough1[(i-1)*ts+k, semester1 + semester2 + (j-1)*Col + (i-1)*10 + k+1] <- -1
            }else if(k<ts){ # Consecutive classes during the day (If number of classes a week = 1)
              rough1[(i-1)*ts+k, semester1 + semester2 + (j-1)*Col + (i-1)*10 + k] <- -1
              rough1[(i-1)*ts+k, semester1 + semester2 + (j-1)*Col + (i-1)*10 + k+1] <- 1
              rough1[(i-1)*ts+k, semester1 + semester2 + (j-1)*Col + (i-1)*10 + k+2] <- -1
            }else{ # Consecutive classes at the end of the day (If number of classes a week = 1)
              rough1[(i-1)*ts+k, semester1 + semester2 + (j-1)*Col + (i-1)*10 + k] <- -1
              rough1[(i-1)*ts+k, semester1 + semester2 + (j-1)*Col + (i-1)*10 + k+1] <- 1
            }
          }
        }
        rough <- rbind(rough, rough1) # Joining this modules consecutive session constraint coefficients to final
        
        rough2 <- matrix(0, nrow = 4, ncol = semester1+semester2+lambda+module+delta+3)
        ts1 <- Col/5 # Number of time slots in a 1 day period
        ts2 <- (Col/5)*2 # Number of time slots over a 2 day period
        for(i in 1:4){
          for(k in 1:ts2){
            rough2[i, semester1 + semester2 + (j-1)*Col + (i-1)*ts1 + k] <- 1
          }
        }
        rough <- rbind(rough, rough2)
      }
    }
  answer1 <- rough[-1,] # Removing first row of zeros
  
  # (16) Double Period S2
  # Consecutive sessions and single sessions with a day break
  rough <- matrix(0, nrow = 1, ncol = semester1+semester2+lambda+module+delta+3)
  
  for(j in 1:C2){
      if(contact_hours2$`Classes a Week`[j]==1 & contact_hours2$`Contact Hours`[j]==2){
        rough1 <- matrix(0, nrow = ts*5, ncol = semester1+semester2+lambda+module+delta+3)
        for(i in 1:5){
          for(k in 1:ts){
            if(k==1){ # Consecutive classes in the beginning of the day (If number of classes a week = 1)
              rough1[(i-1)*ts+k, semester1 + semester2 + C1*Col + (j-1)*Col + (i-1)*10 + k] <- 1
              rough1[(i-1)*ts+k, semester1 + semester2 + C1*Col +  (j-1)*Col + (i-1)*10 + k+1] <- -1
            }else if(k<ts){ # Consecutive classes during the day (If number of classes a week = 1)
              rough1[(i-1)*ts+k, semester1 + semester2 + C1*Col +  (j-1)*Col + (i-1)*10 + k] <- -1
              rough1[(i-1)*ts+k, semester1 + semester2 + C1*Col +  (j-1)*Col + (i-1)*10 + k+1] <- 1
              rough1[(i-1)*ts+k, semester1 + semester2 + C1*Col +  (j-1)*Col + (i-1)*10 + k+2] <- -1
            }else{ # Consecutive classes at the end of the day (If number of classes a week = 1)
              rough1[(i-1)*ts+k, semester1 + semester2 + C1*Col +  (j-1)*Col + (i-1)*10 + k] <- -1
              rough1[(i-1)*ts+k, semester1 + semester2 + C1*Col +  (j-1)*Col + (i-1)*10 + k+1] <- 1
            }
          }
        }
        rough <- rbind(rough, rough1) # Joining this modules consecutive session constraint coefficients to final
        
      }else if(contact_hours2$`Classes a Week`[j]==2 & contact_hours2$`Contact Hours`[j]==1){ # 7: Not have single lectures more than a day apart (If number of classes a week = 2)
        rough2 <- matrix(0, nrow = 4, ncol = semester1+semester2+lambda+module+delta+3)
        ts1 <- Col/5 # Number of time slots in a 1 day period
        ts2 <- (Col/5)*2 # Number of time slots over a 2 day period
        for(i in 1:4){
          for(k in 1:ts2){
            rough2[i, semester1 + semester2 + C1*Col + (j-1)*Col + (i-1)*ts1 + k] <- 1
          }
        }
        rough <- rbind(rough, rough2) # Joining the modules have a day between session if single lessons constraint coefficients to final
      }else {
        rough1 <- matrix(0, nrow = ts*5, ncol = semester1+semester2+lambda+module+delta+3)
        for(i in 1:5){
          for(k in 1:ts){
            if(k==1){ # Consecutive classes in the beginning of the day (If number of classes a week = 1)
              rough1[(i-1)*ts+k, semester1 + semester2 + C1*Col + (j-1)*Col + (i-1)*10 + k] <- 1
              rough1[(i-1)*ts+k, semester1 + semester2 + C1*Col + (j-1)*Col + (i-1)*10 + k+1] <- -1
            }else if(k<ts){ # Consecutive classes during the day (If number of classes a week = 1)
              rough1[(i-1)*ts+k, semester1 + semester2 + C1*Col + (j-1)*Col + (i-1)*10 + k] <- -1
              rough1[(i-1)*ts+k, semester1 + semester2 + C1*Col + (j-1)*Col + (i-1)*10 + k+1] <- 1
              rough1[(i-1)*ts+k, semester1 + semester2 + C1*Col + (j-1)*Col + (i-1)*10 + k+2] <- -1
            }else{ # Consecutive classes at the end of the day (If number of classes a week = 1)
              rough1[(i-1)*ts+k, semester1 + semester2 + C1*Col + (j-1)*Col + (i-1)*10 + k] <- -1
              rough1[(i-1)*ts+k, semester1 + semester2 + C1*Col + (j-1)*Col + (i-1)*10 + k+1] <- 1
            }
          }
        }
        rough <- rbind(rough, rough1) # Joining this modules consecutive session constraint coefficients to final
        
        rough2 <- matrix(0, nrow = 4, ncol = semester1+semester2+lambda+module+delta+3)
        ts1 <- Col/5 # Number of time slots in a 1 day period
        ts2 <- (Col/5)*2 # Number of time slots over a 2 day period
        for(i in 1:4){
          for(k in 1:ts2){
            rough2[i, semester1 + semester2 + C1*Col + (j-1)*Col + (i-1)*ts1 + k] <- 1
          }
        }
        rough <- rbind(rough, rough2)
      }
    }
  answer2 <- rough[-1,] # Removing first row of zeros
  
  answer3 <- matrix(0, nrow = (2*Col), ncol = semester1+semester2+lambda+module+delta+3)
  # Uniqueness Constraint for the lambdas S1
  for(i in 1:Col){
    for(j in 1:C1){
      answer3[i, semester1+semester2+Col*(j-1)+i] <- 1
    }
  }
  
  # Uniqueness Constraint for the lambdas S2
  for(i in 1:Col){
    for(j in 1:C2){
      answer3[Col + i, semester1+semester2+Col*C1+Col*(j-1)+i] <- 1
    }
  }
  
  answer <- rbind(answer1,answer2,answer3)
  return(answer)
}

lect_coef <- function(contact_hours, s1pref, s2pref, lecturer_pref, goals, weights, epsilon, lecturers1, lecturers2){
  # s1pref = z1 (when available, pref for time slot, change teach module)
  # s2pref = z1 (same as line above but for afternoons)
  # lecturer_pref = z2 (lecturer preferences for modules)
  # goal = the targets for z1 and z2
  # weights = the deviance weightings
  # epsilon = the penalty for the sum of the weighted deviances
  # Lecturers1 = availability of the lecturer in S1
  # Lecturers2 = availability of the lecturer in S2
  
  contact_hours1 <- ch[ch$Semester==1,]
  contact_hours2 <- ch[ch$Semester==2,]
  contact_hours0 <- ch[ch$Semester==0,]
  
  # Variables
  n <- dim(lecturers1)[1] # rows/number of lecturers
  C0 <- nrow(contact_hours[contact_hours$Semester==0,]) # columns/number of modules in S0
  C1 <- nrow(contact_hours[contact_hours$Semester==1,]) # columns/number of modules in S1
  C2 <- nrow(contact_hours[contact_hours$Semester==2,]) # columns/number of modules in S2
  C <- nrow(contact_hours) # The total number of modules
  Col <- dim(s1pref)[2] # columns/number of timeslots
  semester1 <- Col*C1*n # Number of Semester 2 decision variables
  semester2 <- Col*C2*n # Number of Semester 2 decision variables
  lambda <- (C1+C2)*Col # Number of lambda decision variables
  module <- n*C # Number of module - lecturer allocation auxiliary decision variables
  delta <- n* C # Number of delta linking variables
  ts <- (Col/5) - 1 # Number of timeslots a day minus 1
  g <- length(goals)
  lect_aloc <- matrix(0, C+n*C+2*n+n*C1+n*C2+2*n*C+C, semester1+semester2+lambda+module+delta+3)
  
  # (23) Each course will have the correct number of classes
  for(i in 1:C1){
    for(j in 1:n){
      lect_aloc[i, semester1 + semester2 + lambda + C1*(j-1) + i] <- 1
    }
  }
  
  for(i in 1:C2){
    for(j in 1:n){
      lect_aloc[C1 + i, semester1 + semester2 + lambda + n*C1 + C2*(j-1) + i] <- 1
    }
  }
  
  for(i in 1:C0){
    for(j in 1:n){
      lect_aloc[C1 + C2 + i, semester1 + semester2 + lambda + n*C1 + n*C2 + C0*(j-1) + i] <- 1
    }
  }
  
  # (24) Teaching Restriction per course by lecturer
  for(i in 1:(n*C)){
    for(j in 1:(n*C)){
      if(i==j){
        lect_aloc[C + i, semester1 + semester2 + lambda + j] <- 1
      }
    }
  }
  
  # (25) Each lecturer should teach a maximum number of hours
  for(i in 1:n){
    for(j in 1:C1){
      lect_aloc[(C+(n*C)+i), semester1 + semester2 + lambda + (C1*(i-1) + j)] <- contact_hours1$`Contact Hours`[j]
    }
    for(k in 1:C2){
      lect_aloc[(C+(n*C)+i), semester1 + semester2 + lambda + (C2*(i-1) + n*C1 + k)] <- contact_hours2$`Contact Hours`[k]
    }
    for(l in 1:C0){
      lect_aloc[(C+(n*C)+i), semester1 + semester2 + lambda + (C0*(i-1) + n*C1 + n*C2 + l)] <- contact_hours0$`Contact Hours`[l]
    }
  }  
  
  # (25) Each lecturer should teach a min number of hours
  for(i in 1:n){
    for(j in 1:C1){
      lect_aloc[(C+(n*C)+n+i), semester1 + semester2 + lambda + (C1*(i-1) + j)] <- contact_hours1$`Contact Hours`[j]
    }
    for(k in 1:C2){
      lect_aloc[(C+(n*C)+n+i), semester1 + semester2 + lambda + (C2*(i-1) + n*C1 + k)] <- contact_hours2$`Contact Hours`[k]
    }
    for(l in 1:C0){
      lect_aloc[(C+(n*C)+n+i), semester1 + semester2 + lambda + (C0*(i-1) + n*C1 + n*C2 + l)] <- contact_hours0$`Contact Hours`[l]
    }
  } 
  
  # (26) Linking constraint S1
  for(i in 1:n){
    for(j in 1:C1){
      for(l in 1:Col){  
        lect_aloc[C+n*C+2*n+C1*(i-1)+j, C1*Col*(i-1) + Col*(j-1) + l] <- 1/(contact_hours1$`Classes a Week`[j]*contact_hours1$`Contact Hours`[j])
      }
      lect_aloc[C+n*C+2*n+C1*(i-1)+j, semester1 + semester2 + lambda + module + C1*(i-1) + j] <- -1
    }
  }
  
  # (26) Linking constraint S2
  for(i in 1:n){
    for(k in 1:C2){
      for(l in 1:Col){
        lect_aloc[C+n*C+2*n+C2*(i-1)+n*C1+k, semester1 + C2*Col*(i-1) + Col*(k-1) + l] <- 1/(contact_hours2$`Classes a Week`[k]*contact_hours2$`Contact Hours`[k])
      }
      lect_aloc[C+n*C+2*n+C2*(i-1)+n*C1+k, semester1 + semester2 + lambda + module + n*C1 + C2*(i-1) + k] <- -1
    }
  }
  
  # (27) Maximum number of classes taught by a lecturer for module i S1
  for(i in 1:n){
    for(j in 1:C1){
      lect_aloc[C+(n*C)+2*n+n*C1+n*C2+(i-1)*C1+j, semester1 + semester2 + lambda + (i-1)*C1 + j] <- 1
      lect_aloc[C+(n*C)+2*n+n*C1+n*C2+(i-1)*C1+j, semester1 + semester2 + lambda + module + (i-1)*C1 + j] <- -1*contact_hours1$`Number of Classes`[j]
    }
  }
  
  # (27) Maximum number of classes taught by a lecturer for module i S2
  for(i in 1:n){
    for(j in 1:C2){
      lect_aloc[C+(n*C)+2*n+n*C1+n*C2+(i-1)*C2+n*C1+j, semester1 + semester2 + lambda + n*C1 + (i-1)*C2 + j] <- 1
      lect_aloc[C+(n*C)+2*n+n*C1+n*C2+(i-1)*C2+n*C1+j, semester1 + semester2 + lambda + module + n*C1+ (i-1)*C2 + j] <- -1*contact_hours2$`Number of Classes`[j]
    }
  }
  
  # (27) Maximum number of classes taught by a lecturer for module i S0
  for(i in 1:n){
    for(j in 1:C0){
      lect_aloc[C+(n*C)+2*n+n*C1+n*C2+(i-1)*C0+n*C1+n*C2+j, semester1 + semester2 + lambda + n*C1 + n*C2 + (i-1)*C0 + j] <- 1
      lect_aloc[C+(n*C)+2*n+n*C1+n*C2+(i-1)*C0+n*C1+n*C2+j, semester1 + semester2 + lambda + module + n*C1 + n*C2 + (i-1)*C0 + j] <- -1*contact_hours0$`Number of Classes`[j]
    }
  }
  
  # (27) Minimum number of classes taught by a lecturer for module i S1
  for(i in 1:n){
    for(j in 1:C1){
      lect_aloc[C+(n*C)+2*n+n*C1+n*C2+n*C+(i-1)*C1+j, semester1 + semester2 + lambda + (i-1)*C1 + j] <- 1
      lect_aloc[C+(n*C)+2*n+n*C1+n*C2+n*C+(i-1)*C1+j, semester1 + semester2 + lambda + module + (i-1)*C1 + j] <- -1*contact_hours1$`Min Classes`[j]
    }
  }
  
  # (27) Minimum number of classes taught by a lecturer for module i S2
  for(i in 1:n){
    for(j in 1:C2){
      lect_aloc[C+(n*C)+2*n+n*C1+n*C2+n*C+(i-1)*C2+n*C1+j, semester1 + semester2 + lambda + n*C1 + (i-1)*C2 + j] <- 1
      lect_aloc[C+(n*C)+2*n+n*C1+n*C2+n*C+(i-1)*C2+n*C1+j, semester1 + semester2 + lambda + module + n*C1+ (i-1)*C2 + j] <- -1*contact_hours2$`Min Classes`[j]
    }
  }
  
  # (27) Minimum number of classes taught by a lecturer for module i S0
  for(i in 1:n){
    for(j in 1:C0){
      lect_aloc[C+(n*C)+2*n+n*C1+n*C2+n*C+(i-1)*C0+n*C1+n*C2+j, semester1 + semester2 + lambda + n*C1 + n*C2 + (i-1)*C0 + j] <- 1
      lect_aloc[C+(n*C)+2*n+n*C1+n*C2+n*C+(i-1)*C0+n*C1+n*C2+j, semester1 + semester2 + lambda + module + n*C1 + n*C2 + (i-1)*C0 + j] <- -1*contact_hours0$`Min Classes`[j]
    }
  }
  
  # (28) Required number of lecturers being allocated a class S1
  for(i in 1:C1){
    for(j in 1:n){
      lect_aloc[C+n*C+2*n+n*C1+n*C2+2*n*C+i, semester1 + semester2 + lambda + module + (j-1)*C1 + i] <- 1
    }
  } 
  
  # (28) Required number of lecturers being allocated a class S2
  for(i in 1:C2){
    for(j in 1:n){
      lect_aloc[C+n*C+2*n+n*C1+n*C2+2*n*C+C1+i, semester1 + semester2 + lambda + module + C1*n + (j-1)*C2 + i] <- 1
    }
  }
  
  # (28) Required number of lecturers being allocated a class S0
  for(i in 1:C0){
    for(j in 1:n){
      lect_aloc[C+n*C+2*n+n*C1+n*C2+2*n*C+C1+C2+i, semester1 + semester2 + lambda + module + C1*n + C2*n + (j-1)*C0 + i] <- 1
    }
  }
  
  return(lect_aloc)
}

lambda_rhs_signs <- function(contact_hours, s1pref, s2pref, lecturer_pref, goals, weights, epsilon, lecturers1, lecturers2){
  # s1pref = z1 (when available, pref for time slot, change teach module)
  # s2pref = z1 (same as line above but for afternoons)
  # lecturer_pref = z2 (lecturer preferences for modules)
  # goal = the targets for z1 and z2
  # weights = the deviance weightings
  # epsilon = the penalty for the sum of the weighted deviances
  # Lecturers1 = availability of the lecturer in S1
  # Lecturers2 = availability of the lecturer in S2
  
  contact_hours1 <- ch[ch$Semester==1,]
  contact_hours2 <- ch[ch$Semester==2,]
  contact_hours0 <- ch[ch$Semester==0,]
  
  # Variables
  n <- dim(lecturers1)[1] # rows/number of lecturers
  C0 <- nrow(contact_hours[contact_hours$Semester==0,]) # columns/number of modules in S0
  C1 <- nrow(contact_hours[contact_hours$Semester==1,]) # columns/number of modules in S1
  C2 <- nrow(contact_hours[contact_hours$Semester==2,]) # columns/number of modules in S2
  C <- nrow(contact_hours) # The total number of modules
  Col <- dim(s1pref)[2] # columns/number of timeslots
  semester1 <- Col*C1*n # Number of Semester 2 decision variables
  semester2 <- Col*C2*n # Number of Semester 2 decision variables
  lambda <- (C1+C2)*Col # Number of lambda decision variables
  module <- n*C # Number of module - lecturer allocation auxiliary decision variables
  delta <- n* C # Number of delta linking variables
  ts <- (Col/5) - 1 # Number of timeslots a day minus 1
  g <- length(goals)
  
  # Right had side
  rhs_coef <- c(t(contact_hours1$`Contact Hours`*contact_hours1$`Classes a Week`*contact_hours1$`Num Teachers`), t(contact_hours2$`Contact Hours`*contact_hours2$`Classes a Week`*contact_hours2$`Num Teachers`), matrix(1,2*n*Col,nrow=1), matrix(0,(C1*Col+C2*Col),nrow=1), t(contact_hours1$`Contact Hours`*contact_hours1$`Classes a Week`), t(contact_hours2$`Contact Hours`*contact_hours2$`Classes a Week`)) #, matrix(1,(n*C1*ts*5+n*C2*ts*5),nrow=1))
  rhs_lec <- c(t(contact_hours1$`Number of Classes`), t(contact_hours2$`Number of Classes`),t(contact_hours0$`Number of Classes`), as.vector(t(t(teaching_restrictions[ch$Semester==1,]))), as.vector(t(t(teaching_restrictions[ch$Semester==2,]))), as.vector(t(t(teaching_restrictions[ch$Semester==0,]))), t(teaching_hours$Max),t(teaching_hours$Min), matrix(0,(n*C1+n*C2+2*n*C),nrow=1), t(contact_hours1$`Num Teachers`),t(contact_hours2$`Num Teachers`),t(contact_hours0$`Num Teachers`))
  
  # Rhs and the signs for the answer1 matrix
  rhs_1 <- vector()
  sign_1 <- vector()
  t <- 0
  
  for(k in 1:n){
    for(j in 1:C1){
      if(contact_hours1$`Classes a Week`[j]==1& contact_hours1$`Contact Hours`[j]==2){
        t <- t + 1
        for(i in t:(t+ts*5-1)){
          sign_1[i] <- "<="
          rhs_1[i] <- 0
        }
        t <- t + ts*5 - 1
      }else if(contact_hours1$`Classes a Week`[j]==2 & contact_hours1$`Contact Hours`[j]==1){
        t <- t + 1
        for(i in t:(t+3)){
          sign_1[i] <- "<="
          rhs_1[i] <- 1
        }
        t <- t + 3
      }else{
        t <- t + 1
        for(i in t:(t+ts*5-1)){
          sign_1[i] <- "<="
          rhs_1[i] <- 0
        }
        t <- t + ts*5
        for(i in t:(t+3)){
          sign_1[i] <- "<="
          rhs_1[i] <- 2
        }
        t <- t + 3
      }
    }
  }
  
  # Rhs and the signs for the answer2 matrix
  rhs_2 <- vector()
  sign_2 <- vector()
  t <- 0
  
  for(k in 1:n){
    for(j in 1:C2){
      if(contact_hours2$`Classes a Week`[j]==1& contact_hours2$`Contact Hours`[j]==2){
        t <- t + 1
        for(i in t:(t+ts*5-1)){
          sign_2[i] <- "<="
          rhs_2[i] <- 0
        }
        t <- t + ts*5 - 1
      }else if(contact_hours2$`Classes a Week`[j]==2 & contact_hours2$`Contact Hours`[j]==1){
        t <- t + 1
        for(i in t:(t+3)){
          sign_2[i] <- "<="
          rhs_2[i] <- 1
        }
        t <- t + 3
      }else{
        t <- t + 1
        for(i in t:(t+ts*5-1)){
          sign_2[i] <- "<="
          rhs_2[i] <- 0
        }
        t <- t + ts*5
        for(i in t:(t+3)){
          sign_2[i] <- "<="
          rhs_2[i] <- 2
        }
        t <- t + 3
      }
    }
  }
  
  class_signs <- c(sign_1,sign_2)
  class_rhs <- c(rhs_1,rhs_2)
  
  # Rhs and the signs for the answer1 matrix
  rhs_1 <- vector()
  sign_1 <- vector()
  t <- 0
  
  for(j in 1:C1){
      if(contact_hours1$`Classes a Week`[j]==1& contact_hours1$`Contact Hours`[j]==2){
        t <- t + 1
        for(i in t:(t+ts*5-1)){
          sign_1[i] <- "<="
          rhs_1[i] <- 0
        }
        t <- t + ts*5 - 1
      }else if(contact_hours1$`Classes a Week`[j]==2 & contact_hours1$`Contact Hours`[j]==1){
        t <- t + 1
        for(i in t:(t+3)){
          sign_1[i] <- "<="
          rhs_1[i] <- 1
        }
        t <- t + 3
      }else{
        t <- t + 1
        for(i in t:(t+ts*5-1)){
          sign_1[i] <- "<="
          rhs_1[i] <- 0
        }
        t <- t + ts*5
        for(i in t:(t+3)){
          sign_1[i] <- "<="
          rhs_1[i] <- 2
        }
        t <- t + 3
      }
  }

  
  # Rhs and the signs for the answer2 matrix
  rhs_2 <- vector()
  sign_2 <- vector()
  t <- 0
  
  for(j in 1:C2){
      if(contact_hours2$`Classes a Week`[j]==1& contact_hours2$`Contact Hours`[j]==2){
        t <- t + 1
        for(i in t:(t+ts*5-1)){
          sign_2[i] <- "<="
          rhs_2[i] <- 0
        }
        t <- t + ts*5 - 1
      }else if(contact_hours2$`Classes a Week`[j]==2 & contact_hours2$`Contact Hours`[j]==1){
        t <- t + 1
        for(i in t:(t+3)){
          sign_2[i] <- "<="
          rhs_2[i] <- 1
        }
        t <- t + 3
      }else{
        t <- t + 1
        for(i in t:(t+ts*5-1)){
          sign_2[i] <- "<="
          rhs_2[i] <- 0
        }
        t <- t + ts*5
        for(i in t:(t+3)){
          sign_2[i] <- "<="
          rhs_2[i] <- 2
        }
        t <- t + 3
      }
  }
  
  rhs <- c(rhs_coef, rhs_1, rhs_2, rep(1,2*Col), rhs_lec)
  
  # Signs
  signs_coef <- c(rep("==", C1+C2), rep("<=", 2*Col*n), rep("==", C1*Col+C2*Col+C1+C2)) #, rep("<=", n*C1*ts*5+n*C2*ts*5))
  signs_lec <- c(rep("==", C), rep("<=", n*C), rep("<=", n), rep(">=", n), rep("==", (n*C1+n*C2)), rep("<=", n*C), rep(">=", n*C), rep("==", C))
  
  signs <- c(signs_coef, sign_1, sign_2, rep("<=", 2*Col), signs_lec)
  
  answer <- rbind(rhs, signs)
  return(answer)
}

goals <- matrix(c(400,600), nrow = 1) # Work out the goal
weights <- matrix(c(0.5,0.8), nrow = 1) # Work out the deviance weighting
epsilon <- 0.1

coef_matrix <- coef_func(contact_hours, z1_morning1, z1_morning2, lecturer_pref, goals, weights, epsilon, lecturers1, lecturers2)

lambda_class_matrix <- lambda_class_coef(L1,L2,contact_hours, z1_morning1, z1_morning2, lecturer_pref, goals, weights, epsilon, lecturers1, lecturers2)

lect_aloc <- lect_coef(contact_hours, z1_morning1, z1_morning2, lecturer_pref, goals, weights, epsilon, lecturers1, lecturers2)

lambda_rhs_signs_answer <- lambda_rhs_signs(contact_hours, z1_morning1, z1_morning2, lecturer_pref, goals, weights, epsilon, lecturers1, lecturers2)
lambda_rhs_other <- lambda_rhs_signs_answer[1,]
lambda_signs_other <- lambda_rhs_signs_answer[2,]

lt_Cheb <- function(coef_matrix, lambda_class_matrix, lect_aloc, rhs_other, signs_other, contact_hours, s1pref, s2pref, lecturer_pref, goals, weights, epsilon, lecturers1, lecturers2){
  # s1pref = z1 (when available, pref for time slot, change teach module)
  # s2pref = z1 (same as line above but for afternoons)
  # lecturer_pref = z2 (lecturer preferences for modules)
  # goal = the targets for z1 and z2
  # weights = the deviance weightings
  # epsilon = the penalty for the sum of the weighted deviances
  # Lecturers1 = availability of the lecturer in S1
  # Lecturers2 = availability of the lecturer in S2
  
  contact_hours1 <- ch[ch$Semester==1,]
  contact_hours2 <- ch[ch$Semester==2,]
  contact_hours0 <- ch[ch$Semester==0,]
  
  # Variables
  n <- dim(lecturers1)[1] # rows/number of lecturers
  C0 <- nrow(contact_hours[contact_hours$Semester==0,]) # columns/number of modules in S0
  C1 <- nrow(contact_hours[contact_hours$Semester==1,]) # columns/number of modules in S1
  C2 <- nrow(contact_hours[contact_hours$Semester==2,]) # columns/number of modules in S2
  C <- nrow(contact_hours) # The total number of modules
  Col <- dim(s1pref)[2] # columns/number of timeslots
  semester1 <- Col*C1*n # Number of Semester 2 decision variables
  semester2 <- Col*C2*n # Number of Semester 2 decision variables
  lambda <- (C1+C2)*Col # Number of lambda decision variables
  module <- n*C # Number of module - lecturer allocation auxiliary decision variables
  delta <- n* C # Number of delta linking variables
  ts <- (Col/5) - 1 # Number of timeslots a day minus 1
  g <- length(goals)
  
  ## Objective function coefficients
  # Coefficients
  coef <- c(as.vector(t(s1pref)), as.vector(t(s2pref)), rep(0, lambda), lecturer_pref, rep(0,delta))
  
  # Variable Type (Also a constraint)
  var_type <- c(rep("B", semester1+semester2 + (C1+C2)*Col), rep("I", C*n), rep("B", C*n))

  coef_matrix <- rbind(coef_matrix, lambda_class_matrix , lect_aloc)
  coef_matrix <- coef_matrix[,1:(semester1+semester2+lambda+module+delta)]
  
  rhs <- rhs_other
  signs <- signs_other
  
  ## Solution
  solution <- Rglpk_solve_LP(obj = coef, mat = coef_matrix, dir = signs, types = var_type, rhs=rhs, max = TRUE)
  return(solution)
}

# Some variables
n <- dim(lecturers1)[1] # rows/number of lecturers
C0 <- nrow(contact_hours[contact_hours$Semester==0,]) # columns/number of modules in S0
C1 <- nrow(contact_hours[contact_hours$Semester==1,]) # columns/number of modules in S1
C2 <- nrow(contact_hours[contact_hours$Semester==2,]) # columns/number of modules in S2
C <- nrow(contact_hours) # The total number of modules
Col <- dim(z1_morning1)[2] # columns/number of timeslots
semester1 <- Col*C1*n # Number of Semester 2 decision variables
semester2 <- Col*C2*n # Number of Semester 2 decision variables
lambda <- (C1+C2)*Col # Number of lambda decision variables
module <- n*C # Number of module - lecturer allocation auxiliary decision variables
delta <- n* C # Number of delta linking variables
g <- length(goals) # Number of goals

#### Morning Preference for Semester 1 and Semester 2 ####
system.time(test <- lt_Cheb(coef_matrix, lambda_class_matrix, lect_aloc, lambda_rhs_other, lambda_signs_other, contact_hours, z1_morning1, z1_morning2, lecturer_pref, goals, weights, epsilon, lecturers1, lecturers2))
test

#### Lecturer Assignment ####
# Running the model
l_assignment <- test$solution[(semester1+semester2+lambda+1):(semester1+semester2+lambda+module)]
l_assignmentS1 <- l_assignment[1:(n*C1)]
l_assignmentS1 <- matrix(l_assignmentS1, ncol = C1, byrow = TRUE)
rownames(l_assignmentS1) <- rownames(lecturers1)
colnames(l_assignmentS1) <- rownames(contact_hours1)
l_assignmentS1

l_assignmentS2 <- l_assignment[(n*C1+1):(n*C1+n*C2)]
l_assignmentS2 <- matrix(l_assignmentS2, ncol = C2, byrow = TRUE)
rownames(l_assignmentS2) <- rownames(lecturers2)
colnames(l_assignmentS2) <- rownames(contact_hours2)
l_assignmentS2

l_assignmentS0 <- l_assignment[(n*C1+n*C2+1):(n*C1+n*C2+n*C0)]
l_assignmentS0 <- matrix(l_assignmentS0, ncol = C0, byrow = TRUE)
rownames(l_assignmentS0) <- rownames(lecturers2)
colnames(l_assignmentS0) <- rownames(contact_hours0)
l_assignmentS0

l_assig <- cbind(l_assignmentS1, l_assignmentS2, l_assignmentS0)

# Checking the required number of classes have been assigned
col_sums(l_assig)
hours <- c(contact_hours1$`Contact Hours`,contact_hours2$`Contact Hours`,contact_hours0$`Contact Hours`)
# Checking lecturer hours have been adhered to
cbind(teaching_hours$Min,l_assig%*%hours,teaching_hours$Max) # Putting Maximum Constraints before Minimum constraints results in both being adhered to

# Which lecturer has been assigned to which module (number of classes)
for(i in 1:ncol(l_assig)){
  for(j in 1:nrow(l_assig)){
    if(l_assig[j,i]>=1){
      print(paste0(colnames(l_assig)[i], ": ", rownames(l_assig)[j], " (", l_assig[j,i],")"))
    }
  }
}

#### Timetable Assignment ####

# Individual lecturer timetables (Semester 1)
for(k in 1:n){
  timetable_m1 <- matrix(0, nrow = 10, ncol = 5)
  row.names(timetable_m1) <- c("8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm")
  colnames(timetable_m1) <- c("Mon", "Tues", "Wed", "Thurs", "Friday")
  m_assign <- matrix(test$solution[(Col*C1*(k-1)+1):(Col*C1*k)], ncol = Col, byrow = TRUE)
  for(i in 1:nrow(m_assign)){
    for(j in 1:ncol(m_assign)){
      if(m_assign[i,j]==1){
        if(j%%10!=0){
          timetable_m1[(j%%10),(j%/%10 + 1)] <- row.names(contact_hours1)[i]
        }else{
          timetable_m1[10,(j%/%10)] <- row.names(contact_hours1)[i]
        }
        
      }
    }
  }
  print(paste0("S1 Lecturer: ", rownames(lecturers1)[k]))
  print(timetable_m1)
}

# Individual lecturer timetables (Semester 2)
for(k in 1:n){
  timetable_m1 <- matrix(0, nrow = 10, ncol = 5)
  row.names(timetable_m1) <- c("8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm")
  colnames(timetable_m1) <- c("Mon", "Tues", "Wed", "Thurs", "Friday")
  m_assign <- matrix(test$solution[(semester1 + Col*C2*(k-1)+1):(semester1+Col*C2*k)], ncol = Col, byrow = TRUE)
  for(i in 1:nrow(m_assign)){
    for(j in 1:ncol(m_assign)){
      if(m_assign[i,j]==1){
        if(j%%10!=0){
          timetable_m1[(j%%10),(j%/%10 + 1)] <- row.names(contact_hours2)[i]
        }else{
          timetable_m1[10,(j%/%10)] <- row.names(contact_hours2)[i]
        }
        
      }
    }
  }
  print(paste0("S2 Lecturer: ", rownames(lecturers1)[k]))
  print(timetable_m1)
}


# Whole timetable Semester 1
both_semesters <- test$solution[(semester1+semester2+1):(semester1+semester2+lambda)]
sem_1 <- both_semesters[1:(C1*Col)]
sem_2 <- both_semesters[(C1*Col+1):(C1*Col+C2*Col)]

sem_1 <- matrix(sem_1, ncol = Col, byrow = TRUE)
row.names(sem_1) <- module1
write.xlsx(sem_1, "venue_s1.xlsx")

sem_2 <- matrix(sem_2, ncol = Col, byrow = TRUE)
row.names(sem_2) <- module2
write.xlsx(sem_2, "venue_s2.xlsx")

timetable_s1 <- matrix(0, nrow = 10, ncol = 5)
row.names(timetable_s1) <- c("8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm")
colnames(timetable_s1) <- c("Mon", "Tues", "Wed", "Thurs", "Friday")
for(i in 1:nrow(sem_1)){
  for(j in 1:ncol(sem_1)){
    if(sem_1[i,j]==1){
      if(j%%10!=0){
        timetable_s1[(j%%10),(j%/%10 + 1)] <- row.names(contact_hours1)[i]
      }else{
        timetable_s1[10,(j%/%10)] <- row.names(contact_hours1)[i]
      }
      
    }
  }
}
# Semester 1 timetable
timetable_s1

timetable_s2 <- matrix(0, nrow = 10, ncol = 5)
row.names(timetable_s2) <- c("8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm")
colnames(timetable_s2) <- c("Mon", "Tues", "Wed", "Thurs", "Friday")
for(i in 1:nrow(sem_2)){
  for(j in 1:ncol(sem_2)){
    if(sem_2[i,j]==1){
      if(j%%10!=0){
        timetable_s2[(j%%10),(j%/%10 + 1)] <- row.names(contact_hours2)[i]
      }else{
        timetable_s2[10,(j%/%10)] <- row.names(contact_hours2)[i]
      }
    }
  }
}
# Semester 2 timetable
timetable_s2

# Timetable objective function value
t(test$solution[1:(semester1+semester2)])%*%c(as.vector(t(z1_morning1)), as.vector(t(z1_morning2)))

# Lecturer objective function value
lec <- test$solution[(semester1+semester2+lambda+1):(semester1+semester2+lambda+module)]
t(l_assignment)%*%as.vector(t(lecturer_pref))
length(lec)
length(as.vector(t(pref)))

#### After Preference for sem 1 and morning preference for sem 2 ####
# Running the model
system.time(testa1m2 <- lt_Cheb(coef_matrix, lambda_class_matrix, lect_aloc, lambda_rhs_other, lambda_signs_other, contact_hours, z1_afternoon1, z1_morning2, lecturer_pref, goals, weights, epsilon, lecturers1, lecturers2))
testa1m2

#### Lecturer Assignment ####
l_assignmenta1m2 <- testa1m2$solution[(semester1+semester2+lambda+1):(semester1+semester2+lambda+module)]
l_assignmentS1a1m2 <- l_assignmenta1m2[1:(n*C1)]
l_assignmentS1a1m2 <- matrix(l_assignmentS1a1m2, ncol = C1, byrow = TRUE)
rownames(l_assignmentS1a1m2) <- rownames(lecturers1)
colnames(l_assignmentS1a1m2) <- rownames(contact_hours1)
l_assignmentS1a1m2

l_assignmentS2a1m2 <- l_assignmenta1m2[(n*C1+1):(n*C1+n*C2)]
l_assignmentS2a1m2 <- matrix(l_assignmentS2a1m2, ncol = C2, byrow = TRUE)
rownames(l_assignmentS2a1m2) <- rownames(lecturers2)
colnames(l_assignmentS2a1m2) <- rownames(contact_hours2)
l_assignmentS2a1m2

l_assignmentS0a1m2 <- l_assignmenta1m2[(n*C1+n*C2+1):(n*C1+n*C2+n*C0)]
l_assignmentS0a1m2 <- matrix(l_assignmentS0a1m2, ncol = C0, byrow = TRUE)
rownames(l_assignmentS0a1m2) <- rownames(lecturers2)
colnames(l_assignmentS0a1m2) <- rownames(contact_hours0)
l_assignmentS0a1m2

l_assiga1m2 <- cbind(l_assignmentS1a1m2, l_assignmentS2a1m2, l_assignmentS0a1m2)

# Checking the required number of classes have been assigned per module
col_sums(l_assiga1m2)
hours <- c(contact_hours1$`Contact Hours`,contact_hours2$`Contact Hours`,contact_hours0$`Contact Hours`)
# Checking the required number of hours has been adhered to
cbind(teaching_hours$Min,l_assiga1m2%*%hours,teaching_hours$Max) # Putting Maximum Constraints before Minimum constraints results in both being adhered to

# What lecturer has been assigned to which module (Number of classes)
for(i in 1:ncol(l_assiga1m2)){
  for(j in 1:nrow(l_assiga1m2)){
    if(l_assig[j,i]>=1){
      print(paste0(colnames(l_assiga1m2)[i], ": ", rownames(l_assiga1m2)[j], " (", l_assiga1m2[j,i],")"))
    }
  }
}

#### Timetable Assignment ####
# Individual lecturer timetables (Semester 1)
for(k in 1:n){
  timetable_ma1m2 <- matrix(0, nrow = 10, ncol = 5)
  row.names(timetable_ma1m2) <- c("8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm")
  colnames(timetable_ma1m2) <- c("Mon", "Tues", "Wed", "Thurs", "Friday")
  m_assigna1m2 <- matrix(testa1m2$solution[(Col*C1*(k-1)+1):(Col*C1*k)], ncol = Col, byrow = TRUE)
  for(i in 1:nrow(m_assigna1m2)){
    for(j in 1:ncol(m_assigna1m2)){
      if(m_assigna1m2[i,j]==1){
        if(j%%10!=0){
          timetable_ma1m2[(j%%10),(j%/%10 + 1)] <- row.names(contact_hours1)[i]
        }else{
          timetable_ma1m2[10,(j%/%10)] <- row.names(contact_hours1)[i]
        }
        
      }
    }
  }
  print(paste0("S1 Lecturer: ", rownames(lecturers1)[k]))
  print(timetable_ma1m2)
}

# Individual lecturer timetables (Semester 2)
for(k in 1:n){
  timetable_ma1m2 <- matrix(0, nrow = 10, ncol = 5)
  row.names(timetable_ma1m2) <- c("8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm")
  colnames(timetable_ma1m2) <- c("Mon", "Tues", "Wed", "Thurs", "Friday")
  m_assigna1m2 <- matrix(testa1m2$solution[(semester1 + Col*C2*(k-1)+1):(semester1+Col*C2*k)], ncol = Col, byrow = TRUE)
  for(i in 1:nrow(m_assigna1m2)){
    for(j in 1:ncol(m_assigna1m2)){
      if(m_assigna1m2[i,j]==1){
        if(j%%10!=0){
          timetable_ma1m2[(j%%10),(j%/%10 + 1)] <- row.names(contact_hours2)[i]
        }else{
          timetable_ma1m2[10,(j%/%10)] <- row.names(contact_hours2)[i]
        }
        
      }
    }
  }
  print(paste0("S2 Lecturer: ", rownames(lecturers1)[k]))
  print(timetable_ma1m2)
}


# Whole timetable Semester 1
both_semestersa1m2 <- testa1m2$solution[(semester1+semester2+1):(semester1+semester2+lambda)]
sem_1a1m2 <- both_semestersa1m2[1:(C1*Col)]
sem_2a1m2 <- both_semestersa1m2[(C1*Col+1):(C1*Col+C2*Col)]

sem_1a1m2 <- matrix(sem_1a1m2, ncol = Col, byrow = TRUE)
row.names(sem_1a1m2) <- module1
write.xlsx(sem_1a1m2, "venue_s1a1m2.xlsx")

sem_2a1m2 <- matrix(sem_2a1m2, ncol = Col, byrow = TRUE)
row.names(sem_2a1m2) <- module2
write.xlsx(sem_2a1m2, "venue_s2a1m2.xlsx")

timetable_s1a1m2 <- matrix(0, nrow = 10, ncol = 5)
row.names(timetable_s1a1m2) <- c("8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm")
colnames(timetable_s1a1m2) <- c("Mon", "Tues", "Wed", "Thurs", "Friday")
for(i in 1:nrow(sem_1a1m2)){
  for(j in 1:ncol(sem_1a1m2)){
    if(sem_1a1m2[i,j]==1){
      if(j%%10!=0){
        timetable_s1a1m2[(j%%10),(j%/%10 + 1)] <- row.names(contact_hours1)[i]
      }else{
        timetable_s1a1m2[10,(j%/%10)] <- row.names(contact_hours1)[i]
      }
      
    }
  }
}
# Semester 1 timetable
timetable_s1a1m2

timetable_s2a1m2 <- matrix(0, nrow = 10, ncol = 5)
row.names(timetable_s2a1m2) <- c("8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm")
colnames(timetable_s2a1m2) <- c("Mon", "Tues", "Wed", "Thurs", "Friday")
for(i in 1:nrow(sem_2a1m2)){
  for(j in 1:ncol(sem_2a1m2)){
    if(sem_2a1m2[i,j]==1){
      if(j%%10!=0){
        timetable_s2a1m2[(j%%10),(j%/%10 + 1)] <- row.names(contact_hours2)[i]
      }else{
        timetable_s2a1m2[10,(j%/%10)] <- row.names(contact_hours2)[i]
      }
    }
  }
}
# Semester 2 timetable
timetable_s2a1m2

# Timetable objective function value
t(testa1m2$solution[1:(semester1+semester2)])%*%c(as.vector(t(z1_afternoon1)), as.vector(t(z1_morning2)))

# Lecturer objective function value
lec <- testa1m2$solution[(semester1+semester2+lambda+1):(semester1+semester2+lambda+module)]
t(l_assignmenta1m2)%*%as.vector(t(lecturer_pref))

#### Afternoon preferences for both semester ####
## Running the model
system.time(testa1a2 <- lt_Cheb(coef_matrix, lambda_class_matrix, lect_aloc, lambda_rhs_other, lambda_signs_other, contact_hours, z1_afternoon1, z1_afternoon2, lecturer_pref, goals, weights, epsilon, lecturers1, lecturers2))
testa1a2

#### Lecturer Assignment ####
l_assignmenta1a2 <- testa1a2$solution[(semester1+semester2+lambda+1):(semester1+semester2+lambda+module)]
l_assignmentS1a1a2 <- l_assignmenta1a2[1:(n*C1)]
l_assignmentS1a1a2 <- matrix(l_assignmentS1a1a2, ncol = C1, byrow = TRUE)
rownames(l_assignmentS1a1a2) <- rownames(lecturers1)
colnames(l_assignmentS1a1a2) <- rownames(contact_hours1)
l_assignmentS1a1a2

l_assignmentS2a1a2 <- l_assignmenta1a2[(n*C1+1):(n*C1+n*C2)]
l_assignmentS2a1a2 <- matrix(l_assignmentS2a1a2, ncol = C2, byrow = TRUE)
rownames(l_assignmentS2a1a2) <- rownames(lecturers2)
colnames(l_assignmentS2a1a2) <- rownames(contact_hours2)
l_assignmentS2a1a2

l_assignmentS0a1a2 <- l_assignmenta1a2[(n*C1+n*C2+1):(n*C1+n*C2+n*C0)]
l_assignmentS0a1a2 <- matrix(l_assignmentS0a1a2, ncol = C0, byrow = TRUE)
rownames(l_assignmentS0a1a2) <- rownames(lecturers2)
colnames(l_assignmentS0a1a2) <- rownames(contact_hours0)
l_assignmentS0a1a2

l_assiga1a2 <- cbind(l_assignmentS1a1a2, l_assignmentS2a1a2, l_assignmentS0a1a2)

# Checking the required number of classes have been allocated
col_sums(l_assiga1a2)
hours <- c(contact_hours1$`Contact Hours`,contact_hours2$`Contact Hours`,contact_hours0$`Contact Hours`)
# Checking the required number of hours have been adhered to
cbind(teaching_hours$Min,l_assiga1a2%*%hours,teaching_hours$Max) # Putting Maximum Constraints before Minimum constraints results in both being adhered to

# Which lecturer has been assigned to which module (Number of classes)
for(i in 1:ncol(l_assiga1a2)){
  for(j in 1:nrow(l_assiga1a2)){
    if(l_assiga1a2[j,i]>=1){
      print(paste0(colnames(l_assiga1a2)[i], ": ", rownames(l_assiga1a2)[j], " (", l_assiga1a2[j,i],")"))
    }
  }
}

#### Timetable Assignment ####
# Individual lecturer timetables (Semester 1)
for(k in 1:n){
  timetable_m1a1a2 <- matrix(0, nrow = 10, ncol = 5)
  row.names(timetable_m1a1a2) <- c("8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm")
  colnames(timetable_m1a1a2) <- c("Mon", "Tues", "Wed", "Thurs", "Friday")
  m_assigna1a2 <- matrix(testa1a2$solution[(Col*C1*(k-1)+1):(Col*C1*k)], ncol = Col, byrow = TRUE)
  for(i in 1:nrow(m_assigna1a2)){
    for(j in 1:ncol(m_assigna1a2)){
      if(m_assigna1a2[i,j]==1){
        if(j%%10!=0){
          timetable_m1a1a2[(j%%10),(j%/%10 + 1)] <- row.names(contact_hours1)[i]
        }else{
          timetable_m1a1a2[10,(j%/%10)] <- row.names(contact_hours1)[i]
        }
        
      }
    }
  }
  print(paste0("S1 Lecturer: ", rownames(lecturers1)[k]))
  print(timetable_m1a1a2)
}

# Individual lecturer timetables (Semester 2)
for(k in 1:n){
  timetable_m1a1a2 <- matrix(0, nrow = 10, ncol = 5)
  row.names(timetable_m1a1a2) <- c("8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm")
  colnames(timetable_m1a1a2) <- c("Mon", "Tues", "Wed", "Thurs", "Friday")
  m_assigna1a2 <- matrix(testa1a2$solution[(semester1 + Col*C2*(k-1)+1):(semester1+Col*C2*k)], ncol = Col, byrow = TRUE)
  for(i in 1:nrow(m_assigna1a2)){
    for(j in 1:ncol(m_assigna1a2)){
      if(m_assigna1a2[i,j]==1){
        if(j%%10!=0){
          timetable_m1a1a2[(j%%10),(j%/%10 + 1)] <- row.names(contact_hours2)[i]
        }else{
          timetable_m1a1a2[10,(j%/%10)] <- row.names(contact_hours2)[i]
        }
        
      }
    }
  }
  print(paste0("S2 Lecturer: ", rownames(lecturers1)[k]))
  print(timetable_m1a1a2)
}


# Whole timetable Semester 1
both_semestersa1a2 <- testa1a2$solution[(semester1+semester2+1):(semester1+semester2+lambda)]
sem_1a1a2 <- both_semestersa1a2[1:(C1*Col)]
sem_2a1a2 <- both_semestersa1a2[(C1*Col+1):(C1*Col+C2*Col)]

sem_1a1a2 <- matrix(sem_1a1a2, ncol = Col, byrow = TRUE)
row.names(sem_1a1a2) <- module1
write.xlsx(sem_1a1a2, "venue_s1a1a2.xlsx")

sem_2a1a2 <- matrix(sem_2a1a2, ncol = Col, byrow = TRUE)
row.names(sem_2a1a2) <- module2
write.xlsx(sem_2a1a2, "venue_s2a1a2.xlsx")

timetable_s1a1a2 <- matrix(0, nrow = 10, ncol = 5)
row.names(timetable_s1a1a2) <- c("8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm")
colnames(timetable_s1a1a2) <- c("Mon", "Tues", "Wed", "Thurs", "Friday")
for(i in 1:nrow(sem_1a1a2)){
  for(j in 1:ncol(sem_1a1a2)){
    if(sem_1a1a2[i,j]==1){
      if(j%%10!=0){
        timetable_s1a1a2[(j%%10),(j%/%10 + 1)] <- row.names(contact_hours1)[i]
      }else{
        timetable_s1a1a2[10,(j%/%10)] <- row.names(contact_hours1)[i]
      }
      
    }
  }
}
# Semester 1 Timetable
timetable_s1a1a2

timetable_s2a1a2 <- matrix(0, nrow = 10, ncol = 5)
row.names(timetable_s2a1a2) <- c("8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm")
colnames(timetable_s2a1a2) <- c("Mon", "Tues", "Wed", "Thurs", "Friday")
for(i in 1:nrow(sem_2a1a2)){
  for(j in 1:ncol(sem_2a1a2)){
    if(sem_2a1a2[i,j]==1){
      if(j%%10!=0){
        timetable_s2a1a2[(j%%10),(j%/%10 + 1)] <- row.names(contact_hours2)[i]
      }else{
        timetable_s2a1a2[10,(j%/%10)] <- row.names(contact_hours2)[i]
      }
    }
  }
}
# Semester 2 Timetable
timetable_s2a1a2

# Timetable objective function value
t(testa1a2$solution[1:(semester1+semester2)])%*%c(as.vector(t(z1_afternoon1)), as.vector(t(z1_afternoon2)))

# Lecturer objective  function value
leca1a2 <- testa1a2$solution[(semester1+semester2+lambda+1):(semester1+semester2+lambda+module)]
t(l_assignmenta1a2)%*%as.vector(t(lecturer_pref))

#### morning preferences for sem 1 and after pref for sem 2 ####
## Running the model
system.time(testm1a2 <- lt_Cheb(coef_matrix, lambda_class_matrix, lect_aloc, lambda_rhs_other, lambda_signs_other, contact_hours, z1_morning1, z1_afternoon2, lecturer_pref, goals, weights, epsilon, lecturers1, lecturers2))
testm1a2

#### Lecturer Assignment ####
l_assignmentm1a2  <- testm1a2$solution[(semester1+semester2+lambda+1):(semester1+semester2+lambda+module)]
l_assignmentS1m1a2  <- l_assignmentm1a2[1:(n*C1)]
l_assignmentS1m1a2 <- matrix(l_assignmentS1m1a2, ncol = C1, byrow = TRUE)
rownames(l_assignmentS1m1a2) <- rownames(lecturers1)
colnames(l_assignmentS1m1a2) <- rownames(contact_hours1)
l_assignmentS1m1a2

l_assignmentS2m1a2 <- l_assignmentm1a2[(n*C1+1):(n*C1+n*C2)]
l_assignmentS2m1a2 <- matrix(l_assignmentS2m1a2, ncol = C2, byrow = TRUE)
rownames(l_assignmentS2m1a2) <- rownames(lecturers2)
colnames(l_assignmentS2m1a2) <- rownames(contact_hours2)
l_assignmentS2m1a2

l_assignmentS0m1a2 <- l_assignmentm1a2[(n*C1+n*C2+1):(n*C1+n*C2+n*C0)]
l_assignmentS0m1a2 <- matrix(l_assignmentS0m1a2, ncol = C0, byrow = TRUE)
rownames(l_assignmentS0m1a2) <- rownames(lecturers2)
colnames(l_assignmentS0m1a2) <- rownames(contact_hours0)
l_assignmentS0m1a2

l_assigm1a2 <- cbind(l_assignmentS1m1a2, l_assignmentS2m1a2, l_assignmentS0m1a2)

# Checking the required number of classes have been allocated
col_sums(l_assigm1a2)
hours <- c(contact_hours1$`Contact Hours`,contact_hours2$`Contact Hours`,contact_hours0$`Contact Hours`)
# Checking the required number of hours have been adhered to (per lecturer)
cbind(teaching_hours$Min,l_assigm1a2%*%hours,teaching_hours$Max) # Putting Maximum Constraints before Minimum constraints results in both being adhered to

# Which lecturer has been assigned to which module (Number of classes)
for(i in 1:ncol(l_assigm1a2)){
  for(j in 1:nrow(l_assigm1a2)){
    if(l_assigm1a2[j,i]>=1){
      print(paste0(colnames(l_assigm1a2)[i], ": ", rownames(l_assigm1a2)[j], " (", l_assigm1a2[j,i],")"))
    }
  }
}

#### Timetable Assignment ####
# Individual lecturer timetables (Semester 1)
for(k in 1:n){
  timetable_m1m1a2 <- matrix(0, nrow = 10, ncol = 5)
  row.names(timetable_m1m1a2) <- c("8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm")
  colnames(timetable_m1m1a2) <- c("Mon", "Tues", "Wed", "Thurs", "Friday")
  m_assignm1a2 <- matrix(testm1a2$solution[(Col*C1*(k-1)+1):(Col*C1*k)], ncol = Col, byrow = TRUE)
  for(i in 1:nrow(m_assignm1a2)){
    for(j in 1:ncol(m_assignm1a2)){
      if(m_assignm1a2[i,j]==1){
        if(j%%10!=0){
          timetable_m1m1a2[(j%%10),(j%/%10 + 1)] <- row.names(contact_hours1)[i]
        }else{
          timetable_m1m1a2[10,(j%/%10)] <- row.names(contact_hours1)[i]
        }
        
      }
    }
  }
  print(paste0("S1 Lecturer: ", rownames(lecturers1)[k]))
  print(timetable_m1m1a2)
}

# Individual Lecturer timetables (Semester 2)
for(k in 1:n){
  timetable_m1m1a2 <- matrix(0, nrow = 10, ncol = 5)
  row.names(timetable_m1m1a2) <- c("8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm")
  colnames(timetable_m1m1a2) <- c("Mon", "Tues", "Wed", "Thurs", "Friday")
  m_assignm1a2 <- matrix(testm1a2$solution[(semester1 + Col*C2*(k-1)+1):(semester1+Col*C2*k)], ncol = Col, byrow = TRUE)
  for(i in 1:nrow(m_assignm1a2)){
    for(j in 1:ncol(m_assignm1a2)){
      if(m_assignm1a2[i,j]==1){
        if(j%%10!=0){
          timetable_m1m1a2[(j%%10),(j%/%10 + 1)] <- row.names(contact_hours2)[i]
        }else{
          timetable_m1m1a2[10,(j%/%10)] <- row.names(contact_hours2)[i]
        }
        
      }
    }
  }
  print(paste0("S2 Lecturer: ", rownames(lecturers1)[k]))
  print(timetable_m1m1a2)
}


# Whole timetable Semester 1
both_semestersm1a2 <- testm1a2$solution[(semester1+semester2+1):(semester1+semester2+lambda)]
sem_1m1a2 <- both_semestersm1a2[1:(C1*Col)]
sem_2m1a2 <- both_semestersm1a2[(C1*Col+1):(C1*Col+C2*Col)]

sem_1m1a2 <- matrix(sem_1m1a2, ncol = Col, byrow = TRUE)
row.names(sem_1m1a2) <- module1
write.xlsx(sem_1m1a2, "venue_s1m1a2.xlsx")

sem_2m1a2 <- matrix(sem_2m1a2, ncol = Col, byrow = TRUE)
row.names(sem_2m1a2) <- module2
write.xlsx(sem_2m1a2, "venue_s2m1a2.xlsx")

timetable_s1m1a2 <- matrix(0, nrow = 10, ncol = 5)
row.names(timetable_s1m1a2) <- c("8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm")
colnames(timetable_s1m1a2) <- c("Mon", "Tues", "Wed", "Thurs", "Friday")
for(i in 1:nrow(sem_1m1a2)){
  for(j in 1:ncol(sem_1m1a2)){
    if(sem_1m1a2[i,j]==1){
      if(j%%10!=0){
        timetable_s1m1a2[(j%%10),(j%/%10 + 1)] <- row.names(contact_hours1)[i]
      }else{
        timetable_s1m1a2[10,(j%/%10)] <- row.names(contact_hours1)[i]
      }
      
    }
  }
}
# Semester 1 Timetable
timetable_s1m1a2

timetable_s2m1a2 <- matrix(0, nrow = 10, ncol = 5)
row.names(timetable_s2m1a2) <- c("8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm")
colnames(timetable_s2m1a2) <- c("Mon", "Tues", "Wed", "Thurs", "Friday")
for(i in 1:nrow(sem_2m1a2)){
  for(j in 1:ncol(sem_2m1a2)){
    if(sem_2m1a2[i,j]==1){
      if(j%%10!=0){
        timetable_s2m1a2[(j%%10),(j%/%10 + 1)] <- row.names(contact_hours2)[i]
      }else{
        timetable_s2m1a2[10,(j%/%10)] <- row.names(contact_hours2)[i]
      }
    }
  }
}
# Semester 2 Timetable
timetable_s2m1a2

# Timetable objective function value
t(testm1a2$solution[1:(semester1+semester2)])%*%c(as.vector(t(z1_morning1)), as.vector(t(z1_afternoon2)))

# Lecturer objective function value
lec <- testm1a2$solution[(semester1+semester2+lambda+1):(semester1+semester2+lambda+module)]
t(l_assignmentm1a2)%*%as.vector(t(lecturer_pref))


