## Set working directory here
# setwd()

#### Load the packages ####
library(Rglpk)
library(readxl)
library(xlsx)
library(dplyr)

#### Load Data ####
## Module Allocation
allocation <- read_excel("Lecturer - Module Choice.xlsx")
alloc <- as.data.frame(allocation)
rownames(alloc) <- alloc[,1]
lecturers <- alloc[,1]
alloc <- alloc[,-1]
allocation <- as.matrix(alloc)

n <- as.numeric(nrow(allocation)) # Number of lecturers
c <- ncol(allocation) # Number of courses

# Semesters
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

### Contact hours
contact_hours <- read_excel("contact hours.xlsx")
ch <- as.data.frame(contact_hours)
rownames(ch) <- ch[,1]
ch <- ch[,-1]

contact_hours1 <- ch[ch$Semester==1,]
contact_hours2 <- ch[ch$Semester==2,]

### Availability for each module
module1 <- rownames(ch[ch$Semester==1,])
module2 <- rownames(ch[ch$Semester==2,])

alloc_1 <- alloc[,module1]
alloc_2 <- alloc[,module2]

## Semester 1
s1 <- length(module1)

semester1_availability <- matrix(0, nrow  = s1, ncol = ncol(lecturers1))
rownames(semester1_availability) <- module1

for(i in 1:s1){
  teaching <- vector() # Create a vector for allocated teachers row reference
  t <- 0 # Teaching index
  for(j in 1:n){
    if(alloc_1[j,i]>=1){ # If teacher is allocated they would teach more than one class
      t <- t + 1 
      teaching[t] <- j
    }
  }
  
  if(t == 1){
    avali <- t(as.matrix(lecturers1[teaching,]))
  }else{
    avali <- lecturers1[teaching,] # Time slots that the given teachers are available
  }
  
  for(k in 1:ncol(lecturers1)){
    if(sum(avali[,k])==t){
      semester1_availability[i,k] <- 1 # All lecturers available at once (t is the number of lecturers available)
    }
  }
}

## Semester 2
s2 <- length(module2)

semester2_availability <- matrix(0, nrow  = s2, ncol = ncol(lecturers2))
rownames(semester2_availability) <- module2

for(i in 1:s2){
  teaching <- vector() # Create a vector for allocated teachers row reference
  t <- 0 # Teaching index
  for(j in 1:n){
    if(alloc_2[j,i]>=1){ # If teacher is allocated they would teach more than one class
      t <- t + 1 
      teaching[t] <- j
    }
  }
  
  if(t == 1){
    avali <- t(as.matrix(lecturers2[teaching,]))
  }else{
    avali <- lecturers2[teaching,] # Time slots that the given teachers are available
  }
  
  for(k in 1:ncol(lecturers2)){
    if(sum(avali[,k])==t){
      semester2_availability[i,k] <- 1
    }
  }
}

#### Model ####
## Creating the module-time slot assignment model as a function
module_assignemnt <- function(c, contact_hours, available){
  # c = Preference weightings for timeslots
  # available = Module availability semester 1
  
  n <- dim(available)[1] # rows/number of modules
  Col <- dim(available)[2] # columns/number of timeslots
  
  ## Objective function coefficients
  # Coefficients
  coef <- as.vector(t(c))
  
  # Variable Type (Also a constraint)
  var_type <- rep("B", n*Col)
  
  ## Constraints
  # Right had side
  rhs_1 <- vector()
  sign_1 <- vector()
  t <- 0
  ts <- (Col/5) - 1 # Number of timeslots a day minus 1
  
  for(j in 1:n){
    if(contact_hours$`Classes a Week`[j]==1& contact_hours$`Contact Hours`[j]==2){
      t <- t + 1
      for(i in t:(t+ts*5-1)){
        sign_1[i] <- "<="
        rhs_1[i] <- 0
      }
      t <- t + ts*5 - 1
    }else if(contact_hours$`Classes a Week`[j]==2 & contact_hours$`Contact Hours`[j]==1){
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
  
  rhs <- c(t(contact_hours$`Contact Hours`*contact_hours$`Classes a Week`), t(rep(1, Col)), rep(1, ts*5*n), t(rhs_1))
  
  # Signs
  signs <- c(rep("==", n), rep("<=", Col), rep("<=", ts*5*n), sign_1)
  
  # Coefficients
  coef_matrix <- matrix(0, (n+Col+n*ts*5), n*Col)
  
  # 2: Number of classes must equal the number of contact hours per week that are needed
  for(i in 1:n){
    for(j in 1:Col){
      coef_matrix[i, (i-1)*Col + j] <- 1
    }
  }
  
  # 3: Unique constraint for each time slot
  for(i in 1:n){
    for(j in 1:Col){
      coef_matrix[(n+j), Col*(i-1) + j] <- 1
    }
  }

  # 4: Have an break between variables
  for(i in 1:n){
    for(j in 1:ts){
      for(k in 1:n){
        for(l in 1:5){
          if(k==i){
            coef_matrix[(n+Col) + (i-1)*ts*5 + (l-1)*ts +j, (k-1)*Col + (l-1)*ts + j] <- 1
          }else{
            coef_matrix[(n+Col) + (i-1)*ts*5 + (l-1)*ts +j, (k-1)*Col + (l-1)*ts + j + 1] <- 1
          }
        }
      }
    }
  }
    
  # Consecutive sessions and single sessions with a day break
  rough <- matrix(0, nrow = 1, ncol = Col*n)
  
  for(j in 1:n){
    if(contact_hours$`Classes a Week`[j]==1 & contact_hours$`Contact Hours`[j]==2){
      rough1 <- matrix(0, nrow = ts*5, ncol = Col*n)
      for(i in 1:5){
        for(k in 1:ts){
          if(k==1){ # Consecutive classes in the beginning of the day (If number of classes a week = 1)
            rough1[(i-1)*ts+k, (j-1)*Col + (i-1)*10 + k] <- 1
            rough1[(i-1)*ts+k, (j-1)*Col + (i-1)*10 + k+1] <- -1
          }else if(k<ts){ # Consecutive classes during the day (If number of classes a week = 1)
            rough1[(i-1)*ts+k, (j-1)*Col + (i-1)*10 + k] <- -1
            rough1[(i-1)*ts+k, (j-1)*Col + (i-1)*10 + k+1] <- 1
            rough1[(i-1)*ts+k, (j-1)*Col + (i-1)*10 + k+2] <- -1
          }else{ # Consecutive classes at the end of the day (If number of classes a week = 1)
            rough1[(i-1)*ts+k, (j-1)*Col + (i-1)*10 + k] <- -1
            rough1[(i-1)*ts+k, (j-1)*Col + (i-1)*10 + k+1] <- 1
          }
        }
      }
      rough <- rbind(rough, rough1) # Joining this modules consecutive session constraint coefficients to final
      
    }else if(contact_hours$`Classes a Week`[j]==2 & contact_hours$`Contact Hours`[j]==1){ # 7: Not have single lectures more than a day apart (If number of classes a week = 2)
      rough2 <- matrix(0, nrow = 4, ncol = Col*n)
      ts1 <- Col/5 # Number of time slots in a 1 day period
      ts2 <- (Col/5)*2 # Number of time slots over a 2 day period
      for(i in 1:4){
        for(k in 1:ts2){
          rough2[i, (j-1)*Col + (i-1)*ts1 + k] <- 1
        }
      }
      rough <- rbind(rough, rough2) # Joining the modules have a day between session if single lessons constraint coefficients to final
    }else {
      rough1 <- matrix(0, nrow = ts*5, ncol = Col*n)
      for(i in 1:5){
        for(k in 1:ts){
          if(k==1){ # Consecutive classes in the beginning of the day (If number of classes a week = 1)
            rough1[(i-1)*ts+k, (j-1)*Col + (i-1)*10 + k] <- 1
            rough1[(i-1)*ts+k, (j-1)*Col + (i-1)*10 + k+1] <- -1
          }else if(k<ts){ # Consecutive classes during the day (If number of classes a week = 1)
            rough1[(i-1)*ts+k, (j-1)*Col + (i-1)*10 + k] <- -1
            rough1[(i-1)*ts+k, (j-1)*Col + (i-1)*10 + k+1] <- 1
            rough1[(i-1)*ts+k, (j-1)*Col + (i-1)*10 + k+2] <- -1
          }else{ # Consecutive classes at the end of the day (If number of classes a week = 1)
            rough1[(i-1)*ts+k, (j-1)*Col + (i-1)*10 + k] <- -1
            rough1[(i-1)*ts+k, (j-1)*Col + (i-1)*10 + k+1] <- 1
          }
        }
      }
      rough <- rbind(rough, rough1) # Joining this modules consecutive session constraint coefficients to final
      
      rough2 <- matrix(0, nrow = 4, ncol = Col*n)
      ts1 <- Col/5 # Number of time slots in a 1 day period
      ts2 <- (Col/5)*2 # Number of time slots over a 2 day period
      for(i in 1:4){
        for(k in 1:ts2){
          rough2[i, (j-1)*Col + (i-1)*ts1 + k] <- 1
        }
      }
      rough <- rbind(rough, rough2)
    }
  }
  answer <- rough[-1,] # Removing first row of zeros
  
  # Final Coefficient Matrix
  coef_matrix <- rbind(coef_matrix, answer)
  
  ## Solution
  solution <- Rglpk_solve_LP(obj = coef, mat = coef_matrix, dir = signs, types = var_type, rhs=rhs, max = TRUE)
  return(solution)
}

#### Semester 1 ####
#### Morning Preference ####
morning <- read_excel("morning_preference-noFriday.xlsx")
morn <- as.data.frame(morning)
rownames(morn) <- morn[,1]
morn <- morn[,-1]
morning <- as.matrix(morn)

# Repeating preferences per modules
c_morning <- t(matrix(as.vector(morning)))
pref_available <- matrix(0, nrow = 1, ncol = ncol(semester1_availability), ) # Multiplying the preference times by when lecturers for each module are available
for(i in 1:s1){
  rough <- c_morning*semester1_availability[i,]
  pref_available <- rbind(pref_available,rough)
}
Morning <- pref_available[-1,]

system.time(m_assignment <- module_assignemnt(Morning, contact_hours1, semester1_availability))
m_assignment
m_assign <- matrix(m_assignment$solution[1:as.numeric(dim(semester1_availability)[1]*dim(semester1_availability)[2])] , dim(semester1_availability)[1], dim(semester1_availability)[2], byrow = TRUE )
row.names(m_assign) <- rownames(semester1_availability)
m_assign
row_sums(m_assign)
col_sums(m_assign)

write.xlsx(m_assign, "Semester1_mornings.xlsx")

timetable_m1 <- matrix(0, nrow = 10, ncol = 5)
row.names(timetable_m1) <- c("8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm")
colnames(timetable_m1) <- c("Mon", "Tues", "Wed", "Thurs", "Friday")
for(i in 1:nrow(m_assign)){
  for(j in 1:ncol(m_assign)){
    if(m_assign[i,j]==1){
      if(j%%10!=0){
        timetable_m1[(j%%10),(j%/%10 + 1)] <- row.names(m_assign)[i]
      }else{
        timetable_m1[10,(j%/%10)] <- row.names(m_assign)[i]
      }
      
    }
  }
}
# Timetable
timetable_m1

#### Afternoon Preference ####
afternoon <- read_excel("afternoon_preference-noFriday.xlsx")
after <- as.data.frame(afternoon)
rownames(after) <- after[,1]
after <- after[,-1]
afternoon <- as.matrix(after)

c_afternoon <- t(matrix(as.vector(afternoon)))
pref_available <- matrix(0, nrow = 1, ncol = ncol(semester1_availability), ) # Multiplying the preference times by when lecturers for each module are available
for(i in 1:s1){
  rough <- c_afternoon*semester1_availability[i,]
  pref_available <- rbind(pref_available,rough)
}
Afternoon <- pref_available[-1,]

system.time(a_assignment <- module_assignemnt(Afternoon, contact_hours1, semester1_availability))
a_assignment

a_assign <- matrix(a_assignment$solution[1:as.numeric(dim(semester1_availability)[1]*dim(semester1_availability)[2])] , dim(semester1_availability)[1], dim(semester1_availability)[2], byrow = TRUE )
row.names(a_assign) <- rownames(semester1_availability)
a_assign
row_sums(a_assign)
col_sums(a_assign)

write.xlsx(a_assign, "Semester1_afternoons.xlsx")

timetable_a1 <- matrix(0, nrow = 10, ncol = 5)
row.names(timetable_a1) <- c("8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm")
colnames(timetable_a1) <- c("Mon", "Tues", "Wed", "Thurs", "Friday")
for(i in 1:nrow(a_assign)){
  for(j in 1:ncol(a_assign)){
    if(a_assign[i,j]==1){
      if(j%%10!=0){
        timetable_a1[(j%%10),(j%/%10 + 1)] <- row.names(a_assign)[i]
      }else{
        timetable_a1[10,(j%/%10)] <- row.names(a_assign)[i]
      }
      
    }
  }
}
# Timetable
timetable_a1

#### Semester 2 ####
#### Morning Preference ####
pref_available2 <- matrix(0, nrow = 1, ncol = ncol(semester2_availability)) # Multiplying the preference times by when lecturers for each module are available
for(i in 1:s2){
  rough <- c_morning*semester2_availability[i,]
  pref_available2 <- rbind(pref_available2,rough)
}
Morning2 <- pref_available2[-1,]

system.time(m_assignment2 <- module_assignemnt(Morning2, contact_hours2, semester2_availability))
m_assignment2
m_assign2 <- matrix(m_assignment2$solution[1:as.numeric(dim(semester2_availability)[1]*dim(semester2_availability)[2])] , dim(semester2_availability)[1], dim(semester2_availability)[2], byrow = TRUE )
row.names(m_assign2) <- rownames(semester2_availability)
m_assign2
row_sums(m_assign2)
col_sums(m_assign2)

write.xlsx(m_assign2, "Semester2_mornings.xlsx")

timetable_m2 <- matrix(0, nrow = 10, ncol = 5)
row.names(timetable_m2) <- c("8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm")
colnames(timetable_m2) <- c("Mon", "Tues", "Wed", "Thurs", "Friday")
for(i in 1:nrow(m_assign2)){
  for(j in 1:ncol(m_assign2)){
    if(m_assign2[i,j]==1){
      if(j%%10!=0){
        timetable_m2[(j%%10),(j%/%10 + 1)] <- row.names(m_assign2)[i]
      }else{
        timetable_m2[10,(j%/%10)] <- row.names(m_assign2)[i]
      }
      
    }
  }
}
# Timetable
timetable_m2

#### Afternoon Preference ####
pref_available <- matrix(0, nrow = 1, ncol = ncol(semester2_availability)) # Multiplying the preference times by when lecturers for each module are available
for(i in 1:s2){
  rough <- c_afternoon*semester2_availability[i,]
  pref_available <- rbind(pref_available,rough)
}
Afternoon2 <- pref_available[-1,]

system.time(a_assignment2 <- module_assignemnt(Afternoon2, contact_hours2, semester2_availability))
a_assignment2
a_assign2 <- matrix(a_assignment2$solution[1:as.numeric(dim(semester2_availability)[1]*dim(semester2_availability)[2])] , dim(semester2_availability)[1], dim(semester2_availability)[2], byrow = TRUE )
row.names(a_assign2) <- rownames(semester2_availability)
a_assign2
row_sums(a_assign2)
col_sums(a_assign2)

write.xlsx(a_assign2, "Semester2_afternoons.xlsx")

timetable_a2 <- matrix(0, nrow = 10, ncol = 5)
row.names(timetable_a2) <- c("8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm")
colnames(timetable_a2) <- c("Mon", "Tues", "Wed", "Thurs", "Friday")
for(i in 1:nrow(a_assign2)){
  for(j in 1:ncol(a_assign2)){
    if(a_assign2[i,j]==1){
      if(j%%10!=0){
        timetable_a2[(j%%10),(j%/%10 + 1)] <- row.names(a_assign2)[i]
      }else{
        timetable_a2[10,(j%/%10)] <- row.names(a_assign2)[i]
      }
      
    }
  }
}
# Timetable
timetable_a2
