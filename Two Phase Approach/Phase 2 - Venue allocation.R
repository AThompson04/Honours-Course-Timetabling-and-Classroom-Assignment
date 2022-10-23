## Set working directory here
# setwd()

#### Load the packages ####
library(Rglpk)
library(readxl)
library(xlsx)

#### Load Data ####
### Contact hours
contact_hours <- read_excel("contact hours.xlsx")
ch <- as.data.frame(contact_hours)
rownames(ch) <- ch[,1]
ch <- ch[,-1]
contact_hours1 <- ch[ch$Semester==1,]
contact_hours2 <- ch[ch$Semester==2,]

### Venue Capacity
v_capacity <- read_excel("VenueCapacity.xlsx")
vc <- as.data.frame(v_capacity)
ven_names <- vc[,1]
v_capacity <- v_capacity[,-1]
v_capacity <- as.matrix(v_capacity)
rownames(v_capacity) <- ven_names

### Module/Class size
m_size <- read_excel("ModuleSize.xlsx")
ms <- as.data.frame(m_size)
rownames(ms) <- ms[,1]
ms <- ms[,-1]
m_size1 <- ms[ms$Semester==1,]
m_size2 <- ms[ms$Semester==2,]

## Venues
v <- length(v_capacity) # Number of venues
venues <- rownames(vc)

venues_1 <- "~/Venues.xlsx"

ven <- list() # Empty list to load the venue availability and preferences
venues1 <- matrix(0, nrow = v, ncol = 50) # Venue availability

# Load the venue availability sheet by sheet
for (i in 1:v){
  ven[[i]] <- read_xlsx(venues_1, sheet = i, col_names = TRUE)
} 

# Converting the list into vectors for each venue - starting with the Monday time slots and then moving to the Tuesday time slots and so on
for (i in 1:v) {
  x <- as.matrix(ven[[i]]) # Making the sheet a matrix
  x <- x[,-1] # Removing the venue from column 1
  x <- as.vector(t(x)) # Converting the matrix into a vector
  x <- as.numeric(x) # Convert 1 and 0s to num type
  venues1[i,] <- x # Add to venue availability matrix
}

rownames(venues1) <- venues

## Can venue fit the class size
fitS1 <- matrix(0, ncol = nrow(m_size1), nrow = nrow(v_capacity))
fitS2 <- matrix(0, ncol = nrow(m_size2), nrow = nrow(v_capacity))

for(i in 1:nrow(v_capacity)){
  for(j in 1:nrow(m_size1)){
    if(m_size1[j,1]<=v_capacity[i]){
      fitS1[i,j] <- 1 # Venue can hold class
    }else{
      fitS1[i,j] <- 0 # Venue is too small for class
    }
  }
  for(k in 1:nrow(m_size2)){
    if(m_size2[k,1]<=v_capacity[i]){
      fitS2[i,k] <- 1 # Venue can fit class
    }else{
      fitS2[i,k] <- 0 # Venue is too small for class
    }
  }
}

# Days of the week and times
day <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
time <- c("8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm")

#### Model ####
## Creating the module-time slot assignment model as a function
venue_assignemnt <- function(c, contact_hours, num){ 
  # c = Venue preference weighting
  # num refers to the number of modules for the semester
  n <- dim(c)[1] # rows/number of venues
  Col <- dim(c)[2] # columns/number of timeslots for all modules
  
  ## Objective function coefficients
  # Coefficients
  coef <- as.vector(t(c))
  
  # Variable Type (Also a constraint)
  var_type <- rep("B", n*Col)
  
  # RHS and Signs for consecutive sessions
  rhs_1 <- vector()
  sign_1 <- vector()
  t <- 0
  ts <- ((Col/num)/5) - 1 # Number of timeslots a day minus 1
  
  for(j in 1:num){
    if(contact_hours$`Contact Hours`[j]==2){
      for(k in 1:n){ 
        t <- t + 1
        for(i in t:(t+ts*5-1)){
          sign_1[i] <- "<="
          rhs_1[i] <- 0
        }
        t <- t + ts*5 - 1
      }
    }
  }
  
  # Signs
  signs <- c(rep("==", num), rep("<=", Col),sign_1)
  
  # RHS
  rhs <- c(t(contact_hours$`Contact Hours`*contact_hours$`Classes a Week`), t(rep(1, Col)), t(rhs_1))
  
  # Coefficients
  coef_matrix <- matrix(0, num + Col, n*Col)
  
  # Constraints
  # 2: Number of classes must equal the number of contact hours per week that are needed
  for(k in 1:num){
    for(i in 1:n){
      for(j in 1:(Col/num)){
        coef_matrix[k, (i-1)*Col + (k-1)*(Col/num) + j] <- 1
      }
    }
  }
  
  # 3: Unique constraint for a venue for each time slot
  for(i in 1:num){
    for(j in 1:(Col/num)){
      for(k in 1:n){
        coef_matrix[(num + (i-1)*(Col/num) + j), (Col/num)*(i-1) + (k-1)*Col + j] <- 1
      }
    }
  }
  
  # Consecutive sessions
  rough <- matrix(0, nrow = 1, ncol = Col*n)
  
  for(j in 1:num){
    if(contact_hours$`Contact Hours`[j]==2){
      rough1 <- matrix(0, nrow = ts*5*n, ncol = Col*n)
      for(h in 1:n){
        for(i in 1:5){
          for(k in 1:ts){
            if(k==1){ # Consecutive classes in the beginning of the day (If number of classes a week = 1)
              rough1[(h-1)*5*ts + (i-1)*ts + k, (h-1)*Col + (j-1)*(Col/num) + (i-1)*10 + k] <- 1
              rough1[(h-1)*5*ts + (i-1)*ts + k, (h-1)*Col + (j-1)*(Col/num) + (i-1)*10 + k+1] <- -1
            }else if(k<ts){ # Consecutive classes during the day (If number of classes a week = 1)
              rough1[(h-1)*5*ts + (i-1)*ts + k, (h-1)*Col + (j-1)*(Col/num) + (i-1)*10 + k] <- -1
              rough1[(h-1)*5*ts + (i-1)*ts + k, (h-1)*Col + (j-1)*(Col/num) + (i-1)*10 + k+1] <- 1
              rough1[(h-1)*5*ts + (i-1)*ts + k, (h-1)*Col + (j-1)*(Col/num) + (i-1)*10 + k+2] <- -1
            }else{ # Consecutive classes at the end of the day (If number of classes a week = 1)
              rough1[(h-1)*5*ts + (i-1)*ts + k, (h-1)*Col + (j-1)*(Col/num) + (i-1)*10 + k] <- -1
              rough1[(h-1)*5*ts + (i-1)*ts + k, (h-1)*Col + (j-1)*(Col/num) + (i-1)*10 + k+1] <- 1
            }
          }
        }
      }
      rough <- rbind(rough, rough1) # Joining this modules consecutive session constraint coefficients to final
    }
  }
  answer <- rough[-1,] # Removing first row of zeros
  
  # Final Coefficient Matrix
  coef_matrix <- rbind(coef_matrix, answer)
  
  ## Solution
  solution <- Rglpk_solve_LP(obj = coef, mat = coef_matrix, dir = signs, types = var_type, rhs=rhs, max = TRUE)
  return(solution)
}

#### Semester 1 Morning Preference (S1 M S2 M) ####
## Preparing the data
s1_morning <- read_excel("venue_s1.xlsx")
s1morn <- as.data.frame(s1_morning)
rownames(s1morn) <- s1morn[,1]
s1morn <- s1morn[,-1]
s1_morning <- as.matrix(s1morn)

s1 <- nrow(contact_hours1)

# Repeating preferences per venue
c_morning1 <- t(matrix(as.vector(t(s1_morning))))
pref_available <- matrix(0, nrow = 1, ncol = ncol(c_morning1)) # Multiplying the preference times by when lecturers for each module are available
for(i in 1:v){
  rough <- c_morning1*rep(venues1[i,],s1)
  pref_available <- rbind(pref_available,rough)
}
Morning_S1 <- pref_available[-1,]

for(i in 1:v){
  Morning_S1[i,] <- Morning_S1[i,]*rep(fitS1[i,], each = 50)
}

## Running the model
system.time(ms1_assig <- venue_assignemnt(Morning_S1, contact_hours1, s1))
ms1_assig
ms1_assign <- matrix(ms1_assig$solution[1:as.numeric(dim(Morning_S1)[1]*dim(Morning_S1)[2])] , dim(Morning_S1)[1], dim(Morning_S1)[2], byrow = TRUE )
ms1_assign

answer_s1m <- matrix(0, nrow = v, ncol = s1)
row.names(answer_s1m) <- venues
modules_s1 <- rownames(contact_hours1)
colnames(answer_s1m) <- modules_s1
# Venue allocation when there is more than one class a week for a module
for(i in 1:s1){
  answer_s1m[,i] <- t(t(rowSums(ms1_assign[,((i-1)*50+1):(i*50)])))
  if(contact_hours1$`Classes a Week`[i]==2){ # If more than one class a week print the solution so we can see when each venue was booked
    x <- ms1_assign[,((i-1)*50+1):(i*50)]
    for(j in 1:nrow(x)){
      for(k in 1:ncol(x)){
        if(x[j,k]>=1){
          if(k%%10!=0){
            print(paste0(modules_s1[i],' venue: ', ven_names[j], " on ", day[k%/%10+1], " at ", time[k%%10]))
          }else{
            print(paste0(modules_s1[i],' venue: ', ven_names[j], " on ", day[k%/%10+1], " at ", time[10]))
          }
          
        }
      }
    }
  }
}
row.names(answer_s1m) <- ven_names
# Venue Allocation
answer_s1m


# Checking that venues were allocated at the correct time
colSums(ms1_assign)==as.vector(t(s1_morning))

write.xlsx(answer_s1m, "Semester1_morning_venues.xlsx")

#### Semester 2 Morning Preference (S1 M S2 M) ####
## Preparing data
s2_morning <- read_excel("venue_s2.xlsx")
s2morn <- as.data.frame(s2_morning)
rownames(s2morn) <- s2morn[,1]
s2morn <- s2morn[,-1]
s2_morning <- as.matrix(s2morn)

s2 <- nrow(contact_hours2)

# Repeating preferences per venue
c_morning2 <- t(matrix(as.vector(t(s2_morning))))
pref_available <- matrix(0, nrow = 1, ncol = ncol(c_morning2)) # Multiplying the preference times by when lecturers for each module are available
for(i in 1:v){
  rough <- c_morning2*rep(venues1[i,],s2)
  pref_available <- rbind(pref_available,rough)
}
Morning_S2 <- pref_available[-1,]

for(i in 1:v){
  Morning_S2[i,] <- Morning_S2[i,]*rep(fitS2[i,], each = 50)
}

## Runnning the model
system.time(ms2_assig <- venue_assignemnt(Morning_S2, contact_hours2, s2))
ms2_assig
ms2_assign <- matrix(ms2_assig$solution[1:as.numeric(dim(Morning_S2)[1]*dim(Morning_S2)[2])] , dim(Morning_S2)[1], dim(Morning_S2)[2], byrow = TRUE )
ms2_assign

answer_s2m <- matrix(0, nrow = v, ncol = s2)
row.names(answer_s2m) <- venues
modules_s2 <- rownames(contact_hours2)
colnames(answer_s2m) <- modules_s2
# Venue allocation when a module has more than one class a week
for(i in 1:s2){
  answer_s2m[,i] <- t(t(rowSums(ms2_assign[,((i-1)*50+1):(i*50)])))
  if(contact_hours2$`Classes a Week`[i]==2){ # If more than one class a week print the solution so we can see when each venue was booked
    x <- ms2_assign[,((i-1)*50+1):(i*50)]
    for(j in 1:nrow(x)){
      for(k in 1:ncol(x)){
        if(x[j,k]>=1){
          if(k%%10!=0){
            print(paste0(modules_s2[i],' venue: ', ven_names[j], " on ", day[k%/%10+1], " at ", time[k%%10]))
          }else{
            print(paste0(modules_s2[i],' venue: ', ven_names[j], " on ", day[k%/%10+1], " at ", time[10]))
          }
          
        }
      }
    }
  }
}
row.names(answer_s2m) <- ven_names
# Venue allocation
answer_s2m


# Checking that venues were allocated at the correct time
colSums(ms2_assign)==as.vector(t(s2_morning))

write.xlsx(answer_s2m, "Semester2_morning_venues.xlsx")


#### Semester 1 Afternoon Preference (S1 A S2 M) ####
## Preparing data
s1_afternoona1m2 <- read_excel("venue_s1a1m2.xlsx")
s1aftera1m2 <- as.data.frame(s1_afternoona1m2)
rownames(s1aftera1m2) <- s1aftera1m2[,1]
s1aftera1m2 <- s1aftera1m2[,-1]
s1_afternoona1m2 <- as.matrix(s1aftera1m2)

s1 <- nrow(contact_hours1)

# Repeating preferences per venue
c_morning1a1m2 <- t(matrix(as.vector(t(s1_afternoona1m2))))
pref_available <- matrix(0, nrow = 1, ncol = ncol(c_morning1a1m2), ) # Multiplying the preference times by when lecturers for each module are available
for(i in 1:v){
  rough <- c_morning1a1m2*rep(venues1[i,],s1)
  pref_available <- rbind(pref_available,rough)
}
Afternoon_S1a1m2 <- pref_available[-1,]

for(i in 1:v){
  Afternoon_S1a1m2[i,] <- Afternoon_S1a1m2[i,]*rep(fitS1[i,], each = 50)
}

## Running the model
system.time(ms1_assiga1m2 <- venue_assignemnt(Afternoon_S1a1m2, contact_hours1, s1))
ms1_assiga1m2
ms1_assigna1m2 <- matrix(ms1_assiga1m2$solution[1:as.numeric(dim(Afternoon_S1a1m2)[1]*dim(Afternoon_S1a1m2)[2])] , dim(Afternoon_S1a1m2)[1], dim(Afternoon_S1a1m2)[2], byrow = TRUE )
ms1_assigna1m2

answer_s1ma1m2 <- matrix(0, nrow = v, ncol = s1)
row.names(answer_s1ma1m2) <- venues
modules_s1a1m2 <- rownames(contact_hours1)
colnames(answer_s1ma1m2) <- modules_s1
# Venue allocation for modules that have more than one class a week
for(i in 1:s1){
  answer_s1ma1m2[,i] <- t(t(rowSums(ms1_assigna1m2[,((i-1)*50+1):(i*50)])))
  if(contact_hours1$`Classes a Week`[i]==2){ # If more than one class a week print the solution so we can see when each venue was booked
    x <- ms1_assigna1m2[,((i-1)*50+1):(i*50)]
    for(j in 1:nrow(x)){
      for(k in 1:ncol(x)){
        if(x[j,k]>=1){
          if(k%%10!=0){
            print(paste0(modules_s1a1m2[i],' venue: ', ven_names[j], " on ", day[k%/%10+1], " at ", time[k%%10]))
          }else{
            print(paste0(modules_s1a1m2[i],' venue: ', ven_names[j], " on ", day[k%/%10+1], " at ", time[10]))
          }
          
        }
      }
    }
  }
}
row.names(answer_s1ma1m2) <- ven_names
# Venue allocation
answer_s1ma1m2


# Checking that venues were allocated at the correct time
colSums(ms1_assigna1m2)==as.vector(t(s1_afternoona1m2))

write.xlsx(answer_s1ma1m2, "Semester1_a1m2_venues.xlsx")

#### Semester 2 Morning Preference (S1 A S2 M) ####
# Preparing the data
s2_morninga1m2 <- read_excel("venue_s2a1m2.xlsx")
s2morna1m2 <- as.data.frame(s2_morninga1m2)
rownames(s2morna1m2) <- s2morna1m2[,1]
s2morna1m2 <- s2morna1m2[,-1]
s2_morninga1m2 <- as.matrix(s2morna1m2)

s2 <- nrow(contact_hours2)

# Repeating preferences per venue
c_morning2a1m2 <- t(matrix(as.vector(t(s2_morninga1m2))))
pref_available <- matrix(0, nrow = 1, ncol = ncol(c_morning2a1m2)) # Multiplying the preference times by when lecturers for each module are available
for(i in 1:v){
  rough <- c_morning2a1m2*rep(venues1[i,],s2)
  pref_available <- rbind(pref_available,rough)
}
Morning_S2a1m2 <- pref_available[-1,]

for(i in 1:v){
  Morning_S2a1m2[i,] <- Morning_S2a1m2[i,]*rep(fitS2[i,], each = 50)
}

## Running the model
system.time(ms2_assiga1m2 <- venue_assignemnt(Morning_S2a1m2, contact_hours2, s2))
ms2_assiga1m2
ms2_assigna1m2 <- matrix(ms2_assiga1m2$solution[1:as.numeric(dim(Morning_S2a1m2)[1]*dim(Morning_S2a1m2)[2])] , dim(Morning_S2a1m2)[1], dim(Morning_S2a1m2)[2], byrow = TRUE )
ms2_assigna1m2

answer_s2ma1m2 <- matrix(0, nrow = v, ncol = s2)
row.names(answer_s2ma1m2) <- venues
modules_s2a1m2 <- rownames(contact_hours2)
colnames(answer_s2ma1m2) <- modules_s2a1m2
# Venue allocation for a module that has more than one class a week
for(i in 1:s2){
  answer_s2ma1m2[,i] <- t(t(rowSums(ms2_assigna1m2[,((i-1)*50+1):(i*50)])))
  if(contact_hours2$`Classes a Week`[i]==2){ # If more than one class a week print the solution so we can see when each venue was booked
    x <- ms2_assigna1m2[,((i-1)*50+1):(i*50)]
    for(j in 1:nrow(x)){
      for(k in 1:ncol(x)){
        if(x[j,k]>=1){
          if(k%%10!=0){
            print(paste0(modules_s2a1m2[i],' venue: ', ven_names[j], " on ", day[k%/%10+1], " at ", time[k%%10]))
          }else{
            print(paste0(modules_s2a1m2[i],' venue: ', ven_names[j], " on ", day[k%/%10+1], " at ", time[10]))
          }
          
        }
      }
    }
  }
}
row.names(answer_s2ma1m2) <- ven_names
# Venue allocation
answer_s2ma1m2

# Checking that venues were allocated at the correct time
colSums(ms2_assigna1m2)==as.vector(t(s2_morninga1m2))

write.xlsx(answer_s2ma1m2, "Semester2_a1m2_venues.xlsx")


#### Semester 1 Afternoon Preference (S1 A S2 A) ####
## Preparing the data
s1_morninga1a2 <- read_excel("venue_s1a1a2.xlsx")
s1morna1a2 <- as.data.frame(s1_morninga1a2)
rownames(s1morna1a2) <- s1morna1a2[,1]
s1morna1a2 <- s1morna1a2[,-1]
s1_morninga1a2 <- as.matrix(s1morna1a2)

s1 <- nrow(contact_hours1)

# Repeating preferences per venue
c_morning1a1a2 <- t(matrix(as.vector(t(s1_morninga1a2))))
pref_available <- matrix(0, nrow = 1, ncol = ncol(c_morning1a1a2)) # Multiplying the preference times by when lecturers for each module are available
for(i in 1:v){
  rough <- c_morning1a1a2*rep(venues1[i,],s1)
  pref_available <- rbind(pref_available,rough)
}
Morning_S1a1a2 <- pref_available[-1,]

for(i in 1:v){
  Morning_S1a1a2[i,] <- Morning_S1a1a2[i,]*rep(fitS1[i,], each = 50)
}

## Running the model
system.time(ms1_assiga1a2 <- venue_assignemnt(Morning_S1a1a2, contact_hours1, s1))
ms1_assiga1a2
ms1_assigna1a2 <- matrix(ms1_assiga1a2$solution[1:as.numeric(dim(Morning_S1a1a2)[1]*dim(Morning_S1a1a2)[2])] , dim(Morning_S1a1a2)[1], dim(Morning_S1a1a2)[2], byrow = TRUE )
ms1_assigna1a2

answer_s1ma1a2 <- matrix(0, nrow = v, ncol = s1)
row.names(answer_s1ma1a2) <- venues
modules_s1a1a2 <- rownames(contact_hours1)
colnames(answer_s1ma1a2) <- modules_s1a1a2
# Venue allocation for modules with more than one class a week
for(i in 1:s1){
  answer_s1ma1a2[,i] <- t(t(rowSums(ms1_assigna1a2[,((i-1)*50+1):(i*50)])))
  if(contact_hours1$`Classes a Week`[i]==2){ # If more than one class a week print the solution so we can see when each venue was booked
    x <- ms1_assigna1a2[,((i-1)*50+1):(i*50)]
    for(j in 1:nrow(x)){
      for(k in 1:ncol(x)){
        if(x[j,k]>=1){
          if(k%%10!=0){
            print(paste0(modules_s1a1a2[i],' venue: ', ven_names[j], " on ", day[k%/%10+1], " at ", time[k%%10]))
          }else{
            print(paste0(modules_s1a1a2[i],' venue: ', ven_names[j], " on ", day[k%/%10+1], " at ", time[10]))
          }
          
        }
      }
    }
  }
}
row.names(answer_s1ma1a2) <- ven_names
# Venue allocation
answer_s1ma1a2


# Checking that venues were allocated at the correct time
colSums(ms1_assigna1a2)==as.vector(t(s1_morninga1a2))

write.xlsx(answer_s1ma1a2, "Semester1_a1a2_venues.xlsx")

#### Semester 2 Afternoon Preference (S1 A S2 A) ####
## Preparing the data
s2_morninga1a2 <- read_excel("venue_s2a1a2.xlsx")
s2morna1a2 <- as.data.frame(s2_morninga1a2)
rownames(s2morna1a2) <- s2morna1a2[,1]
s2morna1a2 <- s2morna1a2[,-1]
s2_morninga1a2 <- as.matrix(s2morna1a2)

s2 <- nrow(contact_hours2)

# Repeating preferences per venue
c_morning2a1a2 <- t(matrix(as.vector(t(s2_morninga1a2))))
pref_available <- matrix(0, nrow = 1, ncol = ncol(c_morning2a1a2)) # Multiplying the preference times by when lecturers for each module are available
for(i in 1:v){
  rough <- c_morning2a1a2*rep(venues1[i,],s2)
  pref_available <- rbind(pref_available,rough)
}
Morning_S2a1a2 <- pref_available[-1,]

for(i in 1:v){
  Morning_S2a1a2[i,] <- Morning_S2a1a2[i,]*rep(fitS2[i,], each = 50)
}

# Running the model
system.time(ms2_assiga1a2 <- venue_assignemnt(Morning_S2a1a2, contact_hours2, s2))
ms2_assiga1a2
ms2_assigna1a2 <- matrix(ms2_assiga1a2$solution[1:as.numeric(dim(Morning_S2a1a2)[1]*dim(Morning_S2a1a2)[2])] , dim(Morning_S2a1a2)[1], dim(Morning_S2a1a2)[2], byrow = TRUE )
ms2_assigna1a2

answer_s2ma1a2 <- matrix(0, nrow = v, ncol = s2)
row.names(answer_s2ma1a2) <- venues
modules_s2a1a2 <- rownames(contact_hours2)
colnames(answer_s2ma1a2) <- modules_s2
# Venue allocation for a module with more than one class a week
for(i in 1:s2){
  answer_s2ma1a2[,i] <- t(t(rowSums(ms2_assigna1a2[,((i-1)*50+1):(i*50)])))
  if(contact_hours2$`Classes a Week`[i]==2){ # If more than one class a week print the solution so we can see when each venue was booked
    x <- ms2_assigna1a2[,((i-1)*50+1):(i*50)]
    for(j in 1:nrow(x)){
      for(k in 1:ncol(x)){
        if(x[j,k]>=1){
          if(k%%10!=0){
            print(paste0(modules_s2a1a2[i],' venue: ', ven_names[j], " on ", day[k%/%10+1], " at ", time[k%%10]))
          }else{
            print(paste0(modules_s2a1a2[i],' venue: ', ven_names[j], " on ", day[k%/%10+1], " at ", time[10]))
          }
          
        }
      }
    }
  }
}
row.names(answer_s2ma1a2) <- ven_names
# Venue allocation
answer_s2ma1a2


# Checking that venues were allocated at the correct time
colSums(ms2_assigna1a2)==as.vector(t(s2_morninga1a2))

write.xlsx(answer_s2ma1a2, "Semester2_a1a2_venues.xlsx")


#### Semester 1 Morning Preference (S1 M S2 A) ####
## Preparing the data
s1_morningm1a2 <- read_excel("venue_s1m1a2.xlsx")
s1mornm1a2 <- as.data.frame(s1_morningm1a2)
rownames(s1mornm1a2) <- s1mornm1a2[,1]
s1mornm1a2 <- s1mornm1a2[,-1]
s1_morningm1a2 <- as.matrix(s1mornm1a2)

s1 <- nrow(contact_hours1)

# Repeating preferences per venue
c_morning1m1a2 <- t(matrix(as.vector(t(s1_morningm1a2))))
pref_available <- matrix(0, nrow = 1, ncol = ncol(c_morning1m1a2)) # Multiplying the preference times by when lecturers for each module are available
for(i in 1:v){
  rough <- c_morning1m1a2*rep(venues1[i,],s1)
  pref_available <- rbind(pref_available,rough)
}
Morning_S1m1a2 <- pref_available[-1,]

for(i in 1:v){
  Morning_S1m1a2[i,] <- Morning_S1m1a2[i,]*rep(fitS1[i,], each = 50)
}

## Running the model
system.time(ms1_assigm1a2 <- venue_assignemnt(Morning_S1m1a2, contact_hours1, s1))
ms1_assigm1a2
ms1_assignm1a2 <- matrix(ms1_assigm1a2$solution[1:as.numeric(dim(Morning_S1m1a2)[1]*dim(Morning_S1m1a2)[2])] , dim(Morning_S1m1a2)[1], dim(Morning_S1m1a2)[2], byrow = TRUE )
ms1_assignm1a2

answer_s1mm1a2 <- matrix(0, nrow = v, ncol = s1)
row.names(answer_s1mm1a2) <- venues
modules_s1m1a2 <- rownames(contact_hours1)
colnames(answer_s1mm1a2) <- modules_s1
# Venue allocation for modules with more than one class a week
for(i in 1:s1){
  answer_s1mm1a2[,i] <- t(t(rowSums(ms1_assignm1a2[,((i-1)*50+1):(i*50)])))
  if(contact_hours1$`Classes a Week`[i]==2){ # If more than one class a week print the solution so we can see when each venue was booked
    x <- ms1_assignm1a2[,((i-1)*50+1):(i*50)]
    for(j in 1:nrow(x)){
      for(k in 1:ncol(x)){
        if(x[j,k]>=1){
          if(k%%10!=0){
            print(paste0(modules_s1m1a2[i],' venue: ', ven_names[j], " on ", day[k%/%10+1], " at ", time[k%%10]))
          }else{
            print(paste0(modules_s1m1a2[i],' venue: ', ven_names[j], " on ", day[k%/%10+1], " at ", time[10]))
          }
          
        }
      }
    }
  }
}
row.names(answer_s1mm1a2) <- ven_names
# Venue allocation
answer_s1mm1a2


# Checking that venues were allocated at the correct time
colSums(ms1_assignm1a2)==as.vector(t(s1_morningm1a2))

write.xlsx(answer_s1mm1a2, "Semester1_m1a2_venues.xlsx")

#### Semester 2 Afternoon Preference (S1 M S2 A) ####
## Preparing the data
s2_morningm1a2 <- read_excel("venue_s2m1a2.xlsx")
s2mornm1a2 <- as.data.frame(s2_morningm1a2)
rownames(s2mornm1a2) <- s2mornm1a2[,1]
s2mornm1a2 <- s2mornm1a2[,-1]
s2_morningm1a2 <- as.matrix(s2mornm1a2)

s2 <- nrow(contact_hours2)

# Repeating preferences per venue
c_morning2m1a2 <- t(matrix(as.vector(t(s2_morningm1a2))))
pref_available <- matrix(0, nrow = 1, ncol = ncol(c_morning2m1a2)) # Multiplying the preference times by when lecturers for each module are available
for(i in 1:v){
  rough <- c_morning2m1a2*rep(venues1[i,],s2)
  pref_available <- rbind(pref_available,rough)
}
Morning_S2m1a2 <- pref_available[-1,]

for(i in 1:v){
  Morning_S2m1a2[i,] <- Morning_S2m1a2[i,]*rep(fitS2[i,], each = 50)
}

## Running the model
system.time(ms2_assigm1a2 <- venue_assignemnt(Morning_S2m1a2, contact_hours2, s2))
ms2_assigm1a2
ms2_assignm1a2 <- matrix(ms2_assigm1a2$solution[1:as.numeric(dim(Morning_S2m1a2)[1]*dim(Morning_S2m1a2)[2])] , dim(Morning_S2m1a2)[1], dim(Morning_S2m1a2)[2], byrow = TRUE )
ms2_assignm1a2

answer_s2mm1a2 <- matrix(0, nrow = v, ncol = s2)
row.names(answer_s2mm1a2) <- venues
modules_s2m1a2 <- rownames(contact_hours2)
colnames(answer_s2mm1a2) <- modules_s2m1a2
# Venue allocation for modules with more than one class a week
for(i in 1:s2){
  answer_s2mm1a2[,i] <- t(t(rowSums(ms2_assignm1a2[,((i-1)*50+1):(i*50)])))
  if(contact_hours2$`Classes a Week`[i]==2){ # If more than one class a week print the solution so we can see when each venue was booked
    x <- ms2_assignm1a2[,((i-1)*50+1):(i*50)]
    for(j in 1:nrow(x)){
      for(k in 1:ncol(x)){
        if(x[j,k]>=1){
          if(k%%10!=0){
            print(paste0(modules_s2m1a2[i],' venue: ', ven_names[j], " on ", day[k%/%10+1], " at ", time[k%%10]))
          }else{
            print(paste0(modules_s2m1a2[i],' venue: ', ven_names[j], " on ", day[k%/%10+1], " at ", time[10]))
          }
          
        }
      }
    }
  }
}
row.names(answer_s2mm1a2) <- ven_names
# Venue allocation
answer_s2mm1a2


# Checking that venues were allocated at the correct time
colSums(ms2_assignm1a2)==as.vector(t(s2_morningm1a2))

write.xlsx(answer_s2mm1a2, "Semester2_m1a2_venues.xlsx")
