#### Load the packages ####
library(Rglpk)
library(readxl)
library(xlsx)

#### Loading the data ####
preferences <- read_excel("lecturer preference.xlsx")

### Cleaning the data

## Lecturer preferences
# Making module names the column names
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

## Teaching Restrictions per course
teaching_restrictions <- read_excel("teaching_restrictions.xlsx")
tr <- as.data.frame(teaching_restrictions)
rownames(tr) <- tr[,1]
tr <- tr[,-1]
teaching_restrictions <- tr

#### Creating the lecturer assignment model as a function ####
lecturer_assignemnt <- function(c, contact_hours){
  # c = lecturer preference weightings (preference to teach a module)
  
  n <- dim(c)[1] # rows/number of lecturers
  Col <- dim(c)[2] # columns/number of courses
  
  ## Objective function coefficients
  # Coefficients
  coef <- as.vector(t(c))
  coef <- c(coef, rep(0, n*Col))
  
  # Variable Type (Also a constraint)
  var_type <- c(rep("I", n*Col), rep("B", n*Col))
  
  ## Constraints
  # Right had side
  rhs <- c(t(contact_hours$`Number of Classes`),as.vector(t(t(teaching_restrictions))),t(teaching_hours$Max),t(teaching_hours$Min), matrix(rep(0,2*n*Col), nrow = 1))

  # Signs
  signs <- c(rep("==", Col), rep("<=", n*Col), rep("<=", n), rep(">=", n), rep("<=", n*Col), rep(">=", n*Col))
  
  # Coefficients
  coef_matrix <- matrix(0, (Col + n*Col + n + n + 2*n*Col), 2*n*Col)
  
  # Each course will have the correct number of classes
  for(i in 1:Col){
    for(j in 1:n){
      coef_matrix[i, Col*(j-1) + i] <- 1
    }
  }
  
  # Teaching Restriction per course by lecturer
  for(i in 1:(n*Col)){
    for(j in 1:(n*Col)){
      if(i==j){
        coef_matrix[Col + i, j] <- 1
      }
    }
  }
  
  # Each lecturer should teach a maximum number of hours
  for(i in 1:n){
    for(j in 1:Col){
      coef_matrix[(Col+(n*Col)+i), (Col*(i-1) + j)] <- contact_hours[j,1]
    }
  }
  
  # Each lecturer should teach a min number of hours
  for(i in 1:n){
    for(j in 1:Col){
      coef_matrix[(Col+(n*Col)+n+i), (Col*(i-1) + j)] <- contact_hours[j,1]
    }
  }
  
  # Maximum number of classes taught by a lecturer for module i
  for(i in 1:n){
    for(j in 1:Col){
      coef_matrix[Col+(n*Col)+n+n+ (i-1)*Col + j, (i-1)*Col + j] <- 1
      coef_matrix[Col+(n*Col)+n+n+ (i-1)*Col + j, n*Col + (i-1)*Col + j] <- -1*contact_hours$`Number of Classes`[j]
    }
  }
  
  # Minimum number of classes taught by a lecturer for module i
  for(i in 1:n){
    for(j in 1:Col){
      coef_matrix[Col+(n*Col)+n+n+n*Col +(i-1)*Col + j, (i-1)*Col + j] <- 1
      coef_matrix[Col+(n*Col)+n+n+n*Col +(i-1)*Col + j, n*Col + (i-1)*Col + j] <- -1*contact_hours$`Min Classes`[j]
    }
  }
  
  ## Solution
  solution <- Rglpk_solve_LP(obj = coef, mat = coef_matrix, dir = signs, types = var_type, rhs=rhs, max = TRUE)
  return(solution)
}

#### Running the model ####
system.time(l_assignment <- lecturer_assignemnt(preferences, contact_hours))
l_assignment
l_assig <- matrix(l_assignment$solution[1:as.numeric(dim(preferences)[1]*dim(preferences)[2])] , dim(preferences)[1], dim(preferences)[2], byrow = TRUE )
l_assig
rownames(l_assig) <- row.names(preferences)
colnames(l_assig) <- colnames(preferences)
l_assig

# Checking the correct number of classes have been assigned
col_sums(l_assig) 
# Checking the lecturer hours have been adhered to
cbind(teaching_hours$Min,l_assig%*%contact_hours$`Contact Hours`,teaching_hours$Max) # Putting Maximum Constraints before Minimum constraints results in both being adhered to

write.xlsx(l_assig, "Lecturer - Module Choice.xlsx")

# Which lecturer is teaching which module (number of classes)
for(i in 1:ncol(l_assig)){
  for(j in 1:nrow(l_assig)){
    if(l_assig[j,i]>=1){
      print(paste0(colnames(l_assig)[i], ": ", rownames(l_assig)[j], " (", l_assig[j,i],")"))
    }
  }
}
