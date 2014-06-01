# Generate some good fake EWS data

fulldata <- twoClassSim(1000, intercept = -10, mislabel = 10)

out <- train(x=fulldata[,-16], y = fulldata[, 16], method = "knn", 
             metric = "ROC", 
             trControl = trainControl(classProbs = TRUE, 
                                      summaryFunction = twoClassSummary ))
names(fulldata) <- c("attendanceTotal", "assessmentMath", "assessmentRead", 
                     "attendance30day", "courseGradesCore", "courseGradesAll", 
                     "GPA", "retention", "remedialCourse", "tardies", 
                     "majorDiscipline", "minorDiscipline", "officeReferral", 
                     "expulsionDays", "suspensionDays", "ethnicity", "FRL", 
                     "gender", "ELP", "schoolMoves", "districtMoves", "IEP", 
                     "age", "graduation")


Tardiesistr
Actual Frequency of Behavioral Incident - Minor
Actual Frequency of Behavioral Incident - Major
Disciplinary (Office) Referrals
Expulsions
Suspensions
504
Ethnicity
Free and Reduced Lunch
Gender
Low English Proficiency
Mobility
Special Education (IEP)
Student Age
Outcome