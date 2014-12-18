## The purpose of this script to create tidy data that can be used 
## for later analysis by combining the train and test files from the 
## Human Activity Recognition Using Smartphones Dataset Version 1.0

## This function will read, combined, label the values and variables and finnally 
## select the columns that contains the mean and standart deviation of the variables 
## on the train and test files.
tidyData<-function(){
      ## This function will download, unzip and read the files if needed. 
      ## It will also label the values and columns of the files
      readFiles<-function(dataSetType){
            # To verify if the argument is valid
            if(!is.character(dataSetType) | !(dataSetType %in% c('test','train'))){
                  stop('Invalid dataSetType')
            }
            # Check is the unzip directory of the files exists. If not, it download and unzip them
            if(is.na(file.info('UCI HAR Dataset')$isdir)){
                  url<-'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
                  download.file(url,destfile = './UCI_HAR_Dataset.zip',method='curl'
                                ,quiet = TRUE)
                  unzip('./UCI_HAR_Dataset.zip',exdir = './')
            }
            path<-paste('.','UCI HAR Dataset',dataSetType,sep='/') # Path of the files
            # Read the subject file (file containing the subjects id)
            subjectID<-read.table(paste(path,paste('subject_',dataSetType,'.txt',sep=''),sep='/'))
            # Read the activity file (file containing the activity code)
            activity<-read.table(paste(path,paste('y_',dataSetType,'.txt',sep=''),sep='/'))
            # Read the feature file (File containing the data of the features array)
            features<-read.table(paste(path,paste('x_',dataSetType,'.txt',sep=''),sep='/'))
            # Read the file containing the activity labels
            activityLabel<-read.table('./UCI HAR Dataset/activity_labels.txt')
            # Read the file containing the feature labels
            featuresLabels<-read.table('./UCI HAR Dataset/features.txt')
            # Assigned the value labels for the activities
            activity$V1<-factor(activity$V1,labels=activityLabel$V2)
            # Rename the column with the subject id
            names(subjectID)<-'subjectID'
            # Rename the column with the activities
            names(activity)<-'activity'
            # Rename the columns of the features array
            names(features)<-featuresLabels$V2
            # Combine the subject, activity and feauteres datasets
            data<-cbind(subjectID,activity,features)
            # Return the combined data set
            data 
      }
      # Call the readFiles function to read, label and combine the test files
      test<-readFiles('test')
      # To add a column indicating that this observations where obtained from the test files
      dataSource<-data.frame(dataSource=rep('test',nrow(test)))
      testFinal<-cbind(dataSource,test)
      # Call the readFiles function to read, label and combine the train files
      train<-readFiles('train')
      # To add a column indicating that this observations where obtained from the test files
      dataSource<-data.frame(dataSource=rep('train',nrow(train)))
      trainFinal<-cbind(dataSource,train)
      # Combin the train and test data sets
      data<-rbind(testFinal,trainFinal)
      # Select only the columns that calculate the mean and standart deviation
      data[,c(1,2,3,which(str_detect(string = names(data),pattern = 'mean()|std()')))]
}
# Call the tidyData function to read, label and combined the train and test files
data<-tidyData()
# To group the data obtained on the previous line by subject id and activity performed
# and calculate for this groups the mean of every column on the dataset. 
# The column indicating the source of the data (trin or test) is excluded from this dataset
dataMean<-aggregate(.~subjectID + activity,data,mean,na.rm=TRUE)[,-c(3)]
