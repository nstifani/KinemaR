# Header ------------------------------------------------------------------
#### KinemaR has been written by Nicolas Stifani contact nstifani@gmail.com
rm(list=ls()) # Clear the workspace just in case you have old stuff there
RequiredPackage<-c("zoo", "tcltk")
for (PackageI in 1:length(RequiredPackage)){ 
  RequiredPackageI<-RequiredPackage[PackageI]
  if (!is.element(RequiredPackageI, installed.packages()[,1])){
    install.packages(RequiredPackageI)
  }
  library(RequiredPackageI, character.only=TRUE) # Load the required packages
} # Download required packages if there are not already there and load them

ListGlobalObject<-c("InputDirPath", "ListInputFilePath","InputDirName",
                    "VideoCalibrationFilePath","VideoCalibrationData",
                    "SubjectBodyFilePath", "SubjectBodyData") # This is the List of Object to be assign in the Global Environment
assign("ListGlobalObject",ListGlobalObject,envir = .GlobalEnv)

# Initlization Functions --------------------------------------------------
ResetTransformDialog.Function<- function(){ # Reset Dialog
  rm(list=ListGlobalObject, envir = .GlobalEnv) # Remove the object # Remove all the object indicated in the list
  # Reset the tclvalues
  tclvalue(SelectedDataNameVar)     <-  "1 Select Data Folder --->"
  tclvalue(SelectedCalibFilenameVar)<-  "2 Select Video Calibration File --->"
  tclvalue(SelectedBodyFilenameVar) <-  "3 Select Subject Body File --->"
} # Reset button Dialog box content

TransformButton.Function<- function(){ # Transform Button from the dialog
  # Make sure all the required files are defined
  for (ObjectI in 1:length(ListGlobalObject)){
    if (!exists(ListGlobalObject[ObjectI])){
      MessageTransformFileError<-paste0("Sorry, not all required files are selected.\nDo you want to select them now?")
      UserResponse<-tk_messageBox(type="yesnocancel", icon="question", caption = "KinemaR Information", message=MessageTransformFileError)
      if (UserResponse=="yes"){
        TransformData.Function()
      } else if(UserResponse=="no"){
        stop(paste0("KinemaR has stopped because all required files weren't provided."))
      }
    }
  } # Check all files are selected if not prompt user if he wants to select them now or abort
  # all files are already selected
  TransformData.Function()
} # Transform button

# Selection Functions --------------------------------------------------------

SelectCSVDataFileDir.Function <- function(){
  MessageChooseDataDir="Choose the folder containing the Kinematic Data CSV Files."
  InputDirPath<-tk_choose.dir(default="", caption=MessageChooseDataDir) # Prompt the user to select an inputdirectory
  InputDirName<-basename(InputDirPath) # Defines Name of Input directory
  ListInputFilePath<-list.files(path=InputDirPath, pattern=".csv", all.files=FALSE, full.names=TRUE, ignore.case = TRUE) # Get the list of CSV filepath within InputDir
 
  
  
  while(length(ListInputFilePath)==0){ # Display a message if the folder does not contain any CSV file
    MessageNoCSVFileError=paste0("Sorry but \"",InputDirName, "\"\ndoes not contain any CSV file.\n")
    MessageNoCSVFileErrorFix=paste0("Please select a folder containing at least one CSV file.\n")
    MessageRetryCancel=paste0("\nClick Retry to Try Again\n or \nCancel to Abort.")
    UserChoice<-tk_messageBox(type = "retrycancel", message=paste0(MessageNoCSVFileError,"\n", MessageNoCSVFileErrorFix, "\n", MessageRetryCancel), caption = "KinemaR Information", icon="question")
    if(UserChoice=="cancel"){
      MessageStop<-"Sorry, KinemaR has stopped.\n"
      stop(paste0(MessageStop,"\n", MessageNoCSVFileError, "\n",MessageNoCSVFileErrorFix))
    } else if (UserChoice=="retry") {
      rm(list=c("InputDirPath","InputDirName", "ListInputFilePath"))
      InputDirPath<-tk_choose.dir(default="", caption=MessageChooseDataDir) 
      InputDirName<-basename(InputDirPath) 
      ListInputFilePath<-list.files(path=InputDirPath, pattern=".CSV", all.files=FALSE, full.names=TRUE, ignore.case = TRUE) # Get the list of CSV filepath within InputDir
    }
  } # Display a message if the folder does not contain any CSV file
  
  for (FileI in 1:length(ListInputFilePath)){
    InputFilePathI <- ListInputFilePath[FileI] # Defines the Path of the File to be processed
    InputFilenameI <- basename(InputFilePathI) # Get the Filename of the File being processed
    InputFilenameINoExt <- gsub(".csv","", InputFilenameI,ignore.case = TRUE) # Create a filename without extension
    
    if(length(unlist(strsplit(InputFilenameINoExt, "_")))<2){
      MessageFilenameError<-paste0("Sorry, ",InputFilenameI, "\ndoes not match KinemaR CSV file name specification.")
      MessageFilenameErrorFix<-paste0("Please, ensure that the name of CSV files include at least 1 underscore (Example: \"Date_Subject.csv\").")
      tk_messageBox(type = "ok", message=paste0(MessageFilenameError,"\n",MessageFilenameErrorFix), caption = "KinemaR Information", icon="warning")
      MessageStop<-"Sorry, KinemaR has stopped.\n"
      stop(paste0(MessageStop,"\n", MessageFilenameError,"\n", MessageFilenameErrorFix))
    } # Check that Filenames have at least 1 underscore
    
    # Read the data to check data Integrity
    InputDataI <- read.table(InputFilePathI, sep = ",", fill = TRUE, header = TRUE,comment.char = "", nrows = 100000)
    InputDataI <- InputDataI[,-seq(4,dim(InputDataI)[2],3)] # remove flag columns
    InputDataI <- InputDataI[-1,] # remove empty first row
    
    if(anyNA(InputDataI)){
      MessageMissingValueError<-paste0("Sorry, ", InputFilenameI, "\nhas missing values.")
      MessageMissingValueFileErrorFix<-paste0("Please check ", InputFilenameI, " the ", FileI,"th file in the folder for missing values.")
      tk_messageBox(type = "ok", message=paste0(MessageMissingValueError, "\n", MessageMissingValueFileErrorFix), caption = "KinemaR Information", icon="warning")
      MessageStop<-"Sorry, KinemaR has stopped.\n"
      stop(paste0(MessageStop,"\n", MessageMissingValueError, "\n", MessageMissingValueFileErrorFix))
    } # If data has missing values then notify the user and stop KinemaR
  } # Check each CSV file
  
  assign("ListInputFilePath", ListInputFilePath, envir = .GlobalEnv) # Assign the Variable to the global environment
  assign("InputDirPath", InputDirPath, envir = .GlobalEnv) # Assign the Variable to the global environment
  assign("InputDirName", InputDirName, envir = .GlobalEnv) # Assign the Variable to the global environment
  assign("ParentInputDirPath", dirname(InputDirPath), envir = .GlobalEnv) # Assign the Variable to the global environment
  
  # Update the Tcl Value 
  tclvalue(SelectedDataNameVar)<-paste0(InputDirName)
  
} # Select input directory

SelectVideoCalibCSV.Function <- function(){
  while(!exists("ListInputFilePath")){
    MessageMissingInputDataError<-"Sorry, the input directory must be defined before the Video Calibration Values CSV file."
    MessageMissingInputDataErrorFix<-"Do you want to choose the folder containing the Kinematic CSV data files now?"
    MessageRetryCancel=paste0("\nClick Yes to select it now\n or \nCancel to Abort.")
    UserChoice<-tk_messageBox(type="yesno", message=paste0(MessageMissingInputDataError, "\n", MessageMissingInputDataErrorFix, "\n", MessageRetryCancel), caption="KinemaR Information", icon="warning" )
    if(UserChoice=="yes"){
      SelectCSVDataFileDir.Function()
    } else if (UserChoice=="no") {
      MessageStop<-"Sorry, KinemaR has stopped.\n"
      stop(paste0(MessageStop,"\n", MessageMissingInputDataError, "\n", MessageMissingInputDataErrorFix))
    }
  } # Ensure the Input directory is select prior to select the Calibration File
  
  CSVFilter<-matrix(c("CSV File",".CSV","CSV File",".csv"),2,2,byrow=TRUE) # Create a filter to select only CSV files
  MessageChooseCalibCSVFile="Choose the CSV file containing Video Calibration Values."
  VideoCalibrationFilePath <- tk_choose.files(default="Video Calibration Values.CSV", caption=MessageChooseCalibCSVFile, multi=FALSE, filters=CSVFilter)
  VideoCalibrationFilename<-basename(VideoCalibrationFilePath)
  VideoCalibrationData <- read.table(VideoCalibrationFilePath, sep = ",", fill = TRUE, header = TRUE,comment.char = "", nrows=1000,check.names = FALSE)
  
  
  while((any(colnames(VideoCalibrationData)=="Filename") # Ensure Video Calibration File has the correct header
         && any(colnames(VideoCalibrationData)=="Width (pixel)")
         && any(colnames(VideoCalibrationData)=="Height (pixel)")
         && any(colnames(VideoCalibrationData)=="Width (cm)")
         && any(colnames(VideoCalibrationData)=="Height (cm)"))!=TRUE ){
    
    MessageCalibrationHeaderError<-paste0("Sorry, ",VideoCalibrationFilename," does not contain the expected columns.")
    MessageCalibrationHeaderErrorFix<-paste0("Video Calibration header must be Filename, Width (pixel), Height (pixel), Width (cm), Height (cm)")
    MessageRetryCancel=paste0("\nClick Retry to Try Again\n or \nCancel to Abort.")
    UserChoice<-tk_messageBox(type = "retrycancel", message=paste0(MessageCalibrationHeaderError,"\n",MessageCalibrationHeaderErrorFix,"\n", MessageRetryCancel), caption = "KinemaR Information", icon="warning")
    if (UserChoice=="retry") {
      rm(list=c("VideoCalibrationFilePath","VideoCalibrationFilename", "VideoCalibrationData"))
      SelectVideoCalibCSV.Function()
    } else if(UserChoice=="cancel"){
      MessageStop<-"Sorry, KinemaR has stopped.\n"
      stop(paste0(MessageStop,"\n", MessageCalibrationHeaderError, "\n", MessageCalibrationHeaderErrorFix))
    } 
  } # Ensure Video Calibration File has the correct header
  
  if(anyNA(VideoCalibrationData)){
    MessageCalibCSVFileMissingValueError<-paste0("Sorry, ", VideoCalibrationFilename, "\nhas missing values.")
    MessageCalibCSVFileMissingValueErrorFix<-paste0("Please check ", VideoCalibrationFilename, " for missing values.")
    tk_messageBox(type = "ok", message=paste0(MessageCalibCSVFileMissingValueError,"\n", MessageCalibCSVFileMissingValueErrorFix), caption = "KinemaR Information", icon="warning")
    MessageStop<-"Sorry, KinemaR has stopped.\n"
    stop(paste0(MessageStop,"\n", MessageCalibCSVFileMissingValueError, "\n", MessageCalibCSVFileMissingValueErrorFix))
  } # Make sure Calibration file as no missing values
  
  # Create a VideoCalibrationID within the CSV video Calibration file
  for (RowI in 1:length(VideoCalibrationData$Filename)){
    DateI<- unlist(strsplit(as.character(VideoCalibrationData$Filename[RowI]), "_"))[1] # Get the name before the 1st underscore
    SubjectI <- unlist(strsplit(as.character(VideoCalibrationData$Filename[RowI]), "_"))[2]
    VideoCalibrationData$VideoCalibrationID[RowI]<-paste0(DateI,"_", SubjectI)
  }  # Create a VideoCalibrationID within the CSV video Calibration file
  
  # Make sure all Input File have a Video Calibration Data
  for (FileI in 1:length(ListInputFilePath)){
    InputFilePathI <- ListInputFilePath[FileI] # Get the Path of the Data file
    InputFilenameI <- basename(InputFilePathI) # Get the name of the data file
    InputFilenameINoExt <- gsub(".csv","", InputFilenameI,ignore.case = TRUE) # remove the csv extension from the name
    DateI <- unlist(strsplit(InputFilenameINoExt, "_"))[1] # Get the name before the 1st underscore
    SubjectI <- unlist(strsplit(InputFilenameINoExt, "_"))[2] # Get the name after the 1st underscore and before the 2nd if more than 1
    VideoCalibrationID <- paste0(DateI,"_",SubjectI) # Create a Calibration ID from the Name of the CSV data file
    
    if(any(as.character(VideoCalibrationData$VideoCalibrationID)==VideoCalibrationID)!=TRUE){
      MessageMissingCalibError=paste0("Sorry, the CSV Data File ", InputFilenameI, " have no calibration.")
      MessageMissingCalibErrorFix=paste0("Please make sure ", VideoCalibrationID," is defined in the Video Calibration CSV file.")
      tk_messageBox(type="ok", message=paste0(MessageMissingCalibError, "\n", MessageMissingCalibErrorFix), caption="KinemaR Information", icon="warning" )
      MessageStop<-"Sorry, KinemaR has stopped.\n"
      stop(paste0(MessageStop,"\n", MessageMissingCalibError, "\n", MessageMissingCalibErrorFix))
    } # Stop if an InputFIle does not have calibration Values
    
    VideoCalibrationValuesI <- VideoCalibrationData[VideoCalibrationData$VideoCalibrationID == VideoCalibrationID,] # Get the Calibration Values for the FileI
    if(dim(VideoCalibrationValuesI)[1]!=1){
      MessageAmbiguousCalibrationError=paste0("Sorry, Video Calibration Values for ", InputFilenameI, " are ambiguous.")
      MessageAmbiguousCalibrationErrorFix=paste0("Please make sure ", VideoCalibrationID," is unique within the Video Calibration CSV file.")
      tk_messageBox(type="ok", message=paste0(MessageAmbiguousCalibrationError, "\n", MessageAmbiguousCalibrationErrorFix), caption="KinemaR Information", icon="warning" )
      MessageStop<-"Sorry, KinemaR has stopped.\n"
      stop(paste0(MessageStop,"\n", MessageAmbiguousCalibrationError, "\n", MessageAmbiguousCalibrationErrorFix))
    } # Make sure the Video Calibration Value for the FileI are unique
    
    PixeltoCmXI <- VideoCalibrationValuesI[["Width (cm)"]]  / VideoCalibrationValuesI[["Width (pixel)"]]
    PixeltoCmYI <- VideoCalibrationValuesI[["Height (cm)"]] / VideoCalibrationValuesI[["Height (pixel)"]]
    if (mode(PixeltoCmYI)!="numeric" || mode(PixeltoCmXI)!="numeric" || PixeltoCmYI==0 || PixeltoCmXI==0){
      MessagePixeltoCmError=paste0("Sorry, calibration values for ", InputFilenameI, " could not be defined.")
      MessagePixeltoCmErrorFix=paste0("Please check calibration values for ", InputFilenameI," .")
      tk_messageBox(type="ok", message=paste0(MessagePixeltoCmError, "\n", MessagePixeltoCmErrorFix), caption="KinemaR Information", icon="warning" )
      MessageStop<-"Sorry, KinemaR has stopped.\n"
      stop(paste0(MessageStop,"\n", MessagePixeltoCmError, "\n", MessagePixeltoCmErrorFix))
      
    }
  } # Stop the process if an Input File does not have Calibration values
  
  assign("VideoCalibrationData", VideoCalibrationData, envir = .GlobalEnv)
  assign("VideoCalibrationFilePath", VideoCalibrationFilePath, envir = .GlobalEnv)
  tclvalue(SelectedCalibFilenameVar)<-paste0(basename(VideoCalibrationFilePath))
} # Select the Video Calibration CSV File

SelectSubjectBodyCSV.Function <- function(){
  while(!exists("ListInputFilePath")){
    MessageMissingInputDataError<-"Sorry, the input directory must be defined before the Subject Body Values CSV file."
    MessageMissingInputDataErrorFix<-"Do you want to choose the folder containing the Kinematic CSV data files now?"
    MessageRetryCancel=paste0("\nClick Yes to select it now\n or \nCancel to Abort.")
    UserChoice<-tk_messageBox(type="yesno", message=paste0(MessageMissingInputDataError, "\n", MessageMissingInputDataErrorFix, "\n", MessageRetryCancel), caption="KinemaR Information", icon="warning" )
    if(UserChoice=="yes"){
      SelectCSVDataFileDir.Function()
    } else if (UserChoice=="no") {
      MessageStop<-"Sorry, KinemaR has stopped.\n"
      stop(paste0(MessageStop,"\n", MessageMissingInputDataError, "\n", MessageMissingInputDataErrorFix))
    }
  } # Ensure the Input directory is selected prior to select the Subject Body File
  while(!exists("VideoCalibrationData")){
    MessageMissingCalibrationDataError<-"Sorry, the Video Calibration CSV File must be defined before the Subject Body Values CSV file."
    MessageMissingCalibrationDataErrorFix<-"Do you want to choose the Video Calibration CSV File now?"
    MessageRetryCancel=paste0("\nClick Yes to select it now\n or \nCancel to Abort.")
    UserChoice<-tk_messageBox(type="yesno", message=paste0(MessageMissingCalibrationDataError, "\n", MessageMissingCalibrationDataErrorFix, "\n", MessageRetryCancel), caption="KinemaR Information", icon="warning" )
    if(UserChoice=="yes"){
      SelectVideoCalibCSV.Function()
    } else if (UserChoice=="no") {
      MessageStop<-"Sorry, KinemaR has stopped.\n"
      stop(paste0(MessageStop,"\n", MessageMissingCalibrationDataError, "\n", MessageMissingCalibrationDataErrorFix))
    }
  } # Ensure the Video Calibration File is selected prior to select the Subject Body File
  
  CSVFilter<-matrix(c("CSV File",".CSV","CSV File",".csv"),2,2,byrow=TRUE) # Create a filter to select only CSV files
    MessageChooseSubjectCSVFile="Choose the CSV file containing Subject Body Values."
  SubjectBodyFilePath <- tk_choose.files(default="Subject Body Values.CSV", caption=MessageChooseSubjectCSVFile, multi=FALSE, filters=CSVFilter)
  SubjectBodyFilename<-basename(SubjectBodyFilePath)
  SubjectBodyData <- read.table(SubjectBodyFilePath, sep = ",", fill = TRUE, header = TRUE,comment.char = "", nrows=1000, check.names = FALSE)

  
  while((any(colnames(SubjectBodyData)=="Subject") # Ensure Subject Body File has the correct header
         && any(colnames(SubjectBodyData)=="Femur (cm)")
         && any(colnames(SubjectBodyData)=="Tibia (cm)"))!=TRUE ){
    MessageSubjectHeaderError<-paste0("Sorry, ",SubjectBodyFilename," does not contain the expected columns.")
    MessageSubjectHeaderErrorFix<-paste0("Subject header must be Subject, Tibia (cm), Femur (cm)")
    MessageRetryCancel=paste0("\nClick Retry to Try Again\n or \nCancel to Abort.")
    UserChoice<-tk_messageBox(type = "retrycancel", message=paste0(MessageSubjectHeaderError,"\n",MessageSubjectHeaderErrorFix,"\n", MessageRetryCancel), caption = "KinemaR Information", icon="warning")
    if (UserChoice=="retry") {
      rm(list=c("SubjectBodyFilePath","SubjectBodyFilename", "SubjectBodyData"))
      SelectSubjectBodyCSV.Function()
    } else if(UserChoice=="cancel"){
      MessageStop<-"Sorry, KinemaR has stopped.\n"
      stop(paste0(MessageStop,"\n", MessageSubjectHeaderError, "\n", MessageSubjectHeaderErrorFix))
    } 
  } # Ensure SubjectBody File has the correct header
  
  if(anyNA(SubjectBodyData)){
    MessageSubjectCSVFileMissingValueError<-paste0("Sorry, ", SubjectBodyFilename, "\nhas missing values.")
    MessageSubjectCSVFileMissingValueErrorFix<-paste0("Please check ", SubjectBodyFilename, " for missing values.")
    tk_messageBox(type = "ok", message=paste0(MessageSubjectCSVFileMissingValueError,"\n", MessageSubjectCSVFileMissingValueErrorFix), caption = "KinemaR Information", icon="warning")
    MessageStop<-"Sorry, KinemaR has stopped.\n"
    stop(paste0(MessageStop,"\n", MessageSubjectCSVFileMissingValueError, "\n", MessageSubjectCSVFileMissingValueErrorFix))
  } # Make sure Subject file as no missing values
  
  # Create a SubjectID within the CSV video Calibration file
  for (RowI in 1:length(SubjectBodyData$Subject)){
    if(length(strsplit(as.character(SubjectBodyData$Subject[RowI]), "_"))>1){
      DateI<- unlist(strsplit(as.character(SubjectBodyData$Subject[RowI]), "_"))[1] # Get the name before the 1st underscore
      SubjectI <- unlist(strsplit(as.character(SubjectBodyData$Subject[RowI]), "_"))[2]
      SubjectBodyData$SubjectID[RowI]<-paste0(DateI,"_", SubjectI)
    } else {
      SubjectBodyData$SubjectID[RowI]<-as.character(SubjectBodyData$Subject[RowI])
    }
  }  # Create a SubjectID within the CSV video Calibration file
  
  # Make sure all Input File have a Video Calibration Data
  for (FileI in 1:length(ListInputFilePath)){
    InputFilePathI <- ListInputFilePath[FileI] # Get the Path of the Data file
    InputFilenameI <- basename(InputFilePathI) # Get the name of the data file
    InputFilenameINoExt <- gsub(".csv","", InputFilenameI,ignore.case = TRUE) # remove the csv extension from the name
    DateI <- unlist(strsplit(InputFilenameINoExt, "_"))[1] # Get the name before the 1st underscore
    SubjectI <- unlist(strsplit(InputFilenameINoExt, "_"))[2] # Get the name after the 1st underscore and before the 2nd if more than 1
    VideoCalibrationID <- paste0(DateI,"_",SubjectI) # Create a Calibration ID from the Name of the CSV data file
    SubjectID <- paste0(SubjectI) # Create a Subject ID from the Name of the CSV data file
    # SubjectID <- paste0(DateI,"_",SubjectI) # Altneratively you can use the Date and Subject as an ID
    if(any(as.character(SubjectBodyData$SubjectID)==SubjectID)!=TRUE){
      MessageMissingSubjectError=paste0("Sorry, the CSV Data File ", InputFilenameI, " have no Subject Body Values")
      MessageMissingSubjectErrorFix=paste0("Please make sure the subject ", SubjectID," is defined in the Subject Body Values CSV file.")
      tk_messageBox(type="ok", message=paste0(MessageMissingSubjectError, "\n", MessageMissingSubjectErrorFix), caption="KinemaR Information", icon="warning" )
      MessageStop<-"Sorry, KinemaR has stopped.\n"
      stop(paste0(MessageStop,"\n", MessageMissingCalibError, "\n", MessageMissingCalibErrorFix))
    } # Make sure all Input File have subject body values
    SubjectBodyValuesI <- SubjectBodyData[SubjectBodyData$SubjectID == SubjectI,]
    if(dim(SubjectBodyValuesI)[1]!=1){
      MessageAmbiguousSubjectError=paste0("Sorry, Subject Body Values for ", InputFilenameI, " are ambiguous.")
      MessageAmbiguousSubjectErrorFix=paste0("Please make sure ", SubjectID," is unique within the Subject Body CSV file.")
      tk_messageBox(type="ok", message=paste0(MessageAmbiguousSubjectError, "\n", MessageAmbiguousSubjectErrorFix), caption="KinemaR Information", icon="warning" )
      MessageStop<-"Sorry, KinemaR has stopped.\n"
      stop(paste0(MessageStop,"\n", MessageAmbiguousSubjectError, "\n", MessageAmbiguousSubjectErrorFix))
    } # Make sure the Video Calibration Value for the FileI are unique
    
    Femur <- SubjectBodyValuesI[["Femur (cm)"]]    # Get the Femur and tibia values
    Tibia <- SubjectBodyValuesI[["Tibia (cm)"]]    # Get the Femur and tibia values
    
    if (mode(Femur)!="numeric" || mode(Tibia)!="numeric" || Femur==0 || Tibia==0){
      MessageFemurTibiaError=paste0("Sorry, Femur or Tibia length for ", InputFilenameI, " could not be defined.")
      MessageFemurTibiaErrorFix=paste0("Please check Femur and Tibia length values for ", InputFilenameI," .")
      tk_messageBox(type="ok", message=paste0(MessageFemurTibiaError, "\n", MessageFemurTibiaErrorFix), caption="KinemaR Information", icon="warning" )
      MessageStop<-"Sorry, KinemaR has stopped.\n"
      stop(paste0(MessageStop,"\n", MessageFemurTibiaError, "\n", MessageFemurTibiaErrorFix))
    } # Make Sure Tibia and Femur are defined
    
    InputDataI <- read.table(InputFilePathI, header = TRUE, sep = ",", fill = TRUE, colClasses = c("character"), comment.char = "", nrows=100000)  # Read the file
    # PreProcess the InputData
    InputDataI <- InputDataI[,c(-4,-7,-10,-13,-16,-17,-18,-19)]    # Delete the Flag columns
    InputDataI <- InputDataI[-1,]    # Delete the first row (always present and empty when generated from KinemaJ)
    Hip.X.Pixel   <- as.numeric(InputDataI[,4])
    Hip.Y.Pixel   <- as.numeric(InputDataI[,5])
    Ankle.X.Pixel <- as.numeric(InputDataI[,6])
    Ankle.Y.Pixel <- as.numeric(InputDataI[,7])
    
    # Get the calibration values corresponding to the CalibrationID
    VideoCalibrationValuesI <- VideoCalibrationData[VideoCalibrationData$VideoCalibrationID == VideoCalibrationID,]
    PixeltoCmXI <- VideoCalibrationValuesI[["Width (cm)"]]  / VideoCalibrationValuesI[["Width (pixel)"]]  # Extract the transformation factor to convert pixel into cm
    PixeltoCmYI <- VideoCalibrationValuesI[["Height (cm)"]] / VideoCalibrationValuesI[["Height (pixel)"]]  # Extract the transformation factor to convert pixel into cm
    
    # Transform the Y coordinates from pixel to Cm and slide to have min value at 0
    Hip.X   <- as.numeric(Hip.X.Pixel * PixeltoCmXI)
    Hip.Y   <- as.numeric(Hip.Y.Pixel   * PixeltoCmYI)
    Ankle.X <- as.numeric(Ankle.X.Pixel * PixeltoCmXI)
    Ankle.Y <- as.numeric(Ankle.Y.Pixel * PixeltoCmYI)
    
    
    HipAnkle<-as.numeric(sqrt((Hip.X - Ankle.X ) ^ 2 + (Hip.Y - Ankle.Y) ^ 2))
    
    # Make sure the the Tibia and Femur are long enough to intersect otherwise stop KinemaR and inform the user
    if(max(HipAnkle) > (Femur+Tibia) || min(HipAnkle) < abs(Femur-Tibia)){
      # Create the error Message
      if(max(HipAnkle) > (Femur+Tibia) && min(HipAnkle) >= abs(Femur-Tibia)){
        MessageErrorReason=paste0("Sum of Femur (",Femur,") and Tibia (", Tibia,") for Subject ",SubjectID," should larger than ",round(max(HipAnkle), digits=2), "cm and the difference smaller than ", round(min(HipAnkle), digits=2),"cm.\n")
      } else if(max(HipAnkle) <= (Femur+Tibia) && min(HipAnkle) < abs(Femur-Tibia)){
        MessageErrorReason=paste0("Difference between Tibia (", Tibia,") and Femur (",Femur,") should be smaller than ", round(min(HipAnkle), digits=2),"cm and the sum larger than ",round(max(HipAnkle), digits=2), "cm.\n")
      } else {
        MessageErrorReason=paste0("Sum of Tibia (",Tibia,") and Femur (", Femur,") for Subject ",SubjectID," should be larger than ", round(max(HipAnkle), digits=2),"cm and the difference smaller than ",round(min(HipAnkle), digits=2), "cm.\n")
      }
      MessageErrorImpossibleKnee=paste0("The position of the Knee can't be calcuated for ", SubjectID,".\nFile: ", InputFilenameI, ".\n", MessageErrorReason, "\nPlease check the Subject Body Values for ", SubjectID,".")
      tk_messageBox(type = "ok", message=paste0(MessageErrorImpossibleKnee), caption = "KinemaR Information", icon="warning")
      stop(paste0("KinemaR has stopped.", MessageErrorImpossibleKnee))
    } # End if Tibia and femur are long enough
  } # Stop the process if an Input File does not have Calibration values
  
  assign("SubjectBodyData", SubjectBodyData, envir = .GlobalEnv)
  assign("SubjectBodyFilePath", SubjectBodyFilePath, envir = .GlobalEnv)
  tclvalue(SelectedBodyFilenameVar)<-paste0(basename(SubjectBodyFilePath))
} # Select the Subject Body Values CSV File

# Transform Data from KinemaJ ----
TransformData.Function <- function() {
  while(!exists("ListInputFilePath") || length(ListInputFilePath)==0){
    SelectCSVDataFileDir.Function()
  }   # Requires InputDirectory with at least 1 CSV file
  while(!exists("VideoCalibrationData")){
    SelectVideoCalibCSV.Function()
  }   # Requires Video Calibration CSV File
  while(!exists("SubjectBodyData")){
    SelectSubjectBodyCSV.Function()
  }   # Requires Subject Body CSV file
  
  for (FileI in 1:length(ListInputFilePath)){ # Process each CSV file
    ################### Progress bar
    assign(paste0("TimeFile",FileI),Sys.time())  
    if (FileI==1){
      TimeElapsed<-2
      EstimatedRemainingTime<-9
      KinemaRProgressBar <- tkProgressBar(title="KinemaR Transformation", label="KinemaR Transformation", min=0, max=100, initial=0, width=300)
    }else if (FileI>1){
      TimeElapsed<-ceiling(as.numeric(difftime(get(paste0("TimeFile",FileI)),TimeFile1, units="secs")))
      ProcessingTimePerFile<-ceiling(as.numeric(difftime(get(paste0("TimeFile",FileI)),get(paste0("TimeFile",FileI-1)), units="secs")))
      NbFileLeft <- length(ListInputFilePath) - FileI
      EstimatedRemainingTime<-ceiling(NbFileLeft*ProcessingTimePerFile)
    }
    ValueProgress<-100*FileI/length(ListInputFilePath)
    ProgressMessage <- sprintf("%d%%", round(ValueProgress))
    setTkProgressBar(KinemaRProgressBar, ValueProgress, title="KinemaR Transformation", label=paste0(ProgressMessage, " done. Elapsed ", TimeElapsed,"s. Remaining ", EstimatedRemainingTime, "s."))
    ################### End of update the progress bar
    
    if (FileI == 1) {  # For the first file create and output folder
      OutputDirName <- paste0(InputDirName,"_Transformed")    # Create an output folder with a suffix to the input folder
      OutputDirPathIni <- file.path(ParentInputDirPath, OutputDirName)    # Create the Path for the Output dolfer
      OutputDirPath    <- OutputDirPathIni
      n=1
      while(file.exists(OutputDirPath)==TRUE){     
        n=n+1;
        OutputDirPath  <- paste0(OutputDirPathIni,"_",n)
      } # Create a unique output folder name
      dir.create(OutputDirPath, showWarnings = FALSE)    # Create the output folder
      OutputDirPathTables  <- file.path(OutputDirPath,"Tables")
      dir.create(OutputDirPathTables, showWarnings = FALSE)    # Create the output folder
    } # Create an output folder
    
    InputFilePathI <- ListInputFilePath[FileI] # Defines the Path of the File to be processed
    InputDataI <- read.table(InputFilePathI, header = TRUE, sep = ",", fill = TRUE, colClasses = c("character"), comment.char = "", nrows=100000)  # Read the file
    InputFilenameI      <- basename(InputFilePathI)  # Get the Filename of the File being processed
    InputFilenameINoExt <- gsub(".csv","", InputFilenameI,ignore.case = TRUE)     # Create a filename without extension
    if (length(unlist(strsplit(InputFilenameINoExt, "_")))>2){
      for (SubNameI in 3:length(unlist(strsplit(InputFilenameINoExt, "_")))){
        assign(paste0("Var", SubNameI), unlist(strsplit(InputFilenameINoExt, "_"))[SubNameI])        # Create an object with the String from the filename
      } # end of for SubnameI
    } # If there is more than 2 variables in the filename assign them to VAR#
    DateI      <- unlist(strsplit(InputFilenameINoExt, "_"))[1]    # Split the Filename into relevant strings
    SubjectI   <- unlist(strsplit(InputFilenameINoExt, "_"))[2]    # Split the Filename into relevant strings
    VideoCalibrationID <- paste0(DateI,"_", SubjectI)     # Get a string to look for the video calibration values later
    SubjectID <- SubjectI    # Get a string to look for the tibia and femur values later
    # If you are using your subject are growing you may want to add the day and use this string instead
    # SubjectCalibrationID<-paste0(DateI,"_",SubjectI)
    
    # PreProcess the InputData
    InputDataI <- InputDataI[,c(-4,-7,-10,-13,-16,-17,-18,-19)]    # Delete the Flag columns
    InputDataI <- InputDataI[-1,]    # Delete the first row (always present and empty when generated from KinemaJ)
    DimDataI <- as.integer(dim(InputDataI)[1])    # Get the dimension of the PreProcessInputData
    
    # Create an OutputData and add the Filename variables
    OutputDataI           <- data.frame(Filename = (as.character(rep(InputFilenameINoExt, DimDataI))))
    
    # Get the coordinates in pixel of the detected markers
    Crest.X.Pixel <- as.numeric(InputDataI[,2])
    Crest.Y.Pixel <- as.numeric(InputDataI[,3])
    Hip.X.Pixel   <- as.numeric(InputDataI[,4])
    Hip.Y.Pixel   <- as.numeric(InputDataI[,5])
    Ankle.X.Pixel <- as.numeric(InputDataI[,6])
    Ankle.Y.Pixel <- as.numeric(InputDataI[,7])
    Paw.X.Pixel   <- as.numeric(InputDataI[,8])
    Paw.Y.Pixel   <- as.numeric(InputDataI[,9])
    Toe.X.Pixel   <- as.numeric(InputDataI[,10])
    Toe.Y.Pixel   <- as.numeric(InputDataI[,11])
    
    # Get the calibration values corresponding to the CalibrationID
    VideoCalibrationValuesI <- VideoCalibrationData[VideoCalibrationData$VideoCalibrationID == VideoCalibrationID,]
    PixeltoCmXI <- VideoCalibrationValuesI[["Width (cm)"]]  / VideoCalibrationValuesI[["Width (pixel)"]]  # Extract the transformation factor to convert pixel into cm
    PixeltoCmYI <- VideoCalibrationValuesI[["Height (cm)"]] / VideoCalibrationValuesI[["Height (pixel)"]]  # Extract the transformation factor to convert pixel into cm
    
    # Transform the coordinates from pixel to Cm
    Crest.X <- as.numeric(Crest.X.Pixel * PixeltoCmXI)
    Crest.Y <- as.numeric(Crest.Y.Pixel * PixeltoCmYI)
    Hip.X   <- as.numeric(Hip.X.Pixel   * PixeltoCmXI)
    Hip.Y   <- as.numeric(Hip.Y.Pixel   * PixeltoCmYI)
    Ankle.X <- as.numeric(Ankle.X.Pixel * PixeltoCmXI)
    Ankle.Y <- as.numeric(Ankle.Y.Pixel * PixeltoCmYI)
    Paw.X   <- as.numeric(Paw.X.Pixel   * PixeltoCmXI)
    Paw.Y   <- as.numeric(Paw.Y.Pixel   * PixeltoCmYI)
    Toe.X   <- as.numeric(Toe.X.Pixel   * PixeltoCmXI)
    Toe.Y   <- as.numeric(Toe.Y.Pixel   * PixeltoCmYI)
    
    
    ########### This part Gets the Subject Body Values matching the file being processed
    SubjectBodyValuesI <- SubjectBodyData[SubjectBodyData$SubjectID == SubjectI,]
    Femur <- SubjectBodyValuesI[["Femur (cm)"]] # Get the Femur and tibia values
    Tibia <- SubjectBodyValuesI[["Tibia (cm)"]] # Get the Femur and tibia values
    HipKnee   <- rep(Femur, DimDataI)    # Create a conventional name for femur and tibia segment length in cm
    KneeAnkle <- rep(Tibia, DimDataI)    # Create a conventional name for femur and tibia segment length in cm
    
    ######### This next block calculates the coordinates of Knee
    # Step 1 Calculate the non anatomical segment HipAnkle required for subsquent calculation
    SegmentLength.Function<-function(Start, End){
      XStart <- get(paste0(Start, ".X"))
      XEnd <- get(paste0(End, ".X"))
      YStart <- get(paste0(Start, ".Y"))
      YEnd <- get(paste0(End, ".Y"))
      as.numeric(sqrt(((XStart - XEnd) ^ 2 + (YStart - YEnd) ^ 2)))
    } # Function to calculate the length between two points
    HipAnkle<-SegmentLength.Function("Hip", "Ankle")    # Calculate the length of the segment between Hip and Ankle
    
    # Here are some explanations on the calculation process
    # The knee can be located at the interesct of a circle of 2*Femur diameter centered on the Hip and on a circle of 2*Tibia diameter cenetered on the Ankle.
    # We define a point P on the segment HipAnkle at A distance from the Hip and B distance from the Ankle.
    # Such as HipAnkle = A + B.
    # P is at the intersect of the line crossing the two circle intersects and the HipAnkle segment
    # H is the segment length between point P and the circle intersects
    # Equation  1: H^2+A^2=HipKnee^2
    # Equation  2: H^2+B^2=KneeAnkle^2
    # Equation  3: A+B=HipAnkle
    # Equation  4: Substracting Equation 2 from Equation 1 gives: A^2-B^2=HipKnee^2-KneeAnkle^2
    # Equation  5: Rearranging Equation 3 gives:   B   = HipAnkle-A
    # Equation  6: Square of Equation 5 gives:     B^2 = (HipAnkle-A)^2
    # Equation  7: Rearranging Equation 6 gives:   B^2 = HipAnkle^2 + A^2 - 2*A*HipAnkle
    # Equation  8: Replacing B^2 in Equation 4 from Equation 7 gives: A^2-(HipAnkle^2 + A^2 - 2*A*HipAnkle) = HipKnee^2-KneeAnkle^2
    # Equation  9: Rearranging Equation 8 gives:                      -HipAnkle^2 +2*A*HipAnkle=HipKnee^2-KneeAnkle^2
    # Equation 10: Isolating A in Equation 9 gives:                    A = (HipKnee^2-KneeAnkle^2+HipAnkle^2)/(2*HipAnkle)
    
    A <- (HipKnee ^ 2 - KneeAnkle ^ 2 + HipAnkle ^ 2) / (2 *HipAnkle) # Step 2 Calculate length of Segment A
    H <- sqrt(HipKnee ^ 2 - A ^ 2)    # Step 3  Calculate the length of segment H
    
    # Calculate the coordinates of the point P
    XP <- Hip.X + ((A * (Ankle.X - Hip.X)) / HipAnkle)
    YP <- Hip.Y + ((A * (Ankle.Y - Hip.Y)) / HipAnkle)
    
    # Calculate the Knee coordinates
    # Subject walks to the right so the Knee is located on the right side of the HipAnkle segment unless the subject dislocated its knee
    Knee.X <- XP - ((H * (Ankle.Y - Hip.Y)) / (HipAnkle))
    Knee.Y <- YP + ((H * (Ankle.X - Hip.X)) / (HipAnkle))
    # Knee coordinates if the subject walks to the left but we recommend to flip the video recording within KinemaJ yet you can use the lines below to calculate the knee coordinates if the animal walks toward the left
    # Knee.X.L<-XP+((H*(Ankle.Y-Hip.Y))/(HipAnkle))
    # Knee.Y.L<-YP-((H*(Ankle.X-Hip.X))/(HipAnkle))
    
    # Make sure the the All knee coordinates are  otherwise stop KinemaR and inform the user
    if(anyNA(Knee.X) || anyNA(Knee.Y)){
      tk_messageBox(type = "ok", message=paste0("Knee coordinates for ", InputFilenameI, " could not be computed. Please check the Subject Body Value CSV File for ", SubjectID, " or the CSV Data File ", InputFilenameI), caption = "KinemaR Information", icon="warning")
      stop(paste0("KinemaR has stopped because the Knee coordinates for ", InputFilenameI, " could not be computed. Please check the Subject Body Value CSV File for ", SubjectID, " or the CSV Data File ", InputFilenameI))
    } # End if condtion
    
    ############### Add the values to the OutputData Table
    OutputDataI$Date      <- as.character(rep(DateI, DimDataI))
    OutputDataI$Subject   <- as.character(rep(SubjectI, DimDataI))
    if (length(unlist(strsplit(InputFilenameINoExt, "_")))>2){
      for (SubNameI in 3:length(unlist(strsplit(InputFilenameINoExt, "_")))){
        OutputDataI[[paste0("Var", SubNameI)]]  <- as.character(rep(get(paste0("Var",SubNameI)), DimDataI))
      }# Add the Variable from the filename to the outputdata table
    }     # If there is more than 2 variables in the filename add it as a variable in the OutputData
    OutputDataI$Frame     <- as.integer(InputDataI[,1])    # Add the Frame number from the ProcessInputData
    MinXI <- min(c(Crest.X, Hip.X, Knee.X, Ankle.X, Paw.X, Toe.X))
    MinYI <- min(c(Crest.Y, Hip.Y, Knee.Y, Ankle.Y, Paw.Y, Toe.Y))
    
    # Transform the coordinates from pixel to Cm and slide to have min value at 0
    OutputDataI$Crest.X <- as.numeric(Crest.X - MinXI) 
    OutputDataI$Crest.Y <- as.numeric(Crest.Y - MinYI) 
    OutputDataI$Hip.X   <- as.numeric(Hip.X - MinXI)   
    OutputDataI$Hip.Y   <- as.numeric(Hip.Y - MinYI)   
    OutputDataI$Knee.X   <- as.numeric(Knee.X - MinXI)   
    OutputDataI$Knee.Y   <- as.numeric(Knee.Y - MinYI)   
    OutputDataI$Ankle.X <- as.numeric(Ankle.X - MinXI) 
    OutputDataI$Ankle.Y <- as.numeric(Ankle.Y - MinYI) 
    OutputDataI$Paw.X   <- as.numeric(Paw.X - MinXI)   
    OutputDataI$Paw.Y   <- as.numeric(Paw.Y - MinYI)   
    OutputDataI$Toe.X   <- as.numeric(Toe.X - MinXI)   
    OutputDataI$Toe.Y   <- as.numeric(Toe.Y - MinYI)   
    
    # Normalizing Coordinates to the position of the Crest to take in account front and back movement
    Hip.X.Norm   <- OutputDataI$Hip.X   - OutputDataI$Crest.X   
    Knee.X.Norm   <- OutputDataI$Knee.X   - OutputDataI$Crest.X   
    Ankle.X.Norm <- OutputDataI$Ankle.X - OutputDataI$Crest.X   
    Paw.X.Norm   <- OutputDataI$Paw.X   - OutputDataI$Crest.X   
    Toe.X.Norm   <- OutputDataI$Toe.X   - OutputDataI$Crest.X   
    MinXNorm<-min(c(Hip.X.Norm, Knee.X.Norm, Ankle.X.Norm, Paw.X.Norm, Toe.X.Norm))    # Get the minimum of the Normalized Data 
    # Add Normalized to Crest X coordinates
    OutputDataI$Hip.X.NormToCrest   <- as.numeric(Hip.X.Norm   - MinXNorm)
    OutputDataI$Knee.X.NormToCrest   <- as.numeric(Knee.X.Norm   - MinXNorm)
    OutputDataI$Ankle.X.NormToCrest <- as.numeric(Ankle.X.Norm - MinXNorm)
    OutputDataI$Paw.X.NormToCrest   <- as.numeric(Paw.X.Norm   - MinXNorm)
    OutputDataI$Toe.X.NormToCrest   <- as.numeric(Toe.X.Norm   - MinXNorm)
    ### We are done with calculating the knee coordinnates
    ### Take a breath
    ### No really ;-)
    
    
    
    ### The next section calculates segment length using the function we have previously defined
    ListJoints<-c("Crest", "Hip", "Knee", "Ankle", "Paw", "Toe")    # Create the list of joints
    AllJointPairs<-combn(ListJoints, 2, simplify=TRUE)    # Create all possible pairs of Joints
    AllJointPairsRev<-AllJointPairs # Use the Reverse
    AllJointPairsRev[1,]<-AllJointPairs[2,] # Use the Reverse
    AllJointPairsRev[2,]<-AllJointPairs[1,] # Use the Reverse
    
    # Because we will calculate angles with the atan2 function which returns value between + pi and -pi
    # and To avoid break in angle continuity the End point of the vector should not cross the horizontal of the start point in negative X
    # In other words the end point cross the horizontal of the start on the left
    # This can occur for the HipKnee AnklePaw AnkleToe and PawToe segments
    # So we need to take their vector in the opposite direction
    # Change the CrestHip into HipCrest
    AllJointPairsRev[1,6]<-"Hip"
    AllJointPairsRev[2,6]<-"Knee"
    AllJointPairsRev[1,13]<-"Ankle"
    AllJointPairsRev[2,13]<-"Paw"
    AllJointPairsRev[1,14]<-"Ankle"
    AllJointPairsRev[2,14]<-"Toe"
    AllJointPairsRev[1,15]<-"Paw"
    AllJointPairsRev[2,15]<-"Toe"
    
    AllJointPairs<-AllJointPairsRev
    
    # For all pair calculate the length of the segment and add it to the Ouputdata
    for (PairI in 1:dim(AllJointPairs)[2]){
      JointStart<-AllJointPairs[1,PairI]
      JointEnd<-AllJointPairs[2,PairI]
      OutputDataI[[paste0("Length.", JointStart, JointEnd)]] <- SegmentLength.Function(JointStart, JointEnd) 
    }
    #### We are done with the segment length
    
    ### This section calculates Inclination
    VectorCoordinate.Function<-function(Start, End, Axis){
      StartCoordinate <- OutputDataI[[paste0(Start, ".", Axis)]]
      EndCoordinate <- OutputDataI[[paste0(End, ".", Axis)]]
      as.numeric(EndCoordinate   - StartCoordinate)
    } # Function to calculate oriented vectors coordinates
    
    for (PairI in 1:dim(AllJointPairs)[2]){
      JointStart<-AllJointPairs[1,PairI]
      JointEnd<-AllJointPairs[2,PairI]
      assign(paste0("Vector.", JointStart, JointEnd, ".X"), VectorCoordinate.Function(JointStart, JointEnd, "X"))
      assign(paste0("Vector.", JointStart, JointEnd, ".Y"), VectorCoordinate.Function(JointStart, JointEnd, "Y"))    
    }       # For all pair calculate the Vector X and Y coordinates and create an Object Vector.StartEnd.X and Vector.StartEnd.Y with its value
    Inclination.Function<-function(Start, End){
      VectorX <- get(paste0("Vector.", Start, End, ".X"))
      VectorY <- get(paste0("Vector.", Start, End, ".Y"))
      as.numeric(180/pi*atan2(VectorY, VectorX))
    }   # Function to calculate the oriented inclination of a vector to the horizontal
    for (PairI in 1:dim(AllJointPairs)[2]){
      JointStart<-AllJointPairs[1,PairI]
      JointEnd<-AllJointPairs[2,PairI]
      assign(paste0("Inclination.", JointStart, JointEnd), Inclination.Function(JointStart, JointEnd))
      OutputDataI[[paste0("Inclination.", JointStart, JointEnd)]] <- Inclination.Function(JointStart, JointEnd)
    }       # For all pair calculate the Inclination angle
    
    
    
    Angle.Function<-function(Start1, End1, Start2, End2){
      VectorX1 <- get(paste0("Vector.", Start1, End1, ".X"))
      VectorY1 <- get(paste0("Vector.", Start1, End1, ".Y"))
      VectorX2 <- get(paste0("Vector.", Start2, End2, ".X"))
      VectorY2 <- get(paste0("Vector.", Start2, End2, ".Y"))
      as.numeric(180/pi*(atan2(VectorY2, VectorX2)- atan2(VectorY1, VectorX1)))
    }   # Function to calculate the oriented inclination of a vector to the horizontal
    OutputDataI$Angle.Hip<-Angle.Function("Hip","Knee","Hip","Crest")
    OutputDataI$Angle.Knee<-Angle.Function("Hip","Knee","Ankle","Knee")
    OutputDataI$Angle.Ankle<-Angle.Function("Ankle","Paw", "Ankle","Knee")
    OutputDataI$Angle.Paw<-Angle.Function("Ankle","Paw","Paw","Toe")
    
    OutputFilenameI <- paste0(InputFilenameINoExt,".csv")       
    OutputFilePathI <- file.path(OutputDirPathTables, OutputFilenameI) # Create the output file Path
    
    write.table(OutputDataI, file = OutputFilePathI, row.names =FALSE, sep = ",")     # Write the table
    
    # Create a table for the first file to merge all the data into a single table. It will be saved for the last file
    if (FileI == 1){
      OutputData     <- OutputDataI # Create a new output Table for the Merged
    } else if (FileI>1){
      if (dim(OutputData)[2] > dim(OutputDataI)[2]){ # Compare the Nb Of Columns if Merge file has more columns
        MissingVars<- setdiff(colnames(OutputData), colnames(OutputDataI)) # Get the Missing Columns
        for (MissingVariableI in 1: length(MissingVars)){
          OutputDataI[[paste0(MissingVars[MissingVariableI])]] <- rep(NA, dim(OutputDataI)[1])
        } # Add Missing Variables to OutputDataI
      } else if (dim(OutputData)[2] < dim(OutputDataI)[2]) { # Compare the Nb Of Columns if I file has more columns
        MissingVars<- setdiff(colnames(OutputDataI), colnames(OutputData)) # Get the Missing Columns
        for (MissingVariableI in 1: length(MissingVars)){
          OutputData[[paste0(MissingVars[MissingVariableI])]] <- rep(NA, dim(OutputData)[1])
        } # Add Missing Variable to OutputData Merged
      } # End of if dimensions is are different
      OutputData     <- rbind(OutputData, OutputDataI)
    } # End Add data to merged File
  } # end for File I  
  
  OutputDataName <- paste0("Merged_Data_Transformed.csv")
  OutputDataPath <- file.path(OutputDirPath, OutputDataName)
  write.table(OutputData, file = OutputDataPath, row.names =FALSE, sep = ",") # Save the Merged table
  tk_messageBox(type = "ok", message=paste0("CSV Files in the folder ", InputDirName," have been transformed and merged. Files are located in ", OutputDirPath), caption = "KinemaR Information", icon="info")
  close(KinemaRProgressBar)
} # end function Transform
 

# Detect Phases ---------------------------------------------------------
DetectPhases.Function<-function(){
  
  CSVFilter<-matrix(c("CSV File",".CSV","CSV File",".csv"),2,2,byrow=TRUE) # Create a filter to select only CSV files
    MessageChooseInputCSVFile="Choose the CSV file containing Merged AND Transformed Data"
  InputFilePath <- tk_choose.files(default="Merged_Data_Transformed.CSV", caption=MessageChooseInputCSVFile, multi=FALSE, filters=CSVFilter)
  InputFilename<-basename(InputFilePath)
  while(grepl(".*_Transformed.CSV", InputFilePath, ignore.case = TRUE)!=TRUE){
    MessageInputFileError<-paste0("Sorry, ",InputFilename," is not a transformed data CSV file.")
    MessageInputFileErrorFix<-paste0("Please select the Merged File created by KinemaR function 1")
    MessageRetryCancel=paste0("\nClick Retry to Try Again\n or \nCancel to Abort.")
    UserChoice<-tk_messageBox(type = "retrycancel", message=paste0(MessageInputFileError,"\n",MessageInputFileErrorFix,"\n", MessageRetryCancel), caption = "KinemaR Information", icon="warning")
    if(UserChoice=="cancel"){
      MessageStop<-"Sorry, KinemaR has stopped.\n"
      stop(paste0(MessageStop,"\n", MessageInputFileError, "\n", MessageInputFileErrorFix))
    }else if (UserChoice=="retry") {
      rm(list=c("InputFilePath","InputFilename"))
      InputFilePath <- tk_choose.files(default="Merged_Data_Transformed.CSV", caption=MessageChooseInputCSVFile, multi=FALSE, filters=CSVFilter)
      InputFilename<-basename(InputFilePath)
    }
  } # Make sure the select Input File is a Transformed.csv
  
  InputDataHeader <- read.table(InputFilePath, sep = ",",fill = TRUE, header = TRUE, nrows = 3,comment.char = "") # Read the first row of the InputFile To check the header
  
  while((any(colnames(InputDataHeader)=="Filename") # Ensure File has the correct header
         && any(colnames(InputDataHeader)=="Toe.X")
         && any(colnames(InputDataHeader)=="Toe.Y")
         && any(colnames(InputDataHeader)=="Frame"))!=TRUE){
    MessageInputHeaderError<-paste0("Sorry, ",InputFilename," does not contain the expected columns.")
    MessageInputHeaderErrorFix<-paste0("File header must contain at least Filename, Frame, Toe.X and Toe.Y")
    MessageRetryCancel=paste0("\nClick Retry to Try Again\n or \nCancel to Abort.")
    UserChoice<-tk_messageBox(type = "retrycancel", message=paste0(MessageInputHeaderError,"\n",MessageInputHeaderErrorFix,"\n", MessageRetryCancel), caption = "KinemaR Information", icon="warning")
    if (UserChoice=="retry") {
      rm(list=c("InputFilePath","InputFilename", "InputDataHeader"))
      DetectPhases.Function()
    } else if(UserChoice=="cancel"){
      MessageStop<-"Sorry, KinemaR has stopped.\n"
      stop(paste0(MessageStop,"\n", MessageInputHeaderError, "\n", MessageInputHeaderErrorFix))
    } 
  } # Ensure File has the correct header
  
  KinemaRProgressBar <- tkProgressBar(title="KinemaR Phase Detection", label="KinemaR Phase Detection", min=0, max=100, initial=0, width=300)
  setTkProgressBar(KinemaRProgressBar, 50, title="KinemaR Phase Detection", label=paste0("Reading Input File. Please be patient."))
  InputData <- read.table(InputFilePath, sep = ",",fill = TRUE, header = TRUE,comment.char = "", nrows=1000000)
  setTkProgressBar(KinemaRProgressBar, 100, title="KinemaR Phase Detection", label=paste0("Reading Input File completed."))
  Sys.sleep(1)
  InputDirPath <- dirname(InputFilePath)
  InputDirName<-basename(InputDirPath)
  ParentInputDirPath <- dirname(InputDirPath)
  
  OutputDirNameIni <- paste0(InputDirName,"_Phase_Detected")   # Create a new outputdir path
  OutputDirPathIni <- file.path(ParentInputDirPath, OutputDirNameIni)   # Create the Path for the Output dolfer
  OutputDirPath    <- OutputDirPathIni
  n=1
  while(file.exists(OutputDirPath)==TRUE){
    n=n+1;
    OutputDirPath  <- paste0(OutputDirPathIni,"_",n)
  } # Create a unique outputfolder
  dir.create(OutputDirPath, showWarnings = FALSE)   # Create the output folder
  OutputDirPathTables<-file.path(OutputDirPath,"Tables")   # Create a subdirectory for graphs to be saved in
  dir.create(OutputDirPathTables)   # Create a subdirectory for graphs to be saved in
  OutputDirPathGraph<-file.path(OutputDirPath,"Graphs")   # Create a subdirectory for graphs to be saved in
  dir.create(OutputDirPathGraph)   # Create a subdirectory for graphs to be saved in
  
  for (FileI in 1:nlevels(InputData$Filename)) {  # Loop around all levels of Filenames  
    
    ################### Progress bar
    assign(paste0("TimeFile",FileI),Sys.time())  
    if (FileI==1){
      TimeElapsed<-2
      EstimatedRemainingTime<-9
    }else if (FileI>1){
      TimeElapsed<-ceiling(as.numeric(difftime(get(paste0("TimeFile",FileI)),TimeFile1, units="secs")))
      ProcessingTimePerFile<-ceiling(as.numeric(difftime(get(paste0("TimeFile",FileI)),get(paste0("TimeFile",FileI-1)), units="secs")))
      NbFileLeft <- nlevels(InputData$Filename) - FileI
      EstimatedRemainingTime<-ceiling(NbFileLeft*ProcessingTimePerFile)
    }
    ValueProgress<-100*FileI/nlevels(InputData$Filename)
    ProgressMessage <- sprintf("%d%%", round(ValueProgress))
    setTkProgressBar(KinemaRProgressBar, ValueProgress, title="KinemaR Phase Detection", label=paste0(ProgressMessage, " done. Elapsed ", TimeElapsed,"s. Remaining ", EstimatedRemainingTime, "s."))
    ################### End of update the progress bar
    
    FilenameI<-as.character(levels(InputData$Filename)[FileI])
    DataI<-InputData[InputData$Filename==levels(InputData$Filename)[FileI],]     # Crop around each file
    Deriv.Function<-function(Object){c(diff(Object),0)} # Function to calculate the derivative
    # Note on the Phase detection: We are looking for the swing onsets and the stance onsets.
    # We first apply a rollmean to smoothen the data
    # Then we look at the derivative (slope)
    # Then we take the sign of the slope
    # Finally we identify when the sign is changing by taking again the derivative
    # This identify the Swings onsets when values=+2 and the Stance onsets when values =-2
    # The Toe Y would be the most accurate variable to use for the detection of swing and stance onsets
    # Yet in our hands it was less robust than the use of the ToeX
    # Because We are using a roll mean function as well as the ToeX we need to correct slighlty the onsets by few frames
    SwingDetectionDataI <- Deriv.Function(sign(Deriv.Function(rollmean(DataI$Toe.X, 9, fill=NA)))) # Derivative of the sign of the derivative with a rollmean of 9 frames
    SwingOnsetsI<-DataI$Frame[SwingDetectionDataI==2]-2  #Swing onset is 2 frame before the detected onset
    StanceDetectionDataI <- Deriv.Function(sign(Deriv.Function(rollmean(DataI$Toe.X, 9, fill=NA))))
    StanceOnsetsI<-DataI$Frame[StanceDetectionDataI== -2]+3 # Stance onset is 3 frames later
    SwingOnsetsI<-SwingOnsetsI[complete.cases(SwingOnsetsI)] # Get only the complete cases
    StanceOnsetsI<-StanceOnsetsI[complete.cases(StanceOnsetsI)]
    DataI$PhaseOnset<-NA # Create a new column with the Phase onsets
    
    for (SwingI in 1:(length(SwingOnsetsI))){
      FrameSwing<-SwingOnsetsI[SwingI]
      DataI$PhaseOnset[DataI$Frame==FrameSwing]<-"Swing"
    } # Add the Swing onsets
    for (StanceI in 1:(length(StanceOnsetsI))){
      FrameStance<-StanceOnsetsI[StanceI]
      DataI$PhaseOnset[DataI$Frame==FrameStance]<-"Stance"
    } # Add the Stance onsets
    
    FirstFrameOnset<- min(SwingOnsetsI, StanceOnsetsI)    # Get the first frame with an Onset
    DataI<-DataI[DataI$Frame >= FirstFrameOnset,]         # Remove the data prior the first phase onset
    LastFrameOnset<- max(SwingOnsetsI, StanceOnsetsI)     # Get the frame of the last Onset
    DataI<-DataI[DataI$Frame <= LastFrameOnset-1,]        # Remove the data after the last onset
    FramePad<-sprintf("%05d", DataI$Frame)                # Format the Frame numbers with leading 0 to a pad of 5
    DataI$Phase <- as.factor(na.locf(DataI$PhaseOnset, na.rm=FALSE))     # Create streches of the PhaseOnset to identify the phase
    DataI$PhaseOnsetID <- ifelse((!is.na(DataI$PhaseOnset)),paste0(FramePad,".",DataI$Phase),NA)     # Create an ID made of Phase.Frame at the Onset
    DataI$PhaseID <- (na.locf(DataI$PhaseOnsetID, na.rm=FALSE))        # Create streches of the PhaseOnsetID to identify the phase
    
    DataI$PhaseDuration<-NA     # Calculate Phase Duration
    for (PhaseI in 1:length(levels(as.factor(DataI$PhaseID)))){
      DataI$PhaseDuration[DataI$PhaseOnsetID==(levels(as.factor(DataI$PhaseID))[PhaseI])] <- as.integer(length(grep(levels(as.factor(DataI$PhaseID))[PhaseI], DataI$PhaseID)))
    }  # Calculate Phase Duration
    
    # Plot a Graph to control the quality of the phase detection
    # Use RGB command to get the right color
    BBlue<-rgb(red=0, green=147, blue=255, max=255)
    PPink<-rgb(red=249, green=84, blue=231, max=255)
    cairo_pdf(file.path(OutputDirPathGraph,paste0(FilenameI, "_Toe Swing and Stance.pdf"))) # Open the graph as pdf
    plot(DataI$Frame, DataI$Toe.X, ylim=c(0,max(DataI$Toe.X)), type="l", col=BBlue, main=FilenameI, ylab="cm", xlab="Frame", lwd=2)
    lines(DataI$Frame, DataI$Toe.Y, col=PPink, lwd=2) # Add ToeY
    abline(v=DataI$Frame[DataI$PhaseOnset=="Swing"], col="blue") # Add Swingonsets
    abline(v=DataI$Frame[DataI$PhaseOnset=="Stance"], col="red") #Add Stance Onsets
    legend("topright", c("Swing", "Stance","ToeX", "ToeY"), text.col=c("blue", "red", BBlue, PPink), inset = .05, bty="n") # Add legend
    dev.off() # Close and save the graph
    
    OutputFilePathI<-file.path(OutputDirPathTables, paste0(FilenameI,".csv")) # Create the OutputFilePath for the file being processed
    write.table(DataI, file = OutputFilePathI, row.names =FALSE, sep = ",")    # Save the data file
    
    if(FileI==1){
      OutputData<-DataI     # Create a Merged Output
    } else if (FileI > 1) {
      OutputData<-rbind(OutputData, DataI) 
    }# Bind the data to the Merged Output
  } # Process each Filename within the Input File
  OutputFilePath<-file.path(OutputDirPath, paste0("Merged_Data_Phase_Detected.CSV"))  # Write the Merged Table table
  write.table(OutputData, file = OutputFilePath, row.names =FALSE, sep = ",")  # Write the Merged Table table
  tk_messageBox(type = "ok", message=paste0("Phases have been detected succesfully for ",InputFilename,". CSV File are located in ", OutputDirPath), caption = "KinemaR Information", icon="info")
  close(KinemaRProgressBar)
  
} # End of Detect Phases function

# Normalize within file ------------------------------------------------
# This function normalize every detected phase to the average phase duration using cubic spline interpolation
NormalizeWithinFile.Function <- function(){
  CSVFilter<-matrix(c("CSV File",".CSV","CSV File",".csv"),2,2,byrow=TRUE) # Create a filter to select only CSV files
  MessageChooseInputCSVFile="Choose the CSV file containing Merged Phase_detected Data"
  InputFilePath <- tk_choose.files(default="Merged_Data_Phase_Detected.CSV", caption=MessageChooseInputCSVFile, multi=FALSE, filters=CSVFilter)
  InputFilename<-basename(InputFilePath)
  while(grepl(".*_Phase_Detected.CSV", InputFilePath, ignore.case=TRUE)!=TRUE){
    MessageInputFileError<-paste0("Sorry, ",InputFilename," is not a Phase_detected data CSV file.")
    MessageInputFileErrorFix<-paste0("Please select the Merged File created by KinemaR function 2")
    MessageRetryCancel=paste0("\nClick Retry to Try Again\n or \nCancel to Abort.")
    UserChoice<-tk_messageBox(type = "retrycancel", message=paste0(MessageInputFileError,"\n",MessageInputFileErrorFix,"\n", MessageRetryCancel), caption = "KinemaR Information", icon="warning")
    if(UserChoice=="cancel"){
      MessageStop<-"Sorry, KinemaR has stopped.\n"
      stop(paste0(MessageStop,"\n", MessageInputFileError, "\n", MessageInputFileErrorFix))
    }else if (UserChoice=="retry") {
      rm(list=c("InputFilePath","InputFilename"))
      InputFilePath <- tk_choose.files(default="Merged_Data_Phase_Detected.CSV", caption=MessageChooseInputCSVFile, multi=FALSE, filters=CSVFilter)
      InputFilename<-basename(InputFilePath)
    }
  } # Make sure the select Input File is a Phase Detected.csv
  
  InputDataHeader <- read.table(InputFilePath, sep = ",",fill = TRUE, header = TRUE, nrows = 3,comment.char = "") # Read the first row of the InputFile To check the header
  
  while((any(colnames(InputDataHeader)=="Filename") # Ensure File has the correct header
         && any(colnames(InputDataHeader)=="Frame")
         && any(colnames(InputDataHeader)=="PhaseDuration")
         && any(colnames(InputDataHeader)=="PhaseOnset")
         && any(colnames(InputDataHeader)=="Phase")
         && any(colnames(InputDataHeader)=="PhaseOnsetID")
         && any(colnames(InputDataHeader)=="PhaseID"))!=TRUE){
    MessageInputHeaderError<-paste0("Sorry, ",InputFilename," does not contain the expected columns.")
    MessageInputHeaderErrorFix<-paste0("File header must contain at least Filename, Frame, PhaseOnset, Phase, PhaseOnsetID, PhaseID, and PhaseDuration")
    MessageRetryCancel=paste0("\nClick Retry to Try Again\n or \nCancel to Abort.")
    UserChoice<-tk_messageBox(type = "retrycancel", message=paste0(MessageInputHeaderError,"\n",MessageInputHeaderErrorFix,"\n", MessageRetryCancel), caption = "KinemaR Information", icon="warning")
    if (UserChoice=="retry") {
      rm(list=c("InputFilePath","InputFilename", "InputDataHeader"))
      NormalizeWithinFile.Function()
    } else if(UserChoice=="cancel"){
      MessageStop<-"Sorry, KinemaR has stopped.\n"
      stop(paste0(MessageStop,"\n", MessageInputHeaderError, "\n", MessageInputHeaderErrorFix))
    } 
  } # Ensure File has the correct header
  
  KinemaRProgressBar <- tkProgressBar(title="KinemaR Normalization", label="KinemaR Normalization", min=0, max=100, initial=0, width=300)
  setTkProgressBar(KinemaRProgressBar, 50, title="KinemaR Normalization", label=paste0("Reading Input File. Please be patient."))
  InputData <- read.table(InputFilePath, sep = ",",fill = TRUE, header = TRUE,comment.char = "", nrows=1000000)
  setTkProgressBar(KinemaRProgressBar, 100, title="KinemaR Normalization", label=paste0("Reading Input File completed."))
  Sys.sleep(1)
  InputDirPath <- dirname(InputFilePath)
  InputDirName<-basename(InputDirPath)
  ParentInputDirPath <- dirname(InputDirPath)
  
  OutputDirNameIni <- paste0(InputDirName,"_Normalized Within")   # Create a new outputdir path
  OutputDirPathIni <- file.path(ParentInputDirPath, OutputDirNameIni)   # Create the Path for the Output dolfer
  OutputDirPath    <- OutputDirPathIni
  n=1
  while(file.exists(OutputDirPath)==TRUE){
    n=n+1;
    OutputDirPath  <- paste0(OutputDirPathIni,"_",n)
  } # Create a unique outputfolder
  dir.create(OutputDirPath, showWarnings = FALSE)   # Create the output folder
  OutputDirPathTables<-file.path(OutputDirPath,"Tables")
  dir.create(OutputDirPathTables, showWarnings = FALSE)   # Create the output folder
  
  for (FileI in 1:nlevels(InputData$Filename)) {
    ################### Progress bar
    assign(paste0("TimeFile",FileI),Sys.time())  
    if (FileI==1){
      TimeElapsed<-2
      EstimatedRemainingTime<-9
    }else if (FileI>1){
      TimeElapsed<-ceiling(as.numeric(difftime(get(paste0("TimeFile",FileI)),TimeFile1, units="secs")))
      ProcessingTimePerFile<-ceiling(as.numeric(difftime(get(paste0("TimeFile",FileI)),get(paste0("TimeFile",FileI-1)), units="secs")))
      NbFileLeft <- nlevels(InputData$Filename) - FileI
      EstimatedRemainingTime<-ceiling(NbFileLeft*ProcessingTimePerFile)
    }
    ValueProgress<-100*FileI/nlevels(InputData$Filename)
    ProgressMessage <- sprintf("%d%%", round(ValueProgress))
    setTkProgressBar(KinemaRProgressBar, ValueProgress, title="KinemaR Normalization", label=paste0(ProgressMessage, " done. Elapsed ", TimeElapsed,"s. Remaining ", EstimatedRemainingTime, "s."))
    ################### End of update the progress bar
    
    FilenameI<-levels(InputData$Filename)[FileI]          # Defines the Filename being processed
    DataI<-InputData[InputData$Filename==FilenameI,]      # Crop around the Data
    for (VariableN in 1:dim(DataI)[2]){     # Refresh factors to remove unused levels
      if(class(DataI[,VariableN])=="factor" || class(DataI[,VariableN])=="character"){
        DataI[,VariableN]<- factor(DataI[,VariableN])
      } # End of If class is factor or character
    }  # Remove unused factors
    
    AvgSwingDuration<-ceiling(mean(DataI$PhaseDuration[DataI$PhaseOnset=="Swing"], na.rm=TRUE)) # Get the average duration of swing and Stance
    AvgStanceDuration<-ceiling(mean(DataI$PhaseDuration[DataI$PhaseOnset=="Stance"], na.rm=TRUE)) # Get the average duration of swing and Stance
    
    # Create a loop for going for all identified PhaseID within the FileI
    for (PhaseIDI in 1:nlevels(DataI$PhaseID)) {
      NamePhaseIDI <- levels(DataI$PhaseID)[PhaseIDI]         # Defines the Phase being processed
      DataIPhaseI <- DataI[DataI$PhaseID==NamePhaseIDI,]       # Crop around the PhaseUIDI
      PhaseI<-as.character(DataIPhaseI$Phase[1])               # Get the Phase
      if(PhaseI=="Swing"){
        PhaseDurationI<-AvgSwingDuration
      } else if(PhaseI=="Stance"){
        PhaseDurationI<-AvgStanceDuration
      } # Defines the Phase Duration
      OutputDataIPhaseI<-DataIPhaseI[1:PhaseDurationI,]       # Create Output Table of the dimension of the avg phase duration  
      for (VariableN in 1:dim(DataIPhaseI)[2]){
        if (class(DataIPhaseI[,VariableN])=="numeric"){
          OutputDataIPhaseI[,VariableN]<- spline(DataIPhaseI[,VariableN],n=PhaseDurationI)$y # If the variable is numeric use cubic spline to interplate the values
        } else{   OutputDataIPhaseI[,VariableN]<- rep(DataIPhaseI[1,VariableN],PhaseDurationI) # Else just add the values
        }  # Loop around all variables
      } # end of for VariableN
      if(PhaseI=="Swing"){
        OutputDataIPhaseI$NormalizedFrame<-c(-PhaseDurationI:-1)
      } else if (PhaseI=="Stance"){
        OutputDataIPhaseI$NormalizedFrame<-c(1:PhaseDurationI)
      }      # Create a Normalized Frame number Negative for Swing and Positive for Stance
      if (PhaseIDI==1){
        OutputDataI <- OutputDataIPhaseI
      } else if(PhaseIDI>1){
        OutputDataI<-rbind(OutputDataI, OutputDataIPhaseI) # If PhaseUIDI is higher than 1 bind the rows to the MergedNormalize data
      }       # OutputDataI is the Data for each processed File
    } # End of for PhaseID loop
    OutputFilenameI <- paste0(FilenameI,".csv")   # Save the FileI table
    OutputFilePathI <- file.path(OutputDirPathTables, OutputFilenameI)   # Save the FileI table
    write.table(OutputDataI, file = OutputFilePathI, row.names =FALSE, sep = ",")   # Save the FileI table
    if (FileI==1){
      OutputData <- OutputDataI      # Create the merge Data Table
    } else if(FileI>1){
      OutputData<-rbind(OutputData, OutputDataI) # Merge each file
    } # Create Merged File
  } # Process each file within the InputData$Filename
  
  OutputFilename <- paste0("Merged_Data_Normalized_Within.CSV")  # Create the Output file Path
  OutputFilePath <- file.path(OutputDirPath, OutputFilename)  # Create the Output file Path
  write.table(OutputData, file = OutputFilePath, row.names =FALSE, sep = ",") # Write the Merged File
  tk_messageBox(type = "ok", message=paste0(InputFilename," has been normalized successfully. Normalized CSV Files are located in ", OutputFilePath), caption = "KinemaR Information", icon="info")
  close(KinemaRProgressBar)
} # End of Normalize withing file function

# Normalize Across file ------------------------------------------------
# This function normalize every detected phase to the 100 data points using cubic spline interpolation
NormalizeAcrossFile.Function <- function(){
  CSVFilter<-matrix(c("CSV File",".CSV","CSV File",".csv"),2,2,byrow=TRUE) # Create a filter to select only CSV files
  MessageChooseInputCSVFile="Choose the CSV file containing Merged Phase_detected Data"
  InputFilePath <- tk_choose.files(default="Merged_Data_Phase_Detected.CSV", caption=MessageChooseInputCSVFile, multi=FALSE, filters=CSVFilter)
  InputFilename<-basename(InputFilePath)
  while(grepl(".*_Phase_Detected.CSV", InputFilePath, ignore.case=TRUE)!=TRUE){
    MessageInputFileError<-paste0("Sorry, ",InputFilename," is not a Phase_detected data CSV file.")
    MessageInputFileErrorFix<-paste0("Please select the Merged File created by KinemaR function 2")
    MessageRetryCancel=paste0("\nClick Retry to Try Again\n or \nCancel to Abort.")
    UserChoice<-tk_messageBox(type = "retrycancel", message=paste0(MessageInputFileError,"\n",MessageInputFileErrorFix,"\n", MessageRetryCancel), caption = "KinemaR Information", icon="warning")
    if(UserChoice=="cancel"){
      MessageStop<-"Sorry, KinemaR has stopped.\n"
      stop(paste0(MessageStop,"\n", MessageInputFileError, "\n", MessageInputFileErrorFix))
    }else if (UserChoice=="retry") {
      rm(list=c("InputFilePath","InputFilename"))
      InputFilePath <- tk_choose.files(default="Merged & Detected DATA.CSV", caption=MessageChooseInputCSVFile, multi=FALSE, filters=CSVFilter)
      InputFilename<-basename(InputFilePath)
    }
  } # Make sure the select Input File is a Phase Detected.csv
  
  InputDataHeader <- read.table(InputFilePath, sep = ",",fill = TRUE, header = TRUE, nrows = 3,comment.char = "") # Read the first row of the InputFile To check the header
  
  while((any(colnames(InputDataHeader)=="Filename") # Ensure File has the correct header
         && any(colnames(InputDataHeader)=="Frame")
         && any(colnames(InputDataHeader)=="PhaseDuration")
         && any(colnames(InputDataHeader)=="PhaseOnset")
         && any(colnames(InputDataHeader)=="Phase")
         && any(colnames(InputDataHeader)=="PhaseOnsetID")
         && any(colnames(InputDataHeader)=="PhaseID"))!=TRUE){
    MessageInputHeaderError<-paste0("Sorry, ",InputFilename," does not contain the expected columns.")
    MessageInputHeaderErrorFix<-paste0("File header must contain at least Filename, Frame, PhaseOnset, Phase, PhaseOnsetID, PhaseID, and PhaseDuration")
    MessageRetryCancel=paste0("\nClick Retry to Try Again\n or \nCancel to Abort.")
    UserChoice<-tk_messageBox(type = "retrycancel", message=paste0(MessageInputHeaderError,"\n",MessageInputHeaderErrorFix,"\n", MessageRetryCancel), caption = "KinemaR Information", icon="warning")
    if (UserChoice=="retry") {
      rm(list=c("InputFilePath","InputFilename", "InputDataHeader"))
      NormalizeAcrossFile.Function()
    } else if(UserChoice=="cancel"){
      MessageStop<-"Sorry, KinemaR has stopped.\n"
      stop(paste0(MessageStop,"\n", MessageInputHeaderError, "\n", MessageInputHeaderErrorFix))
    } 
  } # Ensure File has the correct header
  
  KinemaRProgressBar <- tkProgressBar(title="KinemaR Normalization", label="KinemaR Normalization", min=0, max=100, initial=0, width=300)
  setTkProgressBar(KinemaRProgressBar, 50, title="KinemaR Normalization", label=paste0("Reading Input File. Please be patient."))
  InputData <- read.table(InputFilePath, sep = ",",fill = TRUE, header = TRUE,comment.char = "", nrows=1000000)
  setTkProgressBar(KinemaRProgressBar, 100, title="KinemaR Normalization", label=paste0("Reading Input File completed."))
  Sys.sleep(1)
  InputDirPath <- dirname(InputFilePath)
  InputDirName<-basename(InputDirPath)
  ParentInputDirPath <- dirname(InputDirPath)
  
  OutputDirNameIni <- paste0(InputDirName,"_Normalized Across")   # Create a new outputdir path
  OutputDirPathIni <- file.path(ParentInputDirPath, OutputDirNameIni)   # Create the Path for the Output dolfer
  OutputDirPath    <- OutputDirPathIni
  n=1
  while(file.exists(OutputDirPath)==TRUE){
    n=n+1;
    OutputDirPath  <- paste0(OutputDirPathIni,"_",n)
  } # Create a unique outputfolder
  dir.create(OutputDirPath, showWarnings = FALSE)   # Create the output folder
  OutputDirPathTables<-file.path(OutputDirPath,"Tables")
  dir.create(OutputDirPathTables, showWarnings = FALSE)   # Create the output folder
  
  for (FileI in 1:nlevels(InputData$Filename)) {
    ################### Progress bar
    assign(paste0("TimeFile",FileI),Sys.time())  
    if (FileI==1){
      TimeElapsed<-2
      EstimatedRemainingTime<-9
    }else if (FileI>1){
      TimeElapsed<-ceiling(as.numeric(difftime(get(paste0("TimeFile",FileI)),TimeFile1, units="secs")))
      ProcessingTimePerFile<-ceiling(as.numeric(difftime(get(paste0("TimeFile",FileI)),get(paste0("TimeFile",FileI-1)), units="secs")))
      NbFileLeft <- nlevels(InputData$Filename) - FileI
      EstimatedRemainingTime<-ceiling(NbFileLeft*ProcessingTimePerFile)
    }
    ValueProgress<-100*FileI/nlevels(InputData$Filename)
    ProgressMessage <- sprintf("%d%%", round(ValueProgress))
    setTkProgressBar(KinemaRProgressBar, ValueProgress, title="KinemaR Normalization", label=paste0(ProgressMessage, " done. Elapsed ", TimeElapsed,"s. Remaining ", EstimatedRemainingTime, "s."))
    ################### End of update the progress bar
    
    FilenameI<-levels(InputData$Filename)[FileI]          # Defines the Filename being processed
    DataI<-InputData[InputData$Filename==FilenameI,]      # Crop around the Data
    for (VariableN in 1:dim(DataI)[2]){     # Refresh factors to remove unused levels
      if(class(DataI[,VariableN])=="factor" || class(DataI[,VariableN])=="character"){
        DataI[,VariableN]<- factor(DataI[,VariableN])
      } # End of If class is factor or character
    }  # Remove unused factors
    
    PhaseDurationI<-100
    # Create a loop for going for all identified PhaseID Across the FileI
    for (PhaseIDI in 1:nlevels(DataI$PhaseID)) {
      NamePhaseIDI <- levels(DataI$PhaseID)[PhaseIDI]         # Defines the Phase being processed
      DataIPhaseI <- DataI[DataI$PhaseID==NamePhaseIDI,]      # Crop around the PhaseUIDI
      PhaseI<-as.character(DataIPhaseI$Phase[1])              # Get the Phase
      OutputDataIPhaseI<-DataIPhaseI[1:PhaseDurationI,]       # Create an output table of the same dimension
      for (VariableN in 1:dim(DataIPhaseI)[2]){
        if (class(DataIPhaseI[,VariableN])=="numeric"){
          OutputDataIPhaseI[,VariableN]<- spline(DataIPhaseI[,VariableN],n=PhaseDurationI)$y # If the variable is numeric use cubic spline to interplate the values
        } else{   OutputDataIPhaseI[,VariableN]<- rep(DataIPhaseI[1,VariableN],PhaseDurationI) # Else just add the values
        }
        }           # Loop around all variables
        
      if(DataIPhaseI$Phase[1]=="Swing"){
          OutputDataIPhaseI$NormalizedFrame<-c(-PhaseDurationI:-1)
        } else if (DataIPhaseI$Phase[1]=="Stance"){
          OutputDataIPhaseI$NormalizedFrame<-c(1:PhaseDurationI)
        }      # Create a Normalized Frame number from -100 to -1 for Swing and 1 to 100 for stance
        
      if (PhaseIDI==1){
        OutputDataI <- OutputDataIPhaseI
      } else if(PhaseIDI>1){
        OutputDataI<-rbind(OutputDataI, OutputDataIPhaseI) # If PhaseUIDI is higher than 1 bind the rows to the MergedNormalize data
      }       # OutputDataI is the Data for each processed File
        } # End of for PhaseID loop
    OutputFilenameI <- paste0(FilenameI,".csv")   # Save the FileI table
    OutputFilePathI <- file.path(OutputDirPathTables, OutputFilenameI)   # Save the FileI table
    write.table(OutputDataI, file = OutputFilePathI, row.names =FALSE, sep = ",")   # Save the FileI table
    if (FileI==1){
      OutputData <- OutputDataI      # Create the merge Data Table
    } else if(FileI>1){
      OutputData<-rbind(OutputData, OutputDataI) # Merge each file
    } # Create Merged File
  } # Process each file Across the InputData$Filename
  
  OutputFilename <- paste0("Merged_Data_Normalized_Across.CSV")  # Create the Output file Path
  OutputFilePath <- file.path(OutputDirPath, OutputFilename)  # Create the Output file Path
  write.table(OutputData, file = OutputFilePath, row.names =FALSE, sep = ",") # Write the Merged File
  tk_messageBox(type = "ok", message=paste0(InputFilename," has been normalized successfully. Normalized CSV Files are located in ", OutputFilePath), caption = "KinemaR Information", icon="info")
  close(KinemaRProgressBar)
} # End of Normalize across file function

# Average data  ---------------------------------------------------
# This function Summarize the interpolated data around selected variables using the aggregate function
AverageData.Function <- function(){
  CSVFilter<-matrix(c("CSV File",".CSV","CSV File",".csv"),2,2,byrow=TRUE) # Create a filter to select only CSV files
  MessageChooseInputCSVFile="Choose the CSV file containing Merged Normalized Data"
  InputFilePath <- tk_choose.files(default="Merged_Data_Normalized.CSV", caption=MessageChooseInputCSVFile, multi=FALSE, filters=CSVFilter)
  InputFilename<-basename(InputFilePath)
  while(grepl(".*_Normalized.*.CSV", InputFilePath, ignore.case=TRUE)!=TRUE){
    MessageInputFileError<-paste0("Sorry, ",InputFilename," is not a Normalized data CSV file.")
    MessageInputFileErrorFix<-paste0("Please select the Merged File created by KinemaR function 3")
    MessageRetryCancel=paste0("\nClick Retry to Try Again\n or \nCancel to Abort.")
    UserChoice<-tk_messageBox(type = "retrycancel", message=paste0(MessageInputFileError,"\n",MessageInputFileErrorFix,"\n", MessageRetryCancel), caption = "KinemaR Information", icon="warning")
    if(UserChoice=="cancel"){
      MessageStop<-"Sorry, KinemaR has stopped.\n"
      stop(paste0(MessageStop,"\n", MessageInputFileError, "\n", MessageInputFileErrorFix))
    }else if (UserChoice=="retry") {
      rm(list=c("InputFilePath","InputFilename"))
      InputFilePath <- tk_choose.files(default="Merged_Data_Normalized.CSV", caption=MessageChooseInputCSVFile, multi=FALSE, filters=CSVFilter)
      InputFilename<-basename(InputFilePath)
    }
  } # Make sure the select Input File is a Phase Detected.csv
  
  InputDataHeader <- read.table(InputFilePath, sep = ",",fill = TRUE, header = TRUE, nrows = 3,comment.char = "") # Read the first row of the InputFile To check the header
  
  while((any(colnames(InputDataHeader)=="Filename") # Ensure File has the correct header
         && any(colnames(InputDataHeader)=="Frame")
         && any(colnames(InputDataHeader)=="PhaseDuration")
         && any(colnames(InputDataHeader)=="NormalizedFrame"))!=TRUE){
    MessageInputHeaderError<-paste0("Sorry, ",InputFilename," does not contain the expected columns.")
    MessageInputHeaderErrorFix<-paste0("File header must contain at least Filename, Frame, PhaseDuration and NormalizedFrame")
    MessageRetryCancel=paste0("\nClick Retry to Try Again\n or \nCancel to Abort.")
    UserChoice<-tk_messageBox(type = "retrycancel", message=paste0(MessageInputHeaderError,"\n",MessageInputHeaderErrorFix,"\n", MessageRetryCancel), caption = "KinemaR Information", icon="warning")
    if (UserChoice=="retry") {
      rm(list=c("InputFilePath","InputFilename", "InputDataHeader"))
      NormalizeWithinFile.Function()
    } else if(UserChoice=="cancel"){
      MessageStop<-"Sorry, KinemaR has stopped.\n"
      stop(paste0(MessageStop,"\n", MessageInputHeaderError, "\n", MessageInputHeaderErrorFix))
    } 
  } # Ensure File has the correct header
  
  KinemaRProgressBar <- tkProgressBar(title="KinemaR Averaging", label="KinemaR Averaging", min=0, max=100, initial=0, width=300)
  setTkProgressBar(KinemaRProgressBar, 50, title="KinemaR Averaging", label=paste0("Reading Input File. Please be patient."))
  InputData <- read.table(InputFilePath, sep = ",",fill = TRUE, header = TRUE,comment.char = "", nrows=1000000)
  setTkProgressBar(KinemaRProgressBar, 100, title="KinemaR Averaging", label=paste0("Reading Input File completed."))
  Sys.sleep(1)
  InputDirPath <- dirname(InputFilePath)
  InputDirName<-basename(InputDirPath)
  ParentInputDirPath <- dirname(InputDirPath)
  
  OutputDirNameIni <- paste0(InputDirName,"_Averaged")   # Create a new outputdir path
  OutputDirPathIni <- file.path(ParentInputDirPath, OutputDirNameIni)   # Create the Path for the Output dolfer
  OutputDirPath    <- OutputDirPathIni
  n=1
  while(file.exists(OutputDirPath)==TRUE){
    n=n+1;
    OutputDirPath  <- paste0(OutputDirPathIni,"_",n)
  } # Create a unique outputfolder
  dir.create(OutputDirPath, showWarnings = FALSE)   # Create the output folder
  OutputDirPathTables<-file.path(OutputDirPath,"Tables")
  dir.create(OutputDirPathTables, showWarnings = FALSE)   # Create the output folder
  
  
  # Create a loop for going for all identified Filename
  for (FileI in 1:nlevels(InputData$Filename)) {
    
    ################### Progress bar
    assign(paste0("TimeFile",FileI),Sys.time())  
    if (FileI==1){
      TimeElapsed<-2
      EstimatedRemainingTime<-9
    }else if (FileI>1){
      TimeElapsed<-ceiling(as.numeric(difftime(get(paste0("TimeFile",FileI)),TimeFile1, units="secs")))
      ProcessingTimePerFile<-ceiling(as.numeric(difftime(get(paste0("TimeFile",FileI)),get(paste0("TimeFile",FileI-1)), units="secs")))
      NbFileLeft <- nlevels(InputData$Filename) - FileI
      EstimatedRemainingTime<-ceiling(NbFileLeft*ProcessingTimePerFile)
    }
    ValueProgress<-100*FileI/nlevels(InputData$Filename)
    ProgressMessage <- sprintf("%d%%", round(ValueProgress))
    setTkProgressBar(KinemaRProgressBar, ValueProgress, title="KinemaR Averaging", label=paste0(ProgressMessage, " done. Elapsed ", TimeElapsed,"s. Remaining ", EstimatedRemainingTime, "s."))
    ################### End of update the progress bar

    FilenameI<-levels(InputData$Filename)[FileI]    # Defines the Filename being processed
    DataI<-InputData[InputData$Filename==FilenameI,]    # Crop around the Data
    for (VariableN in 1:dim(DataI)[2]){
      if(class(DataI[,VariableN])=="factor" ||class(DataI[,VariableN])=="character"){
        DataI[,VariableN]<- as.factor(as.character(DataI[,VariableN]))
      } # end of if condition
    }     # Relevel the factors
    DataINumeric<-data.frame(Dummy=c(1:dim(DataI)[1]))   # Create a new Data table same dimension of  non numeric variables
    for (VariableN in 1:dim(DataI)[2]){
      if(class(DataI[,VariableN])=="numeric"){
        DataINumeric[,as.character(colnames(DataI[VariableN]))]<- DataI[,VariableN]
      } # end of if condition
    }     # Add numeric variables to the New data frame
    DataINumeric$Dummy<-NULL    # Remove the first column
    MeanDataI<-aggregate(x=DataINumeric, by=list(DataI$Filename, DataI$NormalizedFrame), FUN="mean")    # Aggregate the New data frame by Filename, Phase and normalizedFrame and calculate the mean
    colnames(MeanDataI)<-paste0(rep("Avg.", length(colnames(MeanDataI))),colnames(MeanDataI))  # Rename the columns by Adding Avg.
    MeanDataI[2]<-NULL
    MeanDataI[1]<-NULL
    MinDataI<-aggregate(x=DataINumeric, by=list(DataI$Filename, DataI$NormalizedFrame), FUN="min")    # Aggregate the New data frame by Filename, Phase and normalizedFrame and calculate the minimum
    colnames(MinDataI)<-paste0(rep("Min.", length(colnames(MinDataI))),colnames(MinDataI))  # Rename the columns by Adding Min.
    MinDataI[2]<-NULL
    MinDataI[1]<-NULL
    MaxDataI<-aggregate(x=DataINumeric, by=list(DataI$Filename, DataI$NormalizedFrame), FUN="max")    # Aggregate the New data frame by Filename, Phase and normalizedFrame and calculate the minimum
    colnames(MaxDataI)<-paste0(rep("Max.", length(colnames(MaxDataI))),colnames(MaxDataI))  # Rename the columns by Adding Min.
    MaxDataI[2]<-NULL
    MaxDataI[1]<-NULL
    FullRange.Function <- function(x){diff(range(x))}
    RangeDataI<-aggregate(x=DataINumeric, by=list(DataI$Filename, DataI$NormalizedFrame), FUN="FullRange.Function")    # Aggregate the New data frame by Filename, Phase and normalizedFrame and calculate the minimum
    colnames(RangeDataI)<-paste0(rep("Range.", length(colnames(RangeDataI))),colnames(RangeDataI))  # Rename the columns by Adding Min.
    RangeDataI[2]<-NULL
    RangeDataI[1]<-NULL
    StDevDataI<-aggregate(x=DataINumeric, by=list(DataI$Filename, DataI$NormalizedFrame), FUN="sd")    # Aggregate the New data frame by Filename, Phase and normalizedFrame and calculate the minimum
    colnames(StDevDataI)<-paste0(rep("StDev.", length(colnames(StDevDataI))),colnames(StDevDataI))  # Rename the columns by Adding Min.
    StDevDataI[2]<-NULL
    StDevDataI[1]<-NULL
    VarDataI<-aggregate(x=DataINumeric, by=list(DataI$Filename, DataI$NormalizedFrame), FUN="var")    # Aggregate the New data frame by Filename, Phase and normalizedFrame and calculate the minimum
    colnames(VarDataI)<-paste0(rep("Var.", length(colnames(VarDataI))),colnames(VarDataI))  # Rename the columns by Adding Min.
    VarDataI[2]<-NULL
    VarDataI[1]<-NULL
    MadDataI<-aggregate(x=DataINumeric, by=list(DataI$Filename, DataI$NormalizedFrame), FUN="mad")    # Aggregate the New data frame by Filename, Phase and normalizedFrame and calculate the minimum
    colnames(MadDataI)<-paste0(rep("Mad.", length(colnames(MadDataI))),colnames(MadDataI))  # Rename the columns by Adding Min.
    MadDataI[2]<-NULL
    MadDataI[1]<-NULL
    
    OutputDataI<- as.data.frame(table(list(DataI$Filename, DataI$NormalizedFrame)))    # Get the n occurence for each aggregated value
    names(OutputDataI)[1]<-"Filename"     # Rename the first free columns
    names(OutputDataI)[2]<-"NormalizedFrame"    # Rename the first free columns
    names(OutputDataI)[3]<-"Nb Phase"    # Rename the first free columns
    OutputDataI$NormalizedFrame<-as.integer(as.character(OutputDataI$NormalizedFrame))     # Transform NormalizedFrame to an integer
    OutputDataI$Phase<-ifelse(OutputDataI$NormalizedFrame<0,"Swing", "Stance")    # Recreate the Phase

    OutputDataI<-cbind(OutputDataI, MeanDataI, MinDataI,MaxDataI, RangeDataI,StDevDataI,VarDataI, MadDataI)
 
    
    # Recreates the Filename Variables
    DimDataI<-dim(OutputDataI)[1]
    FilenameINoExt<-gsub(".csv","",FilenameI,ignore.case = TRUE)
    DateI<-unlist(strsplit(FilenameINoExt, "_"))[1]
    SubjectI<-unlist(strsplit(FilenameINoExt, "_"))[2]
    if (length(unlist(strsplit(FilenameINoExt, "_")))>2){
      for (SubNameI in 3:length(unlist(strsplit(FilenameINoExt, "_")))){
        assign(paste0("Var", SubNameI), unlist(strsplit(FilenameINoExt, "_"))[SubNameI])        
      } # end of for SubnameI
    }# Create an object with the String from the filename
    OutputDataI$Date      <- as.character(rep(DateI, DimDataI))
    OutputDataI$Subject   <- as.character(rep(SubjectI, DimDataI))
    if (length(unlist(strsplit(FilenameINoExt, "_")))>2){
      for (SubNameI in 3:length(unlist(strsplit(FilenameINoExt, "_")))){
        OutputDataI[[paste0("Var", SubNameI)]]  <- as.character(rep(get(paste0("Var",SubNameI)), DimDataI))
      }# Add the Variable from the filename to the outputdata table
    }     # If there is more than 2 variables in the filename add it as a variable in the OutputData
    OutputFilenameI <- paste0(FilenameI,".csv") # Save the FileI table
    OutputFilePathI <- file.path(OutputDirPathTables, OutputFilenameI)# Save the FileI table
    write.table(OutputDataI, file = OutputFilePathI, row.names =FALSE, sep = ",") # Save the FileI table
    
    # Create a table for the first file to merge all the data into a single table. It will be saved for the last file
    if (FileI == 1){
      OutputData     <- OutputDataI # Create a new output Table for the Merged
    } else if (FileI>1){
      if (dim(OutputData)[2] > dim(OutputDataI)[2]){ # Compare the Nb Of Columns if Merge file has more columns
        MissingVars<- setdiff(colnames(OutputData), colnames(OutputDataI)) # Get the Missing Columns
        for (MissingVariableI in 1: length(MissingVars)){
          OutputDataI[[paste0(MissingVars[MissingVariableI])]] <- rep(NA, dim(OutputDataI)[1])
        } # Add Missing Variables to OutputDataI
      } else if (dim(OutputData)[2] < dim(OutputDataI)[2]) { # Compare the Nb Of Columns if I file has more columns
        MissingVars<- setdiff(colnames(OutputDataI), colnames(OutputData)) # Get the Missing Columns
        for (MissingVariableI in 1: length(MissingVars)){
          OutputData[[paste0(MissingVars[MissingVariableI])]] <- rep(NA, dim(OutputData)[1])
        } # Add Missing Variable to OutputData Merged
      } # End of if dimensions is are different
      OutputData     <- rbind(OutputData, OutputDataI)
    } # End Add data to merged File
  } # end for File I  
      OutputFilename <- paste0("Merged_Data_Averaged.CSV")   # Create the output file Path
  OutputFilePath <- file.path(OutputDirPath, OutputFilename)
  write.table(OutputData, file = OutputFilePath, row.names =FALSE, sep = ",")# Write the Merged data and inform of a message
  tk_messageBox(type = "ok", message=paste0(InputFilename," has been averaged successfully. Averaged CSV Files are located in ", OutputDirPath), caption = "KinemaR Information", icon="info")
  close(KinemaRProgressBar)
  
} # End of Average function

# Transfrom Dialog ------------------------------------------------------------------
TransformDialog.Function<-function(){
  TransformDialog <- tktoplevel() # Create a dialog
  tktitle(TransformDialog)<-"KinemaR Transformation" #Change the title of Fialog
  TitleFont <- tkfont.create(family="Arial", size=12)#Create Font
  LabelFont <- tkfont.create(family="Arial", size=11)#Create Font
  NameFont<-tkfont.create(family="Arial", size=11, slant="italic") #Create Font
  tkwm.geometry(TransformDialog, "590x180+305+5")# Change the size
  LabelTitle<-ttklabel(TransformDialog, text=" ") # Add a blank line
  SelectedFileTitle<-ttklabel(TransformDialog, text="Selected Files / Folders",  font=TitleFont)
  ButtonTitle<-ttklabel(TransformDialog, text="       ")
  tkgrid(LabelTitle,SelectedFileTitle,ButtonTitle, sticky="n", padx=3, pady=2)
  SelectDataFolderLabel<-ttklabel(TransformDialog, text="Data Folder :",  font=LabelFont)
  assign("SelectedDataNameVar",tclVar("1 Select Data Folder --->"), envir=.GlobalEnv)
  SelectedDataName<-ttklabel(TransformDialog, textvariable=SelectedDataNameVar, font=NameFont, background="#e6e6e6")
  SelectData.Button<-ttkbutton(TransformDialog,text="Select Folder", command=SelectCSVDataFileDir.Function)
  tkgrid(SelectDataFolderLabel, SelectedDataName, SelectData.Button, padx=3, pady=2)
  tkgrid.configure(SelectDataFolderLabel, sticky = "e")
  SelectCalibFileLabel<-ttklabel(TransformDialog, text="Video Calibration File :",  font=LabelFont)
  assign("SelectedCalibFilenameVar",tclVar("2 Select Video Calibration File --->"), envir=.GlobalEnv)
  SelectedCalibFilename<-ttklabel(TransformDialog, textvariable=SelectedCalibFilenameVar, font=NameFont, background="#e6e6e6")
  SelectCalib.Button<-ttkbutton(TransformDialog,text="Select Calibration CSV File", command=SelectVideoCalibCSV.Function)
  tkgrid(SelectCalibFileLabel,SelectedCalibFilename,SelectCalib.Button, padx=3, pady=2)
  tkgrid.configure(SelectCalibFileLabel, sticky = "e")
  SelectBodyFileLabel<-ttklabel(TransformDialog, text="Subject Body File :",  font=LabelFont)
  assign("SelectedBodyFilenameVar",tclVar("3 Select Subject Body File --->"), envir=.GlobalEnv)
  SelectedBodyFilename<-ttklabel(TransformDialog, textvariable=SelectedBodyFilenameVar, font=NameFont, background="#e6e6e6")
  SelectBody.Button<-ttkbutton(TransformDialog,text="Select Subject Body CSV File", command=SelectSubjectBodyCSV.Function)
  tkgrid(SelectBodyFileLabel,SelectedBodyFilename,SelectBody.Button, padx=3, pady=2)
  tkgrid.configure(SelectBodyFileLabel, sticky = "e")
  tkgrid(ttklabel(TransformDialog,text=" "))  # Insert a blank line
  CloseDialogTransform.Button <- ttkbutton(TransformDialog,text="Close", command=function(){
    ResetTransformDialog.Function()
    tkdestroy(TransformDialog)})  # Create a button to close TransformDialog toolbar
  ResetTransformDialog.Button <- ttkbutton(TransformDialog,text="Reset", command=ResetTransformDialog.Function)  # Create a Reset button
  TransformData.Button <- ttkbutton(TransformDialog, text="Transform Data", command=TransformButton.Function)  # Create the transform button
  tkgrid(CloseDialogTransform.Button,ResetTransformDialog.Button,TransformData.Button, padx=2, pady=1)
  tkgrid(ttklabel(TransformDialog,text=" "))  # Insert a blank line
} # End of Transform Dialog Function

# KinemaR Toolbar ---------------------------------------------------------
KinemaRToolbar           <- tktoplevel()# Create KinemaR Toolbar
tktitle(KinemaRToolbar)  <- "KinemaR Toolbar"# Add a Title to the bar
# Add button for each function
Transform.Button <- ttkbutton(KinemaRToolbar, text = "1 Transform Data", command = TransformDialog.Function)
DetectPhases.Button    <- ttkbutton(KinemaRToolbar,text="2 Detect Phases", command=DetectPhases.Function)
NormalizeWithin.Button <- ttkbutton(KinemaRToolbar,text="Within File", command=NormalizeWithinFile.Function)
NormalizeAcross.Button <- ttkbutton(KinemaRToolbar,text="Across Files", command=NormalizeAcrossFile.Function)
AveragePhase.Button  <- ttkbutton(KinemaRToolbar,text="4 Average Data", command=AverageData.Function)
Reset.Button <- ttkbutton(KinemaRToolbar,text="Reset", command=function(){
  env <- parent.frame()
  rm(list = setdiff(ls(all.names=TRUE, env = env), ListObjectAtStart), envir = env)
})# Create a Reset button and its associated function
Close.Button <- ttkbutton(KinemaRToolbar,text="Close KinemaR", command=function(){tkdestroy(KinemaRToolbar)})# Create a button to close KinemaR toolbar

# Add the button to the toolbar
tkgrid(Transform.Button,padx= 2, pady= 2, columnspan=2,sticky = "ew") 
tkgrid(DetectPhases.Button,padx= 2, pady= 2, columnspan=2,sticky = "ew") 
tkgrid(ttklabel(KinemaRToolbar,text="3 Normalize Phase Duration", justify="center"), columnspan=2 ,sticky = "n")
tkgrid(NormalizeWithin.Button, NormalizeAcross.Button, padx= 2, pady= 2,sticky = "ew" ) 
tkgrid(AveragePhase.Button, columnspan=2, padx= 2, pady= 2, sticky = "ew") 
tkgrid(ttklabel(KinemaRToolbar,text=" "),sticky = "ew")
tkgrid(Close.Button, Reset.Button, padx= 2, pady= 2,sticky = "ew" ) 
ListObjectAtStart<-c(ls(all.names=TRUE),"ListObjectAtStart") # List all objects present after the first run to restaure them when Reset is clicked
