library(taipan)
library(shiny)
library(xfun)
library(ggplot2)
library(shinydashboard)

taipanQuestions <- function(scene, selection){
  structure(
    list(scene = scene, selection = selection),
    class = "taipanQuestions"
  )
}

questions <- taipanQuestions(
  scene = div(radioButtons("Hotdog", label = "Hot dog ?",
                             choices = list("Yes", "No"))),
  selection = div(
    radioButtons("hotdog", label = "Hotdog?",
                 choices = list("Hotdog", "Not hotdog")),
    checkboxGroupInput("extra", label = "Condiments",
                       choices = list("Onion", "Tomato (Ketchup)", "Barbeque", "Mustard"))
  )
)


buildTaipan <- function(questions, images, appdir, launch = TRUE, overwrite = FALSE, skip_check = FALSE, ext_restricted = TRUE){
#images <- tools::file_path_as_absolute(images)
  if(!inherits(questions, "taipanQuestions")){
    stop("Questions must be created using the taipanQuestions() function.")
  }
  if(overwrite){
    message(paste0("Are you sure you want to overwrite '", appdir, "'? All files in this folder will be deleted!\nYes: Delete ALL of these files!\nNo: Keep it the way it is!"))
    if(!skip_check){
      auth <- readline()
      if(toupper(auth)!="YES"){
        message("Aborted building of taipan app.")
        return(invisible(NULL))
      }
    }
    unlink(appdir, recursive = TRUE)
  }
  if(dir.exists(appdir)){
    if(length(list.files(appdir))>0){
      stop(sprintf('Output appdir "%s" already exists, please provide a different location to save app',
                   appdir))
    }
  }
  else{
    dir.create(appdir)
  }
  appdir <- tools::file_path_as_absolute(appdir)
  
  # WRITE APPDIR
  app_files <- list.files(file.path(system.file(package="taipan"), "app"))
  file.copy(file.path(system.file(package="taipan"), "app", app_files), appdir, recursive = TRUE)
  
  # SAVE QUESTIONS
  dir.create(file.path(appdir, "data"))
  saveRDS(questions, file = file.path(appdir, "data", "questions.Rds"))
  
  
  # DELETE UNSUPPORTED IMAGES
  if (ext_restricted){
    valid_ext <- file_ext(images) %in% c("png", "jpeg", "jpg", "svg", "gif")
    if(any(!valid_ext)){
      message <- "Images have been removed due to extension type:\n"
      message <- paste0(message, paste(head(images[!valid_ext]), collapse = ", \n"))
      if (length(images) > 6) {
        extra <- length(images)-6
        message <- paste0(message,", \n and ", extra, " other images.")
      }
      message(message)
      images  <- images[valid_ext] #only keep valid image extensions
    }
  }
  
  # CONSTRUCT IMAGE DIR
  dir.create(file.path(appdir, "www", "app_images"))
  if(any(dirs <- dir.exists(images))){
    images <- c(list.files(images[dirs], full.names = TRUE, recursive = TRUE), images[!dirs])
  }
  img_success <- file.copy(images, file.path(appdir, "www", "app_images", basename(images)))
  if(any(!img_success)){
    # check download method to use
    if (.Platform$OS.type == "windows") {
      method <- "wininet"
    }
    else{
      method <- "auto"
    }
    Map(download.file, url = images[!img_success], mode = "wb", method = method, destfile = file.path(appdir, "www", "app_images", basename(images[!img_success])))
  }
  
  cat(paste("The app has been saved in", appdir))
  
  # LAUNCH APP
  if (launch) {
    runApp(appdir)
  }
  
}


buildTaipan(
  questions = questions,
  images = c("C:/Users/deepchand/Desktop/tercen/1.jpg",
             "C:/Users/deepchand/Desktop/tercen/2.jpg"),
  appdir = file.path(tempdir(), "taipan"), overwrite = TRUE
)


