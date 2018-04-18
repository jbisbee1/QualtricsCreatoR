#' create_survey
#' 
#' This function creates Qualtrics surveys ready for import. Users must provide survey questions and choices as a named list where the list names are the questions and the content of each list are the choices. Encoding can be passed by naming the list choices. For matrix questions, users must provide a vector of answers, again passing encoding via naming the vector. For a detailed overview of possible question, choice, and answer types, please see \code{\link{https://www.qualtrics.com/support/survey-platform/survey-module/survey-tools/import-and-export-surveys/}}.
#' @param question.type One of 'MC' (default), 'Matrix', 'TextEntry' (or 'TE'), 'ConstantSum' (or 'CS'), 'RankOrder' (or 'RO'), or 'Text' (or 'DB').
#' @param answer.type One of 'SingleAnswer' (default), 'MultipleAnswer', 'DropDown', 'Select', 'MultiSelect', 'SingleLine', 'Essay', or 'Form'. 
#' @param orientation Either 'Vertical' (default) or 'Horizontal' for multiple choice question orientation.
#' @param survey.content A list of question choices. The names of the list should be the question text. Choice encoding may be passed by naming the choices.
#' @param matrix.answers A vector of answer choices for the matrix question. Answeer encoding can be passed by naming the vector. Defaults to ``None''.
#' @param n.questions.per.block The number of questions on each Qualtrics block (or page). Defaults to 10.
#' @param filename The name of the output file.
#' @keywords Qualtrics automation survey creator
#' @examples 
#' data("samplesurvey")
#' create_survey(question.type = "MC",
#'               answer.type = "SingleAnswer",
#'               orientation = "Vertical",
#'               survey.content = samplesurvey,
#'               matrix.answers = "None",
#'               n.questions.per.block = 10,
#'               filename = "MCSurvey.txt")
#'               
#' create_survey(question.type = "Matrix",
#'               answer.type = "MultipleAnswer",
#'               orientation = "Vertical",
#'               survey.content = samplesurvey,
#'               matrix.answers = matrixAnswers,
#'               n.questions.per.block = 5,
#'               filename = "MatrixSurvey.txt")
#' @export create_survey

create_survey <- function(question.type = "Matrix",answer.type = "SingleAnswer",
                              orientation = "Vertical",
                              survey.content = list.test,
                              matrix.answers = "None",
                              n.questions.per.block = 10,filename = "choiceblocks") {
  # Protecting against user error
  accept.questions <- c("MC","Matrix","TextEntry","TE","ConstantSum","CS","RankOrder","RO","Text","DB")
  accept.answers <- c("SingleAnswer","MultipleAnswer","DropDown","Select","MultiSelect","SingleLine","Essay","Form")
  accept.orientations <- c("Vertical","Horizontal")
  if(!tolower(question.type) %in% tolower(accept.questions)) {
    stop(paste0("Incorrect question type. Please choose from ",paste(accept.questions,collapse = ", ")))
  } else {
    question.type <- accept.questions[which(tolower(accept.questions) == tolower(question.type))]
  }
  
  if(!tolower(answer.type) %in% tolower(accept.answers)) {
    stop(paste0("Incorrect answer type. Please choose from ",paste(accept.answers,collapse = ", ")))
  } else {
    answer.type <- accept.answers[which(tolower(accept.answers) == tolower(answer.type))]
  }
  
  if(!tolower(orientation) %in% tolower(accept.orientations)) {
    stop(paste0("Incorrect orientation. Please choose from ",paste(accept.orientations,collapse = ", ")))
  } else {
    orientation <- accept.orientations[which(tolower(accept.orientations) == tolower(orientation))]
  }
  
  if(!grepl("\\.txt",filename)) {
    stop("Please make sure the file extension on your filename is .txt")
  }
  # Creates the empty file.
  cat(c("[[AdvancedFormat]]","\n","\n","[[Block:0]]","\n"),file = filename,append = F)
  # Loops through each question in the survey
  j <- 1
  for(q in 1:length(survey.content)) {
    # If no encoding is provided by naming the choices, assigns sequential 1:length(choices)
    if(is.null(names(survey.content[[q]]))) {
      names(survey.content[[q]]) <- 1:length(survey.content[[q]])
    }
    # Creates formatted character vector of choices
    choices <- sapply(1:length(survey.content[[q]]), function(c) paste("[[Choice:",names(survey.content[[q]])[c],"]]","\n",paste(as.character(survey.content[[q]][c]),"\n",sep=""),sep = ""))
    # Creates raw character vector of questions
    if(answer.type %in% c("SingleAnswer","MultipleAnswer")) {
      questions <- c(question.type,answer.type,orientation)
    } else {
      questions <- c(question.type,answer.type)
    }
    # Creates a new block if exceeded the number of questions per block
    if(q %% n.questions.per.block == 0) {
      # Combines the block title with the question and choices
      combined <- c(paste0("[[Block:",j,"]]"),"\n",
                    paste0("[[Question:",paste(questions,collapse=":"),"]]"),"\n",
                    names(survey.content)[q],"\n","\n",
                    "[[AdvancedChoices]]","\n",
                    choices,"\n")
      cat(paste0("\nBlock ",j," complete.\n"))
      j <- j+1
    } else {
      # Combines the question with the choices
      combined <- c(paste0("[[Question:",paste(questions,collapse=":"),"]]"),"\n",
                    names(survey.content)[q],"\n","\n",
                    "[[AdvancedChoices]]","\n",
                    choices,"\n")
    }
    # If requested a matrix formatted question
    if(question.type == "Matrix") {
      if(matrix.answers[1] == "None") {
        stop("You must pass a vector of matrix answers to 'matrix.answers' when using the matrix question type.")
      }
      # If no encoding is provided by naming the matrix answers, assigns sequential 1:length(answers)
      if(is.null(names(matrix.answers))) {
        names(matrix.answers) <- 1:length(matrix.answers)
      }
      # Creates formatted character vector of answers
      answers <- sapply(1:length(matrix.answers), function(a) paste("[[Answer:",names(matrix.answers)[a],"]]","\n",paste(as.character(matrix.answers[a]),"\n",sep=""),sep = ""))
      combined <- c(combined,paste0("[[AdvancedAnswers]]","\n",paste(answers,collapse = ""),"\n"))
    }
    # Appends the formatted question to the file
    cat(combined,file = filename,append = TRUE)
    cat(paste0("."))
  }
  cat(paste0("\nSurvey '",filename,"' complete.\n"))
}
