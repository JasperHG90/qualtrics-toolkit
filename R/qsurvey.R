#' qsurvey
#'
#' Qualtrics survey object
#'
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.string
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#'
#' @param id_or_name Survey ID or name
#' @param clean_html Auto-strip HTML from question names & choices
#' @param include.raw Include raw JSON in survey object
#'
#' @return Qualtrics survey object
#' @export

qsurvey <- function(id_or_name,
                    clean_html = TRUE,
                    include.raw = FALSE) {

  assert_that(is.string(id_or_name))

  ## Get a survey by its ID or name, depending on filter passed
  if (grepl("^SV_.+", id_or_name)) {
    survey_id <- id_or_name
  } else {
    survey_id <- get_survey_id_by_name(id_or_name,
                                       match.exact = TRUE)
  }
  
  ## Get survey metadata and responses from Qualtrics API
  s_meta <- qapi_get_survey(survey_id)
  s_resp <- qapi_response_export(survey_id)
  all_col_names <- colnames(s_resp)
  ## Initialize and qquestions list
  s_qquestions <- list()
  
  ## Loop thru all returned questions (use seq_along to keep track of order)
  qs <- s_meta$questions
  qids <- names(qs)

  for (i in seq_along(qids)) {
    ## Get question ID and question object
    qid <- qids[[i]]
    q_meta <- qs[[qid]]

    ## Add qid and order to question metadata
    q_meta$questionOrder <- as.integer(i)
    q_meta$questionID <- qid
    
    ## Select the responses (and ResponseID) for only this question,
    ## pass that to create new qquestion object along with metadata
   
    # better approach use a list of columns using regex. NOTE - this will STILL FAIL for the user if there are duplicate col names
    cur_qname <- q_meta$questionName
    # get just the cols that begin with the question number of are the question number
    # note that SBS matrices have a slightly more complex structure. They use . notation after the question rather than underscore
    if (q_meta$questionType$type == "SBS") {
      q_cols <- all_col_names[grepl(paste0("^", cur_qname, ".."), all_col_names) | all_col_names == cur_qname]
    } else {
      q_cols <- all_col_names[grepl(paste0("^", cur_qname, "_."), all_col_names) | all_col_names == cur_qname]
    }
    
    q_resp <- select(s_resp, "ResponseID", q_cols)
    
    # This is where the quesiton object is created
    # if it doesn't generate subquestions properly or choices it's likely due to 
    # the question type not being addressed yet in the package
    q_qquestion <- qquestion(q_meta, q_resp, clean_html)

    ## Add qquestion to list of qquestions
    s_qquestions[[qid]] <- q_qquestion
  }

  ## Generate question list from qquestion objects
  s_question_list <- lapply(s_qquestions,
                            function(q) { return(q$meta) })
  # if the user has factors set to true - here you get many warnings
  s_question_list <- bind_rows(s_question_list)
  s_question_list <- auto_reformat(s_question_list, 
                                   strip.html = clean_html)

  ## Parse survey "flow"
  s_flow <- nested_list_to_df(s_meta$flow)
  # this function is way way way too big and does too many things
  s_flow <- auto_reformat(s_flow, prefix = "b", reorder.rows= FALSE)
  s_flow <- cbind(s_flow, desc = NA)
  # Change block 'id' to 'bid'
  names(s_flow)[names(s_flow) == "id"] <- "bid" 
  
  ## Generate DF of survey blocks
  s_blocks <- data.frame()
  
  bs <- s_meta$blocks
  bids <- names(bs)
  # Added line to remove empty blocks
  for (j in seq_along(bids)) {
    bid <- bids[[j]]
    b_meta <- bs[[bid]]
    
    # this fails if there is an empty block
    # test if the block elements is empty
    if (!(length(b_meta$elements) == 0)) {
      b <- nested_list_to_df(b_meta$elements)
      b <- cbind(bid = bid, b)
    } else {
     b <- data.frame(bid = bid, 
                     type = "Empty Block",
                     #description = b_meta$description,
                     note = "This block is empty in your survey. You may want to remove it from the survey flow.")
    }
    

    ## Add the block description to the survey flow DF row with bid
    # but first, remove block rows where bid == NA
    # note that this is a quirk in qualtrics as i can see the empty blocks in the flow 
    # but they are not in the survey. Digging into why this happens
    s_flow <- s_flow[complete.cases(s_flow$bid), ]
    s_flow[s_flow$bid == bid,]$desc <- b_meta$description

    s_blocks <- bind_rows(s_blocks, b)
  }
  
  s_blocks <- auto_reformat(s_blocks, 
                            reorder.cols = FALSE, 
                            strip.html = clean_html)
    
  ## Generate list of respondents
  s_respondent_cols <- names(s_resp)[c(1:11)]
  s_respondents <- s_resp[,s_respondent_cols]
  
  ## Class definition and return
  survey <- list(
      meta = list(
          id              = s_meta$id,
          name            = s_meta$name,
          owner_id        = s_meta$ownerId,
          organization_id = s_meta$organizationId,
          is_active       = s_meta$isActive,
          created         = s_meta$creationDate,
          last_modified   = s_meta$lastModifiedDate,
          expiration      = list(
              start = s_meta$expiration$startDate,
              end   = s_meta$expiration$endDate
          )
      ),
      questionList = s_question_list,
      questions    = s_qquestions, ## the choices and questions are not being cleaned
      respondents  = s_respondents,
      responses    = s_resp,
      flow         = s_flow,
      blocks       = s_blocks
  )

  ## Check for duplicate questions
  check_duplicate_questions(survey)
  

  ## Add raw JSON if specified
  if (include.raw) {
    survey$raw = s_meta
  }

  class(survey) <- "qsurvey"
  return(survey)
}

print.qsurvey <- function(x, ...) {
  c(sprintf("%s (%s)", s$meta$name, s$meta$id),
    sprintf("  $questions:")) 
}
