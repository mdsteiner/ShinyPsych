createDfdList <- function(nOpts = NULL, nOutcs = NULL, loadFile = TRUE,
                          fileName = "TwoOptExample", gambleList = NULL,
                          sep = "\t", defaultGambles = FALSE,
                          randomizeHorizontal = TRUE, randomizeVertical = TRUE,
                          practiceTrial = NULL, withPracticeTrial = FALSE,
                          practiceFileName = NULL){
  
  # read in files
  if (isTRUE(loadFile)){
    
    if (isTRUE(defaultGambles)){
      gamble.df <- read.table(paste0("www/", fileName, ".txt"), header = TRUE,
                              sep = "\t", stringsAsFactors = FALSE)
      
      default.list.names <- c("HertwigBarronWeberErev", "Birnbaum",
                              "GloecknerPachur", "BrooksPetersZank", "Rieskamp",
                              "LoomesMoffattSugden", "TwoOptExample",
                              "ThreeOptExample", "FourOptExample",
                              "FiveOptExample", "SixOptExample")
      default.list.opts <- c(rep(2, 7), 3:6)
      default.list.outc <- c(2, 5, 5, 5, 2, 3, rep(2, 5))
      
      nOpts <- default.list.opts[default.list.names == fileName]
      nOutcs <- default.list.outc[default.list.names == fileName]
      
    } else {
      gamble.df <- read.table(fileName, header = TRUE, sep = sep,
                              stringsAsFactors = FALSE)
    }
  } else {
    gamble.df <- gambleList
    
  }
  
  # read in practice trial files
  if (isTRUE(withPracticeTrial)){
    
    if (isTRUE(defaultGambles)){
      
      practice.df <- read.table(paste0("www/", fileName, "_Practice.txt"),
                                header = TRUE, sep = "\t",
                                stringsAsFactors = FALSE)
      
    } else if (!is.null(practiceFileName)){
      
      practice.df <- read.table(fileName, header = TRUE, sep = sep,
                                stringsAsFactors = FALSE)
      
    } else if (!is.null(practiceTrial)){
      
      if (is.data.frame(practiceTrial)){
        
        practice.df <- practiceTrial
        
      } else {
        
        stop(paste("\"practiceTrial\" must be of class \"data.frame\", yours is",
                   class(practiceTrial)))
        
      }
      
    } else {
      stop("withPracticeTrial is TRUE but no input on where to find the trial info in one of \"defaultGambles\", \"practiceFileName\" or \"practiceTrial\".")
    }
  }
  
  # prepare variables 
  opts.vec <- paste0("g", 1:nOpts)
  outc.vec <- paste0("o", 1:nOutcs)
  prob.vec <- paste0("p", 1:nOutcs)
  
  ind.outc.vec <- do.call(paste, c(expand.grid(opts.vec, outc.vec), sep = ""))
  ind.prob.vec <- do.call(paste, c(expand.grid(opts.vec, prob.vec), sep = ""))
  
  g.rows <- nrow(gamble.df)
  
  if (isTRUE(randomizeVertical)){
    gamble.df <- gamble.df[sample(nrow(gamble.df)), ]
  }
  
  # get outcomes
  outcomes <- lapply(seq_len(g.rows), getDfeSamples, p.ind = ind.prob.vec,
                     o.ind = ind.outc.vec, df = gamble.df, nOpt = nOpts,
                     nVals = nDraws)
  
  if (isTRUE(withPracticeTrial)){
    outcomes.practice <- lapply(seq_len(nrow(practice.df)), getDfeSamples, p.ind = ind.prob.vec,
                                o.ind = ind.outc.vec, df = practice.df, nOpt = nOpts,
                                nVals = nDraws)
    
    outcomes <- c(outcomes.practice, outcomes)
  }
  
  
  
  if (isTRUE(withPracticeTrial)){
    gamble.df <- rbind(practice.df, gamble.df)
    g.rows <- nrow(gamble.df)
  }
  
  if (isTRUE(randomizeHorizontal)){
    locations.r <- matrix(NA, ncol = nOpts, nrow = g.rows)
    # Randomize option locations
    for (ro in seq_len(g.rows)){
      locations.r[ro, seq_len(nOpts)] <- sample(seq_len(nOpts))
      outcomes[[ro]] <- outcomes[[ro]][locations.r[ro,]]
    }
    
    option.order <- vector("list", g.rows)
    for (rows in seq_len(nrow(locations.r))){
      temp.order <- paste(locations.r[rows,], collapse = ";")
      option.order[[rows]] <- rep(temp.order, nDraws)
    }
    
    option.order <- unlist(option.order)
    
  } else {
    
    option.order <- rep(paste(seq_len(nOpts), collapse = ";"), nDraws * g.rows)
  }
  
  container <- list("option.order" = option.order,
                    "outcomes" = outcomes,
                    "gamble.df" = gamble.df,
                    "nDraws" = nDraws,
                    "nOpts" = nOpts,
                    "nOutcs" = nOutcs,
                    "gamble.order" = gamble.df$gamble)
  
  container
  
}