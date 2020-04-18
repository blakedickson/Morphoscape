
#' Splits a weights dataframe into a number of jobs to be computed separately
#'
#' @param weights.df A dataframe of weight combinations to split.
#'   Can be generated from generate_weights
#' @param njobs The number of jobs to split into
#' @param out.dir The directory to save jobs to
#' @param overwrite A logical scalar. Should the output folder be overwitten?
#'   Defaults to FALSE
#'
#' @return Writes njobs number of job files to be computed separatley
#' @export
#'
#' @examples
split.jobs <- function(weights, njobs, dir = NULL){

  to <- floor(seq(0, nrow(weights), length.out = njobs+1))

  from <- to+1
  from <- from[-length(from)]
  to <- to[-1]


  if(is.null(dir)){
    dir = "./jobs"
  }

  job.dir <- paste(dir,"/jobs",sep="")
  ans.dir <- paste(dir,"/ans",sep="")

  unlink(job.dir, recursive = T)
  unlink(ans.dir, recursive = T)

  dir.create(job.dir, recursive = T)
  dir.create(ans.dir, recursive = T)

  for (i in 1:length(from) ) {
    weights.df <- weights[from[i]:to[i],]
    save(weights.df, file= paste(job.dir,"/job_",i,".Rdata",sep=""))
  }

}


init.QTM.cluster <- function(Fn.surfaces, morphospace, dir, perms = NULL, step = NULL, njobs){

  if(!dir.exists(dir)){
    dir.create(dir)
  }


  nvar = length(Fn.surfaces)

  if(is.null(perms)){
    if(is.null(step))
    {stop (cat("step size is not defined"))
    }  else{

      perms <- num.permutations(step = step, verbose = T, nvar= length(Fn.surfaces))
    }
  }

  num.perm <- nrow(perms)

  xmar<-range(Fn.surfaces[[1]][[1]]$x)
  ymar<-range(Fn.surfaces[[1]][[1]]$y)

  split.jobs(perms, njobs, dir)

  save(Fn.surfaces, morphospace, xmar, ymar, num.perm, step, njobs,
       file = paste(dir,"/QTMdata.Rdata",sep=""))

}




loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}



#' recombines completed split jobs
#'
#' @param ans.dir Directory containing compoleted jobs
#'
#' @return returns a combined matrix of all computed weight combinations
#' @export
#'
#' @examples
reduce_ans <- function(ans.dir, sortby = c("z","lik","dist")){
  ls <- list.files(ans.dir)
  ans <- NULL

  for (i in 1:length(ls)){

    tmp.ans<- loadRData(file.path(ans.dir, ls[i]))
    ans <- rbind(ans,tmp.ans)

  }

  ans <- as.data.frame(ans)


  ans <- ans[order(ans[,sortby], decreasing = T),]

  return(ans)

}


reduce_dir <- function(ans.dir, result.dir, sortby = c("z","lik","dist")){

  unlink(result.dir, recursive = T)
  dir.create(result.dir)


  if( length(list.dirs(path = ans.dir)) > 1 ) {
    ls <- list.dirs(path = ans.dir, full.names = F)[-1]
  } else{
    ls <- list.dirs(path = ans.dir)
  }

  ans <- NULL

  for(i in 1:length(ls)){
    dir <- paste(ans.dir,ls[i],sep="")
    ans <- reduce_ans(dir, sortby = sortby)
    write.csv(ans, file= paste(result.dir,ls[i],".csv",sep = ""))
  }

}

split.jobs <- function(weights, njobs, dir = NULL){

  to <- floor(seq(0, nrow(weights), length.out = njobs+1))

  from <- to+1
  from <- from[-length(from)]
  to <- to[-1]


  if(is.null(dir)){
    dir = "./jobs"
  }

  job.dir <- paste(dir,"/jobs",sep="")
  ans.dir <- paste(dir,"/ans",sep="")

  unlink(job.dir, recursive = T)
  unlink(ans.dir, recursive = T)

  dir.create(job.dir, recursive = T)
  dir.create(ans.dir, recursive = T)

  for (i in 1:length(from) ) {
    weights.df <- weights[from[i]:to[i],]
    save(weights.df, file= paste(job.dir,"/job_",i,".Rdata",sep=""))
  }

}

init.QTM.cluster <- function(Fn.surfaces, morphospace, dir, perms = NULL, step = NULL, njobs){

  if(!dir.exists(dir)){
    dir.create(dir)
  }


  nvar = length(Fn.surfaces)

  if(is.null(perms)){
    if(is.null(step))
    {stop (cat("step size is not defined"))
    }  else{

      perms <- generate.weights(step = step, verbose = T, nvar= length(Fn.surfaces))
    }
  }

  num.perm <- nrow(perms)

  xmar<-range(Fn.surfaces[[1]][[1]]$x)
  ymar<-range(Fn.surfaces[[1]][[1]]$y)

  split.jobs(perms, njobs, dir)

  save(Fn.surfaces, morphospace, xmar, ymar, num.perm, step, njobs,
       file = paste(dir,"/QTMdata.Rdata",sep=""))

}


