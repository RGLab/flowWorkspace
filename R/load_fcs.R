#' Read a single FCS file in to a cytoframe
#' 
#' Similar to \code{\link[flowCore]{read.FCS}}, this takes a filename for a single
#' FCS file and returns a \code{cytoframe}.
#'
#' The function \code{load_cytoframe_from_fcs} works with the output of the FACS machine
#' software from a number of vendors (FCS 2.0, FCS 3.0 and List Mode Data LMD).
#' However, the FCS 3.0 standard includes some options that are not yet
#' implemented in this function. If you need extensions, please let us know.
#' The output of the function is an object of class \code{cytoframe}.
#' 
#' For specifications of FCS 3.0 see \url{http://www.isac-net.org} and the file
#' \url{../doc/fcs3.html} in the \code{doc} directory of the package.
#' 
#' The \code{which.lines} arguments allow you to read a subset of the record as
#' you might not want to read the thousands of events recorded in the FCS file.
#' It is mainly used when there is not enough memory to read one single FCS
#' (which probably will not happen).  It will probably take more time than
#' reading the entire FCS (due to the multiple disk IO).
#'
#'
#' @param filename The filename of the single FCS file to be read
#' @param transformation A character string that defines the type of
#' transformation. Valid values are \code{linearize} (default),
#' \code{linearize-with-PnG-scaling}, or \code{scale}.  The \code{linearize}
#' transformation applies the appropriate power transform to the data. The
#' \code{linearize-with-PnG-scaling} transformation applies the appropriate
#' power transform for parameters stored on log scale, and also a linear
#' scaling transformation based on the "gain" (FCS $PnG keywords) for
#' parameters stored on a linear scale. The \code{scale} transformation scales
#' all columns to \eqn{[0,10^{decades}]}.  defaulting to \eqn{decades=0} as in the FCS4
#' specification.  A logical can also be used: \code{TRUE} is equal to
#' \code{linearize} and \code{FALSE}(or \code{NULL}) corresponds to no
#' transformation.  Also, when the transformation keyword of the FCS header is
#' set to "custom" or "applied", no transformation will be used.
#' @param which.lines Numeric vector to specify the indices of the lines to be
#' read. If it is NULL, all the records are read. If it is of length 1, a random sample of
#' the size indicated by \code{which.lines} is read in.
#' @param decades When scaling is activated, the number of decades to use for
#' the output.
#' @param is_h5 Logical indicating whether the data should be stored in h5 format
#' @param h5_filename String specifying a name for the h5 file if \code{is_h5} is TRUE
#' @param min.limit The minimum value in the data range that is allowed. Some
#' instruments produce extreme artifactual values. The positive data range for
#' each parameter is completely defined by the measurement range of the
#' instrument and all larger values are set to this threshold. The lower data
#' boundary is not that well defined, since compensation might shift some
#' values below the original measurement range of the instrument. This can be 
#' set to an arbitrary number or to \code{NULL} (the default value), in which 
#' case the original values are kept.
#' @param truncate_max_range Logical. Default is TRUE. can be optionally
#' turned off to avoid truncating the extreme positive value to the instrument
#' measurement range, i.e. '$PnR'.
#' @param dataset The FCS file specification allows for multiple data segments
#' in a single file. Since the output of \code{load_cytoframe_from_cytoset} is a single
#' \code{cytoframe} we can't automatically read in all available sets. This
#' parameter allows the user to choose one of the subsets for import. Its value should
#' be an integer in the range of available data sets. This argument
#' is ignored if there is only a single data segment in the FCS file.
#' @param emptyValue Logical indicating whether or not to allow empty values for
#' keywords in TEXT segment.  It affects how double delimiters are
#' treated. If TRUE, double delimiters are parsed as a pair of start and
#' end single delimiters for an empty value.  Otherwise, double delimiters are
#' parsed as one part of the string of the keyword value. The default is TRUE.
#' @param num_threads Integer allowing for parallelization of the parsing
#' operation by specifiying a number of threads 
#' @param ignore.text.offset Logical indicating whether to ignore the keyword values in TEXT
#' segment when they don't agree with the HEADER.  Default is FALSE, which
#' throws the error when such a discrepancy is found. Users can turn it on to
#' ignore the TEXT segment when they are sure of the accuracy of the HEADER segment so that the
#' file still can be read.
#' @param text.only whether to only parse text section of FCS (default is FALSE), it is sometime useful to skip loading data section for the faster loading meta data from FCS
#' \code{\link[Biobase]{read.AnnotatedDataFrame}}, see details
#' 
#' @return
#' An object of class
#' \code{\link{cytoframe}} that contains the data, the parameters monitored, 
#' and the keywords and values saved in the header of the FCS file.
#' 
#' @name load_cytoframe_from_fcs
#' @family cytoframe/cytoset IO functions
#' @export
load_cytoframe_from_fcs <- function(filename,
                     transformation="linearize",
                     which.lines=NULL,
                     decades=0,
                      is_h5= NULL,
                      backend = get_default_backend(),
                      uri = NULL,
                      h5_filename = NULL,
                     min.limit=NULL,
                     truncate_max_range = TRUE,
                     dataset=NULL,
                     emptyValue=TRUE,
                     num_threads = 1,
                     ignore.text.offset = FALSE,
                     text.only = FALSE)
{
  backend <- match.arg(backend, c("mem", "h5", "tile"))
  if(!is.null(is_h5))
  {
    warning("'is_h5' argument is deprecated by 'backend'! ")
    if(is_h5)
      backend <-"h5"
  }
  if(!is.null(h5_filename))
  {
    warning("'h5_filename' argument is deprecated by 'uri'! ")
      uri <- h5_filename
  }
  if(backend != "mem")
  {
    if(is.null(uri))
      uri <- tempfile(fileext = paste0(".", backend))
  }else
    uri <- ""
    fr <- new("cytoframe")
    if(is.null(dataset))
      dataset <- 0
    if(is.null(min.limit)){
      truncate_min_val <- FALSE
      min.limit <- -111
    }else
      truncate_min_val <- TRUE
    if(is.null(which.lines))
      which.lines <- numeric()
    else
    {
    	# Verify that which.lines is positive and within file limit.
    	if (length(which.lines) > 1) {
    		which.lines <- which.lines -1
    	}
    }
    fr@pointer <- parseFCS(normalizePath(filename), list(which.lines = which.lines
                                                         , transformation = transformation
                                                         , decades = decades
                                                         , truncate_min_val = truncate_min_val
                                                         , min_limit = min.limit
                                                         , truncate_max_range = truncate_max_range
                                                         , dataset = dataset
                                                         , emptyValue = emptyValue
                                                         , num_threads = num_threads
                                                         , ignoreTextOffset = ignore.text.offset
                                                         )
                                                     , text_only = text.only
                                                    , format = backend
                                                    , uri = uri
                            )
     fr@use.exprs <- !text.only

    return(fr)
}

#' Read one or several FCS files in to a cytoset
#' 
#' Similar to \code{\link[flowCore]{read.flowSet}}, this takes a list of FCS filenames
#' and returns a \code{cytoset}.
#' 
#' There are four different ways to specify the file from which data is to be
#' imported:
#' 
#' First, if the argument \code{phenoData} is present and is of class
#' \code{\link[Biobase:class.AnnotatedDataFrame]{AnnotatedDataFrame}}, then the
#' file names are obtained from its sample names (i.e. row names of the
#' underlying data.frame).  Also column \code{name} will be generated based on
#' sample names if it is not there. This column is mainly used by visualization
#' methods in flowViz.  Alternatively, the argument \code{phenoData} can be of
#' class \code{character}, in which case this function tries to read a
#' \code{AnnotatedDataFrame} object from the file with that name by calling
#' \code{\link[Biobase]{read.AnnotatedDataFrame}(file.path(path,phenoData),\dots{})}.
#' 
#' In some cases the file names are not a reasonable selection criterion and
#' the user might want to import files based on some keywords within the file.
#' One or several keyword value pairs can be given as the phenoData argument in
#' form of a named list.
#' 
#' Third, if the argument \code{phenoData} is not present and the argument
#' \code{files} is not \code{NULL}, then \code{files} is expected to be a
#' character vector with the file names.
#' 
#' Fourth, if neither the argument \code{phenoData} is present nor \code{files}
#' is not \code{NULL}, then the file names are obtained by calling
#' \code{dir(path, pattern)}.
#' @param files Optional character vector with filenames.
#' @param path Directory where to look for the files.
#' @param pattern This argument is passed on to
#' \code{\link[base:list.files]{dir}}, see details.
#' @param phenoData An object of class \code{AnnotatedDataFrame},
#' \code{character} or a list of values to be extracted from the
#' \code{\link{cytoframe}} object, see details.
#' @param descriptions Character vector to annotate the object of class
#' \code{\link{cytoset}}.
#' @param name.keyword An optional character vector that specifies which FCS
#' keyword to use as the sample names. If this is not set, the GUID of the FCS
#' file is used for sampleNames, and if that is not present (or not unique),
#' then the file names are used.
#' @param transformation see \code{\link{load_cytoframe_from_fcs}} for details.
#' @param which.lines see \code{\link{load_cytoframe_from_fcs}} for details.
#' @param decades see \code{\link{load_cytoframe_from_fcs}} for details.
#' @param is_h5 logical indicating whether the data should be stored in h5 format
#' @param min.limit see \code{\link{load_cytoframe_from_fcs}} for details.
#' @param truncate_max_range see \code{\link{load_cytoframe_from_fcs}} for details.
#' @param dataset see \code{\link{load_cytoframe_from_fcs}} for details.
#' @param emptyValue see \code{\link{load_cytoframe_from_fcs}} for details.
#' @param num_threads Integer allowing for parallelization of the parsing
#' operation by specifiying a number of threads
#' @param ignore.text.offset see \code{\link{load_cytoframe_from_fcs}} for details.
#' @param sep Separator character that gets passed on to
#' \code{\link[Biobase]{read.AnnotatedDataFrame}}.
#' @param as.is logical that gets passed on to
#' \code{\link[Biobase]{read.AnnotatedDataFrame}}. This controls the automatic
#' coercion of characters to factors in the \code{phenoData}.
#' @param name An optional character scalar used as name of the object.
#' @param h5_dir String specifying a name for the h5 directory
#' for the h5 files if \code{is_h5} is TRUE
#' @param \dots Further arguments that get passed on to
#' @param file_col_name optionally specify the column name that stores the fcs filename when phenoData is supplied
#' \code{\link[Biobase]{read.AnnotatedDataFrame}}, see details.
#' 
#' @return An object of class \code{\link{cytoset}}.
#' 
#' @name load_cytoset_from_fcs
#' @family cytoframe/cytoset IO functions
#' @export
#' @importFrom Biobase read.AnnotatedDataFrame AnnotatedDataFrame
load_cytoset_from_fcs <- function(files=NULL, path=".", pattern=NULL, phenoData=NULL,
                         descriptions, name.keyword,
                         transformation="linearize",
                         which.lines=NULL,
                         decades=0,
                         is_h5= NULL
                         , h5_dir = NULL
                         , backend = get_default_backend()
                         , backend_dir = tempdir()
                         , min.limit=NULL,
                         truncate_max_range = TRUE,
                         dataset=NULL,
                         emptyValue=TRUE,
                         num_threads = 1,
                         ignore.text.offset = FALSE,
                         sep="\t", as.is=TRUE, name
                        , file_col_name = NULL
                        , ...)
{
  backend <- match.arg(backend, c("mem", "h5", "tile"))
  if(!is.null(is_h5))
  {
    warning("'is_h5' argument is deprecated by 'backend'! ")
    if(is_h5)
      backend <-"h5"
  }
  if(!is.null(h5_dir))
  {
    warning("'h5_dir' argument is deprecated by 'backend_dir'! ")
    backend_dir <- h5_dir
  }
  
    if(!dir.exists(backend_dir))
      dir.create(backend_dir)
  
    if(is.null(dataset))
      dataset <- 1
    if(is.null(min.limit)){
      truncate_min_val <- FALSE
      min.limit <- -111
    }else
      truncate_min_val <- TRUE
    if(is.null(which.lines))
      which.lines <- numeric()
    else
    {     
      if (length(which.lines) > 1) {
        which.lines <- which.lines -1
      }
    }    
    phenoData <- flowCore:::parse_pd_for_read_fs(files, path, pattern, phenoData, sep, as.is, file_col_name = file_col_name, ...)
    pd <- pData(phenoData)
    if(is.null(file_col_name))
    {
      cols <- colnames(pd)
      fidx <- grep("file|filename", cols, ignore.case=TRUE)
      file_col_name <- cols[fidx]
    }
    files <- pd[[file_col_name]]
    names(files) <- rownames(pd)#set guid
    cs <- fcs_to_cytoset(sapply(files, normalizePath), list(which.lines = which.lines
                                            , transformation = transformation
                                            , decades = decades
                                            , truncate_min_val = truncate_min_val
                                            , min_limit = min.limit
                                            , truncate_max_range = truncate_max_range
                                            , dataset = dataset
                                            , emptyValue = emptyValue
                                            , num_threads = num_threads
                                            , ignoreTextOffset = ignore.text.offset
                                          )
                                          , backend = backend
                                          , backend_dir = normalizePath(backend_dir)
                                  )
    cs <- new("cytoset", pointer = cs)
    
   pd[[file_col_name]] <- NULL
   pData(cs) <- pd
	 cs_flush_meta(cs)#make sure pd is synced to disk 

    
    cs
    
}

