#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <stdlib.h>
#include <stdio.h>
#if defined HAVE_NETCDF_H && defined HAVE_LIBNETCDF
#include <netcdf.h>
#endif

#define ERRCODE 2

#define ERR(e) {printf("Error: %s\n", nc_strerror(e)); exit(ERRCODE);}
#define CACHE_SIZE 64000000
#define CACHE_NELEMS 1009
#define CACHE_PREEMPTION .75
#define DEFLATE_LEVEL 2
#if defined HAVE_LIBNETCDF && defined HAVE_NETCDF_H
SEXP createFile(SEXP _fileName, SEXP _X, SEXP _Y, SEXP _Z, SEXP _compress) {
    int NDIMS = 3 ; 
    int dimids[NDIMS];
    int X = INTEGER(_X)[0];
    int Y = INTEGER(_Y)[0];
    int Z = INTEGER(_Z)[0];
    int ncid, retval, varid, x_dimid, y_dimid, z_dimid;
    int compress = LOGICAL(_compress)[0];
    SEXP k = allocVector(LGLSXP,1);
    size_t chunksize[] = {1, Y, X};
    
    if ((retval = nc_create( translateChar(STRING_ELT(_fileName, 0)), NC_NETCDF4, &ncid)))
      ERR(retval);

    if ((retval = nc_def_dim(ncid, "event", X, &x_dimid)))
        ERR(retval);
    if ((retval = nc_def_dim(ncid, "channel", Y, &y_dimid)))
        ERR(retval);
    if ((retval = nc_def_dim(ncid, "sample", Z , &z_dimid))) // was NC_UNLIMITED Z 
        ERR(retval);

    dimids[0] = z_dimid;
    dimids[1] = y_dimid;
    dimids[2] = x_dimid;
    if ((retval = nc_def_var(ncid, "exprsMat", NC_FLOAT, NDIMS, dimids, &varid)))
        ERR(retval);
    
    if (( retval = nc_def_var_chunking(ncid, varid, NC_CHUNKED, chunksize)))
        ERR(retval);

    if (( retval = nc_set_var_chunk_cache(ncid, varid, CACHE_SIZE, CACHE_NELEMS, CACHE_PREEMPTION)))
        ERR(retval);
    
    if(compress) {
        if (( retval = nc_def_var_deflate(ncid, varid, 0, 1, DEFLATE_LEVEL)))
            ERR(retval);
    }

    if ((retval = nc_enddef(ncid)))
        ERR(retval);

    //Save variable attributes 
    //No of channels stored
    if((retval = nc_put_att_int(ncid, varid, "channelCount", NC_INT, 1, &Y)))
        ERR(retval);
    // No of samples 
    if((retval = nc_put_att_int(ncid, varid, "sampleCount", NC_INT, 1, &Z)))
        ERR(retval);
 
    int *eCount = (int *) R_alloc( sizeof(int), Z);
    for(int i =0 ; i < Z; i++) {
        eCount[i] = 0;
    }
    //No of events stored
    if((retval = nc_put_att_int(ncid, varid, "eventCount", NC_INT, Z, eCount)))
        ERR(retval);
    
    if ((retval = nc_close(ncid)))
        ERR(retval);

    return(k);
}

SEXP writeSlice(SEXP _fileName, SEXP _mat, SEXP _sample) {

    int retval, ncid, varid, nRow, nCol, sample;
    SEXP Rdim, k = allocVector(LGLSXP,1);
    Rdim = getAttrib(_mat, R_DimSymbol);
    nRow = INTEGER(Rdim)[0];
    nCol = INTEGER(Rdim)[1];
    sample = INTEGER(_sample)[0]-1;  // R to C indexing
    size_t start[] = {sample, 0, 0};
    size_t count[] = {1, nCol, nRow};
    double *mat = REAL(_mat);

    if ((retval = nc_open(translateChar(STRING_ELT(_fileName, 0)), NC_WRITE, &ncid)))
        ERR(retval);
 
    if((retval = nc_inq_varid (ncid, "exprsMat", &varid)))
        ERR(retval);  
    // check if max rows of ncdf file is exceeded at R level
    if((retval = nc_put_vara_double(ncid, varid, start, count, mat)))
        ERR(retval); 
    
    int sampCount;
    if((retval = nc_get_att_int(ncid, varid, "sampleCount", &sampCount)))
        ERR(retval);
   
    int len = sampCount;
    if(sample >= sampCount) {
        len = sample;
    }
    int *eCount = (int *) R_alloc( sizeof(int), len);
    if((retval = nc_get_att_int(ncid, varid, "eventCount", eCount)))
        ERR(retval);
    eCount[sample] = nRow;
    if((retval = nc_redef(ncid)))
        ERR(retval);
    if((retval = nc_put_att_int(ncid, varid, "eventCount", NC_INT, len, eCount)))
        ERR(retval);
    if((retval = nc_enddef(ncid)))
        ERR(retval);

    if ((retval = nc_close(ncid)))
        ERR(retval);
    return(k);

}

SEXP readSlice(SEXP _fileName, SEXP _y, SEXP _sample ) {

    int retval, ncid, varid, colStart, colEnd, nRow, sample;
    SEXP ans, dnms;
    sample = INTEGER(_sample)[0]-1;  // R to C indexing
    colStart = INTEGER(_y)[0] -1;
    colEnd = INTEGER(_y)[1] -1;
  
    if ((retval = nc_open(translateChar(STRING_ELT(_fileName, 0)), NC_NOWRITE, &ncid)))
        ERR(retval);
 
    if((retval = nc_inq_varid (ncid, "exprsMat", &varid)))
        ERR(retval);  

    int sampCount;
    if((retval = nc_get_att_int(ncid, varid, "sampleCount", &sampCount)))
        ERR(retval);
 
    int *eCount = (int *) R_alloc( sizeof(int), sampCount);
    if((retval = nc_get_att_int(ncid, varid, "eventCount", eCount)))
        ERR(retval);
    nRow = eCount[sample] ;
    
    size_t start[] = {sample, colStart, 0};
    size_t count[] = {1, colEnd +1 - colStart, nRow};
    PROTECT(ans = allocVector(REALSXP, nRow*(colEnd + 1- colStart)));
    double *mat = REAL(ans);
    if((retval = nc_get_vara_double(ncid, varid, start, count, mat)))
        ERR(retval);  
    if ((retval = nc_close(ncid)))
        ERR(retval);
    PROTECT(dnms = allocVector(INTSXP, 2));
    INTEGER(dnms)[0] = nRow;
    INTEGER(dnms)[1]= colEnd + 1 - colStart;
    setAttrib(ans,R_DimSymbol, dnms);
    UNPROTECT(2);
    return(ans);

}

SEXP readEventCounts(SEXP _fileName) {
    int retval, ncid, varid;
    SEXP ans;
    if ((retval = nc_open(translateChar(STRING_ELT(_fileName, 0)), NC_NOWRITE, &ncid)))
        ERR(retval);

    if((retval = nc_inq_varid (ncid, "exprsMat", &varid)))
        ERR(retval);  
     
    int sampCount;
    if((retval = nc_get_att_int(ncid, varid, "sampleCount", &sampCount)))
        ERR(retval);
    
    PROTECT(ans = allocVector(INTSXP, sampCount));
    int *eCount = INTEGER(ans);
    if((retval = nc_get_att_int(ncid, varid, "eventCount", eCount)))
        ERR(retval);

    if ((retval = nc_close(ncid)))
        ERR(retval);
    UNPROTECT(1);
    return(ans);
}
#endif
