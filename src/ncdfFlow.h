
#include <Rinternals.h>
#include <Rdefines.h>
#include <Rmath.h>
#ifdef HAVE_NETCDFDEF
SEXP ncdf_bitarray_set(SEXP bits, SEXP _indx);
SEXP ncdf_bitarray_getSetBitPos(SEXP bits);
SEXP ncdf_bitarray_getBitStatus(SEXP bits);
SEXP ncdf_bitarray_Flip(SEXP bits);
#endif
