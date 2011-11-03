#include <R.h>
#include <R_ext/Rdynload.h>
#include "flowWorkspace.h"
static const R_CallMethodDef CallEntries[] = {
    {"ncdf_bitarray_set", (DL_FUNC)&ncdf_bitarray_set, 2},
    {"ncdf_bitarray_getSetBitPos", (DL_FUNC)&ncdf_bitarray_getSetBitPos, 1},
    {"ncdf_bitarray_getBitStatus", (DL_FUNC)&ncdf_bitarray_getBitStatus, 1},
    {"ncdf_bitarray_Flip", (DL_FUNC)&ncdf_bitarray_Flip, 1},
    {NULL, NULL, 0}
};

void R_init_ncdfFlow(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    //R_useDynamicSymbols(dll, FALSE);

	R_RegisterCCallable("ncdfFlow","ncdf_bitarray_set", (DL_FUNC)&ncdf_bitarray_set);
	R_RegisterCCallable("ncdfFlow","ncdf_bitarray_getSetBitPos",(DL_FUNC)&ncdf_bitarray_getSetBitPos);
	R_RegisterCCallable("ncdfFlow","ncdf_bitarray_getBitStatus",(DL_FUNC)&ncdf_bitarray_getBitStatus);
	R_RegisterCCallable("ncdfFlow","ncdf_bitarray_Flip",(DL_FUNC)&ncdf_bitarray_Flip);
}
