#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

// Declare the C functions made available to R here
extern SEXP r_louvain(SEXP r_input_network, SEXP r_resolution, SEXP r_seed, SEXP r_lower_order);
extern SEXP r_signed_modularity(SEXP r_input_network, SEXP r_input_memberships, SEXP r_resolution);

// Register native routines
static const R_CallMethodDef CallEntries[] = {

    {
        "r_louvain", // Name of function call in R
        (DL_FUNC)&r_louvain, // Name of C function
        4 // Number of arguments
    },
    {
        "r_signed_modularity", // Name of function call in R
        (DL_FUNC)&r_signed_modularity, // Name of C function
        3 // Number of arguments
    },
    {NULL, NULL, 0}

};

// Set up call in package
void R_init_SemNeT(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
