%module gsl
%include "typemaps.i"

%{

#include <gsl/gsl_inline.h>
#include <gsl/gsl_machine.h>
#include <gsl/gsl_poly.h>
#include <gsl/gsl_matrix_double.h>
#include <gsl/gsl_linalg.h>

%}

#if SWIGGUILE
%multiple_values;
#endif

%import <gsl/gsl_inline.h>

%include <gsl/gsl_machine.h>

int gsl_poly_solve_quadratic (double a, double b, double c, double *OUTPUT, double *OUTPUT);
int gsl_poly_solve_cubic (double a, double b, double c, double *OUTPUT, double *OUTPUT, double *OUTPUT);
