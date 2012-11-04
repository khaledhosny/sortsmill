%module gsl

%{

SCM scm_init_gsl_module (void);  
#include <gsl/gsl_machine.h>

%}

//%scheme %{ (load-extension "./gsl_wrap.so" "scm_init_gsl_module") %}

%include <gsl/gsl_machine.h>
