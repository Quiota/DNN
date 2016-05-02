#include <iostream>
#include <omp.h>
using namespace std;
 
#include <mkl.h>
 
#define MATHLIB_STANDALONE
#include <Rmath.h>
 
int main(){
 
  char *ntran = "N";
  char *ytran = "T";
  const double one = 1.0;
  const double zero = 0.0;
 
  //set seed
  set_seed(123,456);
 
  //set threads
  omp_set_num_threads(4);
 
  int n = 1000;
  int nn = n*n;
  double *A = new double[nn];
  double *B = new double[nn];
  double *C = new double[nn];
 
  for(int i = 0; i < nn; i++) A[i] = rnorm(0,1);
 
  //make a pd matrix
  dgemm(ntran, ytran, &n, &n, &n, &one, A, &n, A, &n, &zero, B, &n);
 
  exit(0);
}
