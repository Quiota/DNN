/*******************************************************************************
!   Copyright(C) 2005-2015 Intel Corporation. All Rights Reserved.
!   
!   The source code, information  and  material ("Material") contained herein is
!   owned  by Intel Corporation or its suppliers or licensors, and title to such
!   Material remains  with Intel Corporation  or its suppliers or licensors. The
!   Material  contains proprietary information  of  Intel or  its  suppliers and
!   licensors. The  Material is protected by worldwide copyright laws and treaty
!   provisions. No  part  of  the  Material  may  be  used,  copied, reproduced,
!   modified, published, uploaded, posted, transmitted, distributed or disclosed
!   in any way  without Intel's  prior  express written  permission. No  license
!   under  any patent, copyright  or  other intellectual property rights  in the
!   Material  is  granted  to  or  conferred  upon  you,  either  expressly,  by
!   implication, inducement,  estoppel or  otherwise.  Any  license  under  such
!   intellectual  property  rights must  be express  and  approved  by  Intel in
!   writing.
!   
!   *Third Party trademarks are the property of their respective owners.
!   
!   Unless otherwise  agreed  by Intel  in writing, you may not remove  or alter
!   this  notice or  any other notice embedded  in Materials by Intel or Intel's
!   suppliers or licensors in any way.
!
!*******************************************************************************
!  Content: Intel MKL RCI (P)CG C example
!
!*******************************************************************************/

#include <stdio.h>
#include "mkl_rci.h"
#include "mkl_blas.h"
#include "mkl_spblas.h"
#include "mkl_service.h"

/*MIC Memory Management macros */
#define ALLOC   alloc_if(1)
#define FREE    free_if(1)
#define RETAIN  free_if(0)
#define REUSE   alloc_if(0)

/*---------------------------------------------------------------------------   */
/*  Example program for solving symmetric positive definite system of equations.*/
/*  Full case: full functionality of RCI (P)CG is used.                         */
/*---------------------------------------------------------------------------   */
int main (void)
{
  /* The number of the MIC card to use */
  __declspec(target(mic)) int micno = 0;

  /*---------------------------------------------------------------------------*/
  /* Define arrays for the upper triangle of the coefficient matrix and        */
  /* preconditioner as well as an array for rhs vector                         */
  /* Compressed sparse row storage is used for sparse representation           */
	/*---------------------------------------------------------------------------*/
  MKL_INT n = 8, rci_request, itercount, expected_itercount = 8, i;
  double *rhs = 0;
  /* Fill all arrays containing matrix data. */
  MKL_INT ia[9] = { 1, 5, 8, 10, 12, 15, 17, 18, 19 };
  MKL_INT ja[18] = { 1, 3, 6, 7,
    2, 3, 5,
    3, 8,
    4, 7,
    5, 6, 7,
    6, 8,
    7,
    8
  };
  double a[18] = { 7.E0, 1.E0, 2.E0, 7.E0,
    -4.E0, 8.E0, 2.E0,
    1.E0, 5.E0,
    7.E0, 9.E0,
    5.E0, 1.E0, 5.E0,
    -1.E0, 5.E0,
    11.E0,
    5.E0
  };

	/*---------------------------------------------------------------------------*/
  /* Allocate storage for the solver ?par and temporary storage tmp            */
	/*---------------------------------------------------------------------------*/
  MKL_INT length = 128;
  MKL_INT *ipar = 0;
  double *dpar = 0, *tmp = 0;
	/*---------------------------------------------------------------------------*/
  /* Some additional variables to use with the RCI (P)CG solver                */
	/*---------------------------------------------------------------------------*/
  char matdes[3];
  double *solution, *expected_sol = 0;
  double tmp_expected_sol[8] = { 1.E0, 0.E0, 1.E0, 0.E0, 1.E0, 0.E0, 1.E0, 0.E0 };
  double one = 1.E0;
  double euclidean_norm, temp[8];
  char tr = 'u';
  double eone = -1.E0;
  MKL_INT ione = 1;

  rhs = (double*)malloc((8)*sizeof(double));
  ipar = (MKL_INT*)malloc((128)*sizeof(MKL_INT)); 
  dpar = (double*)malloc((128)*sizeof(double));
  tmp = (double*)malloc((4 * n)*sizeof(double));
  solution = (double*)malloc((8)*sizeof(double));
  expected_sol = (double*)malloc((8)*sizeof(double)); 

  	/*---------------------------------------------------------------------------*/
  /* Initialize the initial guess                                              */
	/*---------------------------------------------------------------------------*/
  for (i = 0; i < n; i++)
    {
        solution[i] = 0.E0;
        expected_sol[i] = tmp_expected_sol[i];
  }
	/*---------------------------------------------------------------------------*/
  /* Initialize the right hand side through matrix-vector product              */
	/*---------------------------------------------------------------------------*/
  mkl_dcsrsymv (&tr, &n, a, ia, ja, expected_sol, rhs);

  matdes[0] = 'd';
  matdes[1] = 'l';
  matdes[2] = 'n';
	/*---------------------------------------------------------------------------*/
  /* Initialize the solver                                                     */
	/*---------------------------------------------------------------------------*/
#pragma offload target(mic:micno) \
        in(n) \
        in(solution:length(n) RETAIN) \
        in(rhs:length(n) RETAIN) \
        out(rci_request) \
        in(ipar:length(128) RETAIN) \
        in(dpar:length(128) RETAIN) \
        in(tmp:length(4*n) RETAIN) 
	{
		dcg_init (&n, solution, rhs, &rci_request, ipar, dpar, tmp);
	}
    if (rci_request != 0)
    goto failure;
	/*---------------------------------------------------------------------------*/
  /* Set the desired parameters:                                               */
  /* INTEGER parameters:                                                        */
  /* set the maximal number of iterations to 100                               */
  /* LOGICAL parameters:                                                       */
  /* run the Preconditioned version of RCI (P)CG with preconditioner C_inverse */
  /* DOUBLE parameters                                                         */
  /* -                                                                         */
	/*---------------------------------------------------------------------------*/
  ipar[4] = 100;
  ipar[10] = 1;
	/*---------------------------------------------------------------------------*/
  /* Check the correctness and consistency of the newly set parameters         */
	/*---------------------------------------------------------------------------*/
#pragma offload target(mic:micno) \
        nocopy(n) \
        nocopy(solution:length(n) REUSE RETAIN) \
        nocopy(rhs:length(n) REUSE RETAIN) \
        out(rci_request) \
        nocopy(ipar:length(128) REUSE RETAIN) \
        nocopy(dpar:length(128) REUSE RETAIN) \
        out(tmp:length(4*n) REUSE RETAIN)
	{
		dcg_check (&n, solution, rhs, &rci_request, ipar, dpar, tmp);
	}
    if (rci_request != 0)
    goto failure;
	/*---------------------------------------------------------------------------*/
  /* Compute the solution by RCI (P)CG solver                                  */
  /* Reverse Communications starts here                                        */
	/*---------------------------------------------------------------------------*/

rci:
#pragma offload target(mic:micno) \
        nocopy(n) \
        inout(solution:length(n) REUSE RETAIN) \
        nocopy(rhs:length(n) REUSE RETAIN) \
        out(rci_request) \
        nocopy(ipar:length(128) REUSE RETAIN) \
        nocopy(dpar:length(128) REUSE RETAIN) \
        inout(tmp:length(4*n) REUSE RETAIN)
	{
		dcg (&n, solution, rhs, &rci_request, ipar, dpar, tmp);
	}
	/*---------------------------------------------------------------------------*/
  /* If rci_request=0, then the solution was found according to the requested  */
  /* stopping tests. In this case, this means that it was found after 100      */
  /* iterations.                                                               */
	/*---------------------------------------------------------------------------*/
  if (rci_request == 0)
    goto getsln;
	/*---------------------------------------------------------------------------*/
  /* If rci_request=1, then compute the vector A*tmp[0]                      */
  /* and put the result in vector tmp[n]                                     */
	/*---------------------------------------------------------------------------*/
  if (rci_request == 1)
    {
     
      mkl_dcsrsymv (&tr, &n, a, ia, ja, tmp, &tmp[n]);

      goto rci;
    }
	/*---------------------------------------------------------------------------*/
  /* If rci_request=2, then do the user-defined stopping test: compute the     */
  /* Euclidean norm of the actual residual using MKL routines and check if     */
  /* it is less than 1.E-8                                                     */
	/*---------------------------------------------------------------------------*/
  if (rci_request == 2)
    {
      mkl_dcsrsymv (&tr, &n, a, ia, ja, solution, temp);
      daxpy (&n, &eone, rhs, &ione, temp, &ione);
      euclidean_norm = dnrm2 (&n, temp, &ione);
		/*---------------------------------------------------------------------------*/
      /* The solution has not been found yet according to the user-defined stopping */
      /* test. Continue RCI (P)CG iterations.                                      */
		/*---------------------------------------------------------------------------*/
      if (euclidean_norm > 1.e-8)
	goto rci;
		/*---------------------------------------------------------------------------*/
      /* The solution has been found according to the user-defined stopping test   */
		/*---------------------------------------------------------------------------*/
      else
	goto getsln;
    }
	/*---------------------------------------------------------------------------*/
  /* If rci_request=3, then compute apply the preconditioner matrix C_inverse  */
  /* on vector tmp[2*n] and put the result in vector tmp[3*n]                  */
  /*---------------------------------------------------------------------------*/
  if (rci_request == 3)
    {
      mkl_dcsrsv (&matdes[2], &n, &one, matdes, a, ja, ia, &ia[1],
		  &tmp[2 * n], &tmp[3 * n]);
      goto rci;
    }
  /*---------------------------------------------------------------------------*/
  /* If rci_request=anything else, then dcg subroutine failed                  */
  /* to compute the solution vector: solution[n]                               */
	/*---------------------------------------------------------------------------*/
  goto failure;

  /*---------------------------------------------------------------------------*/
  /* Reverse Communication ends here                                           */
  /* Get the current iteration number into itercount                           */
  /*---------------------------------------------------------------------------*/
getsln:
#pragma offload target(mic:micno) \
        in(n) \
        inout(solution:length(n) REUSE) \
        nocopy(rhs:length(n) REUSE) \
        out(rci_request) \
        nocopy(ipar:length(128) REUSE) \
        nocopy(dpar:length(128) REUSE) \
        inout(tmp:length(4*n) REUSE) \
        out(itercount)
	{
        dcg_get (&n, solution, rhs, &rci_request, ipar, dpar, tmp,
                                                       &itercount);
    }
	/*---------------------------------------------------------------------------*/
  /* Print solution vector: solution[n] and number of iterations: itercount    */
	/*---------------------------------------------------------------------------*/
  printf ("The system has been solved\n");
  printf ("The following solution obtained\n");
  for (i = 0; i < n / 2; i++)
    printf ("%6.3f  ", solution[i]);
  printf ("\n");
  for (i = n / 2; i < n; i++)
    printf ("%6.3f  ", solution[i]);
  printf ("\nExpected solution is\n");
  for (i = 0; i < n / 2; i++)
    {
      printf ("%6.3f  ", expected_sol[i]);
      expected_sol[i] -= solution[i];
    }
  printf ("\n");
  for (i = n / 2; i < n; i++)
    {
      printf ("%6.3f  ", expected_sol[i]);
      expected_sol[i] -= solution[i];
    }
  printf ("\nNumber of iterations: %d\n", itercount);
  i = 1;
  euclidean_norm = dnrm2 (&n, expected_sol, &i);

	/*-------------------------------------------------------------------------*/
  /* Release internal MKL memory that might be used for computations         */
  /* NOTE: It is important to call the routine below to avoid memory leaks   */
  /* unless you disable MKL Memory Manager                                   */
	/*-------------------------------------------------------------------------*/
  MKL_Free_Buffers ();

  if (itercount == expected_itercount && euclidean_norm < 1.0e-12)
    {
      printf
	("This example has successfully PASSED through all steps of computation!");
      printf ("\n");
      return 0;
    }
  else
    {
      printf
	("This example may have FAILED as either the number of iterations differs");
      printf ("\nfrom the expected number of iterations %d, or the ",
	      expected_itercount);
      printf ("computed solution\ndiffers much from the expected solution ");
      printf ("(Euclidean norm is %e), or both.\n", euclidean_norm);
      return 1;
    }
	/*-------------------------------------------------------------------------*/
  /* Release internal MKL memory that might be used for computations         */
  /* NOTE: It is important to call the routine below to avoid memory leaks   */
  /* unless you disable MKL Memory Manager                                   */
	/*-------------------------------------------------------------------------*/
failure:printf
    ("This example FAILED as the solver has returned the ERROR ");
  printf ("code %d", rci_request);
  MKL_Free_Buffers ();
  return 1;
}
