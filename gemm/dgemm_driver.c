/*******************************************************************************
!  Copyright(C) 2001-2015 Intel Corporation. All Rights Reserved.
!  The source code contained  or  described herein and all documents related to
!  the source code ("Material") are owned by Intel Corporation or its suppliers
!  or licensors.  Title to the  Material remains with  Intel Corporation or its
!  suppliers and licensors. The Material contains trade secrets and proprietary
!  and  confidential  information of  Intel or its suppliers and licensors. The
!  Material  is  protected  by  worldwide  copyright  and trade secret laws and
!  treaty  provisions. No part of the Material may be used, copied, reproduced,
!  modified, published, uploaded, posted, transmitted, distributed or disclosed
!  in any way without Intel's prior express written permission.
!  No license  under any  patent, copyright, trade secret or other intellectual
!  property right is granted to or conferred upon you by disclosure or delivery
!  of the Materials,  either expressly, by implication, inducement, estoppel or
!  otherwise.  Any  license  under  such  intellectual property  rights must be
!  express and approved by Intel in writing.
!
!*******************************************************************************
!  Content:
!    Automatically Offloaded SGEMM Example Program Text
!******************************************************************************/

/* System headers */
#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <stdint.h>
#include "mkl.h"
#include <sys/time.h>

double dtime()
{ 
     double tseconds = 0.0;
     struct timeval mytime;
     gettimeofday(&mytime,(struct timezone*)0);
     tseconds = (double)(mytime.tv_sec + mytime.tv_usec*1.0e-6);
     return( tseconds );
}

int main(int argc, char **argv)
{
	double *A, *B, *C; /* Matrices */
        double tstart, tstop;
	double workdivision;
        int nThreads=atoi(argv[1]); 
        //omp_set_num_threads(nThreads);
        mkl_set_num_threads(nThreads);

	MKL_INT M = 50000; /* Matrix dimensions */
	MKL_INT N = 50000; /* Matrix dimensions */
	MKL_INT K = 50000; /* Matrix dimensions */
	int matrix_bytes; /* Matrix size in bytes */
	int matrix_elements; /* Matrix size in elements */

	double alpha = 1.0, beta = 0.0; /* Scaling factors */
	char transa = 'N', transb = 'N'; /* Transposition options */

	int i, j; /* Counters */

	matrix_elements = M * K;
	matrix_bytes = sizeof(double) * matrix_elements;
	/* Allocate the matrices */
	A = (double *) malloc(matrix_bytes);
	if (A == NULL) {
		printf("Could not allocate matrix A\n");
		return -1;
	}
	for (i = 0; i < matrix_elements; i++)
		A[i] = (double)i; 

	matrix_elements = M*N;
	matrix_bytes = sizeof(double) * matrix_elements;

	B = (double *) malloc(matrix_bytes);
	if (B == NULL) {
		printf("Could not allocate matrix B\n");
		return -1;
	}
	for (i = 0; i < matrix_elements; i++)
		B[i] = 1.0; 
	matrix_elements = K * N;
	matrix_bytes = sizeof(double) * matrix_elements;

	C = (double *)malloc(matrix_bytes);
	if (C == NULL) {
		printf("Could not allocate matrix C\n");
		return -1;
	}
	for (i = 0; i < matrix_elements; i++)
		C[i] = 0.0; 

	printf("Computing DGEMM on the host\n");
        double start=dtime();
	cblas_dgemm(CblasColMajor, CblasTrans, CblasNoTrans, K, N, M, alpha, A, M, B, M, beta, C, K);
        double end=dtime();
        printf("time %4.2f",end-start);
	/* Free the matrix memory */
	free(A); free(B); free(C);

	printf("Done\n");

    return 0;
}
