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
	float *A, *B, *C; /* Matrices */
        double tstart, tstop;
	double workdivision;
        
        if (argc!=4)
          return(-1);

        int size=atoi(argv[1]);
	int nThreads=atoi(argv[2]);
	int nIter=atoi(argv[3]);
        //omp_set_num_threads(nThreads);
        mkl_set_num_threads(nThreads);

	MKL_INT N = size; /* Matrix dimensions */
	MKL_INT LD = N; /* Leading dimension */
	int matrix_bytes; /* Matrix size in bytes */
	int matrix_elements; /* Matrix size in elements */

	float alpha = 1.0, beta = 1.0; /* Scaling factors */
	char transa = 'N', transb = 'N'; /* Transposition options */

	int i, j; /* Counters */

	matrix_elements = N * N;
	matrix_bytes = sizeof(float) * matrix_elements;

	/* Allocate the matrices */
	A = malloc(matrix_bytes);
	if (A == NULL) {
		printf("Could not allocate matrix A\n");
		return -1;
	}

	B = malloc(matrix_bytes);
	if (B == NULL) {
		printf("Could not allocate matrix B\n");
		return -1;
	}

	C = malloc(matrix_bytes);
	if (C == NULL) {
		printf("Could not allocate matrix C\n");
		return -1;
	}

	/* Initialize the matrices */
	for (i = 0; i < matrix_elements; i++) {
		A[i] = 1.0; B[i] = 2.0; C[i] = 0.0;
	}

	printf("Computing SGEMM on the host\n");
        tstart = dtime();
        for (i = 0; i < nIter; i++)
	  sgemm(&transa, &transb, &N, &N, &N, &alpha, A, &N, B, &N,
			&beta, C, &N);
        tstop = dtime();
        double avgtime = (tstop-tstart)/nIter;
        printf("Average Time: %10.4f (s)", avgtime);
        double gflops = (2.0e-9*N*N*N)/avgtime; 
        printf("GFlops/s: %10.4f /s", gflops);

	/* Free the matrix memory */
	free(A); free(B); free(C);

	printf("Done\n");

    return 0;
}
