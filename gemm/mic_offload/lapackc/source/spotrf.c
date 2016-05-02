/*******************************************************************************
!  Copyright(C) 2001-2015 Intel Corporation. All Rights Reserved.
!  
!  The source code, information  and  material ("Material") contained herein is
!  owned  by Intel Corporation or its suppliers or licensors, and title to such
!  Material remains  with Intel Corporation  or its suppliers or licensors. The
!  Material  contains proprietary information  of  Intel or  its  suppliers and
!  licensors. The  Material is protected by worldwide copyright laws and treaty
!  provisions. No  part  of  the  Material  may  be  used,  copied, reproduced,
!  modified, published, uploaded, posted, transmitted, distributed or disclosed
!  in any way  without Intel's  prior  express written  permission. No  license
!  under  any patent, copyright  or  other intellectual property rights  in the
!  Material  is  granted  to  or  conferred  upon  you,  either  expressly,  by
!  implication, inducement,  estoppel or  otherwise.  Any  license  under  such
!  intellectual  property  rights must  be express  and  approved  by  Intel in
!  writing.
!  
!  *Third Party trademarks are the property of their respective owners.
!  
!  Unless otherwise  agreed  by Intel  in writing, you may not remove  or alter
!  this  notice or  any other notice embedded  in Materials by Intel or Intel's
!  suppliers or licensors in any way.
!
!*******************************************************************************
!  Content:
!    SPOTRF  Example Program Text
!******************************************************************************/

/* System headers */
#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <stdint.h>
#include "mkl.h"

int main(int argc, char **argv)
{
	float *A; /* Matrix */
	MKL_INT N; /* Matrix dimension */
	MKL_INT info; /* Information about execution */
	char uplo='U'; /* Store upper triangular part */

	int matrix_bytes; /* Matrix size in bytes */
	int matrix_elements; /* Matrix size in elements */

	int i, j; /* Counters */
	int Rcond; /* Condition number of A */

	/* Check command line arguments */
	if (argc != 2) {
		printf("\nUsage: %s <N>\nDefaulting to N = 10\n\n", argv[0]);
		N = 10;
	} else {
		/* Parse command line arguments */
		N = atoi(argv[1]);
	}
	if (N <= 2) {
		printf("Invalid matrix size\n");
		return -1;
	}


	matrix_elements = N * N;
	matrix_bytes = sizeof(float) * matrix_elements;

	/* Allocate the matrices */
	A = malloc(matrix_bytes);
	if (A == NULL) {
		printf("Could not allocate matrix A\n");
		return -1;
	}

	/* Generate condition number of A */
	Rcond = 1.0 + rand() % 1000;

	/* Initialize the matrices */
	for (i = 0; i < matrix_elements; i++) {
		A[i]  = rand() % 100;
		A[i] = ( 2.0*A[i] - 1.0) *
		    (Rcond - 1.0)/((Rcond + 1.0) * N*1000);
		}

	for (i = 0; i < N; i++)
		A[i + i * N] = 1.0 - A[i + i * N];


#pragma offload target(mic) \
	in(uplo, N, info) \
	in(A:length(matrix_elements)) \
	out(A:length(matrix_elements) alloc_if(0))
	{
		spotrf(&uplo, &N, A, &N, &info);
	}

	/* Display the result */
	printf("The Cholesky factor U  \n");
	for (i = 0; i < N; i++) {
		for (j = 0; j < N; j++)
		    if  (j >= i)
			printf("%7.3f ", A[j + i * N]);
		    else
			printf(" 0.000");
		printf("\n");
	}

	/* Free the matrix memory */
	free(A);

    return 0;
}

