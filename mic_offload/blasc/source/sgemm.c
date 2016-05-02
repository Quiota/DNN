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
!    SGEMM Example Program Text
!******************************************************************************/

/* System headers */
#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <stdint.h>

/* MKL header */

#include "mkl.h"

int main(int argc, char **argv)
{
	float *A, *B, *C; /* Matrices */

	MKL_INT N=5, NP; /* Matrix dimensions */
	int matrix_bytes; /* Matrix size in bytes */
	int matrix_elements; /* Matrix size in elements */

	float alpha = 1.0, beta = 1.0; /* Scaling factors */
	char transa = 'N', transb = 'N'; /* Transposition options */

	int i, j; /* Counters */

	/* Check command line arguments */
	if (argc < 2) {
		printf("\nUsage: %s <N>\n\n", argv[0]);
	} else {
	/* Parse command line arguments */
		N = atoi(argv[1]);
	}

	if (N <= 0) {
		printf("Invalid matrix size\n");
		return -1;
	}

	printf("\nMatrix dimension is being set to %d \n\n", (int)N);

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

#pragma offload target(mic) \
	in(transa, transb, N, alpha, beta) \
	in(A:length(matrix_elements)) \
	in(B:length(matrix_elements)) \
	in(C:length(matrix_elements)) \
	out(C:length(matrix_elements) alloc_if(0))
	{
		sgemm(&transa, &transb, &N, &N, &N, &alpha, A, &N, B, &N,
				&beta, C, &N);
	}

	/* Display the result */
	printf("Resulting matrix C:\n");
	if (N>10) {
		printf("NOTE: C is too large, so print only its upper-left 10x10 block...\n");
		NP=10;
	} else {
		NP=N;
	}
	printf("\n");
	for (i = 0; i < NP; i++) {
		for (j = 0; j < NP; j++)
			printf("%7.3f ", C[i + j * N]);
		printf("\n");
	}

	/* Free the matrix memory */
	free(A); free(B); free(C);

	return 0;
}

