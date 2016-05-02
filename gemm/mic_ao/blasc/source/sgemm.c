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

int main(int argc, char **argv)
{
	float *A, *B, *C; /* Matrices */
	double workdivision;

	MKL_INT N = 2560; /* Matrix dimensions */
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
	sgemm(&transa, &transb, &N, &N, &N, &alpha, A, &N, B, &N,
			&beta, C, &N);

	printf("Enabling Automatic Offload\n");
	/* Alternatively, set environment variable MKL_MIC_ENABLE=1 */
	if (mkl_mic_enable() != 0)
	{
		printf("Could not enable Automatic Offload (no MIC devices?). "
				"Exiting.\n");
		return -1;
	}
	else
	{
		int ndevices = mkl_mic_get_device_count(); /* Number of MIC devices */

		printf("Automatic Offload enabled: %d MIC devices present\n\n",
				mkl_mic_get_device_count());

		printf("Computing SGEMM with automatic workdivision\n\n");
		sgemm(&transa, &transb, &N, &N, &N, &alpha, A, &N, B, &N,
				&beta, C, &N);

		for (i = 0; i < ndevices; i++)
		{
			/* Alternativelly, set environment variable
			 * MKL_MIC<i>_WORKDIVISION=1.0 */
			printf("Setting workdivision for device MIC:%02d to 1.0\n", i);
			mkl_mic_set_workdivision(MKL_TARGET_MIC, i, 1.0);
			printf("Resulting workdivision configuration:\n");
			mkl_mic_get_workdivision(MKL_TARGET_HOST, 0, &workdivision);
			printf("\tworkdivision[HOST] = %+4.2f\n", workdivision);
			for (j = 0; j < ndevices; j++)
			{
				mkl_mic_get_workdivision(MKL_TARGET_MIC, j, &workdivision);
				printf("\tworkdivision[MIC:%02d] = %+4.2f\n", j, workdivision);
			}

			printf("Computing SGEMM on device %02d\n\n", i);
			sgemm(&transa, &transb, &N, &N, &N, &alpha, A, &N, B, &N,
					&beta, C, &N);
		}
	}

	/* Free the matrix memory */
	free(A); free(B); free(C);

	printf("Done\n");

    return 0;
}
