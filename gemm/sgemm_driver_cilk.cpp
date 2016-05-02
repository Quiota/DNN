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
#include <sys/time.h>
#include <cilk/cilk.h>
#include <omp.h>

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
        double tstart, tstop;
	double workdivision;
        
        if (argc!=4)
          return(-1);

        int size=atoi(argv[1]);
	int nThreads=atoi(argv[2]);
	int nIter=atoi(argv[3]);
        omp_set_num_threads(nThreads);


	int i, j, k; /* Counters */

	int matrix_elements = size * size;

	double **A = new double*[size];
	for(int i = 0; i < size; i++) 
        	A[i] = new double[size];


	double **B = new double*[size];
	for(int i = 0; i < size; i++) 
        	B[i] = new double[size];

	double **C = new double*[size];
	for(int i = 0; i < size; i++) 
        	C[i] = new double[size];

	printf("Initializing matrices\n");
	/* Initialize the matrices */
	for (i = 0; i < size; i++) 
	  for (j = 0; j < size; j++){ 
		A[i][j] = 1.0; B[i][j] = 2.0; C[i][j] = 0.0;
	}
	

	printf("Computing mat-multiply in Cilk plus on the host\n");
        tstart = dtime();
        for (k = 0; k < nIter; k++)
          for (i = 0; i < size; i++)
	   for (j = 0; j < size; j++){
		C[i][j] = __sec_reduce_add(A[i][0:size]*B[0:size][j]);
		//printf("i = %d,  c = %g\n",i,c);
		//C[i*size + j] = __sec_reduce_add(A[i*size:(i+1)*size:1] * B[j:((size-1)*size +j):size]);
           }	
        tstop = dtime();
        double avgtime = (tstop-tstart)/nIter;
        printf("Average Time: %10.4f (s)", avgtime);
        double gflops = (2.0e-9*size*size*size)/avgtime; 
        printf("GFlops/s: %10.4f /s", gflops);

	/* Free the matrix memory */
        for(i = 0; i < size; i++) {
           delete [] A[i];
           delete [] B[i];
           delete [] C[i];
        }
	delete [] A;
	delete [] B;
	delete [] C;
	printf("Done\n");

    return 0;
}
