/*
********************************************************************************
*   Copyright(C) 2004-2015 Intel Corporation. All Rights Reserved.
*   
*   The source code, information  and  material ("Material") contained herein is
*   owned  by Intel Corporation or its suppliers or licensors, and title to such
*   Material remains  with Intel Corporation  or its suppliers or licensors. The
*   Material  contains proprietary information  of  Intel or  its  suppliers and
*   licensors. The  Material is protected by worldwide copyright laws and treaty
*   provisions. No  part  of  the  Material  may  be  used,  copied, reproduced,
*   modified, published, uploaded, posted, transmitted, distributed or disclosed
*   in any way  without Intel's  prior  express written  permission. No  license
*   under  any patent, copyright  or  other intellectual property rights  in the
*   Material  is  granted  to  or  conferred  upon  you,  either  expressly,  by
*   implication, inducement,  estoppel or  otherwise.  Any  license  under  such
*   intellectual  property  rights must  be express  and  approved  by  Intel in
*   writing.
*   
*   *Third Party trademarks are the property of their respective owners.
*   
*   Unless otherwise  agreed  by Intel  in writing, you may not remove  or alter
*   this  notice or  any other notice embedded  in Materials by Intel or Intel's
*   suppliers or licensors in any way.
*
********************************************************************************
*   Content : MKL PARDISO C example
*
********************************************************************************
*/
/* -------------------------------------------------------------------- */
/* Example program to show the use of the PARDISO routine */
/* on symmetric linear systems */
/* -------------------------------------------------------------------- */
/* This program is based on the example available for download from the following site: */
/* www.pardiso-project.org */
/* */
/* (C) Olaf Schenk, Department of Computer Science, */
/* University of Basel, Switzerland. */
/* Email: olaf.schenk@unibas.ch */
/* -------------------------------------------------------------------- */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "mkl_pardiso.h"
#include "mkl_types.h"

/* Matrix data */
MKL_INT ia[ 6] = { 1, 4, 6, 9, 12, 14 };
MKL_INT ja[13] = { 1, 2, 4,
	1, 2,
	3, 4, 5,
	1, 3, 4,
	2, 5 };
double a[18] = { 1.0, -1.0, -3.0,
	-2.0, 5.0,
	4.0, 6.0, 4.0,
	-4.0, 2.0, 7.0,
	8.0, -5.0 };

int main( void ) {
	MKL_INT n = 5;
	MKL_INT *p_ia, *p_ja;
	double *p_a;
	MKL_INT mtype = 11; /* Real unsymmetric matrix */
	/* RHS and solution vectors */
	double *p_b, *p_x;
	MKL_INT nrhs = 1; /* Number of right hand sides */
	/* Internal solver memory pointer pt, */
	/* 32-bit: int pt[64]; 64-bit: long int pt[64] */
	/* or void *pt[64] should be acceptable on both architectures */
	void **pt;
	/* PARDISO control parameters */
	MKL_INT *iparm;
	MKL_INT maxfct, mnum, phase, error, msglvl;
	/* Auxiliary variables */
	int i;

	pt = malloc(sizeof(void*) * 64);
	if(pt == NULL)
	{
		printf("Cannot allocate pt array\n");
		exit(1);
	}
	iparm = malloc(sizeof(MKL_INT) * 64);
	if(iparm == NULL)
	{
		printf("Cannot allocate iparm array\n");
		exit(1);
	}
	memset(pt, 0, sizeof(void*) * 64);

	p_ia = malloc(sizeof(ia));
	p_ja = malloc(sizeof(ja));
	p_a  = malloc(sizeof(a));
	p_x  = malloc(sizeof(double) * n);
	p_b  = malloc(sizeof(double) * n);
	if(p_ia == NULL || p_ja == NULL || p_a == NULL || p_x == NULL || p_b == NULL)
	{
		printf("Cannot allocate one of the data arrays\n");
		exit(1);
	}
	memcpy(p_ia, ia, sizeof(ia));
	memcpy(p_ja, ja, sizeof(ja));
	memcpy( p_a,  a, sizeof(a));
/* -------------------------------------------------------------------- */
/* .. Set up PARDISO control parameters */
/* -------------------------------------------------------------------- */
	for (i = 0; i < 64; i++) {
		iparm[i] = 0;
	}
	iparm[0] = 1; /* No solver default */
	iparm[1] = 2; /* Fill-in reordering from METIS */
	/* Numbers of processors, value of OMP_NUM_THREADS */
	iparm[2] = 1;
	iparm[3] = 0; /* No iterative-direct algorithm */
	iparm[4] = 0; /* No user fill-in reducing permutation */
	iparm[5] = 0; /* Write solution into x */
	iparm[6] = 0; /* Not in use */
	iparm[7] = 2; /* Max numbers of iterative refinement steps */
	iparm[8] = 0; /* Not in use */
	iparm[9] = 13; /* Perturb the pivot elements with 1E-13 */
	iparm[10] = 1; /* Use nonsymmetric permutation and scaling MPS */
	iparm[11] = 0; /* Not in use */
	iparm[12] = 1; /* Maximum weighted matching algorithm is switched on 
			  (default for non-symmetric) */
	iparm[13] = 0; /* Output: Number of perturbed pivots */
	iparm[14] = 0; /* Not in use */
	iparm[15] = 0; /* Not in use */
	iparm[16] = 0; /* Not in use */
	iparm[17] = -1; /* Output: Number of nonzeros in the factor LU */
	iparm[18] = -1; /* Output: Mflops for LU factorization */
	iparm[19] = 0; /* Output: Numbers of CG Iterations */
	maxfct = 1; /* Maximum number of numerical factorizations */
	mnum = 1; /* Which factorization to use */
	msglvl = 1; /* Print statistical information in file */
	error = 0; /* Initialize error flag */
/* -------------------------------------------------------------------- */
/* .. Initialize the internal solver memory pointer; this is only */
/* necessary for the FIRST call of the PARDISO solver. */
/* -------------------------------------------------------------------- */
	for (i = 0; i < 64; i++) {
		pt[i] = 0;
	}
	/* Set right hand side to one */
	for (i = 0; i < n; i++) {
		p_b[i] = 1;
		p_x[i] = 0;
	}
/* -------------------------------------------------------------------- */
/* .. Running reordering and symbolic factorization, numerical factorization */
/* and back substitution and iterative refinement */
/* -------------------------------------------------------------------- */
	phase = 13;
#pragma offload target(mic) \
	in(maxfct, mnum, mtype, phase) \
	in(n) \
	in(nrhs) \
	in(msglvl) \
	out(error) \
	in(pt:length(64)) \
	in(p_a:length(18)) \
	in(p_ia:length(6)) \
	in(p_ja:length(13)) \
	in(iparm:length(64)) \
	in(p_b:length(5)) \
	in(p_x:length(5)) \
	out(p_x:length(5) alloc_if(0)) 
	{
		MKL_INT idum; /* Dummy integer */
		pardiso (pt, &maxfct, &mnum, &mtype, &phase,
			&n, p_a, p_ia, p_ja, &idum, &nrhs,
			iparm, &msglvl, p_b, p_x, &error);
	}
	if (error != 0) {
		printf("\nERROR: %d", (int)error);
		exit(1);
	}
	printf("\nPARDISO completed ... ");
	printf("\nNumber of nonzeros in factors = %d", (int)iparm[17]);
	printf("\nNumber of factorization MFLOPS = %d", (int)iparm[18]);
	printf("\nThe solution of the system is: ");
	for (i = 0; i < n; i++) {
		printf("\n x [%d] = % f", i, p_x[i] );
	}
	printf ("\n");
/* -------------------------------------------------------------------- */
/* .. Terminate and release memory */
/* -------------------------------------------------------------------- */
	phase = -1; /* Release internal memory */
#pragma offload target(mic) \
	in(pt:length(64)) \
	in(maxfct, mnum, mtype, phase) \
	in(n) \
	in(p_ia:length(6)) \
	in(p_ja:length(13)) \
	in(nrhs) \
	in(iparm:length(64)) \
	in(msglvl) \
	out(error)
	{
		MKL_INT idum; /* Dummy integer */
		double ddum; /* Dummy float */
		pardiso (pt, &maxfct, &mnum, &mtype, &phase,
			&n, &ddum, p_ia, p_ja, &idum, &nrhs,
			iparm, &msglvl, &ddum, &ddum, &error);
	}
    free(p_b);
    free(p_x);
    free(p_a);
    free(p_ja);
    free(p_ia);
    free(iparm);
    free(pt);
	return 0;
}
