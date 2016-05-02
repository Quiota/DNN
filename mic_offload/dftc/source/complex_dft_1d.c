/*****************************************************************************
! Copyright(C) 2011-2015 Intel Corporation. All Rights Reserved.
!
! The source code, information  and  material ("Material") contained herein is
! owned  by Intel Corporation or its suppliers or licensors, and title to such
! Material remains  with Intel Corporation  or its suppliers or licensors. The
! Material  contains proprietary information  of  Intel or  its  suppliers and
! licensors. The  Material is protected by worldwide copyright laws and treaty
! provisions. No  part  of  the  Material  may  be  used,  copied, reproduced,
! modified, published, uploaded, posted, transmitted, distributed or disclosed
! in any way  without Intel's  prior  express written  permission. No  license
! under  any patent, copyright  or  other intellectual property rights  in the
! Material  is  granted  to  or  conferred  upon  you,  either  expressly,  by
! implication, inducement,  estoppel or  otherwise.  Any  license  under  such
! intellectual  property  rights must  be express  and  approved  by  Intel in
! writing.
!
! *Third Party trademarks are the property of their respective owners.
!
! Unless otherwise  agreed  by Intel  in writing, you may not remove  or alter
! this  notice or  any other notice embedded  in Materials by Intel or Intel's
! suppliers or licensors in any way.
!
!*****************************************************************************
! Content:
!     An example of offloading computation of FFT to MIC
!
!****************************************************************************/

/* Define floating point precision: float or double */
#if !defined(REAL)
#define REAL float
#endif

/* Wrap standard headers by pragma offload_attribute */
#if __INTEL_OFFLOAD
#include <offload.h>
#else
#define _Offload_get_device_number() (-1)
#endif

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <float.h>
#include <complex.h>

#include "mkl.h"

/* Define convenience macros TARGET_MIC and PRAGMA_OFFLOAD */
#if __INTEL_OFFLOAD
#define TARGET_MIC           __declspec(target(mic))
#define PRAGMA_OFFLOAD(args) __pragma(offload args)
#else
#define TARGET_MIC           /*none*/
#define PRAGMA_OFFLOAD(args) /*none*/
#endif

/* The number of the MIC device to use */
TARGET_MIC int micno = 0;

/* Define COMPLEX type for convenience */
typedef REAL _Complex COMPLEX;

/* Two auxiliary functions */
void init(COMPLEX *x, int N, int H);
int  verify(COMPLEX *x, int N, int H);

/* Example of offloading FFT on card */
int demo_fft(int N)
{
    /* Execution status: Nonzero means failure */
    MKL_LONG status = 0;

    /* CAVEAT: current offload usage model doesn't handle
     * auto TARGET_MIC variables properly, so they are static.
     */

    /* Pointer to input/output data */
    static TARGET_MIC COMPLEX *x = 0;

    /* DFTI descriptor */
    static TARGET_MIC DFTI_DESCRIPTOR_HANDLE hand = 0;

    /* What to clean up before returning from demo_fft */
    int for_cleanup = 0;
    const int MIC_HAND = 1;
    const int MIC_X = 2;
    const int HOST_X = 4;

    //------------------------------------------------------------
    printf("\nPrepare DFTI descriptor for N=%i on the target\n", N);
    //------------------------------------------------------------
	/* Note: the offload pragma below requires offload region to be executed
	 * on Intel(R) Xeon Phi(TM) coprocessors if any; otherwise the offload region is executed
	 * on host. */
	
	/* Note: the offload pragma below defines offload target number explicitly 
	 * which makes it impossible for the offload region to be executed 
	 * on the host CPU if there are no Intel(R) Xeon Phi(TM) coprocessors available. 
	 * In such a case the program will exit with an error message. */
    PRAGMA_OFFLOAD(if(micno>=0) target(mic:micno) nocopy(hand) out(status) in(N))
    {
        status = DftiCreateDescriptor(&hand,
            sizeof(REAL)==sizeof(float) ? DFTI_SINGLE : DFTI_DOUBLE,
            DFTI_COMPLEX, 1, (MKL_LONG)N );
        if (0 == status)
        {
            status = DftiCommitDescriptor(hand);
        }
    }
	for_cleanup |= MIC_HAND;
    if (0 != status)
    {
        printf("Error: cannot create descriptor, status=%li (%s)\n",
            (long)status, DftiErrorMessage(status));
		goto cleanup;
    }

    //------------------------------------------------------------
    printf("Allocate space for data on the host.\n"
           "For best performance in offload mode align data on host to 4096 bytes\n"
           "and do not set any special aligment for data on target"
           " - it will be the same automatically\n");
    //------------------------------------------------------------
    x = (COMPLEX*)MKL_malloc( N * sizeof(COMPLEX), 4*1024 );
    if (0 == x)
    {
        printf("Error: no host memory for %li bytes\n", (long)(N*sizeof(COMPLEX)));
        status = -1;
        goto cleanup;
    }
    for_cleanup |= HOST_X;

    //------------------------------------------------------------
    printf("Preallocate buffers on the target\n");
    //------------------------------------------------------------
    PRAGMA_OFFLOAD(if(micno>=0) target(mic:micno) inout(x:length(N) alloc_if(1) free_if(0)))
    {
        // This might fail with COI_RESOURCE_EXHAUSTED error for large N,
        // which cannot be intercepted with current offload usage model
    }
    for_cleanup |= MIC_X;

    //------------------------------------------------------------
    // Now we can use the descriptor for computing
    // as many FFTs of size N as necessary
    //------------------------------------------------------------
    for (int i = 0; i < 3; ++i)
    {
        /* Pick an "arbitrary" harmonic to verify FFT */
        int H = ((i+1)*N) % 11;

        /* Initialize input for forward transform */
        init(x, N, H);

        //------------------------------------------------------------
        printf("Computation is performed on the target, H=%i\n",H);
        //------------------------------------------------------------
        PRAGMA_OFFLOAD(if(micno>=0) target(mic:micno) out(status) nocopy(hand)
            /* Reuse preallocated buffers by specifying alloc_if(0) free_if(0) */
            inout(x:length(N) alloc_if(0) free_if(0))
            in(N,H))
        {
            /* Compute just forward FFT */
            status = DftiComputeForward(hand, x);

            /* Other computation may be done here */
        }
        if (0 != status)
        {
            printf("Error: computing FFT failed, status=%li (%s)\n",
                (long)status, DftiErrorMessage(status));
            goto cleanup;
        }

        /* Verify the result */
        status = verify(x, N, H);
        if (0 != status)
        {
            printf("Error: incorrect result for N=%i, H=%i\n", N, H);
            goto cleanup;
        }
    }

 cleanup:

    //------------------------------------------------------------
    printf("Cleanup resources\n");
    //------------------------------------------------------------
    if (for_cleanup & MIC_HAND)
    {
        PRAGMA_OFFLOAD(if(micno>=0) target(mic:micno) nocopy(hand))
        {
            DftiFreeDescriptor(&hand);
        }
    }
    if (for_cleanup & MIC_X)
    {
        PRAGMA_OFFLOAD(if(micno>=0) target(mic:micno) nocopy(x:length(N) alloc_if(0) free_if(1)))
        {
        }
    }
    if (for_cleanup & HOST_X)
    {
        MKL_free(x);
    }

    printf("Test for N=%i %s\n", N, 0==status ? "passed" : "FAILED");
    return !!status;
}

char *default_argv[] = { "", "1024", "524288" };
int main(int argc, char *argv[])
{
    /* Discover the card to perform work on
     * and report MKL version
     */
    PRAGMA_OFFLOAD(optional target(mic) out(micno))
    {
        char version[DFTI_VERSION_LENGTH];
        micno = _Offload_get_device_number();
        if (micno >= 0) 
        {
            DftiGetValue(0, DFTI_VERSION, version);
            printf("MIC%i using %s\n", micno, version), fflush(0);
        }
    }
    
    if (micno < 0)
    {
		char version[DFTI_VERSION_LENGTH];
		DftiGetValue(0, DFTI_VERSION, version);
		printf("No MIC devices detected, examples will run on CPU (CPU will be both host and target) using %s\n", version), fflush(0);
	}

    if (argc < 2)
    {
        argv = default_argv;
        argc = sizeof(default_argv)/sizeof(default_argv[0]);
    }

    /* Run the demo for each FFT size given on command line */
    int failed = 0;
    for (int a = 1; a < argc; ++a)
    {
        int N = atoi(argv[a]);
        if (N < 0)
        {
            printf("FFT size should be a nonnegative value (got size = %d)\n", N);
            failed++;
        } else {
            failed += demo_fft(N);
        }
    }
    printf("\n%s\n", failed ? "Some tests FAILED" : "All tests passed");
    return failed;
}


/* Compute (K*L)%M accurately */
double moda(int K, int L, int M)
{
    return (double)(((long long)K * L) % M);
}

/* Initialize array with harmonic H */
void init(COMPLEX *x, int N, int H)
{
    double TWOPI = 6.2831853071795864769, phase;

    for (int n = 0; n < N; n++)
    {
        phase  = moda(n,H,N) / N;
        ((REAL*)&x[n])[0] = cos( TWOPI * phase ) / N;
        ((REAL*)&x[n])[1] = sin( TWOPI * phase ) / N;
    }
}

/* Verify that x has unit peak at H */
int verify(COMPLEX *x, int N, int H)
{
    double err, errthr, maxerr;
    int n;

    /*
     * Note: this simple error bound doesn't take into account input 
     * data error
     */
    const double EPSILON = sizeof(REAL)==sizeof(float)
        ? FLT_EPSILON : DBL_EPSILON;
    errthr = 5.0 * log( (double)N ) / log(2.0) * EPSILON;
    printf(" Verifying the result, errthr = %.3lg\n", errthr);

    maxerr = 0;
    for (n = 0; n < N; n++)
    {
        double re_exp = 0.0, im_exp = 0.0, re_got, im_got;

        if ((n-H)%N==0)
        {
            re_exp = 1;
        }

        re_got = ((REAL*)&x[n])[0];
        im_got = ((REAL*)&x[n])[1];
        err  = fabs(re_got - re_exp) + fabs(im_got - im_exp);
        if (err > maxerr) maxerr = err;
        if (!(err <= errthr))
        {
            printf(" x[%i]: ",n);
            printf(" expected (%.17g,%.17g), ",re_exp,im_exp);
            printf(" got (%.17g,%.17g), ",re_got,im_got);
            printf(" err %.3lg\n", err);
            return 100;
        }
    }
    printf(" Verified, maximum error was %.3lg\n", maxerr);
    return 0;
}
