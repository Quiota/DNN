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
! Content:
!       MKL Gaussian RNG interface example program (C-interface)
!
!       Double precision Gaussian Random Number Generation
!
!******************************************************************************/

#include <stdio.h>
#include <math.h>

#include "mkl_vsl.h"
#include "errcheck.inc"

#define SEED    1
#define BRNG    VSL_BRNG_MT2203
#define METHOD  VSL_RNG_METHOD_GAUSSIAN_BOXMULLER2
#define N       1000
#define NN      10

main()
{
    double r[N];
    VSLStreamStatePtr stream;
    int i, errcode;
    int brng = BRNG;
    int seed = SEED;
    int nn   = N;
    int method = METHOD;

    double a=0.0;
    double sigma = 1.0;

    double tM,tD,tQ,tD2;
    double sM,sD;
    double sum, sum2;
    double n,s;
    double DeltaM,DeltaD;

    #pragma offload target(mic) \
    in(brng, seed, method, nn, a, sigma) out(r) nocopy(stream)
    {
        /***** Initialize *****/
        errcode = vslNewStream( &stream, brng, seed );
        CheckVslError( errcode );

        /***** Call RNG *****/
        errcode = vdRngGaussian( method, stream, nn, r, a, sigma );
        CheckVslError( errcode );

        /***** Deinitialize *****/
        errcode = vslDeleteStream( &stream );
        CheckVslError( errcode );
    }

    /***** Compute theoretical moments *****/
    tM=a;
    tD=sigma*sigma;
    tQ=720.0*sigma*sigma*sigma*sigma;

    /***** Compute sample moments *****/
    sum=0.0;
    sum2=0.0;
    for(i=0;i<N;i++) {
      sum+=(double)r[i];
      sum2+=(double)r[i]*(double)r[i];
    }
    sM=sum/((double)N);
    sD=sum2/(double)N-(sM*sM);

    /***** Compare theoretical and sample moments *****/
    n=(double)N;
    tD2=tD*tD;
    s=((tQ-tD2)/n)-(2*(tQ-2*tD2)/(n*n))+((tQ-3*tD2)/(n*n*n));

    DeltaM=(tM-sM)/sqrt(tD/n);
    DeltaD=(tD-sD)/sqrt(s);

    /***** Print results *****/
    printf("Sample of vdRngGaussian.\n");
    printf("------------------------\n\n");
    printf("Parameters:\n");
    printf("    a=%.4f\n",a);
    printf("    sigma=%.4f\n\n",sigma);

    printf("Results (first 10 of 1000):\n");
    printf("---------------------------\n");
    for(i=0;i<NN;i++) {
        printf("r[%d]=%.4f\n",i,r[i]);
    }

    printf("\n");
    if(DeltaM>3.0 || DeltaD>3.0) {
      printf("Error: sample moments (mean=%.2f, variance=%.2f) disagree with theory (mean=%.2f, variance=%.2f).\n",sM,sD,tM,tD);
      return 1;
    }
    else {
      printf("Sample moments (mean=%.2f, variance=%.2f) agree with theory (mean=%.2f, variance=%.2f).\n",sM,sD,tM,tD);
    }

    return 0;
}
