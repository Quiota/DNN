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
*   Content : TR Solver C example
*
********************************************************************************
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "mkl_rci.h"
#include "mkl_types.h"
#include "mkl_service.h"

/*MIC Memory Management macros */
#define ALLOC   alloc_if(1)
#define FREE    free_if(1)
#define RETAIN  free_if(0)
#define REUSE   alloc_if(0)

/* nonlinear least square problem with boundary constraints */
int main ()
{
    /* The number of the MIC card to use */
    __declspec(target(mic)) int micno = 0;

    /* user’s objective function */
    extern void extendet_powell (MKL_INT *, MKL_INT *, double *, double *);
    /* n - number of function variables
       m - dimension of function value 
       res - informs about the task completion */
    MKL_INT n = 4, m = 4, res = -1;
    /* precisions for stop-criteria (see manual for more detailes) */
    double *eps = NULL;
    /* solution vector. contains values x for f(x) */
    double *x = NULL;
    /* iter1 - maximum number of iterations
       iter2 - maximum number of iterations of calculation of trial-step */
    MKL_INT iter1 = 1000, iter2 = 100;
    /* initial step bound */
    double rs = 0.0;
    /* reverse communication interface parameter */
    MKL_INT RCI_Request;
    /* controls of rci cycle */
    MKL_INT successful;
    /* function (f(x)) value vector */
    double *fvec = NULL;
    /* jacobi matrix */
    double *fjac = NULL;
    /* lower and upper bounds */
    double *LW = NULL, *UP = NULL; 
    /* number of iterations */
    MKL_INT iter;
    /* number of stop-criterion */
    MKL_INT st_cr;
    /* initial and final residuals */
    double r1 , r2 ;
    /* TR solver handle */
    static __declspec(target(mic)) _TRNSPBC_HANDLE_t handle;
    /* cycle’s counter */
    MKL_INT i;
    /* results of input parameter checking */
    MKL_INT *info = NULL;
    /* memory allocation flag */
    MKL_INT mem_error, error;

	error = 0;
    /* memory allocation */
    mem_error = 1;
    eps = (double *) malloc (sizeof (double) * 6);
    if (eps == NULL) goto end;
    x = (double *) malloc (sizeof (double) * n);
    if (x == NULL) goto end;
    fvec = (double *) malloc (sizeof (double) * m);
    if (fvec == NULL) goto end;
    fjac = (double *) malloc (sizeof (double) * m * n);
    if (fjac == NULL) goto end;
    LW = (double *) malloc (sizeof (double) * n);
    if (LW == NULL) goto end;
    UP = (double *) malloc (sizeof (double) * n);
    if (UP == NULL) goto end; 
    info = (MKL_INT *) malloc (sizeof (MKL_INT) * 6);
    if (info == NULL) goto end;
    /* memory allocated correctly */
    mem_error = 0;

    /* set precisions for stop-criteria */
    for (i = 0; i < 6; i++)
    {
        eps[i] = 0.00001;
    }
    /* set the initial guess */
    for (i = 0; i < n / 4; i++)
    {
        x[4 * i] = 3.0;
        x[4 * i + 1] = -1.0;
        x[4 * i + 2] = 0.0;
        x[4 * i + 3] = 1.0;
    }
    /* set initial values */
    for (i = 0; i < m; i++)
        fvec[i] = 0.0;
    for (i = 0; i < m * n; i++)
        fjac[i] = 0.0;
    /* set bounds */
    for (i = 0; i < n / 4; i++)
    {
        LW[4 * i] = 0.1;
        LW[4 * i + 1] = -20.0;
        LW[4 * i + 2] = -1.0;
        LW[4 * i + 3] = -1.0;
        UP[4 * i] = 100.0;
        UP[4 * i + 1] = 20.0;
        UP[4 * i + 2] = 1.0;
        UP[4 * i + 3] = 50.0;
    }
    /* initialize solver (allocate mamory, set initial values)
       handle       in/out: TR solver handle
       n       in:     number of function variables
       m       in:     dimension of function value
       x       in:     solution vector. contains values x for f(x)
       LW           in:             lower bound
       UP           in:             upper bound
       eps     in:     precisions for stop-criteria
       iter1   in:     maximum number of iterations
       iter2   in:     maximum number of iterations of calculation of trial-step
       rs      in:     initial step bound */
    #pragma offload target(mic:micno) \
    nocopy(handle) \
    in(n, m) \
    in(x:length(n) RETAIN) \
    in(LW:length(n) RETAIN) \
    in(UP:length(n) RETAIN) \
    in(eps:length(6) RETAIN) \
    in(iter1, iter2) \
    in(rs) \
    out(res)
    {
        res = dtrnlspbc_init (&handle, &n, &m, x, LW, UP, eps, &iter1, &iter2, &rs);
    }
    if (res != TR_SUCCESS)
    {
        /* if function does not complete successfully then print error message */
        printf ("| error in dtrnlspbc_init\n");
        /* Release internal MKL memory that might be used for computations.         */
        /* NOTE: It is important to call the routine below to avoid memory leaks   */
        /* unless you disable MKL Memory Manager                                   */
        MKL_Free_Buffers ();
        /* and exit */
		error = 1;
        goto end;
    }
    /* Checks the correctness of handle and arrays containing Jacobian matrix, 
       objective function, lower and upper bounds, and stopping criteria. */
    #pragma offload target(mic:micno) \
    nocopy(handle) \
    in(n, m) \
    in(fjac:length(m * n) RETAIN) \
    in(fvec:length(m) RETAIN) \
    nocopy(LW:length(n) REUSE RETAIN) \
    nocopy(UP:length(n) REUSE RETAIN) \
    nocopy(eps:length(6) REUSE RETAIN) \
    inout(info:length(6)) \
    out(res)
    {
        res = dtrnlspbc_check (handle, &n, &m, fjac, fvec, LW, UP, eps, info);
    }
    if (res != TR_SUCCESS)
    {
        /* if function does not complete successfully then print error message */
        printf ("| error in dtrnlspbc_init\n");
        /* Release internal MKL memory that might be used for computations.         */
        /* NOTE: It is important to call the routine below to avoid memory leaks   */
        /* unless you disable MKL Memory Manager                                   */
        MKL_Free_Buffers ();
        /* and exit */
		error = 1;
        goto end;

    }
    else
    {
        if (info[0] != 0 || // The handle is not valid.
            info[1] != 0 || // The fjac array is not valid.
            info[2] != 0 || // The fvec array is not valid.
            info[3] != 0 || // The LW array is not valid.
            info[4] != 0 || // The UP array is not valid.
            info[5] != 0    // The eps array is not valid.
           )
        {
            printf ("| input parameters for dtrnlspbc_solve are not valid\n");
            /* Release internal MKL memory that might be used for computations.         */
            /* NOTE: It is important to call the routine below to avoid memory leaks   */
            /* unless you disable MKL Memory Manager                                   */
            MKL_Free_Buffers ();
            /* and exit */
			error = 1;
            goto end;
        }
    }

    /* set initial rci cycle variables */
    RCI_Request = 0;
    successful = 0;
    /* rci cycle */
    while (successful == 0)
    {
        /* call tr solver
           handle               in/out: tr solver handle
           fvec         in:     vector
           fjac         in:     jacobi matrix
           RCI_request in/out:  return number which denote next step for performing */
        #pragma offload target(mic:micno) \
        nocopy(handle) \
        nocopy(fjac:length(m * n) REUSE RETAIN) \
        inout(fvec:length(m) REUSE RETAIN) \
        inout(RCI_Request) \
        out(res)
        {
            res =dtrnlspbc_solve (&handle, fvec, fjac, &RCI_Request); 
        }
        if (res != TR_SUCCESS)
        {
            /* if function does not complete successfully then print error message */
            printf ("| error in dtrnlspbc_solve\n");
            /* Release internal MKL memory that might be used for computations.         */
            /* NOTE: It is important to call the routine below to avoid memory leaks   */
            /* unless you disable MKL Memory Manager                                   */
            MKL_Free_Buffers ();
            /* and exit */
			error = 1;
            goto end;
        }
        /* according with rci_request value we do next step */
        if (RCI_Request == -1 ||
            RCI_Request == -2 ||
            RCI_Request == -3 ||
            RCI_Request == -4 || RCI_Request == -5 || RCI_Request == -6)
            /* exit rci cycle */
            successful = 1;
        if (RCI_Request == 1)
        {
            /* recalculate function value
               m            in:     dimension of function value
               n            in:     number of function variables
               x            in:     solution vector
               fvec    out:    function value f(x) */
            #pragma offload target(mic:micno) \
            in(n, m) \
            nocopy(x:length(n) REUSE RETAIN) \
            nocopy(fvec:length(m) REUSE RETAIN) 
            {
            extendet_powell (&m, &n, x, fvec);
            }
        }
        if (RCI_Request == 2)
        {
            /* compute jacobi matrix
               extendet_powell      in:     external objective function
               n               in:     number of function variables
               m               in:     dimension of function value
               fjac            out:    jacobi matrix
               x               in:     solution vector
               jac_eps         in:     jacobi calculation precision */
            #pragma offload target(mic:micno) \
            in(n, m) \
            nocopy(fjac:length(m * n) REUSE RETAIN) \
            nocopy(fvec:length(m) REUSE RETAIN) \
            nocopy(x:length(n) REUSE RETAIN) \
            nocopy(eps:length(6) REUSE RETAIN) \
            out(res)
            {
                res = djacobi (extendet_powell, &n, &m, fjac, x, eps);
            }
            if (res != TR_SUCCESS)
            {
                /* if function does not complete successfully then print error message */
                printf ("| error in djacobi\n");
                /* Release internal MKL memory that might be used for computations.         */
                /* NOTE: It is important to call the routine below to avoid memory leaks   */
                /* unless you disable MKL Memory Manager                                   */
                MKL_Free_Buffers ();
                /* and exit */
				error = 1;
                goto end;
            }
        }
    }
    /* get solution statuses
       handle            in:        TR solver handle
       iter              out:       number of iterations
       st_cr             out:       number of stop criterion
       r1                out:       initial residuals
       r2                out:       final residuals */
       #pragma offload target(mic:micno) \
       nocopy(handle) \
       in(n, m) \
       inout(iter, st_cr, r1, r2) \
       out(res)
       {
           res = dtrnlspbc_get (&handle, &iter, &st_cr, &r1, &r2);
       }
       if (res != TR_SUCCESS)
       {
               /* if function does not complete successfully then print error message */
            printf ("| error in dtrnlspbc_get\n");
            /* Release internal MKL memory that might be used for computations.         */
            /* NOTE: It is important to call the routine below to avoid memory leaks   */
            /* unless you disable MKL Memory Manager                                   */
            MKL_Free_Buffers ();
            /* and exit */
			error = 1;
            goto end;
        }
       /* free handle memory */
        #pragma offload target(mic:micno) \
        nocopy(handle) \
        out(res)
       {
           res = dtrnlspbc_delete (&handle);
       }
        if (res != TR_SUCCESS)
        {
            /* if function does not complete successfully then print error message */
            printf ("| error in dtrnlspbc_delete\n");
            /* Release internal MKL memory that might be used for computations.         */
            /* NOTE: It is important to call the routine below to avoid memory leaks   */
            /* unless you disable MKL Memory Manager                                   */
            MKL_Free_Buffers ();
            /* and exit */
			error = 1;
            goto end;
        }
    /* free allocated memory */
end:
    free (info);
    free (UP);
    free (LW);
    free (fjac);
    free (fvec);
    free (x);
    free (eps);
	if(error != 0)
	{
		return 1;
	}
    if (mem_error == 1) 
    {
        printf ("| insufficient memory \n");
        return 1;
    }
    /* Release internal MKL memory that might be used for computations.         */
    /* NOTE: It is important to call the routine below to avoid memory leaks   */
    /* unless you disable MKL Memory Manager                                   */
    MKL_Free_Buffers ();
    /* if final residual less then required precision then print pass */
    if (r2 < 0.1)
    {
        printf ("|         dtrnlspbc powell..........PASS\n");
        return 0;
    }
    else
    {
        printf ("|         dtrnlspbc powell..........FAILED\n");
        return 1;
    }
}

/* nonlinear system equations without constraints */
/* routine for extendet powell function calculation
   m     in:     dimension of function value
   n     in:     number of function variables
   x     in:     vector for function calculating
   f     out:    function value f(x) */
__declspec(target(mic))
void extendet_powell (MKL_INT * m, MKL_INT * n, double *x, double *f)
{
    MKL_INT i;
    for (i = 0; i < (*n) / 4; i++)
    {
        f[4 * i] = x[4 * i] + 10.0 * x[4 * i + 1];
        f[4 * i + 1] = 2.2360679774998 * (x[4 * i + 2] - x[4 * i + 3]);
        f[4 * i + 2] =
        (x[4 * i + 1] - 2.0 * x[4 * i + 2]) * (x[4 * i + 1] -
                                               2.0 * x[4 * i + 2]);
        f[4 * i + 3] =
        3.1622776601684 * (x[4 * i] - x[4 * i + 3]) * (x[4 * i] -
                                                       x[4 * i + 3]);
    }
    return;
}
