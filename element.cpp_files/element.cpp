/* This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
 // Author: Matthew Dixon, Souham Biswas
 // Description: This file implements a parallel abstraction which can be used by a developer to perform matrix operations without calling mkl operations.
 // Dependencies: Intel MKL BLAS
 // Revision: 1.0


#include "mkl.h"
#include "omp.h"
#include <iostream>
#include <sys/time.h>

//#define OMP_NUM_THREADS 256
#define AVX_SIZE 512
#define NUM_DOUBLES AVX_SIZE/sizeof(double)
//#define NUM_BLOCKS OMP_NUM_THREADS/NUM_DOUBLES


double dtime()
{
     double tuseconds = 0.0;
     struct timeval mytime;
     gettimeofday(&mytime,(struct timezone*)0);
     tuseconds = (double)(mytime.tv_sec * 1000000 + mytime.tv_usec);
     return tuseconds;
}



// Abstracts the implementation of matrix operations into a single class.
// Common matrix operations like multiplication, addition etc can be performed efficiently
// by declaring the 2D matrices as object instances of this class and directly using the
// mathematical operators on them.
// Example:
//      double k1[2][3] =
//      {
//          {3, 4, 2},
//          {9, 2, 6}
//      };
//      double k2[3][4] =
//      {
//          {1, 2, 3},
//          {4, 5, 6},
//          {7, 8, 9}
//      };
//      element A(2, 3, *k1);
//      element B(3, 4, *k2);
//      A *= B;
// In the example above, the matrix k1 instantiated as an element object A is multiplied to matrix k2 modelled
// as an element object B by simply using the '*=' operator.
class element
{
    // Number of rows in the 2D matrix.
    int M;

    // Number of columns in the 2D matrix.
    int N;



    // Pointer to the first element of the 2D matrix which is represented internally
    // in a 1D memory contiguous fashion.
    double *arr;

    public:

        double startT_dgemm, endT_dgemm;

        // Constructor accepting dimensions of the 2D matrix as arguments.
        // This initializes a blank matrix with the given dimensions.
        // Args:
        //      M_ - Number of Rows
        //      N_ - Number of Columns
        element(int M_, int N_)
        {
            init(M_, N_);
        }

        // Constructor accepting dimensions and the contents (in the form of a pointer to the element at index [0][0] of the input matrix)
        // as arguments. This copies elements of the input matrix in parallel (by row) into the class object.
        // Args:
        //      M_ - Number of Rows
        //      N_ - Number of Columns
        //      *inp - Memory address (pointer) of the element at index [0][0] of the input 2D matrix
        element(int M_, int N_, double *inp)
        {
            init(M_, N_);
            int idx;

            #pragma omp parallel
            for (int i = 0; i < M_; i++)
            {
                for (int j = 0; j < N_; j++)
                {
                    idx = i * N_ + j;
                    arr[idx] = *(inp + idx);
                }
            }
        }

        // Destructor which frees any memory associated with storing the 2D matrix in the class.
        ~element()
        {
            mkl_free(arr);
        }

        // TODO
        element operator+=(const double *c)
        {
            /*
            #pragma omp parallel
            for (int i = 0; i < NUM_BLOCKS; i++)
                for (int j = 0; j < BLOCK_SIZE; j++)
                    this[i * BLOCK_SIZE + j] += c[i * BLOCK_SIZE + j];
            */
        }

        // Gets the value at index (row, col) of the 2D matrix.
        // Args:
        //      row - Row index of the desired value
        //      col - Column index of the desired value
        double get(int row, int col)
        {
            return arr[row * M + col];
        }

        // Prints the elements of the 2D matrix in a pretty format.
        void print_elems()
        {
            for (int i = 0; i < M; i++)
            {
                for (int j = 0; j < N; j++)
                {
                    std::cout << get(i, j) << "[" << i << "," << j << "], ";
                }
                std::cout << std::endl;
            }
        }

        // Common class initialization code called by all the class constructors.
        // This methods initializes class variables storing the number of rows and columns.
        // and allocates memory to store the 2D matrix.
        // Args:
        //      M_ - Number of Rows
        //      N_ - Number of Columns
        void init(int M_, int N_)
        {
            M = M_;
            N = N_;
            arr = (double *) mkl_malloc(M_ * N_ * sizeof(double), 64);
        }

        // Overloaded method for matrix addition. This method adds 2 matrices elementwise in parallel
        // and overwrites the contents of the LHS with the result. It is analogous to the following code:
        //      int A = 5, B = 12;
        //      A += B;
        // In the code above, the variable A is a scalar which is incremented by the value in B.
        // This overload allows similar functionality for 2D matrices in the same fashion
        // where A & B would be element objects and not int.
        // Args:
        //      el - Reference to the element object being added
        element operator+=(element &el)
        {
            #pragma omp parallel
            for (int i = 0; i < M; i++)
            {
                for (int j = 0; j < N; j++)
                {
                    arr[i * N + j] += el.arr[i * N + j];
                }
            }
        }

        // Overloaded method for matrix multiplication. This method uses the cblas_dgemm function to perform
        // the matrix multiplication and overwrites the contents of the LHS with the result. It is analogous to the following code:
        //      int A = 5, B = 12;
        //      A *= B;
        // In the code above, the variable A is a scalar which is multiplied by the value in B.
        // This overload allows similar functionality for 2D matrices in the same fashion
        // where A & B would be element objects and not int.
        // Args:
        //      el - Reference to the element object being added
        element operator*=(element &el)
        {
            // Temporary variable to contain the matrix multiplication result.
            double *tmp = (double *) mkl_malloc(M * el.N * sizeof(double), 64);

            startT_dgemm = dtime();

            // Multiplying matrices using the cblas_dgemm function and storing the result in *tmp variable declared previously.
            cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, M, el.N, N, 1, arr, N, el.arr, el.N, 0, tmp, el.N);

            endT_dgemm = dtime();

            // Freeing memory previously allocated to variable containing original data.
            mkl_free(arr);

            // Setting original pointer to point to the result.
            arr = tmp;

            // Setting the class attribute containing the number of columns to the new number of columns in the result.
            N = el.N;
        }

};

int test_elem_and_test(int rowsA, int colsA, int colsB)
{
    int rowsB = colsA;
    double *a = (double *) malloc(rowsA * colsA * sizeof(double));
    double *b = (double *) malloc(rowsB * colsB * sizeof(double));
    int i, j;

    #pragma omp parallel
    for (i = 0; i < (rowsA * colsA); i++)
    {
        a[i] = 1;
    }

    #pragma omp parallel
    for (i = 0; i < (rowsB * colsB); i++)
    {
        b[i] = 1;
    }

    element A(rowsA, colsA, a);
    element B(rowsB, colsB, b);

    A *= B;

    for (i = 0; i < rowsA; i++)
    {
        for (j = 0; j < colsB; j++)
        {
            if (A.get(i, j) != colsA)
            {
                std::cout << "Incorrect result at [" << i << ", " << j << "] Aborting... :(" << std::endl;
                return 0;
            }
        }
    }
    std::cout << "Correct Results!! :D" << std::endl;
    return 1;
}

void profile_elem(int rowsA, int colsA, int colsB, double *operation_time, double *dgemm_time)
{
    int rowsB = colsA;
    double tStart, tEnd;
    double *a = (double *) malloc(rowsA * colsA * sizeof(double));
    double *b = (double *) malloc(rowsB * colsB * sizeof(double));
    int i, j;

    #pragma omp parallel
    for (i = 0; i < (rowsA * colsA); i++)
    {
        a[i] = 1;
    }

    #pragma omp parallel
    for (i = 0; i < (rowsB * colsB); i++)
    {
        b[i] = 1;
    }

    element A(rowsA, colsA, a);
    element B(rowsB, colsB, b);

    tStart = dtime();
    A *= B;
    tEnd = dtime();

    *dgemm_time = A.endT_dgemm - A.startT_dgemm;
    *operation_time = tEnd - tStart;
}

// objective: create a programming abstraction using OOP which hides the details of calling blas routines
// performance: trade off between performance and convenience of simple
void main(int argc, char* argv[])
{

    /*
    double k1[3][3] =
    {
        {1, 2, 3},
        {4, 5, 6},
        {7, 8, 9}
    };

    double k2[3][4] =
    {
        {1, 2, 3},
        {4, 5, 6},
        {7, 8, 9}
    };
    */

    /*
    int nThreads = atoi(argv[1]);
    omp_set_num_threads(nThreads);
    mkl_set_num_threads(nThreads);
    */

    int sz = atoi(argv[1]);
    double optime, dgtime;
    profile_elem(sz, sz, sz, &optime, &dgtime);
    std::cout << "Total Operation Time taken = " << optime / 1000 << " ms" << std::endl;
    std::cout << "DGEMM Operation Time taken = " << dgtime / 1000 << " ms" << std::endl;
    std::cout << "Time Difference = " << (optime - dgtime) / 1000 << " ms" << std::endl;
    /*
    double k1[2][3] =
    {
        {3, 4, 2},
        {9, 2, 6}
    };

    double k2[3][4] =
    {
        {13, 9, 7, 15},
        {8, 7, 4, 6},
        {6, 4, 0, 3}
    };

    element A(2, 3, *k1);

    element B(3, 4, *k2);

    A.print_elems();
    std::cout << std::endl;
    B.print_elems();
    std::cout << std:: endl;
    A *= B;
    A.print_elems();
    */
}
