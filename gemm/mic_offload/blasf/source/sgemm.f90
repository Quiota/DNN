!*******************************************************************************
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
!    SGEMM Example Program Text
!*******************************************************************************

      PROGRAM MKL_SGEMM_HC 
      USE OMP_LIB
      USE MIC_LIB
      include 'mkl_blas.fi'

      INTEGER :: N = 5, NP, I, NARG, ERROR
      REAL, ALLOCATABLE, DIMENSION( :, : ) :: A, B, C
      CHARACTER(len=32) :: arg

      NARG = IARGC()

      IF (NARG .LT. 1) THEN
        CALL GETARG(0, arg)
        WRITE (*,*), 'Usage: ', TRIM(arg), ' <N>'
        WRITE (*,*)
      ELSE
        CALL GETARG(1, arg)
        READ(arg, '(I10)') N
      ENDIF

      IF (N .le. 0) THEN
        WRITE(*,*) 'Invalid matrix size'
        STOP
      ENDIF

      WRITE(*,*) 'Matrix dimension is being set to', N
      WRITE(*,*)

      ALLOCATE( A( N, N ), B( N, N ), C( N, N ), STAT=ERROR)

      IF (ERROR .ne. 0) THEN
        WRITE(*,*) 'Could not allocate matrix A, B or C'
        STOP
      ENDIF
 
      A = 1.0
      B = 2.0
      C = 0.0

      !DEC$ ATTRIBUTES OFFLOAD : MIC :: SGEMM
      !DEC$ OFFLOAD TARGET( MIC ) IN( N ), IN( A: LENGTH( N * N )), &
      !DEC$ IN( B: LENGTH( N * N )), INOUT( C: LENGTH( N * N ))
      CALL SGEMM( 'N', 'N', N, N, N, 1.0, A, N, B, N, 1.0, C, N )

      WRITE (*,*), 'Resulting matrix C:'
      IF (N .GT. 10) THEN
        WRITE(*,*) 'NOTE: C is too large, so print only its upper-left 10x10 block...'
        NP=10
      ELSE
        NP=N;
      ENDIF

      WRITE (*,*)

      DO I=1,NP
        WRITE(*,'(10(1X,F9.3))'), C(I,1:NP)
      ENDDO

      DEALLOCATE( A, B, C )
      END PROGRAM
