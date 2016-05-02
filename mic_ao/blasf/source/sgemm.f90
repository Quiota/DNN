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
!    Automatically Offloaded SGEMM Example Program Text
!*******************************************************************************
program mkl_ao_sgemm
    include 'mkl_service.fi'
    include 'mkl_blas.fi'

    real, allocatable, dimension( :, : ) :: A, B, C
    integer :: N = 2560
    real :: alpha = 1.0, beta = 1.0
    character :: transa = 'N', transb = 'N'
    integer :: ndevices
    double precision :: wd = -2.0
    integer :: ret = -1

    allocate( A( N, N ), B( N, N ), C( N, N ) )

    A = 1.0
    B = 2.0
    C = 0.0

    write(*, '(A,/)') ' Computing SGEMM on the host'
    call sgemm( transa, transb, N, N, N, &
            alpha, A, N, B, N, beta, C, N )

    write(*, '(A)') ' Enabling Automatic Offload'

    ! Alternatively, set environment variable MKL_MIC_ENABLE=1
    if (mkl_mic_enable() /= 0) then
        write(*, '(A)') ' Could not enable Automatic Offload (no MIC &
            devices?). Exiting.'
        stop -1
    endif

    ndevices = mkl_mic_get_device_count()

    write(*, '(A,I2,A,/)'), ' Automatic Offload enabled:', ndevices, &
        ' MIC devices present'

    write(*, '(A,/)') ' Computing SGEMM with automatic workdivision'

    call sgemm( transa, transb, N, N, N, &
            alpha, A, N, B, N, beta, C, N )

    do i = 0, ndevices - 1
        ! Alternativelly, set environment variable
        ! MKL_MIC<i>_WORKDIVISION=1.0
        write(*, '(A,I2,A)') ' Setting workdivision for device MIC:',&
            i, ' to 1.0'

        ret = mkl_mic_set_workdivision(MKL_TARGET_MIC, i, 1d0)

        write(*, '(A)') ' Resulting workdivision configuration:'

        ret = mkl_mic_get_workdivision(MKL_TARGET_HOST, 0, wd)
        write(*, '(4X,A,I2,A,F4.1)') ' workdivision[HOST:',&
            0, '] = ', wd

        do j = 0, ndevices - 1
            ret = mkl_mic_get_workdivision(MKL_TARGET_MIC, j, wd)
            write(*, '(4X,A,I2,A,F4.1)') ' workdivision[MIC:',&
                j, '] = ', wd
        end do

        write(*, '(A,I2,/)') ' Computing SGEMM on device ', i

        call sgemm( transa, transb, N, N, N, &
            alpha, A, N, B, N, beta, C, N )
    end do

    deallocate( C, B, A )
    write(*, '(A)') ' Done'
end program

