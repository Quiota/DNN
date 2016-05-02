!*****************************************************************************
! Copyright(C) 2012-2015 Intel Corporation. All Rights Reserved.
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
!*****************************************************************************

program complex_dft_2d
  ! define precision
# define SINGLE_PRECISION 
# if defined (__INTEL_OFFLOAD)
    use MIC_LIB
# else
#   define Offload_get_device_number() (-1)
# endif

# if defined (SINGLE_PRECISION)
    ! Working precision is single precision
    integer, parameter :: WP = selected_real_kind(6,37)
# else
    ! Working precision is single precision
    integer, parameter :: WP = selected_real_kind(15,307)
# endif
  integer, parameter :: def_N1 = 32
  integer, parameter :: def_N2 = 48
  integer N1, N2

  ! variables for parsing command line
  integer status, status1, status2, arg_cnt, i
  character*6 c1, c2
   
  !DIR$ ATTRIBUTES OFFLOAD: MIC :: micno
  integer :: micno
  
  integer :: failed  
  !DIR$ OFFLOAD BEGIN optional target(mic) out(micno)
  micno = Offload_get_device_number()
  !DIR$ END OFFLOAD
  
  if (micno < 0) then
    print *,"No MIC devices detected, examples will run on CPU"
    print *,"(CPU will be both host and target)"
  else
    print '("MIC"I0" is using")', micno
  end if


  arg_cnt = command_argument_count ()
  if (arg_cnt < 2) arg_cnt = 1 ! Run default sizes

  status1 = 0
  status2 = 0
  status  = 0

  ! Run the demo for each FFT size !!!given on command line 
  failed = 0
  do i =1,arg_cnt,2 
    if (arg_cnt-i > 0) then 
      call get_command_argument (i  , c1, status = status1)
      call get_command_argument (i+1, c2, status = status2)
      status = status1 + status2
      read( c1, '(I6)', iostat = status1) N1
      read( c2, '(I6)', iostat = status2) N2
      status = status + status1 + status2
    end if
    if (arg_cnt-i < 1 .OR. status /= 0) then
      N1 = def_N1
      N2 = def_N2
      status1 = 0
      status2 = 0
      status  = 0
    end if
     
    ! Temporary workaround, MIC offload section get corrupted value of micno
    ! variable in subroutine demo_fft()
    !failed = failed + demo_fft(N1, N2)
    failed = failed + demo_fft(N1, N2, micno)
  end do
 
  if (failed .gt. 0) then
    print *, "Some tests FAILED" 
  else 
    print *,"All tests passed"
  end if
  call exit(failed)

contains

  ! Example of offloading FFT on target 
  integer function demo_fft(N1, N2, micno)
# if defined (SINGLE_PRECISION)
      use MKL_DFTI, forget => DFTI_SINGLE, DFTI_SINGLE => DFTI_SINGLE_R
#     define PROBLEM_PRECISION DFTI_SINGLE
# else
      use MKL_DFTI, forget => DFTI_DOUBLE, DFTI_DOUBLE => DFTI_DOUBLE_R
#     define PROBLEM_PRECISION DFTI_DOUBLE
# endif
    integer :: N1, N2, micno
    
    ! DFTI descriptor handle
    !DIR$ ATTRIBUTES OFFLOAD: MIC :: hand
    type(DFTI_DESCRIPTOR), POINTER :: hand
    
    ! input/output data 
    !DIR$ ATTRIBUTES OFFLOAD: MIC :: data_array
    complex(WP), allocatable  :: data_array(:,:)
    
    ! Execution status: Nonzero means failure 
    integer :: status
    integer :: i, H1, H2
    integer :: alloc_stat

    ! What to clean up before returning from demo_fft 
    integer for_cleanup
    integer, parameter :: MIC_HAND  = 1
    integer, parameter :: MIC_DATA  = 2
    integer, parameter :: HOST_DATA = 4
    
    hand => null()
    for_cleanup = 0
    status = 0
    
    !------------------------------------------------------------
    print *
    print '("Prepare DFTI descriptor for N1 = "I0", N2 = "I0" on the target")', N1, N2
    print *
    !------------------------------------------------------------
    ! Note: the offload pragma below requires offload region to be executed
    !       on Intel(R) Xeon Phi(TM) coprocessors if any; otherwise the offload region
    !       is executed on host. 
    !DIR$ OFFLOAD BEGIN if(micno>=0) target(mic:micno) nocopy(hand) out(status) in(N1, N2) 
      status = DftiCreateDescriptor(hand, PROBLEM_PRECISION, DFTI_COMPLEX, 2, [N1, N2])
      if (0 == status) status = DftiCommitDescriptor(hand)
    !DIR$ END OFFLOAD  
    for_cleanup = IOR(for_cleanup, MIC_HAND)
    if (0 /= status) then
      print '("Error: cannot create descriptor, status="I0)', status
      print *,DftiErrorMessage(status)
      goto 999
    end if
    
    !--------------------------------------------------------------
    print '("Allocate space for data on the host")'
    print *
    !--------------------------------------------------------------
    allocate (data_array(N1,N2), stat = alloc_stat)
    if (alloc_stat /= 0) then
      print '("Error: no host memory for "I0" elements")', N1*N2
      goto 999
    end if
    for_cleanup = IOR(for_cleanup, HOST_DATA)

    !--------------------------------------------------------------
    print '("Preallocate buffers on the target")'
    print *
    !--------------------------------------------------------------
    !DIR$ OFFLOAD BEGIN if(micno>=0) target(mic:micno) inout(data_array: alloc_if(.TRUE.) free_if(.FALSE.))
      ! This might fail with COI_RESOURCE_EXHAUSTED error for large array_len,
      ! which cannot be intercepted with current offload usage model
    !DIR$ END OFFLOAD
    for_cleanup = IOR(for_cleanup, MIC_DATA)

    !------------------------------------------------------------
    ! Now we can use the descriptor for computing
    ! as many FFTs of size N as necessary
    !------------------------------------------------------------
    do i = 1,3
      ! Pick an "arbitrary" harmonic to verify FFT 
      H1 = mod((i+1)*N1, 11)
      H2 = mod((i+1)*N2, 13)

      ! Initialize input for forward transform 
      call init(data_array, N1, N2, H1, H2)

      !------------------------------------------------------------
      print *
      print '("Offload computation onto the target, H1="I0", H2="I0)',H1, H2
      print *
      !------------------------------------------------------------
      ! Reuse preallocated buffers by specifying alloc_if(.FALSE.) free_if(.FALSE.) 
      !DIR$ OFFLOAD BEGIN if(micno>=0) target(mic:micno) out(status) nocopy(hand) inout(data_array: alloc_if(.FALSE.) free_if(.FALSE.))
        ! Compute just forward FFT 
        status = DftiComputeForward(hand, data_array(:, 1))
        ! Other computation may be done here 
      !DIR$ END OFFLOAD
      if (0 /= status) then
         print '("Error: computing FFT failed, status="I0)', status
         print *
         goto 999
      end if

      ! Verify the result 
      status = verify(data_array, N1, N2, H1, H2)
      if (0 /= status) then
        print '("Error: incorrect result for N1="I0", N2="I0", H1="I0",H2="I0)', N1, N2, H1, H2
        print *
        goto 999
      end if
    end do

    !------------------------------------------------------------
999 print '("Cleanup resources")'
    !------------------------------------------------------------
    if (IAND(for_cleanup, MIC_HAND) /= 0) then
      !DIR$ OFFLOAD BEGIN if(micno>=0) target(mic:micno) nocopy(hand) out(status)
        status = DftiFreeDescriptor(hand)
      !DIR$ END OFFLOAD
    end if

    if (IAND(for_cleanup, MIC_DATA) /= 0) then
      !DIR$ OFFLOAD BEGIN if(micno>=0) target(mic:micno) nocopy(data_array: alloc_if(.FALSE.) free_if(.TRUE.))
      !DIR$ END OFFLOAD
    end if

    if (IAND(for_cleanup, HOST_DATA) /= 0) then
      deallocate(data_array)
    end if


    if (0 == status) then
      print '("Test for N1="I0",N2="I0" passed")', N1, N2
    else 
      print '("Test for N1="I0",N2="I0" failed")', N1, N2
    end if
    demo_fft=status
  end function demo_fft

  ! Compute mod(K*L,M) accurately
  pure real(WP) function moda(k,l,m)
    integer, intent(in) :: k,l,m
    integer*8 :: k8
    k8 = k
    moda = real(mod(k8*l,m),WP)
  end function moda

  ! Initialize array with harmonic /H1, H2/
  subroutine init(x, N1, N2, H1, H2)
    integer N1, N2, H1, H2
    complex(WP) :: x(:,:)

    integer k1, k2
    complex(WP), parameter :: I_TWOPI = (0,6.2831853071795864769_WP)

    forall (k1=1:N1, k2=1:N2)
      x(k1,k2) = exp( I_TWOPI * ( &
        moda(  k1-1,H1, N1) / N1  &
        + moda(k2-1,H2, N2) / N2 )) / (N1*N2)
    end forall
  end subroutine init

  ! Verify that x(N1,N2) is unit peak at x(H1,H2)
  integer function verify(x, N1, N2, H1, H2)
    integer N1, N2, H1, H2
    complex(WP) :: x(:,:)

    integer k1, k2
    real(WP) err, errthr, maxerr
    complex(WP) :: res_exp, res_got

    ! Note, this simple error bound doesn't take into account error of
    ! input data
    errthr = 5.0 * log(real(N1*N2,WP)) / log(2.0_WP) * EPSILON(1.0_WP)
    print '("  Check if err is below errthr " G10.3)', errthr

    maxerr = 0
    do k2 = 1, N2
      do k1 = 1, N1
        if (mod(k1-1-H1, N1)==0 .AND. mod(k2-1-H2, N2)==0) then
          res_exp = 1
        else
          res_exp = 0
        end if
        res_got = x(k1,k2)
        err = abs(res_got - res_exp)
        maxerr = max(err,maxerr)
        if (.not.(err < errthr)) then
          print '("  x("I0","I0"):"$)', k1, k2
          print '(" expected ("G24.17","G24.17"),"$)', res_exp
          print '(" got ("G24.17","G24.17"),"$)', res_got
          print '(" err "G10.3)', err
          print *," Verification FAILED"
          verify = 100
          return
        end if
      end do
    end do
    print '("  Verified,  maximum error was " G10.3)', maxerr
    verify = 0
  end function verify

end program complex_dft_2d