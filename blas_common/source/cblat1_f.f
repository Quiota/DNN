      PROGRAM CBLAT1_F
*     Test program for the COMPLEX    Level 1 BLAS.
*     Based upon the original BLAS test routine together with:
*     F06GAF Example Program Text
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalars in Common ..
      INTEGER          ICASE, INCX, INCY, MODE, N
      LOGICAL          PASS
*     .. Local Scalars ..
      REAL             SFAC
      INTEGER          IC, ERRCNT
*     .. External Subroutines ..
      EXTERNAL         C_CHECK1, HEADER_C
*     .. Common blocks ..
      COMMON           /COMBLA/ICASE, N, INCX, INCY, MODE, PASS
*     .. Data statements ..
      DATA             SFAC/9.765625E-4/
*     .. Executable Statements ..
      ERRCNT = 0
      WRITE (NOUT,99999)
*      IF (IC.EQ.6.OR.IC.EQ.7) THEN
      DO IC = 6, 7
         ICASE = IC
         CALL HEADER_C
*
*        Initialize PASS, INCX, INCY, and MODE for a new case.
*        The value 9999 for INCX, INCY or MODE will appear in the
*        detailed  output, if any, for cases that do not involve
*        these parameters.
*
         PASS = .TRUE.
         INCX = 9999
         INCY = 9999
         MODE = 9999
         CALL C_CHECK1(SFAC)
*        -- Print
         IF (PASS) THEN
         	WRITE (NOUT,99998)
         ELSE
         	ERRCNT = ERRCNT + 1
         ENDIF
      END DO
*      END IF
      IF (ERRCNT > 0) STOP 1
      STOP
*
99999 FORMAT (' Complex BLAS Test Program Results',/1X)
99998 FORMAT ('                                    ----- PASS -----')
      END
