      PROGRAM CBLAT1
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
      EXTERNAL         C_CHECK1, CHECK2, HEADER_C
*     .. Common blocks ..
      COMMON           /COMBLA/ICASE, N, INCX, INCY, MODE, PASS
*     .. Data statements ..
      DATA             SFAC/9.765625E-4/
*     .. Executable Statements ..
      ERRCNT = 0
      WRITE (NOUT,99999)
      DO 20 IC = 1, 5
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
            CALL CHECK2(SFAC)
*        -- Print
         IF (PASS) THEN
         	WRITE (NOUT,99998)
         ELSE
         	ERRCNT = ERRCNT + 1
         ENDIF
   20 CONTINUE
      DO 30 IC = 8, 10
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
   30 CONTINUE
      IF (ERRCNT > 0) STOP 1
      STOP
*
99999 FORMAT (' Complex BLAS Test Program Results',/1X)
99998 FORMAT ('                                    ----- PASS -----')
      END
*
*
      SUBROUTINE CHECK2(SFAC)
*     .. Parameters ..
      INTEGER           NOUT
      PARAMETER         (NOUT=6)
*     .. Scalar Arguments ..
      REAL              SFAC
*     .. Scalars in Common ..
      INTEGER           ICASE, INCX, INCY, MODE, N
      LOGICAL           PASS
*     .. Local Scalars ..
      COMPLEX           CA
      INTEGER           I, J, KI, KN, KSIZE, LENX, LENY, MX, MY
*     .. Local Arrays ..
      COMPLEX           CDOT(1), CSIZE1(4), CSIZE2(7,2), CSIZE3(14),
     +                  CT10X(7,4,4), CT10Y(7,4,4), CT6(4,4), CT7(4,4),
     +                  CT8(7,4,4), CX(7), CX1(7), CY(7), CY1(7)
      INTEGER           INCXS(4), INCYS(4), LENS(4,2), NS(4)
*     .. External Functions ..
      COMPLEX           CDOTC, CDOTU
      EXTERNAL          CDOTC, CDOTU
*     .. External Subroutines ..
      EXTERNAL          CAXPY, CCOPY, CSWAP, CTEST
*     .. Intrinsic Functions ..
      INTRINSIC         ABS, MIN
*     .. Common blocks ..
      COMMON            /COMBLA/ICASE, N, INCX, INCY, MODE, PASS
*     .. Data statements ..
      DATA              CA/(0.4E0,-0.7E0)/
      DATA              INCXS/1, 2, -2, -1/
      DATA              INCYS/1, -2, 1, -2/
      DATA              LENS/1, 1, 2, 4, 1, 1, 3, 7/
      DATA              NS/0, 1, 2, 4/
      DATA              CX1/(0.7E0,-0.8E0), (-0.4E0,-0.7E0),
     +                  (-0.1E0,-0.9E0), (0.2E0,-0.8E0),
     +                  (-0.9E0,-0.4E0), (0.1E0,0.4E0), (-0.6E0,0.6E0)/
      DATA              CY1/(0.6E0,-0.6E0), (-0.9E0,0.5E0),
     +                  (0.7E0,-0.6E0), (0.1E0,-0.5E0), (-0.1E0,-0.2E0),
     +                  (-0.5E0,-0.3E0), (0.8E0,-0.7E0)/
      DATA              ((CT8(I,J,1),I=1,7),J=1,4)/(0.6E0,-0.6E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.32E0,-1.41E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.32E0,-1.41E0),
     +                  (-1.55E0,0.5E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.32E0,-1.41E0), (-1.55E0,0.5E0),
     +                  (0.03E0,-0.89E0), (-0.38E0,-0.96E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0)/
      DATA              ((CT8(I,J,2),I=1,7),J=1,4)/(0.6E0,-0.6E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.32E0,-1.41E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (-0.07E0,-0.89E0),
     +                  (-0.9E0,0.5E0), (0.42E0,-1.41E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.78E0,0.06E0), (-0.9E0,0.5E0),
     +                  (0.06E0,-0.13E0), (0.1E0,-0.5E0),
     +                  (-0.77E0,-0.49E0), (-0.5E0,-0.3E0),
     +                  (0.52E0,-1.51E0)/
      DATA              ((CT8(I,J,3),I=1,7),J=1,4)/(0.6E0,-0.6E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.32E0,-1.41E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (-0.07E0,-0.89E0),
     +                  (-1.18E0,-0.31E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.78E0,0.06E0), (-1.54E0,0.97E0),
     +                  (0.03E0,-0.89E0), (-0.18E0,-1.31E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0)/
      DATA              ((CT8(I,J,4),I=1,7),J=1,4)/(0.6E0,-0.6E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.32E0,-1.41E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.32E0,-1.41E0), (-0.9E0,0.5E0),
     +                  (0.05E0,-0.6E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.32E0,-1.41E0),
     +                  (-0.9E0,0.5E0), (0.05E0,-0.6E0), (0.1E0,-0.5E0),
     +                  (-0.77E0,-0.49E0), (-0.5E0,-0.3E0),
     +                  (0.32E0,-1.16E0)/
      DATA              CT7/(0.0E0,0.0E0), (-0.06E0,-0.90E0),
     +                  (0.65E0,-0.47E0), (-0.34E0,-1.22E0),
     +                  (0.0E0,0.0E0), (-0.06E0,-0.90E0),
     +                  (-0.59E0,-1.46E0), (-1.04E0,-0.04E0),
     +                  (0.0E0,0.0E0), (-0.06E0,-0.90E0),
     +                  (-0.83E0,0.59E0), (0.07E0,-0.37E0),
     +                  (0.0E0,0.0E0), (-0.06E0,-0.90E0),
     +                  (-0.76E0,-1.15E0), (-1.33E0,-1.82E0)/
      DATA              CT6/(0.0E0,0.0E0), (0.90E0,0.06E0),
     +                  (0.91E0,-0.77E0), (1.80E0,-0.10E0),
     +                  (0.0E0,0.0E0), (0.90E0,0.06E0), (1.45E0,0.74E0),
     +                  (0.20E0,0.90E0), (0.0E0,0.0E0), (0.90E0,0.06E0),
     +                  (-0.55E0,0.23E0), (0.83E0,-0.39E0),
     +                  (0.0E0,0.0E0), (0.90E0,0.06E0), (1.04E0,0.79E0),
     +                  (1.95E0,1.22E0)/
      DATA              ((CT10X(I,J,1),I=1,7),J=1,4)/(0.7E0,-0.8E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.6E0,-0.6E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.6E0,-0.6E0), (-0.9E0,0.5E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.6E0,-0.6E0),
     +                  (-0.9E0,0.5E0), (0.7E0,-0.6E0), (0.1E0,-0.5E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0)/
      DATA              ((CT10X(I,J,2),I=1,7),J=1,4)/(0.7E0,-0.8E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.6E0,-0.6E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.7E0,-0.6E0), (-0.4E0,-0.7E0),
     +                  (0.6E0,-0.6E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.8E0,-0.7E0),
     +                  (-0.4E0,-0.7E0), (-0.1E0,-0.2E0),
     +                  (0.2E0,-0.8E0), (0.7E0,-0.6E0), (0.1E0,0.4E0),
     +                  (0.6E0,-0.6E0)/
      DATA              ((CT10X(I,J,3),I=1,7),J=1,4)/(0.7E0,-0.8E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.6E0,-0.6E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (-0.9E0,0.5E0), (-0.4E0,-0.7E0),
     +                  (0.6E0,-0.6E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.1E0,-0.5E0),
     +                  (-0.4E0,-0.7E0), (0.7E0,-0.6E0), (0.2E0,-0.8E0),
     +                  (-0.9E0,0.5E0), (0.1E0,0.4E0), (0.6E0,-0.6E0)/
      DATA              ((CT10X(I,J,4),I=1,7),J=1,4)/(0.7E0,-0.8E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.6E0,-0.6E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.6E0,-0.6E0), (0.7E0,-0.6E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.6E0,-0.6E0),
     +                  (0.7E0,-0.6E0), (-0.1E0,-0.2E0), (0.8E0,-0.7E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0)/
      DATA              ((CT10Y(I,J,1),I=1,7),J=1,4)/(0.6E0,-0.6E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.7E0,-0.8E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.7E0,-0.8E0), (-0.4E0,-0.7E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.7E0,-0.8E0),
     +                  (-0.4E0,-0.7E0), (-0.1E0,-0.9E0),
     +                  (0.2E0,-0.8E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0)/
      DATA              ((CT10Y(I,J,2),I=1,7),J=1,4)/(0.6E0,-0.6E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.7E0,-0.8E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (-0.1E0,-0.9E0), (-0.9E0,0.5E0),
     +                  (0.7E0,-0.8E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (-0.6E0,0.6E0),
     +                  (-0.9E0,0.5E0), (-0.9E0,-0.4E0), (0.1E0,-0.5E0),
     +                  (-0.1E0,-0.9E0), (-0.5E0,-0.3E0),
     +                  (0.7E0,-0.8E0)/
      DATA              ((CT10Y(I,J,3),I=1,7),J=1,4)/(0.6E0,-0.6E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.7E0,-0.8E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (-0.1E0,-0.9E0), (0.7E0,-0.8E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (-0.6E0,0.6E0),
     +                  (-0.9E0,-0.4E0), (-0.1E0,-0.9E0),
     +                  (0.7E0,-0.8E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0)/
      DATA              ((CT10Y(I,J,4),I=1,7),J=1,4)/(0.6E0,-0.6E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.7E0,-0.8E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.7E0,-0.8E0), (-0.9E0,0.5E0),
     +                  (-0.4E0,-0.7E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.7E0,-0.8E0),
     +                  (-0.9E0,0.5E0), (-0.4E0,-0.7E0), (0.1E0,-0.5E0),
     +                  (-0.1E0,-0.9E0), (-0.5E0,-0.3E0),
     +                  (0.2E0,-0.8E0)/
      DATA              CSIZE1/(0.0E0,0.0E0), (0.9E0,0.9E0),
     +                  (1.63E0,1.73E0), (2.90E0,2.78E0)/
      DATA              CSIZE3/(0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (1.17E0,1.17E0),
     +                  (1.17E0,1.17E0), (1.17E0,1.17E0),
     +                  (1.17E0,1.17E0), (1.17E0,1.17E0),
     +                  (1.17E0,1.17E0), (1.17E0,1.17E0)/
      DATA              CSIZE2/(0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (0.0E0,0.0E0),
     +                  (0.0E0,0.0E0), (0.0E0,0.0E0), (1.54E0,1.54E0),
     +                  (1.54E0,1.54E0), (1.54E0,1.54E0),
     +                  (1.54E0,1.54E0), (1.54E0,1.54E0),
     +                  (1.54E0,1.54E0), (1.54E0,1.54E0)/
*     .. Executable Statements ..
      DO 60 KI = 1, 4
         INCX = INCXS(KI)
         INCY = INCYS(KI)
         MX = ABS(INCX)
         MY = ABS(INCY)
*
         DO 40 KN = 1, 4
            N = NS(KN)
            KSIZE = MIN(2,KN)
            LENX = LENS(KN,MX)
            LENY = LENS(KN,MY)
*           .. initialize all argument arrays ..
            DO 20 I = 1, 7
               CX(I) = CX1(I)
               CY(I) = CY1(I)
   20       CONTINUE
            IF (ICASE.EQ.1) THEN
*              .. CDOTC ..
               CDOT(1) = CDOTC(N,CX,INCX,CY,INCY)
               CALL CTEST(1,CDOT,CT6(KN,KI),CSIZE1(KN),SFAC)
            ELSE IF (ICASE.EQ.2) THEN
*              .. CDOTU ..
               CDOT(1) = CDOTU(N,CX,INCX,CY,INCY)
               CALL CTEST(1,CDOT,CT7(KN,KI),CSIZE1(KN),SFAC)
            ELSE IF (ICASE.EQ.3) THEN
*              .. CAXPY ..
               CALL CAXPY(N,CA,CX,INCX,CY,INCY)
               CALL CTEST(LENY,CY,CT8(1,KN,KI),CSIZE2(1,KSIZE),SFAC)
            ELSE IF (ICASE.EQ.4) THEN
*              .. CCOPY ..
               CALL CCOPY(N,CX,INCX,CY,INCY)
               CALL CTEST(LENY,CY,CT10Y(1,KN,KI),CSIZE3,1.0E0)
            ELSE IF (ICASE.EQ.5) THEN
*              .. CSWAP ..
               CALL CSWAP(N,CX,INCX,CY,INCY)
               CALL CTEST(LENX,CX,CT10X(1,KN,KI),CSIZE3,1.0E0)
               CALL CTEST(LENY,CY,CT10Y(1,KN,KI),CSIZE3,1.0E0)
            ELSE
               WRITE (NOUT,*) ' Shouldn''t be here in CHECK2'
               STOP 1
            END IF
*
   40    CONTINUE
   60 CONTINUE
      RETURN
      END
