C     ==================================================================
C     ==================================================================
C     ====  DINIT -- INITIALIZE DOUBLE PRECISION VECTOR TO CONSTANT ====
C     ==================================================================
C     ==================================================================
C
      SUBROUTINE   DINIT   ( N, A, X, INCX )
C
C     ==================================================================
C
C     PURPOSE ... INITIALIZES DOUBLE PRECISION VECTOR TO
C                 A CONSTANT VALUE 'A'
C
C     CREATED ... APR. 14, 1987
C
C     ==================================================================
C
C     -------------
C     ... ARGUMENTS
C     -------------
C
      INTEGER             N, INCX
C
      DOUBLE PRECISION    A, X (*)
C
C     -------------------
C     ... LOCAL VARIABLES
C     -------------------
C
      INTEGER             XADDR, I
C
C     ==================================================================
C
      IF  ( INCX .EQ. 1 )  THEN
C
C         ----------------------------------
C         ... UNIT INCREMENT (STANDARD CASE)
C         ----------------------------------
C
          DO 100 I = 1, N
              X(I) = A
  100     CONTINUE
C
      ELSE
C
C         ----------------------
C         ... NON-UNIT INCREMENT
C         ----------------------
C
          XADDR = 1
          IF  ( INCX .LT. 0 )  THEN
              XADDR = (-N+1)*INCX + 1
          ENDIF
C
          DO 200 I = 1, N
              X (XADDR) = A
              XADDR     = XADDR + INCX
  200     CONTINUE
C
      ENDIF
C
      RETURN
C
      END

      SUBROUTINE   SINIT   ( N, A, X, INCX )
C
C     ==================================================================
C     ==================================================================
C     ====  SINIT -- INITIALIZE REAL VECTOR TO CONSTANT             ====
C     ==================================================================
C     ==================================================================
C
C     PURPOSE ... INITIALIZES REAL VECTOR TO A CONSTANT VALUE 'A'
C
C     CREATED ... APR. 14, 1987
C
C     ==================================================================
C
C     -------------
C     ... ARGUMENTS
C     -------------
C
      INTEGER             N, INCX
C
      REAL                A, X (*)
C
C     -------------------
C     ... LOCAL VARIABLES
C     -------------------
C
      INTEGER             XADDR, I
C
C     ==================================================================
C
      IF  ( INCX .EQ. 1 )  THEN
C
C         ----------------------------------
C         ... UNIT INCREMENT (STANDARD CASE)
C         ----------------------------------
C
          DO 100 I = 1, N
              X(I) = A
  100     CONTINUE
C
      ELSE
C
C         ----------------------
C         ... NON-UNIT INCREMENT
C         ----------------------
C
          XADDR = 1
          IF  ( INCX .LT. 0 )  THEN
              XADDR = (-N+1)*INCX + 1
          ENDIF
C
          DO 200 I = 1, N
              X (XADDR) = A
              XADDR     = XADDR + INCX
  200     CONTINUE
C
      ENDIF
C
      RETURN
C
      END

      SUBROUTINE   CINIT   ( N, A, X, INCX )
C
C     ==================================================================
C     ==================================================================
C     ====  CINIT -- INITIALIZE COMPLEX VECTOR TO CONSTANT          ====
C     ==================================================================
C     ==================================================================
C
C     PURPOSE ... INITIALIZES COMPLEX VECTOR TO A CONSTANT VALUE 'A'
C
C     CREATED ... APR. 14, 1987
C
C     ==================================================================
C
C     -------------
C     ... ARGUMENTS
C     -------------
C
      INTEGER             N, INCX
C
      COMPLEX             A, X (*)
C
C     -------------------
C     ... LOCAL VARIABLES
C     -------------------
C
      INTEGER             XADDR, I
C
C     ==================================================================
C
      IF  ( INCX .EQ. 1 )  THEN
C
C         ----------------------------------
C         ... UNIT INCREMENT (STANDARD CASE)
C         ----------------------------------
C
          DO 100 I = 1, N
              X(I) = A
  100     CONTINUE
C
      ELSE
C
C         ----------------------
C         ... NON-UNIT INCREMENT
C         ----------------------
C
          XADDR = 1
          IF  ( INCX .LT. 0 )  THEN
              XADDR = (-N+1)*INCX + 1
          ENDIF
C
          DO 200 I = 1, N
              X (XADDR) = A
              XADDR     = XADDR + INCX
  200     CONTINUE
C
      ENDIF
C
      RETURN
C
      END

C     ==================================================================
C     ==================================================================
C     ====  ZINIT -- INITIALIZE DOUBLE COMPLEX VECTOR TO CONSTANT   ====
C     ==================================================================
C     ==================================================================
C
      SUBROUTINE   ZINIT   ( N, A, X, INCX )
C
C     ==================================================================
C
C     PURPOSE ... INITIALIZES DOUBLE COMPLEX VECTOR TO
C                 A CONSTANT VALUE 'A'
C
C     CREATED ... APR. 14, 1987
C
C     ==================================================================
C
C     -------------
C     ... ARGUMENTS
C     -------------
C
      INTEGER             N, INCX
C
      COMPLEX * 16        A, X (*)
C
C     -------------------
C     ... LOCAL VARIABLES
C     -------------------
C
      INTEGER             XADDR, I
C
C     ==================================================================
C
      IF  ( INCX .EQ. 1 )  THEN
C
C         ----------------------------------
C         ... UNIT INCREMENT (STANDARD CASE)
C         ----------------------------------
C
          DO 100 I = 1, N
              X(I) = A
  100     CONTINUE
C
      ELSE
C
C         ----------------------
C         ... NON-UNIT INCREMENT
C         ----------------------
C
          XADDR = 1
          IF  ( INCX .LT. 0 )  THEN
              XADDR = (-N+1)*INCX + 1
          ENDIF
C
          DO 200 I = 1, N
              X (XADDR) = A
              XADDR     = XADDR + INCX
  200     CONTINUE
C
      ENDIF
C
      RETURN
C
      END

      SUBROUTINE   ICOPY   ( N, X, INCX, Y, INCY )
C
C     ==================================================================
C     ==================================================================
C     ====  ICOPY -- COPY ONE INTEGER VECTOR TO ANOTHER             ====
C     ==================================================================
C     ==================================================================
C
C     PURPOSE ... (VARIANT OF 'SCOPY')
C                 COPY ONE INTEGER VECTOR TO ANOTHER.
C                 STANDARD INCREMENT OF 1 SHOULD BE USED FOR FORWARD
C                 COPY WITHIN SAME VECTOR.
C
C     CREATED       ... MAR. 12, 1985
C     LAST MODIFIED ... APR. 19, 1985
C
C     ==================================================================
C
C     -------------
C     ... ARGUMENTS
C     -------------
C
      INTEGER             N, INCX, INCY
C
      INTEGER             X (*), Y (*)
C
C     -------------------
C     ... LOCAL VARIABLES
C     -------------------
C
      INTEGER             XADDR, YADDR, I
C
C     ==================================================================
C
      IF  ( INCX .EQ. 1  .AND.  INCY .EQ. 1 )  THEN
C
C         -----------------------------------
C         ... UNIT INCREMENTS (STANDARD CASE)
C         -----------------------------------
C
          DO 100 I = 1, N
              Y (I) = X (I)
  100     CONTINUE
C
      ELSE
C
C         -------------------------
C         ... NON-UNIT INCREMENTS
C             (-1) USED FOR REVERSE
C             COPYING IN SAME ARRAY
C         -------------------------
C
          XADDR = 1
          YADDR = 1
C
          IF  ( INCX .LT. 0 )  THEN
              XADDR = (-N+1)*INCX + 1
          ENDIF
C
          IF  ( INCY .LT. 0 )  THEN
              YADDR = (-N+1)*INCY + 1
          ENDIF
C
          DO 200 I = 1, N
              Y (YADDR) = X (XADDR)
              XADDR     = XADDR + INCX
              YADDR     = YADDR + INCY
  200     CONTINUE
C
      ENDIF
C
      RETURN
C
      END

      SUBROUTINE   IINIT   ( N, A, X, INCX )
C
C     ==================================================================
C     ==================================================================
C     ====  IINIT -- INITIALIZE INTEGER VECTOR TO CONSTANT          ====
C     ==================================================================
C     ==================================================================
C
C     PURPOSE ... INITIALIZES INTEGER VECTOR TO A CONSTANT VALUE 'A'
C
C     CREATED       ... MAR. 8, 1985
C     LAST MODIFIED ... APR. 19, 1985
C
C     ==================================================================
C
C     -------------
C     ... ARGUMENTS
C     -------------
C
      INTEGER             N, INCX
C
      INTEGER             A, X (*)
C
C     -------------------
C     ... LOCAL VARIABLES
C     -------------------
C
      INTEGER             XADDR, I
C
C     ==================================================================
C
      IF  ( INCX .EQ. 1 )  THEN
C
C         ----------------------------------
C         ... UNIT INCREMENT (STANDARD CASE)
C         ----------------------------------
C
          DO 100 I = 1, N
              X(I) = A
  100     CONTINUE
C
      ELSE
C
C         ----------------------
C         ... NON-UNIT INCREMENT
C         ----------------------
C
          XADDR = 1
          IF  ( INCX .LT. 0 )  THEN
              XADDR = (-N+1)*INCX + 1
          ENDIF
C
          DO 200 I = 1, N
              X (XADDR) = A
              XADDR     = XADDR + INCX
  200     CONTINUE
C
      ENDIF
C
      RETURN
C
      END

      LOGICAL FUNCTION  IVSAME   ( N, IX, IY )
C
C     ==================================================================
C
C     LOGICAL FUNCTION  IVSAME  DETERMINES IF THE VECTORS  IX  AND  IY
C     AGREE EXACTLY WITH EACH OTHER.
C
C     ==================================================================
C
C     ------------------------
C     ... VARIABLE DECLARATION
C     ------------------------
C
      INTEGER             I, N, IX (*), IY (*)
C
C     ==================================================================
C
      IVSAME = .TRUE.
C
      IF  ( N .LE. 0 )  RETURN
C
      DO 10 I = 1, N
          IF  ( IX(I) .NE. IY(I) )  THEN
              IVSAME = .FALSE.
              GO TO 20
          ENDIF
   10 CONTINUE
C
   20 RETURN
C
      END

      SUBROUTINE   GNINDX   ( NZ, N, ICLOBR, KINDX, INDX )
C
C     ==================================================================
C     ==================================================================
C     ====  GNINDX -- GENERATE INDEX ARRAY PATTERNS                 ====
C     ==================================================================
C     ==================================================================
C
C     GNINDX GENERATES VARIOUS PATTERNS FOR THE ARRAY INDX BASED
C     ON THE KEY KINDX.  THE GENERATED INDX ARRAY HAS NZ SIGNIFICANT
C     COMPONENTS.  THE REMAINING N-NZ COMPONENTS ARE SET TO
C     ICLOBR.
C
C     ==================================================================
C
C     -------------
C     ... ARGUMENTS
C     -------------
C
      INTEGER             NZ, N, ICLOBR, KINDX, INDX (*)
C
C     -------------------
C     ... LOCAL VARIABLES
C     -------------------
C
      INTEGER             I,  L
C
C     --------------------
C     ... SUBPROGRAMS USED
C     --------------------
C
      EXTERNAL            IINIT
C
C     ==================================================================
C
      IF  ( N .LE. 0 )  RETURN
C
      L = MAX ( N, N-NZ )
      CALL IINIT ( L, ICLOBR, INDX, 1 )
C
      IF  ( NZ .LE. 0 )  RETURN
C
      KINDX = MAX ( KINDX, 1 )
      KINDX = MIN ( KINDX, 5 )
C
C     -------------------
C     ... BRANCH ON KINDX
C     -------------------
C
      GO TO ( 100, 200, 300, 400, 500 ), KINDX
C
C     -----------------------------------
C     ... ASCENDING ORDER - 1, 2, ..., NZ
C     -----------------------------------
C
  100 DO 110 I = 1, NZ
          INDX(I) = I
  110 CONTINUE
      GO TO 900
C
C     ------------------------------------------
C     ... ASCENDING ORDER - N-NZ+1, N-NZ, ..., N
C     ------------------------------------------
C
  200 L = N - NZ
      DO 210 I = 1, NZ
          INDX(I) = L + I
  210 CONTINUE
      GO TO 900
C
C     ---------------------------------------
C     ... DESCENDING ORDER - NZ, NZ-1, ..., 1
C     ---------------------------------------
C
  300 L = NZ
      DO 310 I = 1, NZ
          INDX(I) = L
          L       = L -1
  310 CONTINUE
      GO TO 900
C
C     ------------------------------------------
C     ... DESCENDING ORDER - N, N-1, ..., N-NZ+1
C     ------------------------------------------
C
  400 L = N
      DO 410 I = 1, NZ
          INDX(I) = L
          L       = L - 1
  410 CONTINUE
      GO TO 900
C
C     --------------------------------------------------------
C     ... ALTERNATING ORDER WITH EVEN NUMBERS IN REVERSE ORDER
C     --------------------------------------------------------
C
  500 DO 510 I = 1, NZ, 2
          INDX(I) = I
  510 CONTINUE
C
      L = N
      DO 520 I = 2, NZ, 2
          INDX(I) = L
          L       = L - 2
  520 CONTINUE
      GO TO 900
C
C     ==================================================================
C
  900 RETURN
      END
*
*
*
*
      SUBROUTINE HEADER_C
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalars in Common ..
      INTEGER          ICASE, INCX, INCY, MODE, N
      LOGICAL          PASS
*     .. Local Arrays ..
      CHARACTER*6      L(10)
*     .. Common blocks ..
      COMMON           /COMBLA/ICASE, N, INCX, INCY, MODE, PASS
*     .. Data statements ..
      DATA             L(1)/'CDOTC '/
      DATA             L(2)/'CDOTU '/
      DATA             L(3)/'CAXPY '/
      DATA             L(4)/'CCOPY '/
      DATA             L(5)/'CSWAP '/
      DATA             L(6)/'SCNRM2'/
      DATA             L(7)/'SCASUM'/
      DATA             L(8)/'CSCAL '/
      DATA             L(9)/'CSSCAL'/
      DATA             L(10)/'ICAMAX'/
*     .. Executable Statements ..
      WRITE (NOUT,99999) ICASE, L(ICASE)
      RETURN
*
99999 FORMAT (/' Test of subprogram number',I3,12X,A6)
      END
*
*
      SUBROUTINE C_CHECK1(SFAC)
*     .. Parameters ..
      INTEGER           NOUT
      PARAMETER         (NOUT=6)
*     .. Scalar Arguments ..
      REAL              SFAC, RZERO, RONE, RMONE
      PARAMETER        ( RZERO = 0.0E+0, RONE = 1.0E+0,
     +                   RMONE = -1.0E+0 )

*     .. Scalars in Common ..
      INTEGER           ICASE, INCX, INCY, MODE, N
      LOGICAL           PASS
*     .. Local Scalars ..
      COMPLEX           CA
      REAL              SA
      INTEGER           I, J, LEN, NP1
*     .. Local Arrays ..
      COMPLEX           CTRUE5(8,5,2), CTRUE6(8,5,2), CV(8,5,2), CX(8),
     +                  MWPCS(5), MWPCT(5), ZERO
      PARAMETER        ( ZERO = (0.0E+0, 0.0E+0) )
      REAL              STRUE2(5), STRUE4(5)
      INTEGER           ITRUE3(5)
*     .. External Functions ..
      REAL              SCASUM, SCNRM2
      INTEGER           ICAMAX
      EXTERNAL          SCASUM, SCNRM2, ICAMAX
*     .. External Subroutines ..
      EXTERNAL          CSCAL, CSSCAL, CTEST, ITEST1, STEST1
*     .. Intrinsic Functions ..
      INTRINSIC         MAX
*     .. Common blocks ..
      COMMON            /COMBLA/ICASE, N, INCX, INCY, MODE, PASS
*     .. Data statements ..
      DATA              SA, CA/0.3E0, (0.4E0,-0.7E0)/
      DATA              ((CV(I,J,1),I=1,8),J=1,5)/(0.1E0,0.1E0),
     +                  (1.0E0,2.0E0), (1.0E0,2.0E0), (1.0E0,2.0E0),
     +                  (1.0E0,2.0E0), (1.0E0,2.0E0), (1.0E0,2.0E0),
     +                  (1.0E0,2.0E0), (0.3E0,-0.4E0), (3.0E0,4.0E0),
     +                  (3.0E0,4.0E0), (3.0E0,4.0E0), (3.0E0,4.0E0),
     +                  (3.0E0,4.0E0), (3.0E0,4.0E0), (3.0E0,4.0E0),
     +                  (0.1E0,-0.3E0), (0.5E0,-0.1E0), (5.0E0,6.0E0),
     +                  (5.0E0,6.0E0), (5.0E0,6.0E0), (5.0E0,6.0E0),
     +                  (5.0E0,6.0E0), (5.0E0,6.0E0), (0.1E0,0.1E0),
     +                  (-0.6E0,0.1E0), (0.1E0,-0.3E0), (7.0E0,8.0E0),
     +                  (7.0E0,8.0E0), (7.0E0,8.0E0), (7.0E0,8.0E0),
     +                  (7.0E0,8.0E0), (0.3E0,0.1E0), (0.1E0,0.4E0),
     +                  (0.4E0,0.1E0), (0.1E0,0.2E0), (2.0E0,3.0E0),
     +                  (2.0E0,3.0E0), (2.0E0,3.0E0), (2.0E0,3.0E0)/
      DATA              ((CV(I,J,2),I=1,8),J=1,5)/(0.1E0,0.1E0),
     +                  (4.0E0,5.0E0), (4.0E0,5.0E0), (4.0E0,5.0E0),
     +                  (4.0E0,5.0E0), (4.0E0,5.0E0), (4.0E0,5.0E0),
     +                  (4.0E0,5.0E0), (0.3E0,-0.4E0), (6.0E0,7.0E0),
     +                  (6.0E0,7.0E0), (6.0E0,7.0E0), (6.0E0,7.0E0),
     +                  (6.0E0,7.0E0), (6.0E0,7.0E0), (6.0E0,7.0E0),
     +                  (0.1E0,-0.3E0), (8.0E0,9.0E0), (0.5E0,-0.1E0),
     +                  (2.0E0,5.0E0), (2.0E0,5.0E0), (2.0E0,5.0E0),
     +                  (2.0E0,5.0E0), (2.0E0,5.0E0), (0.1E0,0.1E0),
     +                  (3.0E0,6.0E0), (-0.6E0,0.1E0), (4.0E0,7.0E0),
     +                  (0.1E0,-0.3E0), (7.0E0,2.0E0), (7.0E0,2.0E0),
     +                  (7.0E0,2.0E0), (0.3E0,0.1E0), (5.0E0,8.0E0),
     +                  (0.1E0,0.4E0), (6.0E0,9.0E0), (0.4E0,0.1E0),
     +                  (8.0E0,3.0E0), (0.1E0,0.2E0), (9.0E0,4.0E0)/
      DATA              STRUE2/0.0E0, 0.5E0, 0.6E0, 0.7E0, 0.7E0/
      DATA              STRUE4/0.0E0, 0.7E0, 1.0E0, 1.3E0, 1.7E0/
      DATA              ((CTRUE5(I,J,1),I=1,8),J=1,5)/(0.1E0,0.1E0),
     +                  (1.0E0,2.0E0), (1.0E0,2.0E0), (1.0E0,2.0E0),
     +                  (1.0E0,2.0E0), (1.0E0,2.0E0), (1.0E0,2.0E0),
     +                  (1.0E0,2.0E0), (-0.16E0,-0.37E0), (3.0E0,4.0E0),
     +                  (3.0E0,4.0E0), (3.0E0,4.0E0), (3.0E0,4.0E0),
     +                  (3.0E0,4.0E0), (3.0E0,4.0E0), (3.0E0,4.0E0),
     +                  (-0.17E0,-0.19E0), (0.13E0,-0.39E0),
     +                  (5.0E0,6.0E0), (5.0E0,6.0E0), (5.0E0,6.0E0),
     +                  (5.0E0,6.0E0), (5.0E0,6.0E0), (5.0E0,6.0E0),
     +                  (0.11E0,-0.03E0), (-0.17E0,0.46E0),
     +                  (-0.17E0,-0.19E0), (7.0E0,8.0E0), (7.0E0,8.0E0),
     +                  (7.0E0,8.0E0), (7.0E0,8.0E0), (7.0E0,8.0E0),
     +                  (0.19E0,-0.17E0), (0.32E0,0.09E0),
     +                  (0.23E0,-0.24E0), (0.18E0,0.01E0),
     +                  (2.0E0,3.0E0), (2.0E0,3.0E0), (2.0E0,3.0E0),
     +                  (2.0E0,3.0E0)/
      DATA              ((CTRUE5(I,J,2),I=1,8),J=1,5)/(0.1E0,0.1E0),
     +                  (4.0E0,5.0E0), (4.0E0,5.0E0), (4.0E0,5.0E0),
     +                  (4.0E0,5.0E0), (4.0E0,5.0E0), (4.0E0,5.0E0),
     +                  (4.0E0,5.0E0), (-0.16E0,-0.37E0), (6.0E0,7.0E0),
     +                  (6.0E0,7.0E0), (6.0E0,7.0E0), (6.0E0,7.0E0),
     +                  (6.0E0,7.0E0), (6.0E0,7.0E0), (6.0E0,7.0E0),
     +                  (-0.17E0,-0.19E0), (8.0E0,9.0E0),
     +                  (0.13E0,-0.39E0), (2.0E0,5.0E0), (2.0E0,5.0E0),
     +                  (2.0E0,5.0E0), (2.0E0,5.0E0), (2.0E0,5.0E0),
     +                  (0.11E0,-0.03E0), (3.0E0,6.0E0),
     +                  (-0.17E0,0.46E0), (4.0E0,7.0E0),
     +                  (-0.17E0,-0.19E0), (7.0E0,2.0E0), (7.0E0,2.0E0),
     +                  (7.0E0,2.0E0), (0.19E0,-0.17E0), (5.0E0,8.0E0),
     +                  (0.32E0,0.09E0), (6.0E0,9.0E0),
     +                  (0.23E0,-0.24E0), (8.0E0,3.0E0),
     +                  (0.18E0,0.01E0), (9.0E0,4.0E0)/
      DATA              ((CTRUE6(I,J,1),I=1,8),J=1,5)/(0.1E0,0.1E0),
     +                  (1.0E0,2.0E0), (1.0E0,2.0E0), (1.0E0,2.0E0),
     +                  (1.0E0,2.0E0), (1.0E0,2.0E0), (1.0E0,2.0E0),
     +                  (1.0E0,2.0E0), (0.09E0,-0.12E0), (3.0E0,4.0E0),
     +                  (3.0E0,4.0E0), (3.0E0,4.0E0), (3.0E0,4.0E0),
     +                  (3.0E0,4.0E0), (3.0E0,4.0E0), (3.0E0,4.0E0),
     +                  (0.03E0,-0.09E0), (0.15E0,-0.03E0),
     +                  (5.0E0,6.0E0), (5.0E0,6.0E0), (5.0E0,6.0E0),
     +                  (5.0E0,6.0E0), (5.0E0,6.0E0), (5.0E0,6.0E0),
     +                  (0.03E0,0.03E0), (-0.18E0,0.03E0),
     +                  (0.03E0,-0.09E0), (7.0E0,8.0E0), (7.0E0,8.0E0),
     +                  (7.0E0,8.0E0), (7.0E0,8.0E0), (7.0E0,8.0E0),
     +                  (0.09E0,0.03E0), (0.03E0,0.12E0),
     +                  (0.12E0,0.03E0), (0.03E0,0.06E0), (2.0E0,3.0E0),
     +                  (2.0E0,3.0E0), (2.0E0,3.0E0), (2.0E0,3.0E0)/
      DATA              ((CTRUE6(I,J,2),I=1,8),J=1,5)/(0.1E0,0.1E0),
     +                  (4.0E0,5.0E0), (4.0E0,5.0E0), (4.0E0,5.0E0),
     +                  (4.0E0,5.0E0), (4.0E0,5.0E0), (4.0E0,5.0E0),
     +                  (4.0E0,5.0E0), (0.09E0,-0.12E0), (6.0E0,7.0E0),
     +                  (6.0E0,7.0E0), (6.0E0,7.0E0), (6.0E0,7.0E0),
     +                  (6.0E0,7.0E0), (6.0E0,7.0E0), (6.0E0,7.0E0),
     +                  (0.03E0,-0.09E0), (8.0E0,9.0E0),
     +                  (0.15E0,-0.03E0), (2.0E0,5.0E0), (2.0E0,5.0E0),
     +                  (2.0E0,5.0E0), (2.0E0,5.0E0), (2.0E0,5.0E0),
     +                  (0.03E0,0.03E0), (3.0E0,6.0E0),
     +                  (-0.18E0,0.03E0), (4.0E0,7.0E0),
     +                  (0.03E0,-0.09E0), (7.0E0,2.0E0), (7.0E0,2.0E0),
     +                  (7.0E0,2.0E0), (0.09E0,0.03E0), (5.0E0,8.0E0),
     +                  (0.03E0,0.12E0), (6.0E0,9.0E0), (0.12E0,0.03E0),
     +                  (8.0E0,3.0E0), (0.03E0,0.06E0), (9.0E0,4.0E0)/
      DATA              ITRUE3/0, 1, 2, 2, 2/
*     .. Executable Statements ..
      DO 60 INCX = 1, 2
         DO 40 NP1 = 1, 5
            N = NP1 - 1
            LEN = 2*MAX(N,1)
*           .. Set vector arguments ..
            DO 20 I = 1, LEN
               CX(I) = CV(I,NP1,INCX)
   20       CONTINUE

            IF (ICASE.EQ.6) THEN
*              .. SCNRM2 ..
               CALL STEST1(SCNRM2(N,CX,INCX),STRUE2(NP1),STRUE2(NP1),
     +                     SFAC)
            ELSE IF (ICASE.EQ.7) THEN
*              .. SCASUM ..
               CALL STEST1(SCASUM(N,CX,INCX),STRUE4(NP1),STRUE4(NP1),
     +                     SFAC)
            ELSE IF (ICASE.EQ.8) THEN
*              .. CSCAL ..
               CALL CSCAL(N,CA,CX,INCX)
               CALL CTEST(LEN,CX,CTRUE5(1,NP1,INCX),CTRUE5(1,NP1,INCX),
     +                    SFAC)
            ELSE IF (ICASE.EQ.9) THEN
*              .. CSSCAL ..
               CALL CSSCAL(N,SA,CX,INCX)
               CALL CTEST(LEN,CX,CTRUE6(1,NP1,INCX),CTRUE6(1,NP1,INCX),
     +                    SFAC)
            ELSE IF (ICASE.EQ.10) THEN
*              .. ICAMAX ..
               CALL ITEST1(ICAMAX(N,CX,INCX),ITRUE3(NP1))
            ELSE
               WRITE (NOUT,*) ' Shouldn''t be here in CHECK1'
               STOP
            END IF
*
   40    CONTINUE
   60 CONTINUE
*
*
      DO INCX = 1, 2
         IF (ICASE.EQ.8) THEN
*           CSCAL
*           Add a test for alpha equal to zero.
            CA = (0.0E0,0.0E0)
            DO 80 I = 1, 5
               MWPCT(I) = (0.0E0,0.0E0)
               MWPCS(I) = (1.0E0,1.0E0)
   80       CONTINUE
            CALL CSCAL(5,CA,CX,INCX)
            CALL CTEST(5,CX,MWPCT,MWPCS,SFAC)
         ELSE IF (ICASE.EQ.9) THEN
*           CSSCAL
*           Add a test for alpha equal to zero.
            SA = 0.0E0
            DO 100 I = 1, 5
               MWPCT(I) = (0.0E0,0.0E0)
               MWPCS(I) = (1.0E0,1.0E0)
  100       CONTINUE
            CALL CSSCAL(5,SA,CX,INCX)
            CALL CTEST(5,CX,MWPCT,MWPCS,SFAC)
*           Add a test for alpha equal to one.
            SA = 1.0E0
            DO 120 I = 1, 5
               MWPCT(I) = CX(I)
               MWPCS(I) = CX(I)
  120       CONTINUE
            CALL CSSCAL(5,SA,CX,INCX)
            CALL CTEST(5,CX,MWPCT,MWPCS,SFAC)
*           Add a test for alpha equal to minus one.
            SA = -1.0E0
            DO 140 I = 1, 5
               MWPCT(I) = -CX(I)
               MWPCS(I) = -CX(I)
  140       CONTINUE
            CALL CSSCAL(5,SA,CX,INCX)
            CALL CTEST(5,CX,MWPCT,MWPCS,SFAC)
         END IF
      END DO
*
      RETURN
      END
*
*
      SUBROUTINE STEST(LEN,SCOMP,STRUE,SSIZE,SFAC)
*     ********************************* STEST **************************
*
*     THIS SUBR COMPARES ARRAYS  SCOMP() AND STRUE() OF LENGTH LEN TO
*     SEE IF THE TERM BY TERM DIFFERENCES, MULTIPLIED BY SFAC, ARE
*     NEGLIGIBLE.
*
*     C. L. LAWSON, JPL, 1974 DEC 10
*
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalar Arguments ..
      REAL             SFAC
      INTEGER          LEN
*     .. Array Arguments ..
      REAL             SCOMP(LEN), SSIZE(LEN), STRUE(LEN)
*     .. Scalars in Common ..
      INTEGER          ICASE, INCX, INCY, MODE, N
      LOGICAL          PASS
*     .. Local Scalars ..
      REAL             SD
      INTEGER          I
*     .. External Functions ..
      REAL             SDIFF
      EXTERNAL         SDIFF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS
*     .. Common blocks ..
      COMMON           /COMBLA/ICASE, N, INCX, INCY, MODE, PASS
*     .. Executable Statements ..
*
      DO 40 I = 1, LEN
         SD = SCOMP(I) - STRUE(I)
         IF (SDIFF(ABS(SSIZE(I))+ABS(SFAC*SD),ABS(SSIZE(I))).EQ.0.0E0)
     +       GO TO 40
*
*                             HERE    SCOMP(I) IS NOT CLOSE TO STRUE(I).
*
         IF ( .NOT. PASS) GO TO 20
*                             PRINT FAIL MESSAGE AND HEADER.
         PASS = .FALSE.
         WRITE (NOUT,99999)
         WRITE (NOUT,99998)
   20    WRITE (NOUT,99997) ICASE, N, INCX, INCY, MODE, I, SCOMP(I),
     +     STRUE(I), SD, SSIZE(I)
   40 CONTINUE
      RETURN
*
99999 FORMAT ('                                       FAIL')
99998 FORMAT (/' CASE  N INCX INCY MODE  I                            ',
     +       ' COMP(I)                             TRUE(I)  DIFFERENCE',
     +       '     SIZE(I)',/1X)
99997 FORMAT (1X,I4,I3,3I5,I3,2E36.8,2E12.4)
      END
*
*
      SUBROUTINE STEST1(SCOMP1,STRUE1,SSIZE,SFAC)
*     ************************* STEST1 *****************************
*
*     THIS IS AN INTERFACE SUBROUTINE TO ACCOMODATE THE FORTRAN
*     REQUIREMENT THAT WHEN A DUMMY ARGUMENT IS AN ARRAY, THE
*     ACTUAL ARGUMENT MUST ALSO BE AN ARRAY OR AN ARRAY ELEMENT.
*
*     C.L. LAWSON, JPL, 1978 DEC 6
*
*     .. Scalar Arguments ..
      REAL              SCOMP1, SFAC, STRUE1
*     .. Array Arguments ..
      REAL              SSIZE(*)
*     .. Local Arrays ..
      REAL              SCOMP(1), STRUE(1)
*     .. External Subroutines ..
      EXTERNAL          STEST
*     .. Executable Statements ..
*
      SCOMP(1) = SCOMP1
      STRUE(1) = STRUE1
      CALL STEST(1,SCOMP,STRUE,SSIZE,SFAC)
*
      RETURN
      END
*
*
      REAL             FUNCTION SDIFF(SA,SB)
*     ********************************* SDIFF **************************
*     COMPUTES DIFFERENCE OF TWO NUMBERS.  C. L. LAWSON, JPL 1974 FEB 15
*
*     .. Scalar Arguments ..
      REAL                            SA, SB
*     .. Executable Statements ..
      SDIFF = SA - SB
      RETURN
      END
*
*
      SUBROUTINE CTEST(LEN,CCOMP,CTRUE,CSIZE,SFAC)
*     **************************** CTEST *****************************
*
*     C.L. LAWSON, JPL, 1978 DEC 6
*
*     .. Scalar Arguments ..
      REAL             SFAC
      INTEGER          LEN
*     .. Array Arguments ..
      COMPLEX          CCOMP(LEN), CSIZE(LEN), CTRUE(LEN)
*     .. Local Scalars ..
      INTEGER          I
*     .. Local Arrays ..
      REAL             SCOMP(20), SSIZE(20), STRUE(20)
*      REAL             SCOMP(2*LEN), SSIZE(2*LEN), STRUE(2*LEN)
*     .. External Subroutines ..
      EXTERNAL         STEST
*     .. Intrinsic Functions ..
      INTRINSIC        AIMAG, REAL
*     .. Executable Statements ..
      DO 20 I = 1, LEN
         SCOMP(2*I-1) = REAL(CCOMP(I))
         SCOMP(2*I) = AIMAG(CCOMP(I))
         STRUE(2*I-1) = REAL(CTRUE(I))
         STRUE(2*I) = AIMAG(CTRUE(I))
         SSIZE(2*I-1) = REAL(CSIZE(I))
         SSIZE(2*I) = AIMAG(CSIZE(I))
   20 CONTINUE
*
      CALL STEST(2*LEN,SCOMP,STRUE,SSIZE,SFAC)
      RETURN
      END
*
*
      SUBROUTINE ITEST1(ICOMP,ITRUE)
*     ********************************* ITEST1 *************************
*
*     THIS SUBROUTINE COMPARES THE VARIABLES ICOMP AND ITRUE FOR
*     EQUALITY.
*     C. L. LAWSON, JPL, 1974 DEC 10
*
*     .. Parameters ..
      INTEGER           NOUT
      PARAMETER         (NOUT=6)
*     .. Scalar Arguments ..
      INTEGER           ICOMP, ITRUE
*     .. Scalars in Common ..
      INTEGER           ICASE, INCX, INCY, MODE, N
      LOGICAL           PASS
*     .. Local Scalars ..
      INTEGER           ID
*     .. Common blocks ..
      COMMON            /COMBLA/ICASE, N, INCX, INCY, MODE, PASS
*     .. Executable Statements ..
      IF (ICOMP.EQ.ITRUE) GO TO 40
*
*                            HERE ICOMP IS NOT EQUAL TO ITRUE.
*
      IF ( .NOT. PASS) GO TO 20
*                             PRINT FAIL MESSAGE AND HEADER.
      PASS = .FALSE.
      WRITE (NOUT,99999)
      WRITE (NOUT,99998)
   20 ID = ICOMP - ITRUE
      WRITE (NOUT,99997) ICASE, N, INCX, INCY, MODE, ICOMP, ITRUE, ID
   40 CONTINUE
      RETURN
*
99999 FORMAT ('                                       FAIL')
99998 FORMAT (/' CASE  N INCX INCY MODE                               ',
     +       ' COMP                                TRUE     DIFFERENCE',
     +       /1X)
99997 FORMAT (1X,I4,I3,3I5,2I36,I12)
      END
*
*
      SUBROUTINE DTEST(LEN,SCOMP,STRUE,SSIZE,SFAC)
*     ********************************* STEST **************************
*
*     THIS SUBR COMPARES ARRAYS  SCOMP() AND STRUE() OF LENGTH LEN TO
*     SEE IF THE TERM BY TERM DIFFERENCES, MULTIPLIED BY SFAC, ARE
*     NEGLIGIBLE.
*
*     C. L. LAWSON, JPL, 1974 DEC 10
*
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalar Arguments ..
      DOUBLE PRECISION SFAC
      INTEGER          LEN
*     .. Array Arguments ..
      DOUBLE PRECISION SCOMP(LEN), SSIZE(LEN), STRUE(LEN)
*     .. Scalars in Common ..
      INTEGER          ICASE, INCX, INCY, MODE, N
      LOGICAL          PASS
*     .. Local Scalars ..
      DOUBLE PRECISION SD
      INTEGER          I
*     .. External Functions ..
      DOUBLE PRECISION DDIFF
      EXTERNAL         DDIFF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS
*     .. Common blocks ..
      COMMON           /COMBLA/ICASE, N, INCX, INCY, MODE, PASS
*     .. Executable Statements ..
*
      DO 40 I = 1, LEN
         SD = SCOMP(I) - STRUE(I)
         IF (DDIFF(ABS(SSIZE(I))+ABS(SFAC*SD),ABS(SSIZE(I))).EQ.0.0D0)
     +       GO TO 40
*
*                             HERE    SCOMP(I) IS NOT CLOSE TO STRUE(I).
*
         IF ( .NOT. PASS) GO TO 20
*                             PRINT FAIL MESSAGE AND HEADER.
         PASS = .FALSE.
         WRITE (NOUT,99999)
         WRITE (NOUT,99998)
   20    WRITE (NOUT,99997) ICASE, N, INCX, INCY, MODE, I, SCOMP(I),
     +     STRUE(I), SD, SSIZE(I)
   40 CONTINUE
      RETURN
*
99999 FORMAT ('                                       FAIL')
99998 FORMAT (/' CASE  N INCX INCY MODE  I                            ',
     +       ' COMP(I)                             TRUE(I)  DIFFERENCE',
     +       '     SIZE(I)',/1X)
99997 FORMAT (1X,I4,I3,3I5,I3,2D36.8,2D12.4)
      END
      SUBROUTINE DTEST1(SCOMP1,STRUE1,SSIZE,SFAC)
*     ************************* STEST1 *****************************
*
*     THIS IS AN INTERFACE SUBROUTINE TO ACCOMODATE THE FORTRAN
*     REQUIREMENT THAT WHEN A DUMMY ARGUMENT IS AN ARRAY, THE
*     ACTUAL ARGUMENT MUST ALSO BE AN ARRAY OR AN ARRAY ELEMENT.
*
*     C.L. LAWSON, JPL, 1978 DEC 6
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION  SCOMP1, SFAC, STRUE1
*     .. Array Arguments ..
      DOUBLE PRECISION  SSIZE(*)
*     .. Local Arrays ..
      DOUBLE PRECISION  SCOMP(1), STRUE(1)
*     .. External Subroutines ..
      EXTERNAL          DTEST
*     .. Executable Statements ..
*
      SCOMP(1) = SCOMP1
      STRUE(1) = STRUE1
      CALL DTEST(1,SCOMP,STRUE,SSIZE,SFAC)
*
      RETURN
      END
      DOUBLE PRECISION FUNCTION DDIFF(SA,SB)
*     ********************************* SDIFF **************************
*     COMPUTES DIFFERENCE OF TWO NUMBERS.  C. L. LAWSON, JPL 1974 FEB 15
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION                SA, SB
*     .. Executable Statements ..
      DDIFF = SA - SB
      RETURN
      END
*
*
      SUBROUTINE ZTEST(LEN,CCOMP,CTRUE,CSIZE,SFAC)
*     **************************** CTEST *****************************
*
*     C.L. LAWSON, JPL, 1978 DEC 6
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION SFAC
      INTEGER          LEN
*     .. Array Arguments ..
      COMPLEX*16       CCOMP(LEN), CSIZE(LEN), CTRUE(LEN)
*     .. Local Scalars ..
      INTEGER          I
*     .. Local Arrays ..
      DOUBLE PRECISION SCOMP(20), SSIZE(20), STRUE(20)
*     .. External Subroutines ..
      EXTERNAL         DTEST
*     .. Intrinsic Functions ..
      INTRINSIC        DIMAG, DBLE
*     .. Executable Statements ..
      DO 20 I = 1, LEN
         SCOMP(2*I-1) = DBLE(CCOMP(I))
         SCOMP(2*I) = DIMAG(CCOMP(I))
         STRUE(2*I-1) = DBLE(CTRUE(I))
         STRUE(2*I) = DIMAG(CTRUE(I))
         SSIZE(2*I-1) = DBLE(CSIZE(I))
         SSIZE(2*I) = DIMAG(CSIZE(I))
   20 CONTINUE
*
      CALL DTEST(2*LEN,SCOMP,STRUE,SSIZE,SFAC)
      RETURN
      END
