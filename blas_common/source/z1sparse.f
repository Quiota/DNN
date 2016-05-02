      PROGRAM   TZSPBL
C
C     ==================================================================
C     ==================================================================
C     ====  TZSPBL -- CERTIFY COMPLEX*16 SPARSE BLAS                ====
C     ==================================================================
C     ==================================================================
C
C     TZSPBL IS THE CERTIFICATION PROGRAM FOR THE COMPLEX*16 PRECISION
C     SPARSE BLAS.  THE APPROACH USED TO CERTIFY THE SPARSE BLAS
C     IS AS FOLLOWS:
C
C     1.  READ IN USER SPECIFIED INPUT ON OUTPUT UNIT, THRESHOLD VALUE
C         FOR TEST RATIO, AND THE SPECIFICATIONS FOR NZ, AND A.
C     2.  VERIFY THE CORRECTNESS OF THE USER SPECIFIED INPUT AND
C         ECHO TO THE OUTPUT UNIT.
C     3.  FOR EACH SUBPROGRAM IN THE COMPLEX*16 PRECISION SPARSE BLAS
C         PERFORM ALL THE USER SPECIFIED TESTS AND PRINT A PASS/FAIL
C         MESSAGE.  TESTS WHICH FAIL GENERATE ADDITIONAL OUTPUT.
C
C     SPARSE BLAS SUBPROGRAMS WHICH ARE CERTIFIED BY THIS PROGRAM ARE
C
C         ZAXPYI          ZDOTUI          ZGTHRZ
C         ZDOTCI          ZGTHR           ZSCTR
C
C     THIS PROGRAM REQUIRES AN INPUT FILE ASSIGNED TO UNIT NIN
C     (CURRENTLY SET TO 5 BY A PARAMETER STATEMENT).  THE DATA ON
C     THIS INPUT FILE CONTROLS THE OUTPUT UNIT, THE THRESHOLD VALUE
C     FOR THE NUMERICAL TESTING, AND THE SPECIFICATIONS FOR THE
C     TEST VALUES FOR THE LENGTH OF THE SPARSE VECTORS AND THE SCALARS
C     USED BY THE VARIOUS SUBPROGRAMS.  AN EXAMPLE OF THE INPUT FILE
C     FOLLOWS
C
C LINE  1     'ZBLATS.SUMM'           NAME OF OUTPUT FILE
C LINE  2     6                       UNIT NUMBER OF OUTPUT FILE
C LINE  3     100                     MAX. NO. OF PRINTED ERROR MESSAGES
C LINE  4     5.0                     THRESHOLD VALUE OF TEST RATIO
C LINE  5     6                       NUMBER OF VALUES OF NZ
C LINE  6     -1 0 1 2 5 9            VALUES OF NZ
C LINE  7     3                       NUMBER OF VALUES OF A FOR -AXPYI
C LINE  8     (0.0,0.0) (1.0,0.0) (0.7,0.3)
C                                     VALUES OF A
C
C
C     THIS INPUT FILE IS READ USING FORTRAN-77 STANDARD LIST DIRECTED
C     INPUT.  SINGLE QUOTES ARE REQUIRED AROUND THE NAME OF THE OUTPUT
C     FILE ON LINE 1.  THE NUMBERS ON LINES 6 AND 8 CAN BE
C     DELIMITED BY BLANKS OR COMMAS.
C
C     THIS PROGRAM WAS WRITTEN BY ROGER G. GRIMES, BOEING
C     COMPUTER SERVICES, BELLEVUE, WA. DURING APRIL, 1987.
C
C     ==================================================================
C
C     ------------------------------------
C     ... PROBLEM SPECIFICATION PARAMETERS
C     ------------------------------------
C
C         NIN         INPUT UNIT
C         NZMAX       MAXIMUM VALUE OF ANY SINGLE NZ
C         NNZMAX      MAXIMUM NUMBER OF VALUES OF NZ
C         NAMAX       MAXIMUM NUMBER OF VALUES OF A (-AXPYI
C                     SCALAR)
C
      INTEGER             NIN,    NZMAX,  NNZMAX, NAMAX
C
      PARAMETER         ( NIN = 5,        NZMAX = 320,
     1                    NNZMAX = 24,    NAMAX = 7      )
C
C     ==================================================================
C
C     -----------------------
C     ... COMPUTED PARAMETERS
C     -----------------------
C
      INTEGER             NZMAX2
C
      PARAMETER         ( NZMAX2 = 2 * NZMAX )
C
C     ==================================================================
C
C     ------------------------
C     ... VARIABLE DECLARATION
C     ------------------------
C
      CHARACTER*32        NAMOUT
C
      INTEGER             ERRCNT, ERRMAX, I,      NOUT,   NUMA,
     1                    NUMNZ
C
      INTEGER             INDX  (NZMAX2),     INDXT (NZMAX2),
     1                    LIST  (NZMAX2),     NZVALU(NNZMAX)
C
      DOUBLE PRECISION    EPSILN, EPSSAV, THRESH
C
      COMPLEX*16          X     (NZMAX2),     Y     (NZMAX2),
     1                    XTRUE (NZMAX2),     YTRUE (NZMAX2),
     2                    XSAVE (NZMAX2),     YSAVE (NZMAX2),
     3                    AVALUE(NAMAX)
C
C     --------------------
C     ... SUBPROGRAMS USED
C     --------------------
C
      DOUBLE PRECISION    DDIFF
C
      EXTERNAL            TZXPYI, TZDTCI, TZDTUI, TZGTHR, TZGTHZ,
     1                    TZSCTR, DDIFF
C
C     ==================================================================
C
      ERRCNT = 0
C
C     ------------------------------------------------
C     ... READ IN USER SPECIFIED INPUT FOR OUTPUT UNIT
C     ------------------------------------------------
C
      READ ( NIN, * ) NAMOUT
      READ ( NIN, * ) NOUT
C
C     --------------------
C     ... OPEN OUTPUT UNIT
C     --------------------
C
C      OPEN ( UNIT = NOUT, FILE = NAMOUT, STATUS = 'NEW' )
      OPEN ( UNIT = NOUT, FILE = NAMOUT )
C
C     ------------------------------
C     ... READ IN REMAINDER OF INPUT
C     ------------------------------
C
      READ ( NIN, * ) ERRMAX
      READ ( NIN, * ) THRESH
      READ ( NIN, * ) NUMNZ
C
      IF ( NUMNZ .GT. NNZMAX ) THEN
          ERRCNT = 1
          WRITE ( NOUT, 1100 ) NUMNZ, NNZMAX
          GO TO 900
      END IF
C
      READ ( NIN, * ) ( NZVALU(I), I = 1, NUMNZ )
C
      READ ( NIN, * ) NUMA
C
      IF ( NUMA .GT. NAMAX ) THEN
          ERRCNT = 1
          WRITE ( NOUT, 1110 ) NUMA, NAMAX
          GO TO 900
      END IF
C
      READ ( NIN, * ) ( AVALUE(I), I = 1, NUMA  )
C
C     ------------------------------
C     ... PRINT USER SPECIFIED INPUT
C     ------------------------------
C
      WRITE ( NOUT, 1000 ) NAMOUT, NOUT, ERRMAX, THRESH
      WRITE ( NOUT, 1010 ) NUMNZ
      WRITE ( NOUT, 1020 ) ( NZVALU(I), I = 1, NUMNZ )
      WRITE ( NOUT, 1030 ) NUMA
      WRITE ( NOUT, 1040 ) ( AVALUE(I), I = 1, NUMA  )
C
C     -------------------------------
C     ... VERIFY USER SPECIFIED INPUT
C     -------------------------------
C
      IF  ( THRESH .LE. 0.0D0 )  THEN
          WRITE ( NOUT, 1130 ) THRESH
          THRESH = 10.0E0
      END IF
C
      IF  ( NUMNZ .LE. 0 )  THEN
          WRITE ( NOUT, 1140 ) NUMNZ
          ERRCNT = 1
      END IF
C
      DO 100 I = 1, NUMNZ
          IF  ( NZVALU(I) .GT. NZMAX )  THEN
              WRITE ( NOUT, 1150 ) I, NZVALU(I), NZMAX
              NZVALU(I) = NZMAX
          END IF
  100 CONTINUE
C
      IF  ( ERRCNT .NE. 0 )  GO TO 900
C
C     ---------------------------
C     ... COMPUTE MACHINE EPSILON
C     ---------------------------
C
      EPSILN = 1.0D0
      EPSSAV = 1.0D0
C
  200 IF  ( DDIFF ( 1.0D0 + EPSILN, 1.0D0 ) .EQ. 0.0D0 )  GO TO 210
C
          EPSSAV = EPSILN
          EPSILN = EPSILN * .5D0
          GO TO 200
C
  210 EPSILN = EPSSAV
C
C     ==================================================================
C
C     ---------------------------------------------
C     ... TEST THE COMPLEX*16 PRECISION SPARSE BLAS
C     ---------------------------------------------
C
C     ------------------
C     ... CERTIFY ZAXPYI
C     ------------------
C
      CALL TZXPYI (   NOUT,   EPSILN, THRESH, NZMAX2,
     1                NUMNZ,  NZVALU, NUMA,   AVALUE ,
     2                X,      XSAVE,  XTRUE,  Y,      YSAVE,  YTRUE,
     3                INDX,   INDXT,  LIST,   ERRCNT, ERRMAX )
C
C     ------------------
C     ... CERTIFY ZDOTCI
C     ------------------
C
      CALL TZDTCI (   NOUT,   EPSILN, THRESH, NZMAX2,
     1                NUMNZ,  NZVALU,
     2                X,      XSAVE,  XTRUE,  Y,      YSAVE,  YTRUE,
     3                INDX,   INDXT,  ERRCNT, ERRMAX )
C
C     ------------------
C     ... CERTIFY ZDOTUI
C     ------------------
C
      CALL TZDTUI (   NOUT,   EPSILN, THRESH, NZMAX2,
     1                NUMNZ,  NZVALU,
     2                X,      XSAVE,  XTRUE,  Y,      YSAVE,  YTRUE,
     3                INDX,   INDXT,  ERRCNT, ERRMAX )
C
C     -----------------
C     ... CERTIFY ZGTHR
C     -----------------
C
      CALL TZGTHR (   NOUT,   NZMAX2, NUMNZ,  NZVALU,
     1                X,      XSAVE,  XTRUE,  Y,      YSAVE,  YTRUE,
     2                INDX,   INDXT,  ERRCNT, ERRMAX )
C
C     ------------------
C     ... CERTIFY ZGTHRZ
C     ------------------
C
      CALL TZGTHZ (   NOUT,   NZMAX2, NUMNZ,  NZVALU,
     1                X,      XSAVE,  XTRUE,  Y,      YSAVE,  YTRUE,
     2                INDX,   INDXT,  ERRCNT, ERRMAX )
C
C     -----------------
C     ... CERTIFY ZSCTR
C     -----------------
C
      CALL TZSCTR (   NOUT,   NZMAX2, NUMNZ,  NZVALU,
     1                X,      XSAVE,  XTRUE,  Y,      YSAVE,  YTRUE,
     2                INDX,   INDXT,  ERRCNT, ERRMAX )
C
C     ==================================================================
C
C     -------------------------------------
C     ... PRINT GLOBAL PASS OR FAIL MESSAGE
C     -------------------------------------
C
  900 IF  ( ERRCNT .EQ. 0 )  THEN
          WRITE ( NOUT, 2000 )
      ELSE
          WRITE ( NOUT, 2100 ) ERRCNT
          STOP 1
      END IF
C
C     ---------------------------------------------------------
C     ... END OF CERTIFICATION PROGRAM FOR COMPLEX*16 PRECISION
C         SPARSE BLAS
C     ---------------------------------------------------------
C
      STOP
C
C     ==================================================================
C
C     -----------
C     ... FORMATS
C     -----------
C
 1000 FORMAT( '1' ///
     1          5X, 'START OF CERTIFICATION PROGRAM FOR THE ',
     2              'COMPLEX*16 PRECISION SPARSE BLAS'
     3         /5X, '---------------------------------------',
     4              '--------------------------------'
     5        //5X, 'NAME   OF OUTPUT UNIT              = ', A
     6         /5X, 'NUMBER OF OUTPUT UNIT              = ', I10
     7         /5X, 'MAX. NO. OF PRINTED ERROR MESSAGES = ', I10
     8         /5X, 'THRESHOLD VALUE OF TEST RATIO      = ', F10.1 )
C
 1010 FORMAT ( /5X, 'NUMBER OF VALUES OF NZ        = ', I10 )
C
 1020 FORMAT ( /5X, 'VALUES OF NZ = ', 10I5 )
C
 1030 FORMAT ( /5X, 'NUMBER OF VALUES OF A         = ', I10 )
C
 1040 FORMAT ( /5X, 'VALUES OF A = ',
     1          3 ( 2X, '(', 1PD13.4, ',', 1PD13.4, ')' )     )
C
 1100 FORMAT ( /5X, 'USER SPECIFIED NUMBER OF TEST CASES FOR THE ',
     1              'NUMBER OF NONZEROES EXCEEDS PROGRAM LIMIT.'
     2         /5X, 'NUMBER SPECIFIED = ', I10, 2X, 'PROGRAM LIMIT =',
     3              I10 )
C
 1110 FORMAT ( /5X, 'USER SPECIFIED NUMBER OF TEST CASES FOR THE ',
     1              'SCALAR A EXCEEDS PROGRAM LIMIT.'
     2         /5X, 'NUMBER SPECIFIED = ', I10, 2X, 'PROGRAM LIMIT =',
     3              I10 )
C
 1130 FORMAT ( /5X, 'USER SPECIFIED VALUE FOR THRESHOLD IS ', 1PD15.5,
     1              ' WHICH IS NONPOSITIVE.  IT HAS BEEN RESET TO 10.')
C
 1140 FORMAT ( /5X, 'USER SPECIFIED NUMBER OF VALUES OF NZ IS ', I5,
     1              ' WHICH IS NONPOSITIVE.  NO TESTING WILL OCCUR.' )
C
 1150 FORMAT ( /5X, 'THE ', I3, '-TH USER SPECIFIED VALUE OF NZ IS ',
     1              I8, ' IS LARGER THAN THE MAXIMUM ALLOWABLE ',
     2              'VALUE OF NZ.  IT HAS BEEN RESET TO ', I5 )
C
 2000 FORMAT ( /5X, 'COMPLEX*16 PRECISION SPARSE BLAS HAVE PASSED ALL ',
     1              'TESTS.' )
C
 2100 FORMAT ( /5X, 'COMPLEX*16 PRECISION SPARSE BLAS HAVE FAILED ',
     1         I10, ' TESTS.  SEE ABOVE PRINTED ERROR MESSAGES.' )
C
C     ==================================================================
C
      END
      LOGICAL FUNCTION   ZVSAME   ( N, ZX, ZY )
C
C     ==================================================================
C
C     LOGICAL FUNCTION  ZVSAME  DETERMINES IF THE VECTORS  ZX  AND  ZY
C     AGREE EXACTLY WITH EACH OTHER.
C
C     ==================================================================
C
C     ------------------------
C     ... VARIABLE DECLARATION
C     ------------------------
C
      INTEGER             I, N
C
      COMPLEX*16          ZX (*), ZY (*)
C
C     ==================================================================
C
      ZVSAME = .TRUE.
C
      DO 10 I = 1, N
          IF  ( ZX(I) .NE. ZY(I) )  THEN
              ZVSAME = .FALSE.
              GO TO 20
          ENDIF
   10 CONTINUE
C
   20 RETURN
      END

      SUBROUTINE   TZXPYI   ( NOUT,   EPSILN, THRESH, NZMAX2,
     1                        NUMNZ,  NZVALU, NUMA,   AVALUE,
     2                        X,      XSAVE,  XTRUE,  Y,      YSAVE,
     3                        YTRUE , INDX,   INDXT,  LIST,   ERRCNT,
     4                        ERRMAX )
C
C     ==================================================================
C     ==================================================================
C     ====  TZXPYI  -- CERTIFY  ZAXPYI                              ====
C     ==================================================================
C     ==================================================================
C
C     SUBROUTINE  TZXPYI  IS THE CERTIFICATION MODULE FOR THE SPARSE
C     BASIC LINEAR ALGEBRA SUBROUTINE MODULE  ZAXPYI.
C
C     WRITTEN BY      ROGER G GRIMES
C                     APRIL 1987
C
C     ==================================================================
C
C     -------------
C     ... ARGUMENTS
C     -------------
C
      INTEGER             NOUT,   NZMAX2, NUMNZ,  NUMA,   ERRCNT,
     1                    ERRMAX
C
      INTEGER             NZVALU (*),  INDX (*),    INDXT (*),
     1                    LIST (*)
C
      DOUBLE PRECISION    EPSILN, THRESH
C
      COMPLEX*16          AVALUE (*),
     1                    X (*),       XSAVE (*),   XTRUE (*),
     2                    Y (*),       YSAVE (*),   YTRUE (*)
C
C     -------------------
C     ... LOCAL VARIABLES
C     -------------------
C
      COMPLEX*16          A,      ATRUE,  CLOBBR
C
      INTEGER             COUNT,  I,      ICLOBR, J,      KA,
     1                    KINDX,  KNZ,    N,      NZ,     NZTRUE
C
      DOUBLE PRECISION    ERR,    S,      T
C
C     --------------------
C     ... SUBPROGRAMS USED
C     --------------------
C
      LOGICAL             IVSAME, ZVSAME
C
      EXTERNAL            ICOPY,  ZCOPY,  IINIT,  ZINIT,  GNINDX,
     1                    IVSAME, ZVSAME, ZAXPYI
C
C     ==================================================================
C
C     ------------------
C     ... INITIALIZATION
C     ------------------
C
      COUNT     =   0
C
      CLOBBR    =   ( -1.0D10, -1.0D10 )
      ICLOBR    =   -10000000
C
C     ------------------------------------
C     ... GENERATE SOME VALUES FOR X AND Y
C     ------------------------------------
C
      DO 100 I = 1, NZMAX2
         XSAVE(I) = DCMPLX ( COS ( .6*DBLE(I) ), SIN ( .2*DBLE(I) ) )
         YSAVE(I) = DCMPLX ( SIN ( .7*DBLE(I) ), COS ( .9*DBLE(I) ) )
  100 CONTINUE
C
C     ------------------------
C     ... FOR EACH VALUE OF NZ
C     ------------------------
C
      DO 700 KNZ = 1, NUMNZ
C
          NZTRUE = NZVALU(KNZ)
          N      = 2 * MAX ( NZTRUE, 1 )
C
C         -----------------------
C         ... FOR EACH VALUE OF A
C         -----------------------
C
          DO 600 KA = 1, NUMA
C
              ATRUE = AVALUE(KA)
C
C             -------------------------------
C             ... FOR EACH KIND OF INDX ARRAY
C             -------------------------------
C
              DO 500 KINDX = 1, 5
C
                  CALL GNINDX ( NZTRUE, N, ICLOBR, KINDX, INDXT )
C
                  CALL IINIT ( N, -1, LIST, 1 )
C
                  DO 150 I = 1, NZTRUE
                      LIST (INDXT(I)) = I
  150             CONTINUE
C
C                 -----------------------
C                 ... GENERATE INPUT DATA
C                 -----------------------
C
                  I = MIN ( N, N-NZTRUE )
                  J = N - I + 1
                  CALL ZCOPY ( NZTRUE, XSAVE,  1, XTRUE, 1 )
                  CALL ZINIT ( I,      CLOBBR, XTRUE(J), 1 )
                  CALL ZINIT ( N,      CLOBBR, YTRUE, 1 )
C
                  DO 200 I = 1, NZTRUE
                      YTRUE (INDXT(I)) = YSAVE (INDXT(I))
  200             CONTINUE
C
C                 -------------------
C                 ... COPY TRUE INPUT
C                 -------------------
C
                  A  = ATRUE
                  NZ = NZTRUE
C
                  CALL ZCOPY ( N, YTRUE, 1, Y, 1 )
                  CALL ZCOPY ( N, XTRUE, 1, X, 1 )
                  CALL ICOPY ( N, INDXT, 1, INDX, 1 )
C
C                 --------------------------
C                 ... COMPUTE IN-LINE RESULT
C                 --------------------------
C
                  DO 300 I = 1, NZTRUE
                      YTRUE (INDXT(I)) = YTRUE (INDXT(I))  +
     1                                   ATRUE * XTRUE(I)
  300             CONTINUE
C
C                 ---------------
C                 ... CALL ZAXPYI
C                 ---------------
C
                  CALL ZAXPYI ( NZ, A, X, INDX, Y )
C
C                 -----------------------------------------
C                 ... TEST ARGUMENTS OF ZAXPYI THAT ARE NOT
C                     SUPPOSE TO CHANGE.
C                 -----------------------------------------
C
                  IF  ( NZ .NE. NZTRUE )  THEN
                      COUNT = COUNT + 1
                      IF  ( COUNT .LE. ERRMAX )  THEN
                          WRITE ( NOUT, 1000 ) NZTRUE, ATRUE, KINDX,
     1                                         NZ
                      END IF
                  END IF
C
                  IF  ( A .NE. ATRUE )  THEN
                      COUNT = COUNT + 1
                      IF  ( COUNT .LE. ERRMAX )  THEN
                          WRITE ( NOUT, 1100 ) NZTRUE, ATRUE, KINDX,
     1                                         A
                      END IF
                  END IF
C
                  IF  ( .NOT. ZVSAME ( N, X, XTRUE ) )  THEN
                      COUNT = COUNT + 1
                      IF  ( COUNT .LE. ERRMAX )  THEN
                          WRITE ( NOUT, 1200 ) NZTRUE, ATRUE, KINDX
                      END IF
                  END IF
C
                  IF  ( .NOT. IVSAME ( N, INDX, INDXT ) )  THEN
                      COUNT = COUNT + 1
                      IF  ( COUNT .LE. ERRMAX )  THEN
                          WRITE ( NOUT, 1300 ) NZTRUE, ATRUE, KINDX
                      END IF
                  END IF
C
C                 ---------------------------
C                 ... TEST OUTPUT FROM ZAXPYI
C                 ---------------------------
C
                  DO 400 J = 1, N
                      IF  ( LIST(J) .EQ. -1 )  THEN
                          IF  ( Y(J) .NE. YTRUE(J) )  THEN
                              COUNT = COUNT + 1
                              IF  ( COUNT .LE. ERRMAX )  THEN
                                  WRITE ( NOUT, 1400 ) NZTRUE, ATRUE,
     1                                                 KINDX, J,
     2                                                 Y(J), YTRUE(J)
                              END IF
                          END IF
C
                      ELSE
C
                          S   = ABS ( Y(J) - YTRUE(J) )
                          T   = ABS ( ATRUE) * ABS ( XTRUE (LIST(J)))  +
     1                          ABS ( YTRUE(J))
                          ERR = S / ( EPSILN * T )
                          IF  ( ERR .GT. THRESH )  THEN
                              COUNT = COUNT + 1
                              IF  ( COUNT .LE. ERRMAX )  THEN
                                  WRITE ( NOUT, 1500 ) NZTRUE, ATRUE,
     1                                                 KINDX, J, Y(J),
     2                                                 YTRUE(J), ERR
                              END IF
                          END IF
C
                      END IF
C
  400             CONTINUE
C
  500         CONTINUE
C
  600     CONTINUE
C
  700 CONTINUE
C
C     ==================================================================
C
C     ------------------
C     ... END OF TESTING
C     ------------------
C
      ERRCNT = ERRCNT + COUNT
      IF  ( COUNT .NE. 0 )  GO TO 800
C
C     -----------------------------------
C     ... WRITE PASSED MESSAGE AND RETURN
C     -----------------------------------
C
      WRITE ( NOUT, 2700 )
      GO TO 900
C
C     -----------------------------------
C     ... WRITE FAILED MESSAGE AND RETURN
C     -----------------------------------
C
  800 WRITE ( NOUT, 2800 ) COUNT
C
C     ------------------------
C     ... END OF MODULE TZXPYI
C     ------------------------
C
  900 CONTINUE
      RETURN
C
C     ==================================================================
C
C     -----------
C     ... FORMATS
C     -----------
C
 1000 FORMAT ( 5X, 'ZAXPYI ALTERED NZ FOR TEST WITH NZ = ', I5,
     1             ' A = (', 1PD15.5, ',', 1PD15.5,
     2             ') AND THE INDX TYPE NO. ', I5
     3        /5X, 'ALTERED VALUE OF NZ = ', I5 )
C
 1100 FORMAT ( 5X, 'ZAXPYI ALTERED A FOR TEST WITH NZ = ', I5,
     1             ' A = (', 1PD15.5, ',', 1PD15.5,
     2             ') AND THE INDX TYPE NO. ', I5
     3        /5X, 'ALTERED VALUE OF A = (', 1PD15.5, ',',
     4              1PD15.5, ')' )
C
 1200 FORMAT ( 5X, 'ZAXPYI ALTERED ARRAY X FOR TEST WITH NZ = ', I5,
     1             ' A = (', 1PD15.5, ',', 1PD15.5,
     2             ') AND THE INDX TYPE NO. ', I5 )
C
 1300 FORMAT ( 5X, 'ZAXPYI ALTERED ARRAY INDX FOR TEST WITH NZ = ', I5,
     1             ' A = (', 1PD15.5, ',', 1PD15.5,
     2             ') AND THE INDX TYPE NO. ', I5 )
C
 1400 FORMAT ( 5X, 'ZAXPYI OUTPUT ARRAY Y IS INCORRECT FOR TEST WITH ',
     1             'NZ = ', I5, ' A = (', 1PD15.5, ',', 1PD15.5,
     2             ') AND THE INDX TYPE NO. ', I5
     3        /5X, 'INCORRECT COMPONENT NO. ', I5, ' HAS VALUE = (',
     4             1PD15.5, ',', 1PD15.5,
     5             ') TRUE VALUE = (', 1PD15.5, ',', 1PD15.5, ')' )
C
 1500 FORMAT ( 5X, 'ZAXPYI OUTPUT ARRAY Y IS INACCURATE FOR TEST WITH ',
     1             'NZ = ', I5, ' A = (', 1PD15.5, ',', 1PD15.5,
     2             ') AND THE INDX TYPE NO. ', I5
     3        /5X, 'INACCURATE COMPONENT NO. ', I5, ' HAS VALUE = (',
     4             1PD15.5, ',', 1PD15.5, ') TRUE VALUE = (',
     5             1PD15.5, ',', 1PD15.5, ')'
     6        /5X, 'ERROR = ', 1PD12.1 )
C
 2700 FORMAT ( /5X, 'ZAXPYI PASSED ALL TESTS.' )
C
 2800 FORMAT ( /5X, 'ZAXPYI FAILED', I10, ' TESTS.'  )
C
C     ==================================================================
C
      END
      SUBROUTINE   TZDTCI   ( NOUT,   EPSILN, THRESH, NZMAX2,
     1                        NUMNZ,  NZVALU,
     2                        X,      XSAVE,  XTRUE,  Y,      YSAVE,
     3                        YTRUE , INDX,   INDXT,  ERRCNT, ERRMAX )
C
C     ==================================================================
C     ==================================================================
C     ====  TZDTCI  --  CERTIFY  ZDOTCI                             ====
C     ==================================================================
C     ==================================================================
C
C     SUBROUTINE  TZDTCI  IS THE CERTIFICATION MODULE FOR THE SPARSE
C     BASIC LINEAR ALGEBRA SUBROUTINE MODULE  ZDOTCI.
C
C     WRITTEN BY      ROGER G GRIMES
C                     APRIL 1987
C
C     ==================================================================
C
C     -------------
C     ... ARGUMENTS
C     -------------
C
      INTEGER             NOUT,   NZMAX2, NUMNZ,  ERRCNT,
     1                    ERRMAX
C
      INTEGER             NZVALU (*),  INDX (*),    INDXT (*)
C
      DOUBLE PRECISION    EPSILN, THRESH
C
      COMPLEX*16          X (*),       XSAVE (*),   XTRUE (*),
     1                    Y (*),       YSAVE (*),   YTRUE (*)
C
C     -------------------
C     ... LOCAL VARIABLES
C     -------------------
C
      INTEGER             COUNT,  I,      ICLOBR, J,      KINDX,
     1                    KNZ,    N,      NZ,     NZTRUE
C
      DOUBLE PRECISION    ERR,    S,      T
C
      COMPLEX*16          CLOBBR, V,      W
C
C     --------------------
C     ... SUBPROGRAMS USED
C     --------------------
C
      LOGICAL             IVSAME, ZVSAME
C
      COMPLEX*16          ZDOTCI
C
      EXTERNAL            ICOPY,  ZCOPY,  ZINIT,  GNINDX,
     1                    IVSAME, ZVSAME, ZDOTCI
C
C     ==================================================================
C
C     ------------------
C     ... INITIALIZATION
C     ------------------
C
      COUNT     =   0
C
      CLOBBR    =   ( -1.0D10, -1.0D10 )
      ICLOBR    =   -10000000
C
C     ------------------------------------
C     ... GENERATE SOME VALUES FOR X AND Y
C     ------------------------------------
C
      DO 100 I = 1, NZMAX2
         XSAVE(I) = DCMPLX ( COS ( .6*DBLE(I) ), SIN ( .2*DBLE(I) ) )
         YSAVE(I) = DCMPLX ( SIN ( .7*DBLE(I) ), COS ( .9*DBLE(I) ) )
  100 CONTINUE
C
C     ------------------------
C     ... FOR EACH VALUE OF NZ
C     ------------------------
C
      DO 600 KNZ = 1, NUMNZ
C
          NZTRUE = NZVALU(KNZ)
          N      = 2 * MAX ( NZTRUE, 1 )
C
C         -------------------------------
C         ... FOR EACH KIND OF INDX ARRAY
C         -------------------------------
C
          DO 500 KINDX = 1, 5
C
              CALL GNINDX ( NZTRUE, N, ICLOBR, KINDX, INDXT )
C
C             -----------------------
C             ... GENERATE INPUT DATA
C             -----------------------
C
              I = MIN ( N, N-NZTRUE )
              J = N - I + 1
              CALL ZCOPY ( NZTRUE, XSAVE,  1, XTRUE, 1 )
              CALL ZINIT ( I,      CLOBBR, XTRUE(J), 1 )
              CALL ZINIT ( N,      CLOBBR, YTRUE, 1 )
C
              DO 200 I = 1, NZTRUE
                  YTRUE (INDXT(I)) = YSAVE (INDXT(I))
  200         CONTINUE
C
C             -------------------
C             ... COPY TRUE INPUT
C             -------------------
C
              NZ = NZTRUE
C
              CALL ZCOPY ( N, YTRUE, 1, Y, 1 )
              CALL ZCOPY ( N, XTRUE, 1, X, 1 )
              CALL ICOPY ( N, INDXT, 1, INDX, 1 )
C
C             --------------------------
C             ... COMPUTE IN-LINE RESULT
C             --------------------------
C
              V = ( 0.0D0, 0.0D0 )
C
              DO 300 I = 1, NZTRUE
                  V = V + DCONJG ( XTRUE(I) ) * YTRUE (INDXT(I))
  300         CONTINUE
C
C             --------------
C             ... CALL ZDOTCI
C             --------------
C
              W = ZDOTCI ( NZ, X, INDX, Y )
C
C             -----------------------------------------
C             ... TEST ARGUMENTS OF ZDOTCI THAT ARE NOT
C                     SUPPOSE TO CHANGE.
C             -----------------------------------------
C
              IF  ( NZ .NE. NZTRUE )  THEN
                  COUNT = COUNT + 1
                  IF  ( COUNT .LE. ERRMAX )  THEN
                      WRITE ( NOUT, 1000 ) NZTRUE, KINDX, NZ
                  END IF
              END IF
C
              IF  ( .NOT. ZVSAME ( N, X, XTRUE ) )  THEN
                  COUNT = COUNT + 1
                  IF  ( COUNT .LE. ERRMAX )  THEN
                      WRITE ( NOUT, 1100 ) NZTRUE, KINDX
                  END IF
              END IF
C
              IF  ( .NOT. IVSAME ( N, INDX, INDXT ) )  THEN
                  COUNT = COUNT + 1
                  IF  ( COUNT .LE. ERRMAX )  THEN
                      WRITE ( NOUT, 1200 ) NZTRUE, KINDX
                  END IF
              END IF
C
              IF  ( .NOT. ZVSAME ( N, Y, YTRUE ) )  THEN
                  COUNT = COUNT + 1
                  IF  ( COUNT .LE. ERRMAX )  THEN
                      WRITE ( NOUT, 1300 ) NZTRUE, KINDX
                  END IF
              END IF
C
C             --------------------------
C             ... TEST OUTPUT FROM ZDOTCI
C             --------------------------
C
              S = ABS ( V - W )
C
              T = 0.0D0
              DO 400 I = 1, NZTRUE
                  T = T + ABS ( XTRUE(I) * YTRUE (INDXT(I)) )
  400         CONTINUE
C
              IF  ( T .EQ. 0.0D0 )  T = 1.0D0
C
              ERR = S / ( EPSILN * T )
C
              IF  ( ERR .GT. THRESH )  THEN
                  COUNT = COUNT + 1
                  IF  ( COUNT .LE. ERRMAX )  THEN
                      WRITE ( NOUT, 1400 ) NZTRUE, KINDX,
     1                                     W, V, ERR
                  END IF
              END IF
C
  500     CONTINUE
C
  600 CONTINUE
C
C     ==================================================================
C
C     ------------------
C     ... END OF TESTING
C     ------------------
C
      ERRCNT = ERRCNT + COUNT
      IF  ( COUNT .NE. 0 )  GO TO 800
C
C     -----------------------------------
C     ... WRITE PASSED MESSAGE AND RETURN
C     -----------------------------------
C
      WRITE ( NOUT, 2700 )
      GO TO 900
C
C     -----------------------------------
C     ... WRITE FAILED MESSAGE AND RETURN
C     -----------------------------------
C
  800 WRITE ( NOUT, 2800 ) COUNT
C
C     ------------------------
C     ... END OF MODULE TZDTCI
C     ------------------------
C
  900 CONTINUE
      RETURN
C
C     ==================================================================
C
C     -----------
C     ... FORMATS
C     -----------
C
 1000 FORMAT ( 5X, 'ZDOTCI ALTERED NZ FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5,
     2             '.  ALTERED VALUE OF NZ = ', I5 )
C
 1100 FORMAT ( 5X, 'ZDOTCI ALTERED ARRAY X FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5 )
C
 1200 FORMAT ( 5X, 'ZDOTCI ALTERED ARRAY INDX FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5 )
C
 1300 FORMAT ( 5X, 'ZDOTCI ALTERED ARRAY Y FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5 )
C
 1400 FORMAT ( 5X, 'ZDOTCI OUTPUT W IS INACCURATE FOR TEST WITH ',
     1             'NZ = ', I5, ' AND THE INDX TYPE NO. ', I5
     2        /5X, 'ZDOTCI HAS VALUE = (', 1PD15.5, ',', 1PD15.5,
     3             ') TRUE VALUE = (', 1PD15.5, ',', 1PD15.5,
     4             ') ERROR = ', 1PD12.1 )
C
 2700 FORMAT ( /5X, 'ZDOTCI PASSED ALL TESTS.' )
C
 2800 FORMAT ( /5X, 'ZDOTCI FAILED', I10, ' TESTS.'  )
C
C     ==================================================================
C
      END
      SUBROUTINE   TZDTUI   ( NOUT,   EPSILN, THRESH, NZMAX2,
     1                        NUMNZ,  NZVALU,
     2                        X,      XSAVE,  XTRUE,  Y,      YSAVE,
     3                        YTRUE , INDX,   INDXT,  ERRCNT, ERRMAX )
C
C     ==================================================================
C     ==================================================================
C     ====  TZDTUI  --  CERTIFY  ZDOTUI                             ====
C     ==================================================================
C     ==================================================================
C
C     SUBROUTINE  TZDTUI  IS THE CERTIFICATION MODULE FOR THE SPARSE
C     BASIC LINEAR ALGEBRA SUBROUTINE MODULE  ZDOTUI.
C
C     WRITTEN BY      ROGER G GRIMES
C                     APRIL 1987
C
C     ==================================================================
C
C     -------------
C     ... ARGUMENTS
C     -------------
C
      INTEGER             NOUT,   NZMAX2, NUMNZ,  ERRCNT,
     1                    ERRMAX
C
      INTEGER             NZVALU (*),  INDX (*),    INDXT (*)
C
      DOUBLE PRECISION    EPSILN, THRESH
C
      COMPLEX*16          X (*),       XSAVE (*),   XTRUE (*),
     1                    Y (*),       YSAVE (*),   YTRUE (*)
C
C     -------------------
C     ... LOCAL VARIABLES
C     -------------------
C
      INTEGER             COUNT,  I,      ICLOBR, J,      KINDX,
     1                    KNZ,    N,      NZ,     NZTRUE
C
      DOUBLE PRECISION    ERR,    S,      T
C
      COMPLEX*16          CLOBBR, V,      W
C
C     --------------------
C     ... SUBPROGRAMS USED
C     --------------------
C
      LOGICAL             IVSAME, ZVSAME
C
      COMPLEX*16          ZDOTUI
C
      EXTERNAL            ICOPY,  ZCOPY,  ZINIT,  GNINDX,
     1                    IVSAME, ZVSAME, ZDOTUI
C
C     ==================================================================
C
C     ------------------
C     ... INITIALIZATION
C     ------------------
C
      COUNT     =   0
C
      CLOBBR    =   ( -1.0D10, -1.0D10 )
      ICLOBR    =   -10000000
C
C     ------------------------------------
C     ... GENERATE SOME VALUES FOR X AND Y
C     ------------------------------------
C
      DO 100 I = 1, NZMAX2
         XSAVE(I) = DCMPLX ( COS ( .6*DBLE(I) ), SIN ( .2*DBLE(I) ) )
         YSAVE(I) = DCMPLX ( SIN ( .7*DBLE(I) ), COS ( .9*DBLE(I) ) )
  100 CONTINUE
C
C     ------------------------
C     ... FOR EACH VALUE OF NZ
C     ------------------------
C
      DO 600 KNZ = 1, NUMNZ
C
          NZTRUE = NZVALU(KNZ)
          N      = 2 * MAX ( NZTRUE, 1 )
C
C         -------------------------------
C         ... FOR EACH KIND OF INDX ARRAY
C         -------------------------------
C
          DO 500 KINDX = 1, 5
C
              CALL GNINDX ( NZTRUE, N, ICLOBR, KINDX, INDXT )
C
C             -----------------------
C             ... GENERATE INPUT DATA
C             -----------------------
C
              I = MIN ( N, N-NZTRUE )
              J = N - I + 1
              CALL ZCOPY ( NZTRUE, XSAVE,  1, XTRUE, 1 )
              CALL ZINIT ( I,      CLOBBR, XTRUE(J), 1 )
              CALL ZINIT ( N,      CLOBBR, YTRUE, 1 )
C
              DO 200 I = 1, NZTRUE
                  YTRUE (INDXT(I)) = YSAVE (INDXT(I))
  200         CONTINUE
C
C             -------------------
C             ... COPY TRUE INPUT
C             -------------------
C
              NZ = NZTRUE
C
              CALL ZCOPY ( N, YTRUE, 1, Y, 1 )
              CALL ZCOPY ( N, XTRUE, 1, X, 1 )
              CALL ICOPY ( N, INDXT, 1, INDX, 1 )
C
C             --------------------------
C             ... COMPUTE IN-LINE RESULT
C             --------------------------
C
              V = ( 0.0D0, 0.0D0 )
C
              DO 300 I = 1, NZTRUE
                  V = V + XTRUE(I) * YTRUE (INDXT(I))
  300         CONTINUE
C
C             --------------
C             ... CALL ZDOTUI
C             --------------
C
              W = ZDOTUI ( NZ, X, INDX, Y )
C
C             -----------------------------------------
C             ... TEST ARGUMENTS OF ZDOTUI THAT ARE NOT
C                     SUPPOSE TO CHANGE.
C             -----------------------------------------
C
              IF  ( NZ .NE. NZTRUE )  THEN
                  COUNT = COUNT + 1
                  IF  ( COUNT .LE. ERRMAX )  THEN
                      WRITE ( NOUT, 1000 ) NZTRUE, KINDX, NZ
                  END IF
              END IF
C
              IF  ( .NOT. ZVSAME ( N, X, XTRUE ) )  THEN
                  COUNT = COUNT + 1
                  IF  ( COUNT .LE. ERRMAX )  THEN
                      WRITE ( NOUT, 1100 ) NZTRUE, KINDX
                  END IF
              END IF
C
              IF  ( .NOT. IVSAME ( N, INDX, INDXT ) )  THEN
                  COUNT = COUNT + 1
                  IF  ( COUNT .LE. ERRMAX )  THEN
                      WRITE ( NOUT, 1200 ) NZTRUE, KINDX
                  END IF
              END IF
C
              IF  ( .NOT. ZVSAME ( N, Y, YTRUE ) )  THEN
                  COUNT = COUNT + 1
                  IF  ( COUNT .LE. ERRMAX )  THEN
                      WRITE ( NOUT, 1300 ) NZTRUE, KINDX
                  END IF
              END IF
C
C             --------------------------
C             ... TEST OUTPUT FROM ZDOTUI
C             --------------------------
C
              S = ABS ( V - W )
C
              T = 0.0D0
              DO 400 I = 1, NZTRUE
                  T = T + ABS ( XTRUE(I) * YTRUE (INDXT(I)) )
  400         CONTINUE
C
              IF  ( T .EQ. 0.0D0 )  T = 1.0D0
C
              ERR = S / ( EPSILN * T )
C
              IF  ( ERR .GT. THRESH )  THEN
                  COUNT = COUNT + 1
                  IF  ( COUNT .LE. ERRMAX )  THEN
                      WRITE ( NOUT, 1400 ) NZTRUE, KINDX,
     1                                     W, V, ERR
                  END IF
              END IF
C
  500     CONTINUE
C
  600 CONTINUE
C
C     ==================================================================
C
C     ------------------
C     ... END OF TESTING
C     ------------------
C
      ERRCNT = ERRCNT + COUNT
      IF  ( COUNT .NE. 0 )  GO TO 800
C
C     -----------------------------------
C     ... WRITE PASSED MESSAGE AND RETURN
C     -----------------------------------
C
      WRITE ( NOUT, 2700 )
      GO TO 900
C
C     -----------------------------------
C     ... WRITE FAILED MESSAGE AND RETURN
C     -----------------------------------
C
  800 WRITE ( NOUT, 2800 ) COUNT
C
C     ------------------------
C     ... END OF MODULE TZDTUI
C     ------------------------
C
  900 CONTINUE
      RETURN
C
C     ==================================================================
C
C     -----------
C     ... FORMATS
C     -----------
C
 1000 FORMAT ( 5X, 'ZDOTUI ALTERED NZ FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5,
     2             '.  ALTERED VALUE OF NZ = ', I5 )
C
 1100 FORMAT ( 5X, 'ZDOTUI ALTERED ARRAY X FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5 )
C
 1200 FORMAT ( 5X, 'ZDOTUI ALTERED ARRAY INDX FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5 )
C
 1300 FORMAT ( 5X, 'ZDOTUI ALTERED ARRAY Y FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5 )
C
 1400 FORMAT ( 5X, 'ZDOTUI OUTPUT W IS INACCURATE FOR TEST WITH ',
     1             'NZ = ', I5, ' AND THE INDX TYPE NO. ', I5
     2        /5X, 'ZDOTUI HAS VALUE = (', 1PD15.5, ',', 1PD15.5,
     3             ') TRUE VALUE = (', 1PD15.5, ',', 1PD15.5,
     4             ') ERROR = ', 1PD12.1 )
C
 2700 FORMAT ( /5X, 'ZDOTUI PASSED ALL TESTS.' )
C
 2800 FORMAT ( /5X, 'ZDOTUI FAILED', I10, ' TESTS.'  )
C
C     ==================================================================
C
      END
      SUBROUTINE   TZGTHR   ( NOUT,   NZMAX2, NUMNZ,  NZVALU,
     1                        X,      XSAVE,  XTRUE,  Y,      YSAVE,
     2                        YTRUE , INDX,   INDXT,  ERRCNT, ERRMAX )
C
C     ==================================================================
C     ==================================================================
C     ====  TZGTHR  --  CERTIFY  ZGTHR                              ====
C     ==================================================================
C     ==================================================================
C
C     SUBROUTINE  TZGTHR  IS THE CERTIFICATION MODULE FOR THE SPARSE
C     BASIC LINEAR ALGEBRA SUBROUTINE MODULE  ZGTHR.
C
C     WRITTEN BY      ROGER G GRIMES
C                     APRIL 1987
C
C     ==================================================================
C
C     -------------
C     ... ARGUMENTS
C     -------------
C
      INTEGER             NOUT,   NZMAX2, NUMNZ,  ERRCNT,
     1                    ERRMAX
C
      INTEGER             NZVALU (*),  INDX (*),    INDXT (*)
C
      COMPLEX*16          X (*),       XSAVE (*),   XTRUE (*),
     1                    Y (*),       YSAVE (*),   YTRUE (*)
C
C     -------------------
C     ... LOCAL VARIABLES
C     -------------------
C
      INTEGER             COUNT,  I,      ICLOBR, KINDX,
     1                    KNZ,    N,      NZ,     NZTRUE
C
      COMPLEX*16          CLOBBR
C
C     --------------------
C     ... SUBPROGRAMS USED
C     --------------------
C
      LOGICAL             IVSAME, ZVSAME
C
      EXTERNAL            ICOPY,  ZCOPY,  ZINIT,  GNINDX,
     1                    IVSAME, ZVSAME, ZGTHR
C
C     ==================================================================
C
C     ------------------
C     ... INITIALIZATION
C     ------------------
C
      COUNT     =   0
C
      CLOBBR    =   ( -1.0D10, -1.0D10 )
      ICLOBR    =   -10000000
C
C     ------------------------------------
C     ... GENERATE SOME VALUES FOR X AND Y
C     ------------------------------------
C
      DO 100 I = 1, NZMAX2
         XSAVE(I) = DCMPLX ( COS ( .6*DBLE(I) ), SIN ( .2*DBLE(I) ) )
         YSAVE(I) = DCMPLX ( SIN ( .7*DBLE(I) ), COS ( .9*DBLE(I) ) )
  100 CONTINUE
C
C     ------------------------
C     ... FOR EACH VALUE OF NZ
C     ------------------------
C
      DO 600 KNZ = 1, NUMNZ
C
          NZTRUE = NZVALU(KNZ)
          N      = 2 * MAX ( NZTRUE, 1 )
C
C         -------------------------------
C         ... FOR EACH KIND OF INDX ARRAY
C         -------------------------------
C
          DO 500 KINDX = 1, 5
C
              CALL GNINDX ( NZTRUE, N, ICLOBR, KINDX, INDXT )
C
C             -----------------------
C             ... GENERATE INPUT DATA
C             -----------------------
C
              CALL ZINIT ( N, CLOBBR, XTRUE, 1 )
              CALL ZINIT ( N, CLOBBR, YTRUE, 1 )
C
              DO 200 I = 1, NZTRUE
                  YTRUE (INDXT(I)) = YSAVE (INDXT(I))
  200         CONTINUE
C
C             -------------------
C             ... COPY TRUE INPUT
C             -------------------
C
              NZ = NZTRUE
C
              CALL ZCOPY ( N, YTRUE, 1, Y, 1 )
              CALL ZCOPY ( N, XTRUE, 1, X, 1 )
              CALL ICOPY ( N, INDXT, 1, INDX, 1 )
C
C             --------------------------
C             ... COMPUTE IN-LINE RESULT
C             --------------------------
C
              DO 300 I = 1, NZTRUE
                  XTRUE (I) = YTRUE (INDXT(I))
  300         CONTINUE
C
C             --------------
C             ... CALL ZGTHR
C             --------------
C
              CALL ZGTHR ( NZ, Y, X, INDX )
C
C             ----------------------------------------
C             ... TEST ARGUMENTS OF ZGTHR THAT ARE NOT
C                 SUPPOSE TO CHANGE.
C             ----------------------------------------
C
              IF  ( NZ .NE. NZTRUE )  THEN
                  COUNT = COUNT + 1
                  IF  ( COUNT .LE. ERRMAX )  THEN
                      WRITE ( NOUT, 1000 ) NZTRUE, KINDX, NZ
                  END IF
              END IF
C
              IF  ( .NOT. ZVSAME ( N, Y, YTRUE ) )  THEN
                  COUNT = COUNT + 1
                  IF  ( COUNT .LE. ERRMAX )  THEN
                      WRITE ( NOUT, 1100 ) NZTRUE, KINDX
                  END IF
              END IF
C
              IF  ( .NOT. IVSAME ( N, INDX, INDXT ) )  THEN
                  COUNT = COUNT + 1
                  IF  ( COUNT .LE. ERRMAX )  THEN
                      WRITE ( NOUT, 1200 ) NZTRUE, KINDX
                  END IF
              END IF
C
C             --------------------------
C             ... TEST OUTPUT FROM ZGTHR
C             --------------------------
C
              DO 400 I = 1, N
                  IF  ( X(I) .NE. XTRUE(I) )  THEN
                      COUNT = COUNT + 1
                      IF  ( COUNT .LE. ERRMAX )  THEN
                          WRITE ( NOUT, 1300 ) NZTRUE, KINDX, I,
     1                                         X(I), XTRUE(I)
                      END IF
                  END IF
  400         CONTINUE
C
  500     CONTINUE
C
  600 CONTINUE
C
C     ==================================================================
C
C     ------------------
C     ... END OF TESTING
C     ------------------
C
      ERRCNT = ERRCNT + COUNT
      IF  ( COUNT .NE. 0 )  GO TO 800
C
C     -----------------------------------
C     ... WRITE PASSED MESSAGE AND RETURN
C     -----------------------------------
C
      WRITE ( NOUT, 2700 )
      GO TO 900
C
C     -----------------------------------
C     ... WRITE FAILED MESSAGE AND RETURN
C     -----------------------------------
C
  800 WRITE ( NOUT, 2800 ) COUNT
C
C     ------------------------
C     ... END OF MODULE TZGTHR
C     ------------------------
C
  900 CONTINUE
      RETURN
C
C     ==================================================================
C
C     -----------
C     ... FORMATS
C     -----------
C
 1000 FORMAT ( 5X, 'ZGTHR ALTERED NZ FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5,
     2             '.  ALTERED VALUE OF NZ = ', I5 )
C
 1100 FORMAT ( 5X, 'ZGTHR ALTERED ARRAY Y FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5 )
C
 1200 FORMAT ( 5X, 'ZGTHR ALTERED ARRAY INDX FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5 )
C
 1300 FORMAT ( 5X, 'ZGTHR OUTPUT ARRAY X IS INCORRECT FOR TEST WITH ',
     1             'NZ = ', I5, ' AND THE INDX TYPE NO. ', I5
     2        /5X, 'INACCURATE COMPONENT NO. ', I5, ' HAS VALUE = (',
     3             1PD15.5, ',', 1PD15.5, ') TRUE VALUE = (',
     4             1PD15.5, ',', 1PD15.5, ')' )
C
 2700 FORMAT ( /5X, 'ZGTHR  PASSED ALL TESTS.' )
C
 2800 FORMAT ( /5X, 'ZGTHR  FAILED', I10, ' TESTS.'  )
C
C     ==================================================================
C
      END
      SUBROUTINE   TZGTHZ   ( NOUT,   NZMAX2, NUMNZ,  NZVALU,
     1                        X,      XSAVE,  XTRUE,  Y,      YSAVE,
     2                        YTRUE , INDX,   INDXT,  ERRCNT, ERRMAX )
C
C     ==================================================================
C     ==================================================================
C     ====  TZGTHZ  --  CERTIFY  ZGTHRZ                             ====
C     ==================================================================
C     ==================================================================
C
C     SUBROUTINE  TZGTHZ  IS THE CERTIFICATION MODULE FOR THE SPARSE
C     BASIC LINEAR ALGEBRA SUBROUTINE MODULE  ZGTHRZ.
C
C     WRITTEN BY      ROGER G GRIMES
C                     APRIL 1987
C
C     ==================================================================
C
C     -------------
C     ... ARGUMENTS
C     -------------
C
      INTEGER             NOUT,   NZMAX2, NUMNZ,  ERRCNT,
     1                    ERRMAX
C
      INTEGER             NZVALU (*),  INDX (*),    INDXT (*)
C
      COMPLEX*16          X (*),       XSAVE (*),   XTRUE (*),
     1                    Y (*),       YSAVE (*),   YTRUE (*)
C
C     -------------------
C     ... LOCAL VARIABLES
C     -------------------
C
      INTEGER             COUNT,  I,      ICLOBR, KINDX,
     1                    KNZ,    N,      NZ,     NZTRUE
C
      COMPLEX*16          CLOBBR
C
C     --------------------
C     ... SUBPROGRAMS USED
C     --------------------
C
      LOGICAL             IVSAME, ZVSAME
C
      EXTERNAL            ICOPY,  ZCOPY,  ZINIT,  GNINDX,
     1                    IVSAME, ZVSAME, ZGTHRZ
C
C     ==================================================================
C
C     ------------------
C     ... INITIALIZATION
C     ------------------
C
      COUNT     =   0
C
      CLOBBR    =   ( -1.0D10, -1.0D10 )
      ICLOBR    =   -10000000
C
C     ------------------------------------
C     ... GENERATE SOME VALUES FOR X AND Y
C     ------------------------------------
C
      DO 100 I = 1, NZMAX2
         XSAVE(I) = DCMPLX ( COS ( .6*DBLE(I) ), SIN ( .2*DBLE(I) ) )
         YSAVE(I) = DCMPLX ( SIN ( .7*DBLE(I) ), COS ( .9*DBLE(I) ) )
  100 CONTINUE
C
C     ------------------------
C     ... FOR EACH VALUE OF NZ
C     ------------------------
C
      DO 600 KNZ = 1, NUMNZ
C
          NZTRUE = NZVALU(KNZ)
          N      = 2 * MAX ( NZTRUE, 1 )
C
C         -------------------------------
C         ... FOR EACH KIND OF INDX ARRAY
C         -------------------------------
C
          DO 500 KINDX = 1, 5
C
              CALL GNINDX ( NZTRUE, N, ICLOBR, KINDX, INDXT )
C
C             -----------------------
C             ... GENERATE INPUT DATA
C             -----------------------
C
              CALL ZINIT ( N, CLOBBR, XTRUE, 1 )
              CALL ZINIT ( N, CLOBBR, YTRUE, 1 )
C
              DO 200 I = 1, NZTRUE
                  YTRUE (INDXT(I)) = YSAVE (INDXT(I))
  200         CONTINUE
C
C             -------------------
C             ... COPY TRUE INPUT
C             -------------------
C
              NZ = NZTRUE
C
              CALL ZCOPY ( N, YTRUE, 1, Y, 1 )
              CALL ZCOPY ( N, XTRUE, 1, X, 1 )
              CALL ICOPY ( N, INDXT, 1, INDX, 1 )
C
C             --------------------------
C             ... COMPUTE IN-LINE RESULT
C             --------------------------
C
              DO 300 I = 1, NZTRUE
                  XTRUE (I) = YTRUE (INDXT(I))
                  YTRUE(INDXT(I)) = ( 0.0D0, 0.0D0 )
  300         CONTINUE
C
C             ---------------
C             ... CALL ZGTHRZ
C             ---------------
C
              CALL ZGTHRZ ( NZ, Y, X, INDX )
C
C             -----------------------------------------
C             ... TEST ARGUMENTS OF ZGTHRZ THAT ARE NOT
C                 SUPPOSE TO CHANGE.
C             -----------------------------------------
C
              IF  ( NZ .NE. NZTRUE )  THEN
                  COUNT = COUNT + 1
                  IF  ( COUNT .LE. ERRMAX )  THEN
                      WRITE ( NOUT, 1000 ) NZTRUE, KINDX, NZ
                  END IF
              END IF
C
              IF  ( .NOT. IVSAME ( N, INDX, INDXT ) )  THEN
                  COUNT = COUNT + 1
                  IF  ( COUNT .LE. ERRMAX )  THEN
                      WRITE ( NOUT, 1100 ) NZTRUE, KINDX
                  END IF
              END IF
C
C             ---------------------------
C             ... TEST OUTPUT FROM ZGTHRZ
C             ---------------------------
C
              DO 400 I = 1, N
C
                  IF  ( X(I) .NE. XTRUE(I) )  THEN
                      COUNT = COUNT + 1
                      IF  ( COUNT .LE. ERRMAX )  THEN
                          WRITE ( NOUT, 1200 ) NZTRUE, KINDX, I,
     1                                         X(I), XTRUE(I)
                      END IF
                  END IF
C
                  IF  ( Y(I) .NE. YTRUE(I) )  THEN
                      COUNT = COUNT + 1
                      IF  ( COUNT .LE. ERRMAX )  THEN
                          WRITE ( NOUT, 1300 ) NZTRUE, KINDX, I,
     1                                         Y(I), YTRUE(I)
                      END IF
                  END IF
C
  400         CONTINUE
C
  500     CONTINUE
C
  600 CONTINUE
C
C     ==================================================================
C
C     ------------------
C     ... END OF TESTING
C     ------------------
C
      ERRCNT = ERRCNT + COUNT
      IF  ( COUNT .NE. 0 )  GO TO 800
C
C     -----------------------------------
C     ... WRITE PASSED MESSAGE AND RETURN
C     -----------------------------------
C
      WRITE ( NOUT, 2700 )
      GO TO 900
C
C     -----------------------------------
C     ... WRITE FAILED MESSAGE AND RETURN
C     -----------------------------------
C
  800 WRITE ( NOUT, 2800 ) COUNT
C
C     ------------------------
C     ... END OF MODULE TZGTHZ
C     ------------------------
C
  900 CONTINUE
      RETURN
C
C     ==================================================================
C
C     -----------
C     ... FORMATS
C     -----------
C
 1000 FORMAT ( 5X, 'ZGTHRZ ALTERED NZ FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5,
     2             '.  ALTERED VALUE OF NZ = ', I5 )
C
 1100 FORMAT ( 5X, 'ZGTHRZ ALTERED ARRAY INDX FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5 )
C
 1200 FORMAT ( 5X, 'ZGTHRZ OUTPUT ARRAY X IS INCORRECT FOR TEST WITH ',
     1             'NZ = ', I5, ' AND THE INDX TYPE NO. ', I5
     2        /5X, 'INACCURATE COMPONENT NO. ', I5, ' HAS VALUE = (',
     3             1PD15.5, ',', 1PD15.5, ') TRUE VALUE = (',
     4             1PD15.5, ',', 1PD15.5, ')' )
C
 1300 FORMAT ( 5X, 'ZGTHRZ OUTPUT ARRAY Y IS INCORRECT FOR TEST WITH ',
     1             'NZ = ', I5, ' AND THE INDX TYPE NO. ', I5
     2        /5X, 'INACCURATE COMPONENT NO. ', I5, ' HAS VALUE = (',
     3             1PD15.5, ',', 1PD15.5, ') TRUE VALUE = (',
     4             1PD15.5, ',', 1PD15.5, ')' )
C
 2700 FORMAT ( /5X, 'ZGTHRZ PASSED ALL TESTS.' )
C
 2800 FORMAT ( /5X, 'ZGTHRZ FAILED', I10, ' TESTS.'  )
C
C     ==================================================================
C
      END
      SUBROUTINE   TZSCTR   ( NOUT,   NZMAX2, NUMNZ,  NZVALU,
     1                        X,      XSAVE,  XTRUE,  Y,      YSAVE,
     2                        YTRUE , INDX,   INDXT,  ERRCNT, ERRMAX )
C
C     ==================================================================
C     ==================================================================
C     ====  TZSCTR  --  CERTIFY  ZSCTR                              ====
C     ==================================================================
C     ==================================================================
C
C     SUBROUTINE  TZSCTR  IS THE CERTIFICATION MODULE FOR THE SPARSE
C     BASIC LINEAR ALGEBRA SUBROUTINE MODULE  ZSCTR.
C
C     WRITTEN BY      ROGER G GRIMES
C                     APRIL 1987
C
C     ==================================================================
C
C     -------------
C     ... ARGUMENTS
C     -------------
C
      INTEGER             NOUT,   NZMAX2, NUMNZ,  ERRCNT,
     1                    ERRMAX
C
      INTEGER             NZVALU (*),  INDX (*),    INDXT (*)
C
      COMPLEX*16          X (*),       XSAVE (*),   XTRUE (*),
     1                    Y (*),       YSAVE (*),   YTRUE (*)
C
C     -------------------
C     ... LOCAL VARIABLES
C     -------------------
C
      INTEGER             COUNT,  I,      ICLOBR, J,      KINDX,
     1                    KNZ,    N,      NZ,     NZTRUE
C
      COMPLEX*16          CLOBBR
C
C     --------------------
C     ... SUBPROGRAMS USED
C     --------------------
C
      LOGICAL             IVSAME, ZVSAME
C
      EXTERNAL            ICOPY,  ZCOPY,  ZINIT,  GNINDX,
     1                    IVSAME, ZVSAME, ZSCTR
C
C     ==================================================================
C
C     ------------------
C     ... INITIALIZATION
C     ------------------
C
      COUNT     =   0
C
      CLOBBR    =   ( -1.0D10, -1.0D10 )
      ICLOBR    =   -10000000
C
C     ------------------------------------
C     ... GENERATE SOME VALUES FOR X AND Y
C     ------------------------------------
C
      DO 100 I = 1, NZMAX2
         XSAVE(I) = DCMPLX ( COS ( .6*DBLE(I) ), SIN ( .2*DBLE(I) ) )
         YSAVE(I) = DCMPLX ( SIN ( .7*DBLE(I) ), COS ( .9*DBLE(I) ) )
  100 CONTINUE
C
C     ------------------------
C     ... FOR EACH VALUE OF NZ
C     ------------------------
C
      DO 600 KNZ = 1, NUMNZ
C
          NZTRUE = NZVALU(KNZ)
          N      = 2 * MAX ( NZTRUE, 1 )
C
C         -------------------------------
C         ... FOR EACH KIND OF INDX ARRAY
C         -------------------------------
C
          DO 500 KINDX = 1, 5
C
              CALL GNINDX ( NZTRUE, N, ICLOBR, KINDX, INDXT )
C
C             -----------------------
C             ... GENERATE INPUT DATA
C             -----------------------
C
              I = MIN ( N, N-NZTRUE )
              J = N - I + 1
              CALL ZCOPY ( NZTRUE, XSAVE,  1, XTRUE, 1 )
              CALL ZINIT ( I,      CLOBBR, XTRUE(J), 1 )
              CALL ZINIT ( N,      CLOBBR, YTRUE, 1 )
C
C             -------------------
C             ... COPY TRUE INPUT
C             -------------------
C
              NZ = NZTRUE
C
              CALL ZCOPY ( N, YTRUE, 1, Y, 1 )
              CALL ZCOPY ( N, XTRUE, 1, X, 1 )
              CALL ICOPY ( N, INDXT, 1, INDX, 1 )
C
C             --------------------------
C             ... COMPUTE IN-LINE RESULT
C             --------------------------
C
              DO 300 I = 1, NZTRUE
                  YTRUE (INDXT(I)) = XTRUE (I)
  300         CONTINUE
C
C             --------------
C             ... CALL ZSCTR
C             --------------
C
              CALL ZSCTR ( NZ, X, INDX, Y )
C
C             ----------------------------------------
C             ... TEST ARGUMENTS OF ZSCTR THAT ARE NOT
C                 SUPPOSE TO CHANGE.
C             ----------------------------------------
C
              IF  ( NZ .NE. NZTRUE )  THEN
                  COUNT = COUNT + 1
                  IF  ( COUNT .LE. ERRMAX )  THEN
                      WRITE ( NOUT, 1000 ) NZTRUE, KINDX, NZ
                  END IF
              END IF
C
              IF  ( .NOT. ZVSAME ( N, X, XTRUE ) )  THEN
                  COUNT = COUNT + 1
                  IF  ( COUNT .LE. ERRMAX )  THEN
                      WRITE ( NOUT, 1100 ) NZTRUE, KINDX
                  END IF
              END IF
C
              IF  ( .NOT. IVSAME ( N, INDX, INDXT ) )  THEN
                  COUNT = COUNT + 1
                  IF  ( COUNT .LE. ERRMAX )  THEN
                      WRITE ( NOUT, 1200 ) NZTRUE, KINDX
                  END IF
              END IF
C
C             --------------------------
C             ... TEST OUTPUT FROM ZSCTR
C             --------------------------
C
              DO 400 I = 1, N
                  IF  ( Y(I) .NE. YTRUE(I) )  THEN
                      COUNT = COUNT + 1
                      IF  ( COUNT .LE. ERRMAX )  THEN
                          WRITE ( NOUT, 1300 ) NZTRUE, KINDX, I,
     1                                         Y(I), YTRUE(I)
                      END IF
                  END IF
  400         CONTINUE
C
  500     CONTINUE
C
  600 CONTINUE
C
C     ==================================================================
C
C     ------------------
C     ... END OF TESTING
C     ------------------
C
      ERRCNT = ERRCNT + COUNT
      IF  ( COUNT .NE. 0 )  GO TO 800
C
C     -----------------------------------
C     ... WRITE PASSED MESSAGE AND RETURN
C     -----------------------------------
C
      WRITE ( NOUT, 2700 )
      GO TO 900
C
C     -----------------------------------
C     ... WRITE FAILED MESSAGE AND RETURN
C     -----------------------------------
C
  800 WRITE ( NOUT, 2800 ) COUNT
C
C     ------------------------
C     ... END OF MODULE TZSCTR
C     ------------------------
C
  900 CONTINUE
      RETURN
C
C     ==================================================================
C
C     -----------
C     ... FORMATS
C     -----------
C
 1000 FORMAT ( 5X, 'ZSCTR ALTERED NZ FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5,
     2             '.  ALTERED VALUE OF NZ = ', I5 )
C
 1100 FORMAT ( 5X, 'ZSCTR ALTERED ARRAY X FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5 )
C
 1200 FORMAT ( 5X, 'ZSCTR ALTERED ARRAY INDX FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5 )
C
 1300 FORMAT ( 5X, 'ZSCTR OUTPUT ARRAY Y IS INCORRECT FOR TEST WITH ',
     1             'NZ = ', I5, ' AND THE INDX TYPE NO. ', I5
     2        /5X, 'INACCURATE COMPONENT NO. ', I5, ' HAS VALUE = (',
     3             1PD15.5, ',', 1PD15.5, ') TRUE VALUE = (',
     4             1PD15.5, ',', 1PD15.5, ')' )
C
 2700 FORMAT ( /5X, 'ZSCTR  PASSED ALL TESTS.' )
C
 2800 FORMAT ( /5X, 'ZSCTR  FAILED', I10, ' TESTS.'  )
C
C     ==================================================================
C
      END
