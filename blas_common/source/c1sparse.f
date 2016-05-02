      PROGRAM   TCSPBL
C
C     ==================================================================
C     ==================================================================
C     ====  TCSPBL -- CERTIFY COMPLEX SPARSE BLAS                   ====
C     ==================================================================
C     ==================================================================
C
C     TCSPBL IS THE CERTIFICATION PROGRAM FOR THE COMPLEX PRECISION
C     SPARSE BLAS.  THE APPROACH USED TO CERTIFY THE SPARSE BLAS
C     IS AS FOLLOWS:
C
C     1.  READ IN USER SPECIFIED INPUT ON OUTPUT UNIT, THRESHOLD VALUE
C         FOR TEST RATIO, AND THE SPECIFICATIONS FOR NZ, AND A.
C     2.  VERIFY THE CORRECTNESS OF THE USER SPECIFIED INPUT AND
C         ECHO TO THE OUTPUT UNIT.
C     3.  FOR EACH SUBPROGRAM IN THE COMPLEX PRECISION SPARSE BLAS
C         PERFORM ALL THE USER SPECIFIED TESTS AND PRINT A PASS/FAIL
C         MESSAGE.  TESTS WHICH FAIL GENERATE ADDITIONAL OUTPUT.
C
C     SPARSE BLAS SUBPROGRAMS WHICH ARE CERTIFIED BY THIS PROGRAM ARE
C
C         CAXPYI          CDOTUI          CGTHRZ
C         CDOTCI          CGTHR           CSCTR
C
C     THIS PROGRAM REQUIRES AN INPUT FILE ASSIGNED TO UNIT NIN
C     (CURRENTLY SET TO 5 BY A PARAMETER STATEMENT).  THE DATA ON
C     THIS INPUT FILE CONTROLS THE OUTPUT UNIT, THE THRESHOLD VALUE
C     FOR THE NUMERICAL TESTING, AND THE SPECIFICATIONS FOR THE
C     TEST VALUES FOR THE LENGTH OF THE SPARSE VECTORS AND THE SCALARS
C     USED BY THE VARIOUS SUBPROGRAMS.  AN EXAMPLE OF THE INPUT FILE
C     FOLLOWS
C
C LINE  1     'CBLATS.SUMM'           NAME OF OUTPUT FILE
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
      REAL                EPSILN, EPSSAV, THRESH
C
      COMPLEX             X     (NZMAX2),     Y     (NZMAX2),
     1                    XTRUE (NZMAX2),     YTRUE (NZMAX2),
     2                    XSAVE (NZMAX2),     YSAVE (NZMAX2),
     3                    AVALUE(NAMAX)
C
C     --------------------
C     ... SUBPROGRAMS USED
C     --------------------
C
      REAL                SDIFF
C
      EXTERNAL            TCXPYI, TCDTCI, TCDTUI, TCGTHR, TCGTHZ,
     1                    TCSCTR, SDIFF
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
      IF  ( THRESH .LE. 0.0E0 )  THEN
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
      EPSILN = 1.0E0
      EPSSAV = 1.0E0
C
  200 IF  ( SDIFF ( 1.0E0 + EPSILN, 1.0E0 ) .EQ. 0.0E0 )  GO TO 210
C
          EPSSAV = EPSILN
          EPSILN = EPSILN * .5E0
          GO TO 200
C
  210 EPSILN = EPSSAV
C
C     ==================================================================
C
C     ------------------------------------------
C     ... TEST THE COMPLEX PRECISION SPARSE BLAS
C     ------------------------------------------
C
C     ------------------
C     ... CERTIFY CAXPYI
C     ------------------
C
      CALL TCXPYI (   NOUT,   EPSILN, THRESH, NZMAX2,
     1                NUMNZ,  NZVALU, NUMA,   AVALUE ,
     2                X,      XSAVE,  XTRUE,  Y,      YSAVE,  YTRUE,
     3                INDX,   INDXT,  LIST,   ERRCNT, ERRMAX )
C
C     ------------------
C     ... CERTIFY CDOTCI
C     ------------------
C
      CALL TCDTCI (   NOUT,   EPSILN, THRESH, NZMAX2,
     1                NUMNZ,  NZVALU,
     2                X,      XSAVE,  XTRUE,  Y,      YSAVE,  YTRUE,
     3                INDX,   INDXT,  ERRCNT, ERRMAX )
C
C     ------------------
C     ... CERTIFY CDOTUI
C     ------------------
C
      CALL TCDTUI (   NOUT,   EPSILN, THRESH, NZMAX2,
     1                NUMNZ,  NZVALU,
     2                X,      XSAVE,  XTRUE,  Y,      YSAVE,  YTRUE,
     3                INDX,   INDXT,  ERRCNT, ERRMAX )
C
C     -----------------
C     ... CERTIFY CGTHR
C     -----------------
C
      CALL TCGTHR (   NOUT,   NZMAX2, NUMNZ,  NZVALU,
     1                X,      XSAVE,  XTRUE,  Y,      YSAVE,  YTRUE,
     2                INDX,   INDXT,  ERRCNT, ERRMAX )
C
C     ------------------
C     ... CERTIFY CGTHRZ
C     ------------------
C
      CALL TCGTHZ (   NOUT,   NZMAX2, NUMNZ,  NZVALU,
     1                X,      XSAVE,  XTRUE,  Y,      YSAVE,  YTRUE,
     2                INDX,   INDXT,  ERRCNT, ERRMAX )
C
C     -----------------
C     ... CERTIFY CSCTR
C     -----------------
C
      CALL TCSCTR (   NOUT,   NZMAX2, NUMNZ,  NZVALU,
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
C     -----------------------------------------------------
C     ... END OF CERTIFICATION PROGRAM FOR COMPLEX PRECISION
C         SPARSE BLAS
C     -----------------------------------------------------
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
     1          5X, 'START OF CERTIFICATION PROGRAM FOR THE COMPLEX ',
     2              'PRECISION SPARSE BLAS'
     3         /5X, '-----------------------------------------------',
     4              '---------------------'
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
     1          3 ( 2X, '(', 1PE13.4, ',', 1PE13.4, ')' )     )
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
 1130 FORMAT ( /5X, 'USER SPECIFIED VALUE FOR THRESHOLD IS ', 1PE15.5,
     1              ' WHICH IS NONPOSITIVE.  IT HAS BEEN RESET TO 10.')
C
 1140 FORMAT ( /5X, 'USER SPECIFIED NUMBER OF VALUES OF NZ IS ', I5,
     1              ' WHICH IS NONPOSITIVE.  NO TESTING WILL OCCUR.' )
C
 1150 FORMAT ( /5X, 'THE ', I3, '-TH USER SPECIFIED VALUE OF NZ IS ',
     1              I8, ' IS LARGER THAN THE MAXIMUM ALLOWABLE ',
     2              'VALUE OF NZ.  IT HAS BEEN RESET TO ', I5 )
C
 2000 FORMAT ( /5X, 'COMPLEX PRECISION SPARSE BLAS HAVE PASSED ALL ',
     1              'TESTS.' )
C
 2100 FORMAT ( /5X, 'COMPLEX PRECISION SPARSE BLAS HAVE FAILED ', I10,
     1              ' TESTS.  SEE ABOVE PRINTED ERROR MESSAGES.' )
C
C     ==================================================================
C
      END
      LOGICAL FUNCTION   CVSAME   ( N, CX, CY )
C
C     ==================================================================
C
C     LOGICAL FUNCTION  CVSAME  DETERMINES IF THE VECTORS  CX  AND  CY
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
      COMPLEX             CX (*), CY (*)
C
C     ==================================================================
C
      CVSAME = .TRUE.
C
      DO 10 I = 1, N
          IF  ( CX(I) .NE. CY(I) )  THEN
              CVSAME = .FALSE.
              GO TO 20
          ENDIF
   10 CONTINUE
C
   20 RETURN
      END

      SUBROUTINE   TCXPYI   ( NOUT,   EPSILN, THRESH, NZMAX2,
     1                        NUMNZ,  NZVALU, NUMA,   AVALUE,
     2                        X,      XSAVE,  XTRUE,  Y,      YSAVE,
     3                        YTRUE , INDX,   INDXT,  LIST,   ERRCNT,
     4                        ERRMAX )
C
C     ==================================================================
C     ==================================================================
C     ====  TCXPYI  -- CERTIFY  CAXPYI                              ====
C     ==================================================================
C     ==================================================================
C
C     SUBROUTINE  TCXPYI  IS THE CERTIFICATION MODULE FOR THE SPARSE
C     BASIC LINEAR ALGEBRA SUBROUTINE MODULE  CAXPYI.
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
      REAL                EPSILN, THRESH
C
      COMPLEX             AVALUE (*),
     1                    X (*),       XSAVE (*),   XTRUE (*),
     2                    Y (*),       YSAVE (*),   YTRUE (*)
C
C     -------------------
C     ... LOCAL VARIABLES
C     -------------------
C
      COMPLEX             A,      ATRUE,  CLOBBR
C
      INTEGER             COUNT,  I,      ICLOBR, J,      KA,
     1                    KINDX,  KNZ,    N,      NZ,     NZTRUE
C
      REAL                ERR,    S,      T
C
C     --------------------
C     ... SUBPROGRAMS USED
C     --------------------
C
      LOGICAL             IVSAME, CVSAME
C
      EXTERNAL            ICOPY,  CCOPY,  IINIT,  CINIT,  GNINDX,
     1                    IVSAME, CVSAME, CAXPYI
C
C     ==================================================================
C
C     ------------------
C     ... INITIALIZATION
C     ------------------
C
      COUNT     =   0
C
      CLOBBR    =   ( -1.0E10, -1.0E10 )
      ICLOBR    =   -10000000
C
C     ------------------------------------
C     ... GENERATE SOME VALUES FOR X AND Y
C     ------------------------------------
C
      DO 100 I = 1, NZMAX2
         XSAVE(I) = CMPLX ( COS ( .6*FLOAT(I) ), SIN ( .2*FLOAT(I) ) )
         YSAVE(I) = CMPLX ( SIN ( .7*FLOAT(I) ), COS ( .9*FLOAT(I) ) )
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
                  CALL CCOPY ( NZTRUE, XSAVE,  1, XTRUE, 1 )
                  CALL CINIT ( I,      CLOBBR, XTRUE(J), 1 )
                  CALL CINIT ( N,      CLOBBR, YTRUE, 1 )
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
                  CALL CCOPY ( N, YTRUE, 1, Y, 1 )
                  CALL CCOPY ( N, XTRUE, 1, X, 1 )
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
C                 ... CALL CAXPYI
C                 ---------------
C
                  CALL CAXPYI ( NZ, A, X, INDX, Y )
C
C                 -----------------------------------------
C                 ... TEST ARGUMENTS OF CAXPYI THAT ARE NOT
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
                  IF  ( .NOT. CVSAME ( N, X, XTRUE ) )  THEN
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
C                 ... TEST OUTPUT FROM CAXPYI
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
C     ... END OF MODULE TCXPYI
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
 1000 FORMAT ( 5X, 'CAXPYI ALTERED NZ FOR TEST WITH NZ = ', I5,
     1             ' A = (', 1PE15.5, ',', 1PE15.5,
     2             ') AND THE INDX TYPE NO. ', I5
     3        /5X, 'ALTERED VALUE OF NZ = ', I5 )
C
 1100 FORMAT ( 5X, 'CAXPYI ALTERED A FOR TEST WITH NZ = ', I5,
     1             ' A = (', 1PE15.5, ',', 1PE15.5,
     2             ') AND THE INDX TYPE NO. ', I5
     3        /5X, 'ALTERED VALUE OF A = (', 1PE15.5, ',',
     4              1PE15.5, ')' )
C
 1200 FORMAT ( 5X, 'CAXPYI ALTERED ARRAY X FOR TEST WITH NZ = ', I5,
     1             ' A = (', 1PE15.5, ',', 1PE15.5,
     2             ') AND THE INDX TYPE NO. ', I5 )
C
 1300 FORMAT ( 5X, 'CAXPYI ALTERED ARRAY INDX FOR TEST WITH NZ = ', I5,
     1             ' A = (', 1PE15.5, ',', 1PE15.5,
     2             ') AND THE INDX TYPE NO. ', I5 )
C
 1400 FORMAT ( 5X, 'CAXPYI OUTPUT ARRAY Y IS INCORRECT FOR TEST WITH ',
     1             'NZ = ', I5, ' A = (', 1PE15.5, ',', 1PE15.5,
     2             ') AND THE INDX TYPE NO. ', I5
     3        /5X, 'INCORRECT COMPONENT NO. ', I5, ' HAS VALUE = (',
     4             1PE15.5, ',', 1PE15.5,
     5             ') TRUE VALUE = (', 1PE15.5, ',', 1PE15.5, ')' )
C
 1500 FORMAT ( 5X, 'CAXPYI OUTPUT ARRAY Y IS INACCURATE FOR TEST WITH ',
     1             'NZ = ', I5, ' A = (', 1PE15.5, ',', 1PE15.5,
     2             ') AND THE INDX TYPE NO. ', I5
     3        /5X, 'INACCURATE COMPONENT NO. ', I5, ' HAS VALUE = (',
     4             1PE15.5, ',', 1PE15.5, ') TRUE VALUE = (',
     5             1PE15.5, ',', 1PE15.5, ')'
     6        /5X, 'ERROR = ', 1PE12.1 )
C
 2700 FORMAT ( /5X, 'CAXPYI PASSED ALL TESTS.' )
C
 2800 FORMAT ( /5X, 'CAXPYI FAILED', I10, ' TESTS.'  )
C
C     ==================================================================
C
      END
      SUBROUTINE   TCDTCI   ( NOUT,   EPSILN, THRESH, NZMAX2,
     1                        NUMNZ,  NZVALU,
     2                        X,      XSAVE,  XTRUE,  Y,      YSAVE,
     3                        YTRUE , INDX,   INDXT,  ERRCNT, ERRMAX )
C
C     ==================================================================
C     ==================================================================
C     ====  TCDTCI  --  CERTIFY  CDOTCI                             ====
C     ==================================================================
C     ==================================================================
C
C     SUBROUTINE  TCDTCI  IS THE CERTIFICATION MODULE FOR THE SPARSE
C     BASIC LINEAR ALGEBRA SUBROUTINE MODULE  CDOTCI.
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
      REAL                EPSILN, THRESH
C
      COMPLEX             X (*),       XSAVE (*),   XTRUE (*),
     1                    Y (*),       YSAVE (*),   YTRUE (*)
C
C     -------------------
C     ... LOCAL VARIABLES
C     -------------------
C
      INTEGER             COUNT,  I,      ICLOBR, J,      KINDX,
     1                    KNZ,    N,      NZ,     NZTRUE
C
      REAL                ERR,    S,      T
C
      COMPLEX             CLOBBR, V,      W
C
C     --------------------
C     ... SUBPROGRAMS USED
C     --------------------
C
      LOGICAL             IVSAME, CVSAME
C
      COMPLEX             CDOTCI
C
      EXTERNAL            ICOPY,  CCOPY,  CINIT,  GNINDX,
     1                    IVSAME, CVSAME, CDOTCI
C
C     ==================================================================
C
C     ------------------
C     ... INITIALIZATION
C     ------------------
C
      COUNT     =   0
C
      CLOBBR    =   ( -1.0E10, -1.0E10 )
      ICLOBR    =   -10000000
C
C     ------------------------------------
C     ... GENERATE SOME VALUES FOR X AND Y
C     ------------------------------------
C
      DO 100 I = 1, NZMAX2
         XSAVE(I) = CMPLX ( COS ( .6*FLOAT(I) ), SIN ( .2*FLOAT(I) ) )
         YSAVE(I) = CMPLX ( SIN ( .7*FLOAT(I) ), COS ( .9*FLOAT(I) ) )
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
              CALL CCOPY ( NZTRUE, XSAVE,  1, XTRUE, 1 )
              CALL CINIT ( I,      CLOBBR, XTRUE(J), 1 )
              CALL CINIT ( N,      CLOBBR, YTRUE, 1 )
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
              CALL CCOPY ( N, YTRUE, 1, Y, 1 )
              CALL CCOPY ( N, XTRUE, 1, X, 1 )
              CALL ICOPY ( N, INDXT, 1, INDX, 1 )
C
C             --------------------------
C             ... COMPUTE IN-LINE RESULT
C             --------------------------
C
              V = ( 0.0E0, 0.0E0 )
C
              DO 300 I = 1, NZTRUE
                  V = V + CONJG ( XTRUE(I) ) * YTRUE (INDXT(I))
  300         CONTINUE
C
C             --------------
C             ... CALL CDOTCI
C             --------------
C
              W = CDOTCI ( NZ, X, INDX, Y )
C
C             -----------------------------------------
C             ... TEST ARGUMENTS OF CDOTCI THAT ARE NOT
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
              IF  ( .NOT. CVSAME ( N, X, XTRUE ) )  THEN
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
              IF  ( .NOT. CVSAME ( N, Y, YTRUE ) )  THEN
                  COUNT = COUNT + 1
                  IF  ( COUNT .LE. ERRMAX )  THEN
                      WRITE ( NOUT, 1300 ) NZTRUE, KINDX
                  END IF
              END IF
C
C             --------------------------
C             ... TEST OUTPUT FROM CDOTCI
C             --------------------------
C
              S = ABS ( V - W )
C
              T = 0.0E0
              DO 400 I = 1, NZTRUE
                  T = T + ABS ( XTRUE(I) * YTRUE (INDXT(I)) )
  400         CONTINUE
C
              IF  ( T .EQ. 0.0E0 )  T = 1.0E0
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
C     ... END OF MODULE TCDTCI
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
 1000 FORMAT ( 5X, 'CDOTCI ALTERED NZ FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5,
     2             '.  ALTERED VALUE OF NZ = ', I5 )
C
 1100 FORMAT ( 5X, 'CDOTCI ALTERED ARRAY X FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5 )
C
 1200 FORMAT ( 5X, 'CDOTCI ALTERED ARRAY INDX FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5 )
C
 1300 FORMAT ( 5X, 'CDOTCI ALTERED ARRAY Y FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5 )
C
 1400 FORMAT ( 5X, 'CDOTCI OUTPUT W IS INACCURATE FOR TEST WITH ',
     1             'NZ = ', I5, ' AND THE INDX TYPE NO. ', I5
     2        /5X, 'CDOTCI HAS VALUE = (', 1PE15.5, ',', 1PE15.5,
     3             ') TRUE VALUE = (', 1PE15.5, ',', 1PE15.5,
     4             ') ERROR = ', 1PE12.1 )
C
 2700 FORMAT ( /5X, 'CDOTCI PASSED ALL TESTS.' )
C
 2800 FORMAT ( /5X, 'CDOTCI FAILED', I10, ' TESTS.'  )
C
C     ==================================================================
C
      END
      SUBROUTINE   TCDTUI   ( NOUT,   EPSILN, THRESH, NZMAX2,
     1                        NUMNZ,  NZVALU,
     2                        X,      XSAVE,  XTRUE,  Y,      YSAVE,
     3                        YTRUE , INDX,   INDXT,  ERRCNT, ERRMAX )
C
C     ==================================================================
C     ==================================================================
C     ====  TCDTUI  --  CERTIFY  CDOTUI                             ====
C     ==================================================================
C     ==================================================================
C
C     SUBROUTINE  TCDTUI  IS THE CERTIFICATION MODULE FOR THE SPARSE
C     BASIC LINEAR ALGEBRA SUBROUTINE MODULE  CDOTUI.
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
      REAL                EPSILN, THRESH
C
      COMPLEX             X (*),       XSAVE (*),   XTRUE (*),
     1                    Y (*),       YSAVE (*),   YTRUE (*)
C
C     -------------------
C     ... LOCAL VARIABLES
C     -------------------
C
      INTEGER             COUNT,  I,      ICLOBR, J,      KINDX,
     1                    KNZ,    N,      NZ,     NZTRUE
C
      REAL                ERR,    S,      T
C
      COMPLEX             CLOBBR, V,      W
C
C     --------------------
C     ... SUBPROGRAMS USED
C     --------------------
C
      LOGICAL             IVSAME, CVSAME
C
      COMPLEX             CDOTUI
C
      EXTERNAL            ICOPY,  CCOPY,  CINIT,  GNINDX,
     1                    IVSAME, CVSAME, CDOTUI
C
C     ==================================================================
C
C     ------------------
C     ... INITIALIZATION
C     ------------------
C
      COUNT     =   0
C
      CLOBBR    =   ( -1.0E10, -1.0E10 )
      ICLOBR    =   -10000000
C
C     ------------------------------------
C     ... GENERATE SOME VALUES FOR X AND Y
C     ------------------------------------
C
      DO 100 I = 1, NZMAX2
         XSAVE(I) = CMPLX ( COS ( .6*FLOAT(I) ), SIN ( .2*FLOAT(I) ) )
         YSAVE(I) = CMPLX ( SIN ( .7*FLOAT(I) ), COS ( .9*FLOAT(I) ) )
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
              CALL CCOPY ( NZTRUE, XSAVE,  1, XTRUE, 1 )
              CALL CINIT ( I,      CLOBBR, XTRUE(J), 1 )
              CALL CINIT ( N,      CLOBBR, YTRUE, 1 )
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
              CALL CCOPY ( N, YTRUE, 1, Y, 1 )
              CALL CCOPY ( N, XTRUE, 1, X, 1 )
              CALL ICOPY ( N, INDXT, 1, INDX, 1 )
C
C             --------------------------
C             ... COMPUTE IN-LINE RESULT
C             --------------------------
C
              V = ( 0.0E0, 0.0E0 )
C
              DO 300 I = 1, NZTRUE
                  V = V + XTRUE(I) * YTRUE (INDXT(I))
  300         CONTINUE
C
C             --------------
C             ... CALL CDOTUI
C             --------------
C
              W = CDOTUI ( NZ, X, INDX, Y )
C
C             -----------------------------------------
C             ... TEST ARGUMENTS OF CDOTUI THAT ARE NOT
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
              IF  ( .NOT. CVSAME ( N, X, XTRUE ) )  THEN
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
              IF  ( .NOT. CVSAME ( N, Y, YTRUE ) )  THEN
                  COUNT = COUNT + 1
                  IF  ( COUNT .LE. ERRMAX )  THEN
                      WRITE ( NOUT, 1300 ) NZTRUE, KINDX
                  END IF
              END IF
C
C             --------------------------
C             ... TEST OUTPUT FROM CDOTUI
C             --------------------------
C
              S = ABS ( V - W )
C
              T = 0.0E0
              DO 400 I = 1, NZTRUE
                  T = T + ABS ( XTRUE(I) * YTRUE (INDXT(I)) )
  400         CONTINUE
C
              IF  ( T .EQ. 0.0E0 )  T = 1.0E0
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
C     ... END OF MODULE TCDTUI
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
 1000 FORMAT ( 5X, 'CDOTUI ALTERED NZ FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5,
     2             '.  ALTERED VALUE OF NZ = ', I5 )
C
 1100 FORMAT ( 5X, 'CDOTUI ALTERED ARRAY X FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5 )
C
 1200 FORMAT ( 5X, 'CDOTUI ALTERED ARRAY INDX FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5 )
C
 1300 FORMAT ( 5X, 'CDOTUI ALTERED ARRAY Y FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5 )
C
 1400 FORMAT ( 5X, 'CDOTUI OUTPUT W IS INACCURATE FOR TEST WITH ',
     1             'NZ = ', I5, ' AND THE INDX TYPE NO. ', I5
     2        /5X, 'CDOTUI HAS VALUE = (', 1PE15.5, ',', 1PE15.5,
     3             ') TRUE VALUE = (', 1PE15.5, ',', 1PE15.5,
     4             ') ERROR = ', 1PE12.1 )
C
 2700 FORMAT ( /5X, 'CDOTUI PASSED ALL TESTS.' )
C
 2800 FORMAT ( /5X, 'CDOTUI FAILED', I10, ' TESTS.'  )
C
C     ==================================================================
C
      END
      SUBROUTINE   TCGTHR   ( NOUT,   NZMAX2, NUMNZ,  NZVALU,
     1                        X,      XSAVE,  XTRUE,  Y,      YSAVE,
     2                        YTRUE , INDX,   INDXT,  ERRCNT, ERRMAX )
C
C     ==================================================================
C     ==================================================================
C     ====  TCGTHR  --  CERTIFY  CGTHR                              ====
C     ==================================================================
C     ==================================================================
C
C     SUBROUTINE  TCGTHR  IS THE CERTIFICATION MODULE FOR THE SPARSE
C     BASIC LINEAR ALGEBRA SUBROUTINE MODULE  CGTHR.
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
      COMPLEX             X (*),       XSAVE (*),   XTRUE (*),
     1                    Y (*),       YSAVE (*),   YTRUE (*)
C
C     -------------------
C     ... LOCAL VARIABLES
C     -------------------
C
      INTEGER             COUNT,  I,      ICLOBR, KINDX,
     1                    KNZ,    N,      NZ,     NZTRUE
C
      COMPLEX             CLOBBR
C
C     --------------------
C     ... SUBPROGRAMS USED
C     --------------------
C
      LOGICAL             IVSAME, CVSAME
C
      EXTERNAL            ICOPY,  CCOPY,  CINIT,  GNINDX,
     1                    IVSAME, CVSAME, CGTHR
C
C     ==================================================================
C
C     ------------------
C     ... INITIALIZATION
C     ------------------
C
      COUNT     =   0
C
      CLOBBR    =   ( -1.0E10, -1.0E10 )
      ICLOBR    =   -10000000
C
C     ------------------------------------
C     ... GENERATE SOME VALUES FOR X AND Y
C     ------------------------------------
C
      DO 100 I = 1, NZMAX2
         XSAVE(I) = CMPLX ( COS ( .6*FLOAT(I) ), SIN ( .2*FLOAT(I) ) )
         YSAVE(I) = CMPLX ( SIN ( .7*FLOAT(I) ), COS ( .9*FLOAT(I) ) )
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
              CALL CINIT ( N, CLOBBR, XTRUE, 1 )
              CALL CINIT ( N, CLOBBR, YTRUE, 1 )
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
              CALL CCOPY ( N, YTRUE, 1, Y, 1 )
              CALL CCOPY ( N, XTRUE, 1, X, 1 )
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
C             ... CALL CGTHR
C             --------------
C
              CALL CGTHR ( NZ, Y, X, INDX )
C
C             ----------------------------------------
C             ... TEST ARGUMENTS OF CGTHR THAT ARE NOT
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
              IF  ( .NOT. CVSAME ( N, Y, YTRUE ) )  THEN
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
C             ... TEST OUTPUT FROM CGTHR
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
C     ... END OF MODULE TCGTHR
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
 1000 FORMAT ( 5X, 'CGTHR ALTERED NZ FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5,
     2             '.  ALTERED VALUE OF NZ = ', I5 )
C
 1100 FORMAT ( 5X, 'CGTHR ALTERED ARRAY Y FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5 )
C
 1200 FORMAT ( 5X, 'CGTHR ALTERED ARRAY INDX FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5 )
C
 1300 FORMAT ( 5X, 'CGTHR OUTPUT ARRAY X IS INCORRECT FOR TEST WITH ',
     1             'NZ = ', I5, ' AND THE INDX TYPE NO. ', I5
     2        /5X, 'INACCURATE COMPONENT NO. ', I5, ' HAS VALUE = (',
     3             1PE15.5, ',', 1PE15.5, ') TRUE VALUE = (',
     4             1PE15.5, ',', 1PE15.5, ')' )
C
 2700 FORMAT ( /5X, 'CGTHR  PASSED ALL TESTS.' )
C
 2800 FORMAT ( /5X, 'CGTHR  FAILED', I10, ' TESTS.'  )
C
C     ==================================================================
C
      END
      SUBROUTINE   TCGTHZ   ( NOUT,   NZMAX2, NUMNZ,  NZVALU,
     1                        X,      XSAVE,  XTRUE,  Y,      YSAVE,
     2                        YTRUE , INDX,   INDXT,  ERRCNT, ERRMAX )
C
C     ==================================================================
C     ==================================================================
C     ====  TCGTHZ  --  CERTIFY  CGTHRZ                             ====
C     ==================================================================
C     ==================================================================
C
C     SUBROUTINE  TCGTHZ  IS THE CERTIFICATION MODULE FOR THE SPARSE
C     BASIC LINEAR ALGEBRA SUBROUTINE MODULE  CGTHRZ.
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
      COMPLEX             X (*),       XSAVE (*),   XTRUE (*),
     1                    Y (*),       YSAVE (*),   YTRUE (*)
C
C     -------------------
C     ... LOCAL VARIABLES
C     -------------------
C
      INTEGER             COUNT,  I,      ICLOBR, KINDX,
     1                    KNZ,    N,      NZ,     NZTRUE
C
      COMPLEX             CLOBBR
C
C     --------------------
C     ... SUBPROGRAMS USED
C     --------------------
C
      LOGICAL             IVSAME, CVSAME
C
      EXTERNAL            ICOPY,  CCOPY,  CINIT,  GNINDX,
     1                    IVSAME, CVSAME, CGTHRZ
C
C     ==================================================================
C
C     ------------------
C     ... INITIALIZATION
C     ------------------
C
      COUNT     =   0
C
      CLOBBR    =   ( -1.0E10, -1.0E10 )
      ICLOBR    =   -10000000
C
C     ------------------------------------
C     ... GENERATE SOME VALUES FOR X AND Y
C     ------------------------------------
C
      DO 100 I = 1, NZMAX2
         XSAVE(I) = CMPLX ( COS ( .6*FLOAT(I) ), SIN ( .2*FLOAT(I) ) )
         YSAVE(I) = CMPLX ( SIN ( .7*FLOAT(I) ), COS ( .9*FLOAT(I) ) )
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
              CALL CINIT ( N, CLOBBR, XTRUE, 1 )
              CALL CINIT ( N, CLOBBR, YTRUE, 1 )
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
              CALL CCOPY ( N, YTRUE, 1, Y, 1 )
              CALL CCOPY ( N, XTRUE, 1, X, 1 )
              CALL ICOPY ( N, INDXT, 1, INDX, 1 )
C
C             --------------------------
C             ... COMPUTE IN-LINE RESULT
C             --------------------------
C
              DO 300 I = 1, NZTRUE
                  XTRUE (I) = YTRUE (INDXT(I))
                  YTRUE(INDXT(I)) = ( 0.0E0, 0.0E0 )
  300         CONTINUE
C
C             ---------------
C             ... CALL CGTHRZ
C             ---------------
C
              CALL CGTHRZ ( NZ, Y, X, INDX )
C
C             -----------------------------------------
C             ... TEST ARGUMENTS OF CGTHRZ THAT ARE NOT
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
C             ... TEST OUTPUT FROM CGTHRZ
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
C     ... END OF MODULE TCGTHZ
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
 1000 FORMAT ( 5X, 'CGTHRZ ALTERED NZ FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5,
     2             '.  ALTERED VALUE OF NZ = ', I5 )
C
 1100 FORMAT ( 5X, 'CGTHRZ ALTERED ARRAY INDX FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5 )
C
 1200 FORMAT ( 5X, 'CGTHRZ OUTPUT ARRAY X IS INCORRECT FOR TEST WITH ',
     1             'NZ = ', I5, ' AND THE INDX TYPE NO. ', I5
     2        /5X, 'INACCURATE COMPONENT NO. ', I5, ' HAS VALUE = (',
     3             1PE15.5, ',', 1PE15.5, ') TRUE VALUE = (',
     4             1PE15.5, ',', 1PE15.5, ')' )
C
 1300 FORMAT ( 5X, 'CGTHRZ OUTPUT ARRAY Y IS INCORRECT FOR TEST WITH ',
     1             'NZ = ', I5, ' AND THE INDX TYPE NO. ', I5
     2        /5X, 'INACCURATE COMPONENT NO. ', I5, ' HAS VALUE = (',
     3             1PE15.5, ',', 1PE15.5, ') TRUE VALUE = (',
     4             1PE15.5, ',', 1PE15.5, ')' )
C
 2700 FORMAT ( /5X, 'CGTHRZ PASSED ALL TESTS.' )
C
 2800 FORMAT ( /5X, 'CGTHRZ FAILED', I10, ' TESTS.'  )
C
C     ==================================================================
C
      END
      SUBROUTINE   TCSCTR   ( NOUT,   NZMAX2, NUMNZ,  NZVALU,
     1                        X,      XSAVE,  XTRUE,  Y,      YSAVE,
     2                        YTRUE , INDX,   INDXT,  ERRCNT, ERRMAX )
C
C     ==================================================================
C     ==================================================================
C     ====  TCSCTR  --  CERTIFY  CSCTR                              ====
C     ==================================================================
C     ==================================================================
C
C     SUBROUTINE  TCSCTR  IS THE CERTIFICATION MODULE FOR THE SPARSE
C     BASIC LINEAR ALGEBRA SUBROUTINE MODULE  CSCTR.
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
      COMPLEX             X (*),       XSAVE (*),   XTRUE (*),
     1                    Y (*),       YSAVE (*),   YTRUE (*)
C
C     -------------------
C     ... LOCAL VARIABLES
C     -------------------
C
      INTEGER             COUNT,  I,      ICLOBR, J,      KINDX,
     1                    KNZ,    N,      NZ,     NZTRUE
C
      COMPLEX             CLOBBR
C
C     --------------------
C     ... SUBPROGRAMS USED
C     --------------------
C
      LOGICAL             IVSAME, CVSAME
C
      EXTERNAL            ICOPY,  CCOPY,  CINIT,  GNINDX,
     1                    IVSAME, CVSAME, CSCTR
C
C     ==================================================================
C
C     ------------------
C     ... INITIALIZATION
C     ------------------
C
      COUNT     =   0
C
      CLOBBR    =   ( -1.0E10, -1.0E10 )
      ICLOBR    =   -10000000
C
C     ------------------------------------
C     ... GENERATE SOME VALUES FOR X AND Y
C     ------------------------------------
C
      DO 100 I = 1, NZMAX2
         XSAVE(I) = CMPLX ( COS ( .6*FLOAT(I) ), SIN ( .2*FLOAT(I) ) )
         YSAVE(I) = CMPLX ( SIN ( .7*FLOAT(I) ), COS ( .9*FLOAT(I) ) )
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
              CALL CCOPY ( NZTRUE, XSAVE,  1, XTRUE, 1 )
              CALL CINIT ( I,      CLOBBR, XTRUE(J), 1 )
              CALL CINIT ( N,      CLOBBR, YTRUE, 1 )
C
C             -------------------
C             ... COPY TRUE INPUT
C             -------------------
C
              NZ = NZTRUE
C
              CALL CCOPY ( N, YTRUE, 1, Y, 1 )
              CALL CCOPY ( N, XTRUE, 1, X, 1 )
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
C             ... CALL CSCTR
C             --------------
C
              CALL CSCTR ( NZ, X, INDX, Y )
C
C             ----------------------------------------
C             ... TEST ARGUMENTS OF CSCTR THAT ARE NOT
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
              IF  ( .NOT. CVSAME ( N, X, XTRUE ) )  THEN
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
C             ... TEST OUTPUT FROM CSCTR
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
C     ... END OF MODULE TCSCTR
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
 1000 FORMAT ( 5X, 'CSCTR ALTERED NZ FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5,
     2             '.  ALTERED VALUE OF NZ = ', I5 )
C
 1100 FORMAT ( 5X, 'CSCTR ALTERED ARRAY X FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5 )
C
 1200 FORMAT ( 5X, 'CSCTR ALTERED ARRAY INDX FOR TEST WITH NZ = ', I5,
     1             ' AND THE INDX TYPE NO. ', I5 )
C
 1300 FORMAT ( 5X, 'CSCTR OUTPUT ARRAY Y IS INCORRECT FOR TEST WITH ',
     1             'NZ = ', I5, ' AND THE INDX TYPE NO. ', I5
     2        /5X, 'INACCURATE COMPONENT NO. ', I5, ' HAS VALUE = (',
     3             1PE15.5, ',', 1PE15.5, ') TRUE VALUE = (',
     4             1PE15.5, ',', 1PE15.5, ')' )
C
 2700 FORMAT ( /5X, 'CSCTR  PASSED ALL TESTS.' )
C
 2800 FORMAT ( /5X, 'CSCTR  FAILED', I10, ' TESTS.'  )
C
C     ==================================================================
C
      END