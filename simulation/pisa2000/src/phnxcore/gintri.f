 
**
      SUBROUTINE GINTRI
C.
C.    ******************************************************************
C.    *                                                                *
C.    *      Defines MENUs and COMMANDs                                *
C.    *       User MENUs initialisation by user routine GUINTI         *
C.    *                                                                *
C.    *    ==>Called by : GXINT                                        *
C.    *       Authors:   R.Brun      **********                        *
C.    *                                                                *
C.    ******************************************************************
C.
C.
C.  ---------------------------------------------------------------------
C.
      CALL GKDRAW
      CALL GKGCON
      CALL GKGEOM
      CALL GKCONT
      CALL GKRZ
      CALL GKFZ
      CALL GKDZ
      CALL GKSCAN
      CALL GKPICT
      CALL GKFORT
      CALL GKHIST
      CALL GKPHYS
      CALL VECDEF
      CALL GUINTI
      END
