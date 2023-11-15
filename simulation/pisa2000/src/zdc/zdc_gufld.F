c $Id: zdc_gufld.F,v 1.1 2008/05/21 08:22:22 hpereira Exp $
******************************************************************
* RHIC-PHENIX ZDC detector simulation.
* GEANT tracking user routines.
* Author: Andras Ster, KFKI/Hungary
* Creation date: 20.April.1999
******************************************************************

      !****************************
      SUBROUTINE zdc_gufld(vect, f)

***   Routine called to pass the magnetic field

      IMPLICIT NONE

#include "gcvolu.inc"

      COMMON/ZDCINP/ dxfield, fragment_model, zdc_hbook
      REAL dxfield
      INTEGER fragment_model, zdc_hbook
 
      COMMON/ZDCCOM/ ism41,ism42,zdcl
      INTEGER ism41, ism42
      CHARACTER*4 zdcl

      REAL vect(3), f(3)


      IF(ism41 .EQ. lvolum(nlevel) .OR. ism42 .EQ. lvolum(nlevel)) THEN
         f(1) = 0.0
         f(3) = 0.0
         f(2) = dxfield
      ENDIF

      END


