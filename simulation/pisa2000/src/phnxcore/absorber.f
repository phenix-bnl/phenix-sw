c $Id: absorber.f,v 1.1 2008/05/30 03:09:44 hubert Exp $

      subroutine ABSORBER

c Hubert van Hecke, Dave Lee May 2008
c 
c Hadron absorber, part of the muon trigger upgrade.
c Located between the Central Magnet and Muon Magnet North
c 
c---------.---------.---------.---------.---------.---------.---------.--

      implicit none
 
#include "gcunit.inc"

      real abs_par(5), abs_pos(3)
      integer ivol, icall
      namelist /muon_abs_par/ abs_par, abs_pos
      integer itf_lun             !  geometry description logical unit 
      common /interface/itf_lun
      data icall/0/
 
c------------------------------------------------------------------------

      if(icall.eq.0) then
         icall = 1
      else
         write (LOUT,*)' ABSORBER was already called before - return'
         return         ! absorber called twice ?
      endif

      write(LOUT,* ) 'absorber - reading parameters from the par file'
      rewind(itf_lun)
      read( itf_lun, nml = muon_abs_par, err = 999 )

C     Add absorber in front of station 1

      CALL GSVOLU('ABSO','CONE',17,abs_par,5,IVOL)
      CALL GSPOS('ABSO',1,'HALL',
     &            abs_pos(1), abs_pos(2), abs_pos(3),0,'ONLY')
      CALL GSATT('ABSO','COLO',4)
      CALL GSATT('ABSO','SEEN',1)

c End of absorber geometry set up

      write(LOUT,*) 'absorber - installed'
      return

 999  STOP 'Absorber - Read error in absorber segment of the par file'
      end
