c     $Id: gulast.f,v 1.6 2008/05/21 08:22:09 hpereira Exp $
c     Call standard GEANT termination routine
      subroutine gulast
      implicit none

#include "gcflag.inc"
#include "guevgen.inc"
#include "guphnx.inc"
#include "udst.inc"

      integer ier, icycle

      call glast
      call hldir('//PAWC','T')
      if(iswit(1).gt.1) then
        
        ! compressed output mode closing
         call fzendo(lun_dout,'TX')
         if(zebra_output.eq.1)call lzclose       
        
      else
        
         if(zebra_output.eq.1)call gclose(0,ier)
        
      endif
      
      call hrout(0,icycle,' ')
      call hrend('GEANHIST')
c      write(6,1) chbk_file, icycle
c 1     format( ' gulast - closing ',a,' file at ICYCLE = ',i5)

c     get yield normalization for PHIs if filter was used
      if(nrvphi_evt.gt.0)then
        write(6, '(/,a,i12,a,/)' ) 'PHI event generator was called ',
     1    nrvphi_evt, ' times'
      endif

c     get yield normalization for SNGL_NEUTRAL if filter was used
      if(neutral_evt.gt.0)then
        write(6, '(/,a,i12,a,/)' ) 'SNGL_NEUTRAL generator called ',
     1    neutral_evt, ' times'
      endif

c     get yield normalization for J/PSIs if filter was used
      if(nrvjpsi_evt.gt.0)then
        write(6, '(/,a,i12,a,/)' ) 'J/PSI event generator was called ',
     1    nrvjpsi_evt, ' times'
      endif

c     get yield normalization for Chi(1P) if filter was used
      if(nrvchi_evt.gt.0)then
        write(6, '(/,a,i12,a,/)' ) ' Chi(1P) event generator called ',
     1    nrvchi_evt, ' times'
      endif

c     closing root output     
      if(root_output.eq.1)then
         call closerootout
      endif

      open(unit=99,file='finishPisa')
      write(99,99)
99    format(' PISA job is finished')
      close(unit=99)

      return
      end
