c $Id: phnx_par.f,v 1.2 2007/03/27 14:33:33 hpereira Exp $
c File: phnx_interface.f
c provides common interface to geometry file and binding to the database
c author: Hugo Pereira
c version $Revision: 1.2 $
c date $Date: 2007/03/27 14:33:33 $

c--------------------------------------------      
c     open logical unit
      subroutine init_phnx_par( contents )

      character*(*) contents
      
c
c     interface logical unit
c      
      integer itf_lun
      common /interface/itf_lun

c     hardwired logical unit
      itf_lun = 16
      
c     open unit and write contents
      open(
     +  unit = itf_lun,
     +  status = 'scratch',
     +  iostat = ios )
        
      write( itf_lun, '(a)' ) contents
      rewind( unit=itf_lun )

      return
      end
      
c--------------------------------------------      
c     close logical unit
      subroutine close_phnx_interface()
      
c
c     interface logical unit
c      
      integer itf_lun
      common /interface/itf_lun

c     close unit
      close(unit = itf_lun)
      
      return
      end
      
