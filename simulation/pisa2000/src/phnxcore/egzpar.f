      Integer function egzpar (lun)


c      Routine to read in zebra parameters

      implicit none

c      Global Declarations
c      ===================

*-- Author  S.R. Tonse (original from T. Throw)
#include "evtzebra.inc"
#include "event.inc"
#include "evntcode.inc"

c     Revision History
c        Charles F. Maguire   May 5 '97  Backward compatibility ZEBRA input
c                                        needed by Hiroshima group

c     local variables

      integer lun      ! message logical unit

      namelist /zpar/ z_double_out, z_blocksize,
     1               z_fz_option_out, z_fz_option_in, z_fzlogl

c      Executable code
c      ===============

      egzpar = -1      ! assume success

      z_double_out = .false.      ! set defaults
      z_blocksize = 3600
      z_fz_option_out = 'TXO'
      z_fz_option_in = 'XIDL'     ! direct access, exchange, CFOPEN, input
      z_fzlogl = -2

c     Use default values if egzpar.par does not exist

      open(21,file='egzpar.par',status='OLD',err=120)
      read(21,nml=zpar)
      close (unit=21)


  120 continue
      fz_option_in = z_fz_option_in
      return
      end
