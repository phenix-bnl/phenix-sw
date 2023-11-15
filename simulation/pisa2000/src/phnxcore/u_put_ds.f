      subroutine  u_put_ds(i_xdiv,l_sup,c_fac,c_det,c_ds,chopt)
      implicit   none

c     Original author: Shaheen Tonse
c     Creation date: April, 1992

c     Purpose: Write a d/s to a ZEBRA DST file

c     Revision history
c        Name        Date            Comment
c     C.F. Maguire   July 29, 1997   Fix event header output format




      integer*4   i_xdiv
      integer*4   l_sup
      character*(*)  c_fac
      character*(*)  c_det
      character*(*)  c_ds, chopt

#include "quest.inc"
#include "udst.inc"
#include "guphnx.inc"

      integer*4   u_ctoh4
      integer*4   i_eflag
      integer*4   i
      integer icall /0/
      integer ioch(10)
      save ioch

c     Begin execution

      if(zebra_output.eq.0)then
         return
      endif

      if(icall.eq.0)then
         call mzioch(ioch, 10, '4H 30I -F')
         icall = 1
      endif
      
      call cltou( chopt ) 
      if (l_sup .eq. 0 .and. chopt .ne. 'Z' ) return

      iudst_pr_uhead(1) = u_ctoh4(c_fac)
      iudst_pr_uhead(2) = iudst_otag_typ
      iudst_pr_uhead(3) = u_ctoh4(c_det)
      iudst_pr_uhead(4) = u_ctoh4(c_ds)

      i_eflag = 0
      if (budst_new_evt) i_eflag = 1
      budst_new_evt = .false.

      call fzout(lun_dout,i_xdiv,l_sup,i_eflag,CHOPT,ioch,
     1   iudst_pr_nuh,iudst_pr_uhead)

      if (iudst_ologl .gt. 0)
     1   write(6,101) iquest(1),i_eflag,(iudst_pr_uhead(i),i=1,7)
      return

101   format(' u_put_ds: ',2i6,1x,4(a4,','),3I6)
      end
