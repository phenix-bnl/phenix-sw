c
      subroutine xemctrlist(ltra1, iwall, listtr, listwa, nmtrmx, ijk)
      implicit none
c
c     subroutine to replace IUCOMP
c     used to find index pointer IJK to the input track/wall numbers
c     IUCOMP works only for a single vector list
c
c     Calling Map:  Called by DCTRCF, no internal calls
c
c     calling variables
c
      integer ltra1     ! track number INPUT
      integer iwall     ! wall number INPUT
      integer nmtrmx    ! array size INPUT
      integer listtr(nmtrmx)  ! current track numbers INPUT
      integer listwa(nmtrmx)  ! current wall numbers INPUT
      integer ijk       ! next available position if postive
c
c     ijk will return a negative number if previously stored
c
c
c     local variables
c
      integer itest    ! do loop variable
      integer itrack   ! test track number
c
c     for debugging
c
      if(iwall.eq.8)then
         iwall = 8
      endif
c
c     begin execution
c
      do itest = 1,nmtrmx
c
c     look for track number, or the next available positon
c
         itrack = listtr(itest)
         if(itrack.eq.0)then
c
c        have looped through LISTTR and not found LTRA1
c        so this LTRA1 is a new track
c
            listtr(itest) = ltra1
            listwa(itest) = iwall
            go to 10  ! branch out to set IJK value, and return
         endif  ! check on open position
         if(itrack.eq.ltra1.and.iwall.eq.listwa(itest))then
            ijk = -itest  ! negative means that it was previously stored
            return
         endif  ! check on track/wall equality to list values
      enddo  !  loop over positions
c
c     have looped through all the array elements
c     LTRA1 was not found and there are no more open positions
c
      write(6,1)nmtrmx
 1    format(/,3x,'XEMCTRLIST <E>: NMTRMX = ',i5,
     1            ' is too small in EMC')
      stop 'PISA stopping'
 10   continue
c
c     found LTRA1 or the next open position
c
      ijk = itest
      return
      end
 
