      subroutine pisaquit
c
c     Original author: Charles F. Maguire
c     Creation date: November 20, 1998
c
c     Routine introduced to do STOP for Linux platform
c     Linux platform crashing after EXIT command
c
      implicit none
      write(6,1)
1     format(1h ,'  PISA Quit routine',/)
      call gulast
      stop ' PISA stopping'
      end
