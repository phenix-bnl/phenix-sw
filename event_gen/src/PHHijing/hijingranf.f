c
c     Initially a copy from RQMD
c
c     CFM: Revised Dec. 17 '96 to use GEANT random number generator routines
c
c*****************   r a n f   *******************************          
c
      real function ranf(x)
      real rvec
      integer icall /0/

      real rcall
      common /stat_ran/rcall

      if(icall.eq.0)then
	icall = 1
        rcall = 0.0
      endif
      rcall = rcall + 1.0

      call grndm(rvec, 1)
      if(rcall.eq.1.0)then
         write(6,1)rvec
1        format('  RANF <I> first # = ',f10.7)
      endif
      ranf=rvec                                             
      if(rvec.lt.0.or.rvec.gt.1)then
         write(6,2)rvec
2        format('  RANF <E>  ',e20.11)
      endif
      return                                                            
      end                                                               
