      subroutine hijingseed(iseq,iseq_skip)
      implicit none
c
c     Initialize random number generator
c
c     First version a copy from RQMD code
c
c     C.F. Maguire  revision May 22, 1996  Take out secnds call
c                                          Put in fixed initial seed
c
c     C.F. Maguire  revision Dec 17, 1996  Use GEANT random number routines
c
c     C.F. Maguire  revision Feb 28, 1999  Put in initial skipping of random numbers
c                                          ISEED_SKIP in namelist goes into SKIP_ISEED
c                                          Call random number generator that many times
c                                          initially
c     

      real rcall
      integer skip_iseed
      common /stat_ran/rcall, skip_iseed

      integer iseq
      integer iseq_skip
      integer iseed1
      integer iseed2
      integer iskip
      integer mskip
      real tran
c
c     begin execution
c
      skip_iseed = iseq_skip

      if(iseq.le.1) then
         iseq = 1
      endif
      if(iseq.gt.215)then
         iseq = 215
      endif
      write(6,1)iseq
 1    format(/,2x,'HIJINGSEED <I>: Using GEANT random number '
     +            ,'sequence ',i3)

c
c     Start the sequence
c
      call grndmq(iseed1, iseed2, iseq, ' ')
c
c     Get the first seeds in the sequence
c
      call grndmq(iseed1, iseed2, iseq, 'G')
c
c     Write the first seed values for this sequence
c
      write(6,2)iseed1, iseed2, iseq
 2    format(2x,'HIJINGSEED <I>: Seed pairs = ',2i12,
     +          '  for sequence',i4,/)

      if(skip_iseed.gt.0)then
         write(6,3)skip_iseed
 3       format(2x,'HIJINGSEED <I>: Skipping',i5,' million randoms')
         mskip = 1000000*skip_iseed   ! million multiplier
         do iskip = 1,mskip
            call grndm(tran, 1)
         enddo  ! skipping this many (millions) random numbers
         write(6,4)tran
 4       format(2x,'Last random number after skipping = ',f10.7,/)
      endif

      return                                                            
      end                                                               



