      SUBROUTINE UA1_EVT
      IMPLICIT  NONE

c     "UA1" event generator
c      subroutine will fill the GUEVGEN common block momentum array
c                 for a full event with "filtered" primary pions
c      the P_T spectrum is that given by the Ju Kang-Richard Seto
c                 scaled approximation of the UA1 spectrum
c      the rapidity distribution is Gaussian

c      Revision History
c      H.van Hecke Jan 96: allow vertex spread in x,y also.
c      C.F. Maguire Feb '96: allow both North and South angle cut

c     MAP:
c        called by GUEVGEN
c        calls SCALING, NORRAN, RNDM

c     local specifications

      INTEGER   I
      INTEGER   KSPART, ID_GEANT, IDTEST
      REAL      A, P_T, YIELD
      REAL RAPID, TRAN, AMSQ, THETAU, FFACT, EXRAP, FTEST, ETOTAL
      REAL PTSTEP, HRNDM, RNDM, AMCPSQ, AMNPSQ
      REAL P_Z, PTOT
      REAL THETAU_NS

C     PION MASSES SQUARED

      PARAMETER (AMCPSQ = .139567*.139567)
      PARAMETER (AMNPSQ = .134693*.134693)

c     control parameter file specifications

      integer iincl /3/          ! number of included particles
      integer include(20) /7, 8, 9, 17*0/  ! input list of included GEANT IDs
      INTEGER NTOTAL /9000/      ! number of particles in full event
      INTEGER NHRNDM /10000/     ! size of random array
      REAL THE_MINC /0.0/        ! minimum charged particle theta
      REAL THE_MAXC /180./       ! maximum charged particle theta
      REAL THE_MINN /0.0/        ! minimum neutral particle theta
      REAL THE_MAXN /180./       ! maximum neutral particle theta
      REAL P_MINC /0.0/          ! minimum charged particle momentum
      REAL P_MINN /0.0/          ! minimum neutral particle momentum
      REAL Y_SIG /2.5/           ! Gaussian rapidity width sigma
      REAL PT_MAX /5.0/          ! maximum transverse momentum
      REAL XYZ0_INPUT(3) /3*0.0/ ! Vertex X,Y,Z origin
      REAL VRMS(3) /3*0.0/       ! RMS fluctuation in vertex X,Y,Z about 
				 !                           XYZ0_INPUT
      INTEGER NORTH_SOUTH /0/    ! Control for both North and South angles
 
      NAMELIST /UA1_PAR/ NTOTAL, NHRNDM, THE_MINC, THE_MAXC, P_MINC,
     &                   P_MINN, Y_SIG, PT_MAX,IINCL,INCLUDE,
     &                   THE_MINN, THE_MAXN, VRMS, XYZ0_INPUT,
     &                   NORTH_SOUTH
 
      logical logincl, logexcl ! include/exclude logic switches

c    special endcap switch

      LOGICAL LOGDUO             ! if true take only positive theta

c    special BBC switch (for "small" angles near 0 and near 180 degrees)

      logical log_mirror         ! normally false

c     initialization switch

      LOGICAL  FIRST /.TRUE./

c    SRT points out (6/4/93) that the DEC compiler does not allow one to
c        SAVE variables which have been initialized such as
c             LOGICAL FIRST /.TRUE./
c        In fact it is not necessary to SAVE such variables since variables
c        which are initialed by such DATA-like statemens are automatically
c        saved.  The issue of SAVEing vs making everything STATIC has to be
c        pursued

c     SAVE LOGDUO, FIRST, NTOTAL, THE_MINN, THE_MAXN, P_MINC, Y_SIG
c     SAVE PT_MAX, NHRNDM, LOGINCL, LOGEXCL, P_MINN
c     save the_minc, the_maxc

      save logduo, logincl, logexcl
 
#include "guevgen.inc"
#include "gcflag.inc"
#include "udst.inc"
#include "event.inc"
#include "subevt.inc"
#include "gckine.inc"
 

c     begin execution

 
      if(first)then

c       intialization
        first = .false.
        ipopsub = 100            ! default value

C       see if IPOPSUB value is being passed by KINE data card
        if(pkine(1) .lt. 100000) ipopsub = int(pkine(1))
        write(6,'(1h ,a,i6,a)')'ua1_evt: ',ipopsub,
     &    ' tracks/sub-event (pi+, pi-, pi0 only for now)'
        open(unit=17,file='ua1event.par',status='old',err=5,
     1    access='sequential')
        go to 10
5       continue

        write(6,*)'   Using default UA1 event parameters'
        go to 15
10      continue
        read(17,nml=ua1_par,err=20,end=25)
        close(unit=17)
15      continue
        
        write(6,797)ntotal,nhrndm,the_minc, the_maxc,
     1    the_minn, the_maxn,
     2    p_minc, p_minn, pt_max, y_sig, iincl,
     3    xyz0_input, vrms
          
797     format(/,3x,'UA1_EVT <I>: kinematic cuts from ua1event.par',
     1    ' file',/,3x,
     2    ' ntotal = ',i8,' nhrndm = ',i8, /,3x,
     3    ' the_minc = ',g10.3,' deg.,  the_maxc = ',g10.3,' deg.',/,3x,
     4    ' the_minn = ',g10.3,' deg.,  the_maxn = ',g10.3,' deg.',/,3x,
     5    ' p_min = ',2g10.3,' GeV/c,   pt_max = ',g10.3,' Gev/c',/,3x,
     6    ' ysig = ',g10.3,',  number of included particle IDs = ',
     7    i4,/,3x,'xyz0_input = ',3g8.2,' vrms = ',3g8.2,/)
        if(iincl.ne.0)then
          if(iincl.gt.0)then
            logincl = .true.
            if(iincl.gt.20)iincl = 20
            write(6,798)(include(i),i=1,iincl)
798         format(3x,'The following is a list of INCLUDED',
     1        ' particles:',2(/,3x,10i4),/)
            else
              logexcl = .true.
              iincl = -iincl
              if(iincl.gt.20)iincl = 20
              write(6,799)(include(i),i=1,iincl)
799           format(3x,'The following is a list of EXCLUDED',
     1          ' particles:',2(/,3x,10i4),/)
            endif   ! check on INCLUDE or EXCLUDE logic
          else
            write(6,'(/,a,/)') '  All particle IDs accepted'
          endif    !  check on IINCL different from 0
          if(north_south.eq.1)then
            write(6,'(/,a,/)') ' Angle cut for North and South'
          endif  ! check of North_South switch
          go to 30
20        continue
         stop '  error in reading ua1_par'
25       continue
         stop '  eof in reading ua1_par'
30       continue
         if(the_minc.lt.0.0)then

c         special endcap switch
c         This is an obsolete switch left over from when there was only a
c         North Muon Arm.  The idea was to double the statistics by running
c         the South going particles in the opposite direction.

          logduo = .true.
          the_minc = -the_minc
        else
          logduo = .false.
        endif   ! check on the_minc < 0.0

c       parameters to set up call to Kang-Seto subroutine

        kspart = 1   ! pion
        a = 197.    ! gold

c       prepare the hbook histograms

        CALL hbook1(61,'Differential UA1 P_T distribution', NHRNDM,
     1    0.0, PT_MAX,0.0)
        CALL hbook1(62,'Integrated UA1 P_T distribution', NHRNDM,
     1    0.0, PT_MAX,0.0)
        CALL hbook1(63,'Obtained RAPIDITY', 100,
     1    -7.0, 7.0,0.0)
        CALL hbook1(64,'Obtained P_T', 100,
     1    0.0, PT_MAX,0.0)
        CALL hbook1(65,'Obtained PTOT', 200,
     1    0.0, 2.*PT_MAX,0.0)
        CALL hbook1(66,'Cut RAPIDITY', 100,
     1    -7.0, 7.0,0.0)
        CALL hbook1(67,'Cut P_T', 100,
     1    0.0, PT_MAX,0.0)
        CALL hbook1(68,'Cut PTOT', 200,
     1    0.0, 2.*PT_MAX,0.0)
        CALL hbook1(69,'Cut THETA', 180,
     1    0.0, 180.,0.0)
          
        ptstep = pt_max/float(nhrndm)

c       fill both the differential and the "integrated" distributions
c       upon first call the hrndm, the "integrated" distribution will be summed
        do i = 1, nhrndm       
          p_t = float(i)*ptstep - ptstep/2. 
          
c         call to kang-seto subroutine
          call scaling(kspart,a,p_t,yield)
                
c         differential spectra
          call hf1(61,p_t,p_t*yield)
          call hf1(62,p_t,p_t*yield)
        enddo  
      endif
      
      numevt = numevt + 1
      chevt_name = 'ua1 (kang-seto formula)'

c     fill the GUEVGEN storage array
      mxtot = 0
      do i = 1,ntotal

c       first choose the particle  (right only pi+, pi-, pi0)
        tran = rndm(0)
        if(tran.le.0.3333333)then
          id_geant = 7
          amsq = amnpsq
        endif
        
        if(tran.gt.0.3333333.and.tran.le.0.6666666)then
          id_geant = 8
          amsq = amcpsq
          if(include(1).eq.5)then
            id_geant = 5   ! keep the mass the same
          endif
        endif
        
        if(tran.gt.0.6666666)then
          id_geant = 9
          amsq = amcpsq
          if(include(2).eq.6)then
            id_geant = 6 
          endif
        endif
        
        if(.not.logexcl.and..not.logincl)go to 150

c       check the included or excluded list
        if(logincl)then
          do idtest = 1,iincl
            if(id_geant.eq.include(idtest))then

c             particle is on included list, so accept it
              go to 150
            endif 
          enddo 

c         particle was not on included list, so reject it
          go to 250
        endif  
        
        if(logexcl)then
          do idtest = 1,iincl
            if(id_geant.eq.include(idtest))then
c             particle is on excluded list, so reject it
              go to 250
            endif 
          enddo 

c         particle was not on excluded list, so accept it

          go to 150
        endif
150     continue 

c        choose a p_t value
        P_T = HRNDM(62)

c       get the rapidity
        call norran(tran)
        rapid = y_sig*tran
        if(abs(rapid).gt.7.0)go to 250

c       get the momentum vector
        if(abs(rapid).gt.1.e-4)then
          exrap = exp(2.*rapid)
         else
          thetau = 90.0
          p_z = 0.0
          etotal = sqrt(p_t*p_t + amsq)
          go to 200
        endif   
        
        ffact = (exrap + 1.0)/(exrap - 1.0)
         if(ffact*ffact.eq.1.)then
            print 99,rapid,y_sig
99          format(3x,'rapid,y_sig',2e14.5)
            stop   'ua1_evt <e>: ffact out of bounds'
         endif  
        
         ftest = (amsq + p_t*p_t)/(ffact*ffact - 1.0)
        
         if(ftest.le.0.0)then
            print 101,ftest,rapid,exrap,ffact
101         format(/,3x,'ftest ?',g12.4,' rapid,exrap,ffact ',3g12.4)
            thetau = 90.0
            p_z = 0.0
            etotal = sqrt(p_t*p_t + amsq)
            go to 200
         endif
        
         p_z = sqrt(ftest)
         if(ffact.lt.0.0)p_z = -p_z
         etotal = ffact*p_z
         if(logduo.and.p_z.lt.0.0)p_z = -p_z
         ptot = sqrt(p_z*p_z + p_t*p_t)
         thetau = 57.29578*acos(p_z/ptot)
200      continue

c       fill the raw acceptance histograms
         call hf1(63,rapid,1.)
         call hf1(64,p_t,1.0)
         call hf1(65,ptot,1.)

c       check the cuts
        thetau_ns = thetau
        if(north_south.eq.1)then
          if(thetau.gt.90)then
            thetau_ns = 180 - thetau
          endif ! check on thetau value
        endif ! check on north_south switch set
        
        if((id_geant.eq.7.and.thetau_ns.gt.the_minn.and.
     1    thetau_ns.lt.the_maxn.and.ptot.gt.p_minn).or.
     2    (id_geant.ne.7.and.thetau_ns.gt.the_minc.and.
     3    thetau_ns.lt.the_maxc.and.ptot.gt.p_minc))then
        mxtot = mxtot + 1

c  s    afety check

        if(mxtot.gt.max_mxtot)then
          write(6,*)' '
          write(6,*)' ua1_evt <e>: requested number of',
     1      ' particles exceeds buffer capaicity'
          stop ' max_mxtot is too small in guevgen'
        endif
        
        tran = 360.*rndm(0)
        pptot(2,mxtot) = p_t*cos(tran/57.29578)
        pptot(3,mxtot) = p_t*sin(tran/57.29578)
        pptot(4,mxtot) = p_z
        idtot(mxtot) = id_geant

c fi    ll the filtered acceptance histograms

        call hf1(66,rapid,1.)
        call hf1(67,p_t,1.0)
        call hf1(68,ptot,1.)
        call hf1(69,thetau,1.)
        
      endif   
250   continue   
      enddo

c     for the event header

      nptls = mxtot
      return
      end
      
