c     File:- event.inc
c     =================
c
c     This file is intended to be used by the analysis programs.
c     The information from the ZEBRA banks is being unpacked into
c     the common block /event/ with the primary information as
c     read by the input and with secondary information as requested
c     by setup parameters.
c
c        maxptl  : maximum number of particles pr event.
c        nptls        : number of particles in event
c        nnproj  : # neutron spectators (in proj)
c        npproj  : # proton  spectators (in proj)
c        naproj  : # spectators (in proj)
c        nntarg  : # neutron spectators (in target)
c        nptarg  : # proton  spectators (in target)
c        natarg  : # spectators (in target)
C        npnproj : # The number of proton + neutron participants from
c                    projection
c        npntarg : # The number of proton + neutron participants from
c                    target
c       ntry    : # of tries to get an interaction since last call
c                 This number is needed to get a cross section from
c                  the number of attempts + the impact parameter
c                  range (HIJET info; for HIJING1.37 ntry is number of binary collisions)
c
c        px        : 4 momentum of each particle
c        py        :
c        pz        :
c        energy        :
c        p4vec(1,i) : px for particle i
c        p4vec(2,i) : py for particle i
c        p4vec(3,i) : pz for particle i
c        p4vec(4,i) : energy for particle i
C  NOTE!!!!!!
c        px,py,pz,energy and p4vec are mapped into the same memory locations,
c        only one set will be valid.
c
c        idptl        : ID for particle (as given by event generator)
c        ioptl         : Parent sequence number ( 0 if no parent)
c
c        pt        : perpendicular momentum (sqrt(px**2+py**2)
c        y        : rapidity
c        pt      : transverse momentum (GeV/c)
c        mass        : mass in GeV of particle
c        theta        : polar angle theta in radians
c        phi        : azimuthal angle in radians
c        gtype        : GEANT particle ID.
c        xyzvec(3,I)  : the primary position of the particle. !liq
c        N_event : the No. of events !liq

        integer maxptl
        parameter (maxptl=15000)
        integer nptls, ntry
        integer nnproj,npproj,naproj
        integer nntarg,nptarg,natarg
        integer npntarg,npnproj
        real*4        bimevt
        real*4        px(maxptl)
        real*4        py(maxptl)
        real*4        pz(maxptl)
        real*4        energy(maxptl)
        integer*4 idptl(maxptl)
        integer*4 ioptl(maxptl)
        real*4        xyzvec(3,maxptl)  
c
        real*4  mass(maxptl)
        real*4  theta(maxptl)
        real*4  phi(maxptl)
        real*4  y(maxptl)
        real*4        pt(maxptl)
        integer*4  gtype(maxptl)

c
c       changed to overlap with the venus common block
c
        common /venus_cmn/ nptls, bimevt,
     &                nntarg, nptarg, natarg,
     &                nnproj, npproj, naproj,
     &                ntry,
     &                px,py,pz,energy,xyzvec,
     &                idptl,ioptl,
     &                mass,theta,phi,y,gtype,pt
     &               ,npntarg,npnproj

c
c  Create 2-dimensional array for 4-vector and map over individual arrays
c
        real*4 p4vec(4,maxptl) 
        equivalence (p4vec(1,1),px(1))
        
c
c
c        Reaction description
c
c        atarg        : Target Mass Number
c        ztarg  : Target Charge Number
c        aproj  : Projectile Mass number
c        atarg  : Projectile Charge Number
c        Bmin    : minimum impact parameter (normally 0)
c        Bmax    : maximum impact parameter
c        Nbeam   : Number of beam particles thrown into the
c                  area defined by bmin->bmax
c
        integer atarg, ztarg, aproj, zproj, nbeam,N_event
        real*4  sqrt_s, bmin, bmax
        real*4  ptChange, pzChange
        real*4  p0topch, k0tokch, pitokaon
        common /reaction/ atarg, ztarg, aproj, zproj, sqrt_s
     &                , bmin, bmax, nbeam
     &                , N_event, ptChange, pzChange
     &                , p0topch, k0tokch, pitokaon
 
        integer event_code
        real*4  rextra, event_version
        integer iextra
        common /control/ event_code, event_version,
     &                rextra(50), iextra(50)
