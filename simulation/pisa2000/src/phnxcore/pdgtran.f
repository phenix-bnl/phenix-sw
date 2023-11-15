c $Id: pdgtran.f,v 1.6 2009/03/09 16:35:04 hpereira Exp $ 
      integer function pdgtran(ID,mconv)
            
C...convert (mconv=1) from PDG98 numbering scheme to STD numbering scheme
C...     or (mconv=2) from STD numbering scheme to PDG98 numbering scheme

C           ID = particle identification number

C  Last Modified on June 4, 2002 by Anuj K. Purwar
C     1) Added deuteron/antideuteron (id's pass through to make it
C        consistent with PISA/Exodus)


#include "stdlun.inc"
#include "pisa_parts.inc"
            
C... ITABJ(I) converts miscellaneous PDG92 particle ID's to a standard scheme
      integer itabj(100,2), noant(8,2)

      data itabj/1,2,3,4,5,6,7,8,0,0,
     +           11,12,13,14,15,16,17,18,0,0,
     +           21,22,23,24,25,26,0,0,0,0,
     +           0,32,33,34,35,36,37,0,39,0,
     +           41,42,0,0,45,0,0,0,0,0,
     +           0,0,0,0,55,0,0,0,0,0,
     +           0,0,0,0,0,0,0,0,0,0,
     +           0,0,0,0,0,0,0,0,0,0,
     +           81,82,83,84,85,86,87,88,89,90,
     +           91,92,93,94,95,96,97,98,99,100,
     +           1,2,3,4,5,6,7,8,0,0,
     +           11,12,13,14,15,16,17,18,0,0,
     +           21,22,23,24,25,0,0,0,0,0,
     +           0,32,33,34,35,36,37,0,39,0,
     +           41,42,0,0,45,0,0,0,0,0,
     +           0,0,0,0,55,0,0,0,0,0,
     +           0,0,0,0,0,0,0,0,0,0,
     +           0,0,0,0,0,0,0,0,0,0,
     +           81,82,83,84,85,86,87,88,89,90,
     +           91,92,93,94,95,96,97,98,99,100/
                    
      data noant/-21,-22,-23,-25,-32,-33,-35,-36,
     °           -21,-22,-23,-25,-32,-33,-35,-36/
                    
      save itabj,noant

      pdgtran=id
      ida=iabs(id)
      j1=mod(ida,10)
      i1=mod(ida/10,10)
      i2=mod(ida/100,10)
      i3=mod(ida/1000,10)
      i4=mod(ida/10000,10)
      ksusy = mod(ida/1000000,10)
      ku = mod(ida/10000000,10)
      kqn=mod(ida/1000000000,10)

      ! do not change 'user defined' particles
      if( 
     +  id .ge. PISA_PART_MIN .and. 
     +  id .le. PISA_PART_MAX ) then
        
        pdgtran = id

      else if(ida.eq.0)then
C..        write(lnhout,*) ' PDGTRAN 1: particle ID is zero'
      else if( mconv.lt.1 .or. mconv.gt.2) then
              
        pdgtran = 0
        write(lnhout,*) ' PDGTRAN 2: unallowed conversion option'
        
      !ions not allowed
      elseif(kqn.eq.1) then
              
        pdgtran=0
        write(lnhout,*) ' PDGTRAN 3: unallowed ion'
              
      elseif(ksusy.eq.1 .or. ksusy.eq.2) then
              
C...SUSY 
              
      elseif(ksusy.eq.3) then
              
C...technicolor
        if(mconv.eq.2) then
	         if(ida.eq.3100111)  pdgtran=0
        endif  
              
      elseif(ksusy.eq.4) then
              
C...excited quarks and leptons
              
        elseif(ida.le.100)then
C...Higgs, etc.
          pdgtran=isign(itabj(ida,mconv),id)
C...check for illegal antiparticles
                
          if(id.lt.0)then
            
            do 101 j=1,8
              if(pdgtran.eq.noant(j,mconv)) pdgtran = 0
 101        continue
          endif
                  
        elseif(j1.eq.0)then

c special particles
c check for illegal anti KS, KL
                
          if(id.eq.-130 .or. id.eq.-310) pdgtran=0
                
      elseif(ksusy.eq.9 .and. ku.eq.9 .and. mconv.eq.2) then

        pdgtran = 0
                
      elseif(i1.ne.0 .and. i3.ne.0 .and. j1.eq.2) then
              
C...spin 1/2 baryons
C...no excited baryon states
              
        if(mconv.eq.1)then
                
          if(i4.ne.0) pdgtran = 0
          if(i3.eq.1) pdgtran = 0
          if(i3.eq.2 .and. i2.eq.1 .and. i1.eq.2) pdgtran = 0
          if(i3.eq.2 .and. i2.eq.2 .and. i1.eq.2) pdgtran = 0
                
        elseif(mconv.eq.2) then
      
          if(i3.ge.6) pdgtran=0
                
        endif
              
      elseif(i1.ne.0 .and. i3.ne.0 .and. j1.eq.4)then
              
C...spin 3/2 baryons
C...no excited baryon states
              
        if(mconv.eq.1)then
                
          if(i4.ne.0) pdgtran = 0
          if(i3.ge.3 .and. i2.eq.1 .and. i1.eq.2) pdgtran = 0
          if(i3.eq.1 .and. i2.eq.2 .and. i1.eq.1) pdgtran = 0
          if(i3.eq.2 .and. i2.eq.1 .and. i1.eq.2) pdgtran = 0
                
        elseif(mconv.eq.2) then
                
	         if(i3.ge.6) pdgtran=0
                
        endif
              
      elseif(i1.ne.0 .and. i2.ne.0 .and. i3.eq.0)then
              
C...mesons 
              
        if(mconv.eq.1)then
        elseif(mconv.eq.2) then
      
          if(ida.eq.9020553) pdgtran=0
	         if(i2.ge.6) pdgtran=0
                
        endif
              
C...check for illegal antiparticles
              
        if(i1.eq.i2 .and. id.lt.0) pdgtran=0
        elseif(i2.ne.0 .and. i3.ne.0 .and. i1.eq.0)then
                
C...diquarks
          if(i3.ge.6) pdgtran=0

        else
                
C...undefined
          pdgtran=0
                
        endif
              
C...check for illegal anti KS, KL
        if(id.eq.-130 .or. id.eq.-310) pdgtran=0

c  #if HEPDBG
c  there are too many warnings - use only for debugging purposes
c       if(pdgtran.eq.0 .and. ida.ne.0)then
cscj        if(mconv.eq.1) write(lnhout,111) id
cscj        if(mconv.eq.2) write(lnhout,112) id
cscj      endif
cscj#endif
              
c       write( *,* )
c      + 'pdgtran -',
c      + ' input id: ', id,
c      + ' output id: ', pdgtran
              
      return
 111  format('  PDGTRAN 3: PDG98 particle ',I8,' translates to zero')
 112  format('  PDGTRAN 3: STD   particle ',I8,' translates to zero')
      end
