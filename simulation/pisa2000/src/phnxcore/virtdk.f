        subroutine virtdk(p1mag,p2mag,p12mag,xsam,ysam,p1,p2)
c
c       subroutine for constrained decay of virtual photon
c
c       CFM: 12/8/96  Revise to use the grndm random number call
c                     instead of rndm in order to preserve an ordered
c                     random number sequence.  But there is still a problem
c                     with the use of hrndm in pdalitz which uses independent
c                     random number sequence.
c
c       P1MAG fixed magnitude of electron (input)
c       P2MAG fixed magnitued of positron (input)
c       P12MAG fixed magnitude of virtual photon (input)
c       P1 vector of electron momentum under these constraints (output)
c       P2 vector of positron momentum under these constraints (output)
c
c       Longitudinal (Z) components sum to P12MAG
c       Transverse components are equal and opposite
c
        implicit none
        real p1mag, p2mag, p12mag, p1(3), p2(3), xsam, ysam
        real th1, th2, phi, fact, p1tran, ptemp

        real gran

        if(p1mag.le.0.0.or.p12mag.le.0.0.or.p2mag.le.0.0)then
                write(6,713)p1mag,p12mag,p2mag
713     format(1h ,'call to virtdk with p1,p12,p2 =',3e14.5)
                return
        endif
c
c       randomize between electron and positron
c
        call grndm(gran,1)
        if(gran.gt.0.5)then
                ptemp=p1mag
                p1mag=p2mag
                p2mag=ptemp
        endif
        fact=(p12mag*p12mag+p1mag*p1mag-p2mag*p2mag)/(2.0*p12mag*p1mag)
        if(fact.ge.+1.0)fact=+0.9999999
        if(fact.le.-1.0)fact=-0.9999999
        th1=57.29578*acos(fact)
        p1(3)=p1mag*cos(th1/57.29578)
        p2(3)=p12mag-p1(3)
        p1tran=p1mag*sin(th1/57.29578)
        th2=57.29578*atan2(p1tran,p2(3))
        call grndm(gran,1)
        phi=360.*gran
        p1(1)=cos(phi/57.29578)*p1tran
        p1(2)=sin(phi/57.29578)*p1tran
        p2(1)=-p1(1)
        p2(2)=-p1(2)
c       write(6,714)p12mag,p1mag,p2mag,xsam,ysam,th1,th2,p1(3),p2(3),
c     1         p2(3)*cos(th2/57.29578)
714     format(2x,'p12,p1,p2,xs,ys',5(e12.5,1x),/,2x,
     1  't1,t2,p13,p23,p23 ',5(e12.5,1x))
        return
        end
