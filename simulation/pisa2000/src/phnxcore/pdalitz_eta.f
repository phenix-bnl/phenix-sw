        subroutine pdalitz_eta(etamom, phomom, elemom, posmom)



c       subroutine to generate PI Zero Dalitz decay according to
c       N. Samios, Phys. Rev. 121, (1961) 275

c       CFM: 12/8/96  Revise to use the grndm random number call
c                     instead of rndm in order to preserve an ordered
c                     random number sequence.  But there is still a problem
c                     with the use of hrndm which uses in independent
c                     random number sequence

c	F.Kajihara: 10/27/04 modified pi0 dalitz decay to impliment eta dalitz decay.  

        implicit none

c       ETAMOM  Lab four momentum vector for the ETA (input)
c       PHOMOM  Lab four momentum vector for the Photon (output)
c       ELEMOM  Lab four momentum vector for the Electron (output)
c       POSMOM  Lab four momentum vector for the Positron (output)

        real etamom(4), phomom(4), elemom(4), posmom(4), ptemp

#include "gckine.inc"
#include "gcflag.inc"
#include "subevt.inc"

        real xsam, ysam, etamas, virtm, evirt, pvirt
        real costh, sinth, cosph, sinph, phi, elemas
        real pcmv(4), pcmp(4), eele, epos, pele, ppos
        real pcmele(4), pcmpos(4), beta(4), peta, eeta, xsm2, ysm2
        real hrndm, thpos, phpos, thele, phele, thdiff

        real gran(5)

        parameter (etamas=0.54775, elemas=0.000511)
        integer icall, i, ntlim, ntent
        parameter (ntlim=20000)
        data icall, ntent /0, 0/
        save icall, ntent

c       NTUPLE entries

c       data ch130 /'PIMO', 'PITH', 'PIPH', 'PIET', 'POMO',
c     1 'POTH', 'POPH', 'POET', 'ELMO', 'ELTH', 'ELPH', 'ELET',
c     2 'XSAM', 'YSAM', 'EPTH','XSM2','YSM2','PECM','EECM'/

        if(icall.eq.0)then

c       first call: generate the X and Y distribution functions

                call xysam_eta
                icall=1
        endif

c       now choose a virtual mass fraction XSAM

	call hrndm2(2, xsam, ysam)
        virtm=xsam*etamas

c       calculate the first decay product in the center-of-mass system

        pvirt=etamas*(1.0-xsam*xsam)/2.0
        evirt=sqrt(pvirt*pvirt+virtm*virtm)

c       isotropic decay in the center-of-mass system (virtual photon and
c                                                       real photon)

        call grndm(gran,5)
        costh=-1.0+2.0*gran(1)
        if(costh.ge.+1.0)costh=+0.9999999
        if(costh.le.-1.0)costh=-0.9999999
        sinth=sqrt(1.0-costh*costh)

c       generate random azimuthal direction

        phi=360.*gran(2)
        cosph=cos(phi/57.29578)
        sinph=sin(phi/57.29578)

c       polar coordinates to momentum components for virtual photon

        pcmv(1)=pvirt*sinth*cosph
        pcmv(2)=pvirt*sinth*sinph
        pcmv(3)=pvirt*costh
        pcmv(4)=evirt

c       now real photon

        pcmp(1)=-pcmv(1)
        pcmp(2)=-pcmv(2)
        pcmp(3)=-pcmv(3)
        pcmp(4)=sqrt(pcmp(1)*pcmp(1)+pcmp(2)*pcmp(2)+pcmp(3)*pcmp(3))

c       decay the virtual photon subject to the energy partition constraint

        if(ysam.ge.1.0)ysam=0.9999999
        if(gran(3).gt.0.5)ysam=-ysam
        epos=(ysam*pvirt+evirt)/2.
        eele=evirt-epos
        if(epos.lt.elemas.or.eele.le.elemas)then
                write(6,611)epos,eele
611     format(2x,'mass too low? ',2e14.5)
        endif
        ppos=dsqrt(dabs(dble(epos*epos-elemas*elemas)))
        pele=dsqrt(dabs(dble(eele*eele-elemas*elemas)))

c       constrained decay kinematics (Z axis along PVIRT for now)

        call virtdk(ppos,pele,pvirt,xsam,ysam,pcmele,pcmpos)
        pcmele(4)=dsqrt(dble(pcmele(1)*pcmele(1)+pcmele(2)*pcmele(2)+
     1                  pcmele(3)*pcmele(3)+elemas*elemas))
        pcmpos(4)=dsqrt(dble(pcmpos(1)*pcmpos(1)+pcmpos(2)*pcmpos(2)+
     1                  pcmpos(3)*pcmpos(3)+elemas*elemas))

c       rotate to true center-of-mass frame

        call gurot(pcmele,costh,sinth,cosph,sinph)
        pcmele(4)=dsqrt(dble(pcmele(1)*pcmele(1)+pcmele(2)*pcmele(2)+
     1                  pcmele(3)*pcmele(3)+elemas*elemas))
        call gurot(pcmpos,costh,sinth,cosph,sinph)
        pcmpos(4)=dsqrt(dble(pcmpos(1)*pcmpos(1)+pcmpos(2)*pcmpos(2)+
     1                  pcmpos(3)*pcmpos(3)+elemas*elemas))

c       check that the XSAM and YSAM are still right

           ptemp =
     1          (pcmpos(1)+pcmele(1))*(pcmpos(1)+pcmele(1)) +
     2          (pcmpos(2)+pcmele(2))*(pcmpos(2)+pcmele(2)) +
     3          (pcmpos(3)+pcmele(3))*(pcmpos(3)+pcmele(3))
        if(ptemp.gt.0.0)then
            ptemp = sqrt(ptemp)
            xsm2=sqrt((pcmpos(4)+pcmele(4))*(pcmpos(4)+pcmele(4)) -
     1          ptemp*ptemp)/etamas
            ysm2=abs(pcmpos(4)-pcmele(4))/ptemp
        else
             ysm2=-1.
             xsm2=ptemp
        endif

c       transform all center-of-mass momenta back to lab frame
c       first take frame along  the etaero lab momentum
c       then rotate to the true lab frame

        peta=sqrt(etamom(1)*etamom(1)+etamom(2)*etamom(2)+
     1                  etamom(3)*etamom(3))
        eeta=sqrt(peta*peta+etamas*etamas)
        beta(1)=0.0
        beta(2)=0.0
        if(eeta.eq.0.0.or.peta.eq.0.0)then
                write(6,635) ntent,eeta,peta
635     format(2x,'ntent, eeta, peta ',i5,2e14.5)
        else
                beta(3)=-peta/eeta
                costh=etamom(3)/peta
        endif
        beta(4)=eeta/etamas
        sinth=sqrt(abs(1.-costh*costh))
        if(sinth.ne.0.0.and.peta.ne.0.0)then
                cosph=etamom(1)/(sinth*peta)
                sinph=etamom(2)/(sinth*peta)
        else
                sinph=0.0
                cosph=1.0
        endif

c       First for the real photon
c       Lorentz boost along PI Zero momentum direction

        call guloren(beta,pcmp,phomom)

c       Rotate to actual lab frame

        call gurot(phomom,costh,sinth,cosph,sinph)
        phomom(4)=sqrt(phomom(1)*phomom(1)+phomom(2)*phomom(2)+
     1                  phomom(3)*phomom(3))

c       Now for the positron

        call guloren(beta,pcmpos,posmom)
        call gurot(posmom,costh,sinth,cosph,sinph)
        posmom(4)=dsqrt(dble(posmom(1)*posmom(1)+posmom(2)*posmom(2)+
     1                  posmom(3)*posmom(3)+elemas*elemas))

c       Now for the electron

        call guloren(beta,pcmele,elemom)
        call gurot(elemom,costh,sinth,cosph,sinph)
        elemom(4)=dsqrt(dble(elemom(1)*elemom(1)+elemom(2)*elemom(2)+
     1                  elemom(3)*elemom(3)+elemas*elemas))

c       randomize between positron and electron

        if(gran(4).ge.0.5)then

c       switch positron and electron

                do i=1,4
                ptemp=posmom(i)
                posmom(i)=elemom(i)
                elemom(i)=ptemp
                enddo
        endif

c       fill ntuple

        if(ntent.lt.ntlim)then
                ntent=ntent+1
                ptemp=sqrt(etamom(1)*etamom(1)+etamom(2)*etamom(2)+
     1                          etamom(3)*etamom(3))
                if(ptemp.le.0.0)then
                        write(6,713) ptemp, xsam, ysam, ntent
713     format(3x,'pzt,xs,ys,nt ',3e12.5,i4)
                else
                        costh=etamom(3)/ptemp
                endif
                if(costh.ge.+1.0)costh=+0.9999999
                if(costh.le.-1.0)costh=-0.9999999
                if(sinth.ne.0.0.and.ptemp.ne.0.0)then
                        cosph=etamom(1)/(sinth*ptemp)
                        sinph=etamom(2)/(sinth*ptemp)
                else
                        sinph=0.0
                        cosph=1.0
                endif

c       compute difference angle in lab

                ptemp=sqrt(posmom(1)*posmom(1)+posmom(2)*posmom(2)+
     1                          posmom(3)*posmom(3))
                if(ptemp.le.0.0)then
                        write(6,714) ptemp, xsam, ysam, ntent
714     format(3x,'pzp,xs,ys,nt ',3e12.5,i4)
                else
                        costh=posmom(3)/ptemp
                endif
                if(costh.ge.+1.0)costh=+0.9999999
                if(costh.le.-1.0)costh=-0.9999999
                thpos=57.29578*acos(costh)
                if(sinth.ne.0.0.and.ptemp.ne.0.0)then
                        cosph=posmom(1)/(sinth*ptemp)
                        sinph=posmom(2)/(sinth*ptemp)
                else
                        sinph=0.0
                        cosph=1.0
                endif
                phpos=57.29578*atan2(sinph,cosph)
                ptemp=sqrt(elemom(1)*elemom(1)+elemom(2)*elemom(2)+
     1                          elemom(3)*elemom(3))
                if(ptemp.le.0.0)then
                        write(6,715) ptemp, xsam, ysam, ntent
715     format(3x,'pze,xs,ys,nt ',3e12.5,i4)
                else
                        costh=elemom(3)/ptemp
                endif
                if(costh.ge.+1.0)costh=+0.9999999
                if(costh.le.-1.0)costh=-0.9999999
                thele=57.29578*acos(costh)
                if(sinth.ne.0.0.and.ptemp.ne.0.0)then
                        cosph=elemom(1)/(sinth*ptemp)
                        sinph=elemom(2)/(sinth*ptemp)
                else
                        sinph=0.0
                        cosph=1.0
                endif
                phele=57.29578*atan2(sinph,cosph)
                thdiff=cos(thele/57.29578)*cos(thpos/57.29578) +
     1                  sin(thele/57.29578)*
     2                  sin(thpos/57.29578)*
     3                  cos((phele-phpos)/57.29578)
                if(thdiff.ge.+1.0)thdiff=+0.9999999
                if(thdiff.le.-1.0)thdiff=-0.9999999
                thdiff=57.29578*acos(thdiff)

c       randomize to get symmetrical peak

                if(gran(5).ge.0.5)thdiff=-thdiff
        endif                   ! condition on filling ntuple

        return
        end
