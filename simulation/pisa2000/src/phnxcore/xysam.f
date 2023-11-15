*CMZ :  2.04/00 01/06/93  15.50.58  by  Charles F. Maguire
*CMZ :  2.01/00 09/10/92  14.12.48  by  Charles F. Maguire
*-- Author :
*-- Author :
        subroutine xysam
        implicit none
c
c       subroutine to initialize pi^0 Dalitz decay
c
        real xs, ys, elemas, pizmas, xsam, xmin, delx
        real fxysam, ymax, eta        ! eta is highest limit of y here
        integer  ix, iy
        parameter (pizmas=0.134693, elemas=0.000511)

        call hbook2(1,'Double Differential X-YSAM distribution',1000.0,
     1   xmin, 1.0, 500.0, 0.0, 1.0,0.0)
        call hbook2(2,'Integrated X-YSAM distribution', 1000.0,
     1   xmin, 1.0, 500.0, 0.0, 1.0,0.0)
c
c
c	10/20/2004	F.Kajihara (kajihara@bnl.gov) 
c	Double Differential Conversion Coefficient 
c       By Kroll and Wada equation 13, PR 98 (1958)1355-1359 
c
c	Because it is a even function on y, the integration range 
c	of y becomes from 0.0 to eta with two times factor for the 
c	integration on y. Constants of the equation (\alpha, \pi, 
c	etc.) are abbreviated since HRNDM2 function will normalize 
c	the integration.
c	
c
        xmin=2.0*elemas/pizmas
        delx=(1.0-xmin)/10000.0	          ! more than 1.0e+04  
        ymax=sqrt(1.0-4.0*(elemas/pizmas)*(elemas/pizmas))
        do ix=1,10000                     ! more than 1.0e+04  
          xs=xmin+(float(ix)-0.5)*delx    ! x/mu 
          xsam=xs*pizmas                  ! virtual photon mass
          eta=sqrt(1.0-4.0*(elemas/xsam)*(elemas/xsam))

          do iy=1,500
          ys=0.0+(float(iy)-0.5)*ymax/500.0   ! y energy partition
          if(ys.le.eta)then
            fxysam=((1.0-xs*xs)*(1.0-xs*xs)*(1.0-xs*xs)*
     1         (1.0+ys*ys+4.0*(elemas/xsam)*(elemas/xsam)))/xsam
	  else 
            fxysam=0.0
          endif
            call hf2(1,xs,ys,fxysam)
            call hf2(2,xs,ys,fxysam)
          enddo                          ! iy loop end
        enddo                            ! ix loop end
c

        return
        end
