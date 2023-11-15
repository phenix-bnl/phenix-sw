*CMZ :  2.04/00 05/10/92  11.19.39  by  Charles F. Maguire
*-- Author :
*-- Author :
      SUBROUTINE EULER(R,PHI,THETA,PSI,IFLAG)
C
C     performs Euler rotation with notation in Goldstein, Klass. Mech
C     Wiesbaden 1978
C
       DIMENSION R(3),rprim(3)
       real      phi,theta,psi
       do i=1,3
        RPRIM(I) = R(I) ! save local coordinates
       enddo
       cphi=cos(phi)
       sphi=sin(phi)
       cthe=cos(theta)
       sthe=sin(theta)
       cpsi=cos(psi)
         spsi=sin(psi)
C
C
C
      if(iflag.eq.0) then        ! matrix A
       r(1) =   (cpsi*cphi  - cthe*sphi*spsi)*rprim(1)
     1  + (cpsi*sphi  + cthe*cphi*spsi)*rprim(2)
     1  + (sthe*spsi)             *rprim(3)
       r(2) =   (-spsi*cphi - cthe*sphi*cpsi)*rprim(1)
     1  + (-spsi*sphi + cthe*cphi*cpsi)*rprim(2)
     1  + (sthe*cpsi)             *rprim(3)
       r(3) =   (sthe*sphi)                  *rprim(1)
     1  + (-sthe*cphi)                 *rprim(2)
     1  + (cthe)                       *rprim(3)
      else  ! use inverse matrix  A**-1
       r(1) =   (cpsi*cphi  - cthe*sphi*spsi)*rprim(1)
     1  + (-spsi*cphi - cthe*sphi*cpsi)*rprim(2)
     1  + (sthe*sphi)                  *rprim(3)
       r(2) =   (cpsi*sphi  + cthe*cphi*spsi)*rprim(1)
     1  + (-spsi*sphi + cthe*cphi*cpsi)*rprim(2)
     1  + (-sthe*cphi)                 *rprim(3)
       r(3) =   (sthe*spsi)             *rprim(1)
     1  + (sthe*cpsi)             *rprim(2)
     1  + (cthe)                       *rprim(3)
      endif
99       RETURN
        END
