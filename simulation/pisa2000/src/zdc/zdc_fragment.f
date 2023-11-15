******************************************************************
* RHIC-PHENIX GEANT ZDC detector simulation.
* Event bank modification after collision to produce fragments
* other then elementary particles.
* Author: Andras Ster, KFKI/Hungary
* Creation date: 30.November.1999
******************************************************************

      SUBROUTINE zdc_fragment

      IMPLICIT NONE
 
      INCLUDE 'event.inc'

      COMMON/ZDCINP/ dxfield, fragment_model, zdc_hbook
      REAL dxfield
      INTEGER fragment_model, zdc_hbook
     
      REAL       P_fermi, Mass_neutron, Mass_deuteron
      INTEGER    Id_neutron, Id_proton, Id_deuteron

      PARAMETER (P_fermi = 0.025)
      PARAMETER (Mass_neutron = 0.93956563, Mass_deuteron = 1.875613)
      PARAMETER (Id_neutron = 13, Id_proton = 14, Id_deuteron = 45)

      INTEGER N_spectator_n(2), N_spectator_p(2), index_n(2)
      INTEGER N_spectator_n_expected, NA49_neutron_spectators
      INTEGER Id_particle(2, maxptl)

      COMMON /spectator/ spectator_particles(maxptl), I_spectator,
     +n_participant_part
      INTEGER spectator_particles, I_spectator, n_participant_part

      INTEGER i,j, dir
      REAL    P_z, P_t
      REAL*8  E_beam, P_beam, beta, gamma

      E_beam = sqrt_s / 2.0
      P_beam = sqrt(E_beam**2 - dble(Mass_neutron**2))
      beta   = P_beam / E_beam
      gamma  = 1.0 / sqrt( 1.0 - beta**2)


*     Currently 1 fragmentation model exists

      IF(fragment_model .EQ. 0) RETURN

***   Calculate the number of spectator neutrons (left:1, right:2)

      N_spectator_n_expected = NA49_neutron_spectators(bimevt)

      dir = 1
      N_spectator_n(dir) = 0
      N_spectator_p(dir) = 0
      dir = 2
      N_spectator_n(dir) = 0
      N_spectator_p(dir) = 0

      DO i = 1, nptls

        Id_particle(1, i) = gtype(i)
        Id_particle(2, i) = gtype(i)
        spectator_particles(i) = 0

        IF (Id_particle(1, i) .EQ. Id_neutron .OR. 
     +      Id_particle(1, i) .EQ. Id_proton) THEN

          P_z = abs(p4vec(3,i))
          P_z = abs(gamma * (dble(P_z) - beta * dble(p4vec(4,i))))
          P_t = sqrt(p4vec(1,i)**2 + p4vec(2,i)**2)

          IF (P_t .LE. P_fermi .AND. P_z .LE. P_fermi) THEN
            dir = 1
            IF (p4vec(3,i) .LT. 0.0) dir = 2
            IF (Id_particle(1, i) .EQ. Id_neutron) THEN
              N_spectator_n(dir) = N_spectator_n(dir) + 1
              IF(N_spectator_n(dir) .EQ. N_spectator_n_expected + 1)
     +            index_n(dir) = i
              spectator_particles(i) = 1
            ENDIF
            IF (Id_particle(1, i) .EQ. Id_proton)
     +         N_spectator_p(dir) = N_spectator_p(dir) + 1
            Id_particle(dir, i) = - Id_particle(dir, i)
          ENDIF
        ENDIF

      ENDDO

      print *, ' SPECTATOR NUCLEONS'
      print *, ' expected: ', N_spectator_n_expected
      print *, ' neutrons: ', N_spectator_n
      print *, ' protons : ', N_spectator_p

      n_participant_part = 396
      DO dir = 1, 2
        n_participant_part = n_participant_part - N_spectator_n(dir) 
        n_participant_part = n_participant_part - N_spectator_p(dir) 
      ENDDO

      IF(N_spectator_n(1).LE.N_spectator_n_expected) index_n(1)=nptls+1
      IF(N_spectator_n(2).LE.N_spectator_n_expected) index_n(2)=nptls+1

      IF(index_n(1) .GT. nptls .AND. index_n(2) .GT. nptls ) RETURN


***   Compose deuterons from excess neutrons + protons

      DO dir = 1, 2

      j = 1

      DO i = index_n(dir), nptls

        IF(Id_particle(dir, i) .EQ. -Id_neutron) THEN

          IF (j .GT. nptls) THEN
            gtype(i) = Id_proton
            spectator_particles(i) = 0
          ENDIF

          DO WHILE(j .LE. nptls .AND. Id_particle(dir,j).NE.-Id_proton)
             j = j + 1
          ENDDO
          IF (j .LE. nptls) THEN
            p4vec(1, i) = p4vec(1, i) + p4vec(1, j)
            p4vec(2, i) = p4vec(2, i) + p4vec(2, j)
            p4vec(3, i) = p4vec(3, i) + p4vec(3, j)
            p4vec(4, i) = sqrt(p4vec(1, i)**2 + p4vec(2, i)**2
     +                  +      p4vec(3, i)**2 + Mass_deuteron**2)
            xyzvec(1, i) = (xyzvec(1, i) + xyzvec(1, j) ) / 2.0
            xyzvec(2, i) = (xyzvec(2, i) + xyzvec(2, j) ) / 2.0
            xyzvec(3, i) = (xyzvec(3, i) + xyzvec(3, j) ) / 2.0
*           gtype(i) = Id_deuteron
            gtype(i) = Id_proton
            mass(i)  = Mass_deuteron
            theta(i) = (theta(i) + theta(j)) / 2.0
            phi(i)   = (phi(i) + phi(j)) / 2.0
            y(i)     = y(i)

            Id_particle(dir, j) = 0
            spectator_particles(i) = 0
            j = j + 1
         ENDIF
       ENDIF

      ENDDO

      ENDDO


***   Compress the array of the remainder particles


      j = 0

      DO i = 1, nptls
        IF(Id_particle(1, i) .NE. 0 .AND. Id_particle(2, i) .NE. 0) THEN
          j = j + 1
          p4vec(1, j) = p4vec(1, i)
          p4vec(2, j) = p4vec(2, i)
          p4vec(3, j) = p4vec(3, i)
          p4vec(4, j) = p4vec(4, i)
          xyzvec(1, j) = xyzvec(1, i)
          xyzvec(2, j) = xyzvec(2, i)
          xyzvec(3, j) = xyzvec(3, i)
          idptl(j) = idptl(i)
          ioptl(j) = ioptl(i)
          gtype(j) = gtype(i)
          mass(j)  = mass(i)
          theta(j) = theta(i)
          phi(j)   = phi(i)
          y(j)     = y(i)

          spectator_particles(j) = spectator_particles(i)
        ENDIF
      ENDDO

*     Reset 'nptls' to the new particle number
 
      nptls = j

      RETURN
      END



      INTEGER FUNCTION NA49_neutron_spectators(b)

*     Empirical formula for the number of spectator neutrons

      IMPLICIT NONE
 
      INCLUDE 'event.inc'
     
      REAL    neutron_spectators(3,8), slopes(2,8)
      REAL    b, bb, err, dn, ran
      INTEGER i, n 

      DATA neutron_spectators
*                              ! See ref.: EPJ. A2, 383-390 (1998)
*            b,    N, error,
     1     / 0.0,  0.9, 0.5,   ! from linear interpolation between lines 2-3
 
     2       2.0,  9.1, 3.3,   ! next 4 lines are the pure neutron table
     3       4.0, 17.3, 6.1,
     4       5.8, 27.8, 7.9,
     5       7.6, 37.6, 8.1,

     6      10.0, 39.2, 7.8,   ! from Fig. 6 interpolated and estimated
     7      12.5, 25.3, 5.6,   ! ~20% errors 
     8      15.0, 00.0, 0.0/   ! 

      DATA slopes              ! dN/db, d_error/db calculated from above
     1     /      0.00, 0.00,
     2            4.10, 1.40,
     3            4.10, 1.40,
     4            5.83, 1.00,
     5            5.44, 0.11,
     6            0.66,-0.12,
     7           -5.56,-0.95,
     8           -9.88,-2.24/

      SAVE neutron_spectators, slopes


*     Calculate the expected number of neutrons by interpolating between
*     data points above (N above).


      bb = b
      IF(bb .GT. neutron_spectators(1, 8)) bb = neutron_spectators(1, 8)

      i = 2
      DO WHILE(bb .GT. neutron_spectators(1, i))
        i = i + 1
      ENDDO

      dn  = (bb - neutron_spectators(1, i - 1)) * slopes(1, i) +
     +           neutron_spectators(2, i - 1)

*     err = (bb - neutron_spectators(1, i - 1)) * slopes(2, i) +
*    +           neutron_spectators(3, i - 1)
*
*     CALL grndm(ran, 1)
*     err = err * sqrt((-alog(ran+1.e-10)))
*     CALL grndm(ran, 1)
*     IF(ran .LT. 0.5) err = -err
*     n =  INT(dn + err + 0.5)

*     Gold/Lead correction = 197/208 = 0.94712

      dn = dn * 0.94712

      n = INT(dn + 0.5)
      IF(n .LT. 0) n = 0

      NA49_neutron_spectators =  n

      RETURN
      END



      SUBROUTINE zdc_fragment_update(j, i)

      IMPLICIT NONE
 
      INCLUDE 'event.inc'

      INTEGER j, i
 
      COMMON /spectator/ spectator_particles(maxptl), I_spectator,
     +n_participant_part
      INTEGER spectator_particles, I_spectator, n_participant_part

      spectator_particles(j) = spectator_particles(i)

      RETURN
      END

