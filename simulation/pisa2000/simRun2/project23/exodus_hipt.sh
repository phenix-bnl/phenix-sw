#! /usr/bin/csh -f
#

#setenv WORKDIR /common/b3/phoncs/enterria
setenv WORKDIR /afs/rhic/phenix/users/enterria/shit/exodus

##################### 1. EXODUS #######################

# Selected options:
#
# 6                                        --> Single-particle generator
# $NEVT                                    --> Number of events
# ${WORKDIR}/oscar_files/pi0_run$DST.oscar --> Output OSCAR ascii file
# 1                                        --> Flat distribution
# 111                                      --> 22=photon, 111=pi0, 221=eta
# 0.                                       --> Minimum momentum (GeV)
# 12.                                      --> Maximum momentum (GeV)
# 4                                        --> EMCAL Phi range:
#                                              1. Run-1 PbSc W0-W1 [-34. deg, 12. deg]
#                                              2. Run-1 PbGl E1 [168. deg, 192 deg.] 
#                                              3. Run-1 EMCal (PbSc W0-W1 + PbGl E1) 
#                                              4. Run-2 EMCal (PbSc W0-W3, E2-E3 + PbGl E0-E1) 
#                                              5. "1-unit rapidity & full phi" (|y| < 0.5 , phi = 0-2pi) 
#                                          --> Hard-coded in exodus/src/GenerateSingleParticles.cpp:
#                                             |y| <= 0.386 and |vtx_z| < 30 cm
#

cd ${WORKDIR}

setenv NEVT 12000
set LOOP = 1

while (${LOOP}<7) # 
#foreach LOOP (1 2 3 4 5 6)

set MACHINE = 1

while (${MACHINE}<32) # nodes: 1-32

 ./exodus_generate<<EOF
 6
 ${NEVT}
 ${WORKDIR}/pi0_vanode${MACHINE}_${LOOP}.oscar
 1
 111
 8
 14
 4
EOF

 echo "${WORKDIR}/pi0_node${MACHINE}_${LOOP}.oscar created"
 #mv -f ${WORKDIR}/inputKin.root ${WORKDIR}/inputKin_${LOOP}.root
@ MACHINE += 1
end

@ LOOP += 1
end
