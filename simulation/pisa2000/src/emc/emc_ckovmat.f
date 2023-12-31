      subroutine emc_ckovmat


*	Called by emc_trk_media.f

*	Declares the properties of PbGl for the Cherenkov
*	effect.


      implicit none

cMM   (08.09.2000) include this for the ITCKOV flag

#include "gctmed.inc"


      real*4 pbpm(55),pbap(55),pbde(55),pbri(55)



c     photon momentum in GeV

      data pbpm / 1.508E-9,1.527e-9,1.546e-9,1.565e-9,1.585e-9,
     &		1.606e-9,1.627e-9,1.649e-9,1.671e-9,1.694e-9,
     &		1.718e-9,1.742e-9,1.767e-9,1.792e-9,1.819e-9,
     &		1.846e-9,1.874e-9,1.903e-9,1.932e-9,1.963e-9,
     &		1.995e-9,2.027e-9,2.061e-9,2.096e-9,2.132e-9,
     &		2.17e-9,2.208e-9,2.249e-9,2.29e-9,2.333e-9,
     &		2.378e-9,2.425e-9,2.474e-9,2.524e-9,2.577e-9,
     &		2.631e-9,2.689e-9,2.748e-9,2.811e-9,2.876e-9,
     &		2.945e-9,3.017e-9,3.092e-9,3.171e-9,3.255e-9,
     &		3.343e-9,3.436e-9,3.534e-9,3.638e-9,3.748e-9,
     &		3.865e-9,3.99e-9,4.123e-9,4.265e-9,4.417e-9  /		

c     absorption length in cm

      data pbap / 216.691,223.273,226.981,234.198,240.665,
     &            252.699,270.451,284.825,302.664,319.649,
     &            345.852,370.917,401.482,415.147,427.880,
     &            439.425,457.944,475.751,497.495,521.300,
     &            544.435,566.414,576.381,601.046,612.270,
     &            619.987,648.582,675.259,652.881,652.881,
     &            631.930,586.701,437.458,408.202,538.462,
     &            455.810,391.804,368.112,368.112,349.586,
     &            306.499,249.419,179.455,105.786,58.268,
     &            33.121,15.556,6.349,2.33,0.,
     &            0.,0.,0.,0.,0. /

c     detection efficiency

      data pbde / 0., 0., 0., 0., 0.,
     &            0., 0., 0., 0., 0.,
     &            0., 0., 0., 0., 0.,
     &            0., 0., 0., 0., 0.,
     &            0., 0., 0., 0., 0.,
     &            0., 0., 0., 0., 0.,
     &            0., 0., 0., 0., 0.,
     &            0., 0., 0., 0., 0.,
     &            0., 0., 0., 0., 0.,
     &            0., 0., 0., 0., 0.,
     &            0., 0., 0., 0., 0. /

c     refraction index

      data pbri / 1.636664, 1.636871, 1.637087, 1.637313, 1.637551,
     &            1.637800, 1.638062, 1.638338, 1.638628, 1.638934,
     &            1.639257, 1.639598, 1.639958, 1.640340, 1.640746,
     &            1.641176, 1.641634, 1.642123, 1.642644, 1.643201,
     &            1.643798, 1.644440, 1.645131, 1.645875, 1.646681,
     &            1.647555, 1.648506, 1.649543, 1.650679, 1.651929,
     &            1.653308, 1.654838, 1.656543, 1.658455, 1.660612,
     &            1.663062, 1.665869, 1.669113, 1.672902, 1.677383,
     &            1.682758, 1.689320, 1.697501, 1.707970, 1.721824,
     &            1.740982, 1.769123, 1.814225, 1.896737, 2.074576,
     &            1.449937, 1.1     , 1.1     , 1.1     , 1.1      /



c     (08.09.2000) set itckov flag to one, so that
c     GEANT calculates the step sizes right

      itckov = 1

c     now declare properties

      call gsckov(810,55,pbpm,pbap,pbde,pbri)

      return
      end
