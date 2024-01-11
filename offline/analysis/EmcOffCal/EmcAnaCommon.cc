#include <cstdlib>
#include <iostream>
#include <sstream>

#include "EmcAnaCommon.h"

const double EmcAnaCommon::ECORE_RANGE_FOR_NHIT_HIST[N_ECORE_RANGE_FOR_NHIT_HIST+1] = {
   0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 
   1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 
   2,   3,   4,   5,   6,   7,   8,   9,   10,
   11,  12,  13,  14,  15,  16,  17,  18,  19,  20,
   21,  22,  23,  24,  25,  26,  27,  28,  29,  30
};

const char* EmcAnaCommon::SECTOR_NAME[N_ARMSECT] = {
   "W0", "W1", "W2", "W3", "E0", "E1", "E2", "E3"
};

//void EmcAnaCommon::FindHitPos(
//   TVector3* p, Int_t& arm, Int_t& sect, Int_t& ypos, Int_t& zpos )
//        {
//   const double PI = acos(-1);
//   const double SECT_WIDTH = 22.5 * PI / 180;
//   const double PHI_CENT[N_SECTOR] = {
//      -SECT_WIDTH, 0, SECT_WIDTH, 2*SECT_WIDTH
//      -0.3927, 0.0, 0.3927, 0.7854
//   };
//
//   Double_t phi   = p->Phi();
//   Double_t theta = p->Theta();
//   Double_t phi_tmp, theta_tmp;
//   if ( -acos(-1.0) / 2 < phi && phi < acos(-1.0) / 2) { // West arm
//      arm = 0;
//      phi_tmp   = phi;
//      theta_tmp = theta;
//   } else { // East arm
//      arm = 1;
//      phi_tmp = asin(sin(phi)); // projection to [-pi/2, pi/2]
//      theta_tmp = pi - theta; // eta -> -eta
//   }
//   for (int isect = 0; isect < N_SECTOR; isect++) {
//      if (fabs(phi_tmp - PHI_CENT[isect]) < SECT_WIDTH / 2) {
//         sector = isect;
//         int armsect = 4 * arm + sector;
//         int ny, nz;
//         double width_y, width_z;
//         if (IsPbGl(armsect)) {
//            ny = N_YPOS_PBGL;
//            nz = N_ZPOS_PBGL;
//            width_y = 0.3825;
//            width_z = 0.7650;
//         } else {
//            ny = N_YPOS_PBSC;
//            nz = N_ZPOS_PBSC;
//            width_y = 0.3491;
//            width_z = 0.6982;
//         }
//         int ypos = 
//
//      }
//   }
//
//      if ( (-0.5627 < phi_pr && phi_pr < -0.2227) || // W0
//           (-0.17   < phi_pr && phi_pr <  0.17  ) || // W1
//           ( 0.2227 < phi_pr && phi_pr <  0.5627) || // W2
//           ( 0.6154 < phi_pr && phi_pr <  0.9554)    // W3
//         ) return true;
//   } else { // East arm
//      if ( (-0.5527 < phi_pr && phi_pr < -0.2327) || // E0
//           (-0.16   < phi_pr && phi_pr <  0.16  ) || // E1
//           ( 0.2227 < phi_pr && phi_pr <  0.5627) || // E2
//           ( 0.6154 < phi_pr && phi_pr <  0.9554)    // E3
//         ) return true;
//   }
//
//}


//char* EmcAnaCommon::GetEnvSure(const char* name)
//{
//   char* val = getenv(name);
//   if (! val)
//   {
//      std::cout << "!!! Failed to getenv(" << name << ")" << std::endl;
//      exit(0);
//   }
//   return val;
//}
