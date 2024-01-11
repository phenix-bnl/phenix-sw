#ifndef __HBDGHITV1_H_
#define __HBDGHITV1_H_

#include "PHObject.h"
#include "HbdGhit.h"

class HbdGhitv1 : public HbdGhit
{

 public:

  HbdGhitv1();
  HbdGhitv1(HbdGhitv1 *ghit);  
  virtual ~HbdGhitv1() {}

  // The "standard PHObject response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;

  // Set the data members
  void set_mctrack    (const int val)   {mctrack    = val; return;}
  void set_tof        (const float val) {tof        = val; return;}
  void set_idPart     (const short val)   {idPart     = val; return;}
  void set_itParent     (const short val)   {itParent     = val; return;}
  void set_idParent     (const short val)   {idParent     = val; return;}
  void set_itOrigin     (const short val)   {itOrigin     = val; return;}
  void set_idOrigin     (const short val)   {idOrigin     = val; return;}
  void set_Ptheta     (const float val)   {Ptheta     = val; return;}
  void set_Peta     (const float val)   {Peta     = val; return;}
  void set_Pphi     (const float val)   {Pphi     = val; return;}
  void set_Rvertex     (const float val)   {Rvertex     = val; return;}
  void set_Zvertex     (const float val)   {Zvertex     = val; return;}
  void set_Thetavertex     (const float val)   {Thetavertex     = val; return;}
  void set_Phivertex     (const float val)   {Phivertex     = val; return;}
  void set_track      (const short val)   {track      = val; return;}
  void set_dele       (const float val) {dele       = val; return;}
  void set_pathLength (const float val) {pathLength = val; return;}
  void set_detector   (const int val)   {detector   = val; return;}
  void set_sector     (const int val)   {sector     = val; return;}
  void set_side  (const int val)   {side     = val; return;}
  void set_detflag    (const int val)   {detflag    = val; return;}
  void set_isubevent  (const int val)   {isubevent  = val; return;}
  void set_nfile      (const int val)   {nfile      = val; return;}
  void set_xyzin  (const int ind, const float val)
    {xyzin[ind]  = val; return;}
  void set_xyzout (const int ind, const float val)
    {xyzout[ind] = val; return;}
  void set_pxyz   (const int ind, const float val)
    {pxyz[ind]   = val; return;}
  void set_ptot       (const float val) {ptot       = val; return;}
  void set_pt       (const float val) {pt       = val; return;}
  void set_theta       (const float val) {theta       = val; return;}
  void set_eta       (const float val) {eta       = val; return;}
  void set_phi       (const float val) {phi       = val; return;}
  void set_npe      (const int val)   {npe      = val; return;}
  void set_nentr      (const int val)   {nentr      = val; return;}
  void set_xyzloc (const int ind, const float val)
    {xyzloc[ind] = val; return;}
  void set_xyze (const int ind, const float val)
    {xyze[ind] = val; return;}
  void set_pxyzini   (const int ind, const float val)
    {pxyzini[ind]   = val; return;}

  // Get the data members
  int get_mctrack      () const { return mctrack;   }
  float get_tof        () const { return tof;       }
  short get_idPart       () const { return idPart;    }
  short get_itParent       () const { return itParent;    }
  short get_idParent       () const { return idParent;    }
  short get_itOrigin       () const { return itOrigin;    }
  short get_idOrigin       () const { return idOrigin;    }
  float get_Ptheta       () const { return Ptheta;    }
  float get_Peta       () const { return Peta;    }
  float get_Pphi       () const { return Pphi;    }
  float get_Rvertex       () const { return Rvertex;    }
  float get_Zvertex       () const { return Zvertex;    }
  float get_Thetavertex       () const { return Thetavertex;    }
  float get_Phivertex       () const { return Phivertex;    }
  short get_track        () const { return track;     }
  float get_dele       () const { return dele;      }
  float get_pathLength () const { return pathLength;}
  int get_detector     () const { return detector;  }
  int get_sector       () const { return sector;    }
  int get_side       () const { return side; }
  int get_detflag      () const { return detflag;   }
  int get_isubevent    () const { return isubevent; }
  int get_nfile        () const { return nfile;     }
  float get_xyzin  (const unsigned int ind) const { return xyzin[ind];  }
  float get_xyzout (const unsigned int ind) const { return xyzout[ind]; }
  float get_pxyz   (const unsigned int ind) const { return pxyz[ind];   }
  float get_ptot       () const { return ptot;      }
  float get_pt       () const { return pt;      }
  float get_theta       () const { return theta;      }
  float get_eta       () const { return eta;      }
  float get_phi       () const { return phi;      }
  int get_npe       () const { return npe;      }
  int get_nentr       () const { return nentr;      }
  float get_xyzloc  (const unsigned int ind) const { return xyzloc[ind];  }
  float get_xyze  (const unsigned int ind) const { return xyze[ind];  }
  float get_pxyzini   (const unsigned int ind) const { return pxyzini[ind];   }

  // Methods
  void print();

 protected:

  // Data member definition

  int mctrack;       // Monte Carlo track id
  float tof;         // Time-of-flight
  short idPart;        // GEANT Particle ID
  short itParent;        // GEANT Particle ID
  short idParent;        // GEANT Particle ID
  short itOrigin;        // GEANT Particle ID
  short idOrigin;        // GEANT Particle ID
  float Ptheta;
  float Peta;
  float Pphi;
  float Rvertex;
  float Zvertex;
  float Thetavertex;
  float Phivertex;
  short track;         // Track number
  float dele;        // Energy loss
  float pathLength;  // path length (cm)
  int detector;      // 1 = TPC, 2 = HBD
  int sector;        // sector number
  int side;        // side
  int detflag;       // detector flag (0=South, 1=North)
  int isubevent;     // PISA subevent number
  int nfile;         // PISA file number
  float xyzin[3];    // Entry coordinates
  float xyzout[3];   // Exit coordinates
  float pxyz[3];     // Momentum vector
  float ptot;          // Total momentum
  float pt;             //  Transverse momentum
  float theta;          // Polar angle
  float eta;             // Pseudorapidity
  float phi;             //  Transverse momentum
  int npe;           // Azimuthal angle
  int nentr;             // Numebr of entries
  float xyzloc[3];    // Local coordinates
  float xyze[3];   // Unfolded coordinates
  float pxyzini[3];     // Initial momentum vector


  ClassDef(HbdGhitv1,1)

};

#endif

