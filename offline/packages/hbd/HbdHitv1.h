#ifndef __HBDHITV1_H_
#define __HBDHITV1_H_

#include "PHObject.h"
#include "HbdHit.h"

class HbdHitv1 : public HbdHit
{

 public:

  HbdHitv1();
  HbdHitv1(HbdHitv1 *hit);  
  virtual ~HbdHitv1() {}

  // The "standard PHObject response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;

  // Set the data members
  void set_track      (const int val)   {track      = val; return;}
  void set_idPart     (const int val)   {idPart     = val; return;}
  void set_idParent     (const int val)   {idParent     = val; return;}
  void set_itOrigin     (const int val)   {itOrigin     = val; return;}
  void set_idOrigin     (const int val)   {idOrigin     = val; return;}
  void set_Ptheta     (const float val)   {Ptheta     = val; return;}
  void set_Peta     (const float val)   {Peta     = val; return;}
  void set_Pphi     (const float val)   {Pphi     = val; return;}
  void set_Rvertex     (const float val)   {Rvertex     = val; return;}
  void set_Zvertex     (const float val)   {Zvertex     = val; return;}
  void set_Thetavertex     (const float val)   {Thetavertex     = val; return;}
  void set_Phivertex     (const float val)   {Phivertex     = val; return;}
  void set_sector     (const int val)   {sector     = val; return;}
  void set_npe      (const int val)   {npe      = val; return;}
  void set_nentr      (const int val)   {nentr      = val; return;}
  void set_detflag    (const int val)   {detflag    = val; return;}
  void set_xyzin  (const int ind, const float val)
    {xyzin[ind]  = val; return;}
  void set_pxyz   (const int ind, const float val)
    {pxyz[ind]   = val; return;}
  void set_ptot       (const float val) {ptot       = val; return;}
  void set_theta       (const float val) {theta       = val; return;}
  void set_eta       (const float val) {eta       = val; return;}
  void set_phi       (const float val) {phi       = val; return;}
  void set_xyzloc (const int ind, const float val)
    {xyzloc[ind] = val; return;}
  void set_xyze (const int ind, const float val)
    {xyze[ind] = val; return;}
  void set_pt       (const float val) {pt       = val; return;}
  void set_tof        (const float val) {tof        = val; return;}
  void set_dy       (const float val) {dy       = val; return;}
  void set_dz       (const float val) {dz       = val; return;}
  void set_rmsy       (const float val) {rmsy       = val; return;}
  void set_rmsz       (const float val) {rmsz       = val; return;}
  void set_charge       (const float val) {charge       = val; return;}
  void set_time       (const float val) {time       = val; return;}
  void set_size      (const int val)   {size      = val; return;}
  void set_nct      (const int val)   {nct      = val; return;}
  void set_part       (const float val) {part       = val; return;}
  void set_id      (const int val)   {id      = val; return;}
  void set_pxyzini   (const int ind, const float val)
    {pxyzini[ind]   = val; return;}

  // Get the data members
  int get_track        () const { return track;     }
  int get_idPart       () const { return idPart;    }
  int get_idParent       () const { return idParent;    }
  int get_itOrigin       () const { return itOrigin;    }
  int get_idOrigin       () const { return idOrigin;    }
  float get_Ptheta       () const { return Ptheta;    }
  float get_Peta       () const { return Peta;    }
  float get_Pphi       () const { return Pphi;    }
  float get_Rvertex       () const { return Rvertex;    }
  float get_Zvertex       () const { return Zvertex;    }
  float get_Thetavertex       () const { return Thetavertex;    }
  float get_Phivertex       () const { return Phivertex;    }
  int get_sector       () const { return sector;    }
  int get_npe       () const { return npe;      }
  int get_nentr       () const { return nentr;      }
  int get_detflag      () const { return detflag;   }
  float get_xyzin  (const unsigned int ind) const { return xyzin[ind];  }
  float get_pxyz   (const unsigned int ind) const { return pxyz[ind];   }
  float get_ptot       () const { return ptot;      }
  float get_theta       () const { return theta;      }
  float get_eta       () const { return eta;      }
  float get_phi       () const { return phi;      }
  float get_xyzloc  (const unsigned int ind) const { return xyzloc[ind];  }
  float get_xyze  (const unsigned int ind) const { return xyze[ind];  }
  float get_pt       () const { return pt;      }
  float get_tof        () const { return tof;       }
  float get_dy       () const { return dy;      }
  float get_dz       () const { return dz;      }
  float get_rmsy       () const { return rmsy;      }
  float get_rmsz       () const { return rmsz;      } 
  float get_charge       () const { return charge;      }
  float get_time       () const { return time;      }
  int get_size        () const { return size;     }
  int get_nct        () const { return nct;     }
  float get_part       () const { return part;      }
  int get_id        () const { return id;     }
  float get_pxyzini   (const unsigned int ind) const { return pxyzini[ind];   }

  // Methods
  void print();

 protected:

  // Data member definition
  int track;         // Track number
  int idPart;        // GEANT Particle ID
  int idParent;        // GEANT Particle ID
  int itOrigin;        // GEANT Particle ID
  int idOrigin;        // GEANT Particle ID
  float Ptheta;
  float Peta;
  float Pphi;
  float Rvertex;
  float Zvertex;
  float Thetavertex;
  float Phivertex;
  int sector;        // sector number
  int npe;           // Azimuthal angle
  int nentr;             // Numebr of entries
  int detflag;       // detector flag (HBD=3)
  float xyzin[3];    // Entry coordinates
  float pxyz[3];     // Momentum vector
  float ptot;          // Total momentum
  float theta;          // Polar angle
  float eta;             // Pseudorapidity
  float phi;             //  Transverse momentum
  float xyzloc[3];    // Local coordinates
  float xyze[3];   // Unfolded coordinates
  float pt;             //  Transverse momentum
  float tof;         // Time-of-flight
  float dy;             //  dy
  float dz;             //  dz
  float rmsy;             //  rmsy
  float rmsz;             //  rmsz
  float charge;             //  charge
  float time;             //  time
  int size;         // size
  int nct;         // nct
  float part;             //  part
  int id;         // id
  float pxyzini[3];     // Initial momentum vector


  ClassDef(HbdHitv1,1)

};

#endif

