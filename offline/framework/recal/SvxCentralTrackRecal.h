// ============================
// FILE: SvxCentralTrackRecal.h
// ============================

#ifndef __SVXCENTRALTRACKRECAL_H_
#define __SVXCENTRALTRACKRECAL_H_

#include <PHObject.h>

class SvxCentralTrackRecalList;

class SvxCentralTrackRecal : public PHObject
{

 public:
  SvxCentralTrackRecal();
  SvxCentralTrackRecal(const SvxCentralTrackRecal&);
  virtual ~SvxCentralTrackRecal() { }
  
  // Standard functions of all inheritors of PHObject classes...
  // """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  void Reset();
  void identify(std::ostream &os=std::cout) const;

  // Methods
  // """""""
  virtual SvxCentralTrackRecal* Clone(const char* = "") const {
    return new SvxCentralTrackRecal(*this);
  }
  void print() const;

  // Set the values in the SvxCentralTrackRecal...
  // """""""""""""""""""""""""""""""""""""""""""""
  void set_ID               (const int id   ) { track_id = id;     }
  void set_CNTID            (const int id   ) { cnt_id = id;       }
  void set_HitPattern       (const int val  ) { hit_pattern = val; }
  void set_Unique           (const int val  ) { unique = val;      }
  void set_nhit             (const int layer, const int val)
  { nhit[layer] = val;           }
  void set_ClosestApproach  (const int xyz, const float val) 
  { closest_approach[xyz] = val; }
  void set_Chisquare        (const float val) { chi2 = val;        }
  void set_ChisquareOld     (const float val) { chi2_old = val;    }
  void set_Chisquare2       (const float val) { chi2_2 = val;      }
  void set_ChisquarePhi     (const float val) { chi2_phi = val;    }
  void set_ChisquareZ       (const float val) { chi2_z = val;      }
  void set_DCA2D            (const float val) { dca2d = val;       }
  void set_DCAZ             (const float val) { dcaz = val;        }
  void set_DCA2DOld         (const float val) { dca2d_old = val;   }
  void set_DCAZOld          (const float val) { dcaz_old = val;    }
  void set_ClusterID        (const int layer, const int id, int ihit=0)
  { cluster_id[layer][ihit] = id;     }
  void set_ClusterLadder    (const int layer, const int id, int ihit=0)
  { cluster_ladder[layer][ihit] = id; }
  void set_ClusterSensor    (const int layer, const int id, int ihit=0)
  { cluster_sensor[layer][ihit] = id; }
  void set_ClusterPosition  (const int layer,
			     const float x, const float y, const float z,
			     int ihit=0)
  {
    cluster_position[layer][ihit][0] = x;
    cluster_position[layer][ihit][1] = y;
    cluster_position[layer][ihit][2] = z;
  }
  void set_ClusterDPhi      (const int layer, const float val, int ihit=0)
  { cluster_dphi[layer][ihit] = val; }
  void set_ClusterDZ        (const int layer, const float val, int ihit=0)
  { cluster_dz[layer][ihit] = val;   }
  void set_ProjectedPosition(const int layer,
			     const float x, const float y, const float z)
  {
    projected_position[layer][0] = x;
    projected_position[layer][1] = y;
    projected_position[layer][2] = z;
  }
  void set_phi0             (const float val) { phi0 = val;      }
  void set_the0             (const float val) { the0 = val;      }
  void set_mom              (const float val) { momentum = val;  }
  void set_charge           (const int   val) { charge = val;    }

  //Include multicircle fit dphi and dz
  void set_multi_dphi       (const int layer, const float val) 
  {multi_dphi[layer] = val;        }
  void set_multi_dz         (const int layer, const float val) 
  {multi_dz[layer] = val;          }
  void set_expected_position(const int layer, const int xyz, float val) 
  {expected_position[layer][xyz] = val;        }


  // Get the values from the SvxCentralTrackRecal...
  // """"""""""""""""""""""""""""""""""""
  int   get_ID               (void)          { return track_id;              }
  int   get_CNTID            (void)          { return cnt_id;                }
  int   get_HitPattern       (void)          { return hit_pattern;           }
  int   get_Unique           (void)          { return unique;                }
  int   get_nhit             (void)          { return nhit[0]+nhit[1]+nhit[2]+nhit[3]; }
  int   get_nhit             (const int layer) { return nhit[layer];         }
  float get_ClosestApproach  (const int xyz) { return closest_approach[xyz]; }
  float get_Chisquare        (void)          { return chi2;                  }
  float get_ChisquareOld     (void)          { return chi2_old;              }
  float get_Chisquare2       (void)          { return chi2_2;                }
  float get_ChisquarePhi     (void)          { return chi2_phi;              }
  float get_ChisquareZ       (void)          { return chi2_z;                }
  float get_DCA2D            (void)          { return dca2d;                 }
  float get_DCAZ             (void)          { return dcaz;                  }
  float get_DCA2DOld         (void)          { return dca2d_old;             }
  float get_DCAZOld          (void)          { return dcaz_old;              }
  float get_ClusterID        (const int layer, int ihit=0)
  { return cluster_id[layer][ihit];                                          }
  float get_ClusterLadder    (const int layer, int ihit=0)
  { return cluster_ladder[layer][ihit];                                      }
  float get_ClusterSensor    (const int layer, int ihit=0)
  { return cluster_sensor[layer][ihit];                                      }
  float get_ClusterPosition  (const int layer, const int xyz, int ihit=0)
  { return cluster_position[layer][ihit][xyz];                               }
  float get_ClusterDPhi      (const int layer, int ihit=0)
  { return cluster_dphi[layer][ihit];                                        }
  float get_ClusterDZ        (const int layer, int ihit=0)
  { return cluster_dz[layer][ihit];                                          }
  float get_ProjectedPosition(const int layer, const int xyz)
  { return projected_position[layer][xyz];                                   }
  float get_phi0             (void)          { return phi0;                  }
  float get_the0             (void)          { return the0;                  }
  float get_mom              (void)          { return momentum;              }
  int   get_charge           (void)          { return charge;                }

  //Include multicircle fit dphi and dz
  float get_multi_dphi     (const int layer) {return multi_dphi[layer];         }
  float get_multi_dz       (const int layer) {return multi_dz[layer];           }
  float get_expected_position            (const int layer, const int xyz) {return expected_position[layer][xyz]; }
  
 protected:
  int track_id;
  int cnt_id;
  int hit_pattern;
  int unique;
  int nhit[4];
  float closest_approach[3];
  float chi2;
  float chi2_old;
  float chi2_2;
  float chi2_phi;
  float chi2_z;
  float dca2d;
  float dcaz;
  float dca2d_old;
  float dcaz_old;
  int cluster_id[4][2];
  float cluster_dphi[4][2];
  float cluster_dz[4][2];
  float cluster_position[4][2][3];
  int cluster_ladder[4][2];
  int cluster_sensor[4][2];
  float projected_position[4][3];
  float phi0;
  float the0;
  float momentum;
  int charge;

  //Include multicircle fit dphi and dz
  float multi_dphi[4];
  float multi_dz[4];

  //get expected xyz positoons
  float expected_position[4][3];

  //---
  ClassDef(SvxCentralTrackRecal,1)

};

#endif
