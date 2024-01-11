#ifndef __TMPCEXSHOWER_H__
#define __TMPCEXSHOWER_H__

/**
 * @class  TMpcExShower
 * @author lajoie@iastate.edu
 * @date   July 2015
 * @brief  This is the shower object 
 */

#include "MpcExConstants.h"
#include <vector>
#include <math.h>
#include <map>

class TMpcExCalibHit;
class PHCompositeNode; 
class fkinWrapper; 
class TMpcExHitContainer; 
class TMpcExHit; 

class TMpcExShower {

 public:

  //! Construct the hit with the key
  TMpcExShower(unsigned int arm);

  //! Copy Constructor
  TMpcExShower( TMpcExShower *shwr ); 

  // Merge Constructor
  TMpcExShower( TMpcExShower *shwr1, TMpcExShower *shwr2, TMpcExHitContainer* _hit_map); 

  //! Destructor
  virtual ~TMpcExShower(); 

  //! return the key(s)
  unsigned int key() const { return _key; }
  unsigned int key1() const { return _key1; }
  unsigned int key2() const { return _key2; }
  
  // ---------------------------------------------------------------------------------------------------------
  // TMpcExShower: 
  // The TMpcExShower object contains:
  // -> A list of minipads (by key) belonging to the shower
  // -> Data members calculated from the minipad content
  // -> Data members associating MPC information (cluster and towers) to the shower
  //
  // Associated Minipads: 
  // --------------------
  // Minipads associated by key are stored in a vector and can be accessed and manipulated by the 
  // following functions:
  //
  // addHit(unsigned int key) - add a hit to the shower list
  // sizeHits() - returns the size of the hits vector (# of associated hits)
  // getHit(const size_t index) - retrieves the key for a specific hit in the shower vector
  // ResetHits() - clears the hits vector
  //
  // So, for example, the hits in the shower can be accessed by:
  //
  //	  unsigned int nhits = shower_v1->sizeHits(); 
  //	  for(unsigned int hitNum=0; hitNum<nhits ; hitNum++) {
  //	    TMpcExHit *hit_ptr = _hit_map->get_hit_by_key(shower_v1->getHit(hitNum)); 
  //      ...
  //
  // The list of shower-associated minipads is filled by the mMpcExShower module at shower 
  // finding.
  //
  // Shower Object Data Members:
  // ----------------------------
  //
  // All the data members have corresponding "set_" and "get_" methods. These values are
  // filled my the mMpcExShower modukle as part of shower finding. 
  //
  // Basic Information:
  // ------------------
  // arm: detector arm (0,1)
  // hsx: shower hough slope in "x" in PHENIX coordinate system (low gain, energy weighted)
  // hsy: shower hough slope in "y" in PHENIX coordinate system
  // hsx_layer, hsy_layer: shower hough slopes by layer
  // seed_hsx: starting seed hough slope in "x" in PHENIX coordinate system
  // seed_hsy: starting seed hough slope in "y" in PHENIX coordinate system
  // n_minipads: number of minipads associated with the shower
  // n_sat_minipads: number of saturated minipads (max energy)
  // merged: flag indicting if shower is the result of a merge (1 = merged)
  // delete_me: flag used internally to delete showers that were merged
  // n_iter: number of iterations before shower converged
  // ExpansionFactor: reconstruction second stage shower expansion factor
  // ew_x, ew_y, ew_z: energy weighted avg. x,y,z of shower
  //
  // Shower Shape Information:
  // -------------------------
  // rms_hsx: RMS of the hough slope in x 
  // rms_hsy: RMS of the hough slope in y
  // rms: sqrt( rms_hsx^2 + rms_hsy^2)
  // disp_hsx: DISPERSION of the hough slope in x 
  // disp_hsy: DISPERSION of the hough slope in y
  // disp: sqrt( disp_hsx^2 + disp_hsy^2)
  //
  // Shower Energy Information:
  // --------------------------
  // esum: shower energy sum, fully corrected (GeV)
  // raw_esum: shower energy sum (uncorrected, GeV)
  // nlayers: number of layers with hit minipads (low and high gain)
  // e_layer: summed energy in shower indexed by layer (in keV)
  // n_layer: number of minipads in shower by layer
  // first_layer: first MPC-EX layer with nonzero energy in low, high gain 
  //
  // roughTotE: calibrated total energy MPC-EX + MPC 
  // AlPlateEnergy: results of spline calibration for energy in Al plate in front of MPC 
  // CalibEInRange: flag (0,1) indicating whether or not the shower was withing the calibration table
  //                validity range (1=true)
  // 
  // Kinematics information:
  // -----------------------
  // Px(), Py(), Pz(): x,y,z momentum (GeV) of shower
  // PT(): transverse momentum of shower
  // eta(): pseudorapidity of shower
  // phi(): azimuthal angle of shower
  //
  // MPC Tower Information:
  // ----------------------
  // mpcN3x3, mpcN5x5: number of MPC towers backing up the MPC-EX shower (in a 3x3 or 5x5 array) with nonzero energy 
  // mpcE3x3, mpcE5x5: MPC tower energy sum in a 3x3, 5x5 backing up the MPC-EX shower
  // mpcECorr: corrected total MPC energy (from 3x3 energy, GeV)
  // mpcCentTwr: stored index in mpcTowerContainer of the central tower backing up the MPC-EX shower
  // mpcCentTwrE: energy in the MPC central tower backing up the MPC-EX shower
  // fiducial: flag (0,1) if the shower is backed by a full set of MPC towers in a 3x3 array (1=true)
  // mpc33hsx, mpc33hsy: hough slope in x,y of the energy weighted 3x3 x,y position in the MPC 3x3 array
  // mpc_quality: chi2/ndof for each tower in a 5x5 array around the MPC-EX shower, -9999.0 = no raw tower found (no energy)
  //
  // MPC Cluster Information:
  // ------------------------
  // ClosestMPCClusterDistance: distance in hough space between the closest MPC cluster to the MPC-EX shower
  // ClosestMPCClusterDistanceX: distance in hough space x between the closest MPC cluster to the MPC-EX shower
  // ClosestMPCClusterDistanceY: distance in hough space y between the closest MPC cluster to the MPC-EX shower
  // ClosestMPCClusterEnergy: energy of the closest associated cluster 
  // ClosestMPCClusterIndex: index of the MPC cluster in mpcClusterContainer
  // ClosestMPCClusterClosestFlag: if multiple showers have the same associated MPC cluster, this flags set to "1"
  //                               indicates that this shower was the closest in hough space
  //
  // ---------------------------------------------------------------------------------------------------------
 

  // shower information

  short get_arm()                const { return _arm;      }
  double get_hsx()               const { return _hsx;      }
  double get_hsy()               const { return _hsy;      }
  double get_seed_hsx()               const { return _seed_hsx;      }
  double get_seed_hsy()               const { return _seed_hsy;      }

  double get_ew_x()               const { return _ew_x;      }
  double get_ew_y()               const { return _ew_y;      }
  double get_ew_z()               const { return _ew_z;      }

  double get_rms_hsx()           const { return _rms_hsx;  }
  double get_rms_hsy()           const { return _rms_hsy;  }
  double get_rms()               const { return sqrt( pow(_rms_hsx,2) + pow(_rms_hsy,2));  }
  
  double get_disp_hsx()           const { return _disp_hsx;  }
  double get_disp_hsy()           const { return _disp_hsy;  }
  double get_disp()               const { return sqrt( pow(_disp_hsx,2) + pow(_disp_hsy,2));  }

  double get_rms_hsx_layer(int l) const { if( (l>=0) && (l<MpcExConstants::NLAYERS)) return _rms_hsx_layer[l]; else return 0.0; } 
  double get_rms_hsy_layer(int l) const { if( (l>=0) && (l<MpcExConstants::NLAYERS)) return _rms_hsy_layer[l]; else return 0.0; } 
  double get_disp_hsx_layer(int l) const { if( (l>=0) && (l<MpcExConstants::NLAYERS)) return _disp_hsx_layer[l]; else return 0.0; } 
  double get_disp_hsy_layer(int l) const { if( (l>=0) && (l<MpcExConstants::NLAYERS)) return _disp_hsy_layer[l]; else return 0.0; } 

  double get_rms_layer(int l) const { if( (l>=0) && (l<MpcExConstants::NLAYERS)) 
                                        return sqrt(pow(_rms_hsx_layer[l],2)+pow(_rms_hsy_layer[l],2)); 
                                      else 
					return 0.0; } 
  double get_disp_layer(int l) const { if( (l>=0) && (l<MpcExConstants::NLAYERS)) 
                                         return sqrt(pow(_disp_hsx_layer[l],2)+pow(_disp_hsy_layer[l],2)); 
                                        else 
					 return 0.0; } 

  double get_esum()             const { return _esum;    }
  double get_raw_esum()             const { return _raw_esum;    }

  int get_nlayers()              const { return _nlayers;  }

  double get_e_layer(int l)         const { if( (l>=0) && (l<MpcExConstants::NLAYERS)) return _e_layer[l]; else return 0.0; }       

  int get_n_layer(int l)         const { if( (l>=0) && (l<MpcExConstants::NLAYERS)) return _n_layer[l]; else return 0.0; }       

  int get_first_layer()          const { return _first_layer; }

  int get_mpcN3x3()              const { return _Nmpc3x3;  }
  int get_mpcN5x5()              const { return _Nmpc5x5;  }

  
  double get_mpcE3x3( bool use_cutoff = true )           const { double eret = 0; 
                                         for(int i=1; i<4; i++) { 
					   for(int j=1; j<4; j++) { 
					     if( (_mpcTwrE[i][j]>=MpcExConstants::GetMPCTowerCutoff(_arm, _mpc_pkix+(i-2), _mpc_pkiy+(j-2))) || !use_cutoff ) eret += _mpcTwrE[i][j];
					   }
					 }
					 return eret; }
  double get_mpcE5x5(bool use_cutoff = true )           const { double eret = 0; 
                                         for(int i=0; i<5; i++) { 
					   for(int j=0; j<5; j++) { 
					     if( (_mpcTwrE[i][j]>=MpcExConstants::GetMPCTowerCutoff(_arm, _mpc_pkix+(i-2), _mpc_pkiy+(j-2))) || !use_cutoff ) eret += _mpcTwrE[i][j];
					   }
					 }
					 return eret; }
  double get_mpcECorr()           const { return _EmpcCorr;  }
  int get_mpcCentTwr()           const { return _mpcCentTwr;}
  double get_mpcCentTwrE()       const { if( _mpcTwrE[2][2]>= MpcExConstants::GetMPCTowerCutoff(_arm, _mpc_pkix, _mpc_pkiy)) return _mpcTwrE[2][2]; else return 0.0; }

  double get_mpcTwrE(int i, int j, bool getAll = false) const 
                                                      { if( (i>=0) && (i<5) && (j>=0) && (j<5) ){
							  if( (_mpcTwrE[i][j]>=MpcExConstants::GetMPCTowerCutoff(_arm, _mpc_pkix+(i-2), _mpc_pkiy+(j-2))) || getAll )
							    return _mpcTwrE[i][j]; 
							  else 
							    return 0.0;
							}
							else
							  return 0.0;
                                                      } 

  double get_mpcTwrTOF(int i, int j) const 
                                                      { if( (i>=0) && (i<5) && (j>=0) && (j<5) )
							  return _mpcTwrTOF[i][j]; 
							else
							  return 0.0;
                                                      } 

  double get_mpcTwrHS(int i, int j, int k) const { if( (i>=0) && (i<5) && (j>=0) && (j<5) && (k>=0) && (k<2) ) 
                                                      return _mpcTwrHS[i][j][k]; else return -9999.0; } 

  double get_mpc33hsx(bool use_cutoff = true) const { double hsx = 0; 
                                         double eret = 0; 
                                         for(int i=1; i<4; i++) { 
					   for(int j=1; j<4; j++) {
					     if((_mpcTwrE[i][j]>=MpcExConstants::GetMPCTowerCutoff(_arm, _mpc_pkix+(i-2), _mpc_pkiy+(j-2))) || !use_cutoff){
					       eret += _mpcTwrE[i][j];
					       hsx += _mpcTwrE[i][j]*_mpcTwrHS[i][j][0];
					     }
					   }
					 }
					 if(eret>0.0)
					   return (hsx/eret);
                                         else
					   return -9999.0; }

  double get_mpc33hsy(bool use_cutoff = true)          const { double hsy = 0; 
                                         double eret = 0; 
                                         for(int i=1; i<4; i++) { 
					   for(int j=1; j<4; j++) {
					     if((_mpcTwrE[i][j]>=MpcExConstants::GetMPCTowerCutoff(_arm, _mpc_pkix+(i-2), _mpc_pkiy+(j-2)))||!use_cutoff){
					       eret += _mpcTwrE[i][j];
					       hsy += _mpcTwrE[i][j]*_mpcTwrHS[i][j][1];
					     }
					   }
					 }
					 if(eret>0.0)
					   return (hsy/eret);
                                         else
					   return -9999.0; }

  double get_mpc33hsx_err(bool use_cutoff = true)      const { double hsx_err = 0.0; 
                                         double etot = get_mpcE3x3(use_cutoff); 
					 double twr_err = (2.2/sqrt(12.0))/(MpcExConstants::MPC_REFERENCE_Z-_vertex); 
                                         for(int i=1; i<4; i++) { 
					   for(int j=1; j<4; j++) {
					     if((_mpcTwrE[i][j]>=MpcExConstants::GetMPCTowerCutoff(_arm, _mpc_pkix+(i-2), _mpc_pkiy+(j-2)))||!use_cutoff){
					       hsx_err += pow((_mpcTwrE[i][j]/etot)*twr_err,2);
					     }
					   }
					 }
					 return sqrt(hsx_err); }

  double get_mpc33hsy_err(bool use_cutoff = true)      const { return get_mpc33hsx_err(); }

  int get_mpcPeakix()            const {return _mpc_pkix; }
  int get_mpcPeakiy()            const {return _mpc_pkiy; }

  double get_roughTotE()         const { return _roughTotalEnergy;}

  double get_AlPlateEnergy()     const { return _AlPlateEnergy; }

  int get_CalibEInRange()        const { return _CalibEInRange;}

  double get_ClosestMPCClusterDistance() const { return _ClosestMPCClusterDistance;}
  double get_ClosestMPCClusterDistanceX() const { return _ClosestMPCClusterDistanceX;} 
  double get_ClosestMPCClusterDistanceY() const { return _ClosestMPCClusterDistanceY;}
  double get_ClosestMPCClusterEnergy() const { return _ClosestMPCClusterEnergy;} 
  int get_ClosestMPCClusterIndex() const { return _ClosestMPCClusterIndex;} 
  int get_ClosestMPCClusterClosestFlag() const { return _ClosestMPCCluster;}

  int get_fiducial()               const {return fiducial;}

  double get_n_minipads() const {return _hits.size();} 

  double get_n_sat_minipads() const {return _n_sat_minipads;} 

  int get_merged()              const { return _merged;  }

  int get_delete_me()              const { return _delete_me;  }

  int get_n_iter()              const { return _n_iter;  }

  double get_Pz()               const {	float theta = atan( sqrt(pow(_hsx,2) + pow(_hsy,2)) );
                                        float pz = _roughTotalEnergy*cos(theta);
					return (_arm>0 ? pz : -pz); 
                                      }
  double get_Px()               const { return get_Pz()*_hsx; } 
  double get_Py()               const { return get_Pz()*_hsy; } 

  double get_PT()               const { return sqrt( pow(get_Px(),2) + pow(get_Py(),2)); } 
  double get_eta()                    { return getEta(get_PT(), get_Pz());}
  double get_phi()                    { return XYtoPhi(get_Px(), get_Py());}

  double get_fit_slope(bool use_cutoff = true)              { if(!_fit_cached) ProjectVertex(true, true, use_cutoff); 
                                        return _fit_slope; 
                                      }
  double get_fit_intercept(bool use_cutoff = true)          { if(!_fit_cached) ProjectVertex(true, true, use_cutoff); 
                                        return _fit_intercept; 
                                      }
  double get_fit_chi2dof(bool use_cutoff = true)            { if(!_fit_cached) ProjectVertex(true, true, use_cutoff); 
                                        return _fit_chi2dof; 
                                      }

  double get_vertex()            const { return _vertex;    }

  double get_hsx_layer(int l)    const { if( (l>=0) && (l<MpcExConstants::NLAYERS)) return _hsx_layer[l]; else return 0.0; }  
  double get_hsy_layer(int l)    const { if( (l>=0) && (l<MpcExConstants::NLAYERS)) return _hsy_layer[l]; else return 0.0; }  

  double get_ExpansionFactor()            const { return _ExpansionFactor;    }

  double get_mpc_quality(int i, int j) const { if( (i>=0) && (i<5) && (j>=0) && (j<5) )
                                               {
                                                 return _mpcQual[i][j];
                                               }
                                               else {
					         return -9999.0;
					       }
                                             }  

  void set_arm(short arm)              { _arm = arm;     }
  void set_hsx(double n)               { _hsx = n;       }
  void set_hsy(double n)               { _hsy = n;       }        
  void set_seed_hsx(double n)               { _seed_hsx = n;       }
  void set_seed_hsy(double n)               { _seed_hsy = n;       }        
  void set_rms_hsx(double n)           { _rms_hsx = n;   }
  void set_rms_hsy(double n)           { _rms_hsy = n;   }        
  void set_disp_hsx(double n)           { _disp_hsx = n;   }
  void set_disp_hsy(double n)           { _disp_hsy = n;   }        
  void set_esum(double n)             { _esum = n;     }
  void set_raw_esum(double n)             { _raw_esum = n;     }

  void set_ew_x(double n)               { _ew_x = n;       }
  void set_ew_y(double n)               { _ew_y = n;       }
  void set_ew_z(double n)               { _ew_z = n;       }

  void set_nlayers(int n)              { _nlayers = n;   } 

  void set_n_sat_minipads(int n)              { _n_sat_minipads = n;  } 

  void set_e_layer(int l, double n)    { if( (l>=0) && (l<MpcExConstants::NLAYERS)) _e_layer[l] = n; }           

  void set_n_layer(int l, int n)    { if( (l>=0) && (l<MpcExConstants::NLAYERS)) _n_layer[l] = n; }           

  void set_rms_hsx_layer(int l, double n)    { if( (l>=0) && (l<MpcExConstants::NLAYERS)) _rms_hsx_layer[l] = n; }           
  void set_rms_hsy_layer(int l, double n)    { if( (l>=0) && (l<MpcExConstants::NLAYERS)) _rms_hsy_layer[l] = n; }           
  void set_disp_hsx_layer(int l, double n)    { if( (l>=0) && (l<MpcExConstants::NLAYERS)) _disp_hsx_layer[l] = n; }           
  void set_disp_hsy_layer(int l, double n)    { if( (l>=0) && (l<MpcExConstants::NLAYERS)) _disp_hsy_layer[l] = n; }           

  void set_first_layer(int n)          {  _first_layer = n; }
  
  void set_mpcN3x3(int n)              { _Nmpc3x3 = n;   } 
  void set_mpcN5x5(int n)              { _Nmpc5x5 = n;   } 
  void set_mpcECorr(double n)           { _EmpcCorr = n;   } 
  void set_mpcCentTwr(int n)           { _mpcCentTwr = n;}

  void set_mpcTwrE(int i, int j, double n) { if( (i>=0) && (i<5) && (j>=0) && (j<5) ) 
                                             { _mpcTwrE[i][j] = n; 
					       _fit_cached = false; 
					     } 
                                           }

  void set_mpcTwrTOF(int i, int j, double n) { if( (i>=0) && (i<5) && (j>=0) && (j<5) ) 
                                             { _mpcTwrTOF[i][j] = n; 
					     } 
                                           }

  void set_mpcTwrHS(int i, int j, double hsx, double hsy ) { if( (i>=0) && (i<5) && (j>=0) && (j<5) ){ 
                                                               _mpcTwrHS[i][j][0] = hsx; 
							       _mpcTwrHS[i][j][1] = hsy;
							       _fit_cached = false; 
                                                             }
                                                           }
  void set_mpcPeakix(int n)              { _mpc_pkix = n;   } 
  void set_mpcPeakiy(int n)              { _mpc_pkiy = n;   } 

  void set_roughTotE(double E_in)         { _roughTotalEnergy = E_in;}

  void set_AlPlateEnergy(double E_in)         { _AlPlateEnergy = E_in;}

  void set_CalibEInRange(int n)           { _CalibEInRange = n;}

  void set_ClosestMPCClusterDistance(double dist) { _ClosestMPCClusterDistance = dist;}
  void set_ClosestMPCClusterDistanceX(double distX) { _ClosestMPCClusterDistanceX = distX;} 
  void set_ClosestMPCClusterDistanceY(double distY) { _ClosestMPCClusterDistanceY = distY;}
  void set_ClosestMPCClusterEnergy(double E) { _ClosestMPCClusterEnergy = E;} 
  void set_ClosestMPCClusterIndex(int n) { _ClosestMPCClusterIndex = n;} 
  void set_ClosestMPCClusterClosestFlag(int n) { _ClosestMPCCluster = n;}

  // single-track reco values

  void set_fiducial(int fiducial_in)      {fiducial = fiducial_in;}

  void set_merged(int merged_in)      {_merged = merged_in;}

  void set_delete_me(int del_in)      {_delete_me = del_in;}

  void set_n_iter(int n_in)      {_n_iter = n_in;}

  void set_fit_data(int n_fit, float *r_fit, float *z_fit, float *r_fit_err, float *z_fit_err )        
                                 { if(n_fit<MpcExConstants::NLAYERS)
				     _n_fit = n_fit; 
				   else
				     _n_fit = MpcExConstants::NLAYERS; 
				   for(int i=0; i<_n_fit; i++){
				     _r_fit[i] = r_fit[i]; 
				     _z_fit[i] = z_fit[i]; 
				     _r_fit_err[i] = r_fit_err[i]; 
				     _z_fit_err[i] = z_fit_err[i]; 
				   }
				   _fit_cached = false; 
				 }

  void set_vertex(double v_in)         { _vertex = v_in;}

  void set_hsx_layer(int l, double n)    { if( (l>=0) && (l<MpcExConstants::NLAYERS)) _hsx_layer[l] = n; }           
  void set_hsy_layer(int l, double n)    { if( (l>=0) && (l<MpcExConstants::NLAYERS)) _hsy_layer[l] = n; }           

  void set_ExpansionFactor(double _in)         { _ExpansionFactor = _in;}

  void set_mpc_quality(int i, int j, double n) { if( (i>=0) && (i<5) && (j>=0) && (j<5) ) _mpcQual[i][j] = n; } 
                                           

  // associated hits
  void addHit(unsigned int key);

  //! the number of hits in the container
  size_t sizeHits() const {
    if(localStoreFilled)
      return localHits.size(); 
    else
      return _hits.size();
  }

  //! Get the hit key at a given index 0-size() for random access
  unsigned int getHit(const size_t index); 

  //! general purpose getHit to allow access to local store (if configured)
  TMpcExHit *getHit(unsigned int ihit, TMpcExHitContainer* _hit_map);

  //! delete all of the hits in the container and clear it so there is nothing in the container
  // just clear the list for keys, but delete locally held pointers
  void ResetHits(); 

  // Fit the MPC-EX/MPC hits (r vs. z)
  void ProjectVertex( bool include_mpc = true, bool include_vertex = true, bool use_cutoff = true ); 

  // Build and access the contributors list (for MC)

  void BuildContributorListMC(PHCompositeNode *topNode); 

  // get the full contributor list
  std::pair<std::map<int,float>::iterator,std::map<int,float>::iterator>  get_contributors(){
    return std::make_pair(contributors.begin(),contributors.end()); 
  }

  // request a contributor by dominance
  // 1: most dominant by energy fraction, etc.
  std::map<int,float>::iterator get_contributor_by_dominance(int dom, bool &valid){
    if(dom>0){
      std::map<int,int>::iterator it = ordered_index.find(dom);
      if(it!=ordered_index.end()){
	valid = true; 
	return contributors.find(it->second); 
      }
      else{
	valid = false; 
	return contributors.end(); 
      }
    }
    else{
      valid = false; 
      return contributors.end(); 
    }
  }

  int get_num_contributors(){return contributors.size();}

  float GetTrueTrackFraction(PHCompositeNode *topNode, int true_track);

  void LocallyStoreHits(TMpcExHitContainer* _hit_map); 

  TMpcExShower *getSourceShower(int shnum){
    if(shnum==0) return _shower1; 
    if(shnum==1) return _shower2; 
    return NULL; 
  }

 private:

  std::map<int,float> contributors; 
  std::map<int,int> ordered_index; 

  int GetIndexFromTrueTrack(fkinWrapper *_fkin, int true_track);

  void CalculateShowerMPCEXProperties(TMpcExHitContainer* _hit_map); 

  double getEta(double pt, double pz); 
  double XYtoPhi(double x, double y); 

  //! the key 
  unsigned int _key;

  //! source keys for combined showers
  unsigned int _key1; 
  unsigned int _key2; 
  TMpcExShower *_shower1; 
  TMpcExShower *_shower2; 

  short _arm;
  double _hsx; 
  double _hsy;
  double _seed_hsx; 
  double _seed_hsy;
  double _rms_hsx; 
  double _rms_hsy; 
  double _disp_hsx; 
  double _disp_hsy; 
  double _esum; 
  double _raw_esum; 
  
  double _ew_x; 
  double _ew_y; 
  double _ew_z; 

  int _nlayers; 

  double _e_layer[MpcExConstants::NLAYERS];
  int _n_layer[MpcExConstants::NLAYERS];

  double _rms_hsx_layer[MpcExConstants::NLAYERS];
  double _rms_hsy_layer[MpcExConstants::NLAYERS];
  double _disp_hsx_layer[MpcExConstants::NLAYERS];
  double _disp_hsy_layer[MpcExConstants::NLAYERS];

  double _hsx_layer[MpcExConstants::NLAYERS];
  double _hsy_layer[MpcExConstants::NLAYERS];

  int _first_layer; 

  int _mpcCentTwr; 
  int _Nmpc3x3; 
  int _Nmpc5x5; 
  double _EmpcCorr; 
  double _mpcTwrE[5][5]; 
  double _mpcTwrTOF[5][5]; 
  double _mpcTwrHS[5][5][2]; 
  double _mpcQual[5][5]; 
  int _mpc_pkix; 
  int _mpc_pkiy; 

  double _roughTotalEnergy; 
  int _CalibEInRange; 
  double _AlPlateEnergy; 

  double _ClosestMPCClusterDistance; 
  double _ClosestMPCClusterDistanceX; 
  double _ClosestMPCClusterDistanceY; 
  double _ClosestMPCClusterEnergy; 
  int _ClosestMPCClusterIndex; 
  int _ClosestMPCCluster; 

  int fiducial; 
  int _merged; 
  int _delete_me; 
  
  int _n_iter; 
  double _ExpansionFactor;  

  int _n_sat_minipads; 

  // internal fit data

  float _r_fit[MpcExConstants::NLAYERS+2]; 
  float _r_fit_err[MpcExConstants::NLAYERS+2]; 
  float _z_fit[MpcExConstants::NLAYERS+2]; 
  float _z_fit_err[MpcExConstants::NLAYERS+2];
  int _n_fit;

  bool _fit_cached; 
  double _fit_intercept; 
  double _fit_slope;
  double _fit_chi2dof; 

  double _vertex; 

  typedef std::vector<unsigned int> container;

  //! the internal container of shower-associated hits keys
  container _hits;

  //! local hits storage (for mixing)
  bool localStoreFilled; 
  std::vector<TMpcExHit *> localHits; 

};

#endif /* __TMPCEXSHOWER_H__ */
