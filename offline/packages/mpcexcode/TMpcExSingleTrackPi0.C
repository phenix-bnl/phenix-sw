
//MPC Containers
#include <MpcMap.h> 
#include <mpcTowerContainer.h>
#include <mpcTowerContent.h>
#include <mpcTowerContentV1.h>
#include <mpcRawContainer.h>
#include <mpcRawContent.h>

#include "MpcExConstants.h"
#include "TMpcExSingleTrackPi0.h"


void TMpcExSingleTrackPi0::RecalculateShowerEnergy(mpcTowerContainer *_mpc_tower_container, mpcRawContainer *_mpcraw2_container, bool mixed)
{

  for(int i=0; i<2; i++){
  
    int iMPCTwr_peak; 
    double MPCE[5][5]; 
    double MPCTOF[5][5]; 
    double MPCHS[5][5][2]; 
    double MPCQUAL[5][5]; 
    int N_3x3;
    int N_5x5; 
    int fiducial; 
    int pkix; 
    int pkiy; 

    int arm = get_arm();
    int firstLayer = get_first_layer();

    float dz = MpcExConstants::layerZ[arm][firstLayer] - get_vertex();
    double shx = _peakPositions[0][i]->position/dz; 
    double shy = _peakPositions[1][i]->position/dz;
 
    if(!mixed)

      getMPCTwrInfo(_mpc_tower_container, _mpcraw2_container,
		    arm, shx, shy, iMPCTwr_peak, MPCE, 
		    MPCTOF, MPCHS, MPCQUAL,
		    N_3x3, N_5x5, fiducial, pkix, pkiy ); 

    else

      getMPCTwrInfoMixed(arm, shx, shy, iMPCTwr_peak, MPCE, 
			 MPCTOF, MPCHS, MPCQUAL,
			 N_3x3, N_5x5, fiducial, pkix, pkiy ); 


    // Store the relevant information 

    _STmpcCentTwr[i] = iMPCTwr_peak; 
    _STNmpc3x3[i] = N_3x3; 
    _STNmpc5x5[i] = N_5x5; 
    _STmpc_pkix[i] = pkix; 
    _STmpc_pkiy[i] = pkiy; 

    for(int j=0; j<5; j++){
      for(int k=0; k<5; k++){
	_STmpcTwrE[i][j][k] = MPCE[j][k];
	_STmpcTwrTOF[i][j][k] = MPCTOF[j][k]; 
	_STmpcTwrHS[i][j][k][0] = MPCHS[j][k][0]; 
	_STmpcTwrHS[i][j][k][1] = MPCHS[j][k][1]; 
        _STmpcQual[i][j][k] = MPCQUAL[j][k]; 
      }
    }

  }

  // Get the total MPC energy (don't double-count)
  // This sums exclusive energy in a 3x3 behind each shower
   
  CombinedMPCEnergy = 0.0; 

  for(int i=-1; i<2; i++){
    for(int j=-1; j<2; j++){
      if( _STmpcTwrE[0][2+i][2+j]>=MpcExConstants::GetMPCTowerCutoff(get_arm(), _STmpc_pkix[0]+i, _STmpc_pkiy[0]+j)) CombinedMPCEnergy += _STmpcTwrE[0][2+i][2+j];
    }
  }

  for(int i=-1; i<2; i++){
    for(int j=-1; j<2; j++){

      if( _STmpcTwrE[1][2+i][2+j]<MpcExConstants::GetMPCTowerCutoff(get_arm(), _STmpc_pkix[1]+i, _STmpc_pkiy[1]+j)) continue; 

      // Make sure this tower didn't appear in the first sum 

      int t_ix = _STmpc_pkix[1]+i; 
      int t_iy = _STmpc_pkiy[1]+j;

      if( (t_ix<0) || (t_iy<0) || (t_ix>17) || (t_iy>17) ) continue; 

      bool overlap = false; 
      for(int ii=-1; ii<2; ii++){
	for(int jj=-1; jj<2; jj++){

	  if( (t_ix==(_STmpc_pkix[0]+ii)) &&
	      (t_iy==(_STmpc_pkiy[0]+jj)) )
	    overlap = true; 

	}
      }

      if(!overlap) CombinedMPCEnergy += _STmpcTwrE[1][2+i][2+j];

    }
  }

}


void TMpcExSingleTrackPi0::getMPCTwrInfo(mpcTowerContainer *_mpc_tower_container, mpcRawContainer *_mpcraw2_container, 
					 int arm, double shx, double shy, int &iMPCTwr_peak, double MPCE[5][5], 
					 double MPCTOF[5][5], double MPCHS[5][5][2], double MPCQUAL[5][5],
					 int &N_3x3, int &N_5x5, int &fiducial, int &pkix, int &pkiy )
{

  MpcMap *_mpc_map = MpcMap::instance();

  // Initialize returned shower properties

  pkix = -1; 
  pkiy = -1;
  N_3x3 = 0; 
  N_5x5 = 0; 

  for(int i=0; i<5; i++){
    for(int j=0; j<5; j++){
      MPCE[i][j] = 0.0; 
      MPCTOF[i][j] = -9999.0; 
      MPCHS[i][j][0] = -9999.0; 
      MPCHS[i][j][1] = -9999.0;
      MPCQUAL[i][j] = -9999.0; 
    } 
  }

  fiducial = 0;

  // Sum the MPC calibrated tower energy in a 3x3 and 5x5 around the 
  // MPC-EX track direction.
  
  double peak_dist = 9999.0;
  iMPCTwr_peak = -1; 
  int gridx_peak = -1;
  int gridy_peak = -1; 

  // Loop over all valid towers and look for the closest match to 
  // the MPC-EX projection 

  for(int ix=0; ix<18; ix++){
    for(int iy=0; iy<18; iy++){

      int tow_ch = _mpc_map->getFeeCh(ix,iy,arm);
      if(tow_ch>=0){

	float xloc = _mpc_map->getX(tow_ch); 
	float yloc = _mpc_map->getY(tow_ch); 
	float zloc = 0.0; 

        // don't match if not the same arm!!
        if(_mpc_map->getArm(tow_ch)!= arm) continue; 

	if(_mpc_map->getArm(tow_ch)==0)
	  zloc = -MpcExConstants::MPC_REFERENCE_Z; 
	else
	  zloc = MpcExConstants::MPC_REFERENCE_Z; 

	float mpc_hx = xloc/(zloc-get_vertex()); 
	float mpc_hy = yloc/(zloc-get_vertex()); 

	float dist = sqrt( (shx-mpc_hx)*(shx-mpc_hx) + (shy-mpc_hy)*(shy-mpc_hy) );  

	// Check if the tower is closest:

	if(dist<peak_dist) {
	  peak_dist = dist; 
	  gridx_peak = ix; 
	  gridy_peak = iy;  
	}

      }
    }
  }

  // Now check the region around the projected tower for the 
  // highest energy tower. Because showers enter the MPC at an angle, it
  // is possible that if you project to an edge of one tower, the peak energy could
  // be one tower over (based on angle of entry).  The check is a little different for
  // each quadrant. 

  if((gridx_peak>=0) && (gridy_peak>=0)) {

    int new_gridx_peak = gridx_peak; 
    int new_gridy_peak = gridy_peak; 
    float big_E = 0.0; 
    int big_E_idx = -1; 
    for(int ix=-1; ix<2; ix++){
      for(int iy=-1; iy<2; iy++){

	// only towers that exist
	int tow_ch = _mpc_map->getFeeCh(gridx_peak + ix,gridy_peak + iy,arm);
	if(tow_ch<0) continue; 

	// Match this tow_ch with the tower container to get the energy

	float this_E = 0.0;
	float this_E_idx = -1; 
	int fNMpcTowers = _mpc_tower_container->size();
	for(int iMPCTwr=0 ; iMPCTwr<fNMpcTowers ; iMPCTwr++) {
	  mpcTowerContent *twr = _mpc_tower_container->getTower(iMPCTwr);
          // don't match if not the same arm!!
          if(_mpc_map->getArm(twr->get_ch())!= arm) continue; 
	  if(tow_ch!=twr->get_ch()) continue; 
	  this_E = twr->get_energy();
	  this_E_idx = iMPCTwr; 
	  break; 
	}
 
	// searches by quadrant

	if((gridx_peak<9)&&(gridy_peak<9)){
	  if( (ix>0)||(iy>0) ) continue; 
	}
	else if((gridx_peak>=9)&&(gridy_peak<9)){
	  if( (ix<0)||(iy>0) ) continue; 
	}
	else if((gridx_peak<9)&&(gridy_peak>=9)){
	  if( (ix>0)||(iy<0) ) continue; 
	}
	else if((gridx_peak>=9)&&(gridy_peak>=9)){
	  if( (ix<0)||(iy<0) ) continue; 
	}

	if( (this_E>0.0) && (this_E>big_E) ){
	  new_gridx_peak = gridx_peak + ix; 
	  new_gridy_peak = gridy_peak + iy; 
	  big_E = this_E;
	  big_E_idx = this_E_idx; 
	}
      }
    }

    iMPCTwr_peak = big_E_idx; 

    if(iMPCTwr_peak>=0){

      pkix = new_gridx_peak; 
      pkiy = new_gridy_peak; 
      gridx_peak = new_gridx_peak; 
      gridy_peak = new_gridy_peak; 

      // Calculate energy sums

      int fNMpcTowers = _mpc_tower_container->size();
      for(int iMPCTwr=0 ; iMPCTwr<fNMpcTowers ; iMPCTwr++) {

	mpcTowerContent *twr = _mpc_tower_container->getTower(iMPCTwr);
  
	double mpc_energy = twr->get_energy();
	int tow_ch = twr->get_ch(); 
	int gridx = _mpc_map->getGridX(tow_ch); 
	int gridy = _mpc_map->getGridY(tow_ch); 

	if( (abs(gridx-gridx_peak)<=2) && (abs(gridy-gridy_peak)<=2) && (arm==_mpc_map->getArm(tow_ch)) ) {

	  // if(pFlag){
	  //  cout << gridx-gridx_peak+2 << " " << gridy-gridy_peak+2 << " " << mpc_energy << endl; 
	  // }

	  if( (std::isnan(mpc_energy)==0) && (mpc_energy>0.0) ){
	    MPCE[gridx-gridx_peak+2][gridy-gridy_peak+2] = mpc_energy;
	    // NOT _t0 subtracted!
	    MPCTOF[gridx-gridx_peak+2][gridy-gridy_peak+2] = twr->get_tof();

	    // Match this up with the raw tower and pull out the quality flag 

	    float fqual = -9999.0; 
	    if(_mpcraw2_container){
	      for (unsigned int iraw=0; iraw<_mpcraw2_container->size(); iraw++){
		mpcRawContent *raw = (mpcRawContent *)_mpcraw2_container->getTower(iraw);
		if(raw->get_ch()==twr->get_ch()){
		  fqual = raw->get_fquality();
		  break; 
		}
	      }
	    }
	    MPCQUAL[gridx-gridx_peak+2][gridy-gridy_peak+2] = fqual;

	  } 
	  else{
	    MPCE[gridx-gridx_peak+2][gridy-gridy_peak+2] = 0.0;
	    MPCTOF[gridx-gridx_peak+2][gridy-gridy_peak+2] = 0.0;
	    MPCQUAL[gridx-gridx_peak+2][gridy-gridy_peak+2] = -9999.0;
	  }

	}

      }	

      // Need to loop differently to get the number of valid crystals 
      // as on the DST the tower content only contains towers with nonzero energy

      for(int ix=-2; ix<=2; ix++){
	for(int iy=-2; iy<=2; iy++){
	  int tow_ch = _mpc_map->getFeeCh(gridx_peak+ix,gridy_peak+iy,arm);
	  if(tow_ch>=0){
	    N_5x5++;
	    if((ix>=-1) && (ix<=1) && (iy>=-1) && (iy<=1)) N_3x3++;

	    float xloc = _mpc_map->getX(tow_ch); 
	    float yloc = _mpc_map->getY(tow_ch); 
	    float zloc = 0.0; 

	    if(_mpc_map->getArm(tow_ch)==0)
	      zloc = -MpcExConstants::MPC_REFERENCE_Z; 
	    else
	      zloc = MpcExConstants::MPC_REFERENCE_Z; 

	    MPCHS[ix+2][iy+2][0] = xloc/(zloc-get_vertex());  
	    MPCHS[ix+2][iy+2][1] = yloc/(zloc-get_vertex());  

	  }
	}
      }

      if(N_3x3==9) fiducial = 1; 

    }
    else{
      //cout << PHWHERE << " ERROR - no matched MPC tower?" << endl; 
      pkix = new_gridx_peak; 
      pkiy = new_gridy_peak; 
    }

  }

}

void TMpcExSingleTrackPi0::getMPCTwrInfoMixed(int arm, double shx, double shy, int &iMPCTwr_peak, double MPCE[5][5], 
					      double MPCTOF[5][5], double MPCHS[5][5][2], double MPCQUAL[5][5],
					      int &N_3x3, int &N_5x5, int &fiducial, int &pkix, int &pkiy )
{

  MpcMap *_mpc_map = MpcMap::instance();

  // Initialize returned shower properties

  pkix = -1; 
  pkiy = -1;
  N_3x3 = 0; 
  N_5x5 = 0; 

  for(int i=0; i<5; i++){
    for(int j=0; j<5; j++){
      MPCE[i][j] = 0.0; 
      MPCTOF[i][j] = -9999.0; 
      MPCHS[i][j][0] = -9999.0; 
      MPCHS[i][j][1] = -9999.0;
      MPCQUAL[i][j] = -9999.0; 
    } 
  }

  fiducial = 0;

  // Sum the MPC calibrated tower energy in a 3x3 and 5x5 around the 
  // MPC-EX track direction.
  
  double peak_dist = 9999.0;
  iMPCTwr_peak = -1; // does not correpond to a tower index in the event
  int gridx_peak = -1;
  int gridy_peak = -1; 

  // Loop over all valid towers and look for the closest match to 
  // the MPC-EX projection 

  for(int ix=0; ix<18; ix++){
    for(int iy=0; iy<18; iy++){

      int tow_ch = _mpc_map->getFeeCh(ix,iy,arm);
      if(tow_ch>=0){

	float xloc = _mpc_map->getX(tow_ch); 
	float yloc = _mpc_map->getY(tow_ch); 
	float zloc = 0.0; 

        // don't match if not the same arm!!
        if(_mpc_map->getArm(tow_ch)!= arm) continue; 

	if(_mpc_map->getArm(tow_ch)==0)
	  zloc = -MpcExConstants::MPC_REFERENCE_Z; 
	else
	  zloc = MpcExConstants::MPC_REFERENCE_Z; 

	float mpc_hx = xloc/(zloc-get_vertex()); 
	float mpc_hy = yloc/(zloc-get_vertex()); 

	float dist = sqrt( (shx-mpc_hx)*(shx-mpc_hx) + (shy-mpc_hy)*(shy-mpc_hy) );  

	// Check if the tower is closest:

	if(dist<peak_dist) {
	  peak_dist = dist; 
	  gridx_peak = ix; 
	  gridy_peak = iy;  
	}

      }
    }
  }

  // Now check the region around the projected tower for the energy stored
  // in the shower objects.  This can potentially double-count, but that's 
  // OK - it gets accounted for in RecalculateShowerEnergy. 
  
  pkix = gridx_peak; 
  pkiy = gridy_peak; 

  if((gridx_peak>=0) && (gridy_peak>=0)) {

    for(int i=0; i<2; i++){

      TMpcExShower *shwr = getSourceShower(0); 
      if(i==1) shwr = getSourceShower(1); 
      if(!shwr) continue; 

      int shix = shwr->get_mpcPeakix(); 
      int shiy = shwr->get_mpcPeakiy(); 

      for(int j=0; j<5; j++){
	for(int k=0; k<5; k++){

	  int this_ix = shix + (j-2); 
	  int this_iy = shiy + (k-2); 
	  
	  int d_ix = this_ix - gridx_peak; 
	  int d_iy = this_iy - gridy_peak; 

	  if( (abs(d_ix)<=2) && (abs(d_iy)<=2) ){

	    int idx = d_ix + 2; 
	    int idy = d_iy + 2; 

	    MPCE[idx][idy]     =  shwr->get_mpcTwrE(j, k, false); 
	    MPCTOF[idx][idy]   =  shwr->get_mpcTwrTOF(j, k); 
	    MPCHS[idx][idy][0] =  shwr->get_mpcTwrHS(j, k, 0); 
	    MPCHS[idx][idy][1] =  shwr->get_mpcTwrHS(j, k, 1);
	    MPCQUAL[idx][idy]  =  shwr->get_mpc_quality(j, k);

	    N_5x5++; 
	    if( (abs(d_ix)<=1) && (abs(d_iy)<=1) ) N_3x3++; 
	    
 
	  }

	}

      }
     
    }

  }



}
