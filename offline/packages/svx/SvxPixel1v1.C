// =============================================================================
/// \file SvxPixel1v1.C
/// \brief Implementation of SvxPixel1v1
///
/// Created  by V. L. Rykov: 18-Mar-2004
/// Modified by V. L. Rykov: 22-Apr-2004
///      The methods of the base class, SvxSensor, are used for the
///      Local->Global transformations of the cluster characteristics.
/// Modified by V. L. Rykov: 15-May-2004
///      Sorted hit containers and fast index searches are expolited.
/// Documentation/Clean-up attempted 12-Feb-2013 M. McCumber
// =============================================================================

#include <SvxPixel1v1.h>

// svx includes
#include <SvxCommon.h>
#include <svxAddress.hh>

#include <PdbSvxPixelHotDeadPixelMap.hh>

// standard includes
#include <vector>

/// \struct pixel_cluster
/// \brief This is a temporary storage struct used for cluster construction
///
struct pixel_cluster {
  int x_min;                      ///< minimum local x extent
  int x_max;                      ///< maximum local x extent
  int z_min;                      ///< minimum local z extent
  int z_max;                      ///< maximum local z extent
  int xsum;                       ///< running sum of x positions
  int zsum;                       ///< running sum of z positions
  int nhits;                      ///< running count of hits
  int circumference;              ///< sum of edges around cluster
  bool is_Valid;                  ///< flag for top-level recursive clustering
  std::vector<int> channel_list;  ///< list of 2d position codes (x*NZ+z)
  std::vector<int> rawhitID_list; ///< list of rawhit ids

  /// this method clears out most temp storage
  void clear() {
    nhits = 0;
    circumference = 0;
    channel_list.clear();
    rawhitID_list.clear();
    is_Valid = false;
  };
};

ClassImp(SvxPixel1v1)

/// instumented channel cut-off_set flag
bool SvxPixel1v1::cut_off_set_flag = false;

/// Default constructor. Establishes empty internal storage and determines which
/// parts of the sensor are instrumented.
/// 
/// \param[in] sc section
/// \param[in] lr layer 
/// \param[in] ld ladder
/// \param[in] sn sensor
///
SvxPixel1v1::SvxPixel1v1(const int sc, const int lr, const int ld, const int sn)
  : SvxStripixel<1>::SvxStripixel(sc, lr, ld, sn) {

  // initialize internal sensor storage to empty
  m_hitcount = 0;
  for (int iz=0; iz<NZ; ++iz) {
    is_HitColumn[iz] = false;
    for (int ix=0; ix<NX; ++ix) {
      m_hitmap[0][ix][iz] = m_unHitDefVal;
    }
  }

  // by default the insturmented channel cut-off is not set
  /// \todo check if the chanCutOff is used anywhere outside of this code
  if ( !cut_off_set_flag ) {

    // loop over inherited storage
    for (unsigned int i=0; i<nSection; ++i ) {
      for (unsigned int j=0; j<nReadout; ++j) {

	int area_cutoff = senSec[i][j]->get_nXpitch() +
	  zSlope[i][j]*(senSec[i][j]->get_nZpitch() - 1);
	int inst_cutoff = chanUplim[i][j] - chanOffs[i][j];

	if (inst_cutoff < area_cutoff - 1) {
	  chanCutOff[i][j] = inst_cutoff - zSlope[i][j];
	  if ((inst_cutoff+1)%zSlope[i][j] == 0) {
	    chanCutOff[i][j]++;
	  }
	} else {
	  chanCutOff[i][j] = area_cutoff;
	}
      } // end readout loop
    } // end section loop

    // now the insturmented limits are set
    cut_off_set_flag = true;
  } // end if limits not set

  return;
}

/// PHObject Reset implementation. Clears internal storage and calls reset
/// on inherited object
///
/// \todo figure out if the reset on senSec should be called in constructor 
///
void SvxPixel1v1::Reset()
{
  // initialize internal sensor storage to empty
  m_hitcount = 0;
  for (int iz=0; iz<NZ; ++iz) {
    // skip column if already zero
    if (is_HitColumn[iz]) {
      is_HitColumn[iz] = false;
      for (int ix=0; ix<NX; ++ix) {
	m_hitmap[0][ix][iz] = m_unHitDefVal;
      }
    }
  }

  // call reset on inherited object
  for (unsigned int i=0; i<nSection; ++i) {
    for (unsigned int j=0; j<nReadout; ++j) {
      senSec[i][j]->Reset();
    }
  }

  return;
}

/// Pixel clustering algorithm. Finds all clusters from adjacent rawhits on 
/// the sensor.
/// 
/// \param[in] rawlist pointer to list of raw detector hits
/// \param[in,out] clslist pointer to list of found clusters
/// \param[in,out] raw2cls pointer to rawhit to cluster ancestory
/// \param[in,out] ght2raw pointer to ghit to raw detector hit ancestory
/// \param[in,out] ght2cls pointer to ghit to cluster ancestory
/// \param[in] SvxAddressObject (not used within) 
///
/// \return number of clusters found in sensor
///
int SvxPixel1v1::findClusters(SvxRawhitList* rawlist,
			      SvxClusterList* clslist,
			      SvxRawhitClusterList* raw2cls,
			      SvxGhitRawhitList* ght2raw,
			      SvxGhitClusterList* ght2cls,
			      svxAddress* SvxAddressObject) {

  // if the sensor has no clusters bail...
  if (m_hitcount <= 0) return 0;

  // create a temp storage list of clusters
  std::vector<pixel_cluster> cluster_list(m_hitcount);
  int icluster = 0;

  // loop over all columns
  for (int z=0; z<NZ; ++z) {

    // if there is no hit, skip this column
    if (!is_HitColumn[z]) continue;
      
    // otherwise loop over all rows
    for (int x=0; x<NX; ++x) {
      // only if pixel contains an unclustered hit
      if (m_hitmap[0][x][z]==m_HitDefVal) {
	// reset the cluster	      
	cluster_list[icluster].clear();
	
	// recursively grow the new cluster from this point
	growPixelCluster(cluster_list.at(icluster), icluster, x, z);
	
	// move to a new cluster
	++icluster;
      } // end-if unclustered hit present

    } // row-loop
  } // column-loop

  // at this point all found clusters are in temp storage

  // export valid clusters
  int numClusters = 0;
  for (int i=0; i<icluster; ++i) {
    if (cluster_list[i].is_Valid) {

      // cluster geometry
      short xsize = (short)(cluster_list[i].x_max - cluster_list[i].x_min + 1);
      short zsize = (short)(cluster_list[i].z_max - cluster_list[i].z_min + 1);
      short circumference = (short)(cluster_list[i].circumference);

      // cluster section
      int section = cluster_list[i].zsum / cluster_list[i].nhits / 32 * 2;
      // section edge logic for clusters
      if ((section!=0)&&(cluster_list[i].zsum/cluster_list[i].nhits%32==0)) {
	--section;
      }
      if ((section!=6)&&(cluster_list[i].zsum/cluster_list[i].nhits%32==31)) {
	++section; 
      }
      
      // determine cluster position
      double pos_local[3] = {0.0};
      double pos_global[3] = {0.0};
      double ave_lx = ((double)cluster_list[i].xsum)/cluster_list[i].nhits;
      double ave_lz = ((double)cluster_list[i].zsum)/cluster_list[i].nhits;
      pos_local[0] = (float)get_sensorXpos(section,0,ave_lx);
      pos_local[1] = 0.0;
      pos_local[2] = (float)get_sensorZpos(section,0,ave_lz);
      position_local2global(pos_local, pos_global);

      // flag clusters on edge of readout
      int edge_flag = 0;
      if (cluster_list[i].x_min==0   ) edge_flag |= 1;
      if (cluster_list[i].x_max==NX-1) edge_flag |= 2; 
      if (cluster_list[i].z_min==0   ) edge_flag |= 4;
      if (cluster_list[i].z_max==NZ-1) edge_flag |= 8;

      // count ncold/nhot
      short Ncold=0, Nhot=0;
      for(int iraw=0; iraw<(int)cluster_list[i].rawhitID_list.size(); ++iraw)
        {
	  int rawid = cluster_list[i].rawhitID_list[iraw];
          SvxRawhit *tmp_raw = rawlist->get_Rawhit(rawid);
          if(tmp_raw==NULL){
            std::cerr<<"no rawhit : id="<<iraw<<std::endl;
            continue;
          }
          //std::cout<<"rawid : "<<iraw<<" "<<rawid<<" "<<tmp_raw->get_hitID()<<std::endl;

          int status = tmp_raw->get_HotDeadFlag();
          if ((char)status ==  PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_COLD) Ncold++;
          if ((char)status ==  PdbSvxPixelHotDeadPixelMap::SVX_PIXEL_HOT)  Nhot++;
        
        }
      if (Ncold > 255) Ncold = 255;
      if (Nhot  > 255) Nhot  = 255;

      // fill output storage
      SvxCluster *cluster = clslist->addCluster();
      cluster->set_svxSection(svxSection);
      cluster->set_layer(layer);
      cluster->set_ladder(ladder);
      cluster->set_sensor(sensor);
      cluster->set_sensorType(sensorType);
      cluster->set_adc(0, 1);
      cluster->set_adc(1, 1);
      cluster->set_size(cluster_list[i].nhits);
      cluster->set_xz_size(0, xsize);
      cluster->set_xz_size(1, zsize);
      cluster->set_circumference(circumference);
      cluster->set_edgeflag(edge_flag);
      cluster->set_Nhot(Nhot);
      cluster->set_Ncold(Ncold);
      for ( int j=0; j<3; j++ ) {
	cluster->set_xyz_local (j, pos_local[j]);
	cluster->set_xyz_global(j, pos_global[j]);
      }

      // advance cluster counter
      ++numClusters;

      // create rawhit<->cluster relations
      if (raw2cls) {
	int clus_id = cluster->get_hitID();    

	// loop over all raw hits in cluster
	for (unsigned int iraw=0; 
	     iraw<cluster_list[i].rawhitID_list.size(); 
	     ++iraw) {

	  // create a new association
	  SvxRawhitCluster* r2c = raw2cls->addRawhitCluster();
	  r2c->set_clusterID(clus_id);
	  r2c->set_rawhitID(cluster_list[i].rawhitID_list[iraw]);
	}
      }

      // create ghit<->cluster and ghit<->rawhit relation
      if (ght2cls && ght2raw) {
	int clus_id = cluster->get_hitID(); 

	// temp storage for ghit ids
	std::vector<int> g_id;

	// loop over all rawhits
	for (unsigned int iraw=0; 
	     iraw<cluster_list[i].rawhitID_list.size(); 
	     ++iraw) {

	  int id_raw = cluster_list[i].rawhitID_list[iraw];
	  int g2r_first = 0;
	  int g2r_last = 0;
	  ght2raw->indexOfRawhit(id_raw, g2r_first, g2r_last);

	  for (int ig2r = g2r_first; ig2r <= g2r_last; ig2r++) {
	    // create new ghit<->rawhit relation
	    SvxGhitRawhit* g2r = ght2raw->get_GhitRawhit(ig2r);
	    int id_ghit = g2r->get_ghitID();

	    // if not already stored locally, then add
	    if (find(g_id.begin(), g_id.end(), id_ghit) == g_id.end()) {
	      g_id.push_back(id_ghit);
	    }
	  }
	}

	// loop over temp list of ghits now
	for (unsigned int ighit=0; ighit<g_id.size(); ++ighit) {
	  // create new ghit<->cluster relation
	  SvxGhitCluster* g2c = ght2cls->addGhitCluster();
	  g2c->set_clusterID(clus_id);
	  g2c->set_ghitID(g_id.at(ighit));
	}
      }

    } // end-if valid cluster
  } // end-loop over clusters
  
  // return number of exported clusters
  return numClusters;
}

/// This method, if called on an unclustered hit
/// at position (x,y), grows the cluster object by incorporating
/// all neighboring clusters recursively. Diagonally
/// separated hits are included in the search.
///
/// \param[in,out] cluster cluster information
/// \param[in] icluster cluster id
/// \param[in] x x-location to add to cluster
/// \param[in] z z-location to add to cluster
///
void SvxPixel1v1::growPixelCluster(pixel_cluster &cluster, 
				   int icluster, 
				   int x, 
				   int z) {

  /// \todo add check & bail if accidently called on a clustered-hit or non-hit

  // designate the this spot as clustered in the map
  m_hitmap[0][x][z] = icluster;

  // extend the cluster
  if(!cluster.is_Valid) {
    // if not valid, initialize the cluster
    cluster.is_Valid = true;

    cluster.x_min = x;
    cluster.x_max = x;
    cluster.xsum = x;
    
    cluster.z_min = z;
    cluster.z_max = z;
    cluster.zsum = z;
  } else {
    // if already valid, update these only as needed
    if (cluster.x_min > x) cluster.x_min = x;
    if (cluster.x_max < x) cluster.x_max = x;
    cluster.xsum += x;
  
    if (cluster.z_min > z) cluster.z_min = z;
    if (cluster.z_max < z) cluster.z_max = z;
    cluster.zsum += z;
  }

  cluster.nhits += 1;
  cluster.channel_list.push_back(x*NZ+z);
  cluster.rawhitID_list.push_back(m_hitmap[1][x][z]);

  // now grow the cluster over unclustered neighbors
  // orthogonal recursive calls are made first
  // missing hits add to the circumference calculation

  // down the row
  if (x != 0) {
    if (m_hitmap[0][x-1][z] == m_HitDefVal) {
      growPixelCluster(cluster, icluster, x-1, z);      
    } else if ( m_hitmap[0][x-1][z] == m_unHitDefVal ) { 
      cluster.circumference+=1; 
    }
  } else { 
    cluster.circumference+=1; 
  }

  // up the row
  if (x != NX-1) {
    if (m_hitmap[0][x+1][z] == m_HitDefVal) {
      growPixelCluster(cluster, icluster, x+1, z);
    } else if ( m_hitmap[0][x+1][z] == m_unHitDefVal ) { 
      cluster.circumference+=1; 
    }
  } else { 
    cluster.circumference+=1; 
  }
  
  // down the column
  if (z != 0) {
    if (m_hitmap[0][x][z-1] == m_HitDefVal) {
      growPixelCluster(cluster, icluster, x, z-1);
    } else if (m_hitmap[0][x][z-1] == m_unHitDefVal) {
      cluster.circumference+=1; 
    }
  } else { 
    cluster.circumference+=1; 
  }

  // up the column
  if (z != NZ-1) {
    if (m_hitmap[0][x][z+1] == m_HitDefVal) {
      growPixelCluster(cluster, icluster, x, z+1);
    } else if ( m_hitmap[0][x][z+1] == m_unHitDefVal) {
      cluster.circumference+=1; 
    }
  } else { 
    cluster.circumference+=1; 
  }

  // diagonal recursive calls below
  // missing hits will not increment 
  // the circumference as they add no
  // length

  // down the row, down the column
  if (( x != 0 )&&( z != 0 )) {
    if (m_hitmap[0][x-1][z-1] == m_HitDefVal) {
      growPixelCluster(cluster, icluster, x-1, z-1);      
    }
  }

  // up the row, down the column
  if (( x != NX-1 )&&( z != 0 )) {
    if ( m_hitmap[0][x+1][z-1] == m_HitDefVal ) {
      growPixelCluster(cluster, icluster, x+1, z-1);      
    }
  }

  // down the row, up the column
  if (( x != 0 )&&( z != NZ-1 )) {
    if ( m_hitmap[0][x-1][z+1] == m_HitDefVal ) {
      growPixelCluster(cluster, icluster, x-1, z+1);      
    }
  }

  // up the row, up the column
  if (( x != NX-1 )&&( z != NZ-1 )) {
      if ( m_hitmap[0][x+1][z+1] == m_HitDefVal ) {
	growPixelCluster(cluster, icluster, x+1, z+1);      
      }
  }

  // return upwards in the recusion
  return;
}

/// Gives the local x position from the sensor ix value
///
/// \param[in] section sensor section
/// \param[in] readout ???
/// \param[in] ix local x channel position
///
/// \return local x position 
///
double SvxPixel1v1::get_sensorXpos(const int section, 
				   const int readout, 
				   int ix) const {
  double xPitch = senSec[section][readout]->get_xPitch();
  double xhalfWidth = senSec[section][readout]->get_xhalfWidth();
  double xpos = (ix+0.5)*xPitch - xhalfWidth;
  xpos += secXpos[section][readout];
  if ( xcntRvrs[section][readout] ) xpos *= -1.0;

  return xpos;
}

/// Gives the local z position from the sensor iz value
///
/// \param[in] section sensor section
/// \param[in] readout ???
/// \param[in] iz local z channel position
///
/// \return local z position 
///
double SvxPixel1v1::get_sensorZpos(const int section, 
				   const int readout, 
				   int iz) const {
  // First adjust the Iz to its value within the section
  for (int isec=0; isec<section; isec++) {
    int npitch = senSec[isec][readout]->get_nZpitch();
    iz -= npitch;
  }
  double pitch = senSec[section][readout]->get_zPitch();
  double halfWidth = senSec[section][readout]->get_zhalfWidth();
  double zpos = (iz+0.5)*pitch - halfWidth;
  zpos += secZpos[section][readout];
  if ( zcntRvrs[section][readout] ) zpos *= -1.0;

  return zpos;
}

/// Gives the local x position from the sensor dx value
///
/// \param[in] section sensor section
/// \param[in] readout ???
/// \param[in] dx local x channel position
///
/// \return local x position 
///
double SvxPixel1v1::get_sensorXpos(const int section, 
				   const int readout, 
				   double dx) const {
  double xPitch = senSec[section][readout]->get_xPitch();
  double xhalfWidth = senSec[section][readout]->get_xhalfWidth();
  double xpos = (dx+0.5)*xPitch - xhalfWidth;
  xpos += secXpos[section][readout];
  if (xcntRvrs[section][readout]) xpos *= -1.0;

  return xpos;
}

/// Gives the local z position from the sensor dz value
///
/// \param[in] section sensor section
/// \param[in] readout ???
/// \param[in] dz local z channel position
///
/// \return local z position 
///
double SvxPixel1v1::get_sensorZpos(const int section, 
				   const int readout, 
				   double dz) const {
  // First adjust the Iz to its value within the section
  for (int isec=0; isec<section; isec++) {
    int npitch = senSec[isec][readout]->get_nZpitch();
    dz -= npitch;
  }
  double zpitch = senSec[section][readout]->get_zPitch();
  double zhalfWidth = senSec[section][readout]->get_zhalfWidth();
  double zpos = (dz+0.5)*zpitch - zhalfWidth;
  zpos += secZpos[section][readout];
  if ( zcntRvrs[section][readout] ) zpos *= -1.0;

  return zpos;
}

