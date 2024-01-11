// ====================
// FILE: SvxStrip11v1.C
// ====================
// ***************************************************************************
// Implementation of SvxStrip11v1
// ---
// Created  by V. L. Rykov: 18-Mar-2004
// ---
// Modified by V. L. Rykov: 22-Apr-2004
//      The methods of the base class, SvxSensor, are used for the
//      Local->Global transformations of the cluster characteristics.
// Modified by V. L. Rykov: 15-May-2004
//      Sorted hit containers and fast index searches are expolited.
// ***************************************************************************

#include <SvxCommon.h>
#include <SvxStrip11v1.h>

#include <algorithm>
#include <iostream>

ClassImp(SvxStrip11v1)

// Flag for the sensor configuration applicability (check by makeMosaic)
bool SvxStrip11v1::config_checked   = false; // set true if config is correct
// Instumented channel cut-off_set flag
bool SvxStrip11v1::cut_off_set_flag = false; // true if chanCutOff[][] set

// Constructor(s) & Destructor
// """""""""""""""""""""""""""
SvxStrip11v1::SvxStrip11v1(const int sc,const int lr,const int ld,const int sn)
  : SvxStripixel<11>::SvxStripixel(sc, lr, ld, sn) {

  // Set instrumented channel cut-off
  if ( !cut_off_set_flag ) {
    for ( unsigned int i = 0; i < nSection; i++ ) {
      for ( unsigned int j = 0; j < nReadout; j++ ) {
        if(senSec[i][j]==NULL){
          std::cerr<<"SvxStrip11v2::ctor no template idx is available"<<std::endl;
          continue;
        }
        
  int area_cutoff = senSec[i][j]->get_nXpitch() +
    zSlope[i][j]*(senSec[i][j]->get_nZpitch() - 1);
  int inst_cutoff = chanUplim[i][j] - chanOffs[i][j];
  if ( inst_cutoff < area_cutoff - 1 ) {
    chanCutOff[i][j] = inst_cutoff - zSlope[i][j];
  } else {
    chanCutOff[i][j] = area_cutoff;
  }
      }
    }
    cut_off_set_flag = true;
  }

  m_sNOISE             = 0;
  m_ADCThresholdZS     = 0;
  m_adc_min_rawhit     = 0;
  m_adc_min_rawhit_sum = 0;

  //std::cout << "SvxStrip11v1 object created" << std::endl;
}

SvxStrip11v1::~SvxStrip11v1() {
  //std::cout << "SvxStrip11v1 object destroyed" << std::endl;
}

int SvxStrip11v1::AddNoise(const int rhit_first, const int rhit_last, SvxRawhitList* rawlist, 
                          SvxGhitRawhitList* g2raw, int g2rFirst) const {
  //  printf("sNOISE=%f adcthresh=%d\n",m_sNOISE,m_ADCThresholdZS);

  // The number of channels for each section / readout = 384
  int  chmax = chanCutOff[0][0];
  int  RawhitID[nSection][nReadout][chmax];

  // Initialized
  for(unsigned int isec=0; isec<nSection; isec++){
    for(unsigned int iro=0; iro<nReadout; iro++){
      for(int ich=0; ich<chmax; ich++){
  RawhitID[isec][iro][ich]  = -999;
      }
    }
  }

  int n_g2r = g2raw->get_nGhitRawhits();
  int nhit_removed=0;

  static const int thre_negative = 1;
  // make signal fluctuated by noise
  for(int iraw = rhit_first; iraw < rhit_last; iraw++) {
    SvxRawhit* raw = rawlist -> get_Rawhit(iraw);
    int raw_layer  = raw -> get_layer();
    int raw_ladder = raw -> get_ladder();
    int raw_sensor = raw -> get_sensor();
    int raw_sensec = raw -> get_sensorSection();
    int raw_xuro   = raw -> get_sensorReadout();
    int raw_ch     = raw -> get_channel();
    int raw_adc    = raw -> get_adc();

    if( layer==raw_layer && ladder==raw_ladder && sensor==raw_sensor ){
      int ADCwNOISE = (int)rnd->Gaus(raw_adc, m_sNOISE);
      if( ADCwNOISE < thre_negative ) { // added by TH 2011.02.25, avoid adc value to be negative by noise 
        int rawid = raw->get_hitID();
        rawlist->removeRawhit(iraw);
  /*
        { // added by TH
          cout<<PHWHERE<<"   Hit is removed "<<rawid<<" "<<raw_layer<<" "<<raw_ladder<<" "<<raw_sensor<<" ";
          cout<<raw_sensec<<" "<<raw_xuro<<" "<<raw_ch<<" "<<raw_adc<<" "<<ADCwNOISE<<" ";
          cout<<" thre : "<<thre_negative <<" Threshold value hardcoded. should change"<<endl;
        }
  */
        // Invalidate the rawhit references in the g2raw list
        for ( int i = g2rFirst; i < n_g2r; i++ ) {
          SvxGhitRawhit* g2r = g2raw->get_GhitRawhit(i);
          if ( g2r->get_rawhitID() == rawid ) g2r->set_rawhitID(-1);
        }
        nhit_removed++;
      }
      else {
        if( ADCwNOISE > adctop ) ADCwNOISE = adctop;
        raw -> set_adc( ADCwNOISE );
        RawhitID[raw_sensec][raw_xuro][raw_ch] = iraw;
      }
    } else {
       std::cerr << "SvxStrip11v1::AddNoise() - It must be true.  Find & fix a problem." << std::endl;
       exit(1);
    }
  }

  //-- added by TH 20110225
  // clear up g2raw list if the adc=0 happen
  if(nhit_removed>0){
    for ( int i = g2rFirst; i < n_g2r; i++ ) {
      SvxGhitRawhit* g2r = g2raw->get_GhitRawhit(i);
      if(!g2r) continue;
      if(g2r->get_rawhitID()<0) {
        //--cout<<"   "<<PHWHERE<<"  g2raw removed: "<<g2r->get_rawhitID()<<" "<<g2r->get_ghitID()<<endl;
        g2raw->removeGhitRawhit(i);
      }
    }

    ////////
    rawlist->Compress();
    g2raw  ->Compress();
  }

  // add noise hit 
  //--int noiseid_begin = rawlist->get_nRawhits();

  int nhit = 0;
  for (unsigned int isec = 0; isec < nSection; isec++) {
    for (unsigned int iro = 0; iro < nReadout; iro++) {
      for (int ich = 0; ich < chmax; ich++) {
  if( RawhitID[isec][iro][ich] != -999 ) continue;

  int ADCwNOISE = (int) rnd->Gaus(0, m_sNOISE);
  if( ADCwNOISE < m_ADCThresholdZS ) continue;
  if( ADCwNOISE > adctop ) ADCwNOISE = adctop;

  SvxRawhit* raw = rawlist->addRawhit();
  raw -> set_svxSection(svxSection);
  raw -> set_layer( layer );
  raw -> set_ladder( ladder );
  raw -> set_sensor( sensor );
  raw -> set_sensorSection( isec );
  raw -> set_sensorReadout( iro );
  raw -> set_sensorType( sensorType );
  raw -> set_channel( ich );
  raw -> set_adc( ADCwNOISE );

  nhit++;

        //--{
        //--  cout<<"   "<<PHWHERE<<"  noise hit: "<<raw->get_hitID()<<" ";
        //--  cout<<layer<<" "<<ladder<<" "<<sensor<<" "<<ich<<" "<<ADCwNOISE<<endl;
        //--}
      } // ich
    } // iro
  } //isec

  //--int noiseid_end = rawlist->get_nRawhits();
  //--{
  //--  if(nhit>0) cout<<"   "<<PHWHERE<<"  Nnoise hit: "<<nhit<<"  start-end= "<<noiseid_begin<<" "<<noiseid_end<<endl;
  //--}

//  return nhit;
  return (nhit - nhit_removed);
}

//----------------------------------------------------------------------------------------------------

int SvxStrip11v1::findClusters(
  SvxRawhitList* rawlist,
  SvxClusterList* clslist,
  SvxRawhitClusterList* raw2cls,
  SvxGhitRawhitList* ght2raw,
  SvxGhitClusterList* ght2cls,
  svxAddress* SvxAddressObject)
{

  RawhitGroupList_t** grp_list;
  grp_list = new RawhitGroupList_t*[nSection];
  for (unsigned int i = 0; i < nSection; i++) grp_list[i] = new RawhitGroupList_t[nReadout];

  //  makeGrouphits(rawlist, grp_list);
  makeGrouphits(grp_list);
  int nClusters = makeMosaic(grp_list, clslist, raw2cls, ght2raw, ght2cls, SvxAddressObject);

  for (unsigned int i = 0; i < nSection; i++) delete [] grp_list[i];
  delete [] grp_list;
  
  return nClusters;
}

//----------------------------------------------------------------------------------------------------

//int SvxStrip11v1::makeGrouphits(SvxRawhitList* rawlist, RawhitGroupList_t** grp_list)
int SvxStrip11v1::makeGrouphits(RawhitGroupList_t** grp_list)
{

  /*
  int nraw = rawlist->get_nRawhits();
  if (nraw <= 0) return 0;
  /// Search the indices for get_Rawhit() which of rawhits are at the same sensor.
  int idx_raw_first, idx_raw_last;
  if (! rawlist->indexOfRawhit(this, idx_raw_first, idx_raw_last)) return 0;

  // Select rawhits and create pointers in the senSec objects
  for (int iraw = idx_raw_first; iraw <= idx_raw_last; iraw++) {
     SvxRawhit* rhit = rawlist->get_Rawhit(iraw);    

     if (rhit->get_adc() >= m_adc_min_rawhit) {
        senSec[rhit->get_sensorSection()][rhit->get_sensorReadout()]
           ->addRawhit(rhit);
     }
  }
  */
  for (unsigned int isec = 0; isec < nSection; isec++) {
     for (unsigned int irout = 0; irout < nReadout; irout++) {
        grp_list[isec][irout].reserve(32);
        RawhitGroup_t group;
        while (CollectNeighborRawhits(isec, irout, group)) {
           int adc_sum = 0;
           for (unsigned int i = 0; i < group.size(); i++) {
              adc_sum += group.at(i)->get_adc();
           }
           if (adc_sum >= m_adc_min_rawhit_sum) grp_list[isec][irout].push_back(group);
           group.clear();
        }
     }
  }
  return 0;
}

//----------------------------------------------------------------------------------------------------

bool SvxStrip11v1::CollectNeighborRawhits(
   int section, int readout, RawhitGroup_t& group, SvxRawhit* rhit)
{
  SvxPixStructure* sensor = senSec[section][readout];
  if (sensor->get_nRawptr() == 0) return false;

  if (! rhit) rhit = sensor->get_rawPtr(0);
  sensor->removeRawhit(rhit);
  if(rhit->get_HotDeadFlag()==0){ 
    group.push_back(rhit);

    for (int dch = -1; dch <= 1; dch++) {
      if (dch == 0) continue;
      int chan = rhit->get_channel() + dch;
      // channel offset must be taken into account if it is non-zero...
      if (chan < 0 || chan > chanUplim[section][readout]) {
        if (readout == 0) continue; // x-strip, no loopback
        else { // u-strip, chan=0~28 cover chan=384~412
          if (chan < 0) chan = chanUplim[section][readout];
          else          chan = 0;
        }
      }
      for (unsigned int i_rhit = 0; i_rhit < sensor->get_nRawptr(); i_rhit++) {
         SvxRawhit* rhit_tmp = sensor->get_rawPtr(i_rhit);
         if (rhit_tmp->get_channel() == chan) {
            CollectNeighborRawhits(section, readout, group, rhit_tmp);
            break;
         }
      }
    }
  }
  return true;
}

//----------------------------------------------------------------------------------------------------

int SvxStrip11v1::makeMosaic(RawhitGroupList_t**   grp_list,
                             SvxClusterList*       clslist,
                             SvxRawhitClusterList* raw2cls,
                             SvxGhitRawhitList*    ght2raw,
                             SvxGhitClusterList*   ght2cls,
                             svxAddress* SvxAddressObject)
{

  // Get the beginning of the free area in clslist
  int firstCls = clslist->get_nClusters();

  // Length of ght2raw list
//  int n_g2raw  = ( ght2raw ) ? ght2raw->get_nGhitRawhits() : 0;
	

  // Cycle over pixel sections
  for (unsigned int isec = 0; isec < nSection; isec++) {
	
		/*for (unsigned int igrx=0; igrx<grp_list[isec][0].size();igrx++){
		RawhitGroup_t* grp_x = &grp_list[isec][0].at(igrx);
		unsigned int nhitx = grp_x->size();
		if(nhitx>5){
		 int ch_x_high = -1;
		 int ch_x_low = -1;
		 for (unsigned int ix=0; ix<nhitx; ix++){
			 int ch = grp_x->at(ix)->get_channel()-chanOffs[isec][0];
			 if(ch>ch_x_high || ch_x_high<0) ch_x_high = ch;
			 if(ch<ch_x_low || ch_x_low<0) ch_x_low = ch;
			 }
		//std::cout<<"xcl,"<<nhitx<<","<<ch_x_low<<","<<ch_x_high<<std::endl;
		}
	}
	for (unsigned int igru=0; igru<grp_list[isec][1].size();igru++){
		RawhitGroup_t* grp_u = &grp_list[isec][1].at(igru);
		unsigned int nhitu = grp_u->size();
		if(nhitu>5){
		 int ch_u_high = -1;
		 int ch_u_low = -1;
		 for (unsigned int iu=0; iu<nhitu; iu++){
			 int ch = grp_u->at(iu)->get_channel()-chanOffs[isec][0];
			 if(ch>ch_u_high || ch_u_high<0) ch_u_high = ch;
			 if(ch<ch_u_low || ch_u_low<0) ch_u_low = ch;
			 }
		//std::cout<<"ucl,"<<nhitu<<","<<ch_u_low<<","<<ch_u_high<<std::endl;
		}
	}*/	
	
     int   nXpitch    = senSec[isec][0]->get_nXpitch();    /// number of pixels in a row
     int   nZpitch    = senSec[isec][0]->get_nZpitch();    /// number of column in a sensorSection
     float xPitch     = senSec[isec][0]->get_xPitch();     /// pixel length in x-direction
     float zPitch     = senSec[isec][0]->get_zPitch();     /// pixel length in z-direction
     float xhalfWidth = senSec[isec][0]->get_xhalfWidth(); /// 0.5*nXpitch*xPitch
     float zhalfWidth = senSec[isec][0]->get_zhalfWidth(); /// 0.5*nZpitch*zPitch
     // Z-positions of senSec[isec][0] edges in senSec[isec][1] frame
     float z0min = secZpos[isec][0] - secZpos[isec][1];
     if ( zcntRvrs[isec][1] ) z0min = -z0min;
     float z0max = z0min + 2.*zhalfWidth;

     for (unsigned int igrpx = 0; igrpx < grp_list[isec][0].size(); igrpx++) {
       RawhitGroup_t* grp_x = &grp_list[isec][0].at(igrpx);
       unsigned int nhitx = grp_x->size();
       
       int ch_x_high = -1;
       int ch_x_low  = -1;
       float ch_x_mean = 0;
       int adc_sum_x = 0;
       for (unsigned int i = 0; i < nhitx; i++) {
         int ch = grp_x->at(i)->get_channel() - chanOffs[isec][0];
         int adc = grp_x->at(i)->get_adc();
         if (ch > ch_x_high || ch_x_high < 0) ch_x_high = ch;
         if (ch < ch_x_low  || ch_x_low  < 0) ch_x_low  = ch;
         ch_x_mean += ch * adc;
         adc_sum_x  += adc;
       }
       ch_x_mean /= adc_sum_x;   /// ch_x_mean : weighted mean of hit pixel channel
  
        int edgeXflag = ( ch_x_high < chanCutOff[isec][0]) ?  0 : 16;
        if ( ch_x_low == 0 ) {
           edgeXflag = ( xcntRvrs[isec][0] ) ? edgeXflag | 2 : edgeXflag | 1;
        } else if ( ch_x_high == nXpitch-1 ) {
           edgeXflag = ( xcntRvrs[isec][0] ) ? edgeXflag | 1 : edgeXflag | 2;
        }

        // X-position (channel center)
        float x = xPitch * (ch_x_mean + 0.5) - xhalfWidth; // in secSen[isec][0] frame
        if ( xcntRvrs[isec][0] ) x = -x;
        x += secXpos[isec][0]; // in the sensor frame
        x -= secXpos[isec][1];
        if ( xcntRvrs[isec][1] ) x = -x;
        x += xhalfWidth; // in senSec[isec][1] frame
        // float x_width = xPitch * nhitx;

        // Find U-rows overlapping with the x-group
        for (unsigned int igrpu = 0; igrpu < grp_list[isec][1].size(); igrpu++) {
        RawhitGroup_t* grp_u = &grp_list[isec][1].at(igrpu);
        unsigned int nhitu = grp_u->size();
    
        bool bl_at_edge = false;
        for (unsigned int i = 0; i < nhitu; i++) {
          int ch = grp_u->at(i)->get_channel() - chanOffs[isec][1];
          if (ch == 0 || ch == chanUplim[isec][1]) bl_at_edge = true;
        }
        int ch_u_high = -1;
        int ch_u_low  = -1;
        float ch_u_mean = 0;
        int adc_sum_u = 0;
        for (unsigned int i = 0; i < nhitu; i++) {
          int ch = grp_u->at(i)->get_channel() - chanOffs[isec][1];
          int adc = grp_u->at(i)->get_adc();

          if ( bl_at_edge && ch>(int)nhitu ) { ch -= chanUplim[isec][1]+1; }
          // if bl_at_edge is true, cluster of u-strips will be devided into upper side and lower side.
          // if ch > nhitu, the hit should belong to the upper side.
          // move upper-side channels to lower side
          // so that channel numbers are continuous
          // ex) 382, 383, 0  ->  -2, -1, 0
      
          if (ch > ch_u_high || ch_u_high < 0) ch_u_high = ch;
          if (ch < ch_u_low  || ch_u_low  < 0) ch_u_low  = ch;
      
          /// The fllowing lines were moved to 2 lines upper (modified by akimoto)
          /*
            if (bl_at_edge && ch > chanOffs[isec][1] / 2) {
            // move upper-side channels (ch ~ chanUplim[]) to lower side
            // so that channel numbers are continuous
            // ex) 382, 383, 0  ->  -2, -1, 0
            ch -= chanUplim[isec][1] + 1;
                  }
          */

          ch_u_mean += ch * adc;
          adc_sum_u  += adc;
        }
        ch_u_mean /= adc_sum_u;   /// ch_u_mean : weighted mean of hit pixel channel
    
        // take into account the fact that
        // U-strips at sensor edge cover both edges (x ~ 0 & x ~ chanUplim).

        if ( isec==0 ) {
          if (ch_x_mean < (float)nZpitch-1. && ch_u_mean > (float)(chanUplim[isec][1]-nZpitch+1)) {
            ch_u_high -= chanUplim[isec][1] + 1;
            //--ch_u_low  -= chanUplim[isec][1] + 1;
            ch_u_mean -= chanUplim[isec][1] + 1;
          }
        } else if ( isec==1 ) {
          if (ch_x_mean > (float)(chanUplim[isec][0]-nZpitch+1) && ch_u_mean < (float)nZpitch-1.) {
            ch_u_high += chanUplim[isec][1] + 1;
            //--ch_u_low  += chanUplim[isec][1] + 1;
            ch_u_mean += chanUplim[isec][1] + 1;
          }
        } else {
          std::cout << "SvxStrip11v1::makeMosaic() : Wrong sensorSection input = " << isec << std::endl;
          continue;
        }
    
        // Z-position where the center of the u-hit group crosses "x"
        //float ch_z_mean = ch_u_mean - (x / xPitch - 0.5);
        float ch_z_mean = - ch_u_mean + ch_x_mean + (float)isec*(nZpitch-1);
        float z = zPitch * (ch_z_mean + 0.5);
    
        if (ch_z_mean < 0 || ch_z_mean >= nZpitch) continue;
    
        int edgeUflag = ( ch_u_high < chanUplim[isec][1] + 29) ?  0 : 16;
        float zmin = zPitch*(int)ch_z_mean;
        float zmax = zmin + zPitch;
        if (zmin <= z0min) {
          //--zmin = z0min;
          edgeUflag = (zcntRvrs[isec][1]) ? edgeUflag | 8 : edgeUflag | 4;
        }
        if (zmax >= z0max) {
          //--zmax = z0max;
          edgeUflag = (zcntRvrs[isec][1]) ? edgeUflag | 4 : edgeUflag | 8;
        }
    
        // Position in the sensor frame
        double xl[3];
        xl[0] = x - xhalfWidth;
        xl[1] = 0;
        xl[2] = z - zhalfWidth;
        if ( xcntRvrs[isec][1] ) xl[0] = -xl[0];
        if ( zcntRvrs[isec][1] ) xl[2] = -xl[2];
        xl[0] += secXpos[isec][1];
        xl[2] += secZpos[isec][1];
    
        SvxCluster* cluster = clslist->addCluster();
        cluster->set_svxSection(svxSection);
        cluster->set_layer(layer);
        cluster->set_ladder(ladder);
        cluster->set_sensor(sensor);
        cluster->set_sensorType(sensorType);
        cluster->set_edgeflag(edgeXflag | edgeUflag);
        cluster->set_adc(0, adc_sum_x);
        cluster->set_adc(1, adc_sum_u);
        cluster->set_size((short)(nhitu*nhitx));
        cluster->set_xz_size(0, (short)nhitx);
        cluster->set_xz_size(1, (short)nhitu);
        for ( int i = 0; i < 3; i++ ) {
          cluster->set_xyz_local(i, xl[i]);
        }
        double xg[3];
        position_local2global(xl, xg);
        for ( int i = 0; i < 3; i++ ) {
          cluster->set_xyz_global(i, xg[i]);
        }
    
        /*
         * this is not used now (akimoto 1/25/2011)
         *
         // Compute and fill global resolution estimates & size
         //   The resolutions for each (nhitx, nhitu) pair are based on
         //   the difference between cluster and GEANT hit
         * static const float RESO_X[3][3] = { // [nhitx-1][nhitu-1]
         *    { .0017, .0022, .0020 },
         *    { .0024, .0011, .0019 },
         *    { .0051, .0036, .0030 }
         * };
         * static const float RESO_Z[3][3] = {
         *    { .038, .052, .079 },
         *    { .054, .044, .071 },
         *    { .080, .073, .072 }
         * };
         * float reso_l[3][3] = {{0.,0.,0.},{0.,0.,0.},{0.,0.,0.}};
         * float reso_g[3][3] = {{0.,0.,0.},{0.,0.,0.},{0.,0.,0.}};
         * reso_l[0][0] = RESO_X[nhitx<3 ? nhitx-1 : 2][nhitu<3 ? nhitu-1 : 2];
         * reso_l[1][1] = 0.288675134*0.0625;
         * reso_l[2][2] = RESO_Z[nhitx<3 ? nhitx-1 : 2][nhitu<3 ? nhitu-1 : 2];
         *
         // reso_l[0][0] = xPitch * xPitch *  ch_x_vari2 / adc_sum_x;
         // reso_l[2][2] = zPitch * zPitch * (ch_x_vari2 / adc_sum_x
         //			     + ch_u_vari2 / adc_sum_u);
         // reso_l[1][1] = nhitx*1000 + nhitu;
         *
         *
         * matrix_local2global(reso_l, reso_g);
         * for (int i = 0; i < 3; i++) {
         *   for (int j = 0; j < 3; j++) {
         *      cluster->set_size_xyz_local (i, j, reso_l[i][j]);
         *      cluster->set_size_xyz_global(i, j, reso_g[i][j]);
         *   }
         * }
         */

        ////////////////////////////////////////////////////////////////
    
        int clus_id = cluster->get_hitID();
    
        // Create Rawhit<->Cluster relation
        if (! raw2cls) continue;
        for (unsigned int i = 0; i < nhitx; i++) {
          SvxRawhitCluster* r2c = raw2cls->addRawhitCluster();
          r2c->set_clusterID(clus_id);
          r2c->set_rawhitID( grp_x->at(i)->get_hitID() );
        }
        for (unsigned int i = 0; i < nhitu; i++) {
          SvxRawhitCluster* r2c = raw2cls->addRawhitCluster();
          r2c->set_clusterID(clus_id);
          r2c->set_rawhitID( grp_u->at(i)->get_hitID() );
        }
    
        /*
        // Create Ghit<->Cluster relation
        if (! ght2cls || ! ght2raw) continue;
        SvxGhitRawhit* g2r = 0;
        //int g2cFirst = ght2cls->get_nGhitClusters();
        std::vector<int> g_id_x;
        std::vector<int> g_id_u;
        for (unsigned int i_hitx = 0; i_hitx < nhitx; i_hitx++) {
          int id_raw = grp_x->at(i_hitx)->get_hitID();
          int g2r_1st, g2r_lst;
          ght2raw->indexOfRawhit(id_raw, g2r_1st, g2r_lst);
          for (int i_g2r = g2r_1st; i_g2r <= g2r_lst; i_g2r++) {
            g2r = ght2raw->get_GhitRawhit(i_g2r);
            int id_ghit = g2r->get_ghitID();
            if (find(g_id_x.begin(), g_id_x.end(), id_ghit) == g_id_x.end()) g_id_x.push_back(id_ghit);
          }
        }
    
        for (unsigned int i_hitu = 0; i_hitu < nhitu; i_hitu++) {
          int id_raw = grp_u->at(i_hitu)->get_hitID();
          int g2r_1st, g2r_lst;
          ght2raw->indexOfRawhit(id_raw, g2r_1st, g2r_lst);
          for (int i_g2r = g2r_1st; i_g2r <= g2r_lst; i_g2r++) {
            g2r = ght2raw->get_GhitRawhit(i_g2r);
            int id_ghit = g2r->get_ghitID();
            if (find(g_id_u.begin(), g_id_u.end(), id_ghit) == g_id_u.end()) g_id_u.push_back(id_ghit);
          }
        }
    
        for (unsigned int ix = 0; ix < g_id_x.size(); ix++) {
          for (unsigned int iu = 0; iu < g_id_u.size(); iu++) {
            if (g_id_x.at(ix) == g_id_u.at(iu)) {
        SvxGhitCluster* g2c = ght2cls->addGhitCluster();
        g2c->set_clusterID(clus_id);
        g2c->set_ghitID(g_id_x.at(ix));
        break; // iu loop
            }
          }
        }
        */
        } // igrpu end
     } // igrpx end
  }
  return clslist->get_nClusters() - firstCls;
}

void SvxStrip11v1::add_rawhit(SvxRawhit *rawhit)
{
  int section = rawhit->get_sensorSection();
  int readout = rawhit->get_sensorReadout();
  int adc = rawhit->get_adc();
  if ( adc>=m_adc_min_rawhit ) {
    senSec[section][readout]->addRawhit(rawhit);
  }
}
