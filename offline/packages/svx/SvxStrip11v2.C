// =============================================================================
///
/// \file SvxStrip11v2.C
/// \brief implementation of SvxStrip11v2
/// \author M. McCumber (under the SVX documetaion effort)
///
/// Created  by V. L. Rykov: 18-Mar-2004
/// Modified by V. L. Rykov: 22-Apr-2004
///      The methods of the base class, SvxSensor, are used for the
///      Local->Global transformations of the cluster characteristics.
/// Modified by V. L. Rykov: 15-May-2004
///      Sorted hit containers and fast index searches are expolited.
/// Modified by Alex Shaver: 11 Aug 2011
///      Rewrote cluster finding to deal with pathological cases
/// Modified by D. McGlinchey: 11 Jan 2016
///      Removed addition of random noise in AddNoise()
// =============================================================================

#include <SvxStrip11v2.h>

#include <algorithm>

#include <SvxCommon.h>
#include <iostream>

ClassImp(SvxStrip11v2)

// Instumented channel cut-off_set flag
bool SvxStrip11v2::cut_off_set_flag = false; // true if chanCutOff[][] set

/// Default constructor. Determines which parts of the sensor are instrumented.
/// 
/// \param[in] sc section
/// \param[in] lr layer 
/// \param[in] ld ladder
/// \param[in] sn sensor
///
SvxStrip11v2::SvxStrip11v2(const int sc,
			   const int lr,
			   const int ld,
			   const int sn)
: SvxStripixel<11>::SvxStripixel(sc, lr, ld, sn) {

  // by default the insturmented channel cut-off is not set
  /// \todo check if the chanCutOff is used anywhere outside of this code
  if ( !cut_off_set_flag ) {

    // loop over inherited storage
    for (unsigned int i=0; i<nSection; ++i) {
      for (unsigned int j=0; j<nReadout; ++j) {
        if(senSec[i][j]==NULL){
          std::cerr<<"SvxStrip11v2::ctor no template idx is available"<<std::endl;
          continue;
        }

        int area_cutoff = senSec[i][j]->get_nXpitch() +
	  zSlope[i][j]*(senSec[i][j]->get_nZpitch() - 1);
        int inst_cutoff = chanUplim[i][j] - chanOffs[i][j];

        if (inst_cutoff < area_cutoff - 1) {
          chanCutOff[i][j] = inst_cutoff - zSlope[i][j];
        } else {
          chanCutOff[i][j] = area_cutoff;
        }
      } // end readout loop
    } // end section loop

    // now the insturmented limits are set
    cut_off_set_flag = true;
  } // end if limits not set

  // initialize noise hit settings
  m_sNOISE             = 0.0;
  m_ADCThresholdZS     = 0;

  // initialize clustering settings
  m_adc_min_rawhit     = 0;
  m_adc_min_rawhit_sum = 0;

  return;
}

/// Stripixel clustering algorithm. Groups neighboring hits in X and U and then
/// produces spacial points from the crossings.
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
int SvxStrip11v2::findClusters(SvxRawhitList* rawlist,
			       SvxClusterList* clslist,
			       SvxRawhitClusterList* raw2cls,
			       SvxGhitRawhitList* ght2raw,
			       SvxGhitClusterList* ght2cls,
			       svxAddress* SvxAddressObject) {

  // create storage for neighboring hit channels
  RawhitGroupList_t** GroupList = new RawhitGroupList_t*[nSection];
  for (unsigned int i=0; i<nSection; ++i) {
    GroupList[i] = new RawhitGroupList_t[nReadout];
  }

  // create storage for cluster properties
  GroupDataList_t** GroupDataList = new GroupDataList_t*[nSection];
  for (unsigned int i=0; i<nSection; ++i) {
    GroupDataList[i] = new GroupDataList_t[nReadout];
  }

  // group neighboring hits
  makeGrouphits(GroupList, GroupDataList);

  // cross groupings to produce clusters
  int nClusters = makeMosaic(GroupList, 
			     GroupDataList, 
			     clslist, 
			     raw2cls, 
			     ght2raw, 
			     ght2cls, 
			     SvxAddressObject);

  // clean-up on the fly storage
  for (unsigned int i=0; i<nSection; ++i) {
    delete [] GroupList[i]; 
    delete [] GroupDataList[i];
  }
  delete [] GroupList;
  delete [] GroupDataList;

  // return cluster count
  return nClusters;
}

/// Artificailly inserts noise hits to the stripixel sensor.
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
int SvxStrip11v2::AddNoise(const int rhit_first, 
			   const int rhit_last, 
			   SvxRawhitList* rawlist, 
			   SvxGhitRawhitList* g2raw, 
			   int g2rFirst) const {

  int n_g2r = g2raw->get_nGhitRawhits();
  int nhit = 0;
  int nhit_removed = 0;

  // ADC threshold for hit removal, different than hit creation!
  static const int thre_negative = 1;

  // loop over all the raw hits and add noise
  for(int iraw = rhit_first; iraw<rhit_last; ++iraw) {

    // grab a pointer to an existing hit
    SvxRawhit* raw = rawlist->get_Rawhit(iraw);
    int raw_layer  = raw->get_layer();
    int raw_ladder = raw->get_ladder();
    int raw_sensor = raw->get_sensor();
  
    // if in the right place
    if (layer==raw_layer && ladder==raw_ladder && sensor==raw_sensor) {


      // current raw hit ADC
      int raw_adc = raw->get_adc();

      // toss an ADC noise value around the current rawhit ADC
      int ADCwNOISE = (int)rnd->Gaus(raw_adc, m_sNOISE);

      // if adc is now below threshold (ADC = 1)
      // remove this hit
      if(ADCwNOISE < thre_negative) {
        

	// remove hit from container
        int rawid = raw->get_hitID();
        rawlist->removeRawhit(iraw);
	
        // invalidate the rawhit references in the g2raw list
	// at least this would throw a segfault sometimes on real data
        for (int i=g2rFirst; i<n_g2r; ++i) {
          SvxGhitRawhit* g2r = g2raw->get_GhitRawhit(i);
          if (g2r->get_rawhitID() == rawid) g2r->set_rawhitID(-1);
        }

	// count hit in removed
        ++nhit_removed;

      } else {

	// cap ADC at saturation
        if (ADCwNOISE > adctop) ADCwNOISE = adctop;

	// update ADC
        raw -> set_adc( ADCwNOISE );

      }
    } else {
      std::cerr << "SvxStrip11v2::AddNoise() - It must be true. "
		<< "Find & fix a problem." << std::endl;
      exit(1); // stop everything.
    } // if hit is in correct location
  } // loop over all hits
  
  // clear up ghit-rawhit relationships if the hit was removed
  if (nhit_removed>0) {
    for (int i=g2rFirst; i<n_g2r; ++i) {
      SvxGhitRawhit* g2r = g2raw->get_GhitRawhit(i);
      if (!g2r) continue;

      // kill the invalidated entries
      if(g2r->get_rawhitID()<0) {
        g2raw->removeGhitRawhit(i);
      }
    }

    // release storage of removed hits
    rawlist->Compress();
    g2raw->Compress();
  }

  // D. McGlinchey 11 Jan 2016: Remove the addition of new noise hits.
  //   This had problems with adding noise to existing hits, and should
  //   really be its own function. Deemed unnecessary for analysis efforts 
  //   since we can embed in real data events.

  // return net number of hits added
  return (nhit - nhit_removed);
}

/// This method creates a list of neighboring hit strips groups.
///
/// \param[out] GroupList a vector of elements RawhitGroup;
///               is itself used as a 2D array [sec][readout]
/// \param[out] GroupDataList a vector of elements GroupData_t;
///               is itself used as a 2D array [sec][readout]
///
/// \return zero
///
int SvxStrip11v2::makeGrouphits(RawhitGroupList_t** GroupList, 
                                GroupDataList_t** GroupDataList) {

  for (unsigned int isec=0; isec<nSection; ++isec) {
    for (unsigned int irout=0; irout<nReadout; ++irout) {

///////
      //--int layer=-1, ladder=-1;
      //--{
      //--  SvxPixStructure* sensor = senSec[isec][irout];
      //--  if(sensor->get_nRawptr()!=0) {
      //--    SvxRawhit* rhit = sensor->get_rawPtr(0);
      //--    layer  = rhit->get_layer();
      //--    ladder = rhit->get_ladder();
      //--  }
      //--}
///////

      // start a new group of hits
      float groupindex = 0.0;
      RawhitGroup_t    group;
      std::vector<int> group_wrapup;

      // go into hit collection
      while (CollectNeighborRawhits(isec, irout, group, group_wrapup)) {
	// return from hit grouping with a new group
//--        std::cout<<"group : "<<group.size()<<std::endl;
//--        for(unsigned int ihit=0; ihit<group.size(); ihit++){
//--          std::cout<<"  "<<ihit<<" "<<group.at(ihit)->get_channel()<<" "<<group_wrapup[ihit]<<std::endl;
//--        }

	// loop over the collected strips and calculate group properties
	float ch_high   = -9999; // highest hit strip
	float ch_low    =  9999; // lowest hit strip
	short edge_flag =     0; // bit pattern flags groups on the edge of a sensor
	float adc_sum   =     0; // ADC sum of the strips
	float ch_mean   =     0; // average ADC weighted group position
	for (unsigned int i=0; i<group.size(); ++i) {
          float ch  = (float)group.at(i)->get_channel()-chanOffs[isec][irout];
          float adc = (float)group.at(i)->get_adc();

          if(irout==1) { // only Ustrip
            int wrapupflag = group_wrapup.at(i);
            if(wrapupflag!=0) ch += ( wrapupflag * (chanUplim[isec][irout]+1) );
          }

          if (ch > ch_high) ch_high = ch;
          if (ch < ch_low)  ch_low  = ch;

          ch_mean += ch*adc;
          adc_sum += adc;

//--          std::cout<<"  ch:"<<ch<<" "<<ch_mean<<std::endl;
        }
        ch_mean /= adc_sum;

        // for u strip, shift the average channel if wrapping up
        if(irout==1){
          ch_mean = shiftChannel(isec, irout, ch_mean);
          ch_high = shiftChannel(isec, irout, ch_high);
          ch_low  = shiftChannel(isec, irout, ch_low);
        }
        //--std::cout<<"ch_mean "<<ch_mean<<std::endl;

	// calculate edge bit pattern
	edge_flag = ((short)ch_high < chanCutOff[isec][irout]) ? 0 : 16;
	if (ch_low == 0) edge_flag |= (1+3*isec);
	if (ch_high == chanUplim[isec][irout]) edge_flag |= (1+6*isec);

	// check ADC sum of group
        if (adc_sum >= m_adc_min_rawhit_sum) {

	  // add group to output
          GroupList[isec][irout].push_back(group);

	  // create a new group properties object
	  GroupData_t GroupData;
          GroupData.push_back( groupindex          );
          GroupData.push_back( ch_mean             );
          GroupData.push_back( adc_sum             );
          GroupData.push_back( (float)group.size() );
          GroupData.push_back( ch_high             );
          GroupData.push_back( ch_low              );
          GroupData.push_back( (float)edge_flag    );

          //--std::cout<<"rawgroup : "<<layer<<" "<<ladder<<" "<<isec<<" "<<irout<<" : "<<ch_mean<<std::endl;

	  // add group properites to output
          GroupDataList[isec][irout].push_back(GroupData);

	  // increment to next group index
          ++groupindex;
        } // if group part of output

	// clear storage for new search
        group.clear();
      } // back to new group search
    } // all readouts
  } // all sections

  return 0;
}

/// This method produces a group of neighboring hit strips. Can be called
/// recursively via the 4th argument which is a pointer to a hit to add to 
/// the group. If no pointer is passed, the algorith will just use the
/// first one available.
///
/// \param[in] section sensor section location
/// \param[in] readout sensor readout location
/// \param[out] group a group to which raw hits are added.
/// \param[out] additinal info of a group for which raw hits are added with wrraping up/down.
/// \param[in] rhit a starting point hit if null, first hit is used
///
/// \return true if at least one raw hit is added to the group.
///
bool SvxStrip11v2::CollectNeighborRawhits(int section, 
					  int readout, 
					  RawhitGroup_t& group, 
					  std::vector<int>& group_wrapup, 
					  SvxRawhit* rhit, 
                                          int edgeflag) {

  // get curent inherited structure for this part of the detector
  SvxPixStructure* sensor = senSec[section][readout];
  //std::cout<<"  collect nrawhit : "<<sensor->get_nRawptr()<<" : "<<section<<" "<<readout<<std::endl;

  // if there are no hits, then bail
  if (sensor->get_nRawptr()==0) return false;

  // if no hit get first one
  if (!rhit) rhit = sensor->get_rawPtr(0);

  /// \todo removed hit from list does this impact other code?
  sensor->removeRawhit(rhit);

  /// \todo SEVERE no hot/dead flag or threshold used in clustering 
  /// verify this is the intended behavior!
  // if (rhit->get_HotDeadFlag() == 0 && rhit->get_ThreshDiff() > 0) { 

  // add to group
  group.push_back(rhit);
  group_wrapup.push_back(edgeflag);

  // there is no check for u-strip when edge of ustrip

  // call grouping up and down one channel...
  for (int dch = -1; dch <= 1; dch += 2) {

    // look aside
    int chan = rhit->get_channel() + dch;

    int edgeflip = 0;

    // check for out-of-bounds and wrap-around channels channels
    if (chan < 0 || chan > chanUplim[section][readout]) {
      if (readout == 0) { // x-strips don't wrap-around
	continue;
      } else { // u-strips do wrap-around 
	// u-strip, chan=0~28 cover chan=384~412
	if (chan < 0) { chan = chanUplim[section][readout]; edgeflip = -1;}
	else          { chan = 0;                           edgeflip =  1;}
      }
    }

    //-- std::cout<<"   collect : "<<chan<<" "<<rhit->get_channel()<<" "<<edgeflip;

    // loop over all remaining hits
    for (unsigned int i_rhit=0; i_rhit<sensor->get_nRawptr(); ++i_rhit) {
      
      // get a hit pointer
      SvxRawhit* rhit_tmp = sensor->get_rawPtr(i_rhit);
      
      // if on a neighboring channel
      if (rhit_tmp->get_channel() == chan) {

        if(edgeflag==0) edgeflag = edgeflip;
        else if(edgeflag==-1){
          if(dch==-1) edgeflag = -1; // shouldn't happen
          if(dch== 1) edgeflag =  0; // keep
        }
        else {
          if(dch==-1) edgeflag =  0; // keep
          if(dch== 1) edgeflag =  1; // shouldn't happen
        }

	// recursively add to this group
        //-- std::cout<<" go next"<<std::endl;
	CollectNeighborRawhits(section, readout, group, group_wrapup, rhit_tmp, edgeflag);
	break; // leaves the other side of the hit for the +dch part of the loop
      }
    }
    //-- std::cout<<std::endl;
  } // look left and right

  //} // removed hot/dead map & threshold check

  return true;
}

/// This method produces a set of spacial point clusters from the overlaps
/// of the groups of neighboring hit strips. CAUTION: This mosaic builder 
/// is strictly for the "conventional" stripixel sensor:
///   1) nReadout = 2
///   2) 2 pairs of IDENTICAL sections: secTYPE[i][0] == secTYPE[i][1]
///   3) xSlope[i][0] =    xSlope[i][1] = 1
///   4) zSlope[i][0] = 0; zSlope[i][1] = 1
///
/// \param[in] GroupList set of grouped hits
/// \param[in] GroupDataList properties of the grouped hits
/// \param[in,out] clslist cluster container
/// \param[in,out] raw2cls rawhit to cluster ancestry container
/// \param[in,out] ght2raw ghit to rawhit ancestry container
/// \param[in,out] ght2cls ghit to cluster ancestry container
/// \param[in] SvxAddressObject
///
/// \return number of clusters added to container
///
int SvxStrip11v2::makeMosaic(RawhitGroupList_t**   GroupList,
                             GroupDataList_t**     GroupDataList,
                             SvxClusterList*       clslist,
                             SvxRawhitClusterList* raw2cls,
                             SvxGhitRawhitList*    ght2raw,
                             SvxGhitClusterList*   ght2cls,
                             svxAddress* SvxAddressObject) {

  // get the beginning of the free area in clslist
  int firstCls = clslist->get_nClusters();

  // create X-U match dictionary, section boundary dictionary
  // uses dictionary to store section, index of x group, index of u group
  MatchList_t MatchList; 

  // uses dictionary to store readout (x=0,u=1), index of group, 
  // number of intersections along that group; a boundary between sections 
  // is stored separately
  AmbiguousList_t AmbiguousList;

  // boundary for the ambiguous list between sections
  int section_boundary;

  // uses dictionary like match_list. Stores intersections that lie near 
  // boundary between sections.
  //--SectionEdgeList_t SectionEdgeList;

  // each sensor is divided left and right
  for (short isec=0; isec<2; ++isec) {
    // number of column in a sensorSection
    int nZpitch = senSec[isec][0]->get_nZpitch();
    
    // find all groups with readout index B that match group with index A
    // readout index 0 = X
    // readout index 1 = U
    {
      short A=0; // for x-strip
      short B=1; // for u-strip
      short matchcount = 0;

      // loop over all X (or U) groupings
      for (short igrpA=0; 
	   igrpA<(short)GroupDataList[isec][A].size(); 
	   ++igrpA) {

	// select an X (or U) group
	GroupData_t grp_A = GroupDataList[isec][A].at(igrpA);

	// pick other readout group set for set B.
	//unsigned int B = (A) ? 0 : 1;
	for (unsigned int igrpB=0; 
	     igrpB<GroupDataList[isec][B].size(); 
	     ++igrpB) {

	  // get a U (or X) group
	  GroupData_t grp_B = GroupDataList[isec][B].at(igrpB);

	  // find the mean postition of both groups
	  float Amean = grp_A[1];
	  float Bmean = grp_B[1];

	  // take into account U strips at sensor edge cover both edges
	  //	  bool edgeflip = false;
	  // within wrap-around region
	  if (Amean > (chanUplim[isec][A]-nZpitch+1) && Bmean < (nZpitch)) {
	    //	    edgeflip=true;
	    Bmean +=chanUplim[isec][B]+1;
	  } else if (Amean < (nZpitch) && Bmean > (chanUplim[isec][B]-nZpitch+1)) {
	    //	    edgeflip=true;
	    Bmean -=chanUplim[isec][B]+1;
	  }
	  
	  /// \todo i challenge anyone to follow this logic quickly, simplify
	  // but it looks like a minimum separation test, don't know
	  // what the cast-crazy fudge factor is for (adds or subtracts a half
	  // width
//	  std::cout<<"   mosaic : "<<Bmean<<" "<<Amean<<" "<<(pow(-1,(float)((bool)A^(bool)isec)) *0.5*(nZpitch-1))<<" ";
//	  std::cout<<" ("<<pow(-1,(float)((bool)A^(bool)isec))<<","<< 0.5*(nZpitch-1)<<") ";
//	  std::cout<<fabs(Bmean-Amean+(pow(-1,(float)((bool)A^(bool)isec))*0.5*(nZpitch-1)))<<std::endl; 

          float ux_diff = ((A^isec)==0 ? -1.0 : 1.0) * (Bmean - Amean);

	  //--std::cout<<"   mosaic : "<<Bmean<<" "<<Amean<<" : "<<ux_diff<<" : "<<isec<<" "<<A<<" flip:"<<edgeflip<<std::endl;
          if ( -0.5<= ux_diff && ux_diff< 29.5 ) 
	  {

	    ++matchcount;

	    // only A=0=X & B=1=U crossings produce clusters
            {
	      dict_t MatchEntry;
	      MatchEntry.push_back(isec);
	      MatchEntry.push_back(igrpA);
	      MatchEntry.push_back(igrpB);
	      MatchList.push_back(MatchEntry);

	      //--float b = (isec^A) ? grp_B[5] : grp_B[4]; // 4=ch_high, 5=ch_low
	      //--float a = (isec^A) ? grp_A[4] : grp_A[5]; // 4=ch_high, 5=ch_low
	      //--if (edgeflip) {
	      //--  if (A) b += pow(-1,(float)A)*(chanUplim[isec][B]-1);
	      //--  else   a -= pow(-1,(float)A)*(chanUplim[isec][A]-1);
	      //--}
	      //--/// \todo simplify this logic statement
	      //--if (B && 
	      //--    b == a + (float)(!(bool)isec)*(pow(-1,(float)A))*(nZpitch-1)) {
	      //--  SectionEdgeList.push_back(MatchEntry);
	      //--}
	    } // add to MatchList, SectionEdgeList.

	  } // matched commands
	} // loop over B groups

	// records the number of clusters produced by this channel
	// probably the reason A & B need to be looped over
	dict_t AmbigEntry;
	AmbigEntry.push_back(A);
	AmbigEntry.push_back(igrpA);
	AmbigEntry.push_back(matchcount);
	if (matchcount>1) AmbiguousList.push_back(AmbigEntry);

      }//loop over A groups
    }//loop over choices of A

    section_boundary = AmbiguousList.size();
  }//loop over section
  
  // create clusters from matching dictionaries.
  for (int i=0; i<(int)MatchList.size(); ++i) {
    short isec  = MatchList[i][0]; // sensor side
    short igrpx = MatchList[i][1]; // x-grouping index
    short igrpu = MatchList[i][2]; // u-grouping index
    
    // number of pixels in a row - unused
    int   nXpitch    = senSec[isec][0]->get_nXpitch();
    // number of column in a sensorSection    
    int   nZpitch    = senSec[isec][0]->get_nZpitch();
    // pixel length in x-direction
    float xPitch     = senSec[isec][0]->get_xPitch();
    // pixel length in z-direction
    float zPitch     = senSec[isec][0]->get_zPitch();
    // 0.5*nXpitch*xPitch
    float xhalfWidth = senSec[isec][0]->get_xhalfWidth();
    // 0.5*nZpitch*zPitch
    float zhalfWidth = senSec[isec][0]->get_zhalfWidth();
    
    // z-positions of senSec[isec][0] edges in senSec[isec][1] frame
    //--float z0min = secZpos[isec][0] - secZpos[isec][1];
    //--if (zcntRvrs[isec][1]) z0min = -z0min;
    
    // grap the two group objects
    GroupData_t grp_x = GroupDataList[isec][0].at(igrpx);
    GroupData_t grp_u = GroupDataList[isec][1].at(igrpu);
    
    // ADC weighted average channel positions
    float ch_x_mean = grp_x[1];
    float ch_u_mean = grp_u[1];
    
    // diagram of the stripixel channel numbering scheme.
    //
    //               isec = 0               isec = 1
    //
    // x = 383 |---------------------|---------------------|
    //  ...    |                     |/ u=28            / /|
    //         |                     |                 / / |
    //         |                     |                / / /|
    //         |                     |               / / / |
    //         |                     |         u = 0  / /  |
    //         |                     |        u = 383  /   |
    //         |                     |                /    |
    //         |                     |          u = 382    |
    //         |                     |                     |
    //         | u = 1               |                     |
    //         |   /  u = 0          |                     |
    //         |  / /                |                     |
    //         | / / / u = 383       |                     |
    //  ...    |/ / /                |                     |
    // x = 2   | / /         u = 355 |                     |
    // x = 1   |/ /                 /|                     |
    // x = 0   |---------------------|---------------------|
    //          z=0,1,2...       ...29 z=0,1,2...       ...29
      
    // Since the readout in x and u is square in number of channels
    // the difference between the x and u channel number can be used
    // to indicate the fictious z channel number. Of course, as the
    // wrap around gives larger differences, some constant shifts need
    // to be applied in that area. And since the numbering is done somewhat
    // differently on the right half of the chip, we need a different
    // set of constants there too. -MPM

    // z-position given where the center of the u-hit group crosses "x"
    float ch_z_mean = -9999.9;

    if (isec == 0) { // left side of sensor
      // identify the wrap around region
      // ch_x_mean < 29 and ch_u_mean > 354
      if ((ch_x_mean < nZpitch-0.5) && (ch_u_mean > nXpitch - (nZpitch-0.5))) { 
	// within wrap around region
	ch_z_mean = ch_x_mean - ch_u_mean + (float)nXpitch;
      } else if ((ch_x_mean > nXpitch -1) && (ch_u_mean < 0.0)) { 
	ch_z_mean = ch_x_mean - ch_u_mean - (float)nXpitch;
      } else {
	ch_z_mean = ch_x_mean - ch_u_mean;
      }
    } else if (isec == 1) { // right side of sensor
      // identify the wrap around region
      // ch_x_mean > 354 and ch_u_mean < 29
      if ( (ch_x_mean > nXpitch - (nZpitch-0.5))  && (ch_u_mean < nZpitch-0.5) ) {
	// within wrap around region
	ch_z_mean = ch_x_mean - ch_u_mean - (float)(nXpitch - nZpitch + 1);
      } else {
	// separation gives the channel number
	ch_z_mean = ch_x_mean - ch_u_mean + (float)(nZpitch - 1);
      }
    } else {
      // isn't called unless the section is set improperly
      // then skips cluster generation
      continue;
    }
    // additional check
    bool add_wrap = false;
    if(ch_z_mean<-0.5){ ch_z_mean+=nXpitch; add_wrap=true;}
    if(ch_z_mean>29.5){ ch_z_mean-=nXpitch; add_wrap=true;}

    if(add_wrap) std::cout<<"additional wrap z "<<ch_z_mean<<" ch_x, u="<<ch_x_mean<<" "<<ch_u_mean<<" : "<<isec<<std::endl;
	
    int edgeXflag = grp_x[6];
    int edgeUflag = grp_u[6];
    
    // position relative to section mid-point
    float x = xPitch * (ch_x_mean + 0.5) - xhalfWidth; 
    float z = zPitch * (ch_z_mean + 0.5) - zhalfWidth;

    // range check with 0.1um allowance
    bool rangeOK = ( fabs(x)<=xhalfWidth+0.00001 && fabs(z)<=zhalfWidth+0.00001 );

    // sensor frame coordinates
    x += secXpos[isec][0]; // + 0.0 cm
    z += secZpos[isec][1]; // + {-1.5,1.5} cm


    // local coordinate of the cluster
    double xl[3];
    xl[0] = x; // lx 
    xl[1] = 0; // ly
    xl[2] = z; // lz

    // Get ambiguous flag
    short ambiguous = 0;
    for (unsigned int j = (0+isec*section_boundary);
	 j < (section_boundary + (isec*(AmbiguousList.size()-section_boundary))); 
	 ++j) {
      if ((AmbiguousList[j][0]==0 && AmbiguousList[j][1]==igrpx)
	  || (AmbiguousList[j][0]==1 && AmbiguousList[j][1]==igrpu)) {
	ambiguous+=AmbiguousList[j][2];
      }
    }

    // \todo merge cluster data across sensor sections.
    // Not yet implemented 8.11.11 - Alex Shaver
    
    // create the new cluster
    short sx = (short) grp_x[3];
    short su = (short) grp_u[3];
    SvxCluster* cluster = clslist->addCluster();
    cluster->set_svxSection(svxSection);
    cluster->set_layer(layer);
    cluster->set_ladder(ladder);
    cluster->set_sensor(sensor);
    cluster->set_sensorType(sensorType);
    cluster->set_edgeflag(edgeXflag | edgeUflag);
    cluster->set_adc(0, grp_x[2]);
    cluster->set_adc(1, grp_u[2]);
    cluster->set_size(sx*su);
    cluster->set_xz_size(0, sx);
    cluster->set_xz_size(1, su);
    for (int i = 0; i < 3; ++i) {
      cluster->set_xyz_local(i, xl[i]);
    }
    double xg[3];
    position_local2global(xl, xg);
    for (int i = 0; i < 3; ++i) {
      cluster->set_xyz_global(i, xg[i]);
    }
    cluster->set_ambiguous(ambiguous);

    if(!rangeOK){
      std::cerr<<"ClusterPosition out of range : "<<xl[0]<<" "<<xl[2]<<std::endl;
      std::cerr<<"  "<<ch_x_mean<<" "<<ch_u_mean<<" "<<ch_z_mean<<" sec="<<isec<<std::endl;
    }

      
    //-------------------------------------
    // if ancestry object exists add to it, but otherwise just skip this part 
    if (!raw2cls) continue;

    RawhitGroup_t* rgrp_x = &GroupList[isec][0].at(grp_x[0]);
    RawhitGroup_t* rgrp_u = &GroupList[isec][1].at(grp_u[0]);
      
    int clus_id = cluster->get_hitID();
    
    // create Rawhit<->Cluster relation   
    std::vector<int> rawhitID_list;
    for (unsigned int i = 0; i < grp_x[3]; ++i) {
      SvxRawhitCluster* r2c = raw2cls->addRawhitCluster();
      r2c->set_clusterID(clus_id);
      int id_raw = rgrp_x->at(i)->get_hitID();
      r2c->set_rawhitID(id_raw);
      rawhitID_list.push_back(id_raw);
    }
    for (unsigned int i = 0; i < grp_u[3]; ++i) {
      SvxRawhitCluster* r2c = raw2cls->addRawhitCluster();
      r2c->set_clusterID(clus_id);
      int id_raw = rgrp_u->at(i)->get_hitID();
      r2c->set_rawhitID(id_raw);
      rawhitID_list.push_back(id_raw);
    }

    // create Ghit<->Cluster relation   
    if (ght2cls && ght2raw) {
      std::vector<int> g_id;
      for (unsigned int iraw=0; iraw<rawhitID_list.size(); ++iraw) {
	// make a list of ghit id
	int id_raw = rawhitID_list[iraw];
	int g2r_first = 0;
	int g2r_last = 0;
	ght2raw->indexOfRawhit(id_raw, g2r_first, g2r_last);
	for ( int ig2r=g2r_first; ig2r<=g2r_last; ig2r++ ) {
	  SvxGhitRawhit* g2r = ght2raw->get_GhitRawhit(ig2r);
	  int id_ghit = g2r->get_ghitID();
	  // if not already stored locally, then add
	  if (find(g_id.begin(), g_id.end(), id_ghit) == g_id.end()) {
	    g_id.push_back(id_ghit);
	  } // if : find()
	} // for : ig2r 
      } // for : iraw
      for (unsigned int ighit=0; ighit<g_id.size(); ++ighit) {
	// fill in ght2cls
	SvxGhitCluster* g2c = ght2cls->addGhitCluster();
	g2c->set_clusterID(clus_id);
	g2c->set_ghitID(g_id.at(ighit));
      } // for : ighit
    } // if : ght2cls && ght2raw
  }

  return clslist->get_nClusters() - firstCls;
}

/// This method adds a hit to storage
///
/// \param[in] rawhit pointer to hit to be added to the sensor
///
void SvxStrip11v2::add_rawhit(SvxRawhit *rawhit) {

  int section = rawhit->get_sensorSection();
  int readout = rawhit->get_sensorReadout();
  int adc = rawhit->get_adc();

  if ( adc>=m_adc_min_rawhit ) {
    senSec[section][readout]->addRawhit(rawhit);
  }

  return;
}

/// shift the channel after averaging if the averaged channel is out of range
///
/// \param[in] section num=0,1
/// \param[in] readout num=0,1
/// \param[in] averaged channel
///
/// \return averaged channel after shifting so that the channel is within the range.
///
float SvxStrip11v2::shiftChannel(int section, int readout, float channel)
{
  int chanLim = chanUplim[section][readout] +1;

  float channel_out = channel;
  //--if(     channel < 0)        channel_out += chanLim;
  //--else if(channel >= chanLim) channel_out -= chanLim;
  if(     channel < -0.5)           channel_out += chanLim;
  else if(channel >= (chanLim-0.5)) channel_out -= chanLim;

  return channel_out;
}

/// test colection of neighbor rawhits
///
#include "SvxRawhitv4.h"
void SvxStrip11v2::test_CollectNeighborRawhits(){
  std::cout<<std::endl;
  std::cout<<"test_CollectNeighborRawhits begins"<<std::endl;

  bool result = true;

  //////////////////////////////
  std::vector< std::vector<int> > vAnswer(2);
  { //  Test 1
    int rawhitAry[] = {382,383,0,1,2};
    // X strip test
    int iro = 0;  
    vAnswer.resize(2);
    {
      vAnswer[0].assign(&rawhitAry[0], (&rawhitAry[0])+2);
      vAnswer[1].assign(&rawhitAry[2], (&rawhitAry[2])+3);
    }
    result &= testSingle_CollectNeighborRawhits(iro, sizeof(rawhitAry)/sizeof(int), rawhitAry, vAnswer);
    // U strip test
    iro = 1; 
    vAnswer.clear();
    vAnswer.resize(1);
    {
      vAnswer[0].assign(&rawhitAry[0], (&rawhitAry[0])+5);
    }
    result &= testSingle_CollectNeighborRawhits(iro, sizeof(rawhitAry)/sizeof(int), rawhitAry, vAnswer);
  }

  //////////////////////////////
  { //  Test 2
    int rawhitAry[] = {0,1,2,3,4};
    // X strip test
    int iro = 0;  
    vAnswer.clear();
    vAnswer.resize(1);
    {
      vAnswer[0].assign(&rawhitAry[0], (&rawhitAry[0])+5);
    }
    result &= testSingle_CollectNeighborRawhits(iro, sizeof(rawhitAry)/sizeof(int), rawhitAry, vAnswer);
    // U strip test
    iro = 1; 
    vAnswer.clear();
    vAnswer.resize(1);
    {
      vAnswer[0].assign(&rawhitAry[0], (&rawhitAry[0])+5);
    }
    result &= testSingle_CollectNeighborRawhits(iro, sizeof(rawhitAry)/sizeof(int), rawhitAry, vAnswer);
  }

  //////////////////////////////
  { //  Test 3
    int rawhitAry[] = {0,1,2,383,382,3,4};

    // X strip test
    int iro = 0;  
    vAnswer.clear();
    vAnswer.resize(2);
    {
      vAnswer[0].assign(&rawhitAry[0], (&rawhitAry[0])+3);
      vAnswer[0].push_back(rawhitAry[5]);
      vAnswer[0].push_back(rawhitAry[6]);
      vAnswer[1].assign(&rawhitAry[3], (&rawhitAry[3])+2);
    }
    result &= testSingle_CollectNeighborRawhits(iro, sizeof(rawhitAry)/sizeof(int), rawhitAry, vAnswer);
    // U strip test
    iro = 1; 
    vAnswer.clear();
    vAnswer.resize(1);
    {
      vAnswer[0].assign(&rawhitAry[0], (&rawhitAry[0])+7);
    }
    result &= testSingle_CollectNeighborRawhits(iro, sizeof(rawhitAry)/sizeof(int), rawhitAry, vAnswer);
  }

  // ----------- result -----------------------
  std::cout<<" test_CollectNeighborRawhit : result = "<<(result ? "Pass" : "Fail")<<std::endl;
}

/// test single colection of neighbor rawhits
///
/// \param[in] iro readout flag 0=X, 1=U
/// \param[in] rawSize raw cluster size
/// \param[in] rawhitAry raw hit ary
/// \param[in] vAnswer
///
bool SvxStrip11v2::testSingle_CollectNeighborRawhits(int iro, int rawSize, int* rawhitAry, std::vector< std::vector<int> >& vAnswer)
{

  std::cout<<" testSingle_CollectNeighborRawhits :"<<std::endl;
  std::cout<<"     input iro : "<<iro<<"("<<(iro==0?"X":"U")<<")"<<", ch : "<<std::flush;

  std::vector<SvxRawhit*> vRawhit;
  int isen=0;
  {
    for(int ich=0; ich<rawSize; ich++){
      SvxRawhit *rawhit = new SvxRawhitv4();
      rawhit->set_sensorSection(isen);
      rawhit->set_sensorReadout(iro);
      rawhit->set_adc          (10);
      rawhit->set_channel      (rawhitAry[ich]);
      
      senSec[isen][iro]->addRawhit(rawhit);
      vRawhit.push_back(rawhit);
      std::cout<<rawhitAry[ich]<<" "<<std::flush;
    }
  }
  std::cout<<std::endl;

  RawhitGroup_t group;
  std::vector<int> group_wrapup;

  bool result = true;
  {
    int igroup=0;
    while(CollectNeighborRawhits(isen, iro, 
                                 group, 
                                 group_wrapup) )
    {
      bool hitlistOK=true;

      //std::cout<<"    group : "<<group.size()<<std::endl;
      for(unsigned int ihit=0; ihit<group.size(); ihit++){
        //std::cout<<"       "<<ihit<<" "<<group.at(ihit)->get_channel()<<" "<<group_wrapup[ihit]<<std::endl;
        
        bool hitOK=false;
        for(unsigned int ians=0; ians<vAnswer[igroup].size(); ians++){
          if(vAnswer[igroup][ians]==group.at(ihit)->get_channel()) {
            hitOK=true;
            break;
          }
        }
        hitlistOK &= hitOK;
      }
      std::cout<<"            All hits tested : "<<(hitlistOK ? "Pass" : " Fail")<<std::endl;

      result &= hitlistOK;

      group.clear();
      group_wrapup.clear();
      igroup++;
    }
  }

  for(std::vector<SvxRawhit*>::iterator itr=vRawhit.begin(); itr!=vRawhit.end(); ++itr){
    SvxRawhit *rawhit = *itr;
    if(rawhit) delete rawhit;
  }
  vRawhit.clear();

  return result;
}

/// test make group list
///
void SvxStrip11v2::test_makeGrouphits(){
  std::cout<<std::endl;
  std::cout<<"test_makeGrouphits begins"<<std::endl;

  bool result = true;

  /////////////////////
  //----------------------
  // Test1
  {
    int rawhitAry[] = {382,383,0,1,2}; // input ary
    float mean_ch_x[2] = {382.5, 1.0}; // result for x
    float mean_ch_u[1] = {0};          // result for u
    // for X
    result &= testSingle_makeGrouphits(0, sizeof(rawhitAry)/sizeof(int), rawhitAry, 
                                          sizeof(mean_ch_x)/sizeof(float), mean_ch_x);
    // for U
    result &= testSingle_makeGrouphits(1, sizeof(rawhitAry)/sizeof(int), rawhitAry, 
                                          sizeof(mean_ch_u)/sizeof(float), mean_ch_u);
  }
 
  //----------------------
  // Test2
  {
    int rawhitAry[] = {0,1,2,3,4}; // input ary
    float mean_ch_x[1] = {2.0};    // result for x
    float mean_ch_u[1] = {2.0};    // result for u
    // for X
    result &= testSingle_makeGrouphits(0, sizeof(rawhitAry)/sizeof(int), rawhitAry, 
                                          sizeof(mean_ch_x)/sizeof(float), mean_ch_x);
    // for U
    result &= testSingle_makeGrouphits(1, sizeof(rawhitAry)/sizeof(int), rawhitAry, 
                                          sizeof(mean_ch_u)/sizeof(float), mean_ch_u);
  }

  //----------------------
  // Test3

  {
    int rawhitAry[] = {0,1,2,383,382,3,4}; // input ary
    float mean_ch_x[2] = {2, 382.5};       // result for x
    float mean_ch_u[1] = {1};              // result for u
    // for X
    result &= testSingle_makeGrouphits(0, sizeof(rawhitAry)/sizeof(int), rawhitAry, 
                                          sizeof(mean_ch_x)/sizeof(float), mean_ch_x);
    // for U
    result &= testSingle_makeGrouphits(1, sizeof(rawhitAry)/sizeof(int), rawhitAry, 
                                          sizeof(mean_ch_u)/sizeof(float), mean_ch_u);
  }
  
  std::cout<<"test_makeGrouphits  result = "<<(result?"Pass":"Fail")<<std::endl;
}

/// test single make grouphits
///
/// \param[in] rawSize raw cluster size
/// \param[in] rawhitAry raw hit ary
/// \param[in] resultSize cluster size
/// \param[in] resultAry hit ary
///
/// \return true if mean group hits is OK
///
bool SvxStrip11v2::testSingle_makeGrouphits(int iro, int rawSize, int* rawhitAry, 
                                                     int resultSize, float* resultAry)
{

  /////////////////////
  // create storage for neighboring hit channels
  RawhitGroupList_t** GroupList = new RawhitGroupList_t*[nSection];
  for (unsigned int i=0; i<nSection; ++i) {
    GroupList[i] = new RawhitGroupList_t[nReadout];
  }

  // create storage for cluster properties
  GroupDataList_t** GroupDataList = new GroupDataList_t*[nSection];
  for (unsigned int i=0; i<nSection; ++i) {
    GroupDataList[i] = new GroupDataList_t[nReadout];
  }

  ////////// make rawhit as input
  std::cout<<" testSingle_makeRawhits :"<<std::endl;
  std::cout<<"     input iro : "<<iro<<"("<<(iro==0?"X":"U")<<")"<<", ch : "<<std::flush;

  int isen=0;
  std::vector<SvxRawhit*> vRawhit;
  {
    for(int ich=0; ich<rawSize; ich++){
      SvxRawhit *rawhit = new SvxRawhitv4();
      rawhit->set_sensorSection(isen);
      rawhit->set_sensorReadout(iro);
      rawhit->set_adc          (10);
      rawhit->set_channel      (rawhitAry[ich]);
      
      senSec[isen][iro]->addRawhit(rawhit);
      vRawhit.push_back(rawhit);

      std::cout<<rawhitAry[ich]<<" "<<std::flush;
    }
  }
  std::cout<<std::endl;

  // do reconstruction
  makeGrouphits(GroupList, GroupDataList);

  // check result
  bool meanlistOK=true;
  for(unsigned int idat=0; idat<GroupDataList[isen][iro].size(); idat++){
    bool meanOK = (resultAry[idat] == GroupDataList[isen][iro][idat].at(1));
    //std::cout<<" meanOK : "<<(meanOK ? "Pass": "Fail")<<std::endl;

    meanlistOK &= meanOK;
  }
  std::cout<<"     mean is : "<<(meanlistOK ? "Pass": "Fail")<<std::endl;


  ////////////////////////////////////////////////
  // delete strage
  for (std::vector<SvxRawhit*>::iterator itr=vRawhit.begin(); itr!=vRawhit.end(); ++itr){
    SvxRawhit* rawhit = *itr;
    if(rawhit) delete rawhit;
  }

  // delete strage
  for (unsigned int i=0; i<nSection; ++i) { 
    delete [] GroupList[i];
    delete [] GroupDataList[i];
  }
  delete [] GroupList;
  delete [] GroupDataList;

  return meanlistOK;
}


//#include <SvxRawhitv4.h>
#include <SvxRawhitListv4.h>
#include <SvxClusterListv5.h>
void SvxStrip11v2::test_findCluster(svxAddress* SvxAddressObject, int sec, double xpar, double zpar)
{
  SvxRawhitList*  rawhit = new SvxRawhitListv4();

  int sc=0, rd=0;
  double nx = senSec[sc][rd]->get_nXpitch   ();
  double xp = senSec[sc][rd]->get_xPitch    ();
  double xhw= senSec[sc][rd]->get_xhalfWidth();
  double nz = senSec[sc][rd]->get_nZpitch   ();
  double zp = senSec[sc][rd]->get_zPitch    ();
  double zhw= senSec[sc][rd]->get_zhalfWidth();
  std::cout<<nx<<" "<<xp<<" "<<xhw<<" : "<<nz<<" "<<zp<<" "<<zhw<<std::endl;
  

  for(int ixtest=0; ixtest<384; ixtest++)
  {
    for(int iztest=0; iztest<30; iztest++)
    {
      double zoff  = (sec==0? (-2*zhw) : 0);
      double zsign = (sec==0? -1.0 : 1.0);

      // generate dummy data
      double x = -xhw + xpar*xp + xp*ixtest;
      double z = zoff + zpar*zp + zp*iztest;

      if(fabs(x)          >xhw) { std::cout<<"x="<<x<<" out of range"<<std::endl; continue;}
      if(fabs(z-zsign*zhw)>zhw) { std::cout<<"z="<<z<<" out of range"<<std::endl; continue;}

      // init channel
      int xch = int( (x + xhw) / xp );
      int zch = int( (z - zoff)/ zp );
      int uch = (sec==0) ? xch - zch : xch - zch + 29;

      if(xch<0||384<=xch){std::cout<<"xch=="<<xch<<std::endl; continue;}

      std::vector<SvxRawhit*> vRaw;
      for(int iadd=0; iadd<1; iadd++){
        int xchraw = xch + iadd;
  
        // convert dummy data to rawhit
        if(0<=xchraw&&xchraw<=383) {
          SvxRawhit* tmp_raw_x = new SvxRawhitv4();
          tmp_raw_x->set_svxSection(0);
          tmp_raw_x->set_layer(2);
          tmp_raw_x->set_ladder(0);
          tmp_raw_x->set_sensor(0);
          tmp_raw_x->set_sensorSection(sec);
          tmp_raw_x->set_sensorReadout(0); // x
          tmp_raw_x->set_adc(50);
          tmp_raw_x->set_channel(xchraw);
          vRaw.push_back(tmp_raw_x);
          add_rawhit(tmp_raw_x);
        }
        else {std::cout<<"xch=="<<xchraw<<std::endl;}
  
     
        {
          int uchraw = uch + iadd;
          if(uchraw<0)         uchraw+=384;
          else if(uchraw>=384) uchraw-=384;
  
          SvxRawhit* tmp_raw_u = new SvxRawhitv4();
          tmp_raw_u->set_svxSection(0);
          tmp_raw_u->set_layer(2);
          tmp_raw_u->set_ladder(0);
          tmp_raw_u->set_sensor(0);
          tmp_raw_u->set_sensorSection(sec);
          tmp_raw_u->set_sensorReadout(1); // u
          tmp_raw_u->set_adc(50);
          tmp_raw_u->set_channel(uchraw);
          vRaw.push_back(tmp_raw_u);
          add_rawhit(tmp_raw_u);
        }
  
      }

      // clustering
      SvxClusterList* cluster = new SvxClusterListv5();
      int ncluster = findClusters(rawhit,
                                  cluster,
                                  NULL,
                                  NULL,
                                  NULL,
                                  SvxAddressObject);

      // check result
      double clxyz[3] = {-999, -999, -999};
      if(ncluster>0){
        SvxCluster *cls = cluster->get_Cluster(0);
        clxyz[0] = cls->get_xyz_local(0);
        clxyz[1] = cls->get_xyz_local(1);
        clxyz[2] = cls->get_xyz_local(2);
      }

      bool xcheck = fabs(x-clxyz[0])< 0.00405;
      bool zcheck = fabs(z-clxyz[2])< 0.05005 ;
      bool check = xcheck & zcheck;

      std::cout<<ixtest<<" "<<iztest<<" : "
               <<x<<" "<<z<<" : "
               <<xch<<" "<<uch<<" : "
               <<clxyz[0]<<" "<<clxyz[2]<<" : "
               <<(check ? "PASS" : "FAIL")
               <<std::endl;

      delete cluster;
      std::vector<SvxRawhit*>::iterator itr;
      for(itr=vRaw.begin(); itr!=vRaw.end(); ++itr){
        SvxRawhit* raw = *itr;
        delete raw;
      }
      vRaw.clear();
    }
  }

  delete rawhit;
}

void SvxStrip11v2::test_makeMosaic(svxAddress* SvxAddressObject)
{
  // create storage for neighboring hit channels
  RawhitGroupList_t** GroupList = new RawhitGroupList_t*[nSection];
  for (unsigned int i=0; i<nSection; ++i) {
    GroupList[i] = new RawhitGroupList_t[nReadout];
  }

  // create storage for cluster properties
  GroupDataList_t** GroupDataList = new GroupDataList_t*[nSection];
  for (unsigned int i=0; i<nSection; ++i) {
    GroupDataList[i] = new GroupDataList_t[nReadout];
  }

  {
     // input data
     //int sc=1;
     //float data_xu[2] = {0, 383.908};
     int sc=0;
     float data_xu[2] = {383, -0.5};

     float groupindex = 0;
     for(int ixu=0; ixu<2; ixu++){
       GroupData_t GroupData;
       GroupData.push_back( 0.0          );
       GroupData.push_back( data_xu[ixu] );
       GroupData.push_back( 50.          );
       GroupData.push_back( 1.0          );
       GroupData.push_back( data_xu[ixu] );
       GroupData.push_back( data_xu[ixu] );
       GroupData.push_back( 0.           );
       
       // add group properites to output
       GroupDataList[sc][ixu].push_back(GroupData);
       
     }
     // increment to next group index
     ++groupindex;
  }

  // cross groupings to produce clusters
  SvxClusterList* clslist = new SvxClusterListv5();
  makeMosaic(GroupList, 
             GroupDataList, 
             clslist, 
             NULL, 
             NULL, 
             NULL, 
             SvxAddressObject);


  // clean-up on the fly storage
  for (unsigned int i=0; i<nSection; ++i) {
    delete [] GroupList[i]; 
    delete [] GroupDataList[i];
  }
  delete [] GroupList;
  delete [] GroupDataList;

  delete clslist;

}

void SvxStrip11v2::test_makeMosaicAll(svxAddress* SvxAddressObject, int sec, int xline, double xpar, double zpar)
{
  // create storage for neighboring hit channels
  RawhitGroupList_t** GroupList = new RawhitGroupList_t*[nSection];
  for (unsigned int i=0; i<nSection; ++i) {
    GroupList[i] = new RawhitGroupList_t[nReadout];
  }

  // create storage for cluster properties
  GroupDataList_t** GroupDataList = new GroupDataList_t*[nSection];
  for (unsigned int i=0; i<nSection; ++i) { GroupDataList[i] = new GroupDataList_t[nReadout];
  }

  int sc=0, rd=0;
  double nx = senSec[sc][rd]->get_nXpitch   ();
  double xp = senSec[sc][rd]->get_xPitch    ();
  double xhw= senSec[sc][rd]->get_xhalfWidth();
  double nz = senSec[sc][rd]->get_nZpitch   ();
  double zp = senSec[sc][rd]->get_zPitch    ();
  double zhw= senSec[sc][rd]->get_zhalfWidth();
  std::cout<<nx<<" "<<xp<<" "<<xhw<<" : "<<nz<<" "<<zp<<" "<<zhw<<std::endl;

  //////////////////////////////
  //
  for(int iztest=0; iztest<30; iztest++){
    //--double xpar = 0.5, zpar =  -0.5;
    //--int sec = 0;

    double zoff  = (sec==0? (-2*zhw) : 0);
    //double zsign = (sec==0? -1.0 : 1.0);

    int ixtest = xline;
    // generate dummy data
    double x = -xhw + xpar*xp + xp*ixtest;
    double z = zoff + zpar*zp + zp*iztest;

     // init channel
    double xch = ((x + xhw) / xp) - 0.5;
    double zch = ((z - zoff)/ zp) - 0.5;
    double uch = (sec==0) ? xch - zch : xch - zch + 29.;
    if(     uch> 383.5) uch-=384.;
    else if(uch<  -0.5) uch+=384.;
    
    // input data
    //int sc=1;
    //float data_xu[2] = {0, 383.908};
    //int sc=0;
    //float data_xu[2] = {383, -0.5};
    int sc=sec;
    double data_xu[2] = {xch, uch};

    for(int ixu=0; ixu<2; ixu++){
      GroupData_t GroupData;
      GroupData.push_back( 0.0          );
      GroupData.push_back( data_xu[ixu] );
      GroupData.push_back( 50.          );
      GroupData.push_back( 1.0          );
      GroupData.push_back( data_xu[ixu] );
      GroupData.push_back( data_xu[ixu] );
      GroupData.push_back( 0.           );
      
      // add group properites to output
      GroupDataList[sc][ixu].push_back(GroupData);
      
    }

    // cross groupings to produce clusters
    SvxClusterList* clslist = new SvxClusterListv5();
    int ncluster = makeMosaic(GroupList, 
                              GroupDataList, 
                              clslist, 
                              NULL, 
                              NULL, 
                              NULL, 
                              SvxAddressObject);

    // check result
    double clxyz[3] = {-999, -999, -999};
    if(ncluster>0){
      SvxCluster *cls = clslist->get_Cluster(0);
      clxyz[0] = cls->get_xyz_local(0);
      clxyz[1] = cls->get_xyz_local(1);
      clxyz[2] = cls->get_xyz_local(2);
    }

    bool xcheck = fabs(x-clxyz[0])< 0.00405;
    bool zcheck = fabs(z-clxyz[2])< 0.05005 ;
    bool check = xcheck & zcheck;

    std::cout<<ixtest<<" "<<iztest<<" : "
             <<x<<" "<<z<<" : "
             <<xch<<" "<<zch<<" "<<uch<<" : "
             <<clxyz[0]<<" "<<clxyz[2]<<" : "
             <<(check ? "PASS" : "FAIL")
             <<std::endl;

    
    for(int ixu=0; ixu<2; ixu++){
      GroupDataList[sc][ixu].clear();
    }

    delete clslist;
  }

  //////////////////////////////////////////////

  // clean-up on the fly storage
  for (unsigned int i=0; i<nSection; ++i) {
    delete [] GroupList[i]; 
    delete [] GroupDataList[i];
  }
  delete [] GroupList;
  delete [] GroupDataList;

}
