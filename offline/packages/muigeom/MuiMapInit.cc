// $Id: MuiMapInit.cc,v 1.6 2007/04/05 08:26:53 hpereira Exp $

/*!
  \file MuiMapInit.cc
  \brief muid L1 and L2 map initialisations
  \author Jason Newby
  \version $Revision: 1.6 $
  \date $Date: 2007/04/05 08:26:53 $
*/

#include "TMuiChannelId.hh"
#include "TMuiTwoPackGeo.hh"
#include "TMuiPanelGeo.hh"
#include "TMuiGeometry.hh"
#include "TMuiAddressTable.hh"


#include <fstream>
#include <map>

using namespace std;

//___________________________________________________________
int SearchArray (int n, float array[], float key)
{
  int left = 1;
  int right = n - 1;
  int i;

  if (key >= array[n - 1]) return n;
  else if (key < array[0]) return 0;

  while (right >= left)
  {
    i = (left + right) / 2;
    if (key >= array[i - 1] && key < array[i]) return i;
    if (key < array[i - 1]) right = i - 1;
    else left = i + 1;
  }
  
  return 0;
}


//___________________________________________________________
void MuiMapInit (hashVector < TMuiChannelId, int >&map)
{

  /*
   * Arrays for sorting/storing normalized tube locations on gap 0.
   */
  float arrtwopackXoZ[TMuiChannelId::kTwoPacksPerPlaneMax];
  float idxXoZ[TMuiChannelId::kTwoPacksPerPlaneMax];
  unsigned int arrsort[TMuiChannelId::kTwoPacksPerPlaneMax];
  unsigned int arrpanel[TMuiChannelId::kTwoPacksPerPlaneMax];
  unsigned int arrtwopack[TMuiChannelId::kTwoPacksPerPlaneMax];

  /* 
   * Geometry object pointers.
   */
  TMuiGeometry *g = TMuiGeometry::Geom ();

  /*
   * Loop over arm and orientation, get one map array for each.
   */
  ofstream mapOut ("lvl2map.adb");
  ofstream ll1MapOut ("ll1map.adb");
  ofstream cosOut ("lvl2cosines.adb");

  for( unsigned int arm = 0; arm < TMuiChannelId::kArmsTotal; arm++)
  {
    for( unsigned int orient = 0; orient < TMuiChannelId::kOrientations; orient++)
    {
      /*
      * Generate a twopack index for each twopack in gap 0. 
      * This is done by generating an ordered list of twopack centers 
      * (normalized by their distance to the target) and collecting 
      * all twopacks that are within half the nominal twopack 
      * separation. The mean position of the pack index is also 
      * stored for comparison to following gaps.
      */
      
      int gap = 0;
      unsigned int ntwopack_gap = 0;
      float gap0Z = 0;
      
      /*
      * Loop over panels.
      */
      std::multimap < double, int >two_pack_center_map;
      for( unsigned int panel = 0; panel < TMuiChannelId::kPanelsPerPlane; panel++)
      {
        /*
        * Get pointer to panel geometry object.
        * Pull out # twopacks in panel, panel center location.
        */
        TMuiPanelGeo *p = g->getPanel (arm, gap, panel);
        int ntwopack_panel = p->getTwoPackCount ((EOrient_t) orient);
        
        // retrieve panel center
        float panelX, panelY, panelZ;
        p->CenterPos (panelX, panelY, panelZ);        
        float panelXY( orient == kVERT ? panelX : panelY );
        
        /*
        * Loop over twopacks.
        */
        for(int twopack = 0; twopack < ntwopack_panel; twopack++)
        {

          // twopack center location.
          float twopackX, twopackY, twopackZ;
          p->TwoPackPointer ((EOrient_t) orient, twopack)->CenterPos (twopackX, twopackY, twopackZ);
          float twopackXY( orient == kVERT ? twopackX : twopackY );

          // map center location to twopack id.
          two_pack_center_map.insert (make_pair((twopackXY + panelXY) / fabs (twopackZ + panelZ), ntwopack_gap));

          arrpanel[ntwopack_gap] = panel;
          arrtwopack[ntwopack_gap] = twopack;
          gap0Z += fabs (twopackZ + panelZ);
          ntwopack_gap += 1;
        }
      }
      gap0Z = gap0Z / (float) ntwopack_gap;
      
      /* 
      port the sorted map of center position
      and indexes in the corresponding sorted arrays
      */
      unsigned int index = 0;
      for (std::multimap < double, int >::iterator iter = two_pack_center_map.begin (); iter != two_pack_center_map.end () && index < ntwopack_gap; iter++, index++)
      {
        arrtwopackXoZ[index] = iter->first;
        arrsort[index] = iter->second;
      }

      /*
      * Collapse twopack indices where the twopack positions are within
      * half the nominal twopack width.
      */
      
      /* 
      * Initialize.
      */
      for( unsigned int idx = 0; idx < TMuiChannelId::kTwoPacksPerPlaneMax; idx++)
      { idxXoZ[idx] = 0; }
      
      unsigned int idx = 0;
      float start = arrtwopackXoZ[0];
      float sum = 0;
      unsigned int cnt = 0;
      
      /* 
      * Loop over ordered array of two-pack positions.
      */
      for( unsigned int iarr = 0; iarr < ntwopack_gap; iarr++)
      {
        /* 
        * If the twopacks are close, increment count and running sum.
        */
        if (arrtwopackXoZ[iarr] - start < TMuiGeometry::dx_twopack / 2.0 / gap0Z)
        {
          sum += arrtwopackXoZ[iarr];
          cnt += 1;
        }
        /*
        * Else calculate index position, increment index, reinitialize
        * count and running sum.
        */
        else
        {
          idxXoZ[idx] = sum / (float) cnt;
          cosOut << idx << "\t" << arm << "\t" << orient << "\t" << idxXoZ[idx] << endl;
          idx ++;
          start = arrtwopackXoZ[iarr];
          sum = arrtwopackXoZ[iarr];
          cnt = 1;
        }
        /* 
        * For every twopack, assign entry in the map array.
        */
        unsigned int panel = arrpanel[arrsort[iarr]];
        unsigned int twopack = arrtwopack[arrsort[iarr]];
        map[TMuiChannelId(arm, gap, panel, (EOrient_t) orient, twopack)] = idx;
        TMuiReadoutID tmpAddress (TMuiAddressTable::Table ()->HardwareAddress (TMuiChannelId(arm, gap, panel, (EOrient_t) orient, twopack)));
        mapOut << tmpAddress.FEM () << "\t" << tmpAddress.ROC () << "\t" << tmpAddress.Word () << "\t" << tmpAddress.Channel () << "\t" << idx << "\t" << gap << "\t" << panel << endl;

        ll1MapOut << arm << "\t"
          << orient << "\t"
          << tmpAddress.ROC () << "\t"
          << tmpAddress.Word () << "\t"
          << tmpAddress.Channel () << "\t"
          << gap << "\t"
          << panel << "\t" << twopack << "\t" << idx << endl;
        
      }	

      /*
      * Calculate position of last twopack index and store total
      * number of indices.
      */
      idxXoZ[idx] = sum / (float) cnt;
      unsigned int nidx = idx;
      
      /*
      * Now loop over other gaps' panels and twopacks for this 
      * arm/orientation. Compare the normalized twopack positions 
      * to the normalized positions of the indexed twopacks in 
      * gap0. Assign the index with the closest position to each 
      * two pack.
      */
      for (gap = 1; gap < TMuiChannelId::kPlanesPerArm; gap++)
      {
        /*
        *Loop over panels.
        */
        for( unsigned int panel = 0; panel < TMuiChannelId::kPanelsPerPlane; panel++)
        {
          /*
          * Get pointer to panel geometry object.
          * Pull out # twopacks in panel, panel center location.
          */
          TMuiPanelGeo *p = g->getPanel (arm, gap, panel);
          unsigned int ntwopack_panel = p->getTwoPackCount ((EOrient_t) orient);
          
          // center position
          float panelX, panelY, panelZ;
          p->CenterPos (panelX, panelY, panelZ);
          float panelXY( orient == kVERT ? panelX : panelY );

          // Loop over twopacks.
          for(unsigned int twopack = 0; twopack < ntwopack_panel; twopack++)
          {
            
            // twopack center location.            
            float twopackX, twopackY, twopackZ;
            p->TwoPackPointer ((EOrient_t) orient, twopack)->CenterPos (twopackX, twopackY, twopackZ);
            float twopackXY( orient == kVERT ? twopackX : twopackY );
            float twopackXoZ = (twopackXY + panelXY) / fabs (twopackZ + panelZ);
            
            /* 
            * Search for tube location in twopack-index position array.
            */
            idx = SearchArray (nidx, idxXoZ, twopackXoZ);
            
            /*
            * The value of idx returned by this routine is such
            * that idxXoZ[idx-1] < twopackXoZ < idxXoZ[idx]
            * So I need to check idx and idx+1 to see which is closer.
            * Note that idx can range from 0 to nidx. In order to avoid
            * boundary errors, the case in which idx=0 is explicitly 
            * ignored (idx is left equal to 0) and the case in which
            * idx=nidx is explicitly addressed (idx is set equal to nidx-1). 
            */
            if (idx > 0 && idx < nidx)
            {
              if (twopackXoZ - idxXoZ[idx - 1] < idxXoZ[idx] - twopackXoZ) idx -= 1;
            } else if (idx == nidx) idx = nidx - 1;
            /* 
            * Assign entry in map array.
            */
            map[TMuiChannelId(arm, gap, panel, (EOrient_t) orient, twopack)] = idx;
            TMuiReadoutID tmpAddress (TMuiAddressTable::Table ()->HardwareAddress (TMuiChannelId(arm, gap, panel,(EOrient_t) orient, twopack)));
            mapOut << tmpAddress.FEM () << "\t" << tmpAddress.ROC () << "\t" << tmpAddress.Word () << "\t" << tmpAddress.Channel () << "\t" << idx << "\t" << gap << "\t" << panel << endl;

            ll1MapOut << arm << "\t"
              << orient << "\t"
              << tmpAddress.ROC () << "\t"
              << tmpAddress.Word () << "\t"
              << tmpAddress.Channel () << "\t"
              << gap << "\t"
              << panel << "\t" << twopack << "\t" << idx << endl;
            
          }
        }
      }
    }	
  }
  mapOut.close ();
  ll1MapOut.close ();
  cosOut.close ();

  //Now lets output the LL1 Map file which is in the following format
  //Filename: LL1MapArmXOrientX.map
  //Header: For each ROC
  //
  //Fiber  0
  //========
  //ROC bit Gap Logical tube

  //96 Entries for each map
  //
  for (int femID = 0; femID < TMuiReadoutID::kFEMsTotal; femID++)
  {
    char ofilename[256];
    int armID = femID / 2;
    int orientID = femID % 2;
    //Construct MuID module id
    // Should be:
    // 0x0000 SH
    // 0x0010 SV
    // 0x1000 NH
    // 0x1010 NV
    int moduleID = 4096 * armID + 16 * orientID;
    sprintf (ofilename, "LL1MapArm%dOrient%d.map", armID, orientID);
    ofstream ofile (ofilename);
    for (int rocID = 0; rocID < TMuiReadoutID::kROCsPerFEM; rocID++)
    {
      ofile << "Fiber  " << rocID << endl;
      ofile << "========\n";
      ofile << "ROC bit\t\tGap\t\tLogical tube\n";
      //Let's Loop over each of 96 bits
      for (int bitID = 0; bitID < TMuiReadoutID::kWordsPerROC * TMuiReadoutID::kChannelsPerWord; bitID++)
      {
        // Since the map is indexed by software address
        // we first have to convert HardWare to SoftWare
        const TMuiReadoutID tRO (moduleID, rocID, bitID / TMuiReadoutID::kChannelsPerWord, bitID % TMuiReadoutID::kChannelsPerWord);
        TMuiChannelId tCO (TMuiAddressTable::Table ()->SoftwareAddress (tRO));
        int dxID = -1;
        if (tCO.TwoPack () >= 0) dxID = map[tCO];
        ofile << bitID << "\t\t" << tCO.Plane () << "\t\t" << dxID << endl;
      }			
    }
    ofile.close ();
  }
}
