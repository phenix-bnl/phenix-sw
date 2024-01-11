// $Id: MuiMapInit_Trigger.cc,v 1.2 2009/04/27 16:58:41 hpereira Exp $

/*!
	 \file MuiMapInit_Trigger.cc
	 \brief muid L1 and L2 map initialisations
	 \author Jason Newby
	 \version $Revision: 1.2 $
	 \date $Date: 2009/04/27 16:58:41 $
*/

#include <TMuiChannelId.hh>
#include <TMuiTwoPackGeo.hh>
#include <TMuiPanelGeo.hh>
#include <TMuiGeometry.hh>
#include <TMuiAddressTable.hh>
#include <math.h>
#include <map>

/*  Jason Newby's MuiMapInit.cc at offline/packages/mui
 *  removed unnecessary I/O and some part of the code that's causing 
 *  trouble */

//___________________________________________________________
int SearchArray( const int& n, float* array, const float& key)
{
  int left = 1;
  int right = n-1;
  int i;

  if (key >= array[n-1]) return n;
  else if (key < array[0]) return 0;

  while (right >= left) {
    i = (left + right) / 2;
    if (key >= array[i-1] && key < array[i]) return i;
    if (key < array[i-1]) right = i-1;
    else left = i+1;
  }
  return 0;
}

//___________________________________________________________
void MuiMapInit_Trigger (hashVector < TMuiChannelId, int >&map)
{
  short arm;
  short orient;
  short gap;
  short panel;
  short twopack;

  /*
   * Panel and twopack position arrays.
   */
  float panelXY;
  float panelX, panelY, panelZ;

  float twopackXY;
  float twopackX, twopackY, twopackZ;

  /*
   * Arrays for sorting/storing normalized tube locations on gap 0.
   */
  float arrtwopackXoZ[TMuiChannelId::kTwoPacksPerPlaneMax];
  float idxXoZ[TMuiChannelId::kTwoPacksPerPlaneMax];
  int arrsort[TMuiChannelId::kTwoPacksPerPlaneMax];
  int arrpanel[TMuiChannelId::kTwoPacksPerPlaneMax];
  int arrtwopack[TMuiChannelId::kTwoPacksPerPlaneMax];

  /*
   * Miscellaneous variables.
   */
  int ntwopack_gap;
  int ntwopack_panel;
  int cnt;
  int nidx;
  int idx;
  int iarr;
  float sum;
  float gap0Z;
  float start;
  float twopackXoZ;

  /* 
   * Geometry object pointers.
   */
  TMuiGeometry *g;
  TMuiPanelGeo *p;

  /* 
   * Get pointer to MUID geometry object.
   */
  g = TMuiGeometry::Geom ();

  /*
   * Loop over arm and orientation, get one map array for each.
   */
  for (arm = 0; arm < TMuiChannelId::kArmsTotal; arm++)
    {
      for (orient = 0; orient < TMuiChannelId::kOrientations; orient++)
	{
	  /*
	   * Generate a twopack index for each twopack in gap 0. 
	   * This is done by generating an ordered list of twopack centers 
	   * (normalized by their distance to the target) and collecting 
	   * all twopacks that are within half the nominal twopack 
	   * separation. The mean position of the pack index is also 
	   * stored for comparison to following gaps.
	   */

	  gap = 0;
	  ntwopack_gap = 0;
	  gap0Z = 0;

	  /*
	   * Loop over panels.
	   */
	  std::multimap < double, int >two_pack_center_map;
	  for (panel = 0; panel < TMuiChannelId::kPanelsPerPlane; panel++)
	    {
	      /*
	       * Get pointer to panel geometry object.
	       * Pull out # twopacks in panel, panel center location.
	       */
	      p = g->getPanel (arm, gap, panel);
	      ntwopack_panel = p->getTwoPackCount ((EOrient_t) orient);
	      p->CenterPos (panelX, panelY, panelZ);
	      orient == kVERT ? panelXY = panelX : panelXY = panelY;
	      /*
	       * Loop over twopacks.
	       */
	      for (twopack = 0; twopack < ntwopack_panel; twopack++)
		{
		  /*
		   * Pull out twopack center location.
		   */
		  p->TwoPackPointer ((EOrient_t) orient,
				     twopack)->CenterPos (twopackX, twopackY,
							  twopackZ);
		  orient == kVERT ? twopackXY = twopackX : twopackXY =
		    twopackY;

		  // map center location to twopack id.
		  two_pack_center_map.
		    insert (std::make_pair
			    ((twopackXY + panelXY) / fabs (twopackZ + panelZ),
			     ntwopack_gap));

		  arrpanel[ntwopack_gap] = panel;
		  arrtwopack[ntwopack_gap] = twopack;
		  gap0Z += fabs (twopackZ + panelZ);
		  ntwopack_gap += 1;
		}		/* End loop over twopacks. */
	    }			/* End loop over panels. */
	  gap0Z = gap0Z / (float) ntwopack_gap;


	  /* 
	     port the sorted map of center position
	     and indexes in the corresponding sorted arrays
	   */
	  int index = 0;
	  for (std::multimap < double, int >::iterator iter =
	       two_pack_center_map.begin ();
	       iter != two_pack_center_map.end () && index < ntwopack_gap;
	       iter++, index++)
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
	  for (idx = 0; idx < TMuiChannelId::kTwoPacksPerPlaneMax; idx++)
	    idxXoZ[idx] = 0;
	  idx = 0;
	  start = arrtwopackXoZ[0];
	  sum = 0;
	  cnt = 0;

	  /* 
	   * Loop over ordered array of two-pack positions.
	   */
	  for (iarr = 0; iarr < ntwopack_gap; iarr++)
	    {
	      /* 
	       * If the twopacks are close, increment count and running sum.
	       */
	      if (arrtwopackXoZ[iarr] - start <
		  TMuiGeometry::dx_twopack / 2.0 / gap0Z)
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
		  idx += 1;
		  start = arrtwopackXoZ[iarr];
		  sum = arrtwopackXoZ[iarr];
		  cnt = 1;
		}
	      /* 
	       * For every twopack, assign entry in the map array.
	       */
	      panel = arrpanel[arrsort[iarr]];
	      twopack = arrtwopack[arrsort[iarr]];
	      map[TMuiChannelId
		  (arm, gap, panel, (EOrient_t) orient, twopack)] = idx;
	      TMuiReadoutID tmpAddress (TMuiAddressTable::Table ()->
					HardwareAddress (TMuiChannelId
							 (arm, gap, panel,
							  (EOrient_t) orient,
							  twopack)));
	    }			/* End loop over ordered twopacks. */

	  /*
	   * Calculate position of last twopack index and store total
	   * number of indices.
	   */
	  idxXoZ[idx] = sum / (float) cnt;
	  nidx = idx;

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
	      for (panel = 0; panel < TMuiChannelId::kPanelsPerPlane; panel++)
		{
		  /*
		   * Get pointer to panel geometry object.
		   * Pull out # twopacks in panel, panel center location.
		   */
		  p = g->getPanel (arm, gap, panel);
		  ntwopack_panel = p->getTwoPackCount ((EOrient_t) orient);
		  p->CenterPos (panelX, panelY, panelZ);
		  orient == kVERT ? panelXY = panelX : panelXY = panelY;
		  /*
		   * Loop over twopacks.
		   */
		  for (twopack = 0; twopack < ntwopack_panel; twopack++)
		    {
		      /*
		       * Pull out twopack center location.
		       */
		      p->TwoPackPointer ((EOrient_t) orient,
					 twopack)->CenterPos (twopackX,
							      twopackY,
							      twopackZ);
		      orient == kVERT ? twopackXY = twopackX : twopackXY =
			twopackY;
		      twopackXoZ =
			(twopackXY + panelXY) / fabs (twopackZ + panelZ);
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
			  if (twopackXoZ - idxXoZ[idx - 1] <
			      idxXoZ[idx] - twopackXoZ)
			    idx -= 1;
			}
		      else if (idx == nidx)
			idx = nidx - 1;
		      /* 
		       * Assign entry in map array.
		       */
		      map[TMuiChannelId
			  (arm, gap, panel, (EOrient_t) orient, twopack)] =
			idx;
		      TMuiReadoutID tmpAddress (TMuiAddressTable::Table ()->
						HardwareAddress (TMuiChannelId
								 (arm, gap,
								  panel,
								  (EOrient_t)
								  orient,
								  twopack)));
		    }		/* End loop over twopacks. */
		}		/* End loop over panels. */
	    }			/*End loop over gaps. */

	}			/* End loop over orientations. */
    }				/* End loop over arms. */
}
