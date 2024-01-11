// $Id: mMuiClusterFinder.cxx,v 1.5 2009/06/02 23:23:50 hpereira Exp $

/*!
	\file		mMuiClusterFinder.cxx
	\brief	 muid cluster finder
	\author	Jason Newby, Hugo Pereira
	\version $Revision: 1.5 $
	\date		$Date: 2009/06/02 23:23:50 $
*/

#include <iostream>
#include "PHIODataNode.h"
#include "PHTable.hh"

#include "mMuiClusterFinder.h"

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;
typedef list<TMuiHitO>::iterator hit_list_iterator;

//_______________________________________________________________
mMuiClusterFinder::mMuiClusterFinder():
	_timer( PHTimeServer::get()->insert_new("mMuiClusterFinder") )
	{}

//_______________________________________________________________
PHBoolean mMuiClusterFinder::event(PHCompositeNode *top_node)
{
	_timer.get()->restart();
	try {

		// Reset IOC pointers
		set_interface_ptrs(top_node);
		_mui_cluster_map->clear();

		// Associate groups of contiguous hits with TMuiClusterO objects
		find_clusters();

	} catch(std::exception& e) {
		MUIOO::TRACE(e.what());
		return False;
	}
	_timer.get()->stop();
	return True;
}

//_______________________________________________________________
void mMuiClusterFinder::set_interface_ptrs(PHCompositeNode* top_node)
{
	 // module runtime parameters
	_mod_par = TMutNode<mMuiClusterFinderPar>::find_node(top_node,"mMuiClusterFinderPar");

	//Find the new TMuiHitO objects
	_mui_hit_map = TMutNode<TMuiHitMapO>::find_node(top_node,"TMuiHitMapO");

	//Find the new TMuiClusterO objects
	_mui_cluster_map = TMutNode<TMuiClusterMapO>::find_node(top_node,"TMuiClusterMapO");

	return;
}

//_______________________________________________________________
void mMuiClusterFinder::find_clusters()
{

  // loop over arm, plane and panels
  for (short arm=0; arm<TMuiChannelId::kArmsTotal; arm++)
  {
		for (short plane=0; plane<TMuiChannelId::kPlanesPerArm; plane++)
    {
			for (short panel=0; panel<TMuiChannelId::kPanelsPerPlane; panel++)
      {

				for (short orient=0; orient<TMuiChannelId::kOrientations; orient++)
        {

          /*
            retrieve hits for this orientation.
            Note: by construction hits are ordered by their twopack index
          */
          TMuiHitMapO::iterator hit_iter = _mui_hit_map->get(arm,plane,panel,orient);

          if (_mod_par->get_clustering_mode() == mMuiClusterFinderPar::NONE)
          {

            while(TMuiHitMapO::pointer hit_ptr = hit_iter.next())  make_new_cluster(hit_ptr);

          } else {

            // current cluster
            TMuiClusterMapO::pointer clus_ptr = 0;
            UShort_t current_twopack = 0;

            // loop over hits
            while(TMuiHitMapO::pointer hit_ptr = hit_iter.next())
            {

              /*
              create new cluster is current cluster is 0
              or if new hit is more that one twopack away from current cluster
              */
              if( (!clus_ptr) || abs( hit_ptr->get()->get_twopack() - current_twopack ) > 1 )
              { clus_ptr = make_new_cluster( hit_ptr ); }

              // otherwise associate the hit to the current cluster
              else
              { PHKey::associate( hit_ptr, clus_ptr ); }

              // update current two-pack
              current_twopack = hit_ptr->get()->get_twopack();
            }

            // In case we are in the ISOLATED mode, split too large clusters..
            if (_mod_par->get_clustering_mode() == mMuiClusterFinderPar::ISOLATED)
            {

              TMuiClusterMapO::iterator clus_iter = _mui_cluster_map->get(arm, plane, panel, orient);
              while(TMuiClusterMapO::pointer clus_ptr = clus_iter.next()) split_cluster(clus_ptr);

            }

          }

        }

        //get a pointer to this panel's geometry object
				TMuiChannelId pPanelId(arm,plane,panel);
				TMuiPanelGeo* fpMuiPanelGeo	 = TMuiGeometry::Geom()->getPanel(pPanelId);

        // calculate the position of found clusters
        TMuiClusterMapO::iterator clus_iter = _mui_cluster_map->get(arm, plane, panel);
        while(TMuiClusterMapO::pointer clus_ptr = clus_iter.next()) calc_position(clus_ptr, fpMuiPanelGeo);


      }

    }

  }

	return;
}

//_______________________________________________________________
void mMuiClusterFinder::calc_position(TMuiClusterMapO::pointer clus_ptr, TMuiPanelGeo *fpMuiPanelGeo)
{

	// iterator to hits associated with this cluster
	TMuiHitMapO::key_iterator hit_iter = clus_ptr->get()->get_associated<TMuiHitO>();

	// loop over associated hits and get their positions
	PHPoint clpos( 0, 0, 0 );
	PHPoint clbegin( 0, 0, 0 );
	PHPoint clend( 0, 0, 0 );
	PHPoint clerr( 0, 0, 0 );

	int clnhit = 0;
	EOrient_t orient = kHORIZ;
	while(TMuiHitMapO::pointer clus_hit_ptr = hit_iter.next())
  {

		TMuiChannelId pTPID(clus_hit_ptr->get()->get_arm(),
			clus_hit_ptr->get()->get_plane(),
			clus_hit_ptr->get()->get_panel(),
			(EOrient_t)clus_hit_ptr->get()->get_orientation(),
			clus_hit_ptr->get()->get_twopack());

		orient = (EOrient_t)clus_hit_ptr->get()->get_orientation();
		TMuiTwoPackGeo* fpMuiTwoPackGeo = TMuiGeometry::Geom()->getTwoPack(pTPID);

		//Now we need to find the centroid of this tube
		if (fpMuiPanelGeo && fpMuiTwoPackGeo) {

			// update center position
			float x, y, z;
			fpMuiTwoPackGeo->CenterPos(x, y, z);
			PHPoint pos(x, y, z);
			PHPoint fCentroidPos = fpMuiPanelGeo->TransformToGlobal(pos);
			clpos = clpos + fCentroidPos;

			/* update begin/end point. Use of internal xMin, xMax, yMin and yMax depend on the tube orientation */
			PHPoint begin( 0, 0, 0 );
			PHPoint end( 0, 0, 0 );
			if( orient == kHORIZ ) {

				begin = PHPoint(
					( fpMuiTwoPackGeo->FrontTube().XMin() + fpMuiTwoPackGeo->BackTube().XMin() )/2,
					( fpMuiTwoPackGeo->FrontTube().YMin() + fpMuiTwoPackGeo->BackTube().YMin() +
						fpMuiTwoPackGeo->FrontTube().YMax() + fpMuiTwoPackGeo->BackTube().YMax() )/4,
					( fpMuiTwoPackGeo->FrontTube().ZMin() + fpMuiTwoPackGeo->BackTube().ZMin() +
						fpMuiTwoPackGeo->FrontTube().ZMax() + fpMuiTwoPackGeo->BackTube().ZMax() )/4 );

				end = PHPoint(
					( fpMuiTwoPackGeo->FrontTube().XMax() + fpMuiTwoPackGeo->BackTube().XMax() )/2,
					( fpMuiTwoPackGeo->FrontTube().YMin() + fpMuiTwoPackGeo->BackTube().YMin() +
						fpMuiTwoPackGeo->FrontTube().YMax() + fpMuiTwoPackGeo->BackTube().YMax() )/4,
					( fpMuiTwoPackGeo->FrontTube().ZMin() + fpMuiTwoPackGeo->BackTube().ZMin() +
						fpMuiTwoPackGeo->FrontTube().ZMax() + fpMuiTwoPackGeo->BackTube().ZMax() )/4 );

			} else {

				begin = PHPoint(
					( fpMuiTwoPackGeo->FrontTube().XMin() + fpMuiTwoPackGeo->BackTube().XMin() +
						fpMuiTwoPackGeo->FrontTube().XMax() + fpMuiTwoPackGeo->BackTube().XMax() )/4,
					( fpMuiTwoPackGeo->FrontTube().YMin() + fpMuiTwoPackGeo->BackTube().YMin() )/2,
					( fpMuiTwoPackGeo->FrontTube().ZMin() + fpMuiTwoPackGeo->BackTube().ZMin() +
						fpMuiTwoPackGeo->FrontTube().ZMax() + fpMuiTwoPackGeo->BackTube().ZMax() )/4 );

				end = PHPoint(
					( fpMuiTwoPackGeo->FrontTube().XMin() + fpMuiTwoPackGeo->BackTube().XMin() +
						fpMuiTwoPackGeo->FrontTube().XMax() + fpMuiTwoPackGeo->BackTube().XMax() )/4,
					( fpMuiTwoPackGeo->FrontTube().YMax() + fpMuiTwoPackGeo->BackTube().YMax() )/2,
					( fpMuiTwoPackGeo->FrontTube().ZMin() + fpMuiTwoPackGeo->BackTube().ZMin() +
						fpMuiTwoPackGeo->FrontTube().ZMax() + fpMuiTwoPackGeo->BackTube().ZMax() )/4	);
			}


			begin = fpMuiPanelGeo->TransformToGlobal(begin);
			clbegin = clbegin + begin;

			end = fpMuiPanelGeo->TransformToGlobal(end);
			clend = clend + end;

			// update error
			/*
				note: unlike the position the error is expressed in *local* coordinates.
				It is not rotated with the panel angle. The reason is that would it be rotated, a
				3 by 3 matrix should be needed instead of a simple vector (indeed, the rotation would
				introduce correlations between the errors on x, y, z)
			*/
			fpMuiTwoPackGeo->CenterSigma(x, y, z);
			PHPoint fCentroidErr(x, y, z);
			clerr = clerr + fCentroidErr;

			clnhit++;

		}
	}

	if (clnhit > 0)
	{
		clpos = clpos*(1.0/clnhit);
		clbegin = clbegin*(1.0/clnhit);
		clend = clend*(1.0/clnhit);

		/*
			we should scale the error also
			if vert., the values/errors are the same for the two-packs in y,z
			if horiz., the values/errors are the same for the two-packs in x,z
		*/
		if (orient == kHORIZ)
		{ // horizontal, don't scale y-value
			clerr.setX(clerr.getX() *( 1.0/clnhit) );
			clerr.setZ(clerr.getZ() *( 1.0/clnhit) );
		} else
		{ // vertical, don't scale x-value
			clerr.setY(clerr.getY() *( 1.0/clnhit) );
			clerr.setZ(clerr.getZ() *( 1.0/clnhit) );
		}

		clus_ptr->get()->set_centroidpos( clpos );
		clus_ptr->get()->set_coord_begin(clbegin);
		clus_ptr->get()->set_coord_end(clend);
		clus_ptr->get()->set_centroidsigma( clerr );
		clus_ptr->get()->set_size(clnhit);

	} else {

		// couldn't locate coordinates of any hits.
		// That must be bad. We might as well remove this cluster
		_mui_cluster_map->erase(clus_ptr->get()->get_key());

	}
	return;
}

//_______________________________________________________________
TMuiClusterMapO::pointer mMuiClusterFinder::make_new_cluster(TMuiHitMapO::pointer hit_ptr)
{

  // make a new cluster with same location as hit
	TMuiClusterMapO::iterator clus_iter =
		_mui_cluster_map->insert_new(hit_ptr->get()->get_arm(),
						 hit_ptr->get()->get_plane(),
						 hit_ptr->get()->get_panel(),
						 hit_ptr->get()->get_orientation());

	// Associate new cluster with hit
	PHKey::associate(hit_ptr,clus_iter.current());
  return clus_iter.current();

}

//_______________________________________________________________
void mMuiClusterFinder::split_cluster(TMuiClusterMapO::pointer clus_ptr)
{

  // take the (two) hits in this cluster and make separate clusters from them
	TMuiHitMapO::key_iterator hit_iter = clus_ptr->get()->get_associated<TMuiHitO>();

  // check size. If small enough, do nothing
  if ( hit_iter.count() <=  _mod_par->get_max_cluster_width() ) return;

	// loop over associated hits and create single hit clusters
	while(TMuiHitMapO::pointer clus_hit_ptr = hit_iter.next()) make_new_cluster(clus_hit_ptr);

	// delete the original cluster
	_mui_cluster_map->erase(clus_ptr->get()->get_key());

}
