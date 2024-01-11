#include <cmath>
#include <iostream>
#include <string>
#include <climits>

#include <boost/bind.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/connected_components.hpp>

#include <PHTFileServer.h>
#include <mFvtxFindSvxClusters.h>
#include <mFvtxFindSvxClustersPar.h>
#include <TMutNode.h>
#include <TFvtxSvxClusterMap.h>
#include <SvxClusterListv4.h>
#include <SvxClusterv4.h>
#include <PHException.h>
#include <FVTXOO.h>
#include <FVTXGEOM.h>

#include <TTree.h>

mFvtxFindSvxClusters::mFvtxFindSvxClusters() :
  _mod_par(NULL),
  _svxclus_node(NULL),
  _clus_map(NULL),
  _timer( PHTimeServer::get()->insert_new("mFvtxFindSvxClusters") ),
  _eval_tree(NULL)
{
  FVTXOO::TRACE("initializing module mFvtxFindSvxClusters compiled " __DATE__ " " __TIME__);
}

void
mFvtxFindSvxClusters::init_run(PHCompositeNode*)
{
  _ievent = 0;
}

void
mFvtxFindSvxClusters::init(PHCompositeNode*)
{
}

void
mFvtxFindSvxClusters::end(PHCompositeNode*)
{
  std::cout << "mFvtxFindSvxClusters::end " << __DATE__ << " " << __TIME__ << std::endl;
  if ( _eval_tree ) _eval_tree->AutoSave();
  _timer.get()->print_stat();
}

void
mFvtxFindSvxClusters::init_trees()
{
  std::string f = "mFvtxFindSvxClusters_eval.root";
  std::cout << "mFvtxStraightLineFit::init: Writing to eval file " << f << std::endl;
  PHTFileServer::get().open( f.c_str(), "RECREATE" );
  
  std::cout << "mFvtxFindSvxClusters: Booking eval tree(s)" << std::endl;
  _eval_tree = new TTree("svxclus_eval","svxclus_eval");
  _eval_tree->Branch("event",&_ievent,"event/I",BUFFER_SIZE);
  _eval_tree->Branch("layer",&_layer,"layer/S",BUFFER_SIZE);
  _eval_tree->Branch("ladder",&_ladder,"ladder/S",BUFFER_SIZE);
  _eval_tree->Branch("sensor",&_sensor,"sensor/S",BUFFER_SIZE);
  _eval_tree->Branch("x",&_x,"x/F",BUFFER_SIZE);
  _eval_tree->Branch("y",&_y,"y/F",BUFFER_SIZE);
  _eval_tree->Branch("z",&_z,"z/F",BUFFER_SIZE);
  _eval_tree->Branch("xerr",&_xerr,"xerr/F",BUFFER_SIZE);
  _eval_tree->Branch("yerr",&_yerr,"yerr/F",BUFFER_SIZE);
  _eval_tree->Branch("zerr",&_zerr,"zerr/F",BUFFER_SIZE);
  _eval_tree->SetAutoSave(AUTO_SAVE);   
}

void
mFvtxFindSvxClusters::fill_trees()
{
  if ( !_eval_tree ) return;

  TFvtxSvxClusterMap::const_iterator clus_iter = _clus_map->range();
  while ( TFvtxSvxClusterMap::const_pointer clus_ptr = clus_iter.next() )
    {
      const SvxCluster* c = clus_ptr->get()->get_cluster();
      _layer = c->get_layer();
      _ladder = c->get_ladder();
      _sensor = c->get_sensor();
      _x = c->get_xyz_global(0);
      _y = c->get_xyz_global(1);
      _z = c->get_xyz_global(2);
      //_xerr = c->get_size_xyz_global(0,0);
      // _yerr = c->get_size_xyz_global(1,1);
      //_zerr = c->get_size_xyz_global(2,2);
      _eval_tree->Fill();
    }
}

PHBoolean
mFvtxFindSvxClusters::event(PHCompositeNode* top_node)
{
  _ievent++;

  _timer.get()->restart();

  try
    {
      // Reset IOC pointers
      set_interface_ptrs(top_node);

      // clear cluster map
      _clus_map->clear();

      if ( _mod_par->get_do_evaluation() && _eval_tree == 0 ) init_trees();

      initialize_event();

      //
      find_clusters();

      fill_trees();
    }
  catch (const std::exception& e)
    {
      FVTXOO::TRACE(e.what());
      return False;
    }

  _timer.get()->stop();

  // if verbose dump the contents of the cluster map
  //
  if(_mod_par->get_verbosity() >= FVTXOO::ALOT) _clus_map->print();
  if(_mod_par->get_verbosity() >= FVTXOO::SOME) _timer.get()->print();

  return True;
}

void
mFvtxFindSvxClusters::set_interface_ptrs(PHCompositeNode* top_node)
{
  _mod_par = TMutNode<mFvtxFindSvxClustersPar>::find_node(top_node,"mFvtxFindSvxClustersPar");
  _clus_map = TMutNode<TFvtxSvxClusterMap>::find_node(top_node, "TFvtxSvxClusterMap");

  PHNodeIterator nodeItr(top_node);
  if ( PHCompositeNode* svx_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "SVX")) )
    {
      _svxclus_node = TMutNode<SvxClusterList>::find_io_node(svx_node,"SvxClusterList");
    }
  else
    {
      _svxclus_node = 0;
    }

}


void
mFvtxFindSvxClusters::find_clusters()
{
  if ( ! _svxclus_node ) return;

  std::list<SvxCluster*> merged;

  // Perform merging on the first two layers
#if 0
  merge_clusters(_svxClusters[0],merged);
  merge_clusters(_svxClusters[1],merged);
#else
  for(int i=0; i<2; i++)
    {
      for (unsigned int j=0; j<_svxClusters[i].size(); j++)
	{
	  SvxCluster* p = new SvxClusterv4(_svxClusters[i][j]);
	  merged.push_back(p);
	}
    }
#endif

  // Just copy the third and fourth
	/*
  for(int i=2; i<4; i++)
    {
      for (unsigned int j=0; j<_svxClusters[i].size(); j++)
	{
	  SvxCluster* p = new SvxClusterv4(_svxClusters[i][j]);
	  merged.push_back(p);
	}
    }
	*/
//   int nClusters = _svxclus_node->get_nClusters();
//   for (int i=0; i<nClusters; i++)
//     {
//       TFvtxSvxClusterMap::iterator clus_iter( _clus_map->insert_new() );
//       const SvxCluster* c = _svxclus_node->get_Cluster(i);
//       clus_iter->get()->set_cluster( c );

//       // TODO: Currently it appears that the pixels have no errors assigned to
//       // them.  Perhaps we include something in the wrapper object.
//     }

  std::list<SvxCluster*>::iterator i = merged.begin();
  for ( ; i!=merged.end(); i++)
    {
      TFvtxSvxClusterMap::iterator clus_iter( _clus_map->insert_new() );
      const SvxCluster* c = *i;
      clus_iter->get()->set_cluster( c );

      // TODO: Currently it appears that the pixels have no errors assigned to
      // them.  Perhaps we include something in the wrapper object.
    }

  // Now clean up the allocated merged clustere (the fvtx wrapper contains a copy,
  // so the original is no longer needed).
  for ( i=merged.begin(); i!=merged.end(); i++)
    {
      delete *i;
    }
  merged.clear();
}

void
mFvtxFindSvxClusters::initialize_event()
{
  if ( !_svxclus_node ) return;

  // Load the clusters into the per-layer lists
  //
  _svxClusters[0].clear();
  _svxClusters[1].clear();
  _svxClusters[2].clear();
  _svxClusters[3].clear();
  int nclus = _svxclus_node->get_nClusters();
  for(int i=0; i<nclus; i++)
    {
      SvxCluster* p = _svxclus_node->get_Cluster(i);
      int layer = p->get_layer();
      if ( layer > 3 ) continue;
      _svxClusters[layer].push_back(p);
    }
  if(_mod_par->get_verbosity() >= FVTXOO::ALOT) std::cout << "mFvtxFindSvxClusters::initialize_event: " << _svxClusters[0].size()+_svxClusters[1].size() << " clusters in VTX pixels" << std::endl;
}

template<typename T>
struct CompZandPhi : std::binary_function<T*,T*,bool>
{
  bool operator()(const T* a, const T* b) const
  {
    if ( fabs(a->get_xyz_local(2)-b->get_xyz_local(2)) < 1.0e-4 )
      {
	// Hits have same Z - Compare local X (increasing local X corresponds to decreasing phi)
	return a->get_xyz_local(0) > b->get_xyz_local(0);
	//double phi_a = atan2(a->get_xyz_global(1),a->get_xyz_global(0));
	//double phi_b = atan2(b->get_xyz_global(1),b->get_xyz_global(0));
	//return FVTXOO::angle_normalize(phi_a-phi_b) < 0.0;
      }
    else
      {
	// Hits have different Z - Compare the Z
	return a->get_xyz_local(2) < b->get_xyz_local(2);
      }
  }
};

void
mFvtxFindSvxClusters::merge_clusters(std::vector<SvxCluster*>& clus, std::list<SvxCluster*>& merged)
{
  // First sort the cluster list in ascending global z then phi
  //
  std::sort(clus.begin(),clus.end(),CompZandPhi<SvxCluster>());

  // We model the hits as a graph, and adjacent hits as connected components
  // of that graph.  Thus when we find the connected components, we will be
  // able to merge each set together.

  using namespace boost;
  typedef adjacency_list <vecS, vecS, undirectedS> Graph;

  Graph G;

  for(unsigned int i=0; i<clus.size(); i++)
    {
      unsigned int j=i+1;
      if ( j<clus.size() )
	{
 	  if ( fabs(clus[i]->get_xyz_local(0) - clus[j]->get_xyz_local(0)) < 0.006 )
 	    {
 	      add_edge(i,j,G);
 	    }
	  else add_edge(i,i,G);
	}
      else add_edge(i,i,G);
    }

  std::vector<int> component(num_vertices(G));
  //int num = connected_components(G, &component[0]);

  std::set<int> comps; // Number of unique components
  std::multimap<int,SvxCluster*> groups;
  for(unsigned int i=0; i<component.size(); i++)
    {
      comps.insert(component[i]);
      groups.insert(std::pair<int,SvxCluster*>(component[i],clus[i]));
    }

//   for(unsigned int i=0; i<clus.size(); i++)
//     {
//       SvxCluster* p = clus[i];
//       std::cout << p->get_hitID() << ", layer " << p->get_layer()
// 		<< ", comp " << component[i]
// 		<< ": "
// 		<< p->get_xyz_local(0) << " " << p->get_xyz_local(1) << " " << p->get_xyz_local(2) << " / "
// 		<< p->get_xyz_global(0) << " " << p->get_xyz_global(1) << " " << p->get_xyz_global(2) << std::endl;
//     }


//   std::cout << "Total number of components: " << num << std::endl;
//   for (std::vector<int>::size_type i = 0; i != component.size(); ++i)
//     std::cout << "Vertex " << i <<" is in component " << component[i] << std::endl;

  std::map<int,SvxCluster*>::iterator b, e;
  for(std::set<int>::iterator i = comps.begin(); i!=comps.end(); i++)
    {
      //std::cout << "id " << *i << " has " << groups.count(*i) << " elements" << std::endl;
      int nhits = groups.count(*i);
      boost::tie(b,e)=groups.equal_range(*i);
      std::map<int,SvxCluster*>::iterator n=b;
      SvxCluster* p = b->second;
      double xmean = p->get_xyz_global(0);
      double ymean = p->get_xyz_global(1);
      double zmean = p->get_xyz_global(2);
      for( n++; n!=e; n++ )
 	{
	  xmean += (n->second)->get_xyz_global(0);
	  ymean += (n->second)->get_xyz_global(1);
	  zmean += (n->second)->get_xyz_global(2);
 	}
      xmean /= nhits;
      ymean /= nhits;
      zmean /= nhits;

      // Create a new "merged" cluster.  I don't yet know how to define some
      // of the merged quantities, like "size" and adc.  Size is a bit of a problem,
      // since the only size info we get is in global coords, and the new size needs to be
      // transformed to global coords.  But I don't know how to get the transformation
      // matrix for the local sensor coords yet.
      //std::cout << "New cluster " << *i << ": " << xmean << " " << ymean << std::endl;
      SvxCluster* newClus = new SvxClusterv4(p);
      newClus->set_xyz_global(0,xmean);
      newClus->set_xyz_global(1,ymean);
      newClus->set_xyz_global(2,zmean);
      merged.push_back(newClus);
    }

//   std::cout << "Total clusters = " << clus.size() << std::endl;
//   std::cout << "Total comps size = " << comps.size() << std::endl;
//   std::cout << "Total joined clusters = " << num << std::endl;
//   std::cout << "Total merged clusters = " << num << std::endl;

  return;
}
