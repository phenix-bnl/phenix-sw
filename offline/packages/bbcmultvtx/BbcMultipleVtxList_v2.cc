#include "BbcMultipleVtxList_v2.hh"
#include "BbcMultipleVtxCluster_v1.hh"
#include "BbcMultipleVtxPoint_v1.hh"

#include <phool.h>

#include <TClonesArray.h>

#include <cstdlib>

using namespace std;

ClassImp(BbcMultipleVtxList_v2)

BbcMultipleVtxList_v2::BbcMultipleVtxList_v2()
: binsize(0.0)
  , clusters_north( new TClonesArray("BbcMultipleVtxCluster_v1", 1000) )
  , clusters_south( new TClonesArray("BbcMultipleVtxCluster_v1", 1000) )
  , vertex_points( new TClonesArray("BbcMultipleVtxPoint_v1", 1000) )
{
  clusters_north->SetOwner(kTRUE);
  clusters_south->SetOwner(kTRUE);
  vertex_points->SetOwner(kTRUE);
}

BbcMultipleVtxList_v2::~BbcMultipleVtxList_v2()
{
  clusters_north->Delete();
  clusters_south->Delete();
  vertex_points->Delete();
  delete clusters_north;
  delete clusters_south;
  delete vertex_points;
}

void BbcMultipleVtxList_v2::Reset()
{
  clusters_north->Clear();
  clusters_south->Clear();
  vertex_points->Clear();
}

void BbcMultipleVtxList_v2::print()
{
  cout << Form("BbcMultipleVtxList_v2:: Bin Size = %.2f ns", binsize) << endl;
  cout << Form("  Cluster North") << endl;
  for (Int_t iclus = 0; iclus < clusters_north->GetEntries(); iclus++)
    {
      this->get_cluster(Bbc::North, iclus)->print();
    }

  cout << Form("  Cluster South") << endl;
  for (Int_t iclus = 0; iclus < clusters_south->GetEntries(); iclus++)
    {
      this->get_cluster(Bbc::South, iclus)->print();
    }

  cout << Form("  Vertex Point") << endl;
  for (Int_t ivtx = 0; ivtx < vertex_points->GetEntries(); ivtx++)
    {
      this->get_vertex_point(ivtx)->print();
    }

  cout << "  ----------\n" << endl;

}

void BbcMultipleVtxList_v2::add_cluster(const int arm, const int order, const int size, const float tof)
{
  if ( Bbc::South == arm )
    {
      TClonesArray& clusters = *clusters_south;
      new(clusters[clusters.GetEntries()]) BbcMultipleVtxCluster_v1();
      BbcMultipleVtxCluster *cluster
	= (BbcMultipleVtxCluster*)( clusters.At(clusters.GetEntries() - 1) );
      cluster->set_order( order );
      cluster->set_size( size );
      cluster->set_tof( tof );

    }
  else if ( Bbc::North == arm )
    {
      TClonesArray& clusters = *clusters_north;
      new(clusters[clusters.GetEntries()]) BbcMultipleVtxCluster_v1();
      BbcMultipleVtxCluster *cluster
	= (BbcMultipleVtxCluster*)( clusters.At(clusters.GetEntries() - 1) );
      cluster->set_order( order );
      cluster->set_size( size );
      cluster->set_tof( tof );

    }
  else
    {
    }
}

void BbcMultipleVtxList_v2::add_vtx_point(const int order_south, const int order_north,
					  const float vtxz, const float t0)
{
  TClonesArray& points = *vertex_points;
  new(points[points.GetEntries()]) BbcMultipleVtxPoint_v1();
  BbcMultipleVtxPoint *point
    = (BbcMultipleVtxPoint*)(points.At(points.GetEntries() - 1));
  point->set_cluster_order( Bbc::South, order_south );
  point->set_cluster_order( Bbc::North, order_north );
  point->set_vtxz( vtxz );
  point->set_t0( t0 );
}


const int BbcMultipleVtxList_v2::get_cluster_number(const int arm) const
{
  if ( Bbc::South == arm )
    {
      return clusters_south->GetEntries();
    }
  else if ( Bbc::North == arm )
    {
      return clusters_north->GetEntries();
    }
  else
    {
      return -1;
    }
}

BbcMultipleVtxCluster* BbcMultipleVtxList_v2::get_cluster(const int arm, const int order) const
{
  if ( Bbc::South == arm )
    {
      if ( order < clusters_south->GetEntries() )
        {
          return (BbcMultipleVtxCluster*)(clusters_south->At(order));
        }
      else
        {
          return 0;
        }
    }
  else if ( Bbc::North == arm )
    {
      if ( order < clusters_north->GetEntries() )
        {
          return (BbcMultipleVtxCluster*)(clusters_north->At(order));
        }
      else
        {
          return 0;
        }
    }
  else
    {
      return 0;
    }

}

BbcMultipleVtxPoint* BbcMultipleVtxList_v2::get_vertex_point(const int n) const
{
  if ( n < vertex_points->GetEntries() )
    {
      return (BbcMultipleVtxPoint*)(vertex_points->At(n));
    }
  else
    {
      return 0;
    }
}

const int BbcMultipleVtxList_v2::get_cluster_size(const int arm, const int order) const
{
  if ( Bbc::South == arm )
    {
      if ( order < clusters_south->GetEntries() )
        {
          return ((BbcMultipleVtxCluster*)(clusters_south->At(order)))->get_size();
        }
      else
        {
          return -1;
        }
    }
  else if ( Bbc::North == arm )
    {
      if ( order < clusters_north->GetEntries() )
        {
          return ((BbcMultipleVtxCluster*)(clusters_north->At(order)))->get_size();
        }
      else
        {
          return -1;
        }
    }
  else
    {
      cout << PHWHERE << "Invalid arm " << arm << endl;
      exit(-1);
    }
}

const float BbcMultipleVtxList_v2::get_cluster_tof(const int arm, const int order) const
{
  if ( Bbc::South == arm )
    {
      if ( order < clusters_south->GetEntries() )
        {
          return ((BbcMultipleVtxCluster*)(clusters_south->At(order)))->get_tof();
        }
      else
        {
          return -9999.0;
        }
    }
  else if ( Bbc::North == arm )
    {
      if ( order < clusters_north->GetEntries() )
        {
          return ((BbcMultipleVtxCluster*)(clusters_north->At(order)))->get_tof();
        }
      else
        {
          return -9999.0;
        }
    }
  else
    {
      cout << PHWHERE << "Invalid arm " << arm << endl;
      exit(-1);
    }
}


const int BbcMultipleVtxList_v2::get_vertex_number() const
{
  return vertex_points->GetEntries();
}


const float BbcMultipleVtxList_v2::get_vertex_z(const int n) const
{
  if ( n < vertex_points->GetEntries() )
    {
      return ((BbcMultipleVtxPoint*)(vertex_points->At(n)))->get_vtxz();
    }
  else
    {
      return -9999.0;
    }
}


const float BbcMultipleVtxList_v2::get_vertex_t0(const int n) const
{
  if ( n < vertex_points->GetEntries() )
    {
      return ((BbcMultipleVtxPoint*)(vertex_points->At(n)))->get_t0();
    }
  else
    {
      return -9999.0;
    }
}
