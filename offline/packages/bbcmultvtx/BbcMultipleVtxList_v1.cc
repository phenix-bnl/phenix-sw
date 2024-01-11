#include "BbcMultipleVtxList_v1.hh"
#include "BbcMultipleVtxCluster.hh"
#include "BbcMultipleVtxPoint.hh"

#include <phool.h>

#include <TObjArray.h>

#include <cstdlib>

using namespace std;

ClassImp(BbcMultipleVtxList_v1)

BbcMultipleVtxList_v1::BbcMultipleVtxList_v1()
: binsize(0.0)
  , clusters_north( new TObjArray() )
  , clusters_south( new TObjArray() )
  , vertex_points( new TObjArray() )
{
  clusters_north->SetOwner(kTRUE);
  clusters_south->SetOwner(kTRUE);
  vertex_points->SetOwner(kTRUE);
}

BbcMultipleVtxList_v1::~BbcMultipleVtxList_v1()
{
  clusters_north->Delete();
  clusters_south->Delete();
  vertex_points->Delete();
  delete clusters_north;
  delete clusters_south;
  delete vertex_points;
}

void BbcMultipleVtxList_v1::Reset()
{
  clusters_north->Delete();
  clusters_south->Delete();
  vertex_points->Delete();
}

void BbcMultipleVtxList_v1::print()
{
  cout << Form("BbcMultipleVtxList_v1:: Bin Size = %.2f ns", binsize) << endl;
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

void BbcMultipleVtxList_v1::add_cluster(const int arm, BbcMultipleVtxCluster *cluster)
{
  if ( Bbc::South == arm )
    {
      clusters_south->Add( cluster );
    }
  else if ( Bbc::North == arm )
    {
      clusters_north->Add( cluster );
    }
  else
    {
    }

}

void BbcMultipleVtxList_v1::add_vtx_point(BbcMultipleVtxPoint *vtxpoint)
{
  vertex_points->Add( vtxpoint );
}


const int BbcMultipleVtxList_v1::get_cluster_number(const int arm) const
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

BbcMultipleVtxCluster* BbcMultipleVtxList_v1::get_cluster(const int arm, const int order) const
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

BbcMultipleVtxPoint* BbcMultipleVtxList_v1::get_vertex_point(const int n) const
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

const int BbcMultipleVtxList_v1::get_cluster_size(const int arm, const int order) const
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

const float BbcMultipleVtxList_v1::get_cluster_tof(const int arm, const int order) const
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


const int BbcMultipleVtxList_v1::get_vertex_number() const
{
  return vertex_points->GetEntries();
}


const float BbcMultipleVtxList_v1::get_vertex_z(const int n) const
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


const float BbcMultipleVtxList_v1::get_vertex_t0(const int n) const
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
