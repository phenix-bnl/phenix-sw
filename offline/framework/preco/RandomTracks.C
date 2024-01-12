#include "RandomTracks.h"

#include "getClass.h"
#include "PHCompositeNode.h"
#include "PHParticle.h"

#include <vector>
#include <algorithm>

using namespace std;
using namespace findNode;

// 3 constructors -- input one or two track types as char *, or many in vector form
RandomTracks::RandomTracks(vector<string> trktype)
{
  //  cout << "RandomTracks vector constructor " << endl;
  TrackTypes = trktype;

  nTypes = TrackTypes.size();
//  for (unsigned int i = 0; i<nTypes; i++)
//    {
//      cout << TrackTypes[i] << " ";
//    }
//  cout << nTypes << endl;

  ncalls = 0;
  verbosity = 0;
  return;
}
RandomTracks::RandomTracks(const char *ATrack)
{
  //  cout << "RandomTracks string constructor " << endl;

  string trk = (string) ATrack;
  TrackTypes.push_back(trk); 
  nTypes = TrackTypes.size();
  //  cout << ATrack << " " << TrackTypes[0] << " " << nTypes << endl;
  ncalls = 0;
  verbosity = 0;
  return;
}
RandomTracks::RandomTracks(const char *ATrack1, const char *ATrack2)
{
  //  cout << "RandomTracks 2 string constructor " << endl;

  string trk1 = (string) ATrack1;
  string trk2 = (string) ATrack2;
  TrackTypes.push_back(trk1); 
  TrackTypes.push_back(trk2); 
  nTypes = TrackTypes.size();
  //  cout << ATrack1 << " " << TrackTypes[0] << " " << ATrack2 << " " << TrackTypes[1] << " " << nTypes << endl;
  ncalls = 0;
  verbosity = 0;
  return;
}

int RandomTracks::process_event(PHCompositeNode *topNode)
{
  if(++ncalls%1000==0) cout << "RandomTracks:: process_event: " << ncalls << endl;

  //  cout << "make vectors and clear them" << endl;
  vector<PHParticle *> Originals;
  vector<PHParticle *> Copies;
  vector<unsigned int> nTracks;
  Originals.clear();
  Copies.clear();
  nTracks.clear();

  //  cout << " make Copies clones of PHParticles of types from input, get nTracks and clear Originals " << endl;
  for (unsigned int j=0; j < nTypes; j++)
    {
      string AType = TrackTypes[j];
      //      cout << AType << endl;
      Originals.push_back(getClass<PHParticle>(topNode,AType.c_str()));
      Copies.push_back   (Originals[j]->clone());
      nTracks.push_back  (Copies[j]->get_npart());

      Originals[j]->Reset();
    }

  //  cout << "ensure that nTracks is the same for all types -- only do if there are more than one type" << endl;
  if (nTypes>1)
    {
      for (unsigned int j=0; j < nTypes-1; j++)
	{
	  if (nTracks[j] != nTracks[j+1])
	    {
	      cout << PHWHERE << "Error: " << TrackTypes[j] << " and " << TrackTypes[j+1];
	      cout << " have different numbers of tracks " << nTracks[j] << " and " << nTracks[j+1];
	      cout << " -- will NOT shuffle track order" << endl;
	      return -1;
	    }
	}
    }

  //  cout << " make and shuffle track indices vector of size " << nTracks[0] << endl;
  vector<unsigned int> indices;
  for (unsigned int i=0; i<nTracks[0]; i++)
    {
      indices.push_back(i);
      //      cout << i << "  " << indices[i] << endl;
    }
  random_shuffle(indices.begin(), indices.end());

  if (verbosity>0)
    {
      cout << " new order: " << endl;
      for (unsigned int i=0; i<nTracks[0]; i++)
	{
	  cout << i << "  " << indices[i] << endl;
	}
    }

  //  cout <<" assign shuffled tracks to the Originals -- same shuffle order for all data types" << endl;
  for(unsigned int i=0; i < nTracks[0]; i++)
    {
       for (unsigned int j=0; j < nTypes; j++)
	 { 
	   //	   cout << "swap " << i << " for " << indices[i] << " on type " << TrackTypes[j] << endl;
	   Originals[j]->AddPHParticle(i, Copies[j]->GetSingleParticle(indices[i]));
	 }
    }

  // place shuffled order into the top node ??


  //  cout << "delete each pointer in Copies " << endl;
  for (unsigned int j=0; j < nTypes; j++)
    {
      //      cout << "delete " << j << endl;
      Copies[j]->Delete();
    }

  return 0;
}

