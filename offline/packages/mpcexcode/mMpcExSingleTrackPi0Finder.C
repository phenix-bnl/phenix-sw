#include "mMpcExSingleTrackPi0Finder.h"
#include "TMpcExSingleTrackPi0.h"
#include <TH2D.h>
#include <Exogram.h>
#include <MpcExConstants.h>
#include <TMpcExHitContainer.h>
#include <TMpcExShower.h>
#include <TSpectrum.h>
#include "mMpcExMiniClusters.h"

mMpcExSingleTrackPi0Finder::mMpcExSingleTrackPi0Finder(){
   _peakFinder = new TSpectrum();
  _exo = new Exogram("tempExo", "", 300,-20,20, 300,-20,20, 8,-0.5,7.5, 1);
  _exo->SetDirectory(NULL);
  _ex_mini_clusters = new mMpcExMiniClusters();
  _cutSmooth = 1;
  _cutSigma = 1;
  _cutHeight=0.09;
  _method = MINICLUSTER;
 
}


mMpcExSingleTrackPi0Finder::~mMpcExSingleTrackPi0Finder(){
  delete _peakFinder;
  delete _exo;
  if(_ex_mini_clusters) delete _ex_mini_clusters;
  _peakFinder = NULL;
  _exo = NULL;
  _ex_mini_clusters = NULL;
}

TMpcExSingleTrackPi0* mMpcExSingleTrackPi0Finder::ReconstructSingleTrackPi0(TMpcExShower* shower, TMpcExHitContainer* hits, float mindist){
  if(_method==TSPECTRUM) return TSpecPi0Reco(shower,hits);
  else return MiniClusterPi0Reco(shower,hits,mindist);
}

TMpcExSingleTrackPi0* mMpcExSingleTrackPi0Finder::MiniClusterPi0Reco(TMpcExShower* shower,
                                                                     TMpcExHitContainer* hits, 
								     float mindist){
  //it will reset for new shower
  _ex_mini_clusters->ConstructMiniClusters(shower,hits);
  //only pick the first two ones
  //the first peak has the highest peak energy
  //the second peak has highest Ex energy except the first one
  std::vector<TMpcExMiniCluster*> mclus_list = _ex_mini_clusters->GoodMiniClusters(mindist);
  
  if(mclus_list.size()<2) return NULL;

  TMpcExSingleTrackPi0 *pi0 = new TMpcExSingleTrackPi0(shower);

  for(unsigned int i=0;i<2;i++){
    TMpcExMiniCluster* mclus =  mclus_list[i];

    pi0->AddPeak(0,mclus->GetX(),mclus->GetExE());
    pi0->AddPeak(1,mclus->GetY(),mclus->GetExE());
    pi0->AddMiniCluster(mclus->Clone());

  }

  return pi0;
  
}


TMpcExSingleTrackPi0* mMpcExSingleTrackPi0Finder::TSpecPi0Reco(TMpcExShower* shower,
                                                               TMpcExHitContainer* hits){
   _exo->Reset();
  for(unsigned ihit=0; ihit<shower->sizeHits(); ihit++)
    {
      //TMpcExHit *hit_ptr = hits->get_hit_by_key(shower->getHit(ihit));
      TMpcExHit *hit_ptr = shower->getHit(ihit,hits);
      _exo->FillEx(hit_ptr->key(), hit_ptr->combined());
    }//for(unsigned ihit=0; ihit<shower->sizeHits(); ihit++)

  TH2D *h2d_odd = new TH2D("h2d_odd", ";x;y", 300,-20,20, 300,-20,20);
  TH2D *h2d_even = new TH2D("h2d_even", ";x;y", 300,-20,20, 300,-20,20);
  TH2D *h2d = new TH2D("h2d", ";x;y", 300,-20,20, 300,-20,20);

  // add even layers together and odd layers together
  for(int ilayer=0; ilayer<MpcExConstants::NLAYERS; ilayer++)
    {
      TH2D *tmp = _exo->GetLayer(ilayer);
      if((ilayer+1)%2!=0) h2d_odd->Add(tmp);
      else h2d_even->Add(tmp);
      h2d->Add(tmp);
      delete tmp;
    }

  //if any layer sums has zero entries, we won't find two peaks
  //so we bail 
  if(h2d_even->GetEntries()==0 || h2d_odd->GetEntries()==0) {
    delete h2d_even;
    delete h2d_odd;
    delete h2d;
    return NULL;
  }

  //here is the pi0 object that we will fill and return if valid
  TMpcExSingleTrackPi0 *pi0 = new TMpcExSingleTrackPi0(shower);

  // look for peaks with tspectrum in the odd and even summed histograms
  for(int i_dim=0; i_dim<2; i_dim++) //For Odd(x) and Even(y)
    {
      TH1D *h_proj = (i_dim==0) ? h2d_odd->ProjectionX("h_proj_odd_x")
	: h2d_even->ProjectionY("h_proj_even_y");
      h_proj->Smooth(_cutSmooth);

      //do the peak finding in this particular dimension
      int nfound = _peakFinder->Search(h_proj,_cutSigma,"goff",_cutHeight);

      //not a valid pi0 if no peaks are found in a dimension: return NULL
      if(nfound == 0){
	delete pi0;
	delete h_proj;
	delete h2d_even;
	delete h2d_odd;
	delete h2d;
	return NULL;
      }

      for (int i_peak=0; i_peak<nfound; i_peak++) //1st and 2nd peak
	{
	  double d_peak_pos = _peakFinder->GetPositionX()[i_peak];
	  double d_peak_val = h_proj->GetBinContent(h_proj->FindBin(d_peak_pos));
	  //	  double d_peak_val = _peakFinder->GetPositionY()[i_peak];

	  if (std::abs(d_peak_pos) > 1.e-4 && std::abs(d_peak_pos) < 20.)
	    {
	      pi0->AddPeak(i_dim,d_peak_pos,d_peak_val);
	    }//if a valid peak
	}//loop over two peaks
      delete h_proj;
    }//loop over dimension
  delete h2d_even;
  delete h2d_odd;
  delete h2d;

  //Determine patID
  TMpcExSingleTrackPi0::Pattern patID = pi0->GetPeakPattern();

  //if there is one peak in each, not a valid pi0: return NULL
  if(patID == TMpcExSingleTrackPi0::kX1Y1){
    delete pi0;
    return NULL;
  }

  //all done
  return pi0;
}
