#include "MuoAdcToMutooHit.h"

#include <TMath.h>

#include "Fun4AllReturnCodes.h"
#include "recoConsts.h"
#include "getClass.h"
#include "MuonUtil.h"
#include "mMutCalibrate.h"
#include "mMutUnpackPar.h"
#include "mMutZeroSupPar.h"
#include "mMutCalibratePar.h"
#include "TMutHitMap.h"
#include "PHMuoTracksOut.h"
#include "PHMuoTracksAdc.h"
#include "PHMuoTrackAdc.h"

const int MuoAdcToMutooHit::MAX_GAP[3]={3,3,2};

///////////////////////////////////////////////////////////

MuoAdcToMutooHit::MuoAdcToMutooHit(const char *name) : MuonSubsysReco(name){
}

///////////////////////////////////////////////////////////

MuoAdcToMutooHit::~MuoAdcToMutooHit(void){
  if(mut_calib_mod){delete mut_calib_mod; mut_calib_mod=NULL;}
}

///////////////////////////////////////////////////////////

int MuoAdcToMutooHit::InitRun(PHCompositeNode *top_node){
  // Create Node Tree
  CreateNodeTree(top_node);

  // Initialize Database
  MuonUtil::initialize_database(top_node,true,true);

  mut_calib_mod=new mMutCalibrate();

  return 0;
}

///////////////////////////////////////////////////////////////////

int MuoAdcToMutooHit::process_event(PHCompositeNode *top_node){
  muo_trks=findNode::getClass<PHMuoTracksOut>(top_node,"PHMuoTracksOO");
  muo_adcs=findNode::getClass<PHMuoTracksAdc>(top_node,"PHMuoTracksAdc");
  mut_hitmap=findNode::getClass<TMutHitMap>(top_node,"TMutHitMap");

  if(!muo_trks || !muo_adcs || !mut_hitmap){
    printf("Error - %s : Missing Objects.",Name());
    printf("Error - %s : PHMuoTracksOut = %p\n",Name(),muo_trks);
    printf("Error - %s : PHMuoTracksAdc = %p\n",Name(),muo_adcs);
    printf("Error - %s : TMutHitMap     = %p\n",Name(),mut_hitmap);
    return ABORTRUN;
  }

  // clear maps
  mut_hitmap->clear();

  FillHitAdc();

  mut_calib_mod->event(top_node);

  write_maps_if_needed();

  return 0;
}

////////////////////////////////////////////////////////////////////////

int MuoAdcToMutooHit::CreateNodeTree(PHCompositeNode *top_node){
  PHNodeIterator itr_top(top_node);

  // Prepare DST node
  PHCompositeNode *dst_node=
    dynamic_cast<PHCompositeNode*>(itr_top.findFirst("PHCompositeNode","DST"));
  if(!dst_node){
    dst_node=new PHCompositeNode("DST");
    top_node->addNode(dst_node);
  }

  // Prepare MUTOO node
  PHCompositeNode *mutoo_node=
    dynamic_cast<PHCompositeNode*>(itr_top.findFirst("PHCompositeNode","MUTOO"));
  if(!mutoo_node){
    mutoo_node=new PHCompositeNode("MUTOO");
    top_node->addNode(mutoo_node);
  }

  // Add TMutHitMap objects
  TMutHitMap *mut_hitmap=
    findNode::getClass<TMutHitMap>(mutoo_node,"TMutHitMap");
  if(!mut_hitmap){
    mut_hitmap=TMutNode<TMutHitMap>::new_node(mutoo_node,"TMutHitMap");
    mut_hitmap->make_persistant(dst_node,"TMutHit");
  }

  // Add mMutUnpackPar object
  mMutUnpackPar* mMutUnpack_par=
    findNode::getClass<mMutUnpackPar>(mutoo_node,"mMutUnpackPar");
  if(!mMutUnpack_par){
    mMutUnpack_par=
      TMutNode<mMutUnpackPar>::new_node(mutoo_node,"mMutUnpackPar");
    mMutUnpack_par->set_verbosity(MUTOO::NONE);
    mMutUnpack_par->set_check_user_word(false);
    mMutUnpack_par->set_check_detector_id(false);
  }

  // Add mMutZeroSupPar object
  mMutZeroSupPar* mMutZeroSup_par=
    findNode::getClass<mMutZeroSupPar>(mutoo_node,"mMutZeroSuppar");
  if(!mMutZeroSup_par){
    mMutZeroSup_par=
      TMutNode<mMutZeroSupPar>::new_node(mutoo_node,"mMutZeroSupPar");
    mMutZeroSup_par->set_verbosity(MUTOO::NONE);
  }

  // Add mMutCalibratePar object
  mMutCalibratePar* mMutCalibrate_par=
    findNode::getClass<mMutCalibratePar>(mutoo_node,"mMutCalibratePar");
  if(!mMutCalibrate_par){
    mMutCalibrate_par=
      TMutNode<mMutCalibratePar>::new_node(mutoo_node,"mMutCalibratePar");
  }

  // Print parameters
  if(recoConsts::instance()->get_IntFlag("PRINT_MUTOO_PARAMETERS",1)){
    mMutUnpack_par->print();
    mMutZeroSup_par->print();
    mMutCalibrate_par->print();
  }

  return 0;
}

////////////////////////////////////////////////////////////////////////

int MuoAdcToMutooHit::FillHitAdc(void){
  for(unsigned int itrk=0; itrk<muo_trks->get_npart(); itrk++){
    unsigned int uid_trk=muo_trks->get_uid(itrk);

    PHMuoTrackAdc *muo_adc=NULL;
    for(unsigned int itrk2=0; itrk2<muo_adcs->GetSize(); itrk2++){
      unsigned int uid_adc=muo_adcs->Get(itrk2)->GetTrackUid();
      if(uid_trk==uid_adc){muo_adc=muo_adcs->Get(itrk2); break;}
    }

    if(!muo_adc){
      printf("Error - %s : No associated PHMuoTrackAdc.\n",Name());
      continue;
    }

    for(int ist=0; ist<MUTOO::MAX_STATION; ist++){
      double xpos=muo_trks->get_xpos(ist+1,itrk);
      double ypos=muo_trks->get_ypos(ist+1,itrk);
      double zpos=muo_trks->get_zpos(ist+1,itrk);

      int arm,oct,hoct;
      GetArmOct(xpos,ypos,zpos,arm,oct,hoct);

      for(int igap=0; igap<MAX_GAP[ist]; igap++){
	for(int icath=0; icath<MUTOO::MAX_CATHODE; icath++){

	  int nstp=(int)muo_adc->GetNStrip(ist,igap,icath);
	  for(int istp=0; istp<nstp; istp++){
	    int adc=(int)muo_adc->GetAdc(ist,igap,icath,istp,2);
	    if(adc==0){continue;} // skip empty hit by looking at ADC2

	    int strip=muo_adc->GetStrip(ist,igap,icath,istp);

	    TMutHitMap::iterator hit_itr=
	      mut_hitmap->insert_new(arm,ist,oct,hoct,igap,icath,strip);

	    for(int iadc=0; iadc<PHMuoTrackAdc::N_ADC_SAMPLE; iadc++){
	      int adc=(int)muo_adc->GetAdc(ist,igap,icath,istp,iadc);
	      hit_itr->get()->set_adc(iadc,adc);
	    }
	  }
	}
      }
    }
  }

  return 0;
}

////////////////////////////////////////////////////////////////////////

int MuoAdcToMutooHit::GetArmOct(double xpos,double ypos,double zpos,
				int &arm,int &oct,int &hoct){
  const double DPHI_HOCT=TMath::Pi()/8.0;

  double phi=TMath::Pi()+TMath::ATan2(-ypos,-xpos);
  int hoct_num=((int)(phi/DPHI_HOCT)+1)%16;

  arm=(zpos>0.0 ? 1 : 0);
  oct=(int)(hoct_num/2);
  hoct=hoct_num%2;

  return 0;
}

////////////////////////////////////////////////////////////////////////
