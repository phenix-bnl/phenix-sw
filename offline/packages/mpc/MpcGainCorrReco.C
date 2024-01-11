//  GENERal PHENIX tools
#include <Fun4AllServer.h>
#include <getClass.h>
#include <PHCompositeNode.h>
#include <phool.h>
#include <Fun4AllHistoManager.h>
#include <Fun4AllReturnCodes.h>
#include <RunToTime.hh>


//  Data classes I am using in analysis
#include <TriggerHelper.h>
#include <TrigLvl1.h>
#include <mpcTowerContainer.h>
#include <mpcTowerContent.h>
#include <MpcGainCorrReco.h>
#include <MpcMap.h>
#include <RunHeader.h>

//  Root histogram types
#include <TH1.h>
#include <TH2.h>
#include <TCanvas.h>
#include <TFile.h>
#include <TLorentzVector.h>
#include <TVector3.h>
#include <cmath>
#include <TString.h>


using namespace std;
using namespace findNode;

namespace {
const float PI0_MASS = 0.1349766;
}

MpcGainCorrReco::MpcGainCorrReco(const char* name) : SubsysReco(name)
{
  return;
}

MpcGainCorrReco::~MpcGainCorrReco()
{
  delete trighelp;
}

int MpcGainCorrReco::InitRun(PHCompositeNode *topNode)
{
  mpcmap = getClass<MpcMap>(topNode, "MpcMap");
/*
  if ( new_mpcmap==0 ) new_mpcmap = new MpcMap(topNode);
  else
    {
      // get new MpcMap since the run number has changed
      
      delete new_mpcmap;
      new_mpcmap = new MpcMap(topNode);
    }
*/
  
  
  trighelp = new TriggerHelper(topNode);
  if ( trighelp==0 )
    {
      cout << "MpcGainCorrReco::InitRun, TriggerHelper not found" << endl;
      return ABORTRUN;
    }
  runheader = getClass<RunHeader>   (topNode, "RunHeader");
  if(!runheader)
    {
      cout << "RunHeader not found\n";
      return ABORTRUN;
    }
  runnum = runheader -> get_RunNumber();
  daflag = 1;
  if(runnum>=254945) daflag = 0;
  
  
  float time = GetTime(runnum);
  InitGainParam(daflag, time);
  SetGainCorr(time);
  

  return 0;
}


int MpcGainCorrReco::Init(PHCompositeNode *topNode)
{
  
  return 0;
}

int MpcGainCorrReco::process_event(PHCompositeNode *topNode)
{
  mpcmap = getClass<MpcMap>(topNode, "MpcMap");
  
  // informational message...
  static int ncalls = 0;
  ncalls++;
  if (ncalls % 1000 == 0 && verbosity> 2)
    {
      cout << "MpcGainCorrReco Ncalls = " << ncalls << endl;
    }
  
 
  mpctower = getClass<mpcTowerContainer>(topNode,"mpcTowerContainer");
  
  if(!mpctower){
    if(verbosity > 2)
      {
	cout << "cant' find mpctowter\n"
	     << "mpctow: " << mpctower << endl
	     << endl;
      }
    return 0;
  }
    
  int ntow = mpctower->size();
  
  for (int itow=0; itow<ntow; itow++)
    {
      mpcTowerContent *tow = mpctower->getTower(itow);
      int ch = tow->get_ch();
      int gridx = mpcmap->getGridX(ch);
      if(gridx < 0){
	cout << "non-existent channel: " << ch << endl;
	continue;
      }
      
      float e = tow->get_energy();
      
      if(verbosity > 2){
	cout << "Old Tower Energy is: " << tow->get_energy() << endl;
	cout << "gaincorr is " << ch << ": " << gaincorr[ch] << endl;
      }
      float enew = e*gaincorr[ch];
      
      tow->set_energy(enew);
      if(verbosity > 2){
	cout << "New Tower Energy is: " << tow->get_energy() << endl;
	cout << "New Tower Energy2 is: " << enew << endl;
      }
    }      
  return EVENT_OK;
}



int MpcGainCorrReco::End(PHCompositeNode *topNode)
{
  return 0;
}


int MpcGainCorrReco::SetGainCorr(float time)
{

  float scaleN = scaleparamN[0] + scaleparamN[1]*time;
  float scaleS = scaleparamS[0] + scaleparamS[1]*time;
  if(scaleN < 0.00001) scaleN = 1;
  if(scaleS < 0.00001) scaleS = 1;
  
  for(int ich=0;ich<576;ich++){
    gaincorr[ich] = 1.0; //just to ensure all gains are initialized
    int gridx = mpcmap->getGridX(ich);
    if(gridx < 0) continue;
    float gain = gainparam[0][ich] + time*gainparam[1][ich];
    if(gain < 0.00001) continue;
    float netgain = gain/scaleS;
    if(ich >=288) netgain = gain/scaleN;
    gaincorr[ich] = netgain;
  }
  
  return 0;
}


float MpcGainCorrReco::GetTime(int run)
{
  RunToTime* runtotime = RunToTime::instance();
  PHTimeStamp* ts_o = runtotime->getBeginTime(245796);
  float time_o = ts_o->getTics();

  PHTimeStamp* ts_1 = runtotime->getBeginTime(run);
  float time1 = ts_1->getTics();
  PHTimeStamp* ts_2 = runtotime->getEndTime(run);
  float time2 = ts_2->getTics();

  float time_f = (time1+time2)/2.0;
  cout << "run,begin,end,avg: " << run << ", "
       << time1 << ", " << time2 << ", " << time_f << endl;
  
  float time = (time_f - time_o)/3600.0/24.0;
  cout << "time is\t" << time << endl;
  delete ts_o;
  delete ts_1;
  delete ts_2;
  return time;
}


int MpcGainCorrReco::InitGainParam(int type, float time)
{
  scaleparamN[0] = 1;
  scaleparamN[1] = 0;
  scaleparamS[0] = 1;
  scaleparamS[1] = 0;
  
  for(int ich=0;ich<576;ich++){
    gainparam[0][ich] = 1;
    gainparam[1][ich] = 0;
  }

  if(daflag > 0){
    scaleparamS[0] = 1.413329e-01/PI0_MASS;
    scaleparamS[1] = -1.482191e-04/PI0_MASS;
    
    scaleparamN[0] = 1.475462e-01/PI0_MASS;
    scaleparamN[1] = -8.586367e-05/PI0_MASS;
    // 0       1.413329e-01
    // 1       -1.482191e-04
    // Fitting results:
    // Parameters:
    // NO.             VALUE
    // 0       1.475462e-01
    // 1       -8.586367e-05
    
  }
  else if(daflag == 0){
    scaleparamS[0] = 0.138218/PI0_MASS;
    scaleparamS[1] = 0/PI0_MASS;
    
    scaleparamN[0] = 0.144317/PI0_MASS;
    scaleparamN[1] = 0/PI0_MASS;
    
  }

  


  if(daflag > 0){
    gainparam[0][0] = 0.938462019; gainparam[1][0] = 0.00226272992; 
    gainparam[0][1] = 1.04281998; gainparam[1][1] = -3.52419993e-05; 
    gainparam[0][2] =         1; gainparam[1][2] =         0; 
    gainparam[0][3] = 0.750639021; gainparam[1][3] = 0.00371414004; 
    gainparam[0][4] = 1.07123995; gainparam[1][4] = 0.000692623027; 
    gainparam[0][5] = 0.876443982; gainparam[1][5] = 0.00477277022; 
  gainparam[0][6] = 0.887645006; gainparam[1][6] = 0.00453356979; 
  gainparam[0][7] = 1.06437004; gainparam[1][7] = 0.00108405994; 
  gainparam[0][8] = 1.01568997; gainparam[1][8] = 0.00601704977; 
  gainparam[0][9] = 0.753221989; gainparam[1][9] = 0.00824289955; 
  gainparam[0][10] = 1.04894996; gainparam[1][10] = 0.00372804003; 
  gainparam[0][11] = 0.615751982; gainparam[1][11] = 0.00885722972; 
  gainparam[0][12] = 1.26258004; gainparam[1][12] = 0.0071190102; 
  gainparam[0][13] = 0.812449992; gainparam[1][13] = 0.00538682006; 
  gainparam[0][14] = 1.06155002; gainparam[1][14] = 0.00788472965; 
  gainparam[0][15] = 0.837374985; gainparam[1][15] = 0.00556637021; 
  gainparam[0][16] = 0.777719021; gainparam[1][16] = 0.00857769977; 
  gainparam[0][17] = 0.890088022; gainparam[1][17] = 0.0109609002; 
  gainparam[0][18] = 1.10381997; gainparam[1][18] = 0.00765554002; 
  gainparam[0][19] = 0.989937007; gainparam[1][19] = 0.00512090977; 
  gainparam[0][20] = 1.06832004; gainparam[1][20] = -1.37118996e-05; 
  gainparam[0][21] = 0.80625999; gainparam[1][21] = 0.00833025016; 
  gainparam[0][22] = 0.90230298; gainparam[1][22] = 0.00704511022; 
  gainparam[0][23] = 0.847877979; gainparam[1][23] = 0.0124388002; 
  gainparam[0][24] = 0.848622978; gainparam[1][24] = 0.00469083991; 
  gainparam[0][25] = 0.855000973; gainparam[1][25] = 0.00185589003; 
  gainparam[0][26] = 0.832614005; gainparam[1][26] = 0.00239387993; 
  gainparam[0][27] = 0.833818018; gainparam[1][27] = 0.00445111981; 
  gainparam[0][31] = 1.25399005; gainparam[1][31] = -8.4487001e-05; 
  gainparam[0][41] = 0.97724098; gainparam[1][41] = 0.00390229002; 
  gainparam[0][44] = 0.783828974; gainparam[1][44] = 0.00507640978; 
  gainparam[0][45] = 1.12074995; gainparam[1][45] = -0.00212599011; 
  gainparam[0][46] = 0.83870697; gainparam[1][46] = 0.00726283016; 
  gainparam[0][47] = 0.722832024; gainparam[1][47] = 0.00652230019; 
  gainparam[0][48] = 0.750777006; gainparam[1][48] = 0.00498015014; 
  gainparam[0][49] = 0.917604029; gainparam[1][49] = 0.00254676002; 
  gainparam[0][50] = 0.690432012; gainparam[1][50] = 0.00573313981; 
  gainparam[0][51] = 0.72556901; gainparam[1][51] = 0.00592414988; 
  gainparam[0][68] = 0.673186004; gainparam[1][68] = 0.00608095992; 
  gainparam[0][69] = 0.716257989; gainparam[1][69] = 0.00472312979; 
  gainparam[0][70] = 1.08256996; gainparam[1][70] = -0.00038721101; 
  gainparam[0][71] = 0.834173024; gainparam[1][71] = 0.00429194979; 
  gainparam[0][72] = 0.825893998; gainparam[1][72] = 0.00612206012; 
  gainparam[0][73] = 0.901187003; gainparam[1][73] = 0.00130447; 
  gainparam[0][74] = 0.826865017; gainparam[1][74] = 0.0038322499; 
  gainparam[0][75] = 0.778742015; gainparam[1][75] = 0.00587804988; 
  gainparam[0][92] = 0.729701996; gainparam[1][92] = 0.00622458011; 
  gainparam[0][93] = 0.827093005; gainparam[1][93] = 0.0105934003; 
  gainparam[0][94] =         1; gainparam[1][94] =         0; 
  gainparam[0][95] =         1; gainparam[1][95] =         0; 
  gainparam[0][96] =         1; gainparam[1][96] =         0; 
  gainparam[0][97] =         1; gainparam[1][97] =         0; 
  gainparam[0][98] = 0.961566985; gainparam[1][98] = 0.00898414012; 
  gainparam[0][99] =         1; gainparam[1][99] =         0; 
  gainparam[0][102] = 1.09819996; gainparam[1][102] = 0.00909462012; 
  gainparam[0][112] =         1; gainparam[1][112] =         0; 
  gainparam[0][116] = 0.978500009; gainparam[1][116] = 0.00431548012; 
  gainparam[0][117] = 0.881810009; gainparam[1][117] = 0.00546948984; 
  gainparam[0][118] = 0.891764998; gainparam[1][118] = 0.00864345022; 
  gainparam[0][119] = 0.863955021; gainparam[1][119] = 0.0114780003; 
  gainparam[0][120] = 0.890169024; gainparam[1][120] = 0.0117614996; 
  gainparam[0][121] = 1.01240003; gainparam[1][121] = 0.0063148099; 
  gainparam[0][122] =         1; gainparam[1][122] =         0; 
  gainparam[0][123] =         1; gainparam[1][123] =         0; 
  gainparam[0][124] = 1.04595006; gainparam[1][124] = 0.00791839976; 
  gainparam[0][125] = 1.20203996; gainparam[1][125] = 0.00475643994; 
  gainparam[0][126] = 1.19052005; gainparam[1][126] = 0.0101958998; 
  gainparam[0][127] = 0.974659026; gainparam[1][127] = 0.00796461012; 
  gainparam[0][128] = 0.770950019; gainparam[1][128] = 0.00644274987; 
  gainparam[0][129] = 0.881148994; gainparam[1][129] = 0.00584683986; 
  gainparam[0][130] = 0.868130028; gainparam[1][130] = 0.00521887979; 
  gainparam[0][131] = 0.809378028; gainparam[1][131] = 0.0122971004; 
  gainparam[0][132] = 0.836638987; gainparam[1][132] = 0.00458370009; 
  gainparam[0][133] = 1.38706005; gainparam[1][133] = 0.00613535009; 
  gainparam[0][134] = 0.858503997; gainparam[1][134] = 0.00575402007; 
  gainparam[0][135] = 1.09213996; gainparam[1][135] = 0.00761276996; 
  gainparam[0][136] = 0.899919987; gainparam[1][136] = 0.00537161017; 
  gainparam[0][137] = 0.81408602; gainparam[1][137] = 0.00386821991; 
  gainparam[0][138] = 0.927694976; gainparam[1][138] = 0.00235720002; 
  gainparam[0][139] = 0.886514008; gainparam[1][139] = 0.00779272011; 
  gainparam[0][140] = 0.85454601; gainparam[1][140] = 0.00621635979; 
  gainparam[0][141] =  0.913773; gainparam[1][141] = 0.00859215017; 
  gainparam[0][142] = 1.11222005; gainparam[1][142] = 0.00251562009; 
  gainparam[0][143] = 0.962273002; gainparam[1][143] = 0.00537094986; 
  gainparam[0][144] =         1; gainparam[1][144] =         0; 
  gainparam[0][145] = 1.03146005; gainparam[1][145] = 0.00890299026; 
  gainparam[0][146] = 0.843124986; gainparam[1][146] = 0.00447413977; 
  gainparam[0][147] = 0.977832019; gainparam[1][147] = 0.00830767024; 
  gainparam[0][148] = 0.88418299; gainparam[1][148] = 0.00576204015; 
  gainparam[0][149] = 1.25056005; gainparam[1][149] = 0.00516963005; 
  gainparam[0][150] =  0.896375; gainparam[1][150] = 0.00883792993; 
  gainparam[0][151] = 0.975344002; gainparam[1][151] = 0.00816132966; 
  gainparam[0][152] = 0.774541974; gainparam[1][152] = 0.00558116008; 
  gainparam[0][153] = 0.966252983; gainparam[1][153] = 0.00475247018; 
  gainparam[0][154] = 0.827717006; gainparam[1][154] = 0.0068979701; 
  gainparam[0][155] = 1.05581999; gainparam[1][155] = 0.0082947202; 
gainparam[0][156] = 0.675267994; gainparam[1][156] = 0.00650722999; 
 gainparam[0][157] = 0.799825013; gainparam[1][157] = 0.00511546014; 
gainparam[0][158] = 0.690100014; gainparam[1][158] = 0.00529628014; 
 gainparam[0][159] = 0.831596017; gainparam[1][159] = 0.00425645988; 
 gainparam[0][160] = 0.965071976; gainparam[1][160] = 0.00617692014; 
gainparam[0][161] = 0.947727025; gainparam[1][161] = 0.00365204993; 
 gainparam[0][162] = 0.733286023; gainparam[1][162] = 0.00588672981; 
 gainparam[0][163] = 0.821967006; gainparam[1][163] = 0.00642953999; 
 gainparam[0][164] = 0.863902986; gainparam[1][164] = 0.00407721009; 
 gainparam[0][165] = 1.02934003; gainparam[1][165] = 0.00437047984; 
 gainparam[0][166] = 1.02283001; gainparam[1][166] = 0.00695449999; 
 gainparam[0][167] = 0.946048021; gainparam[1][167] = 0.00624587992; 
 gainparam[0][168] = 0.691753983; gainparam[1][168] = 0.00664096978; 
 gainparam[0][169] = 0.858120978; gainparam[1][169] = 0.00696047023; 
gainparam[0][170] = 0.827301979; gainparam[1][170] = 0.00703633996; 
gainparam[0][171] = 0.819754004; gainparam[1][171] = 0.00805934984; 
gainparam[0][172] = 0.869491994; gainparam[1][172] = 0.00615001982; 
gainparam[0][173] = 0.789036989; gainparam[1][173] = 0.00862674974; 
gainparam[0][174] = 0.875957012; gainparam[1][174] = 0.00764852995; 
gainparam[0][175] = 0.891875029; gainparam[1][175] = 0.00920610968; 
gainparam[0][177] = 0.697480023; gainparam[1][177] = 0.00622058986; 
gainparam[0][179] = 0.793471992; gainparam[1][179] = 0.00712908013; 
gainparam[0][181] = 0.708037972; gainparam[1][181] = 0.0051029101; 
gainparam[0][183] = 0.507125974; gainparam[1][183] = 0.00744308019; 
gainparam[0][184] = 0.741369009; gainparam[1][184] = 0.00614360021; 
gainparam[0][185] = 0.999140978; gainparam[1][185] = 0.00751307001; 
gainparam[0][186] = 1.06342006; gainparam[1][186] = 0.00444306014; 
gainparam[0][187] = 0.929046988; gainparam[1][187] = 0.00577097991; 
gainparam[0][188] = 0.811471999; gainparam[1][188] = 0.00712750014; 
gainparam[0][189] = 0.876963973; gainparam[1][189] = 0.00399179012; 
gainparam[0][190] = 0.659592986; gainparam[1][190] = 0.00669874018; 
gainparam[0][191] = 0.788437009; gainparam[1][191] = 0.00782861002; 
gainparam[0][192] = 0.521359026; gainparam[1][192] = 0.00591494003; 
gainparam[0][193] = 0.517413974; gainparam[1][193] = 0.00553207006; 
gainparam[0][194] = 0.602981985; gainparam[1][194] = 0.00743830018; 
gainparam[0][195] = 0.610988021; gainparam[1][195] = 0.0135051003; 
gainparam[0][198] = 0.627156019; gainparam[1][198] = 0.00820870977; 
gainparam[0][199] = 0.710907996; gainparam[1][199] = 0.00273337006; 
gainparam[0][208] = 0.570641994; gainparam[1][208] = 0.00214692997; 
gainparam[0][209] = 0.508309007; gainparam[1][209] = 0.00476462999; 
gainparam[0][212] =         1; gainparam[1][212] =         0; 
gainparam[0][213] = 0.914409995; gainparam[1][213] = 0.00101611996; 
gainparam[0][214] = 0.583130002; gainparam[1][214] = 0.00387652009; 
gainparam[0][215] = 0.555638015; gainparam[1][215] = 0.0050559002; 
gainparam[0][216] = 0.709619999; gainparam[1][216] = 0.00630353997; 
gainparam[0][217] = 0.586551011; gainparam[1][217] = 0.0087736398; 
gainparam[0][218] = 0.857559025; gainparam[1][218] = 0.00341642997; 
gainparam[0][219] = 0.672819018; gainparam[1][219] = 0.00633230014; 
gainparam[0][222] = 0.839035988; gainparam[1][222] = 0.00391983986; 
gainparam[0][223] = 0.889065981; gainparam[1][223] = 0.00314546004; 
gainparam[0][232] = 0.68382901; gainparam[1][232] = 0.00633519003; 
gainparam[0][233] = 0.600351989; gainparam[1][233] = 0.00705394987; 
gainparam[0][236] = 1.12969005; gainparam[1][236] = 0.00143181998; 
gainparam[0][237] = 0.895253003; gainparam[1][237] = 0.00569935003; 
gainparam[0][238] = 0.634842992; gainparam[1][238] = 0.00655651977; 
gainparam[0][239] = 0.675166011; gainparam[1][239] = 0.00800925028; 
gainparam[0][240] = 0.916040003; gainparam[1][240] = 0.00485047977; 
gainparam[0][241] = 0.557780981; gainparam[1][241] = 0.0143547002; 
gainparam[0][242] = 0.855355024; gainparam[1][242] = 0.0103703998; 
gainparam[0][243] = 0.855410993; gainparam[1][243] = 0.0065843598; 
gainparam[0][244] = 1.05186999; gainparam[1][244] = 0.00297540007; 
gainparam[0][245] = 0.961926997; gainparam[1][245] = 0.00757932011; 
gainparam[0][246] = 0.722064972; gainparam[1][246] = 0.00466084015; 
gainparam[0][247] = 0.835207999; gainparam[1][247] = 0.00713266013; 
gainparam[0][248] = 0.647427022; gainparam[1][248] = 0.00949288998; 
gainparam[0][250] = 0.865478992; gainparam[1][250] = 0.00484789023; 
gainparam[0][252] = 0.828426003; gainparam[1][252] = 0.00977669004; 
gainparam[0][254] = 0.773586988; gainparam[1][254] = 0.00800391007; 
gainparam[0][256] = 0.910970986; gainparam[1][256] = 0.00657240022; 
gainparam[0][257] = 0.945713997; gainparam[1][257] = 0.00200323993; 
gainparam[0][258] = 0.794084013; gainparam[1][258] = 0.00812245999; 
gainparam[0][259] = 0.796509027; gainparam[1][259] = 0.00843331032; 
gainparam[0][260] = 0.862631977; gainparam[1][260] = 0.00852391962; 
gainparam[0][261] = 1.07105994; gainparam[1][261] = 0.00598863978; 
gainparam[0][262] = 0.750440001; gainparam[1][262] = 0.00482524; 
gainparam[0][263] = 0.875837982; gainparam[1][263] = 0.00210743002; 
gainparam[0][264] = 0.704584002; gainparam[1][264] = 0.00619611982; 
gainparam[0][265] = 0.825551987; gainparam[1][265] = 0.00609348016; 
gainparam[0][266] = 0.988834977; gainparam[1][266] = 0.00415517017; 
gainparam[0][267] = 0.749503016; gainparam[1][267] = 0.00774753001; 
gainparam[0][268] = 0.894873023; gainparam[1][268] = 0.00395555981; 
gainparam[0][269] = 0.609806001; gainparam[1][269] = 0.00521994988; 
gainparam[0][270] = 0.765641987; gainparam[1][270] = 0.00394950015; 
gainparam[0][271] = 0.828743994; gainparam[1][271] = 0.00473106001; 
gainparam[0][272] = 0.87569499; gainparam[1][272] = 0.00823307037; 
gainparam[0][273] = 0.660542011; gainparam[1][273] = 0.00778069021; 
gainparam[0][274] = 0.887304008; gainparam[1][274] = 0.00636362005; 
gainparam[0][275] = 0.622287989; gainparam[1][275] = 0.00558272982; 
gainparam[0][276] = 0.870877981; gainparam[1][276] = 0.00768670999; 
gainparam[0][277] = 0.571350992; gainparam[1][277] = 0.00687364023; 
gainparam[0][278] = 0.828576982; gainparam[1][278] = 0.0102567002; 
gainparam[0][279] = 0.647122025; gainparam[1][279] = 0.00643604994; 
gainparam[0][280] = 0.806125998; gainparam[1][280] = 0.00630164985; 
gainparam[0][281] = 0.710645974; gainparam[1][281] = 0.00907386001; 
gainparam[0][282] =  0.791336; gainparam[1][282] = 0.00729899993; 
gainparam[0][283] = 0.540374994; gainparam[1][283] = 0.0109740002; 
gainparam[0][284] =         1; gainparam[1][284] =         0; 
gainparam[0][285] =         1; gainparam[1][285] =         0; 
gainparam[0][286] = 0.712440014; gainparam[1][286] = 0.00829541963; 
gainparam[0][287] = 0.86502099; gainparam[1][287] = 0.00531949988; 
gainparam[0][288] = 0.818965018; gainparam[1][288] = 0.00675893016; 
gainparam[0][289] = 0.767515004; gainparam[1][289] = 0.00338464999; 
gainparam[0][290] = 0.794553995; gainparam[1][290] = 0.00424495991; 
gainparam[0][291] =         1; gainparam[1][291] =         0; 
gainparam[0][292] = 0.83861798; gainparam[1][292] = 0.00580585981; 
gainparam[0][293] = 0.962794006; gainparam[1][293] = 0.00193363999; 
gainparam[0][294] = 0.912622988; gainparam[1][294] = 0.00543187; 
gainparam[0][295] = 0.887811005; gainparam[1][295] = 0.0034622401; 
gainparam[0][296] = 0.707526028; gainparam[1][296] = 0.00529601984; 
gainparam[0][297] = 0.767799973; gainparam[1][297] = 0.00804201979; 
gainparam[0][298] = 0.810387015; gainparam[1][298] = 0.00375023996; 
gainparam[0][299] = 0.821609974; gainparam[1][299] = 0.00459869998; 
gainparam[0][300] = 0.78667599; gainparam[1][300] = 0.00432308018; 
gainparam[0][301] = 0.758840024; gainparam[1][301] = 0.00601032982; 
gainparam[0][302] = 0.70043999; gainparam[1][302] = 0.00462264987; 
gainparam[0][303] = 0.942236006; gainparam[1][303] = 0.00199191994; 
gainparam[0][304] =   1.08559; gainparam[1][304] = 0.00662133005; 
gainparam[0][305] = 0.909254014; gainparam[1][305] = 0.00405724021; 
gainparam[0][306] = 0.936713994; gainparam[1][306] = 0.00292390003; 
gainparam[0][307] = 0.875775993; gainparam[1][307] = 0.00316015002; 
gainparam[0][308] = 0.91019398; gainparam[1][308] = 0.00319928001; 
gainparam[0][309] = 0.688300014; gainparam[1][309] = 0.00446414016; 
gainparam[0][310] = 1.07799006; gainparam[1][310] = 0.00175416004; 
gainparam[0][311] = 0.794057012; gainparam[1][311] = 0.00412400998; 
gainparam[0][312] = 0.897710025; gainparam[1][312] = 0.00252838992; 
gainparam[0][313] = 1.03726995; gainparam[1][313] = 0.00506858015; 
gainparam[0][314] = 0.950179994; gainparam[1][314] = 0.00386308995; 
gainparam[0][315] = 0.791104972; gainparam[1][315] = 0.00505277002; 
gainparam[0][316] = 1.03252995; gainparam[1][316] = 0.00228279992; 
gainparam[0][317] = 0.745917976; gainparam[1][317] = 0.00521423994; 
gainparam[0][318] = 0.829639971; gainparam[1][318] = 0.00423535984; 
gainparam[0][319] = 0.877507985; gainparam[1][319] = 0.00296575995; 
gainparam[0][321] = 1.01986003; gainparam[1][321] = 0.00361667993; 
gainparam[0][323] = 1.06826997; gainparam[1][323] = 0.00114633003; 
gainparam[0][325] = 1.06649995; gainparam[1][325] = 0.00233082008; 
gainparam[0][327] = 1.01662004; gainparam[1][327] = 0.00422695978; 
gainparam[0][328] = 1.18912995; gainparam[1][328] = -0.000289107003; 
gainparam[0][329] = 0.917577982; gainparam[1][329] = 0.00396397011; 
gainparam[0][330] = 1.36027002; gainparam[1][330] = 0.00116612995; 
gainparam[0][331] = 0.917128026; gainparam[1][331] = 0.00197520992; 
gainparam[0][332] = 0.799714983; gainparam[1][332] = 0.00702612987; 
gainparam[0][333] = 1.02081001; gainparam[1][333] = 0.000657172001; 
gainparam[0][334] = 1.00452995; gainparam[1][334] = 0.00276221009; 
gainparam[0][335] = 1.03621995; gainparam[1][335] = 0.00205843011; 
gainparam[0][336] = 0.827679992; gainparam[1][336] = 0.00481336983; 
gainparam[0][337] = 0.803124011; gainparam[1][337] = 0.00407795003; 
gainparam[0][338] = 0.955911994; gainparam[1][338] = 0.00357702002; 
gainparam[0][339] = 0.866577983; gainparam[1][339] = 0.00465284986; 
gainparam[0][342] = 1.22564006; gainparam[1][342] = 0.00799204037; 
gainparam[0][343] = 0.956237972; gainparam[1][343] = 0.00647709006; 
gainparam[0][352] = 1.72009003; gainparam[1][352] = 0.00252092001; 
gainparam[0][353] = 1.49820995; gainparam[1][353] = 0.00416482007; 
gainparam[0][356] = 1.05772996; gainparam[1][356] = 0.00267487997; 
gainparam[0][357] = 0.819613993; gainparam[1][357] = 0.00741497986; 
gainparam[0][358] = 0.889123976; gainparam[1][358] = 0.00623256993; 
gainparam[0][359] = 0.960276008; gainparam[1][359] = 0.00357928989; 
gainparam[0][360] = 0.834779024; gainparam[1][360] = 0.00533530023; 
gainparam[0][361] = 0.823468029; gainparam[1][361] = 0.00415648008; 
gainparam[0][362] = 0.917096019; gainparam[1][362] = 0.00460409978; 
gainparam[0][363] = 0.96617198; gainparam[1][363] = 0.00482517993; 
gainparam[0][366] = 1.11476004; gainparam[1][366] = 0.00429983996; 
gainparam[0][367] = 1.07402003; gainparam[1][367] = 0.00275759003; 
gainparam[0][376] = 1.51898003; gainparam[1][376] = 0.00614074012; 
gainparam[0][377] = 1.63002002; gainparam[1][377] = 0.00668691983; 
gainparam[0][380] = 0.962450027; gainparam[1][380] = 0.00499942992; 
gainparam[0][381] = 0.991043985; gainparam[1][381] = 0.00459132018; 
gainparam[0][382] = 0.882304013; gainparam[1][382] = 0.00921277981; 
gainparam[0][383] = 1.11925006; gainparam[1][383] = 0.00128215004; 
gainparam[0][384] = 0.706816971; gainparam[1][384] = 0.0045864; 
gainparam[0][385] = 0.731721997; gainparam[1][385] = 0.00316870003; 
gainparam[0][386] = 0.710968018; gainparam[1][386] = 0.00342654996; 
gainparam[0][387] = 0.725216985; gainparam[1][387] = 0.00388232991; 
gainparam[0][388] = 0.824689984; gainparam[1][388] = 0.00293101999; 
gainparam[0][389] = 1.11711001; gainparam[1][389] = 0.00373082003; 
gainparam[0][390] = 0.837943971; gainparam[1][390] = 0.00293586007; 
gainparam[0][391] = 0.800316989; gainparam[1][391] = 0.00290997; 
gainparam[0][392] = 0.779012024; gainparam[1][392] = 0.00873486046; 
gainparam[0][394] = 0.988231003; gainparam[1][394] = 0.00241759; 
gainparam[0][396] =         1; gainparam[1][396] =         0; 
gainparam[0][398] =         1; gainparam[1][398] =         0; 
gainparam[0][400] = 1.05054998; gainparam[1][400] = 0.00386884995; 
gainparam[0][401] = 1.25683999; gainparam[1][401] = 0.000938059005; 
gainparam[0][402] =         1; gainparam[1][402] =         0; 
gainparam[0][403] =         1; gainparam[1][403] =         0; 
gainparam[0][404] = 1.23415005; gainparam[1][404] = 0.00393701019; 
gainparam[0][405] = 1.00287998; gainparam[1][405] = 0.00390644977; 
gainparam[0][406] =         1; gainparam[1][406] =         0; 
gainparam[0][407] = 1.15603006; gainparam[1][407] = 0.00258358009; 
gainparam[0][408] = 0.838007987; gainparam[1][408] = 0.00202036998; 
gainparam[0][409] = 0.906075001; gainparam[1][409] = 0.00322158006; 
gainparam[0][410] = 0.718638003; gainparam[1][410] = 0.00430485979; 
gainparam[0][411] = 0.769023001; gainparam[1][411] = 0.00434789015; 
gainparam[0][412] = 1.13742995; gainparam[1][412] = 0.00493767997; 
gainparam[0][413] = 1.05394995; gainparam[1][413] = 0.00427153986; 
gainparam[0][414] = 1.02576995; gainparam[1][414] = 0.00335582998; 
gainparam[0][415] = 1.02464998; gainparam[1][415] = 0.00303433998; 
gainparam[0][416] = 1.05505002; gainparam[1][416] = 0.00494620996; 
gainparam[0][417] = 0.674405992; gainparam[1][417] = 0.0071707; 
gainparam[0][418] = 0.956524014; gainparam[1][418] = 0.00591736985; 
gainparam[0][419] = 0.857316971; gainparam[1][419] = 0.00414681016; 
gainparam[0][420] =         1; gainparam[1][420] =         0; 
gainparam[0][421] =         1; gainparam[1][421] =         0; 
gainparam[0][422] =         1; gainparam[1][422] =         0; 
gainparam[0][423] =         1; gainparam[1][423] =         0; 
gainparam[0][424] = 0.960515022; gainparam[1][424] = 0.0021200499; 
gainparam[0][425] = 1.10576999; gainparam[1][425] = 0.00273454003; 
gainparam[0][426] =         1; gainparam[1][426] =         0; 
gainparam[0][427] =         1; gainparam[1][427] =         0; 
gainparam[0][428] = 1.19375002; gainparam[1][428] = 0.00847455021; 
gainparam[0][429] = 1.23874998; gainparam[1][429] = 0.00587126007; 
gainparam[0][430] = 1.12571001; gainparam[1][430] = 0.00166667998; 
gainparam[0][431] =         1; gainparam[1][431] =         0; 
gainparam[0][432] = 0.767830014; gainparam[1][432] = 0.00509150978; 
gainparam[0][433] = 0.687524974; gainparam[1][433] = 0.00281972997; 
gainparam[0][434] = 0.815989017; gainparam[1][434] = 0.000706423016; 
gainparam[0][435] = 0.908747971; gainparam[1][435] = 0.00316062011; 
gainparam[0][436] =  0.904661; gainparam[1][436] = 0.00390002993; 
gainparam[0][437] = 0.82992202; gainparam[1][437] = 0.00450584013; 
gainparam[0][438] = 0.890106022; gainparam[1][438] = 0.00483322982; 
gainparam[0][439] = 0.846713006; gainparam[1][439] = 0.00131843996; 
gainparam[0][440] = 0.574140012; gainparam[1][440] = 0.00596403982; 
gainparam[0][441] = 0.707961977; gainparam[1][441] = 0.00418002019; 
gainparam[0][442] = 0.666854024; gainparam[1][442] = 0.00577322999; 
gainparam[0][443] = 0.751663983; gainparam[1][443] = 0.00482460996; 
gainparam[0][444] =         1; gainparam[1][444] =         0; 
gainparam[0][445] =         1; gainparam[1][445] =         0; 
gainparam[0][446] =         1; gainparam[1][446] =         0; 
gainparam[0][447] =         1; gainparam[1][447] =         0; 
gainparam[0][448] =         1; gainparam[1][448] =         0; 
gainparam[0][449] = 0.680204988; gainparam[1][449] = 0.00348497997; 
gainparam[0][450] = 0.540983975; gainparam[1][450] = 0.00238038995; 
gainparam[0][451] =         1; gainparam[1][451] =         0; 
gainparam[0][452] = 0.698647976; gainparam[1][452] = 0.00389598007; 
gainparam[0][453] = 0.908735991; gainparam[1][453] = 0.00482761022; 
gainparam[0][454] =         1; gainparam[1][454] =         0; 
gainparam[0][455] =         1; gainparam[1][455] =         0; 
gainparam[0][456] =         1; gainparam[1][456] =         0; 
gainparam[0][457] =         1; gainparam[1][457] =         0; 
gainparam[0][458] = 0.856441975; gainparam[1][458] = 0.00358080002; 
gainparam[0][459] = 0.809056997; gainparam[1][459] = 0.00302266004; 
gainparam[0][460] = 0.725481987; gainparam[1][460] = 0.00356566999; 
gainparam[0][462] =         1; gainparam[1][462] =         0; 
gainparam[0][463] = 0.838154972; gainparam[1][463] = 0.00452387007; 
gainparam[0][465] = 0.752475977; gainparam[1][465] = 0.00768304011; 
gainparam[0][467] = 0.820273995; gainparam[1][467] = -0.000182821997; 
gainparam[0][469] =         1; gainparam[1][469] =         0; 
gainparam[0][471] =         1; gainparam[1][471] =         0; 
gainparam[0][472] = 1.07028997; gainparam[1][472] = 0.00360648008; 
gainparam[0][473] = 0.720902979; gainparam[1][473] = 0.00836766046; 
gainparam[0][474] = 1.01225996; gainparam[1][474] = 0.00696125021; 
gainparam[0][476] = 0.760299981; gainparam[1][476] = 0.00563945994; 
gainparam[0][477] = 0.789471984; gainparam[1][477] = 0.00329107; 
gainparam[0][478] = 0.811778009; gainparam[1][478] = 0.00338158011; 
gainparam[0][479] = 0.957376003; gainparam[1][479] = 0.0027063; 
gainparam[0][480] =         1; gainparam[1][480] =         0; 
gainparam[0][481] =         1; gainparam[1][481] =         0; 
gainparam[0][482] = 0.792680979; gainparam[1][482] = 0.00451808004; 
gainparam[0][483] = 0.770821989; gainparam[1][483] = 0.00654056016; 
gainparam[0][486] =         1; gainparam[1][486] =         0; 
gainparam[0][487] =         1; gainparam[1][487] =         0; 
gainparam[0][496] = 0.931959987; gainparam[1][496] = 0.00411021989; 
gainparam[0][497] = 1.06254005; gainparam[1][497] = 0.00380064989; 
gainparam[0][500] = 0.85124898; gainparam[1][500] = 0.00651849015; 
gainparam[0][501] = 0.904255986; gainparam[1][501] = 0.00442657014; 
gainparam[0][502] = 0.842399001; gainparam[1][502] = 0.00312262005; 
gainparam[0][503] =  0.741292; gainparam[1][503] = 0.00512378011; 
gainparam[0][504] =         1; gainparam[1][504] =         0; 
gainparam[0][505] =         1; gainparam[1][505] =         0; 
gainparam[0][506] = 0.692458987; gainparam[1][506] = 0.00335242995; 
gainparam[0][507] = 0.54816097; gainparam[1][507] = 0.00690412009; 
gainparam[0][510] =         1; gainparam[1][510] =         0; 
gainparam[0][511] =         1; gainparam[1][511] =         0; 
gainparam[0][520] = 0.837949991; gainparam[1][520] = 0.00376621005; 
gainparam[0][521] = 0.944705009; gainparam[1][521] = 0.00316929002; 
gainparam[0][524] = 0.726454973; gainparam[1][524] = 0.00488170981; 
gainparam[0][525] = 0.848124027; gainparam[1][525] = 0.00373370992; 
gainparam[0][526] = 0.709200978; gainparam[1][526] = 0.00341690006; 
gainparam[0][527] = 0.805739999; gainparam[1][527] = 0.00286547001; 
gainparam[0][528] =         1; gainparam[1][528] =         0; 
gainparam[0][529] =         1; gainparam[1][529] =         0; 
gainparam[0][530] = 1.07405996; gainparam[1][530] = 0.000475648994; 
gainparam[0][531] = 0.861288011; gainparam[1][531] = 0.00281775999; 
gainparam[0][533] = 1.10353994; gainparam[1][533] = 0.00200450001; 
gainparam[0][534] = 0.868292987; gainparam[1][534] = 0.00233763992; 
gainparam[0][535] =         1; gainparam[1][535] =         0; 
gainparam[0][536] = 0.810618997; gainparam[1][536] = 0.00330379; 
gainparam[0][538] = 0.920139015; gainparam[1][538] = 0.00477802986; 
gainparam[0][540] = 0.994044006; gainparam[1][540] = 0.00526025007; 
gainparam[0][542] = 1.10547996; gainparam[1][542] = 0.000626757974; 
gainparam[0][544] = 0.720090985; gainparam[1][544] = 0.00285650999; 
gainparam[0][545] = 0.64477998; gainparam[1][545] = 0.00676605012; 
gainparam[0][547] = 0.943193972; gainparam[1][547] = 0.00499856984; 
gainparam[0][548] = 0.766281009; gainparam[1][548] = 0.00324308989; 
gainparam[0][549] = 0.590357006; gainparam[1][549] = 0.00213226001; 
gainparam[0][550] = 0.832957983; gainparam[1][550] = 0.00507871015; 
gainparam[0][551] = 0.750567019; gainparam[1][551] = 0.0035155199; 
gainparam[0][552] = 0.801566005; gainparam[1][552] = 0.00261870003; 
gainparam[0][553] = 0.90100801; gainparam[1][553] = 0.00659833988; 
gainparam[0][554] = 1.23605001; gainparam[1][554] = 0.00249633007; 
gainparam[0][555] = 1.13059998; gainparam[1][555] = -0.000249011006; 
gainparam[0][556] = 0.777212977; gainparam[1][556] = 0.0045265099; 
gainparam[0][557] = 0.743453979; gainparam[1][557] = 0.00576731982; 
gainparam[0][558] = 0.750589013; gainparam[1][558] = 0.00540158013; 
gainparam[0][559] = 0.895408988; gainparam[1][559] = 0.00321223005; 
gainparam[0][560] = 0.679521024; gainparam[1][560] = 0.00329628005; 
gainparam[0][561] = 0.748106003; gainparam[1][561] = 0.00285678008; 
gainparam[0][562] =  0.722911; gainparam[1][562] = 0.00603868999; 
gainparam[0][563] = 0.709170997; gainparam[1][563] = 0.00442267023; 
 gainparam[0][564] = 0.758800983; gainparam[1][564] = 0.00409983005; 
gainparam[0][565] = 0.709118009; gainparam[1][565] = 0.00365287997; 
 gainparam[0][566] = 0.745890021; gainparam[1][566] = 0.00653599016; 
gainparam[0][567] = 0.684068978; gainparam[1][567] = 0.00439357013; 
 gainparam[0][568] = 0.715781987; gainparam[1][568] = 0.00384919997; 
 gainparam[0][569] = 0.673507988; gainparam[1][569] = 0.00258916011; 
gainparam[0][570] = 0.673901975; gainparam[1][570] = 0.00475567998; 
 gainparam[0][571] = 0.734889984; gainparam[1][571] = 0.00385215995; 
 gainparam[0][572] = 0.919822991; gainparam[1][572] = 0.00503192982; 
 gainparam[0][573] = 0.791737974; gainparam[1][573] = 0.00298246997; 
 gainparam[0][574] = 0.718881011; gainparam[1][574] = 0.00220839004; 
 gainparam[0][575] = 0.776998997; gainparam[1][575] = 0.00260106008; 
  }
  if(daflag == 0){
    gainparam[0][0] = 0.912921011; gainparam[1][0] = 0.00355649996; 
    gainparam[0][1] = 0.752892971; gainparam[1][1] = 0.00530298008; 
    gainparam[0][2] = 1.06783998; gainparam[1][2] = -0.00128909003; 
    gainparam[0][3] = 0.731405973; gainparam[1][3] = 0.0034511399; 
    gainparam[0][4] = 0.922115028; gainparam[1][4] = 0.00408193981; 
    gainparam[0][5] = 0.972045004; gainparam[1][5] = 0.00210688007; 
    gainparam[0][6] = 0.985117018; gainparam[1][6] = 0.00270623993; 
    gainparam[0][7] = 0.730880022; gainparam[1][7] = 0.00670963991; 
    gainparam[0][8] = 0.889310002; gainparam[1][8] = 0.00489977002; 
    gainparam[0][9] = 0.625155985; gainparam[1][9] = 0.00581202004; 
    gainparam[0][10] = 0.917715013; gainparam[1][10] = 0.00454023993; 
    gainparam[0][11] = 0.590502024; gainparam[1][11] = 0.00504839979; 
    gainparam[0][12] = 1.35499001; gainparam[1][12] = 0.0028738; 
    gainparam[0][13] = 0.965865016; gainparam[1][13] = 0.00126549997; 
    gainparam[0][14] = 1.32433999; gainparam[1][14] = 0.00140569999; 
    gainparam[0][15] = 0.846913993; gainparam[1][15] = 0.00282782991; 
    gainparam[0][16] = 0.809684992; gainparam[1][16] = 0.00407561008; 
    gainparam[0][17] = 0.900848985; gainparam[1][17] = 0.00354498997; 
    gainparam[0][18] = 1.09220004; gainparam[1][18] = 0.00459052017; 
    gainparam[0][19] = 1.20480001; gainparam[1][19] = -0.000322501; 
    gainparam[0][20] = 0.965614021; gainparam[1][20] = 0.00194513996; 
    gainparam[0][21] = 1.05970001; gainparam[1][21] = 0.00145305996; 
    gainparam[0][22] = 0.852541983; gainparam[1][22] = 0.00456001982; 
    gainparam[0][23] = 1.07897997; gainparam[1][23] = 0.0020389799; 
    gainparam[0][24] = 0.80934298; gainparam[1][24] = 0.00539533002; 
    gainparam[0][25] = 0.927890003; gainparam[1][25] = 0.00213550008; 
gainparam[0][26] = 1.06114995; gainparam[1][26] = 2.52704995e-05; 
gainparam[0][27] = 0.801400006; gainparam[1][27] = 0.00327470992; 
gainparam[0][31] = 1.60337996; gainparam[1][31] = -0.000431197986; 
gainparam[0][41] = 1.18376994; gainparam[1][41] = 0.00296682003; 
gainparam[0][44] = 0.84788698; gainparam[1][44] = 0.00285427994; 
gainparam[0][45] =  0.661789; gainparam[1][45] = 0.00468005007; 
gainparam[0][46] = 1.40632999; gainparam[1][46] = -0.000567978015; 
gainparam[0][47] = 0.857846022; gainparam[1][47] = 0.00252850004; 
gainparam[0][48] = 0.527207971; gainparam[1][48] = 0.0072214799; 
gainparam[0][49] = 0.916494012; gainparam[1][49] = 0.00372740999; 
gainparam[0][50] = 0.920809984; gainparam[1][50] = 0.00147705001; 
gainparam[0][51] = 0.833840013; gainparam[1][51] = 0.00229310011; 
gainparam[0][68] = 0.91813302; gainparam[1][68] = 0.00122596999; 
gainparam[0][69] = 0.727191985; gainparam[1][69] = 0.00264456007; 
gainparam[0][70] = 1.14256001; gainparam[1][70] = 0.00223611994; 
gainparam[0][71] = 0.70362097; gainparam[1][71] = 0.00519753015; 
gainparam[0][72] = 0.64330399; gainparam[1][72] = 0.00723225018; 
gainparam[0][73] = 1.37803996; gainparam[1][73] = -0.00188664999; 
gainparam[0][74] = 0.885419011; gainparam[1][74] = 0.00294867996; 
gainparam[0][75] = 0.999505997; gainparam[1][75] = 0.000819829991; 
gainparam[0][92] = 1.08748996; gainparam[1][92] = -0.000312363001; 
gainparam[0][93] = 1.18798995; gainparam[1][93] = 0.000896085985; 
gainparam[0][94] = 0.757852972; gainparam[1][94] = 0.00436649006; 
gainparam[0][95] =         1; gainparam[1][95] =         0; 
gainparam[0][96] =         1; gainparam[1][96] =         0; 
gainparam[0][97] =         1; gainparam[1][97] =         0; 
gainparam[0][98] = 0.689727008; gainparam[1][98] = 0.00820085034; 
gainparam[0][99] =         1; gainparam[1][99] =         0; 
gainparam[0][102] = 0.936029971; gainparam[1][102] = 0.00749076996; 
gainparam[0][112] = 0.932006001; gainparam[1][112] = 0.00638087979; 
gainparam[0][116] = 1.00226998; gainparam[1][116] = 0.00179977994; 
gainparam[0][117] = 0.865462005; gainparam[1][117] = 0.00290779001; 
gainparam[0][118] = 1.27497005; gainparam[1][118] = 0.00176674; 
gainparam[0][119] = 1.18475997; gainparam[1][119] = 0.00349339005; 
gainparam[0][120] = 1.25549996; gainparam[1][120] = 0.00380152999; 
gainparam[0][121] = 1.35738003; gainparam[1][121] = 0.00302223; 
gainparam[0][122] =         1; gainparam[1][122] =         0; 
gainparam[0][123] =         1; gainparam[1][123] =         0; 
gainparam[0][124] = 1.12951005; gainparam[1][124] = 0.00399036985; 
gainparam[0][125] = 1.45244002; gainparam[1][125] = 0.00156517001; 
gainparam[0][126] = 1.49609995; gainparam[1][126] = 0.000102088001; 
gainparam[0][127] = 0.757682979; gainparam[1][127] = 0.00784055982; 
gainparam[0][128] = 1.00511003; gainparam[1][128] = 0.00197867001; 
gainparam[0][129] = 1.30865002; gainparam[1][129] = 0.00133300002; 
gainparam[0][130] = 0.988070011; gainparam[1][130] = 0.00245840009; 
gainparam[0][131] = 1.24690998; gainparam[1][131] = 0.00439856015; 
gainparam[0][132] = 0.823487997; gainparam[1][132] = 0.00357173989; 
gainparam[0][133] = 1.45229995; gainparam[1][133] = 0.00731034996; 
gainparam[0][134] =   1.00167; gainparam[1][134] = 0.00290466007; 
gainparam[0][135] = 1.40777004; gainparam[1][135] = 0.00434766989; 
gainparam[0][136] = 0.74545002; gainparam[1][136] = 0.00538741006; 
gainparam[0][137] = 1.00999999; gainparam[1][137] = 0.00201961002; 
gainparam[0][138] = 0.891833007; gainparam[1][138] = 0.0035645701; 
gainparam[0][139] = 0.79717201; gainparam[1][139] = 0.00655724993; 
gainparam[0][140] = 0.926522017; gainparam[1][140] = 0.00313152; 
gainparam[0][141] = 0.808349013; gainparam[1][141] = 0.00614786008; 
gainparam[0][142] = 0.941500008; gainparam[1][142] = 0.00460977014; 
gainparam[0][143] = 0.680101991; gainparam[1][143] = 0.00767106004; 
gainparam[0][144] =         1; gainparam[1][144] =         0; 
gainparam[0][145] = 0.958609998; gainparam[1][145] = 0.00879391003; 
gainparam[0][146] = 0.514428973; gainparam[1][146] = 0.00731091993; 
gainparam[0][147] = 1.06488001; gainparam[1][147] = 0.00516034011; 
gainparam[0][148] = 0.884836972; gainparam[1][148] = 0.00455394015; 
gainparam[0][149] = 0.616859972; gainparam[1][149] = 0.0110588996; 
gainparam[0][150] = 1.31651998; gainparam[1][150] = 0.00218408997; 
gainparam[0][151] = 1.08004999; gainparam[1][151] = 0.0035816899; 
gainparam[0][152] = 1.16671002; gainparam[1][152] = 0.000286101014; 
gainparam[0][153] = 0.594350994; gainparam[1][153] = 0.00736093987; 
gainparam[0][154] = 0.915807009; gainparam[1][154] = 0.00391884008; 
gainparam[0][155] = 0.618130028; gainparam[1][155] = 0.00872612; 
gainparam[0][156] = 0.712615013; gainparam[1][156] = 0.00408508023; 
gainparam[0][157] = 1.04628003; gainparam[1][157] = 0.00110373995; 
gainparam[0][158] = 1.16767001; gainparam[1][158] = -0.000471679989; 
gainparam[0][159] = 0.836121976; gainparam[1][159] = 0.00378311006; 
gainparam[0][160] = 1.08238006; gainparam[1][160] = 0.00372736994; 
gainparam[0][161] = 1.11028004; gainparam[1][161] = 0.00203498988; 
gainparam[0][162] = 1.04436004; gainparam[1][162] = 0.00106976996; 
gainparam[0][163] = 0.854831994; gainparam[1][163] = 0.00365086994; 
gainparam[0][164] = 0.991397023; gainparam[1][164] = 0.00172714004; 
gainparam[0][165] = 1.50011003; gainparam[1][165] = 3.18238017e-05; 
gainparam[0][166] = 1.16237998; gainparam[1][166] = 0.00357661; 
gainparam[0][167] = 1.02250004; gainparam[1][167] = 0.00382575998; 
gainparam[0][168] = 0.936946988; gainparam[1][168] = 0.00104580005; 
gainparam[0][169] = 0.944797993; gainparam[1][169] = 0.00324271992; 
gainparam[0][170] = 0.975619018; gainparam[1][170] = 0.00263553998; 
gainparam[0][171] =   1.10349; gainparam[1][171] = 0.00124695001; 
gainparam[0][172] = 0.59958601; gainparam[1][172] = 0.0078483196; 
gainparam[0][173] = 0.860903978; gainparam[1][173] = 0.00588413002; 
gainparam[0][174] = 0.748102009; gainparam[1][174] = 0.00611953018; 
gainparam[0][175] = 1.26487994; gainparam[1][175] = 0.00262699998; 
gainparam[0][177] = 0.889612019; gainparam[1][177] = 0.00325179007; 
gainparam[0][179] = 0.739313006; gainparam[1][179] = 0.00597140007; 
gainparam[0][181] = 1.15155995; gainparam[1][181] = -0.000561847992; 
gainparam[0][183] = 0.830834985; gainparam[1][183] = 0.00255730003; 
gainparam[0][184] = 1.23642004; gainparam[1][184] = -0.000156288996; 
gainparam[0][185] = 0.587588012; gainparam[1][185] = 0.0100953998; 
gainparam[0][186] = 1.58626997; gainparam[1][186] = -0.000754538982; 
gainparam[0][187] = 1.64058006; gainparam[1][187] = -0.00238640001; 
gainparam[0][188] = 1.05792999; gainparam[1][188] = 0.00248119002; 
gainparam[0][189] = 0.943180978; gainparam[1][189] = 0.00275741005; 
gainparam[0][190] = 0.880760014; gainparam[1][190] = 0.00282218005; 
gainparam[0][191] = 1.58911002; gainparam[1][191] = -0.00294124009; 
gainparam[0][192] = 0.578782022; gainparam[1][192] = 0.00222899998; 
gainparam[0][193] = 0.716705024; gainparam[1][193] = 0.000263769005; 
gainparam[0][194] = 0.511039019; gainparam[1][194] = 0.00364628993; 
gainparam[0][195] = 0.874087989; gainparam[1][195] = 0.000953055976; 
gainparam[0][198] = 0.634800017; gainparam[1][198] = 0.00307108997; 
gainparam[0][199] = 1.03147995; gainparam[1][199] = -0.00256302999; 
gainparam[0][208] = 0.761348009; gainparam[1][208] = 7.72014027e-05; 
gainparam[0][209] = 0.809189975; gainparam[1][209] = -0.000435766007; 
gainparam[0][212] = 0.149986997; gainparam[1][212] = 0.00840851013; 
gainparam[0][213] = 1.20448995; gainparam[1][213] = -0.00194420002; 
gainparam[0][214] =   1.03792; gainparam[1][214] = -0.00264284993; 
gainparam[0][215] = 0.878040016; gainparam[1][215] = -0.000678794982; 
gainparam[0][216] = 0.858470023; gainparam[1][216] = 0.00185752998; 
gainparam[0][217] = 0.880428016; gainparam[1][217] = 0.00132193998; 
gainparam[0][218] = 0.823284984; gainparam[1][218] = 0.00256380998; 
gainparam[0][219] = 0.724739015; gainparam[1][219] = 0.00298742997; 
gainparam[0][222] = 1.04742002; gainparam[1][222] = 0.00138856994; 
gainparam[0][223] = 0.711914003; gainparam[1][223] = 0.00484271022; 
gainparam[0][232] = 0.884109974; gainparam[1][232] = 0.00237854989; 
gainparam[0][233] = 0.742842019; gainparam[1][233] = 0.00310264993; 
gainparam[0][236] = 0.938446999; gainparam[1][236] = 0.00419841008; 
gainparam[0][237] = 0.883381009; gainparam[1][237] = 0.00409951014; 
gainparam[0][238] = 1.24091005; gainparam[1][238] = -0.00193229003; 
gainparam[0][239] = 0.768800974; gainparam[1][239] = 0.00388127007; 
gainparam[0][240] = 0.524051011; gainparam[1][240] = 0.0087617198; 
gainparam[0][241] = 0.759720027; gainparam[1][241] = 0.00431752997; 
gainparam[0][242] = 0.774821997; gainparam[1][242] = 0.00606811978; 
gainparam[0][243] = 0.99297303; gainparam[1][243] = 0.00236854004; 
gainparam[0][244] = 1.13080001; gainparam[1][244] = 0.00445029; 
gainparam[0][245] = 0.926814973; gainparam[1][245] = 0.0061429101; 
gainparam[0][246] = 0.73248899; gainparam[1][246] = 0.00336546008; 
gainparam[0][247] = 1.26095998; gainparam[1][247] = 0.00076188799; 
gainparam[0][248] = 0.99990797; gainparam[1][248] = 0.000981917954; 
gainparam[0][250] = 0.955029011; gainparam[1][250] = 0.00367263006; 
gainparam[0][252] = 0.487217009; gainparam[1][252] = 0.00954390038; 
gainparam[0][254] = 0.632673979; gainparam[1][254] = 0.00639038999; 
gainparam[0][256] = 1.35072005; gainparam[1][256] = 0.00103809999; 
gainparam[0][257] =   1.59138; gainparam[1][257] = -0.00223562005; 
gainparam[0][258] = 0.561767995; gainparam[1][258] = 0.00837315992; 
gainparam[0][259] = 1.42631996; gainparam[1][259] = -0.00112902001; 
gainparam[0][260] = 0.844282985; gainparam[1][260] = 0.00658618007; 
gainparam[0][261] = 0.537406027; gainparam[1][261] = 0.0110157002; 
gainparam[0][262] = 1.31157005; gainparam[1][262] = -0.00214508991; 
gainparam[0][263] = 1.00317001; gainparam[1][263] = 0.00143511; 
gainparam[0][264] = 0.866221011; gainparam[1][264] = 0.0028262001; 
gainparam[0][265] = 0.693189025; gainparam[1][265] = 0.00703418022; 
gainparam[0][266] = 1.16182995; gainparam[1][266] = 0.00215200009; 
gainparam[0][267] = 0.904429018; gainparam[1][267] = 0.00239457004; 
gainparam[0][268] = 0.826887012; gainparam[1][268] = 0.00376635999; 
gainparam[0][269] = 1.08080995; gainparam[1][269] = -0.00105295004; 
gainparam[0][270] = 1.00308001; gainparam[1][270] = 0.00138792; 
gainparam[0][271] = 0.821419001; gainparam[1][271] = 0.00515137007; 
gainparam[0][272] = 1.16805005; gainparam[1][272] = 0.000757706002; 
gainparam[0][273] = 1.02421999; gainparam[1][273] = 0.000990718952; 
gainparam[0][274] = 0.838257015; gainparam[1][274] = 0.00391381001; 
gainparam[0][275] = 1.23105001; gainparam[1][275] = -0.00246535009; 
gainparam[0][276] = 0.888826013; gainparam[1][276] = 0.00293135992; 
gainparam[0][277] = 0.769338012; gainparam[1][277] = 0.00131149997; 
gainparam[0][278] = 0.462220997; gainparam[1][278] = 0.00854439009; 
gainparam[0][279] = 0.913843989; gainparam[1][279] = 0.00103364; 
gainparam[0][280] = 0.870301008; gainparam[1][280] = 0.00316006993; 
gainparam[0][281] = 1.01551998; gainparam[1][281] = 0.00171620003; 
gainparam[0][282] = 0.749328971; gainparam[1][282] = 0.00414092001; 
gainparam[0][283] = 0.943890989; gainparam[1][283] = 0.000405992992; 
gainparam[0][284] =         1; gainparam[1][284] =         0; 
gainparam[0][285] =         1; gainparam[1][285] =         0; 
gainparam[0][286] = 0.569629014; gainparam[1][286] = 0.00606278982; 
gainparam[0][287] = 0.682425976; gainparam[1][287] = 0.00604983; 
gainparam[0][288] = 0.339520007; gainparam[1][288] = 0.00991939008; 
gainparam[0][289] = 0.505703986; gainparam[1][289] = 0.00568918977; 
gainparam[0][290] = 0.671841025; gainparam[1][290] = 0.00456329994; 
gainparam[0][291] = 0.822041988; gainparam[1][291] = 0.00216814992; 
gainparam[0][292] = 0.621002018; gainparam[1][292] = 0.00686985021; 
gainparam[0][293] = 0.921142995; gainparam[1][293] = 0.00312327011; 
gainparam[0][294] = 0.47685799; gainparam[1][294] = 0.00855887029; 
gainparam[0][295] = 0.570787013; gainparam[1][295] = 0.0066824602; 
gainparam[0][296] = 0.666787982; gainparam[1][296] = 0.00446075993; 
gainparam[0][297] = 0.452443987; gainparam[1][297] = 0.00907951966; 
gainparam[0][298] = 0.639652014; gainparam[1][298] = 0.00519873993; 
gainparam[0][299] = 0.902410984; gainparam[1][299] = 0.00323778996; 
gainparam[0][300] = 0.676276982; gainparam[1][300] = 0.00463664997; 
gainparam[0][301] = 0.47012201; gainparam[1][301] = 0.00682620006; 
gainparam[0][302] = 0.858449996; gainparam[1][302] = 0.00187772; 
gainparam[0][303] = 0.657760978; gainparam[1][303] = 0.00660566986; 
gainparam[0][304] = 1.21053004; gainparam[1][304] = 0.00303747994; 
gainparam[0][305] = 0.837862015; gainparam[1][305] = 0.00410667993; 
gainparam[0][306] = 0.610692024; gainparam[1][306] = 0.00666022999; 
gainparam[0][307] = 0.712571025; gainparam[1][307] = 0.00508608017; 
gainparam[0][308] = 0.292133987; gainparam[1][308] = 0.00886442978; 
gainparam[0][309] = 0.766413987; gainparam[1][309] = 0.00195110997; 
gainparam[0][310] = 1.05806994; gainparam[1][310] = 0.00186116004; 
gainparam[0][311] = 0.955861986; gainparam[1][311] = 0.000419340009; 
gainparam[0][312] = 0.370315999; gainparam[1][312] = 0.00896636955; 
gainparam[0][313] = 0.562879026; gainparam[1][313] = 0.0102850003; 
gainparam[0][314] = 0.405106992; gainparam[1][314] = 0.0101969; 
gainparam[0][315] = 0.752012014; gainparam[1][315] = 0.00429651001; 
gainparam[0][316] = 0.517569005; gainparam[1][316] = 0.00990060996; 
gainparam[0][317] = 0.622003019; gainparam[1][317] = 0.00561420992; 
gainparam[0][318] = 0.500539005; gainparam[1][318] = 0.00772128999; 
gainparam[0][319] = 0.496766001; gainparam[1][319] = 0.00741581991; 
gainparam[0][321] = 0.54063201; gainparam[1][321] = 0.00962707028; 
gainparam[0][323] = 0.690787971; gainparam[1][323] = 0.00791558996; 
gainparam[0][325] = 0.465003997; gainparam[1][325] = 0.0107981004; 
gainparam[0][327] = 0.0320886001; gainparam[1][327] = 0.0160783008; 
gainparam[0][328] = 0.742655993; gainparam[1][328] = 0.00718876021; 
gainparam[0][329] = 1.09336996; gainparam[1][329] = 0.00135958998; 
gainparam[0][330] = 1.49328005; gainparam[1][330] = 0.00347957993; 
gainparam[0][331] = 0.314599991; gainparam[1][331] = 0.00925948005; 
gainparam[0][332] = 0.137116998; gainparam[1][332] = 0.0110619999; 
gainparam[0][333] = 0.48547399; gainparam[1][333] = 0.00673918985; 
gainparam[0][334] = 1.06594002; gainparam[1][334] = 0.00179655; 
gainparam[0][335] = 0.554938972; gainparam[1][335] = 0.00769096985; 
gainparam[0][336] = 0.405937999; gainparam[1][336] = 0.00916546024; 
gainparam[0][337] =  0.798051; gainparam[1][337] = 0.00434945989; 
gainparam[0][338] = 0.766429007; gainparam[1][338] = 0.00604791008; 
gainparam[0][339] = 0.602770984; gainparam[1][339] = 0.00659128977; 
gainparam[0][342] = 0.817094982; gainparam[1][342] = 0.0124033997; 
gainparam[0][343] = 0.695230007; gainparam[1][343] = 0.00914284028; 
gainparam[0][352] = 1.07490003; gainparam[1][352] = 0.0141153997; 
gainparam[0][353] = 0.763418019; gainparam[1][353] = 0.0134410998; 
gainparam[0][356] = 0.250479013; gainparam[1][356] = 0.0128196999; 
gainparam[0][357] = 0.655673981; gainparam[1][357] = 0.00691147009; 
gainparam[0][358] = 0.256552011; gainparam[1][358] = 0.0126282005; 
gainparam[0][359] = 0.431470007; gainparam[1][359] = 0.0100250999; 
gainparam[0][360] = 0.718948007; gainparam[1][360] = 0.00604315987; 
gainparam[0][361] = 0.702983022; gainparam[1][361] = 0.00522339018; 
gainparam[0][362] = 0.568351984; gainparam[1][362] = 0.00842440035; 
gainparam[0][363] = 0.694203019; gainparam[1][363] = 0.00732917013; 
gainparam[0][366] = 0.813320994; gainparam[1][366] = 0.00957875978; 
gainparam[0][367] = 1.33388996; gainparam[1][367] = 0.00226894999; 
gainparam[0][376] = 1.18517995; gainparam[1][376] = 0.0112017998; 
gainparam[0][377] = 0.511524022; gainparam[1][377] = 0.0206847992; 
gainparam[0][380] = 0.275932014; gainparam[1][380] = 0.0124645; 
gainparam[0][381] =   1.08067; gainparam[1][381] = 0.00413277978; 
gainparam[0][382] = 0.585530996; gainparam[1][382] = 0.00915648974; 
gainparam[0][383] = 0.235326007; gainparam[1][383] = 0.0133065004; 
gainparam[0][384] = 0.803440988; gainparam[1][384] = 0.00247621001; 
gainparam[0][385] = 0.433544993; gainparam[1][385] = 0.00633761007; 
gainparam[0][386] = 0.554570019; gainparam[1][386] = 0.00494283019; 
gainparam[0][387] = 0.740966022; gainparam[1][387] = 0.0031079601; 
gainparam[0][388] = 0.655598998; gainparam[1][388] = 0.00529140001; 
gainparam[0][389] = 0.985266984; gainparam[1][389] = 0.00869293977; 
gainparam[0][390] = 0.518841982; gainparam[1][390] = 0.00682024006; 
gainparam[0][391] = 0.762310028; gainparam[1][391] = 0.00430777017; 
gainparam[0][392] = -0.271786988; gainparam[1][392] = 0.0190968998; 
gainparam[0][394] = 0.371829987; gainparam[1][394] = 0.0107241999; 
gainparam[0][396] =         1; gainparam[1][396] =         0; 
gainparam[0][398] =         1; gainparam[1][398] =         0; 
gainparam[0][400] = 0.384938985; gainparam[1][400] = 0.00997937005; 
gainparam[0][401] = 0.915313005; gainparam[1][401] = 0.00728134997; 
gainparam[0][402] =         1; gainparam[1][402] =         0; 
gainparam[0][403] =         1; gainparam[1][403] =         0; 
gainparam[0][404] = 1.02498996; gainparam[1][404] = 0.00521491002; 
gainparam[0][405] = 0.469783992; gainparam[1][405] = 0.00909642037; 
gainparam[0][406] = 0.166097999; gainparam[1][406] = 0.0145563995; 
gainparam[0][407] = -0.326815993; gainparam[1][407] = 0.0194255002; 
gainparam[0][408] = 0.946730018; gainparam[1][408] = 0.00142351002; 
gainparam[0][409] = 0.854517996; gainparam[1][409] = 0.00408085994; 
gainparam[0][410] = 0.546599984; gainparam[1][410] = 0.00540217012; 
gainparam[0][411] = 0.77007997; gainparam[1][411] = 0.0034296501; 
gainparam[0][412] = 0.775225997; gainparam[1][412] = 0.00935408007; 
gainparam[0][413] = 0.619229019; gainparam[1][413] = 0.00893479958; 
gainparam[0][414] = 0.43335101; gainparam[1][414] = 0.0103569003; 
gainparam[0][415] = 0.425799012; gainparam[1][415] = 0.0100677004; 
gainparam[0][416] = 0.00365338009; gainparam[1][416] = 0.0166437998; 
gainparam[0][417] = 0.588604987; gainparam[1][417] = 0.00679075997; 
gainparam[0][418] = 0.685690999; gainparam[1][418] = 0.00760531006; 
gainparam[0][419] = 0.734135985; gainparam[1][419] = 0.00502210995; 
gainparam[0][420] =         1; gainparam[1][420] =         0; 
gainparam[0][421] =         1; gainparam[1][421] =         0; 
gainparam[0][422] =         1; gainparam[1][422] =         0; 
gainparam[0][423] =         1; gainparam[1][423] =         0; 
gainparam[0][424] = 0.54797101; gainparam[1][424] = 0.0063165999; 
gainparam[0][425] = 0.387396991; gainparam[1][425] = 0.0119655002; 
gainparam[0][426] =         1; gainparam[1][426] =         0; 
gainparam[0][427] =         1; gainparam[1][427] =         0; 
gainparam[0][428] = 0.533468008; gainparam[1][428] = 0.0098122796; 
gainparam[0][429] = -0.032433901; gainparam[1][429] = 0.0166880991; 
gainparam[0][430] = 0.413549006; gainparam[1][430] = 0.00984533038; 
gainparam[0][431] = 0.320540011; gainparam[1][431] = 0.0142326001; 
gainparam[0][432] = 0.816703975; gainparam[1][432] = 0.00377723994; 
gainparam[0][433] = 0.510704994; gainparam[1][433] = 0.00459979009; 
gainparam[0][434] = 0.405734986; gainparam[1][434] = 0.00611229986; 
gainparam[0][435] = 0.200095996; gainparam[1][435] = 0.0107923001; 
gainparam[0][436] = 0.228873998; gainparam[1][436] = 0.0111648003; 
gainparam[0][437] = 0.757104993; gainparam[1][437] = 0.00436183996; 
gainparam[0][438] = 0.394333988; gainparam[1][438] = 0.00885527954; 
gainparam[0][439] = 0.687442005; gainparam[1][439] = 0.00387371005; 
gainparam[0][440] = 0.318603992; gainparam[1][440] = 0.00705173006; 
gainparam[0][441] = 0.814158976; gainparam[1][441] = 0.0026453801; 
gainparam[0][442] = 0.908056021; gainparam[1][442] = 0.000957649027; 
gainparam[0][443] = 0.475885987; gainparam[1][443] = 0.00677580014; 
gainparam[0][444] =         1; gainparam[1][444] =         0; 
gainparam[0][445] =         1; gainparam[1][445] =         0; 
gainparam[0][446] =         1; gainparam[1][446] =         0; 
gainparam[0][447] =         1; gainparam[1][447] =         0; 
gainparam[0][448] =         1; gainparam[1][448] =         0; 
gainparam[0][449] = 0.425078988; gainparam[1][449] = 0.00546356011; 
gainparam[0][450] = 0.842701018; gainparam[1][450] = -0.000922931999; 
gainparam[0][451] =         1; gainparam[1][451] =         0; 
gainparam[0][452] = 0.507010996; gainparam[1][452] = 0.00505923992; 
gainparam[0][453] = 0.61255002; gainparam[1][453] = 0.00713414978; 
gainparam[0][454] = -1.27892995; gainparam[1][454] = 0.0317047983; 
gainparam[0][455] = 0.398254991; gainparam[1][455] = 0.00671726977; 
gainparam[0][456] =         1; gainparam[1][456] =         0; 
gainparam[0][457] =         1; gainparam[1][457] =         0; 
gainparam[0][458] = 0.429446012; gainparam[1][458] = 0.00826335978; 
gainparam[0][459] = 0.711601973; gainparam[1][459] = 0.0048928801; 
gainparam[0][460] = 1.08702004; gainparam[1][460] = -0.0020121499; 
gainparam[0][462] =         1; gainparam[1][462] =         0; 
gainparam[0][463] = 0.644161999; gainparam[1][463] = 0.00611711014; 
gainparam[0][465] = 1.53357995; gainparam[1][465] = -0.00362105994; 
gainparam[0][467] = 0.766982973; gainparam[1][467] = 0.00211513997; 
gainparam[0][469] =         1; gainparam[1][469] =         0; 
gainparam[0][471] =         1; gainparam[1][471] =         0; 
gainparam[0][472] = 1.30685997; gainparam[1][472] = 0.00150792999; 
gainparam[0][473] = 0.968379974; gainparam[1][473] = 0.00150364998; 
gainparam[0][474] = 0.939846992; gainparam[1][474] = 0.00582320988; 
gainparam[0][476] = 0.836773992; gainparam[1][476] = 0.00316893007; 
gainparam[0][477] = 0.675302029; gainparam[1][477] = 0.00413533999; 
gainparam[0][478] = 0.918743014; gainparam[1][478] = 0.00206938991; 
gainparam[0][479] = 0.767744005; gainparam[1][479] = 0.00530656986; 
gainparam[0][480] =         1; gainparam[1][480] =         0; 
gainparam[0][481] =         1; gainparam[1][481] =         0; 
gainparam[0][482] = 0.311212987; gainparam[1][482] = 0.00979606993; 
gainparam[0][483] = 0.455065012; gainparam[1][483] = 0.00869711954; 
gainparam[0][486] =         1; gainparam[1][486] =         0; 
gainparam[0][487] =         1; gainparam[1][487] =         0; 
gainparam[0][496] = 0.668802977; gainparam[1][496] = 0.00667712977; 
gainparam[0][497] = 0.407631993; gainparam[1][497] = 0.0115719996; 
gainparam[0][500] = 0.662961006; gainparam[1][500] = 0.00657971017; 
gainparam[0][501] = 0.54746902; gainparam[1][501] = 0.00758958003; 
gainparam[0][502] = 0.518097997; gainparam[1][502] = 0.00699508982; 
gainparam[0][503] = 0.890060008; gainparam[1][503] = 0.0023614401; 
gainparam[0][504] =         1; gainparam[1][504] =         0; 
gainparam[0][505] =         1; gainparam[1][505] =         0; 
gainparam[0][506] = 0.734604001; gainparam[1][506] = 0.0024868201; 
gainparam[0][507] = 0.260823995; gainparam[1][507] = 0.00803682022; 
gainparam[0][510] =         1; gainparam[1][510] =         0; 
gainparam[0][511] =         1; gainparam[1][511] =         0; 
gainparam[0][520] = 0.621583998; gainparam[1][520] = 0.00597030018; 
gainparam[0][521] = 0.59228301; gainparam[1][521] = 0.00733492011; 
gainparam[0][524] = 0.668246984; gainparam[1][524] = 0.00425152993; 
gainparam[0][525] = 0.675056994; gainparam[1][525] = 0.00537281018; 
gainparam[0][526] = 0.806316972; gainparam[1][526] = 0.00171223003; 
gainparam[0][527] = 0.65892297; gainparam[1][527] = 0.0041877199; 
gainparam[0][528] =         1; gainparam[1][528] =         0; 
gainparam[0][529] =         1; gainparam[1][529] =         0; 
gainparam[0][530] = -0.00492158998; gainparam[1][530] = 0.0138095999; 
gainparam[0][531] = 0.715415001; gainparam[1][531] = 0.00476273987; 
gainparam[0][533] = 0.669371009; gainparam[1][533] = 0.00725213019; 
gainparam[0][534] = 0.750226974; gainparam[1][534] = 0.00397718977; 
gainparam[0][535] =         1; gainparam[1][535] =         0; 
gainparam[0][536] = 1.09714997; gainparam[1][536] = 0.000373796996; 
gainparam[0][538] = 0.198824003; gainparam[1][538] = 0.0120553002; 
gainparam[0][540] = 0.903445005; gainparam[1][540] = 0.0062243999; 
gainparam[0][542] = 0.813091993; gainparam[1][542] = 0.00503128022; 
gainparam[0][544] = 0.953594983; gainparam[1][544] = -3.37263991e-05; 
gainparam[0][545] = 0.89724201; gainparam[1][545] = 0.00172330998; 
gainparam[0][547] = 0.958545983; gainparam[1][547] = 0.00411956012; 
gainparam[0][548] = 0.317155004; gainparam[1][548] = 0.00805099029; 
gainparam[0][549] = 0.404861003; gainparam[1][549] = 0.00365954009; 
gainparam[0][550] = 0.616786003; gainparam[1][550] = 0.00595078012; 
gainparam[0][551] = 0.594438016; gainparam[1][551] = 0.00475955987; 
gainparam[0][552] = 0.779356003; gainparam[1][552] = 0.00262348005; 
gainparam[0][553] = 0.887140989; gainparam[1][553] = 0.00476083998; 
gainparam[0][554] = 1.59281003; gainparam[1][554] = -0.00112556003; 
gainparam[0][555] = 0.379011005; gainparam[1][555] = 0.00825242046; 
gainparam[0][556] = 0.466044992; gainparam[1][556] = 0.00733065978; 
gainparam[0][557] = 0.616464019; gainparam[1][557] = 0.0054734; 
gainparam[0][558] = 0.436284989; gainparam[1][558] = 0.00738593005; 
gainparam[0][559] = 0.693292022; gainparam[1][559] = 0.00456565013; 
gainparam[0][560] = 0.451002985; gainparam[1][560] = 0.00525985006; 
gainparam[0][561] = 0.870985985; gainparam[1][561] = 0.00139593997; 
gainparam[0][562] = 0.528626978; gainparam[1][562] = 0.00610119011; 
gainparam[0][563] = 0.595519006; gainparam[1][563] = 0.0046160901; 
gainparam[0][564] = 0.583845973; gainparam[1][564] = 0.00520798005; 
gainparam[0][565] = 0.603864014; gainparam[1][565] = 0.0039523202; 
gainparam[0][566] = 0.57663101; gainparam[1][566] = 0.00605345005; 
gainparam[0][567] = 0.502794027; gainparam[1][567] = 0.00554576004; 
gainparam[0][568] = 0.87518698; gainparam[1][568] = 0.00104968995; 
gainparam[0][569] = 0.495543003; gainparam[1][569] = 0.00332799996; 
gainparam[0][570] = 0.690339983; gainparam[1][570] = 0.00338826003; 
gainparam[0][571] = 0.729798019; gainparam[1][571] = 0.00411230978; 
gainparam[0][572] = 0.554524004; gainparam[1][572] = 0.00874216016; 
gainparam[0][573] = 0.688165009; gainparam[1][573] = 0.00414888002; 
gainparam[0][574] = 0.660665989; gainparam[1][574] = 0.00284599001; 
gainparam[0][575] = 0.362796009; gainparam[1][575] = 0.00574682979; 

  }
  return 0;
}
