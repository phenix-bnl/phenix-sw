#include "MatchrecalRecoRun12UU192.h"
#include <Fun4AllReturnCodes.h>

#include <PHCentralTrack.h>
#include <PHGlobal.h>
#include <PHSnglCentralTrack.h>
#include <getClass.h>
#include <RunHeader.h>

#include <PHCompositeNode.h>
#include <RunNumberRanges.h>
#include <recoConsts.h>
#include <iostream>

//Run12 ToF matching recalibrator by Iurii Mitrankov

using namespace std;
using namespace findNode;


MatchrecalRecoRun12UU192::MatchrecalRecoRun12UU192(const string &name):
        Recalibrator(name)
{
    baseclasses.insert("PHCentralTrack");
    initParameters();
    //1********************************************par fo fits************************************
    //tof
    fit_UU_z_sig = NULL;
    fit_UU_phi_sig = NULL;
    fit_UU_z_mean = NULL;
    fit_UU_phi_mean = NULL;
    //stof
    fit_UU_sz_sig = NULL;
    fit_UU_sphi_sig = NULL;
    fit_UU_sz_mean = NULL;
    fit_UU_sphi_mean = NULL;
    //1********************************************end of par fo fits************************************

}


int MatchrecalRecoRun12UU192::Init(PHCompositeNode *topNode)
{
    //1********************************************par fo fits************************************
    //tof
    fit_UU_z_sig = new TF1("fit_UU_z_sig","[0]+[1]*x+[2]/x+[3]*x*x+[4]*x*x*x+[5]*x*x*x*x+[6]/sqrt(x)",0.4,5.0);
    fit_UU_phi_sig = new TF1("fit_UU_phi_sig","[0]+[1]*x+[2]/x+[3]*x*x+[4]*x*x*x+[5]*x*x*x*x+[6]/sqrt(x)",0.4,5.0);
    fit_UU_z_mean = new TF1("fit_UU_z_mean","[0]+[1]*x+[2]/x+[3]*x*x+[4]/sqrt(x)",0.4,5.0);
    fit_UU_phi_mean = new TF1("fit_UU_phi_mean","[0]+[1]*x+[2]/x+[3]*x*x+[4]/sqrt(x)",0.4,5.0);
    //stof
    fit_UU_sz_sig = new TF1("fit_UU_sz_sig","[0]+[1]*x+[2]/x+[3]*x*x+[4]/sqrt(x)+[5]*x*x*x+[6]*x*x*x*x",0.4,5.0);
    fit_UU_sphi_sig = new TF1("fit_UU_sphi_sig","[0]+[1]*x+[2]/x+[3]*x*x+[4]/sqrt(x)+[5]*x*x*x+[6]*x*x*x*x",0.4,5.0);
    fit_UU_sz_mean = new TF1("fit_UU_sz_mean","[0]+[1]*x+[2]/x+[3]*x*x+[4]/sqrt(x)",0.4,5.0);
    fit_UU_sphi_mean = new TF1("fit_UU_sphi_mean","[0]+[1]*x+[2]/x+[3]*x*x+[4]/sqrt(x)",0.4,5.0);
    //1********************************************end of par fo fits************************************

    return EVENT_OK;
}
int MatchrecalRecoRun12UU192::isValidRun(const int runno) const
{
    if (runno < MINRUN || runno > MAXRUN)
    {
        return 0;
    }
    return 1;
}

int MatchrecalRecoRun12UU192::process_event(PHCompositeNode *topNode)
{
    PHGlobal *global = findNode::getClass<PHGlobal>(topNode,"PHGlobal");
    if(!global) return  0;
    int cent = (int)global->getCentrality() - 1;

    PHCentralTrack *d_cnt = getClass<PHCentralTrack>(topNode, "PHCentralTrack");
    if (!d_cnt)
    {
        return 0;
    }
    for(unsigned int i=0; i<d_cnt->get_npart(); i++){

        float mom       = -9999;
        float pt        = -9999;
        float zed       = -9999;
        int   dcarm     = -9999;
        int   charge    = -9999;
        float tofdphi   = -9999;
        float tofdz     = -9999;

        float tofsdphi  = -9999;
        float tofsdz    = -9999;




        mom       = d_cnt->get_mom(i);
        pt        = mom*sin(d_cnt->get_the0(i));
        zed       = d_cnt->get_zed(i);
        dcarm     = d_cnt->get_dcarm(i);
        charge    = d_cnt->get_charge(i);


        int iZed    = -9999;
        int iCent   = -9999;

        iCent=cent/20;
        iZed = get_ZED_bin(zed);

        tofdphi    = d_cnt->get_tofdphi(i);
        tofdz      = d_cnt->get_tofdz(i);


        if (tofdphi>-9999 && tofdz>-9999 && iZed>-1  &&  iZed<10  &&  iCent>-1  &&  iCent<5 && dcarm == 0 && pt>0.5 && pt<4.5)
        {
            tofsdphi=get_tofsdphi(pt,charge,iZed,tofdphi);
            tofsdz=get_tofsdz(pt,charge,iZed,tofdz,iCent);
        }

        d_cnt->set_tofsdphi(i,tofsdphi);
        d_cnt->set_tofsdz(i,tofsdz);
    }

    return EVENT_OK;
}


int MatchrecalRecoRun12UU192::get_ZED_bin(float ized)
{
    //if (m2_iner==-9999)  return -9999;
    int ized_bin=-1;
    for (int i = 0; i < -10*ized_bin; i++) {
        if(zed_bin_UU[2*i]<=ized && ized<zed_bin_UU[2*i+1])
        {
            ized_bin=i;
        }
    }

    return ized_bin;
}


float MatchrecalRecoRun12UU192::get_tofsdphi(float ipt, int icharge, int ized, float itofdphi)
{

    float itofsdphi=-9999;
    if(icharge==1)
    { //tof
        fit_UU_phi_sig->SetParameters(par_UU_sigma0[ized]);
        fit_UU_phi_mean->SetParameters(par_UU_mean0[ized]);
        //stof
        fit_UU_sphi_sig->SetParameters(par_UU_ssigma0[ized]);
        fit_UU_sphi_mean->SetParameters(par_UU_smean0[ized]);
    }
    if(icharge==-1)
    {
        fit_UU_phi_sig->SetParameters(par_UU_sigma1[ized]);
        fit_UU_phi_mean->SetParameters(par_UU_mean1[ized]);
        //stof
        fit_UU_sphi_sig->SetParameters(par_UU_ssigma1[ized]);
        fit_UU_sphi_mean->SetParameters(par_UU_smean1[ized]);
    }

    itofsdphi=((itofdphi-fit_UU_phi_mean->Eval(ipt))/fit_UU_phi_sig->Eval(ipt)-fit_UU_sphi_mean->Eval(ipt))/fit_UU_sphi_sig->Eval(ipt);

    return itofsdphi;
}

float MatchrecalRecoRun12UU192::get_tofsdz(float ipt, int icharge, int ized, float itofdz, int icentrality)
{
    // if (itofdz==-9999)  return -9999;
    float itofsdz=-9999;
    if(icharge==1)
    { //tof
        fit_UU_z_sig->SetParameters(par_UU_sigma2[ized]);
        fit_UU_z_mean->SetParameters(par_UU_mean2[ized]);
        //stof
        fit_UU_sz_sig->SetParameters(par_UU_ssigma2[ized]);
        fit_UU_sz_mean->SetParameters(par_UU_smean2[ized]);
    }
    if(icharge==-1)
    {
        fit_UU_z_sig->SetParameters(par_UU_sigma3[ized]);
        fit_UU_z_mean->SetParameters(par_UU_mean3[ized]);
        //stof
        fit_UU_sz_sig->SetParameters(par_UU_ssigma3[ized]);
        fit_UU_sz_mean->SetParameters(par_UU_smean3[ized]);
    }

    itofsdz=((itofdz-fit_UU_z_mean->Eval(ipt))/fit_UU_z_sig->Eval(ipt)-fit_UU_sz_mean->Eval(ipt)-Keff_UU_central_mean[icentrality])/fit_UU_sz_sig->Eval(ipt)/(Keff_UU_central_sigma[icentrality]);

    return itofsdz;
}

void MatchrecalRecoRun12UU192::initParameters()
{
    zed_bin_UU[0] = -70;  zed_bin_UU[1] = -57;  zed_bin_UU[2] = -57;  zed_bin_UU[3] = -44;
    zed_bin_UU[4] = -44;  zed_bin_UU[5] = -31;  zed_bin_UU[6] = -31;  zed_bin_UU[7] = -18;
    zed_bin_UU[8] = -18;  zed_bin_UU[9] = -5;  zed_bin_UU[10] = 5;  zed_bin_UU[11] = 18;
    zed_bin_UU[12] = 18;  zed_bin_UU[13] = 31;  zed_bin_UU[14] = 31;  zed_bin_UU[15] = 44;
    zed_bin_UU[16] = 44;  zed_bin_UU[17] = 57;  zed_bin_UU[18] = 57;  zed_bin_UU[19] = 70;

    Keff_UU_central_mean[0] = 0; Keff_UU_central_mean[1] = 0; Keff_UU_central_mean[2] = -0.085; Keff_UU_central_mean[3] = -0.115; Keff_UU_central_mean[4] = -0.12;
    Keff_UU_central_sigma[0] = 1; Keff_UU_central_sigma[1] = 1; Keff_UU_central_sigma[2] = 1.33; Keff_UU_central_sigma[3] = 1.9; Keff_UU_central_sigma[4] = 2;

    par_UU_mean0[0][0] = 0.0160336; par_UU_mean0[0][1] = -0.00431315; par_UU_mean0[0][2] = 0.000741549; par_UU_mean0[0][3] = 0.000557069; par_UU_mean0[0][4] = -0.010906;
    par_UU_mean0[1][0] = 0.018259; par_UU_mean0[1][1] = -0.00444613; par_UU_mean0[1][2] = 0.00377159; par_UU_mean0[1][3] = 0.00056639; par_UU_mean0[1][4] = -0.0156253;
    par_UU_mean0[2][0] = 0.0112133; par_UU_mean0[2][1] = -0.00234089; par_UU_mean0[2][2] = 0.00244024; par_UU_mean0[2][3] = 0.000309381; par_UU_mean0[2][4] = -0.00863358;
    par_UU_mean0[3][0] = 0.00494313; par_UU_mean0[3][1] = -0.000601591; par_UU_mean0[3][2] = 0.000599314; par_UU_mean0[3][3] = 8.16596e-05; par_UU_mean0[3][4] = -0.0020233;
    par_UU_mean0[4][0] = 0.00841992; par_UU_mean0[4][1] = -0.00172765; par_UU_mean0[4][2] = 0.00163024; par_UU_mean0[4][3] = 0.000239463; par_UU_mean0[4][4] = -0.00556949;
    par_UU_mean0[5][0] = -0.0254205; par_UU_mean0[5][1] = 0.0055423; par_UU_mean0[5][2] = -0.011477; par_UU_mean0[5][3] = -0.00050498; par_UU_mean0[5][4] = 0.034457;
    par_UU_mean0[6][0] = -0.0203458; par_UU_mean0[6][1] = 0.00486664; par_UU_mean0[6][2] = -0.00823202; par_UU_mean0[6][3] = -0.000455608; par_UU_mean0[6][4] = 0.0265097;
    par_UU_mean0[7][0] = -0.00828952; par_UU_mean0[7][1] = 0.00275248; par_UU_mean0[7][2] = -0.00267903; par_UU_mean0[7][3] = -0.000272941; par_UU_mean0[7][4] = 0.0106496;
    par_UU_mean0[8][0] = -0.00318862; par_UU_mean0[8][1] = 0.0019151; par_UU_mean0[8][2] = -0.00166515; par_UU_mean0[8][3] = -0.000225035; par_UU_mean0[8][4] = 0.0051282;
    par_UU_mean0[9][0] = -0.0329609; par_UU_mean0[9][1] = 0.00864332; par_UU_mean0[9][2] = -0.0151606; par_UU_mean0[9][3] = -0.00106994; par_UU_mean0[9][4] = 0.0416581;

    par_UU_mean1[0][0] = 0.0217327; par_UU_mean1[0][1] = -0.00376925; par_UU_mean1[0][2] = 0.0119026; par_UU_mean1[0][3] = 0.000371063; par_UU_mean1[0][4] = -0.0290152;
    par_UU_mean1[1][0] = 0.0138835; par_UU_mean1[1][1] = -0.0026168; par_UU_mean1[1][2] = 0.00627921; par_UU_mean1[1][3] = 0.000296913; par_UU_mean1[1][4] = -0.0170327;
    par_UU_mean1[2][0] = 0.0082243; par_UU_mean1[2][1] = -0.00209956; par_UU_mean1[2][2] = 0.00172558; par_UU_mean1[2][3] = 0.000295696; par_UU_mean1[2][4] = -0.00764321;
    par_UU_mean1[3][0] = 0.0158876; par_UU_mean1[3][1] = -0.00419409; par_UU_mean1[3][2] = 0.00418684; par_UU_mean1[3][3] = 0.000596382; par_UU_mean1[3][4] = -0.0164394;
    par_UU_mean1[4][0] = 0.0239582; par_UU_mean1[4][1] = -0.00646053; par_UU_mean1[4][2] = 0.00593426; par_UU_mean1[4][3] = 0.000889122; par_UU_mean1[4][4] = -0.0241584;
    par_UU_mean1[5][0] = -0.01314; par_UU_mean1[5][1] = 0.00247501; par_UU_mean1[5][2] = -0.00843061; par_UU_mean1[5][3] = -0.00023684; par_UU_mean1[5][4] = 0.0195233;
    par_UU_mean1[6][0] = -0.0241161; par_UU_mean1[6][1] = 0.00511707; par_UU_mean1[6][2] = -0.0116556; par_UU_mean1[6][3] = -0.000556432; par_UU_mean1[6][4] = 0.0314157;
    par_UU_mean1[7][0] = -0.0374854; par_UU_mean1[7][1] = 0.00904196; par_UU_mean1[7][2] = -0.0152265; par_UU_mean1[7][3] = -0.00110474; par_UU_mean1[7][4] = 0.0452968;
    par_UU_mean1[8][0] = -0.0104037; par_UU_mean1[8][1] = 0.00326608; par_UU_mean1[8][2] = -0.00274044; par_UU_mean1[8][3] = -0.00045961; par_UU_mean1[8][4] = 0.0112769;
    par_UU_mean1[9][0] = -0.000850048; par_UU_mean1[9][1] = 0.00140993; par_UU_mean1[9][2] = 0.0037092; par_UU_mean1[9][3] = -0.000232483; par_UU_mean1[9][4] = -0.00312077;

    par_UU_mean2[0][0] = 8.90689; par_UU_mean2[0][1] = -2.48588; par_UU_mean2[0][2] = 2.56852; par_UU_mean2[0][3] = 0.301022; par_UU_mean2[0][4] = -8.66793;
    par_UU_mean2[1][0] = 7.4441; par_UU_mean2[1][1] = -1.94782; par_UU_mean2[1][2] = 2.29766; par_UU_mean2[1][3] = 0.232138; par_UU_mean2[1][4] = -7.45977;
    par_UU_mean2[2][0] = 6.16991; par_UU_mean2[2][1] = -1.56273; par_UU_mean2[2][2] = 1.90388; par_UU_mean2[2][3] = 0.186797; par_UU_mean2[2][4] = -6.23446;
    par_UU_mean2[3][0] = 3.83302; par_UU_mean2[3][1] = -0.991753; par_UU_mean2[3][2] = 1.08031; par_UU_mean2[3][3] = 0.118138; par_UU_mean2[3][4] = -3.77822;
    par_UU_mean2[4][0] = 0.905167; par_UU_mean2[4][1] = -0.323464; par_UU_mean2[4][2] = 0.0424665; par_UU_mean2[4][3] = 0.0422175; par_UU_mean2[4][4] = -0.597528;
    par_UU_mean2[5][0] = -0.278368; par_UU_mean2[5][1] = 0.0305897; par_UU_mean2[5][2] = -0.271736; par_UU_mean2[5][3] = 0.000194453; par_UU_mean2[5][4] = 0.45844;
    par_UU_mean2[6][0] = -2.9543; par_UU_mean2[6][1] = 0.701829; par_UU_mean2[6][2] = -1.2472; par_UU_mean2[6][3] = -0.0772555; par_UU_mean2[6][4] = 3.42378;
    par_UU_mean2[7][0] = -4.8532; par_UU_mean2[7][1] = 1.20154; par_UU_mean2[7][2] = -2.06256; par_UU_mean2[7][3] = -0.137431; par_UU_mean2[7][4] = 5.59941;
    par_UU_mean2[8][0] = -7.37874; par_UU_mean2[8][1] = 1.88497; par_UU_mean2[8][2] = -2.90408; par_UU_mean2[8][3] = -0.21869; par_UU_mean2[8][4] = 8.23082;
    par_UU_mean2[9][0] = -9.51152; par_UU_mean2[9][1] = 2.56806; par_UU_mean2[9][2] = -3.5366; par_UU_mean2[9][3] = -0.303582; par_UU_mean2[9][4] = 10.403;

    par_UU_mean3[0][0] = 7.39013; par_UU_mean3[0][1] = -2.17882; par_UU_mean3[0][2] = 2.00763; par_UU_mean3[0][3] = 0.265612; par_UU_mean3[0][4] = -6.63435;
    par_UU_mean3[1][0] = 6.856; par_UU_mean3[1][1] = -1.80838; par_UU_mean3[1][2] = 2.27119; par_UU_mean3[1][3] = 0.214578; par_UU_mean3[1][4] = -6.78142;
    par_UU_mean3[2][0] = 4.59574; par_UU_mean3[2][1] = -1.17061; par_UU_mean3[2][2] = 1.49652; par_UU_mean3[2][3] = 0.137142; par_UU_mean3[2][4] = -4.4591;
    par_UU_mean3[3][0] = 2.87217; par_UU_mean3[3][1] = -0.733373; par_UU_mean3[3][2] = 0.898126; par_UU_mean3[3][3] = 0.0860981; par_UU_mean3[3][4] = -2.78403;
    par_UU_mean3[4][0] = 1.86951; par_UU_mean3[4][1] = -0.497526; par_UU_mean3[4][2] = 0.553403; par_UU_mean3[4][3] = 0.0599443; par_UU_mean3[4][4] = -1.96113;
    par_UU_mean3[5][0] = -1.7149; par_UU_mean3[5][1] = 0.409596; par_UU_mean3[5][2] = -0.839946; par_UU_mean3[5][3] = -0.0470441; par_UU_mean3[5][4] = 1.9855;
    par_UU_mean3[6][0] = -6.31331; par_UU_mean3[6][1] = 1.49979; par_UU_mean3[6][2] = -2.67023; par_UU_mean3[6][3] = -0.173768; par_UU_mean3[6][4] = 7.35779;
    par_UU_mean3[7][0] = -5.83176; par_UU_mean3[7][1] = 1.43137; par_UU_mean3[7][2] = -2.44204; par_UU_mean3[7][3] = -0.162306; par_UU_mean3[7][4] = 6.61949;
    par_UU_mean3[8][0] = -8.09047; par_UU_mean3[8][1] = 2.05298; par_UU_mean3[8][2] = -3.19456; par_UU_mean3[8][3] = -0.236655; par_UU_mean3[8][4] = 8.93317;
    par_UU_mean3[9][0] = -9.56595; par_UU_mean3[9][1] = 2.58689; par_UU_mean3[9][2] = -3.57655; par_UU_mean3[9][3] = -0.30236; par_UU_mean3[9][4] = 10.2786;


    par_UU_sigma0[0][0] = 0.108501; par_UU_sigma0[0][1] = -0.0448216; par_UU_sigma0[0][2] = 0.0310884; par_UU_sigma0[0][3] = 0.0165236; par_UU_sigma0[0][4] = -0.00336178; par_UU_sigma0[0][5] = 0.000278052; par_UU_sigma0[0][6] = -0.103262;
    par_UU_sigma0[1][0] = 0.0358873; par_UU_sigma0[1][1] = -0.0156594; par_UU_sigma0[1][2] = 0.00935712; par_UU_sigma0[1][3] = 0.00687949; par_UU_sigma0[1][4] = -0.00162717; par_UU_sigma0[1][5] = 0.000153546; par_UU_sigma0[1][6] = -0.0300796;
    par_UU_sigma0[2][0] = 0.057058; par_UU_sigma0[2][1] = -0.0266993; par_UU_sigma0[2][2] = 0.0136649; par_UU_sigma0[2][3] = 0.0109889; par_UU_sigma0[2][4] = -0.00239096; par_UU_sigma0[2][5] = 0.000208013; par_UU_sigma0[2][6] = -0.0478958;
    par_UU_sigma0[3][0] = 0.0540931; par_UU_sigma0[3][1] = -0.0179648; par_UU_sigma0[3][2] = 0.0162002; par_UU_sigma0[3][3] = 0.0047759; par_UU_sigma0[3][4] = -0.000576975; par_UU_sigma0[3][5] = 2.54941e-05; par_UU_sigma0[3][6] = -0.0512327;
    par_UU_sigma0[4][0] = 0.053391; par_UU_sigma0[4][1] = -0.0174398; par_UU_sigma0[4][2] = 0.0166842; par_UU_sigma0[4][3] = 0.00527359; par_UU_sigma0[4][4] = -0.000905719; par_UU_sigma0[4][5] = 7.02169e-05; par_UU_sigma0[4][6] = -0.05165;
    par_UU_sigma0[5][0] = 0.00487249; par_UU_sigma0[5][1] = 0.00202581; par_UU_sigma0[5][2] = 0.00267903; par_UU_sigma0[5][3] = -0.00118683; par_UU_sigma0[5][4] = 0.000283227; par_UU_sigma0[5][5] = -1.79569e-05; par_UU_sigma0[5][6] = -0.00358704;
    par_UU_sigma0[6][0] = -0.051122; par_UU_sigma0[6][1] = 0.0264525; par_UU_sigma0[6][2] = -0.0124453; par_UU_sigma0[6][3] = -0.00959625; par_UU_sigma0[6][4] = 0.00179983; par_UU_sigma0[6][5] = -0.000124607; par_UU_sigma0[6][6] = 0.0500172;
    par_UU_sigma0[7][0] = -0.0516004; par_UU_sigma0[7][1] = 0.0281932; par_UU_sigma0[7][2] = -0.0111617; par_UU_sigma0[7][3] = -0.0102841; par_UU_sigma0[7][4] = 0.00188823; par_UU_sigma0[7][5] = -0.000126779; par_UU_sigma0[7][6] = 0.0481544;
    par_UU_sigma0[8][0] = -0.025252; par_UU_sigma0[8][1] = 0.016822; par_UU_sigma0[8][2] = -0.00357091; par_UU_sigma0[8][3] = -0.00605953; par_UU_sigma0[8][4] = 0.000988772; par_UU_sigma0[8][5] = -4.90675e-05; par_UU_sigma0[8][6] = 0.0220337;
    par_UU_sigma0[9][0] = 0.0690812; par_UU_sigma0[9][1] = -0.0231015; par_UU_sigma0[9][2] = 0.0228115; par_UU_sigma0[9][3] = 0.00757279; par_UU_sigma0[9][4] = -0.0015137; par_UU_sigma0[9][5] = 0.000133489; par_UU_sigma0[9][6] = -0.0700645;

    par_UU_sigma1[0][0] = 0.131534; par_UU_sigma1[0][1] = -0.053897; par_UU_sigma1[0][2] = 0.0372252; par_UU_sigma1[0][3] = 0.0190309; par_UU_sigma1[0][4] = -0.00370626; par_UU_sigma1[0][5] = 0.000296182; par_UU_sigma1[0][6] = -0.125259;
    par_UU_sigma1[1][0] = 0.129792; par_UU_sigma1[1][1] = -0.0554535; par_UU_sigma1[1][2] = 0.0355085; par_UU_sigma1[1][3] = 0.020362; par_UU_sigma1[1][4] = -0.00407505; par_UU_sigma1[1][5] = 0.000330243; par_UU_sigma1[1][6] = -0.121415;
    par_UU_sigma1[2][0] = 0.13167; par_UU_sigma1[2][1] = -0.0602603; par_UU_sigma1[2][2] = 0.0332551; par_UU_sigma1[2][3] = 0.0233573; par_UU_sigma1[2][4] = -0.00488297; par_UU_sigma1[2][5] = 0.000410165; par_UU_sigma1[2][6] = -0.118502;
    par_UU_sigma1[3][0] = 0.0437929; par_UU_sigma1[3][1] = -0.0182064; par_UU_sigma1[3][2] = 0.0111591; par_UU_sigma1[3][3] = 0.00694876; par_UU_sigma1[3][4] = -0.00141818; par_UU_sigma1[3][5] = 0.000120164; par_UU_sigma1[3][6] = -0.0372477;
    par_UU_sigma1[4][0] = 0.0279108; par_UU_sigma1[4][1] = -0.00797121; par_UU_sigma1[4][2] = 0.00863345; par_UU_sigma1[4][3] = 0.00261012; par_UU_sigma1[4][4] = -0.000552792; par_UU_sigma1[4][5] = 5.77773e-05; par_UU_sigma1[4][6] = -0.0253287;
    par_UU_sigma1[5][0] = 0.0552974; par_UU_sigma1[5][1] = -0.0195572; par_UU_sigma1[5][2] = 0.0167007; par_UU_sigma1[5][3] = 0.00655448; par_UU_sigma1[5][4] = -0.00128474; par_UU_sigma1[5][5] = 0.000112366; par_UU_sigma1[5][6] = -0.0527546;
    par_UU_sigma1[6][0] = 0.0809624; par_UU_sigma1[6][1] = -0.0316448; par_UU_sigma1[6][2] = 0.0231834; par_UU_sigma1[6][3] = 0.011076; par_UU_sigma1[6][4] = -0.00219183; par_UU_sigma1[6][5] = 0.000185765; par_UU_sigma1[6][6] = -0.0764984;
    par_UU_sigma1[7][0] = 0.153778; par_UU_sigma1[7][1] = -0.068778; par_UU_sigma1[7][2] = 0.0410517; par_UU_sigma1[7][3] = 0.0267485; par_UU_sigma1[7][4] = -0.00573634; par_UU_sigma1[7][5] = 0.00050072; par_UU_sigma1[7][6] = -0.142615;
    par_UU_sigma1[8][0] = 0.0875228; par_UU_sigma1[8][1] = -0.0333335; par_UU_sigma1[8][2] = 0.0257521; par_UU_sigma1[8][3] = 0.0110977; par_UU_sigma1[8][4] = -0.00200309; par_UU_sigma1[8][5] = 0.000150208; par_UU_sigma1[8][6] = -0.0842535;
    par_UU_sigma1[9][0] = 0.147018; par_UU_sigma1[9][1] = -0.061202; par_UU_sigma1[9][2] = 0.0416924; par_UU_sigma1[9][3] = 0.0219827; par_UU_sigma1[9][4] = -0.00429649; par_UU_sigma1[9][5] = 0.000339113; par_UU_sigma1[9][6] = -0.140531;

    par_UU_sigma2[0][0] = 13.8046; par_UU_sigma2[0][1] = -6.04115; par_UU_sigma2[0][2] = 4.29001; par_UU_sigma2[0][3] = 2.32435; par_UU_sigma2[0][4] = -0.48535; par_UU_sigma2[0][5] = 0.0414236; par_UU_sigma2[0][6] = -12.7347;
    par_UU_sigma2[1][0] = 13.7433; par_UU_sigma2[1][1] = -6.08239; par_UU_sigma2[1][2] = 4.27484; par_UU_sigma2[1][3] = 2.38377; par_UU_sigma2[1][4] = -0.509395; par_UU_sigma2[1][5] = 0.0443245; par_UU_sigma2[1][6] = -12.689;
    par_UU_sigma2[2][0] = 9.77283; par_UU_sigma2[2][1] = -3.90772; par_UU_sigma2[2][2] = 3.44794; par_UU_sigma2[2][3] = 1.44694; par_UU_sigma2[2][4] = -0.297262; par_UU_sigma2[2][5] = 0.0255655; par_UU_sigma2[2][6] = -9.33243;
    par_UU_sigma2[3][0] = 8.07581; par_UU_sigma2[3][1] = -2.9936; par_UU_sigma2[3][2] = 3.10156; par_UU_sigma2[3][3] = 1.06801; par_UU_sigma2[3][4] = -0.212931; par_UU_sigma2[3][5] = 0.0181682; par_UU_sigma2[3][6] = -7.87963;
    par_UU_sigma2[4][0] = 9.60541; par_UU_sigma2[4][1] = -3.64992; par_UU_sigma2[4][2] = 3.45656; par_UU_sigma2[4][3] = 1.29785; par_UU_sigma2[4][4] = -0.256157; par_UU_sigma2[4][5] = 0.0213657; par_UU_sigma2[4][6] = -9.28703;
    par_UU_sigma2[5][0] = 11.2728; par_UU_sigma2[5][1] = -4.61263; par_UU_sigma2[5][2] = 3.70957; par_UU_sigma2[5][3] = 1.70024; par_UU_sigma2[5][4] = -0.344346; par_UU_sigma2[5][5] = 0.0290254; par_UU_sigma2[5][6] = -10.5606;
    par_UU_sigma2[6][0] = 9.75144; par_UU_sigma2[6][1] = -4.13197; par_UU_sigma2[6][2] = 3.16547; par_UU_sigma2[6][3] = 1.56035; par_UU_sigma2[6][4] = -0.318912; par_UU_sigma2[6][5] = 0.0270738; par_UU_sigma2[6][6] = -8.85335;
    par_UU_sigma2[7][0] = 9.68533; par_UU_sigma2[7][1] = -4.04811; par_UU_sigma2[7][2] = 3.26; par_UU_sigma2[7][3] = 1.5268; par_UU_sigma2[7][4] = -0.30937; par_UU_sigma2[7][5] = 0.0256434; par_UU_sigma2[7][6] = -8.97497;
    par_UU_sigma2[8][0] = 15.3805; par_UU_sigma2[8][1] = -6.80313; par_UU_sigma2[8][2] = 4.6249; par_UU_sigma2[8][3] = 2.59007; par_UU_sigma2[8][4] = -0.530365; par_UU_sigma2[8][5] = 0.0438876; par_UU_sigma2[8][6] = -14.1322;
    par_UU_sigma2[9][0] = 11.088; par_UU_sigma2[9][1] = -4.97418; par_UU_sigma2[9][2] = 3.4196; par_UU_sigma2[9][3] = 1.96649; par_UU_sigma2[9][4] = -0.417498; par_UU_sigma2[9][5] = 0.0361548; par_UU_sigma2[9][6] = -9.92555;

    par_UU_sigma3[0][0] = 7.53894; par_UU_sigma3[0][1] = -3.60225; par_UU_sigma3[0][2] = 2.43878; par_UU_sigma3[0][3] = 1.57669; par_UU_sigma3[0][4] = -0.360935; par_UU_sigma3[0][5] = 0.0328355; par_UU_sigma3[0][6] = -6.44816;
    par_UU_sigma3[1][0] = 7.61749; par_UU_sigma3[1][1] = -3.32848; par_UU_sigma3[1][2] = 2.56904; par_UU_sigma3[1][3] = 1.33685; par_UU_sigma3[1][4] = -0.285949; par_UU_sigma3[1][5] = 0.0248061; par_UU_sigma3[1][6] = -6.78011;
    par_UU_sigma3[2][0] = 3.91382; par_UU_sigma3[2][1] = -1.63542; par_UU_sigma3[2][2] = 1.60941; par_UU_sigma3[2][3] = 0.746049; par_UU_sigma3[2][4] = -0.180711; par_UU_sigma3[2][5] = 0.0175835; par_UU_sigma3[2][6] = -3.31473;
    par_UU_sigma3[3][0] = 3.90477; par_UU_sigma3[3][1] = -1.34403; par_UU_sigma3[3][2] = 1.79431; par_UU_sigma3[3][3] = 0.542765; par_UU_sigma3[3][4] = -0.123402; par_UU_sigma3[3][5] = 0.0120003; par_UU_sigma3[3][6] = -3.61809;
    par_UU_sigma3[4][0] = 3.92718; par_UU_sigma3[4][1] = -1.06595; par_UU_sigma3[4][2] = 1.93312; par_UU_sigma3[4][3] = 0.35212; par_UU_sigma3[4][4] = -0.0698796; par_UU_sigma3[4][5] = 0.00663323; par_UU_sigma3[4][6] = -3.90973;
    par_UU_sigma3[5][0] = 7.06776; par_UU_sigma3[5][1] = -2.67567; par_UU_sigma3[5][2] = 2.72236; par_UU_sigma3[5][3] = 1.0101; par_UU_sigma3[5][4] = -0.211555; par_UU_sigma3[5][5] = 0.0184978; par_UU_sigma3[5][6] = -6.7731;
    par_UU_sigma3[6][0] = 7.04955; par_UU_sigma3[6][1] = -2.84457; par_UU_sigma3[6][2] = 2.77585; par_UU_sigma3[6][3] = 1.15612; par_UU_sigma3[6][4] = -0.256437; par_UU_sigma3[6][5] = 0.0231188; par_UU_sigma3[6][6] = -6.74196;
    par_UU_sigma3[7][0] = 23.5209; par_UU_sigma3[7][1] = -10.4731; par_UU_sigma3[7][2] = 6.83576; par_UU_sigma3[7][3] = 3.95303; par_UU_sigma3[7][4] = -0.809286; par_UU_sigma3[7][5] = 0.0672325; par_UU_sigma3[7][6] = -21.9323;
    par_UU_sigma3[8][0] = 17.823; par_UU_sigma3[8][1] = -7.89008; par_UU_sigma3[8][2] = 5.34006; par_UU_sigma3[8][3] = 2.97932; par_UU_sigma3[8][4] = -0.603383; par_UU_sigma3[8][5] = 0.0492889; par_UU_sigma3[8][6] = -16.5402;
    par_UU_sigma3[9][0] = 17.152; par_UU_sigma3[9][1] = -6.71819; par_UU_sigma3[9][2] = 5.68566; par_UU_sigma3[9][3] = 2.25277; par_UU_sigma3[9][4] = -0.404451; par_UU_sigma3[9][5] = 0.0296786; par_UU_sigma3[9][6] = -16.8121;


    par_UU_smean0[0][0] = -1.82992; par_UU_smean0[0][1] = 0.462561; par_UU_smean0[0][2] = -0.585783; par_UU_smean0[0][3] = -0.0550587; par_UU_smean0[0][4] = 1.97444;
    par_UU_smean0[1][0] = 0.371812; par_UU_smean0[1][1] = -0.168256; par_UU_smean0[1][2] = 0.0968497; par_UU_smean0[1][3] = 0.03375; par_UU_smean0[1][4] = -0.34491;
    par_UU_smean0[2][0] = 6.41957; par_UU_smean0[2][1] = -1.75322; par_UU_smean0[2][2] = 2.35344; par_UU_smean0[2][3] = 0.233011; par_UU_smean0[2][4] = -7.28744;
    par_UU_smean0[3][0] = 4.72306; par_UU_smean0[3][1] = -1.47305; par_UU_smean0[3][2] = 1.72437; par_UU_smean0[3][3] = 0.221128; par_UU_smean0[3][4] = -5.22042;
    par_UU_smean0[4][0] = 0.967739; par_UU_smean0[4][1] = -0.562268; par_UU_smean0[4][2] = 0.433185; par_UU_smean0[4][3] = 0.119716; par_UU_smean0[4][4] = -0.99862;
    par_UU_smean0[5][0] = 4.81713; par_UU_smean0[5][1] = -1.06205; par_UU_smean0[5][2] = 2.35875; par_UU_smean0[5][3] = 0.132938; par_UU_smean0[5][4] = -6.28018;
    par_UU_smean0[6][0] = 1.56509; par_UU_smean0[6][1] = -0.382728; par_UU_smean0[6][2] = 0.820014; par_UU_smean0[6][3] = 0.0561533; par_UU_smean0[6][4] = -2.09779;
    par_UU_smean0[7][0] = 5.30142; par_UU_smean0[7][1] = -1.21368; par_UU_smean0[7][2] = 2.18375; par_UU_smean0[7][3] = 0.139784; par_UU_smean0[7][4] = -6.41975;
    par_UU_smean0[8][0] = -1.24949; par_UU_smean0[8][1] = 0.281974; par_UU_smean0[8][2] = -0.413323; par_UU_smean0[8][3] = -0.0297112; par_UU_smean0[8][4] = 1.395;
    par_UU_smean0[9][0] = 16.6183; par_UU_smean0[9][1] = -3.4366; par_UU_smean0[9][2] = 7.91141; par_UU_smean0[9][3] = 0.369847; par_UU_smean0[9][4] = -21.4795;

    par_UU_smean1[0][0] = -1.13054; par_UU_smean1[0][1] = 0.193814; par_UU_smean1[0][2] = -0.569704; par_UU_smean1[0][3] = -0.0164714; par_UU_smean1[0][4] = 1.52625;
    par_UU_smean1[1][0] = -1.45234; par_UU_smean1[1][1] = 0.339657; par_UU_smean1[1][2] = -0.587608; par_UU_smean1[1][3] = -0.0412586; par_UU_smean1[1][4] = 1.74003;
    par_UU_smean1[2][0] = -0.297306; par_UU_smean1[2][1] = 0.100449; par_UU_smean1[2][2] = -0.0705105; par_UU_smean1[2][3] = -0.0168242; par_UU_smean1[2][4] = 0.282088;
    par_UU_smean1[3][0] = -1.48523; par_UU_smean1[3][1] = 0.300835; par_UU_smean1[3][2] = -0.605586; par_UU_smean1[3][3] = -0.0297462; par_UU_smean1[3][4] = 1.80816;
    par_UU_smean1[4][0] = -0.238777; par_UU_smean1[4][1] = 0.0942998; par_UU_smean1[4][2] = 0.0471078; par_UU_smean1[4][3] = -0.0139639; par_UU_smean1[4][4] = 0.0854373;
    par_UU_smean1[5][0] = -0.760469; par_UU_smean1[5][1] = 0.155285; par_UU_smean1[5][2] = -0.351387; par_UU_smean1[5][3] = -0.0163297; par_UU_smean1[5][4] = 0.978745;
    par_UU_smean1[6][0] = 0.994192; par_UU_smean1[6][1] = -0.238645; par_UU_smean1[6][2] = 0.40814; par_UU_smean1[6][3] = 0.0301062; par_UU_smean1[6][4] = -1.18337;
    par_UU_smean1[7][0] = 1.51548; par_UU_smean1[7][1] = -0.344516; par_UU_smean1[7][2] = 0.644273; par_UU_smean1[7][3] = 0.0413709; par_UU_smean1[7][4] = -1.85685;
    par_UU_smean1[8][0] = 0.051747; par_UU_smean1[8][1] = -0.0155946; par_UU_smean1[8][2] = 0.0454654; par_UU_smean1[8][3] = 0.00424286; par_UU_smean1[8][4] = -0.0929732;
    par_UU_smean1[9][0] = 0.352171; par_UU_smean1[9][1] = -0.074143; par_UU_smean1[9][2] = 0.150027; par_UU_smean1[9][3] = 0.00902482; par_UU_smean1[9][4] = -0.447571;

    par_UU_smean2[0][0] = 1.89202; par_UU_smean2[0][1] = -0.469616; par_UU_smean2[0][2] = 0.776324; par_UU_smean2[0][3] = 0.0563193; par_UU_smean2[0][4] = -2.30572;
    par_UU_smean2[1][0] = -0.813164; par_UU_smean2[1][1] = 0.189123; par_UU_smean2[1][2] = -0.277078; par_UU_smean2[1][3] = -0.0241038; par_UU_smean2[1][4] = 0.883986;
    par_UU_smean2[2][0] = -1.11996; par_UU_smean2[2][1] = 0.249489; par_UU_smean2[2][2] = -0.418019; par_UU_smean2[2][3] = -0.0296251; par_UU_smean2[2][4] = 1.28607;
    par_UU_smean2[3][0] = 0.172518; par_UU_smean2[3][1] = -0.0415114; par_UU_smean2[3][2] = 0.0990061; par_UU_smean2[3][3] = 0.00445299; par_UU_smean2[3][4] = -0.254096;
    par_UU_smean2[4][0] = 0.251088; par_UU_smean2[4][1] = -0.0571433; par_UU_smean2[4][2] = 0.116903; par_UU_smean2[4][3] = 0.00660503; par_UU_smean2[4][4] = -0.324729;
    par_UU_smean2[5][0] = -0.413033; par_UU_smean2[5][1] = 0.0852195; par_UU_smean2[5][2] = -0.198074; par_UU_smean2[5][3] = -0.0104664; par_UU_smean2[5][4] = 0.541951;
    par_UU_smean2[6][0] = 0.61223; par_UU_smean2[6][1] = -0.168397; par_UU_smean2[6][2] = 0.201458; par_UU_smean2[6][3] = 0.0220969; par_UU_smean2[6][4] = -0.666424;
    par_UU_smean2[7][0] = -0.65469; par_UU_smean2[7][1] = 0.156524; par_UU_smean2[7][2] = -0.235203; par_UU_smean2[7][3] = -0.0202939; par_UU_smean2[7][4] = 0.742892;
    par_UU_smean2[8][0] = -1.81712; par_UU_smean2[8][1] = 0.431236; par_UU_smean2[8][2] = -0.648295; par_UU_smean2[8][3] = -0.0518873; par_UU_smean2[8][4] = 2.05555;
    par_UU_smean2[9][0] = 0.142594; par_UU_smean2[9][1] = -0.035216; par_UU_smean2[9][2] = 0.111149; par_UU_smean2[9][3] = 0.00192753; par_UU_smean2[9][4] = -0.250549;

    par_UU_smean3[0][0] = 1.90516; par_UU_smean3[0][1] = -0.449323; par_UU_smean3[0][2] = 0.873645; par_UU_smean3[0][3] = 0.0526972; par_UU_smean3[0][4] = -2.43026;
    par_UU_smean3[1][0] = 0.687371; par_UU_smean3[1][1] = -0.21443; par_UU_smean3[1][2] = 0.236201; par_UU_smean3[1][3] = 0.02903; par_UU_smean3[1][4] = -0.768656;
    par_UU_smean3[2][0] = -1.11847; par_UU_smean3[2][1] = 0.356909; par_UU_smean3[2][2] = -0.26578; par_UU_smean3[2][3] = -0.0542914; par_UU_smean3[2][4] = 1.0363;
    par_UU_smean3[3][0] = 0.109521; par_UU_smean3[3][1] = -0.0466276; par_UU_smean3[3][2] = 0.0430552; par_UU_smean3[3][3] = 0.00759025; par_UU_smean3[3][4] = -0.131292;
    par_UU_smean3[4][0] = 0.135547; par_UU_smean3[4][1] = -0.0376509; par_UU_smean3[4][2] = 0.0553309; par_UU_smean3[4][3] = 0.00436377; par_UU_smean3[4][4] = -0.165176;
    par_UU_smean3[5][0] = -0.335719; par_UU_smean3[5][1] = 0.0680567; par_UU_smean3[5][2] = -0.151095; par_UU_smean3[5][3] = -0.00757415; par_UU_smean3[5][4] = 0.430181;
    par_UU_smean3[6][0] = -0.177061; par_UU_smean3[6][1] = 0.0466452; par_UU_smean3[6][2] = -0.0302085; par_UU_smean3[6][3] = -0.00467128; par_UU_smean3[6][4] = 0.157964;
    par_UU_smean3[7][0] = -0.997307; par_UU_smean3[7][1] = 0.217577; par_UU_smean3[7][2] = -0.379546; par_UU_smean3[7][3] = -0.0242757; par_UU_smean3[7][4] = 1.1695;
    par_UU_smean3[8][0] = 0.999999; par_UU_smean3[8][1] = -0.205924; par_UU_smean3[8][2] = 0.475128; par_UU_smean3[8][3] = 0.0197075; par_UU_smean3[8][4] = -1.30976;
    par_UU_smean3[9][0] = 0.893246; par_UU_smean3[9][1] = -0.253611; par_UU_smean3[9][2] = 0.370045; par_UU_smean3[9][3] = 0.0351925; par_UU_smean3[9][4] = -1.07872;


    par_UU_ssigma0[0][0] = 30.8592; par_UU_ssigma0[0][1] = -14.3827; par_UU_ssigma0[0][2] = 7.36357; par_UU_ssigma0[0][3] = 5.44665; par_UU_ssigma0[0][4] = -27.3816; par_UU_ssigma0[0][5] = -1.07099; par_UU_ssigma0[0][6] = 0.0816195;
    par_UU_ssigma0[1][0] = -31.5458; par_UU_ssigma0[1][1] = 13.0404; par_UU_ssigma0[1][2] = -9.24814; par_UU_ssigma0[1][3] = -4.15233; par_UU_ssigma0[1][4] = 32.1484; par_UU_ssigma0[1][5] = 0.716908; par_UU_ssigma0[1][6] = -0.0497298;
    par_UU_ssigma0[2][0] = -18.9687; par_UU_ssigma0[2][1] = 9.75544; par_UU_ssigma0[2][2] = -4.66152; par_UU_ssigma0[2][3] = -3.82679; par_UU_ssigma0[2][4] = 17.8729; par_UU_ssigma0[2][5] = 0.799828; par_UU_ssigma0[2][6] = -0.0646597;
    par_UU_ssigma0[3][0] = -9.49701; par_UU_ssigma0[3][1] = 4.49845; par_UU_ssigma0[3][2] = -2.7597; par_UU_ssigma0[3][3] = -1.6044; par_UU_ssigma0[3][4] = 9.94837; par_UU_ssigma0[3][5] = 0.325359; par_UU_ssigma0[3][6] = -0.0261478;
    par_UU_ssigma0[4][0] = -2.74173; par_UU_ssigma0[4][1] = 3.6181; par_UU_ssigma0[4][2] = 0.0344637; par_UU_ssigma0[4][3] = -2.26669; par_UU_ssigma0[4][4] = 1.64463; par_UU_ssigma0[4][5] = 0.664383; par_UU_ssigma0[4][6] = -0.0682956;
    par_UU_ssigma0[5][0] = -7.52; par_UU_ssigma0[5][1] = 5.02512; par_UU_ssigma0[5][2] = -1.49495; par_UU_ssigma0[5][3] = -2.40568; par_UU_ssigma0[5][4] = 6.71049; par_UU_ssigma0[5][5] = 0.607875; par_UU_ssigma0[5][6] = -0.0576784;
    par_UU_ssigma0[6][0] = -12.5235; par_UU_ssigma0[6][1] = 5.80834; par_UU_ssigma0[6][2] = -3.41257; par_UU_ssigma0[6][3] = -2.09192; par_UU_ssigma0[6][4] = 12.6611; par_UU_ssigma0[6][5] = 0.430453; par_UU_ssigma0[6][6] = -0.035321;
    par_UU_ssigma0[7][0] = 87.1362; par_UU_ssigma0[7][1] = -34.595; par_UU_ssigma0[7][2] = 24.9242; par_UU_ssigma0[7][3] = 11.0781; par_UU_ssigma0[7][4] = -85.7435; par_UU_ssigma0[7][5] = -1.90241; par_UU_ssigma0[7][6] = 0.130611;
    par_UU_ssigma0[8][0] = -18.2496; par_UU_ssigma0[8][1] = 9.86403; par_UU_ssigma0[8][2] = -4.8209; par_UU_ssigma0[8][3] = -4.27639; par_UU_ssigma0[8][4] = 17.5696; par_UU_ssigma0[8][5] = 0.988033; par_UU_ssigma0[8][6] = -0.0874924;
    par_UU_ssigma0[9][0] = 165.951; par_UU_ssigma0[9][1] = -61.1532; par_UU_ssigma0[9][2] = 52.6915; par_UU_ssigma0[9][3] = 18.1045; par_UU_ssigma0[9][4] = -171.855; par_UU_ssigma0[9][5] = -2.86295; par_UU_ssigma0[9][6] = 0.179712;

    par_UU_ssigma1[0][0] = 130.983; par_UU_ssigma1[0][1] = -52.5926; par_UU_ssigma1[0][2] = 37.1028; par_UU_ssigma1[0][3] = 16.7491; par_UU_ssigma1[0][4] = -128.555; par_UU_ssigma1[0][5] = -2.82758; par_UU_ssigma1[0][6] = 0.189132;
    par_UU_ssigma1[1][0] = -30.3011; par_UU_ssigma1[1][1] = 15.1297; par_UU_ssigma1[1][2] = -7.64991; par_UU_ssigma1[1][3] = -5.83881; par_UU_ssigma1[1][4] = 28.5172; par_UU_ssigma1[1][5] = 1.19586; par_UU_ssigma1[1][6] = -0.0955383;
    par_UU_ssigma1[2][0] = 30.6741; par_UU_ssigma1[2][1] = -14.3987; par_UU_ssigma1[2][2] = 7.20085; par_UU_ssigma1[2][3] = 5.53769; par_UU_ssigma1[2][4] = -26.9649; par_UU_ssigma1[2][5] = -1.13305; par_UU_ssigma1[2][6] = 0.0922681;
    par_UU_ssigma1[3][0] = 66.3024; par_UU_ssigma1[3][1] = -30.5052; par_UU_ssigma1[3][2] = 16.3469; par_UU_ssigma1[3][3] = 11.0326; par_UU_ssigma1[3][4] = -60.2848; par_UU_ssigma1[3][5] = -2.06668; par_UU_ssigma1[3][6] = 0.150952;
    par_UU_ssigma1[4][0] = 23.0713; par_UU_ssigma1[4][1] = -7.36967; par_UU_ssigma1[4][2] = 7.57329; par_UU_ssigma1[4][3] = 1.6305; par_UU_ssigma1[4][4] = -23.9269; par_UU_ssigma1[4][5] = -0.0835355; par_UU_ssigma1[4][6] = -0.0113098;
    par_UU_ssigma1[5][0] = -114.683; par_UU_ssigma1[5][1] = 52.3961; par_UU_ssigma1[5][2] = -32.2271; par_UU_ssigma1[5][3] = -19.7548; par_UU_ssigma1[5][4] = 111.571; par_UU_ssigma1[5][5] = 4.03593; par_UU_ssigma1[5][6] = -0.325645;
    par_UU_ssigma1[6][0] = 21.8245; par_UU_ssigma1[6][1] = -8.53164; par_UU_ssigma1[6][2] = 5.98218; par_UU_ssigma1[6][3] = 2.71749; par_UU_ssigma1[6][4] = -20.5754; par_UU_ssigma1[6][5] = -0.445076; par_UU_ssigma1[6][6] = 0.0283158;
    par_UU_ssigma1[7][0] = -84.2215; par_UU_ssigma1[7][1] = 38.8095; par_UU_ssigma1[7][2] = -22.3389; par_UU_ssigma1[7][3] = -14.3367; par_UU_ssigma1[7][4] = 80.457; par_UU_ssigma1[7][5] = 2.85593; par_UU_ssigma1[7][6] = -0.225834;
    par_UU_ssigma1[8][0] = -2.66052; par_UU_ssigma1[8][1] = 3.70164; par_UU_ssigma1[8][2] = -0.358185; par_UU_ssigma1[8][3] = -2.40429; par_UU_ssigma1[8][4] = 2.12248; par_UU_ssigma1[8][5] = 0.693076; par_UU_ssigma1[8][6] = -0.0699339;
    par_UU_ssigma1[9][0] = -76.923; par_UU_ssigma1[9][1] = 34.5323; par_UU_ssigma1[9][2] = -21.0815; par_UU_ssigma1[9][3] = -12.3745; par_UU_ssigma1[9][4] = 74.6438; par_UU_ssigma1[9][5] = 2.37488; par_UU_ssigma1[9][6] = -0.180119;

    par_UU_ssigma2[0][0] = -42.5731; par_UU_ssigma2[0][1] = 18.8948; par_UU_ssigma2[0][2] = -11.3303; par_UU_ssigma2[0][3] = -6.45012; par_UU_ssigma2[0][4] = 41.1312; par_UU_ssigma2[0][5] = 1.1819; par_UU_ssigma2[0][6] = -0.0870335;
    par_UU_ssigma2[1][0] = -30.7588; par_UU_ssigma2[1][1] = 14.2067; par_UU_ssigma2[1][2] = -8.28954; par_UU_ssigma2[1][3] = -5.10304; par_UU_ssigma2[1][4] = 29.9128; par_UU_ssigma2[1][5] = 0.987615; par_UU_ssigma2[1][6] = -0.0763205;
    par_UU_ssigma2[2][0] = -78.3476; par_UU_ssigma2[2][1] = 36.311; par_UU_ssigma2[2][2] = -20.3409; par_UU_ssigma2[2][3] = -13.2542; par_UU_ssigma2[2][4] = 74.0968; par_UU_ssigma2[2][5] = 2.57939; par_UU_ssigma2[2][6] = -0.1986;
    par_UU_ssigma2[3][0] = 6.51648; par_UU_ssigma2[3][1] = -2.68773; par_UU_ssigma2[3][2] = 1.53802; par_UU_ssigma2[3][3] = 1.10909; par_UU_ssigma2[3][4] = -5.41255; par_UU_ssigma2[3][5] = -0.243496; par_UU_ssigma2[3][6] = 0.0205374;
    par_UU_ssigma2[4][0] = 36.2254; par_UU_ssigma2[4][1] = -14.4419; par_UU_ssigma2[4][2] = 10.6276; par_UU_ssigma2[4][3] = 4.89275; par_UU_ssigma2[4][4] = -35.631; par_UU_ssigma2[4][5] = -0.901325; par_UU_ssigma2[4][6] = 0.0662796;
    par_UU_ssigma2[5][0] = 17.022; par_UU_ssigma2[5][1] = -7.35266; par_UU_ssigma2[5][2] = 4.34789; par_UU_ssigma2[5][3] = 2.75195; par_UU_ssigma2[5][4] = -15.436; par_UU_ssigma2[5][5] = -0.54539; par_UU_ssigma2[5][6] = 0.0419392;
    par_UU_ssigma2[6][0] = -100.073; par_UU_ssigma2[6][1] = 45.3951; par_UU_ssigma2[6][2] = -26.5512; par_UU_ssigma2[6][3] = -16.3409; par_UU_ssigma2[6][4] = 95.4879; par_UU_ssigma2[6][5] = 3.1481; par_UU_ssigma2[6][6] = -0.240596;
    par_UU_ssigma2[7][0] = 27.1976; par_UU_ssigma2[7][1] = -10.9153; par_UU_ssigma2[7][2] = 7.80007; par_UU_ssigma2[7][3] = 3.75235; par_UU_ssigma2[7][4] = -26.3574; par_UU_ssigma2[7][5] = -0.695996; par_UU_ssigma2[7][6] = 0.0510135;
    par_UU_ssigma2[8][0] = 6.91534; par_UU_ssigma2[8][1] = -2.33207; par_UU_ssigma2[8][2] = 2.29213; par_UU_ssigma2[8][3] = 0.953762; par_UU_ssigma2[8][4] = -6.80814; par_UU_ssigma2[8][5] = -0.21677; par_UU_ssigma2[8][6] = 0.0184196;
    par_UU_ssigma2[9][0] = 24.2893; par_UU_ssigma2[9][1] = -10.3267; par_UU_ssigma2[9][2] = 6.78633; par_UU_ssigma2[9][3] = 3.86959; par_UU_ssigma2[9][4] = -23.0827; par_UU_ssigma2[9][5] = -0.790061; par_UU_ssigma2[9][6] = 0.0638526;

    par_UU_ssigma3[0][0] = 64.8985; par_UU_ssigma3[0][1] = -29.3673; par_UU_ssigma3[0][2] = 15.9973; par_UU_ssigma3[0][3] = 10.5649; par_UU_ssigma3[0][4] = -59.2252; par_UU_ssigma3[0][5] = -2.01152; par_UU_ssigma3[0][6] = 0.150937;
    par_UU_ssigma3[1][0] = 62.9778; par_UU_ssigma3[1][1] = -25.132; par_UU_ssigma3[1][2] = 17.9307; par_UU_ssigma3[1][3] = 8.12977; par_UU_ssigma3[1][4] = -61.6599; par_UU_ssigma3[1][5] = -1.41002; par_UU_ssigma3[1][6] = 0.0974973;
    par_UU_ssigma3[2][0] = 43.2299; par_UU_ssigma3[2][1] = -14.6107; par_UU_ssigma3[2][2] = 13.6921; par_UU_ssigma3[2][3] = 3.90002; par_UU_ssigma3[2][4] = -44.7478; par_UU_ssigma3[2][5] = -0.544787; par_UU_ssigma3[2][6] = 0.0294017;
    par_UU_ssigma3[3][0] = 12.5907; par_UU_ssigma3[3][1] = -5.17829; par_UU_ssigma3[3][2] = 3.27036; par_UU_ssigma3[3][3] = 1.88944; par_UU_ssigma3[3][4] = -11.3964; par_UU_ssigma3[3][5] = -0.363452; par_UU_ssigma3[3][6] = 0.0269499;
    par_UU_ssigma3[4][0] = -29.576; par_UU_ssigma3[4][1] = 12.4171; par_UU_ssigma3[4][2] = -8.54443; par_UU_ssigma3[4][3] = -4.0281; par_UU_ssigma3[4][4] = 29.9126; par_UU_ssigma3[4][5] = 0.711682; par_UU_ssigma3[4][6] = -0.051138;
    par_UU_ssigma3[5][0] = 13.7329; par_UU_ssigma3[5][1] = -4.13209; par_UU_ssigma3[5][2] = 4.2851; par_UU_ssigma3[5][3] = 0.944625; par_UU_ssigma3[5][4] = -13.8818; par_UU_ssigma3[5][5] = -0.0855929; par_UU_ssigma3[5][6] = -0.000209437;
    par_UU_ssigma3[6][0] = 65.0757; par_UU_ssigma3[6][1] = -24.3138; par_UU_ssigma3[6][2] = 19.2309; par_UU_ssigma3[6][3] = 7.19505; par_UU_ssigma3[6][4] = -65.1779; par_UU_ssigma3[6][5] = -1.11937; par_UU_ssigma3[6][6] = 0.0679702;
    par_UU_ssigma3[7][0] = -5.67259; par_UU_ssigma3[7][1] = 5.20653; par_UU_ssigma3[7][2] = -0.512477; par_UU_ssigma3[7][3] = -2.69405; par_UU_ssigma3[7][4] = 3.98116; par_UU_ssigma3[7][5] = 0.668753; par_UU_ssigma3[7][6] = -0.0614403;
    par_UU_ssigma3[8][0] = 0.828025; par_UU_ssigma3[8][1] = -0.582192; par_UU_ssigma3[8][2] = -0.715179; par_UU_ssigma3[8][3] = 0.243922; par_UU_ssigma3[8][4] = 1.19651; par_UU_ssigma3[8][5] = -0.0292609; par_UU_ssigma3[8][6] = -9.16859e-05;
    par_UU_ssigma3[9][0] = 17.4816; par_UU_ssigma3[9][1] = -8.52461; par_UU_ssigma3[9][2] = 3.28675; par_UU_ssigma3[9][3] = 3.26555; par_UU_ssigma3[9][4] = -13.9313; par_UU_ssigma3[9][5] = -0.644356; par_UU_ssigma3[9][6] = 0.0498065;

 }