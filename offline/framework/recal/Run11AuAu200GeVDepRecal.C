#include "Run11AuAu200GeVDepRecal.h"

#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>

#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <getClass.h>
#include <RunHeader.h>

#include <PHGlobal.h>

#include <PHCompositeNode.h>
#include <recoConsts.h>

#include <PHString.h>
#include <TH2.h>

#include <iostream>

using namespace std;
using namespace findNode;

Run11AuAu200GeVDepRecal::Run11AuAu200GeVDepRecal(const string &name): Recalibrator(name)
{
    baseclasses.insert("PHCentralTrack");

    runNumber = 0;
    d_cnt = NULL;
    d_global = NULL;
    hdep_pt_e = NULL;
    hsdep_pt_e = NULL;
    hdep_pt_p = NULL;
    hsdep_pt_p = NULL;
}

int Run11AuAu200GeVDepRecal::Init(PHCompositeNode *topNode)
{

    if (fillhistos)
    {
        PHString nodename =  topNode->getName().getString();
        PHString histname = "Dep_e";
        histname += nodename;
        hdep_pt_e = new TH2F(histname.getString(), "dep vs pt for electrons", 120, 0, 12., 120, -6, 6);
        histname = "sDep_e";
        histname += nodename;
        hsdep_pt_e = new TH2F(histname.getString(), "dep vs pt for electrons (swapped)", 120, 0, 12., 120, -6, 6);
        histname = "Dep_p";
        histname += nodename;
        hdep_pt_p = new TH2F(histname.getString(), "dep vs pt for positrons", 120, 0, 12., 120, -6, 6);
        histname = "sDep_p";
        histname += nodename;
        hsdep_pt_p = new TH2F(histname.getString(), "dep vs pt for positrons (swapped)", 120, 0, 12., 120, -6, 6);

        Fun4AllServer *se = Fun4AllServer::instance();
        se->registerHisto(hdep_pt_e);
        se->registerHisto(hsdep_pt_e);
        se->registerHisto(hdep_pt_p);
        se->registerHisto(hsdep_pt_p);

    }


    return 0;
}

int Run11AuAu200GeVDepRecal::InitRun(PHCompositeNode *topNode)
{

    RunHeader *d_run = findNode::getClass<RunHeader>(topNode, "RunHeader");
    if (!d_run)
    {
        cout << PHWHERE << " RunHeader not found" << endl;
        return 0;
    }

    runNumber = d_run->get_RunNumber();

    return 0;
}

int Run11AuAu200GeVDepRecal::isValidRun(const int runno) const
{

    // Run11 AuAu200 GeV all runs
    if ( 343031 <= runno && runno <= 349680)
    {
        return 1;
    }

    return 0;
}

void Run11AuAu200GeVDepRecal::help()
{
    cout << "===================================================================" << endl;
    cout << "Run11AuAu200GeVDepRecal::help method output"                    << endl;
    cout << "Author:  Lei Ding (lding@iastate.edu). Copy from Run8PC3MatchRecal code" << endl;
    cout << "Comment: Run-11 AuAu 200GeV is applicable for runs 346973-run349680 (after 4th pixel repair)  "             << endl;
    cout << "         This method updates the dep value in PHCentralTrack   "              << endl;
    cout << "                  pT range is >=0.8GeV                               "                        << endl;
    cout << "             Interpolation method is used to decide " << endl;
    cout << "             the offset and sigma value as a function of pt             "  << endl;
    cout << "       The correction parameter from SvxCntQA ntuples from pro90."       << endl;
    cout << endl;
    cout << "===================================================================" << endl;
}


int Run11AuAu200GeVDepRecal::process_event(PHCompositeNode *topNode)
{

    d_cnt    = findNode::getClass<PHCentralTrack>(topNode, inputnodename.c_str());
    d_global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");

    if (!d_cnt || !d_global) return 0;

    Calibrate_Run11AuAu200GeV_dep();




    return EVENT_OK;
}

int Run11AuAu200GeVDepRecal::Calibrate_Run11AuAu200GeV_dep()
{
    for (unsigned int itrk = 0; itrk < d_cnt->get_npart(); itrk++)
    {

        PHSnglCentralTrack *sngltrk = d_cnt->get_track(itrk);
        sngltrk->ShutUp();
        if (
            sngltrk->isImplemented(sngltrk->get_the0()) &&
            sngltrk->isImplemented(sngltrk->get_mom()) &&
            sngltrk->isImplemented(sngltrk->get_charge()) &&
            sngltrk->isImplemented(sngltrk->get_dcarm()) &&
            sngltrk->isImplemented(sngltrk->get_sect()) &&
            sngltrk->isImplemented(sngltrk->get_ecore())
        )
        {
            if (verbosity > 0) cout << PHWHERE << " " << Name() << "Workable" << endl;
        }
        else
        {
            sngltrk->ShutUp(1);
            if (verbosity > 0) cout << PHWHERE << " " << Name() << "Not workable" << endl;
            continue;
        }

        sngltrk->ShutUp(1); // enable virtual warnings again

        if (! isfinite(sngltrk->get_the0()) || ! isfinite(sngltrk->get_mom()))
        {
            continue;
        }
        float the0 = sngltrk->get_the0();
        float mom  = sngltrk->get_mom();
        float pt   = mom;
        if (the0 > -999)
        {
            pt = mom * sin(the0);
        }

        int  charge = (int)sngltrk->get_charge();
        int dcarm = (int) sngltrk->get_dcarm();
        int emcsect = (int)sngltrk->get_sect();
        int sect = dcarm * 4 + emcsect;

        float ecore = float(sngltrk->get_ecore());
        float eop;
        if (mom > 0 && ecore > -1000) eop = ecore / mom;
        else
            eop = -9999.; // this is the variable to be recalibrated


        //Calculate the new variables
        // ------------------
        float dep = calculate_dep(sect, charge,  pt, eop);
        // ------------------

        //Set the new variables
        sngltrk->set_dep(dep);


        if (fillhistos)
        {
            if (pt >= .8)
            {
                if (sngltrk->get_n0() >= 4) //only if n0>=4, tight eID cuts
                {
                    if (charge > 0) hdep_pt_p->Fill(pt, dep);
                    else if (charge < 0) hdep_pt_e->Fill(pt, dep);
                }
                if (sngltrk->get_sn0() >= 4) //swapped sn0>=4
                {
                    if (charge > 0) hsdep_pt_p->Fill(pt, dep);
                    else if (charge < 0) hsdep_pt_e->Fill(pt, dep);
                }
            }
        }
    }

    return EVENT_OK;
}


float Run11AuAu200GeVDepRecal::calculate_dep(const int sector, const int charge, const float pt, const float ep)
{

    float ptcent[] = {0.85, 0.95, 1.1, 1.3, 1.5, 1.7, 1.9, 2.25, 2.75, 3.25, 3.75, 5};
    int nptbins = sizeof(ptcent) / sizeof(float);

    if (pt < ptcent[0] || ep < -1000 || sector >= 8 || sector < 0 || charge == 0)
    {
        return -9999.;  //only calibrate >0.8gev e
    }

    //E/p offset and sigma at the ptcent[i] for e- and e+ and 8 emcal sectors.
    float epmean[2][8][12] =
    {
        {   {0.903879, 0.914382, 0.921572, 0.93061, 0.937855, 0.943369, 0.951372, 0.95641, 0.965325, 0.968098, 0.971414, 0.979652},
            {0.89007, 0.902245, 0.909692, 0.917961, 0.927501, 0.936236, 0.940569, 0.948862, 0.959681, 0.969556, 0.971561, 0.979197},
            {0.910619, 0.915705, 0.918676, 0.922736, 0.92551, 0.92575, 0.927396, 0.927314, 0.929766, 0.931192, 0.930107, 0.927062},
            {0.922569, 0.929934, 0.934466, 0.938309, 0.940838, 0.94195, 0.941212, 0.940346, 0.941211, 0.941573, 0.94053, 0.938056},
            {0.90723, 0.911664, 0.913969, 0.917252, 0.918442, 0.918365, 0.916288, 0.914843, 0.913774, 0.913421, 0.908771, 0.905361},
            {0.875407, 0.886796, 0.893428, 0.89936, 0.904344, 0.908152, 0.905643, 0.903748, 0.906318, 0.901555, 0.895109, 0.885659},
            {0.905107, 0.916059, 0.920814, 0.925496, 0.92638, 0.925515, 0.925617, 0.925486, 0.920297, 0.917046, 0.912523, 0.906237},
            {0.89784, 0.903925, 0.906814, 0.907464, 0.9107, 0.910731, 0.909593, 0.910394, 0.907517, 0.902184, 0.898773, 0.902912}
        },
        {   {0.90657, 0.915408, 0.919676, 0.924188, 0.929476, 0.933888, 0.938078, 0.938085, 0.941408, 0.945582, 0.944643, 0.945661},
            {0.887132, 0.899148, 0.906686, 0.916958, 0.92582, 0.934716, 0.937833, 0.944159, 0.952468, 0.95798, 0.960321, 0.961126},
            {0.910082, 0.91628, 0.920319, 0.925055, 0.927668, 0.927881, 0.929324, 0.92911, 0.930648, 0.928921, 0.927929, 0.926516},
            {0.907692, 0.914147, 0.918507, 0.922818, 0.925826, 0.926835, 0.928313, 0.926725, 0.926906, 0.927055, 0.927369, 0.923146},
            {0.909896, 0.914652, 0.917361, 0.920531, 0.922587, 0.922302, 0.922166, 0.921184, 0.922677, 0.923708, 0.926072, 0.925052},
            {0.875697, 0.889921, 0.895599, 0.901867, 0.905448, 0.909996, 0.908346, 0.906653, 0.908814, 0.911566, 0.915138, 0.914435},
            {0.901254, 0.907, 0.910196, 0.913333, 0.913657, 0.91312, 0.911771, 0.909882, 0.909862, 0.912103, 0.913685, 0.913835},
            {0.924381, 0.929929, 0.932995, 0.933812, 0.935163, 0.93578, 0.933926, 0.932287, 0.929329, 0.923904, 0.929412, 0.922932}
        }
    };

    float epsig[2][8][12] =
    {
        {   {0.100052, 0.094926, 0.091655, 0.0887913, 0.0869635, 0.0800415, 0.0862437, 0.0762912, 0.0780599, 0.0759711, 0.0764569, 0.0828053},
            {0.103995, 0.0975044, 0.0951865, 0.0902981, 0.0917162, 0.0895272, 0.0817961, 0.0791763, 0.0756389, 0.0766179, 0.0780135, 0.0739094},
            {0.0906959, 0.0861754, 0.0829925, 0.0794086, 0.0775236, 0.0725924, 0.0762949, 0.0696585, 0.0678112, 0.0666728, 0.0662378, 0.0658601},
            {0.0954184, 0.0886218, 0.0855784, 0.0830458, 0.0819785, 0.0814319, 0.0826281, 0.0754746, 0.0719673, 0.0717543, 0.071344, 0.0829843},
            {0.0915659, 0.0874919, 0.0850748, 0.0830366, 0.0817762, 0.0815343, 0.0748651, 0.0733897, 0.071175, 0.0709513, 0.0717162, 0.0801707},
            {0.102491, 0.0978349, 0.0945401, 0.0914989, 0.0871343, 0.0905232, 0.0924139, 0.0910487, 0.0846978, 0.0873731, 0.0870116, 0.0994771},
            {0.0978264, 0.0958508, 0.0930484, 0.0886408, 0.0864581, 0.0843546, 0.0936243, 0.0916442, 0.0774817, 0.092031, 0.0888879, 0.0877451},
            {0.0895601, 0.0864106, 0.0845219, 0.0813385, 0.0787542, 0.0765178, 0.0804904, 0.0811846, 0.0711498, 0.0707061, 0.0697379, 0.0647602}
        },
        {   {0.0987946, 0.0923155, 0.0888422, 0.0848573, 0.0830743, 0.0832097, 0.0855692, 0.0784684, 0.0779101, 0.0782839, 0.0724472, 0.0746807},
            {0.102721, 0.0962288, 0.093017, 0.0892685, 0.0874209, 0.0856355, 0.078765, 0.0768681, 0.0760666, 0.0753499, 0.0782473, 0.0759728},
            {0.0906253, 0.0864901, 0.0834471, 0.0798414, 0.0778581, 0.0729435, 0.0751244, 0.0693561, 0.0675623, 0.0666765, 0.0667045, 0.0649816},
            {0.091938, 0.0878092, 0.0844307, 0.0803247, 0.0797764, 0.0770023, 0.0815232, 0.0753629, 0.0735653, 0.0733223, 0.0770547, 0.0781551},
            {0.0921846, 0.0883667, 0.0856836, 0.0822729, 0.0817867, 0.0818041, 0.0818856, 0.0758924, 0.0758811, 0.0769625, 0.0717757, 0.0707502},
            {0.103621, 0.0983866, 0.0963776, 0.0922576, 0.0900227, 0.0921167, 0.0956079, 0.0909035, 0.0881838, 0.0891886, 0.0862418, 0.0740863},
            {0.0933001, 0.090064, 0.0871666, 0.0842295, 0.0855112, 0.0889713, 0.0898002, 0.0851331, 0.082676, 0.0825065, 0.0798046, 0.0844755},
            {0.095482, 0.0889123, 0.0860478, 0.0827383, 0.0816284, 0.0829599, 0.0841979, 0.0774462, 0.0757996, 0.086566, 0.0765151, 0.0746619}
        }
    };


    //cout<<"total ptbins:"<<nptbins<<endl;
    int ichg;
    if (charge < 0) ichg = 0;
    else if (charge > 0) ichg = 1;

    int ipt;
    float mean = -9999.;
    float sig = -9999.;

    if (pt >= ptcent[nptbins - 1])
    {
        //if pt > the maxmium pt for the interpolation method, the mean and sigma at the maximium pt (5GeV in this code) will be used for dep calculation
        mean = epmean[ichg][sector][nptbins - 1];
        sig = epsig[ichg][sector][nptbins - 1];
    }
    else
    {
        for (ipt = 0; ipt < nptbins-1; ipt++)
        {
            if (pt >= ptcent[ipt] && pt < ptcent[ipt + 1])
            {
                break;
            }
        }
        //use interpolation method to calculate mean and sigma of E/p for any pt value
	if(ipt != nptbins-1)
	  {
	    mean = (epmean[ichg][sector][ipt + 1] - epmean[ichg][sector][ipt]) / (ptcent[ipt + 1] - ptcent[ipt]) * (pt - ptcent[ipt]) + epmean[ichg][sector][ipt];
	    sig = (epsig[ichg][sector][ipt + 1] - epsig[ichg][sector][ipt]) / (ptcent[ipt + 1] - ptcent[ipt]) * (pt - ptcent[ipt]) + epsig[ichg][sector][ipt];
	  }
    }
    return (ep - mean) / sig;

}



