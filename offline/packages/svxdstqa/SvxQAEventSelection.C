#include <SvxQAEventSelection.h>

#include <PHCompositeNode.h>

#include <getClass.h>
#include <TrigLvl1.h>
#include <TriggerHelper.h>

#include <EventHeader.h>
#include <RunHeader.h>
#include <PreviousEvent.h>
#include <VtxOut.h>
#include <PHPoint.h>
#include <BbcOut.h>
#include <Bbc.hh>

#include <iostream>
#include <cmath>

SvxQAEventSelection::SvxQAEventSelection() :
    d_runheader(NULL),
    d_bbc(NULL),
    d_vtxout(NULL),
    d_peve(NULL),
    m_verbosity(0),
    m_bbczcut(10)
{

    is_tickcut = true;
    is_bbcq10percent = false;

}

bool SvxQAEventSelection::GetNodes(PHCompositeNode *topNode)
{

    //-------------------------RunHeader-------------------------------
    d_runheader = NULL;
    d_runheader = findNode::getClass<RunHeader>(topNode, "RunHeader");
    if (d_runheader == NULL)
    {
        if (m_verbosity > 0)
        {
            std::cout << "SvxQAEventSelection::GetNodes -"
                      << "Can't find RunHeader. " << std::endl;
        }
        return false;
    }
    //------------------------------------------------------------------

    //-------------------BbcOut----------------------------------------
    d_bbc = NULL;
    d_bbc = findNode::getClass<BbcOut>(topNode, "BbcOut");
    if (d_bbc == NULL)
    {
        if (m_verbosity > 0)
        {
            std::cout << "SvxQAEventSelection::GetNodes -"
                      << "Can't find BbcOut." << std::endl;
        }
        return false;
    }
    //------------------------------------------------------------------

    //------------------PreviousEvent--------------------------------
    d_peve = NULL;
    d_peve    = findNode::getClass<PreviousEvent>(topNode, "PreviousEvent"); // for Tick cut
    if (d_peve == NULL)
    {
        if (m_verbosity > 0)
        {
            std::cout << "SvxQAEventSelection::GetNodes -"
                      << " No PreviousEvent object !" << std::endl;
        }
        return false;
    }
    //-------------------------------------------------------------------

    //------------------Vertex------------------------------
    d_vtxout = NULL;
    d_vtxout = findNode::getClass<VtxOut>(topNode, "VtxOut");
    if (d_vtxout == NULL)
    {
        if (m_verbosity > 0)
        {
            std::cout << "SvxQAEventSelection::GetNodes -"
                      << " VtxOut node not found." << std::endl;
        }
        return false;
    }
    //-----------------------------------------------------


    return true;
}


bool SvxQAEventSelection::EventSelection(PHCompositeNode *topNode)
{
  //static bool isFirst = true;

    if (!GetNodes(topNode))
    {
        if (m_verbosity > 0)
        {
            std::cout << "SvxQAEventSelection::EventSelection - "
                      << "Did not find all required nodes. Not a good event!" << std::endl;
        }
        return false;
    }

    //First get the run number
    //int runnumber = d_runheader->get_RunNumber();



    //pixel tick cut
    // Cuts out events that are too close together in time.
    if (is_tickcut)
    {
        int pticks[3] = {0};
        for ( int i = 0; i < 3; i++ ) pticks[i] = (d_peve != NULL) ? d_peve->get_clockticks(i) : -999;

        bool tick =  ( ( 50 < pticks[0] && pticks[0] < 120) ||
                       (700 < pticks[1] && pticks[1] < 780) );


        if (tick)
        {
            if (m_verbosity > 0)
            {
                std::cout << "SvxQAEventSelection::EventSelection - "
                          << "Failed tick cut. Not a good event!" << std::endl;
                std::cout << "                                      pticks: "
                          << pticks[0] << " " << pticks[1] << " " << pticks[2] << std::endl;

            }
            return false;
        }
    }




    //bbcz vertex cut
    float zbbc = (d_vtxout->get_Vertex("BBC")).getZ();
    if (fabs(zbbc) > m_bbczcut)
    {
        if (m_verbosity > 0)
        {
            std::cout << "SvxQAEventSelection::EventSelection - Failed BBCZ cut "
                      << "(|" << zbbc << "| > " << m_bbczcut << ")."
                      << " Not a good event!" << std::endl;
        }
        return false;
    }


		// Multiplicity cut for jumb pixel problem
		// --Select events in the top 10% bbc multiplicity
		if (is_bbcq10percent)
		{
			float bbc_q = d_bbc->get_ChargeSum(Bbc::North) + d_bbc->get_ChargeSum(Bbc::South);
			if (bbc_q < 1200)
			{
 	      if (m_verbosity > 0)
 	      {
          std::cout << "SvxQAEventSelection::EventSelection - Failed BBC charge cut "
                      <<  bbc_q << " < "  << "1200."
                      << " Not within 10%!" << std::endl;
        }
        return false;
    	}
		}


    //Edit as of 15-05-2015 by Theok
    //We no longer need the trigger selection here because it is done at the macro level

    /*
    //trigger selection
    TriggerHelper trghelp(topNode);

    bool isGoodTrigger = true;
    if (343031 <= runnumber && runnumber <= 349680) // run11 AuAu 200 run 343801-349680
    {
        if (isFirst)
            std::cout << "SvxQAEventSelection::EventSelection. Run11 AuAu 200GeV" << std::endl;

        bool isL1           = trghelp.didLevel1TriggerGetScaled( "BBCLL1(>1 tubes)" ); //BBCL1
        bool isnarrow       = trghelp.didLevel1TriggerGetScaled( "BBCLL1(>1 tubes) narrowvtx" ); //BBCL1 narrow
        bool isnarrow_copyA = trghelp.didLevel1TriggerGetScaled( "BBCLL1(>1 tubes) narrowvtx Copy A" ); // BBCL1 narrow A
        bool isnarrow_copyB = trghelp.didLevel1TriggerGetScaled( "BBCLL1(>1 tubes) narrowvtx Copy B" ); // BBCL1 narrow B
        bool isnovtx        = trghelp.didLevel1TriggerGetScaled( "BBCLL1(>1 tubes) novertex" ); // BBCL1 novertex

        isGoodTrigger = ( isL1 || isnarrow || isnarrow_copyA || isnarrow_copyB || isnovtx);
    }
    else if ((358513 <= runnumber && runnumber <= 363228) || // run12 pp 200
             (364580 <= runnumber && runnumber <= 368800) || // run12 pp 510
             (386773 <= runnumber && runnumber <= 398149)) // run13 pp 510
    {
        if (isFirst)
        {
            std::cout << "SvxQAEventSelection::EventSelection " << std::flush;
            if (358513 <= runnumber && runnumber <= 363228) std::cout << "Run12 pp 200GeV" << std::flush;
            if (364580 <= runnumber && runnumber <= 368800) std::cout << "Run12 pp 510GeV" << std::flush;
            if (386773 <= runnumber && runnumber <= 398149) std::cout << "Run13 pp 510GeV" << std::flush;
            std::cout << std::endl;
        }

        bool isL1           = trghelp.didLevel1TriggerGetScaled("BBCLL1(>0 tubes)" );
        bool isnarrow       = trghelp.didLevel1TriggerGetScaled("BBCLL1(>0 tubes) narrowvtx" );
        bool isnovtx        = trghelp.didLevel1TriggerGetScaled("BBCLL1(>0 tubes) novertex" );
        bool isnovtx_zdc    = trghelp.didLevel1TriggerGetScaled("BBCLL1(noVtx)&(ZDCN||ZDCS)" );

        isGoodTrigger =  ( isL1 || isnarrow || isnovtx || isnovtx_zdc );
    }
    else if (369327 <= runnumber && runnumber <= 372006) // run12 UU
    {
        if (isFirst)
            std::cout << "SvxQAEventSelection::EventSelection. Run12 UU 197" << std::endl;

        bool isnarrow1      = trghelp.didLevel1TriggerGetScaled("BBBCLL1(>1 tubes) narrowvtx");
        bool isL1           = trghelp.didLevel1TriggerGetScaled("BBCLL1(>1 tubes)");
        bool isnarrow       = trghelp.didLevel1TriggerGetScaled("BBCLL1(>1 tubes) narrowvtx");
        bool isnovtx        = trghelp.didLevel1TriggerGetScaled("BBCLL1(>1 tubes) novertex");

        isGoodTrigger =  ( isnarrow1 || isL1 || isnarrow || isnovtx );
    }
    else if (372402 <= runnumber && runnumber <= 377310) // run 12 Cu+Au
    {
        if (isFirst)
            std::cout << "SvxQAEventSelection::EventSelection. Run12 CuAu 200" << std::endl;

        bool isL1           = trghelp.didLevel1TriggerGetScaled("BBCLL1(>1 tubes)");
        bool isnarrow       = trghelp.didLevel1TriggerGetScaled("BBCLL1(>1 tubes) narrowvtx");
        bool isnarrow_copyA = trghelp.didLevel1TriggerGetScaled("BBCLL1(>1 tubes) narrowvtx Copy A");
        bool isnarrow_copyB = trghelp.didLevel1TriggerGetScaled("BBCLL1(>1 tubes) narrowvtx Copy B");
        bool isnovtx        = trghelp.didLevel1TriggerGetScaled("BBCLL1(>1 tubes) novertex");
        bool iszdcn         = trghelp.didLevel1TriggerGetScaled("ZDCLL1narrow");
        bool iszdcw         = trghelp.didLevel1TriggerGetScaled("ZDCLL1wide");
        bool iszdc          = trghelp.didLevel1TriggerGetScaled("ZDCNS");

        isGoodTrigger =  ( isL1 || isnarrow || isnarrow_copyA || isnarrow_copyB || isnovtx || iszdcn || iszdcw || iszdc );
    }
    else if (runnumber >= 401000 && runnumber <= 405182) // run 14 AuAu 15 GeV
    {
        if (isFirst)
            std::cout << "SvxQAEventSelection::EventSelection. Run14 AuAu 15" << std::endl;

        bool isnarrow       = trghelp.didLevel1TriggerGetScaled("BBCLL1(>1 tubes) narrowvtx");

        //for low E AuAu require > 5 tubes in each bbc to get rid of junk data
        bool bbcn           = d_bbc->get_nPmt(Bbc::North) >= 3;
        bool bbcs           = d_bbc->get_nPmt(Bbc::South) >= 3;

        isGoodTrigger = (isnarrow) && bbcn && bbcs;
    }
    else if (runnumber >= 405327 && runnumber <= 414988) // run 14 AuAu 200 GeV
    {
        if (isFirst)
            std::cout << "SvxQAEventSelection::EventSelection. Run14 AuAu 200GeV" << std::endl;

        bool isL1           = trghelp.didLevel1TriggerGetScaled( "BBCLL1(>1 tubes)" ); //BBCL1
        bool isnarrow       = trghelp.didLevel1TriggerGetScaled( "BBCLL1(>1 tubes) narrowvtx" ); //BBCL1 narrow
        bool isnarrow_copyA = trghelp.didLevel1TriggerGetScaled( "BBCLL1(>1 tubes) narrowvtx CopyA" ); // BBCL1 narrow A
        bool isnarrow_copyB = trghelp.didLevel1TriggerGetScaled( "BBCLL1(>1 tubes) narrowvtx Copy B" ); // BBCL1 narrow B
        bool isnovtx        = trghelp.didLevel1TriggerGetScaled( "BBCLL1(>1 tubes) novertex" ); // BBCL1 novertex

        isGoodTrigger = ( isL1 || isnarrow || isnarrow_copyA || isnarrow_copyB || isnovtx);

    }
    else if (runnumber >= 415390 && runnumber <= 416894) // run 14 He3Au 200 GeV
    {
        if (isFirst)
            std::cout << "SvxQAEventSelection::EventSelection. Run14 He3Au 200GeV" << std::endl;

        bool isL1           = trghelp.didLevel1TriggerGetScaled( "BBCLL1(>0 tubes)" ); //BBCL1
        bool isnarrow       = trghelp.didLevel1TriggerGetScaled( "BBCLL1(>0 tubes) narrowvtx" ); //BBCL1 narrow
        bool isnarrow_copyA = trghelp.didLevel1TriggerGetScaled( "BBCLL1(>0 tubes) narrowvtx CopyA" ); // BBCL1 narrow A
        bool isnarrow_copyB = trghelp.didLevel1TriggerGetScaled( "BBCLL1(>0 tubes) narrowvtx Copy B" ); // BBCL1 narrow B
        bool isnovtx        = trghelp.didLevel1TriggerGetScaled( "BBCLL1(>0 tubes) novertex" ); // BBCL1 novertex

        isGoodTrigger = ( isL1 || isnarrow || isnarrow_copyA || isnarrow_copyB || isnovtx );
    }
    else if(runnumber >= 421716 && runnumber <= 432008)// Run15 pp 200 GeV
      {
	if (isFirst)
	  std::cout << "SvxQAEventSelection::EventSelection. Run15 pp 200GeV" << std::endl;

        bool isL1           = trghelp.didLevel1TriggerGetScaled( "BBCLL1(>0 tubes)" ); //BBCL1
        bool isnarrow       = trghelp.didLevel1TriggerGetScaled( "BBCLL1(>0 tubes) narrowvtx" ); //BBCL1 narrow
        bool isnovtx        = trghelp.didLevel1TriggerGetScaled( "BBCLL1(>0 tubes) novertex" ); // BBCL1 novertex
        bool iszdcwide      = trghelp.didLevel1TriggerGetScaled( "ZDCLL1wide" ); // ZDCLL1 Wide
	bool iszdcns        = trghelp.didLevel1TriggerGetScaled( "ZDCNS" ); // ZDCLL1 NS
	bool isL1andzdc     = trghelp.didLevel1TriggerGetScaled( "BBCLL1(noVtx)&(ZDCN||ZDCS)" ); // BBC LL1(no vtx) & (ZDCN||ZDCS)

        isGoodTrigger = ( isL1 || isnarrow || isnovtx || iszdcwide || iszdcns || isL1andzdc);
      }
    else if(runnumber > 432008)// Run15 pAu 200 GeV
      {
	if (isFirst)
	  std::cout << "SvxQAEventSelection::EventSelection. Run15 pAu 200GeV" << std::endl;

        bool isL1           = trghelp.didLevel1TriggerGetScaled( "BBCLL1(>0 tubes)" ); //BBCL1
        bool isnarrow       = trghelp.didLevel1TriggerGetScaled( "BBCLL1(>0 tubes) narrowvtx" ); //BBCL1 narrow
        bool isnovtx        = trghelp.didLevel1TriggerGetScaled( "BBCLL1(>0 tubes) novertex" ); // BBCL1 novertex
        bool iszdcwide      = trghelp.didLevel1TriggerGetScaled( "ZDCLL1wide" ); // ZDCLL1 Wide
	bool iszdcns        = trghelp.didLevel1TriggerGetScaled( "ZDCNS" ); // ZDCLL1 NS
	bool iscent35       = trghelp.didLevel1TriggerGetScaled( "BBCLL1(>0 tubes)_central35_narrowvtx" ); // BBC LL1 Central 35 Tubes
	

        isGoodTrigger = ( isL1 || isnarrow || isnovtx || iszdcwide || iszdcns || iscent35);

      }
    else
    {
        if (isFirst)
            std::cout << "SvxQAEventSelection::EventSelection -"
                      << " No trigger selection for runnumber "
                      << runnumber << " : take all the events" << std::endl;
        isGoodTrigger = true;
    }

    isFirst = false;

    if ( isGoodTrigger )
    {
        //OK. This is triggered event that we want
        return true;
    }
    else
    {
        return false;
    }
    */
    return true;
}
