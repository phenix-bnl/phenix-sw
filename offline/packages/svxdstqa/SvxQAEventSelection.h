#ifndef __SVXQAEVENTSELECTION_H__
#define __SVXQAEVENTSELECTION_H__

class PHCompositeNode;
class RunHeader;
class BbcOut;
class EventHeader;
class VtxOut;
class PreviousEvent;

class SvxQAEventSelection
{

public:

    SvxQAEventSelection();
    virtual ~SvxQAEventSelection() {}

    void Set_Verbosity(int verb)
    {
    	m_verbosity = verb;
    }
    void Set_BBCZCut(float cut)
    {
        m_bbczcut = cut;
    };
    void Set_TickCut(bool is)
    {
        is_tickcut = is;
    }
    void Set_BbcQ10Percent(bool is)
    {
        is_bbcq10percent = is;
    }
    bool EventSelection(PHCompositeNode *topNode);


private:

    //
    // Nodes
    //
    bool GetNodes(PHCompositeNode *topNode);
    RunHeader       *d_runheader;
    BbcOut          *d_bbc;
    VtxOut          *d_vtxout;
    PreviousEvent   *d_peve;



    //
    // Variables
    //
    int m_verbosity; // >0 means print stuff out, 0 means remain silent
    float m_bbczcut;
    bool is_tickcut;
		bool is_bbcq10percent;

};


#endif //__SVXQAEVENTSELECTION_H__
