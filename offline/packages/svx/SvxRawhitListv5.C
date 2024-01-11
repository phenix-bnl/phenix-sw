#include <SvxRawhitListv5.h>

#include <algorithm>
#include <cstdlib>
#include <vector>

ClassImp(SvxRawhitListv5)

//----------------------------------------------------------------------------//
SvxRawhitListv5::SvxRawhitListv5(const unsigned int length)
{
    m_hit_list = new TClonesArray("SvxRawhitv5", length);
    m_hit_id_unused = 0;
    unSort();
    listPointersSet = false;
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
SvxRawhitListv5::~SvxRawhitListv5()
{
    delete m_hit_list;
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
void SvxRawhitListv5::Reset()
{
    restoreListPointers();
 
    m_hit_id_unused = 0;
    m_hit_list->Clear();
    if ( m_hit_list->GetSize() > SVXNRAWHIT ) m_hit_list->Expand(SVXNRAWHIT);
    unSort();
    listPointersSet = false;
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
int SvxRawhitListv5::isValid() const
{
    return ( ( get_nRawhits() > 0 ) ? 1 : 0 ) ;
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
void SvxRawhitListv5::identify(std::ostream &os) const
{
    os << "Identify yourself: SvxRawhitListv5 object: nRawhits = "
       << get_nRawhits() << std::endl;
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
SvxRawhit* SvxRawhitListv5::addRawhit(const int ihit)
{
    int index = ( ihit < 0 ) ? get_nRawhits() : ihit;
    TClonesArray   &P = *m_hit_list;
    SvxRawhit* rawhit = new(P[index]) SvxRawhitv5(this);
    rawhit->set_hitID(m_hit_id_unused++);
    if ( get_nRawhits() == 0 ) listPointersSet = true;
    unSort();
    return rawhit;
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
void SvxRawhitListv5::removeRawhit(const unsigned int ihit)
{
    m_hit_list->RemoveAt(ihit);
    unSort();
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
int SvxRawhitListv5::Compress()
{
    m_hit_list->Compress();
    return (int) get_nRawhits();
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
int SvxRawhitListv5::set_TClonesArraySize(const unsigned int nhit)
{
    if ( nhit > (unsigned int)get_nRawhits() ) m_hit_list->Expand(nhit);
    return m_hit_list->GetSize();
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
void SvxRawhitListv5::restoreListPointers()
{
    SvxRawhitv5* rawhit = NULL;
    for (int i = 0; i < get_nRawhits(); i++ )
    {
        if ( (rawhit = (SvxRawhitv5*) m_hit_list->UncheckedAt(i)) )
            rawhit->set_ListPointer(this);
    }
    listPointersSet = true;
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
void SvxRawhitListv5::sort_hitID()
{
    if ( m_hitIDsorted )
        return;
    if ( !listPointersSet )
        restoreListPointers();

    SvxRawhitv5 hit;
    hit.set_sortingSwitch(-1);
    SortSTL();
    m_hitIDsorted      = true;
    m_sensorIDsorted   = false;
    m_pixelROCIDsorted = false;
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
void SvxRawhitListv5::sort_sensorID () {
    if ( m_sensorIDsorted )
        return;
    if ( !listPointersSet )
        restoreListPointers();

    SvxRawhitv5 hit;
    hit.set_sortingSwitch(1);
    hit.set_compareChan(3);
    SortSTL();
    m_sensorIDsorted   = true;
    m_hitIDsorted      = false;
    m_pixelROCIDsorted = false;
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
void SvxRawhitListv5::sort_pixelROCID () {
    if ( m_pixelROCIDsorted )
        return;
    if ( !listPointersSet )
        restoreListPointers();

    // NOTE that SvxHit::set_sortingSwitch() and SvxRawhit::set_compareChan()
    //      are setting static global variables in the namespace and that's
    //      why creating a hit here and setting the values effects the
    //      SortSTL() function below.
    SvxRawhitv5 hit;
    hit.set_sortingSwitch(1);
    hit.set_compareChan(5);
    SortSTL();
    m_pixelROCIDsorted = true;
    m_hitIDsorted      = false;
    m_sensorIDsorted   = false;
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
void SvxRawhitListv5::unSort()
{
    m_hitIDsorted      = false;
    m_sensorIDsorted   = false;
    m_pixelROCIDsorted = false;
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
void SvxRawhitListv5::SortSTL()
{
    int n_ent = get_nRawhits();

    // make and sort a temporary array
    std::vector<SvxHit*> hlist(n_ent);
    for (int i = 0; i < n_ent; i++)
        hlist[i] = get_Rawhit(i)->Clone();

    std::sort(hlist.begin(), hlist.end(), SvxHit::CompPointer);

    for (int i = 0; i < n_ent; i++)
    {
        get_Rawhit(i)->Copy(hlist[i]);
        delete hlist[i];
    }
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
int SvxRawhitListv5::indexOfRawhit(const int hitid,
                                   int ifrom,
                                   int iupto) const
{
    if (get_nRawhits() == 0) return -1;

    if (iupto < 0)
        iupto = get_nRawhits() - 1;

    if (ifrom < 0 || iupto >= get_nRawhits() || ifrom > iupto)
    {
        std::cerr << "SvxHitListv2::FindHitIndex(): Invalid search range, from "
                  << ifrom << " upto " << iupto << ", where the hit list has "
                  << get_nRawhits() << " entries." << std::endl;
        exit(1);
    }

    if ( iupto - ifrom > 8 && m_hitIDsorted )
    {
        int i_low = ifrom - 1;
        int i_up  = iupto + 1;
        while (i_up - i_low > 1)
        {
            int i_cent     = (i_low + i_up) / 2;
            int hitid_curr = get_Rawhit(i_cent)->get_hitID();

            if      (hitid_curr == hitid)
                return i_cent;
            else if (hitid_curr <  hitid)
                i_low = i_cent;
            else
                i_up = i_cent;
        }

        return -1;

    }
    else
    {
        SvxRawhit* phit = NULL;
        for ( int i = ifrom; i <= iupto; i++ )
        {
            if ( (phit = get_Rawhit(i)) )
            {
                if ( phit->get_hitID() == hitid )
                    return i;
            }
        }
        return -1;

    } // if (iupto-ifrom > 8 && hitIDsorted)
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
int SvxRawhitListv5::indexOfRawhit (SvxRawhit& rawhit,
                                    const int npar,
                                    const int ifrom,
                                    const int iupto) const
{

    int indto = ( iupto < 0 ) ? get_nRawhits() : iupto;

    SvxRawhitv5* hit = (SvxRawhitv5*) &rawhit;
    hit->set_sortingSwitch(1);
    hit->set_compareChan(npar);

    if ( indto - ifrom > 8 && m_sensorIDsorted )
    {
        int ind = m_hit_list->BinarySearch(hit, indto);
        if ( (ind == ifrom ) || ind < 0 )
            return ind;
        SvxRawhit* phit = NULL;

        if ( ind < ifrom )
        {
            for ( int i = ifrom; i < indto; i++ )
            {
                if ( (phit = get_Rawhit(i)) )
                {
                    if ( hit->Compare(phit) )
                    {
                        return -1;
                    }
                    else
                    {
                        if ( npar > 2 )
                        {
                            std::cout << "SvxRawhitListv5::indexOfRawhit--WARNING: "
                                      << "More than one SvxRawhits for the same channel"
                                      << " found in the list" << std::endl;
                        }
                        return i;
                    }
                }
            }
            return -1;
        }
        else
        {
            int ilast = ind;
            for ( int i = ind - 1; i >= ifrom; i-- )
            {
                if ( (phit = get_Rawhit(i)) )
                {
                    if ( hit->Compare(phit) )
                    {
                        return ilast;
                    }
                    else
                    {
                        ilast = i;
                        if ( npar > 2 )
                        {
                            std::cout << "SvxRawhitListv5::indexOfRawhit--WARNING: "
                                      << "More than one SvxRawhits for the same channel"
                                      << " found in the list" << std::endl;
                        }
                    }
                }
            }
            return ilast;
        }
    }
    else
    {
        SvxRawhit* phit = NULL;
        for ( int i = ifrom; i < indto; i++ )
        {
            if ( (phit = get_Rawhit(i)) )
            {
                if ( !hit->Compare(phit) )
                    return i;
            }
        }
        return -1;

    } // if (indto-ifrom > 8 && hitIDsorted)
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
int SvxRawhitListv5::indexOfRawhit (const SvxRawhit* hit,
                                    const int ifrom,
                                    const int iupto) const
{
    int indto = ( iupto < 0 ) ? get_nRawhits() : iupto;

    for ( int i = ifrom; i < indto; i++ )
    {
        if ( get_Rawhit(i) == hit )
            return i;
    }
    return -1;
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
bool SvxRawhitListv5::indexOfRawhit(const SvxSensor* sensor,
                                    int& idx_lb, int& idx_ub,
                                    int ifrom,
                                    int iupto) const
{

    if (! m_sensorIDsorted)
    {
        std::cerr << "SvxRawhitListv5::indexOfRawhit(): "
                  << "The hit list hasn't been sorted by sensorID."
                  << std::endl;
        exit(1);
    }

    idx_lb = -1;
    idx_ub = -2;
    if (get_nRawhits() == 0)
        return false;
    if (iupto < 0)
        iupto = get_nRawhits() - 1;
    if (ifrom < 0 || iupto >= get_nRawhits() || ifrom > iupto)
    {
        std::cerr << "SvxRawhitListv5::indexOfRawhit(): Invalid search range, from "
                  << ifrom << " upto " << iupto << ", where the hit list has "
                  << get_nRawhits() << " entries." << std::endl;
        exit(1);
    }

    SvxRawhitv5* hit_cmp = new SvxRawhitv5();
    hit_cmp->set_svxSection(sensor->get_svxSection());
    hit_cmp->set_layer(sensor->get_layer());
    hit_cmp->set_ladder(sensor->get_ladder());
    hit_cmp->set_sensor(sensor->get_sensor());
    hit_cmp->set_sortingSwitch(1); // sort by sensor ID
    hit_cmp->set_compareChan(0);

    int i_low = ifrom - 1;
    int i_up  = iupto + 1;
    int i_mid = -1; // index of a first matched entry
    while (i_up - i_low > 1)
    {
        int i_cent     = (i_low + i_up) / 2;
        int cmp = hit_cmp->Compare(get_Rawhit(i_cent));
        if (cmp == 0)
        {
            i_mid = i_cent;
            break;
        }
        else if (cmp >  0)
            i_low = i_cent;
        else
            i_up  = i_cent;
    }
    if (i_mid < 0)
    {
        delete hit_cmp;
        return false;
    }

    idx_lb = idx_ub = i_mid;
    for (; idx_lb > i_low + 1 && hit_cmp->Compare(get_Rawhit(idx_lb - 1)) == 0; idx_lb--);
    for (; idx_ub < i_up  - 1 && hit_cmp->Compare(get_Rawhit(idx_ub + 1)) == 0; idx_ub++);

    delete hit_cmp;
    return true;
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
bool SvxRawhitListv5::indexRangeOfRawhitsPixelROC(SvxRawhit* rawhit,
        int& idx_lb, int& idx_ub,
        int ifrom,
        int iupto) const
{

    if (! m_pixelROCIDsorted)
    {
        std::cerr << "SvxRawhitListv5::indexOfRawhit(): "
                  << "The hit list hasn't been sorted by pixelROC."
                  << std::endl;
        exit(1);
    }

    idx_lb = -1;
    idx_ub = -2;
    if (get_nRawhits() == 0)
        return false;
    if (iupto < 0)
        iupto = get_nRawhits() - 1;
    if (ifrom < 0 || iupto >= get_nRawhits() || ifrom > iupto)
    {
        std::cerr << "SvxRawhitListv5::indexOfRawhit(): Invalid search range, from "
                  << ifrom << " upto " << iupto << ", where the hit list has "
                  << get_nRawhits() << " entries." << std::endl;
        exit(1);
    }

    // sort by pixelROC ID
    SvxRawhitv5* hit_cmp = new SvxRawhitv5(rawhit);
    hit_cmp->set_sortingSwitch(1);
    hit_cmp->set_compareChan(4);

    int i_low = ifrom - 1;
    int i_up  = iupto + 1;
    int i_mid = -1; // index of a first matched entry
    while (i_up - i_low > 1)
    {
        int i_cent     = (i_low + i_up) / 2;
        int cmp = hit_cmp->Compare(get_Rawhit(i_cent));
        if (cmp == 0)
        {
            i_mid = i_cent;
            break;
        }
        else if (cmp >  0)
            i_low = i_cent;
        else
            i_up  = i_cent;
    }
    if (i_mid < 0)
    {
        delete hit_cmp;
        return false;
    }

    idx_lb = idx_ub = i_mid;
    for (; idx_lb > i_low + 1 && hit_cmp->Compare(get_Rawhit(idx_lb - 1)) == 0; idx_lb--);
    for (; idx_ub < i_up  - 1 && hit_cmp->Compare(get_Rawhit(idx_ub + 1)) == 0; idx_ub++);

    delete hit_cmp;
    return true;
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
void SvxRawhitListv5::print() const
{
    std::cout << "SvxRawhitListv5: nRawhits = " << get_nRawhits()
              << " hitIDsorted = "      << m_hitIDsorted
              << " sensorIDsorted = "   << m_sensorIDsorted
              << " pixelROCIDsorted = " << m_pixelROCIDsorted << std::endl;
    SvxRawhit* rawhit = NULL;
    for (int i = 0; i < get_nRawhits(); i++)
    {
        std::cout << "SvxRawhit number " << i << ":";
        if ( (rawhit = get_Rawhit(i)) )
        {
            std::cout << std::endl;
            rawhit->print();
        }
        else
        {
            std::cout << " missing ..." << std::endl;
        }
    }
}
//----------------------------------------------------------------------------//
