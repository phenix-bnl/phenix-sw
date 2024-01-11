#include "SvxRawhitv5.h"

ClassImp(SvxRawhitv5)

unsigned int SvxRawhitv5::compareChan = 3;    //! varies from 0 to 5;

//----------------------------------------------------------------------------//
SvxRawhitv5::SvxRawhitv5(SvxRawhitList* lst, SvxRawhit* rawhit)
    : SvxRawhit(rawhit)
{
    rawhitList = lst;
    if ( rawhit )
    {
        sensorSection     = (short) rawhit->get_sensorSection();
        sensorReadout     = (short) rawhit->get_sensorReadout();
        sensorType        = (short) rawhit->get_sensorType();
        channel           =         rawhit->get_channel();
        adc               =         rawhit->get_adc();
        pixelModule       =         rawhit->get_pixelModule();
        pixelROC          =         rawhit->get_pixelROC();
        HotDeadFlag       =         rawhit->get_HotDeadFlag();
        isOkForClustering =         rawhit->get_isOkForClustering();
    }
    else
    {
        sensorSection     = -9999;
        sensorReadout     = -9999;
        sensorType        = -9999;
        channel           = -9999;
        adc               = -9999;
        pixelModule       = -9999;
        pixelROC          = -9999;
        HotDeadFlag       = 0;
        isOkForClustering = true;
    }
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
SvxRawhitv5::SvxRawhitv5(SvxRawhit* rawhit) : SvxRawhit(rawhit)
{
    rawhitList = 0;
    if ( rawhit )
    {
        sensorSection     = (short) rawhit->get_sensorSection();
        sensorReadout     = (short) rawhit->get_sensorReadout();
        sensorType        = (short) rawhit->get_sensorType();
        channel           =         rawhit->get_channel();
        adc               =         rawhit->get_adc();
        pixelModule       =         rawhit->get_pixelModule();
        pixelROC          =         rawhit->get_pixelROC();
        HotDeadFlag       =         rawhit->get_HotDeadFlag();
        isOkForClustering =         rawhit->get_isOkForClustering();
    }
    else
    {
        sensorSection     = -9999;
        sensorReadout     = -9999;
        sensorType        = -9999;
        channel           = -9999;
        adc               = -9999;
        pixelModule       = -9999;
        pixelROC          = -9999;
        HotDeadFlag       = 0;
        isOkForClustering = true;
    }
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
void SvxRawhitv5::Reset()
{
    SvxHit::Reset();
    sensorSection     = -9999;
    sensorReadout     = -9999;
    sensorType        = -9999;
    channel           = -9999;
    adc               = -9999;
    pixelModule       = -9999;
    pixelROC          = -9999;
    HotDeadFlag       = 0;
    isOkForClustering = true;
    if ( rawhitList )
        rawhitList->unSort();
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
void SvxRawhitv5::identify(std::ostream& os) const
{
    os << "SvxRawhitv5 object: hitID = "
       << hitID << std::endl;
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
Int_t SvxRawhitv5::Compare(const TObject* svxhit) const
{
    Int_t result = SvxHit::Compare(svxhit);
    if ( sorting_switch != 1 || result != 0 || compareChan == 0 )
        return result;

    const SvxRawhit* rhit = dynamic_cast<const SvxRawhit*>(svxhit);

    // D. McGlinchey - Keep this for backward compatability
    //                 But ... I don't really understand it
    if (compareChan <= 3)
    {
        // Check against the sensor section
        int diffp = sensorSection - rhit->get_sensorSection();
        if ( diffp < 0 )        return -1;
        if ( diffp > 0 )        return  1;
        if ( compareChan == 1 ) return  0;

        // Check against the sensor readout
        diffp = sensorReadout - rhit->get_sensorReadout();
        if ( diffp < 0 )        return -1;
        if ( diffp > 0 )        return  1;
        if ( compareChan == 2 ) return  0;

        // Check against the channel
        diffp = channel - rhit->get_channel();
        if ( diffp < 0 ) return -1;
        if ( diffp > 0 ) return  1;
        return 0;
    }

    // Check against the pixel ROC
    // D. McGlinchey - NOTE, for strip hits, pixelROC should be -9999.
    // And therefore the difference should always be 0. For strip hits then,
    // compareChan == 4 is the same as compareChan == 0
    int diffp = pixelROC - rhit->get_pixelROC();
    if ( diffp < 0 )        return -1;
    if ( diffp > 0 )        return  1;
    if ( compareChan == 4 ) return  0;

    // Check against the channel
    diffp = channel - rhit->get_channel();
    if ( diffp < 0 ) return -1;
    if ( diffp > 0 ) return  1;

    return 0;

}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
void SvxRawhitv5::Copy(SvxHit* hit)
{
    SvxHit::Copy(hit);
    SvxRawhit* rhit = dynamic_cast<SvxRawhit*>(hit);
    sensorSection     = (short) rhit->get_sensorSection();
    sensorReadout     = (short) rhit->get_sensorReadout();
    sensorType        = (short) rhit->get_sensorType();
    channel           =         rhit->get_channel();
    adc               =         rhit->get_adc();
    pixelModule       =         rhit->get_pixelModule();
    pixelROC          =         rhit->get_pixelROC();
    HotDeadFlag       =         rhit->get_HotDeadFlag();
    isOkForClustering =         rhit->get_isOkForClustering();
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
void SvxRawhitv5::print() const
{
    std::cout << "SvxRawhitv5 derived from ";
    SvxHit::print();
    std::cout << "  sensorSection     = " << sensorSection     << std::endl;
    std::cout << "  sensorReadout     = " << sensorReadout     << std::endl;
    std::cout << "  sensorType        = " << sensorType        << std::endl;
    std::cout << "  channel           = " << channel           << std::endl;
    std::cout << "  adc               = " << adc               << std::endl;
    std::cout << "  pixelModule       = " << pixelModule       << std::endl;
    std::cout << "  pixelROC          = " << pixelROC          << std::endl;
    std::cout << "  HotDeadFlag       = " << HotDeadFlag       << std::endl;
    std::cout << "  isOkForClustering = " << isOkForClustering << std::endl;
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
void SvxRawhitv5::set_hitID(const int val)
{
    hitID = val;
    if ( rawhitList )
        rawhitList->set_hitIDsorted(false);
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
void SvxRawhitv5::set_svxSection(const int val)
{
    svxSection = (short) val;
    if ( rawhitList )
    {
        rawhitList->set_sensorIDsorted(false);
        rawhitList->set_pixelROCIDsorted(false);
    }
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
void SvxRawhitv5::set_layer(const int val)
{
    layer = (short) val;
    if ( rawhitList )
    {
        rawhitList->set_sensorIDsorted(false);
        rawhitList->set_pixelROCIDsorted(false);
    }
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
void SvxRawhitv5::set_ladder(const int val)
{
    ladder = (short) val;
    if ( rawhitList )
    {
        rawhitList->set_sensorIDsorted(false);
        rawhitList->set_pixelROCIDsorted(false);
    }
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
void SvxRawhitv5::set_sensor(const int val)
{
    sensor = (short) val;
    if ( rawhitList )
    {
        rawhitList->set_sensorIDsorted(false);
        rawhitList->set_pixelROCIDsorted(false);
    }
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
void SvxRawhitv5::set_sensorSection(const int val)
{
    sensorSection = (short) val;
    if ( rawhitList )
    {
        rawhitList->set_sensorIDsorted(false);
        rawhitList->set_pixelROCIDsorted(false);
    }
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
void SvxRawhitv5::set_sensorReadout(const int val)
{
    sensorReadout = (short) val;
    if ( rawhitList )
    {
        rawhitList->set_sensorIDsorted(false);
        rawhitList->set_pixelROCIDsorted(false);
    }
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
void SvxRawhitv5::set_channel(const int val)
{
    channel = val;
    if ( rawhitList )
    {
        rawhitList->set_sensorIDsorted(false);
        rawhitList->set_pixelROCIDsorted(false);
    }
}
//----------------------------------------------------------------------------//


//----------------------------------------------------------------------------//
void SvxRawhitv5::set_pixelROC(const int val)
{
    pixelROC = (short) val;
    if ( rawhitList )
    {
        rawhitList->set_pixelROCIDsorted(false);
    }
}
//----------------------------------------------------------------------------//
