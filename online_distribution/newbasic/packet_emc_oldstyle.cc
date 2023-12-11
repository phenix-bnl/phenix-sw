#include <packet_emc_oldstyle.h>

#include <packet_emc_olddcm32.h>
#include <packet_emc_fpga.h>



Packet_emc_oldstyle::Packet_emc_oldstyle(PACKET_ptr data)
  : Packet_w4 (data)
{

  int real_hitformat  = getUnstructPacketHitFormat((PHDWORD *)data);

  if ( real_hitformat == IDEMC_FPGA || real_hitformat == IDEMC_FPGA0SUP) 
    {
      
      the_real_packet = new Packet_emc_fpga((PHDWORD *) data);
      is_a_fpga_packet = 1;
    }
  else
    {
      the_real_packet = new Packet_emc_olddcm32((PHDWORD *) data);
      is_a_fpga_packet = 0;
    }

}
  
Packet_emc_oldstyle::~Packet_emc_oldstyle()
{
  delete the_real_packet;
}

int   Packet_emc_oldstyle::iValue(const int ich)
{
  if (  is_a_fpga_packet )
    {
      int chip = ich/16*5;
      int channel = ich%16*5;
      if (channel >= 12*5) return 0;

      return ( 4095 - the_real_packet->iValue(chip*12*5 +channel));
    }
  else
    {
      return ( the_real_packet->iValue(ich));
    }
}


int   Packet_emc_oldstyle::iValue(const int ich, const int iy)
{
  if (  is_a_fpga_packet )
    {
      int chip = ich/16;
      int channel = ich%16;
      if (channel >= 12) return 0;

      return ( 4095 - the_real_packet->iValue(chip*12+channel, iy));
    }
  else
    {
      return ( the_real_packet->iValue(ich, iy));
    }
}


int   Packet_emc_oldstyle::iValue(const int ich, const char * what)
{
  return ( the_real_packet->iValue(ich, what));
}

int   Packet_emc_oldstyle::fillIntArray ( int array[], const int len, int *nw ,const char * what)
{
  return ( the_real_packet->fillIntArray (array, len, nw ,what));
}


// ------------------------------------------------------

void Packet_emc_oldstyle::dump ( std::ostream &os) 
{
  
  os << "EMC oldstyle packet handler " << std::endl;
  the_real_packet->dump(os);
  
}







  
