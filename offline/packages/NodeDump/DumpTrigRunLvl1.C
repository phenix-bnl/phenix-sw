#include <DumpTrigRunLvl1.h>

#include <TrigRunLvl1.h>

#include <PHIODataNode.h>

#include <string>

using namespace std;

typedef PHIODataNode<TrigRunLvl1> MyNode_t;

DumpTrigRunLvl1::DumpTrigRunLvl1(const string &NodeName): DumpObject(NodeName)
{
  write_run_event = 0; // do not write info for each event
  node_written = 0;  // write runwise nodes only once
  return ;
}

int DumpTrigRunLvl1::process_Node(PHNode *myNode)
{
  TrigRunLvl1 *trigrunlvl1 = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      trigrunlvl1 = thisNode->getData();
    }
  if (!node_written && trigrunlvl1 && trigrunlvl1->isValid())
    {
      trigrunlvl1->ShutUp();
      *fout << "get_lvl1_trigger_description(): " << trigrunlvl1->get_lvl1_trigger_description() << endl;
      *fout << "get_lvl1_trigger_version(): " << trigrunlvl1->get_lvl1_trigger_version() << endl;
      *fout << "get_lvl1_bbcll1_description(): " << trigrunlvl1->get_lvl1_bbcll1_description() << endl;
      *fout << "get_lvl1_bbcll1_version(): " << trigrunlvl1->get_lvl1_bbcll1_version() << endl;
      *fout << "get_lvl1_partition_name(): " << trigrunlvl1->get_lvl1_partition_name() << endl;
      int i;
      for (i = 0;i < 32;i++)
        {
          *fout << "get_lvl1_trig_name(" << i << "): " << trigrunlvl1->get_lvl1_trig_name(i) << endl;
          if (trigrunlvl1->get_lvl1_trig_name_bybit(i))
            {
              *fout << "get_lvl1_trig_name_bybit(" << i << "): " << trigrunlvl1->get_lvl1_trig_name_bybit(i) << endl;
            }
          *fout << "get_lvl1_trig_bitmask(" << i << "): " << trigrunlvl1->get_lvl1_trig_bitmask(i) << endl;
          *fout << "get_lvl1_trigger_enable(" << i << "): " << trigrunlvl1->get_lvl1_trigger_enable(i) << endl;
          *fout << "get_lvl1_trigger_enable_bybit(" << i << "): " << trigrunlvl1->get_lvl1_trigger_enable_bybit(i) << endl;
          *fout << "get_lvl1_trig_bit(" << i << "): " << trigrunlvl1->get_lvl1_trig_bit(i) << endl;
          *fout << "get_lvl1_trig_index(" << i << "): " << trigrunlvl1->get_lvl1_trig_index(i) << endl;
          *fout << "get_lvl1_trig_scale_down(" << i << "): " << trigrunlvl1->get_lvl1_trig_scale_down(i) << endl;
          *fout << "get_lvl1_lvl2_reject_enable(" << i << "): " << trigrunlvl1->get_lvl1_lvl2_reject_enable(i) << endl;
          *fout << "get_lvl1_lvl2_reject_enable_bybit(" << i << "): " << trigrunlvl1->get_lvl1_lvl2_reject_enable_bybit(i) << endl;
          *fout << "get_lvl1_trig_rate_begin(" << i << "): " << trigrunlvl1->get_lvl1_trig_rate_begin(i) << endl;
        }
      for (i = 0;i < 130;i++)
        {
          *fout << "get_lvl1_rbit_name(" << i << "): " << trigrunlvl1->get_lvl1_rbit_name(i) << endl;
        }
      *fout << "get_run_number(): " << trigrunlvl1->get_run_number() << endl;
      *fout << "get_start_time(): " << trigrunlvl1->get_start_time() << endl;
      node_written = 1;
    }
  return 0;
}

