/*----------------------------------------------------------------------------*
 *  Copyright (c) 2000        Southeastern Universities Research Association, *
 *                            Thomas Jefferson National Accelerator Facility  *
 *                                                                            *
 *    This software was developed under a United States Government license    *
 *    described in the NOTICE file included as part of this distribution.     *
 *                                                                            *
 * TJNAF Data Acquisition Group, 12000 Jefferson Ave., Newport News, VA 23606 *
 *      heyes@cebaf.gov   Tel: (804) 269-7030    Fax: (804) 269-5800          *
 *----------------------------------------------------------------------------*
 * Description:
 *      Java class for setting colors in ET viewer
 *
 * Author:
 *	Carl Timmer
 *	TJNAF Data Acquisition Group
 *
 * Revision History:
 *
 *----------------------------------------------------------------------------*/

import java.lang.*;
import java.awt.*;

public class EtColors {
  // cannot construct object
  private EtColors () {
  }

  public static final Color title    = Color.blue;
  public static final Color text     = Color.black;
  public static final Color border   = Color.black;
  public static final Color unlocked = Color.black;
  public static final Color locked   = Color.red;
  public static final Color inList   = Color.red;
  public static final Color outList  = Color.blue;
  public static final Color active   = Color.green;
  public static final Color inActive = Color.red;
  public static final Color background = new Color(230, 200, 130);
  public static final Color lightBackground = new Color(255, 215, 0);
  public static final Color textBackground = Color.white;
}
