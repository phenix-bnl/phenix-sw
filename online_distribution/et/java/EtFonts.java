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
 *      Java class for defining fonts and html font tags for ET viewer
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

public class EtFonts {
  // cannot construct object
  private EtFonts () {
  }

  public static final Font titleFont = new Font("Lucida Sans Typewriter", Font.BOLD, 14);
  public static final String titleHTML = new String("<font size=3 face=\"Lucida Sans Typewriter\">");
  public static final Font inputFont = new Font("Lucida Sans Typewriter", Font.BOLD, 14);
  public static final Font displayFont = new Font("Helvetica", Font.PLAIN, 10);
  public static final String displayHTML = new String("<font size=1 face=\"Helvetica\">");
  public static final Font rateFont = new Font("Helvetica", Font.BOLD, 14);
}
