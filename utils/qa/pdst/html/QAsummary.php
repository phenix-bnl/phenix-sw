<?php
include ("QA_plot.php");
include ("QAsummary.inc");

echo "
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" \"http://www.w3.org/TR/html4/frameset.dtd\">
<html>
     <head>
        <META HTTP-EQUIV=\"CONTENT-TYPE\" CONTENT=\"text/html; charset=utf-8\">
        <title>QA Summary</title>
        <STYLE  type=\"text/css\">{font-style: SMALL}
        <!ENTITY % HTMLspecial PUBLIC \"-//W3C//ENTITIES Special//EN//HTML\">
        </STYLE>
    </head>
<body style=\"background-color: rgb(204, 255, 255); color: rgb(0, 0, 0);\">
<center><img src=\"logo.small.gif\" title=\"\" alt=\"phenix logo\" style=\"\" align=\"middle\">
<font size=\"+2\" style=\"color: rgb(255, 102, 0);\">   QA SUMMARY              </font></center>
<FORM  action=\"http://".$HTTP_SERVER_VARS['HTTP_HOST']
       ."/".dirname($HTTP_SERVER_VARS['PHP_SELF'])
       ."/QA_plot.php\" target=\"showing\">
<table border=\"0\" style=\"text-align: left; width: 100%;\">
  <tbody>
      <td style=\"vertical-align: top;\">";
           list_parameters();
echo "
         <br><input size=\"6\" name=\"runmin\" value=\"99999\">&le RUN Number &le <input size=\"6\" name=\"runmax\" value=\"999999\">
         <br><input size=\"7\" name=\"valmin\" value=\"-9999.9\"> &le parameter &le <input size=\"7\"  name=\"valmax\" value=\"9999.9\">
         <br><input size=\"60\" type=\"text\" name=\"seltext\" value=\"\"><br>
         <small>Selection : Ex.: BBC-&gt;*Z*=0 and Electron-&gt;N. Electron&gt;100</small><br>";
list_tags();
echo "         
          </td>
      <td style=\"vertical-align: left;\">
          <input type=\"radio\" name=\"option\" value=\"1\">Plot Parameter x Run <br>
          <input type=\"radio\" name=\"option\" value=\"2\">Select Runs with parameter into the range<br>
         <input type=\"radio\" name=\"option\" value=\"3\">Print parameter values<br>
      </td>
      <td style=\"vertical-align: center;\">
          <center><INPUT TYPE=\"submit\" VALUE=\"Submit Query\"><INPUT type=\"reset\"><center><br>
          <center><A target=\"showing\" HREF=\"QAtutorial.html\">QA TUTORIAL</A></center>
          <center>Mantained by : <A HREF=\"mailto:slash@bnl.gov\">Cesar Luiz da Silva<br>
          University of Sao Paulo</A></center>
     </td>
  </tbody>
</table></FORM>
</body>
</html>";
?>
