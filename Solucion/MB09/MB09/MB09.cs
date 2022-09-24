using QA1XENT;
using System;
using System.Collections.Generic;
using System.Data.Odbc;
using System.Text;

namespace MB09
{
    class MB09
    {
        static void Main(string[] args)
        {
           
           QA1XENT.QA1XENT qa1xent = new QA1XENT.QA1XENT();
           string cadenaPS9;cadenaPS9 = "<IH>26OLV3    OLV3    NETNET  00000000MB09    000057511O06YE</IH><ME>0673BFILLERFILLER  A0b06c940932c4d04a37cc62384e1f13d          A                  A95461620569614  A            A            A                 A               A             A             A     A                      </ME>";



            OdbcConnection conn = new OdbcConnection($"DSN=Solucion;UID=MAZP;PWD=DESM4ZP;");
            try
            {

                conn.Open();
                string result = qa1xent.execute(conn, cadenaPS9);
                Console.WriteLine("Cadena Ps9: " + result);
                conn.Close();
                Console.ReadLine();
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
            }
        }
    }
}
