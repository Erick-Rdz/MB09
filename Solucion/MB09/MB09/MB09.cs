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
           string cadenaPS9;cadenaPS9 = "<IH>26PQ2K    PQ2K    NETNET  00000000MB09    010057511O01YE</IH><ME>0210BFILLERFILLER  A77c674735fa746da96d3660dd52cec0b          A                  A95461698452611  A            A            A                 A               A             A             A     A                      </ME>";



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
