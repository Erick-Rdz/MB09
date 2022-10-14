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
            //Se instancia el objeto de la QA1XENT para hacer el proceso de ejecución de nuestra transacción
            QA1XENT.QA1XENT qa1xent = new QA1XENT.QA1XENT();
            //Simula la cadena de entrada PS9
            string cadenaPS9; cadenaPS9 = "<IH>26PQ2K    PQ2K    NETNET  00000000MB09    010057511O01YE</IH><ME>0210BFILLERFILLER  A693d4754f284494c89b23b0276559834          A                  A06730100291521  A            A            A                 A               A             A             A     A                      </ME>";
            //string cadenaPS9 = "<IH>26OLV3    OLV3    NETNET  00000000MT05    000057511O06YE</IH><ME>0034BFILLERFILLER  AC  A                   </ME>";
            Console.WriteLine("Se inicia la conexion");
            //Cadena de conexión para acceder a la base de datos de desarrollo
            //Se hará la conexión por medio de ODBC
            OdbcConnection connection = new OdbcConnection($"DSN=Solucion;UID=MAZP;PWD=DESM4ZP;");
            //OdbcConnection connection = new OdbcConnection($"DSN=Integracion;UID=MAZP;PWD=INTM4ZP;");
            //Inicia el proceso de conexión
            try
            {
                //Se abre la conexión
                connection.Open();
                Console.WriteLine(qa1xent.execute(cadenaPS9, connection));

            }
            catch (Exception ex)
            {
                Console.WriteLine(ex);
                throw;
            }
            //Se cierra la conexión
            connection.Close();
            //Se espera teclear en consola
            Console.ReadLine();
        }
    }
}
