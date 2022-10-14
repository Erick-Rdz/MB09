using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Odbc;
//using System.Data.SqlClient;
using System.Diagnostics;
using System.Net;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;


namespace QA1XENT
{
    [Serializable]
    public class QA1XENT
    {

        public static List<string> StackResults = new List<string>();
        public static StringBuilder Result = new StringBuilder();
        public static QAEXCA.QAEXCA qaexca = new QAEXCA.QAEXCA();
        public static QA1XOP9.QA1XOP9 qa1xop9 = new QA1XOP9.QA1XOP9();
        public static QA1XIP9.QA1XIP9 qa1xip9 = new QA1XIP9.QA1XIP9();
        public static MT2X0005.MT2X0005 mt2x0005 = new MT2X0005.MT2X0005();

        public static int qaeVal = -1;
        public static string rsp = string.Empty;
        public static string errMsg = string.Empty;

        public string execute(string input, OdbcConnection conn)
        {

            Stopwatch timer = new Stopwatch();
            timer.Start();
            //Console.WriteLine("Inicia QA1CENT_NET\n\n");
            Console.WriteLine("\t\t\t*******  INICIA LA QA1CENT_NET ********");


            Console.WriteLine("Cadena de Entrada: [{0}] \n\n", input);
            //Console.ReadKey();

            String result = String.Empty;
            String PS9 = String.Empty;

            //Console.WriteLine("Inicia QA1CIP9_NET");
            //Console.ReadKey();

            qaeVal = qa1xip9.ObtenerCadenaEntrada(input, qaexca);

            if (qaeVal == 99)
            {
                qaexca.ServiceResponse = "5";
                qaexca.ProcessControl = "A";

                Console.WriteLine("QA1COP9_NET", "QA1CIP9_NET", "las longitudes de Entrada no son correctas");
                //Console.ReadKey();

                qaexca.IdProtocolo = "26";
                Result.AppendLine();
                Console.WriteLine("Error QA1CIP9_NET las longitudes de Entrada no son correctas");
            }
            else
            {

                qaexca.IdTask = 1;
                Console.WriteLine(qaexca.getStringValues()); Console.WriteLine("\n \n");
                //Console.ReadKey();
                Console.WriteLine("Inicia IsAllowToRes \n");
                //Console.ReadKey();
                qaexca.IdTask = 2;
                call_IsAllow(qaexca.IdTransaccion, qaexca.UsuarioDeTerminal);

                if (qaexca.Canal == string.Empty)
                {
                    qaexca.CaaErrVaria1 = "CANAL";
                    qaeVal = 1;
                }
                if (qaexca.TerminalLogica == string.Empty)
                {
                    qaexca.CaaErrVaria1 = "TERMINAL";
                    qaeVal = 1;
                }
                if (qaexca.IdTransaccion == string.Empty)
                {
                    qaexca.CaaErrVaria1 = "TRANSACCION";
                    qaeVal = 1;
                }

                if (qaeVal != 1)
                {
                    Console.WriteLine("\n Inicia SP QA_VALIDACIONES \n");
                    qaexca.IdTask = 3;
                    int qaespVal = call_spVal(qaexca.Canal, qaexca.TerminalLogica, qaexca.IdTransaccion, conn);
                    if (qaeVal < 1)
                        qaeVal = qaespVal;

                }

                qaexca.IdTask = 4;
                if (qaeVal < 1)
                    seralize(qaexca.ProgramaTx, conn);

                switch (qaeVal)
                {
                    case 1:
                        qaexca.CaaSwErrcod = "QAE0040";
                        Result.Append(qaexca.CaaErrVaria1);
                        qaexca.ServiceResponse = "2";
                        break;
                    case 2:
                        qaexca.CaaSwErrcod = "QCE0015";
                        qaexca.CaaErrVaria1 = qaexca.IdTransaccion;
                        qaexca.ServiceResponse = "2";
                        break;
                    case 10:
                        qaexca.CaaSwErrcod = "QAE1011";
                        qaexca.CaaErrVaria1 = qaexca.UsuarioDeTerminal;
                        qaexca.ServiceResponse = "2";
                        break;
                    case 100:
                        qaexca.CaaSwErrcod = "QCE0007";
                        qaexca.CaaErrVaria1 = qaexca.TerminalLogica;
                        break;
                    case 101:
                        qaexca.CaaSwErrcod = "QCE0007";
                        qaexca.CaaErrVaria1 = qaexca.TerminalLogica;
                        break;
                    case 102:
                        qaexca.CaaSwErrcod = "QCE0007";
                        qaexca.CaaErrVaria1 = qaexca.TerminalLogica;
                        break;
                    default:
                        break;
                }
            }

            getHost();
            Console.WriteLine("Se obtuvo el CICS: [{0}]", qaexca.Cics);


            Console.WriteLine("Inicia Generación PS9 de Salida");
            //Console.ReadKey();

            qaexca.IdTask = 5;
            Stopwatch timer1 = new Stopwatch();
            timer1.Start();
            PS9 = qa1xop9.ConstruirSalida(qaexca).ToString();

            Console.WriteLine("Cadena PS9: [{0}] \n", PS9);
            //Console.ReadKey();

            timer1.Stop();
            long timeElapsed1 = timer1.ElapsedMilliseconds;
            Console.WriteLine("Termina Generación PS9 de Salida: [{0} ms.]", timeElapsed1);
            //Console.ReadKey();

            if (Convert.ToInt32(qaexca.OutputLength) < 26)
            {
                Console.WriteLine("Tuve un error Al Generar la PS9: Cadena Incompleta \n");
                //Console.ReadKey();
                qaexca.ServiceResponse = "5";
                qaexca.ProcessControl = "A";
                if (qaeVal != 99)
                {
                    qaeVal = 99;
                    Console.WriteLine("QA1COP9_NET", "QA_VALIDACIONES", "Cadena Incompleta");
                    //Console.ReadKey();
                }

            }

            timer.Stop();

            long timeElapsed = timer.ElapsedMilliseconds;
            qaexca.TotalTime = (double)timeElapsed / 1000;
            qaexca.IdTask = 6;

            if (qaeVal != 99)
                call_spTtcl(conn, qaexca);
            else
                call_spLog(conn, qaexca, errMsg);

            Result.AppendLine();
            result = Result.ToString();
            if (qaeVal == 99)
                EventLog.WriteEntry("QA1CENT_NET", result, EventLogEntryType.Error);
            else if (qaexca.TotalTime > 1.5)
                EventLog.WriteEntry("QA1CENT_NET", result, EventLogEntryType.Warning);
            //else if ((int)Registry.GetValue("HKEY_LOCAL_MACHINE\\SOFTWARE\\Wow6432Node\\Arquitectura\\QA1AXENT", "MustWriteAllEvents", 0) == 1)
            //{ }
            //else if ((int)Registry.GetValue("HKEY_LOCAL_MACHINE\\SOFTWARE\\Arquitectura\\QA1CENT_NET", "MustWriteAllEvents", 0) == 1)
            EventLog.WriteEntry("QA1CENT_NET", result, EventLogEntryType.Information);




            return PS9;
        }

        public static int call_spVal(string canal, string terminal, string Id_TX, OdbcConnection conn)
        {
            int resp = 0;

            Console.WriteLine("Entrada:");
            Console.WriteLine("\n");
            Console.WriteLine("Terminal: [{0}]", terminal);
            Console.WriteLine("Transaccion: [{0}]", Id_TX);
            Console.WriteLine("Canal: [{0}]", canal);
            //Console.ReadKey();
            OdbcCommand cmd = new OdbcCommand();



            try
            {
                Stopwatch timer = new Stopwatch();
                timer.Start();

                //String sqlStoredProcedure = "QA_VALIDACIONES";
                //OdbcCommand cmdSql = new OdbcCommand(sqlStoredProcedure, conn);
                OdbcCommand cmdSql = new OdbcCommand("{call MAZP.QA_VALIDACIONES (?,?,?,?,?,?)}", conn);
                int sqlRespuesta = 0;
                try
                {

                    cmdSql.CommandType = System.Data.CommandType.StoredProcedure;
                    cmdSql.Parameters.Add("@canal_cod", OdbcType.VarChar, 2).Value = canal;
                    cmdSql.Parameters.Add("@terminal", OdbcType.VarChar, 4).Value = terminal;
                    cmdSql.Parameters.Add("@Id_tx", OdbcType.VarChar, 4).Value = Id_TX;

                    cmdSql.Parameters.Add("@DATOS", OdbcType.VarChar, 3000).Direction = ParameterDirection.Output;
                    cmdSql.Parameters.Add("@MENSAJE", OdbcType.VarChar, 70).Direction = ParameterDirection.Output;
                    cmdSql.Parameters.Add("@return", OdbcType.Int).Direction = ParameterDirection.Output;

                    //cmdSql.Connection = conn;
                    sqlRespuesta = cmdSql.ExecuteNonQuery();

                    if (sqlRespuesta > 0)
                        sqlRespuesta = 1;
                    else
                        sqlRespuesta = 0;
                }
                catch (Exception ex)
                {
                    throw ex;
                }

                //cmdSql.CommandText = "{call MAZP.QA_VALIDACIONES(?,?,?,?,?,?)}";
                //cmdSql.CommandType = CommandType.StoredProcedure;


                //cmdSql.Parameters.Add("@canal_cod", SqlDbType.VarChar, 2).Value = canal;
                //cmdSql.Parameters.Add("@terminal", SqlDbType.VarChar, 4).Value = terminal;
                //cmdSql.Parameters.Add("@Id_tx", SqlDbType.VarChar, 4).Value = Id_TX;

                //cmdSql.Parameters.Add("@DATOS", SqlDbType.VarChar, 3000).Direction = ParameterDirection.Output;
                //cmdSql.Parameters.Add("@MENSAJE", SqlDbType.VarChar, 70).Direction = ParameterDirection.Output;
                //cmdSql.Parameters.Add("@return", SqlDbType.Int).Direction = ParameterDirection.Output;

                //cmdSql.Connection = conn;

                //cmdSql.ExecuteNonQuery();

                Console.WriteLine("\n Salida: \n");
                //Console.ReadKey();

                rsp = cmdSql.Parameters["@DATOS"].Value.ToString();
                Console.WriteLine($"@DATOS: {rsp}, Length: {rsp.Length}");
                Console.WriteLine("Datos: [{0}] \n", rsp);
                //Console.ReadKey();

                object mensaje = cmdSql.Parameters["@MENSAJE"].Value;
                Console.WriteLine("Mensaje: [{0}]", mensaje);

                resp = (int)cmdSql.Parameters["@return"].Value;

                Console.WriteLine("Return Value: [{0}]", resp);

                if (rsp.Length > 5)
                {
                    getData(rsp);
                }
                else
                    qaexca.ServiceResponse = "2";

                if (resp == 1)
                {
                    if (mensaje.ToString().Contains("terminal"))
                        qaexca.CaaErrVaria1 = "TERMINAL";
                    if (mensaje.ToString().Contains("canal"))
                        qaexca.CaaErrVaria1 = "CANAL";
                    if (mensaje.ToString().Contains("transaccion"))
                        qaexca.CaaErrVaria1 = "TRANSACCION";
                }

                timer.Stop();
                long timeElapsed = timer.ElapsedMilliseconds;

                Console.WriteLine("\n Termina SP QA_VALIDACIONES: [{0} ms.]  \n", timeElapsed);
                //Console.ReadKey();


            }
            catch (Exception e)
            {
                Console.WriteLine("Tuve un error en el SP: {0} \n", e.ToString());
                //Console.ReadKey();

                qaexca.ServiceResponse = "5";
                qaexca.ProcessControl = "A";
                qaeVal = 99;

                Console.WriteLine("ERROR QA7CVAQ", "QA_VALIDACIONES", e.ToString());
                //Console.ReadKey();

            }




            return resp;

        }

        public static void executetx(string Dll_TX, OdbcConnection conn)
        {
            StringBuilder dll = new StringBuilder();
            Stopwatch timer = new Stopwatch();
            timer.Start();

            Console.WriteLine("\n Inicia Llamado a Componente de transacción \n");
            //Console.ReadKey();
            Console.WriteLine("Componente: [ {0} ]", (Dll_TX + ".dll"));
            //Console.ReadKey();
            Console.WriteLine("Entrada: [{0}] \n", qaexca.MensajeEntrada);
            //Console.ReadKey();

            dll = dll.Append(Dll_TX.Substring(0, 3)).Append("X").Append(Dll_TX.Substring(4, 4));

            //Console.ReadKey();
            Console.WriteLine("ANTES DE LA UBICACION");
            //string DLL0 = @"d:\Users\B1007063\source\repos\AMBIENTE_NEW\TX\" + qaexca.IdTransaccion + @"\" + dll + ".dll";
            string DLL0 = @"d:\Users\PEIM9901\Documents\PLEM\TX\MB95_2_0\MB2X0095\MB2X0095\bin\x86\Debug\" + dll + ".dll";

            Console.WriteLine("DLL0: [{0}]", DLL0, "\n");

            try
            {
                string rspTX = string.Empty;
                //Assembly assembly0 = Assembly.LoadFrom(DLL0);
                QAEXCA.QAEXCA result = new QAEXCA.QAEXCA();


                Console.WriteLine("\n QACCA :{0} \n", qaexca.Result);

                Console.WriteLine("Salida: agregada para ver lo que tiene la cadena: {0} \n", rspTX);


                result = mt2x0005.ExecuteTx(conn, qaexca);

                qaexca = result;
                rspTX = qaexca.Result;

                //foreach (Type type in assembly0.GetExportedTypes())
                //{
                //    object c = Activator.CreateInstance(type);

                //    //result = (QAEXCA.QAEXCA)type.InvokeMember("ExecuteTx", BindingFlags.InvokeMethod, null, c, new object[] { conn, qaexca });

                //    qaexca = result;
                //    rspTX = qaexca.Result;

                //    Console.WriteLine("\n QACCA :{0} \n", qaexca.Result);
                //    Console.ReadKey();
                //    Console.WriteLine("Salida: agregada para ver lo que tiene la cadena: {0} \n", rspTX);
                //    Console.ReadKey();

                //}
                if (rspTX.Contains("ERROR"))
                {
                    //Result.AppendFormat("LLamado al programa: {0}-{1}", Dll_TX, rspTX);
                    Console.WriteLine("LLamado al programa: {0}-{1}", Dll_TX, rspTX);
                    //Console.ReadKey();
                    qaexca.ServiceResponse = "5";
                    qaexca.ProcessControl = "A";
                    qaeVal = 99;
                    Console.WriteLine(Dll_TX, Dll_TX, rspTX);
                    //Console.ReadKey();
                    //getErrMsg(Dll_TX, Dll_TX, rspTX);
                }
                else
                {
                    Console.WriteLine("Salida: [{0}]", result.ToString());
                    //Console.ReadKey();
                }


                timer.Stop();
                long timeElapsed = timer.ElapsedMilliseconds;
                Console.WriteLine("\n Termina Llamado a Componente de transacción: [{0} ms.] \n", timeElapsed);
                //Console.ReadKey();

            }
            catch (Exception e)
            {
                Console.WriteLine("\n Resultado del LLamado al programa dummy: {0}-{1} \n", DLL0, e.ToString());
                //Console.ReadKey();
                qaexca.ServiceResponse = "5";
                qaexca.ProcessControl = "A";
                qaeVal = 99;
            }

        }

        public static void seralize(string Dll_TX, OdbcConnection conn)
        {
            try
            {
                executetx(Dll_TX, conn);

            }
            catch (Exception e)
            {
                Console.WriteLine("Error al serializar: {0}", e.ToString());
                //Console.ReadKey();
                qaexca.ServiceResponse = "5";
                qaexca.ProcessControl = "A";
                qaeVal = 99;
                Console.WriteLine("ERROR", Dll_TX, Dll_TX, e.ToString());
                //Console.ReadKey();
            }


        }

        public static void call_IsAllow(string transactionName, string userName)
        {

            try
            {

                Console.WriteLine("Entrada: \n");
                //Console.ReadKey();
                Console.WriteLine("Usuario: [{0}]", userName);
                Console.WriteLine("Transaccion: [{0}]", transactionName);

                string domainControllerName = "DC=produccion,DC=bancoazteca";
                string transactionOU = "OU=Resources";
                string userOU = "OU=UsuariosAlnova";
                transactionName = "CN=" + transactionName;
                userName = "CN=" + userName;

                Stopwatch timer = new Stopwatch();
                timer.Start();

                int valueR = 1;
                //int valueR = IsAllowToRes(domainControllerName, transactionOU, userOU, transactionName, userName);

                timer.Stop();
                long timeElapsed = timer.ElapsedMilliseconds;
                if (valueR == 1)
                {
                    Console.WriteLine("Salida: [Usuario Autorizado]" + valueR);
                    //Console.ReadKey();
                }
                else
                {
                    qaeVal = 10;
                    Console.WriteLine("Salida: [Usuario No Autorizado]" + valueR);
                    Console.WriteLine("\n");
                    //Console.ReadKey();
                }
                Console.WriteLine("Termina IsAllowToRes: [{0} ms.]", timeElapsed);
                Console.WriteLine("\n");
                //Console.ReadKey();
            }
            catch (Exception e)
            {
                Result.AppendFormat("Error al serializar: {0}", e.ToString());
                qaexca.ServiceResponse = "5";
                qaexca.ProcessControl = "A";
                qaeVal = 99;
                Console.WriteLine("QA7CSEG", "IsAllowToRes", e.ToString());
            }

        }

        public void call_spTtcl(OdbcConnection conn, QAEXCA.QAEXCA commarea)
        {

            OdbcCommand cmd = new OdbcCommand();

            try
            {
                Stopwatch timer = new Stopwatch();
                timer.Start();

                //odbc
                //String sqlStoredProcedure = "QA_TECLEOS";
                //SqlCommand cmdSql = new SqlCommand(sqlStoredProcedure, conn);
                OdbcCommand cmdSql = new OdbcCommand("{call MAZP.QA_TECLEOS(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}", conn);
                //cmd.CommandText = "{call MAZP.QA_TECLEOS(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
                cmd.CommandType = CommandType.StoredProcedure;


                cmd.Parameters.Add("@ENT", OdbcType.VarChar, 4).Value = "";
                cmd.Parameters.Add("@BRN_ACCT", OdbcType.VarChar, 4).Value = commarea.Centro;
                cmd.Parameters.Add("@ACCT_NTNM", OdbcType.VarChar, 8).Value = commarea.TerminalFisica;
                cmd.Parameters.Add("@ACCT_TRM", OdbcType.VarChar, 4).Value = commarea.TerminalFisica;
                cmd.Parameters.Add("@DAT_ACCT", OdbcType.VarChar, 8).Value = "";
                cmd.Parameters.Add("@DAT_OPERATION", OdbcType.VarChar, 8).Value = "";
                cmd.Parameters.Add("@BRN_LOCATION", OdbcType.VarChar, 4).Value = commarea.Centro;
                cmd.Parameters.Add("@NETNAME", OdbcType.VarChar, 8).Value = commarea.TerminalFisica;
                cmd.Parameters.Add("@TERMINAL", OdbcType.VarChar, 4).Value = commarea.TerminalFisica;
                cmd.Parameters.Add("@USER_ID", OdbcType.VarChar, 8).Value = commarea.UsuarioDeTerminal;
                cmd.Parameters.Add("@SESSION", OdbcType.VarChar, 1).Value = "";
                cmd.Parameters.Add("@TYP_TERMINAL", OdbcType.VarChar, 2).Value = commarea.IdProtocolo;
                cmd.Parameters.Add("@TERMCODE", OdbcType.VarChar, 2).Value = "";
                cmd.Parameters.Add("@DAT_TRMS", OdbcType.VarChar, 8).Value = "";
                cmd.Parameters.Add("@TIM_TRMS", OdbcType.VarChar, 6).Value = "";
                cmd.Parameters.Add("@CICS", OdbcType.VarChar, 4).Value = commarea.Cics;
                cmd.Parameters.Add("@NUM_TASK", OdbcType.Decimal, 5).Value = 0;
                cmd.Parameters.Add("@COD_START", OdbcType.VarChar, 2).Value = "";
                cmd.Parameters.Add("@STT_TRNS", OdbcType.VarChar, 4).Value = "";
                cmd.Parameters.Add("@LNG_TERMINAL", OdbcType.VarChar, 1).Value = commarea.Idioma;
                cmd.Parameters.Add("@COD_APP_1", OdbcType.VarChar, 2).Value = commarea.Modulo;
                cmd.Parameters.Add("@COD_TRNS_1", OdbcType.VarChar, 4).Value = commarea.IdTransaccion;
                cmd.Parameters.Add("@COD_PROGRAM_1", OdbcType.VarChar, 8).Value = commarea.ProgramaTx;
                cmd.Parameters.Add("@PASS_CTRL_2", OdbcType.VarChar, 5).Value = "";
                cmd.Parameters.Add("@COD_APP_2", OdbcType.VarChar, 2).Value = "";
                cmd.Parameters.Add("@COD_TRNS_2", OdbcType.VarChar, 4).Value = "";
                cmd.Parameters.Add("@COD_PROGRAM_2", OdbcType.VarChar, 8).Value = "";
                cmd.Parameters.Add("@ST_INP", OdbcType.VarChar, 1).Value = "";
                cmd.Parameters.Add("@CASE_A1", OdbcType.VarChar, 1).Value = "";
                cmd.Parameters.Add("@DTA_INP_T", OdbcType.VarChar, 30).Value = "";
                cmd.Parameters.Add("@DTA_LTH_INP", OdbcType.Decimal, 5).Value = Convert.ToDecimal(commarea.TamañoMensajeEntrada);
                cmd.Parameters.Add("@ST_OUT", OdbcType.VarChar, 1).Value = "";
                cmd.Parameters.Add("@CASE_A2", OdbcType.VarChar, 1).Value = "";
                cmd.Parameters.Add("@DTA_OUTPUT", OdbcType.VarChar, 30).Value = "";
                cmd.Parameters.Add("@DTA_LTH_OUT", OdbcType.Decimal, 5).Value = Convert.ToDecimal(commarea.OutputLength);
                cmd.Parameters.Add("@COD_NEXT_TRNS", OdbcType.VarChar, 4).Value = "";
                cmd.Parameters.Add("@ACTION", OdbcType.VarChar, 3).Value = "";
                cmd.Parameters.Add("@JOB", OdbcType.VarChar, 1).Value = "";
                cmd.Parameters.Add("@CASE", OdbcType.VarChar, 1).Value = "";
                cmd.Parameters.Add("@DTA", OdbcType.VarChar, 30).Value = "";
                cmd.Parameters.Add("@AUTHORIZATION", OdbcType.VarChar, 150).Value = "";
                cmd.Parameters.Add("@DES_INP_CPY", OdbcType.VarChar, 8).Value = commarea.DesInpCpy;
                cmd.Parameters.Add("@INP_PANL", OdbcType.VarChar, 8).Value = "";
                cmd.Parameters.Add("@TYP_PROCESS", OdbcType.VarChar, 1).Value = "";
                cmd.Parameters.Add("@NUM_SEQUENCE", OdbcType.VarChar, 5).Value = commarea.NumeroSecuenciaPs9;
                cmd.Parameters.Add("@KEY", OdbcType.VarChar, 2).Value = "";
                cmd.Parameters.Add("@ATM", OdbcType.VarChar, 1).Value = "";
                cmd.Parameters.Add("@DTA_INP_MOD", OdbcType.VarChar, 1).Value = "";
                cmd.Parameters.Add("@TYP_MESSAGE", OdbcType.VarChar, 4).Value = commarea.Canal;
                cmd.Parameters.Add("@ERR_COD", OdbcType.VarChar, 7).Value = commarea.CaaSwErrcod;
                cmd.Parameters.Add("@COD_NOTIFY1", OdbcType.VarChar, 7).Value = commarea.CaaSwCodWA1;
                cmd.Parameters.Add("@COD_NOTIFY2", OdbcType.VarChar, 7).Value = commarea.CaaSwCodWA2;
                cmd.Parameters.Add("@ERR_VAR_1", OdbcType.VarChar, 20).Value = commarea.CaaErrVaria1;
                cmd.Parameters.Add("@ERR_VAR_2", OdbcType.VarChar, 20).Value = commarea.CaaErrVaria2;
                cmd.Parameters.Add("@NTF_1_VAR_1", OdbcType.VarChar, 20).Value = commarea.CaaWarn1Varia1;
                cmd.Parameters.Add("@NTF_1_VAR_2", OdbcType.VarChar, 20).Value = commarea.CaaWarn1Varia2;
                cmd.Parameters.Add("@NTF_2_VAR_1", OdbcType.VarChar, 20).Value = commarea.CaaWarn2Varia1;
                cmd.Parameters.Add("@NTF_2_VAR_2", OdbcType.VarChar, 20).Value = commarea.CaaWarn2Varia2;
                cmd.Parameters.Add("@AMT_DISPENSER", OdbcType.Decimal, 9).Value = 0;
                cmd.Parameters.Add("@LCL_EJOU", OdbcType.VarChar, 80).Value = "";
                cmd.Parameters.Add("@TYP_OUTPUT", OdbcType.VarChar, 1).Value = "";
                cmd.Parameters.Add("@DES_OUT_CPY", OdbcType.VarChar, 8).Value = "";
                cmd.Parameters.Add("@OUT_PANL", OdbcType.VarChar, 8).Value = "";
                cmd.Parameters.Add("@DESTINATIONS", OdbcType.VarChar, 75).Value = "";
                cmd.Parameters.Add("@FLG_PEND_AUT", OdbcType.VarChar, 1).Value = "";
                cmd.Parameters.Add("@AMT_PEND_AUT", OdbcType.Decimal, 9).Value = 0;
                cmd.Parameters.Add("@AUT_REF", OdbcType.VarChar, 20).Value = "";
                cmd.Parameters.Add("@ENT_ANALYTIC", OdbcType.VarChar, 4).Value = "";
                cmd.Parameters.Add("@CEN_ANALYTIC", OdbcType.VarChar, 4).Value = "";
                cmd.Parameters.Add("@ANL_PROD", OdbcType.VarChar, 20).Value = "";
                cmd.Parameters.Add("@DTA_ANL_1", OdbcType.VarChar, 27).Value = "";
                cmd.Parameters.Add("@TYP_OPE", OdbcType.VarChar, 1).Value = "";
                cmd.Parameters.Add("@ACCT_OPE", OdbcType.VarChar, 1).Value = "";
                cmd.Parameters.Add("@DTA_APP", OdbcType.VarChar, 20).Value = "";
                cmd.Parameters.Add("@DTA_ANL_2", OdbcType.VarChar, 30).Value = commarea.Canal + "DS";
                cmd.Parameters.Add("@NUM_TASK_TIM", OdbcType.Decimal).Value = Convert.ToDecimal(commarea.TotalTime);
                cmd.Parameters.Add("@NUM_APP_TIM_1", OdbcType.Decimal, 5).Value = 0;
                cmd.Parameters.Add("@NUM_APP_TIM_2", OdbcType.Decimal, 5).Value = 0;
                cmd.Parameters.Add("@FLG_EMULATION", OdbcType.VarChar, 1).Value = "";
                cmd.Parameters.Add("@NUM_VAL_RUT", OdbcType.Decimal, 5).Value = 0;
                cmd.Parameters.Add("@DEB_CASH", OdbcType.Decimal, 9).Value = 0;
                cmd.Parameters.Add("@DEB_CLEARING", OdbcType.Decimal, 9).Value = 0;
                cmd.Parameters.Add("@AMT_CSHCRE", OdbcType.Decimal, 9).Value = 0;
                cmd.Parameters.Add("@AMT_CLCRE", OdbcType.Decimal, 9).Value = 0;
                cmd.Parameters.Add("@NUM_JOU_REC", OdbcType.Decimal, 5).Value = 0;
                cmd.Parameters.Add("@FLG_UPD_STR", OdbcType.VarChar, 1).Value = "";
                cmd.Parameters.Add("@NUM_SEND", OdbcType.Decimal, 5).Value = 0;
                cmd.Parameters.Add("@NUM_CHARACTER", OdbcType.Decimal, 5).Value = 0;
                cmd.Parameters.Add("@NUM_MAP_DOCU", OdbcType.Decimal, 5).Value = 0;
                cmd.Parameters.Add("@NUM_MAP_SCR", OdbcType.Decimal, 5).Value = 0;
                cmd.Parameters.Add("@NUM_STD_MAP", OdbcType.Decimal, 5).Value = 0;
                cmd.Parameters.Add("@NUM_NOSTD_MAP", OdbcType.Decimal, 5).Value = 0;
                cmd.Parameters.Add("@PREFORMAT_1", OdbcType.VarChar, 7).Value = "";
                cmd.Parameters.Add("@PREFORMAT_2", OdbcType.VarChar, 7).Value = "";
                cmd.Parameters.Add("@ERR_OBJECT", OdbcType.VarChar, 8).Value = "";
                cmd.Parameters.Add("@SQLCODE", OdbcType.Int, 4).Value = 0;
                cmd.Parameters.Add("@SQLERRM", OdbcType.VarChar, 70).Value = "";
                cmd.Parameters.Add("@EIBFN", OdbcType.VarChar, 2).Value = "";
                cmd.Parameters.Add("@EIBRSRCE", OdbcType.VarChar, 8).Value = "";
                cmd.Parameters.Add("@EIBRCODE", OdbcType.VarChar, 6).Value = "";
                cmd.Parameters.Add("@EIBRESP1", OdbcType.Int, 4).Value = 0;
                cmd.Parameters.Add("@EIBRESP2", OdbcType.Int, 4).Value = 0;
                cmd.Parameters.Add("@INP_MSG_LTH", OdbcType.Decimal, 5).Value = Convert.ToDecimal(commarea.TamañoMensajeEntrada);
                cmd.Parameters.Add("@INP_MSG", OdbcType.VarChar, 2048).Value = commarea.MensajeEntrada;
                cmd.Parameters.Add("@ERRORES", OdbcType.VarChar, 50).Direction = ParameterDirection.Output;
                cmd.Parameters.Add("@return", OdbcType.Int, 3).Direction = ParameterDirection.ReturnValue;



                //cmd.Connection = conn;
                cmd.ExecuteNonQuery();
                Result.AppendFormat("Salida: [{0}] ", cmd.Parameters["@ERRORES"].Value);
                Result.AppendLine();
                timer.Stop();
                long timeElapsed = timer.ElapsedMilliseconds;
                Result.AppendFormat("Termina Registro en Tecleos - SP QA_TECLEOS: [{0} ms.]  ", timeElapsed);
            }
            catch (Exception e)
            {
                Result.AppendFormat("Tuve un error en el SP: {0}", e.ToString());
                Result.AppendLine();
                qaexca.ServiceResponse = "5";
                qaexca.ProcessControl = "A";
                qaeVal = 99;
                //getErrMsg("QA7CITCL", "QA_TECLEOS", e.ToString());
            }


        }

        public void call_spLog(OdbcConnection conn, QAEXCA.QAEXCA commarea, string excMsg)
        {

            //SqlConnection cmd = new SqlConnection();

            try
            {

                //String sqlStoredProcedure = "QA_VALIDACIONES";

                //int sqlRespuesta = 0;
                //try
                //{

                //    cmdSql.CommandType = System.Data.CommandType.StoredProcedure;
                //    cmdSql.Parameters.Add("@canal_cod", SqlDbType.VarChar, 2).Value = canal;
                //    cmdSql.Parameters.Add("@terminal", SqlDbType.VarChar, 4).Value = terminal;
                //    cmdSql.Parameters.Add("@Id_tx", SqlDbType.VarChar, 4).Value = Id_TX;

                //    cmdSql.Parameters.Add("@DATOS", SqlDbType.VarChar, 3000).Direction = ParameterDirection.Output;
                //    cmdSql.Parameters.Add("@MENSAJE", SqlDbType.VarChar, 70).Direction = ParameterDirection.Output;
                //    cmdSql.Parameters.Add("@return", SqlDbType.Int).Direction = ParameterDirection.Output;


                Stopwatch timer = new Stopwatch();
                timer.Start();

                //String sqlStoredProcedure = "QA_QADTLOG";
                //SqlCommand cmdSql = new SqlCommand(sqlStoredProcedure, conn);
                OdbcCommand cmdSql = new OdbcCommand("{call MAZP.QA_QADTLOG(?,?,?,?,?,?,?,?,?)}", conn);
                //cmd.CommandText = "{call MAZP.QA_QADTLOG(?,?,?,?,?,?,?,?,?)}";
                cmdSql.CommandType = CommandType.StoredProcedure;

                cmdSql.Parameters.Add("@Sys_Id", OdbcType.VarChar, 4).Value = commarea.Cics;
                cmdSql.Parameters.Add("@Num_task", OdbcType.Decimal).Value = commarea.IdTask;
                cmdSql.Parameters.Add("@Cod_trns", OdbcType.VarChar, 4).Value = commarea.IdTransaccion;
                cmdSql.Parameters.Add("@Nom_TRM", OdbcType.VarChar, 8).Value = commarea.TerminalFisica;
                cmdSql.Parameters.Add("@User", OdbcType.VarChar, 8).Value = commarea.UsuarioDeTerminal;
                cmdSql.Parameters.Add("@Nom_PGM", OdbcType.VarChar, 8).Value = commarea.ProgramaTx;
                cmdSql.Parameters.Add("@Exec_MSG", OdbcType.VarChar, 150).Value = excMsg;
                cmdSql.Parameters.Add("@Mensaje_ret", OdbcType.VarChar, 50).Direction = ParameterDirection.Output;
                cmdSql.Parameters.Add("@return", OdbcType.Int, 3).Direction = ParameterDirection.Output;



                //cmd.Connection = conn;
                cmdSql.ExecuteNonQuery();
                Result.AppendFormat("Salida: [{0}] ", cmdSql.Parameters["@Mensaje_ret"].Value);
                Result.AppendLine();
                timer.Stop();
                long timeElapsed = timer.ElapsedMilliseconds;
                Result.AppendFormat("Termina Registro en QADTLOG - SP Insert Tlog: [{0} ms.]  ", timeElapsed);
            }
            catch (Exception e)
            {
                Result.AppendFormat("Tuve un error en el SP: {0}", e.ToString());
                Result.AppendLine();
                qaexca.ServiceResponse = "5";
                qaexca.ProcessControl = "A";
                qaeVal = 99;
                getErrMsg("QA1CENT", "QA_QADTLOG", e.ToString());
            }
        }

        void getErrMsg(String PGM, String OBJ, String ERR)
        {
            StringBuilder buildErrorLog = new StringBuilder();
            buildErrorLog.Append("ERROR APL. - PGM: ");
            buildErrorLog.Append(PGM.PadRight(23, ' '));
            buildErrorLog.Append(".OBJ.ERR: ");
            OBJ += ERR;
            if (OBJ.Length > 73)
                OBJ = OBJ.Substring(0, 73);
            buildErrorLog.Append(OBJ.PadRight(73, ' '));
            buildErrorLog.Append(".REF: ");

            errMsg = buildErrorLog.ToString();
        }

        public static void getData(String s)
        {
            string value = "|T,   ,MB63,S,MB2C0630,MB,OLV3,9546,06,MBNC0630|";
            qaexca.ServiceResponse = "1";
            qaexca.ProcessControl = "000     ";

            char[] searchString = { ',', '|' };
            String[] listData = s.Split(searchString);
            qaexca.PanelFormat = listData[2];
            qaexca.TipoTX = listData[1];
            qaexca.ProgramaTx = listData[5];
            qaexca.Modulo = listData[6];
            qaexca.Centro = listData[8];
            qaexca.DesInpCpy = listData[10];
        }


        public static void getHost()
        {
            try
            {
                // Get the local computer host name.
                qaexca.Cics = Dns.GetHostName();
                qaexca.Cics = qaexca.Cics.Substring(qaexca.Cics.Length - 4);
            }
            catch (Exception e)
            {
                Console.WriteLine("\n Tuve un error en el GetHost: {0} \n", e.ToString());
                //Console.ReadKey();

                //this.Result.AppendFormat("Tuve un error en el GetHost: {0}", e.ToString());
                //this.Result.AppendLine();
                qaexca.ServiceResponse = "5";
                qaexca.ProcessControl = "A";
                qaeVal = 99;
                Console.WriteLine("\n QA1CENT", "GetHost", e.ToString());
                //getErrMsg("QA1CENT", "GetHost", e.ToString());
            }

        }

        //[DllImport(@"d:\Users\PEIM9901\source\repos\AMBIENTE_DE_PRUEBAS\MBBL\IsAllowToRes.dll",
        //    EntryPoint = "IsAllowToRes", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        [DllImport(@"C:\Users\PROR9902\Desktop\MT05\MT05\MT05\IsAllowToRes.dll",
            EntryPoint = "IsAllowToRes", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        public static extern int IsAllowToRes(string s1, string s2, string s3, string s4, string s5);
    }
}
