using System;
using System.Collections.Generic;
using System.Data.Odbc;
using System.Data;
using System.Text;
using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Reflection;
using System.Net;
using Microsoft.Win32;
using QAEXCA;
using QA1XOP9;
using QA1XIP9;

#region INFORMACION DEL COMPONENTE
//******************************************************************
//*                         QA1CENT                                *
//*                         -------                                *
//*                         B.OR.S                                 *
//******************************************************************
//*                         A.OR.S                                 *
//*       MODULO DIRECTOR DE ENTRADA. DIRECCIONA TODO LA LOGIGA
//*   DE CONTROL DEL FLUJO DE ENTRADA DE ALTAMIRA.
//*
//*      RECIBE EL CONTROL AL PRINCIPIO DE CUALQUIER TRANSACCION
//*   OBTENIENDO EL MENSAJE DE ENTRADA.
//*
//* PRINCIPALES FUNCIONES QUE REALIZA ESTE PROGRAMA:
//*
//*   - FILTRA TODO TIPO DE ENTRADA.
//*
//** TRATA CUALQUIER TIPO DE MENSAJE COMO UN MENSAJE COMUN.
//*     ESTE FILTRO ES REALIZADO POR EL MODULO QA1CMSE.
//*
//*   - CAPTURA INFORMACION DEL SISTEMA NECESARIA PARA LA MAYORIA
//*     DE LAS TRANSACCIONES.
//*
//*   - ACCEDE A LAS TABLAS DEL SISTEMA.
//*
//*   - CAPTURA INFORMACION DE AUDITORIA PARA SER SALVADA EN LAS
//* TABLAS DB2 POSTERIORMENTE POR EL MODULO DIRECTOR DE SALIDA.
//*
//*   - GESTIONA LA POSIBLE DESACATIVACION DE LAS APLICACIONES O
//*      LAS TRANSACCIONES.
//*
//*   - GESTIONA LA RECEPCION DE LA PSEUDOCONVERSACION Y MANTIENE
//*     LOS DATOS DE CONTEXTO.
//*
//*A.OR.E*
//******************************************************************
//******************************************************************
//*
//*  QA7CSQ1: LEE LAS TABLAS DB2 DE LA ARQUITECTURA
//*  QG1CABC: MODULO GESTOR DE ERRORES.
//*  QA6CFMT: RUTINA PARA FORMATEAR LA FECHA Y LA HORA
//*  QA6CTRA: MODULO GESTOR DEL TRACE
//*  QA6CASG: RUTINA ASSIGN SYSID
//*  QA6CASK: RUTINA QUE OBTIENE EL TIEMPO.
//*
//*  QG2CSEG: RUTINA DE SEGURIDAD
//*  QG6CTUT: RUTINA PARA LEER/ESCRIBIR/O BORRAR LA INFORMACION
//*  DE LA DFHEIBLK.
//*
//*  QA6CGTM: RUTINA PARA RESERVAR MEMORIA
//*  QA6CFRE: RUTINA PARA LIBERAR MEMORIA
//*  QA1CMSE: MODULO DE GESTION DE LOS MENSAJES
//*  QA1CGTS: GESTIONA LA PAGINACION
//*  QA1CSAL: MODULO DIRECTOR DE SALIDA
//*  QA2CAUS: MODULO DE AUTORIZACIONES
//*  TC7C3000: MODULO DE INTERFASE DE TABLAS CORPORATIVAS
//*  A.PR.E
//******************************************************************
//******************************************************************
//*  A.PR.S
//*  COMMAREA CON OTROS MODULOS :
//******************************************************************
//*
//*  QAECCIA: CON LOS MODULOS DE ARQUITECTURA
//*  QGECCAA: CON LOS MODULO DE APLICACION
//*  A.PR.E
//******************************************************************
#endregion
#region LOG MODIFICACIONES
//  *----------------------------------------------------------------*
//  *          L O G    D E    M O D I F I C A C I O N E S           *
//  *----------------------------------------------------------------*
//  *                                                                *
//  * MARCA         AUTOR   FECHA    DESCRIPCION                     *
//  * -----------  -------  -------  ------------------------------- *
//  * @PE00001     EFREN    01-02-21 SE MODIFICA EL METODO GET DATA  *
//  *                                                                *
//  *                                                                *
//  *                                                                *
//  *                                                                *
//  *                                                                *
//  *----------------------------------------------------------------*
#endregion
namespace QA1XENT
{
    [Serializable]
    public class QA1XENT
    {
        //sb = StringBuilder
        //str = string
        //int = integer
        //dcm = decimal
        //bln = boolean
        //flt = float
        //flt = float

        /// <summary>
        /// STRING BUILDER QUE GUARDA CUALQUIER ERROR QUE SE PRESENTE EN EL PROCESO DE LA TRANSACCION
        /// </summary>
        public StringBuilder sbResult = new StringBuilder();
        QAEXCA.QAEXCA qaeaxca = new QAEXCA.QAEXCA();
        QA1XOP9.QA1XOP9 qa1axop9 = new QA1XOP9.QA1XOP9();
        QA1XIP9.QA1XIP9 qa1axip9 = new QA1XIP9.QA1XIP9();
        public static MB2X0009.MB2X0009 _componente = new MB2X0009.MB2X0009();

        int intQaeVal = -1;
        string strRsp = string.Empty;
        string strErrMsg = string.Empty;

        /// <summary>
        /// METODO PRINCIPAL DEL PROGRAMA DE LA QA1AXENT QUE RECIBE LA CADENA DE ENTRADA Y EL OBJETO DE LA CONEXION A LA BASE DE DATOS
        /// </summary>
        /// <param name="conn">OBJETO DE CONEXION DE LA BASE DE DATOS</param>
        /// <param name="input">CADENA DE ENTRADA QUE SE VALIDA EN EL COPY QA1AXIP9</param>
        /// <returns></returns>
        public string execute(OdbcConnection conn, string input)
        {

            Stopwatch timer = new Stopwatch();
            timer.Start();
            sbResult.AppendLine();
            sbResult.Append("Inicia QA1XENT");
            sbResult.AppendLine();
            sbResult.AppendLine();
            sbResult.AppendFormat("Cadena de Entrada: [{0}]", input);
            sbResult.AppendLine();
            sbResult.AppendLine();
            String strResult = String.Empty;
            String strPS9 = String.Empty;

            sbResult.AppendLine();
            sbResult.Append("Inicia QA1XIP9");
            sbResult.AppendLine();

            //SE LLAMA A LA NUEVA DLL QUE GENERA LA ENTRADA
            intQaeVal = qa1axip9.ConstruirCadenaEntrada(input, qaeaxca);
            //qaeVal = qaecca.build(input);

            if (intQaeVal == 99)
            {
                qaeaxca.ServiceResponse = "5";
                qaeaxca.ProcessControl = "A";
                getErrMsg("QA1XOP9", "QA1XIP9", "las longitudes de Entrada no son correctas");
                qaeaxca.IdProtocolo = "26";
                sbResult.AppendLine();
                sbResult.Append("Error QA1XIP9 las longitudes de Entrada no son correctas");
                sbResult.AppendLine();
            }
            else
            {
                qaeaxca.IdTask = 1;
                sbResult.Append(qaeaxca.getStringValues());
                sbResult.AppendLine();

                sbResult.AppendLine();
                sbResult.Append("Inicia IsAllowToRes");
                sbResult.AppendLine();
                qaeaxca.IdTask = 2;
                call_IsAllow(qaeaxca.IdTransaccion, qaeaxca.UsuarioDeTerminal);

                if (qaeaxca.Canal == string.Empty)
                {
                    qaeaxca.CaaErrVaria1 = "CANAL";
                    intQaeVal = 1;
                }
                if (qaeaxca.TerminalLogica == string.Empty)
                {
                    qaeaxca.CaaErrVaria1 = "TERMINAL";
                    intQaeVal = 1;
                }
                if (qaeaxca.IdTransaccion == string.Empty)
                {
                    qaeaxca.CaaErrVaria1 = "TRANSACCION";
                    intQaeVal = 1;
                }

                if (intQaeVal != 1)
                {
                    sbResult.AppendLine();
                    sbResult.Append("Inicia SP QA_VALIDACIONES");
                    sbResult.AppendLine();
                    qaeaxca.IdTask = 3;
                    int qaespVal = call_spVal(conn, qaeaxca.Canal, qaeaxca.TerminalLogica, qaeaxca.IdTransaccion);
                    if (intQaeVal < 1)
                        intQaeVal = qaespVal;

                }

                qaeaxca.IdTask = 4;
                if (intQaeVal < 1)
                    seralize(qaeaxca.ProgramaTx, conn);

                switch (intQaeVal)
                {
                    case 1:
                        qaeaxca.CaaSwErrcod = "QAE0040";
                        sbResult.Append(qaeaxca.CaaErrVaria1);
                        qaeaxca.ServiceResponse = "2";
                        break;
                    case 2:
                        qaeaxca.CaaSwErrcod = "QCE0015";
                        qaeaxca.CaaErrVaria1 = qaeaxca.IdTransaccion;
                        qaeaxca.ServiceResponse = "2";
                        break;
                    case 10:
                        qaeaxca.CaaSwErrcod = "QAE1011";
                        qaeaxca.CaaErrVaria1 = qaeaxca.UsuarioDeTerminal;
                        qaeaxca.ServiceResponse = "2";
                        break;
                    case 100:
                        qaeaxca.CaaSwErrcod = "QCE0007";
                        qaeaxca.CaaErrVaria1 = qaeaxca.TerminalLogica;
                        break;
                    case 101:
                        qaeaxca.CaaSwErrcod = "QCE0007";
                        qaeaxca.CaaErrVaria1 = qaeaxca.TerminalLogica;
                        break;
                    case 102:
                        qaeaxca.CaaSwErrcod = "QCE0007";
                        qaeaxca.CaaErrVaria1 = qaeaxca.TerminalLogica;
                        break;
                    default:
                        break;
                }
            }

            getHost();
            sbResult.AppendLine();
            sbResult.AppendFormat("Se obtuvo el CICS: [{0}]", qaeaxca.Cics);
            sbResult.AppendLine();


            sbResult.AppendLine();
            sbResult.Append("Inicia Generación PS9 de Salida");
            sbResult.AppendLine();

            qaeaxca.IdTask = 5;
            Stopwatch timer1 = new Stopwatch();
            timer1.Start();
            //PS9 = qaecca.buildSalida().ToString();
            //SE LLAMA A LA NUEVA DLL QUE CREA LA SALIDA Y SE ENVIA EL OBJETO CON LA INFORMACION ALMACENADA
            strPS9 = qa1axop9.ConstruirSalida(qaeaxca).ToString();
            sbResult.AppendFormat("Cadena PS9: [{0}]", strPS9);
            sbResult.AppendLine();
            timer1.Stop();
            long timeElapsed1 = timer1.ElapsedMilliseconds;
            sbResult.AppendFormat("Termina Generación PS9 de Salida: [{0} ms.]", timeElapsed1);
            sbResult.AppendLine();

            if (Convert.ToInt32(qaeaxca.OutputLength) < 26)
            {
                sbResult.AppendFormat("Tuve un error Al Generar la PS9: Cadena Incompleta");
                sbResult.AppendLine();
                qaeaxca.ServiceResponse = "5";
                qaeaxca.ProcessControl = "A";
                if (intQaeVal != 99)
                {
                    intQaeVal = 99;
                    getErrMsg("QA1XOP9", "QA_VALIDACIONES", "Cadena Incompleta");
                }

            }

            sbResult.AppendLine();
            sbResult.Append("Inicia Registro en Tablas - SP");
            sbResult.AppendLine();


            timer.Stop();
            long timeElapsed = timer.ElapsedMilliseconds;
            qaeaxca.TotalTime = (double)timeElapsed / 1000;
            qaeaxca.IdTask = 6;

            if (intQaeVal != 99)
                call_spTtcl(conn, qaeaxca);
            else
                call_spLog(conn, qaeaxca, strErrMsg);

            sbResult.AppendLine();
            strResult = sbResult.ToString();
            if (intQaeVal == 99)
                EventLog.WriteEntry("QA1XENT", strResult, EventLogEntryType.Error);
            else if (qaeaxca.TotalTime > 1.5)
                EventLog.WriteEntry("QA1XENT", strResult, EventLogEntryType.Warning);
            else if ((int)Registry.GetValue("HKEY_LOCAL_MACHINE\\SOFTWARE\\Wow6432Node\\Arquitectura\\QA1AXENT", "MustWriteAllEvents", 0) == 1)
            //else if ((int)Registry.GetValue("HKEY_LOCAL_MACHINE\\SOFTWARE\\Arquitectura\\QA1AXENT", "MustWriteAllEvents", 0) == 1)
                EventLog.WriteEntry("QA1XENT", strResult, EventLogEntryType.Information);
            return strPS9;
        }
        /// <summary>
        /// METODO QUE LLAMA EL STOREDPROCEDURE DE VALIDACIONES
        /// </summary>
        /// <param name="conn">OBJETO DE LA CONEXION DE LA BASE DE DATOS</param>
        /// <param name="canal">CANAL DE LA TRANSACCION</param>
        /// <param name="terminal">TERMINAL DE LA TRANSACCION</param>
        /// <param name="Id_TX" >ID DE TRANSACCION</param>
        /// <returns>RETORNA UN CODIGO DE VALIDACION </returns>
        public int call_spVal(OdbcConnection conn, string canal, string terminal, string Id_TX)
        {
            int resp = 0;
            this.sbResult.Append("Entrada:");
            this.sbResult.AppendLine();
            this.sbResult.AppendFormat("Terminal: [{0}]", terminal);
            this.sbResult.AppendLine();
            this.sbResult.AppendFormat("Transaccion: [{0}]", Id_TX);
            this.sbResult.AppendLine();
            this.sbResult.AppendFormat("Canal: [{0}]", canal);
            this.sbResult.AppendLine();
            OdbcCommand cmd = new OdbcCommand();

            try
            {
                Stopwatch timer = new Stopwatch();
                timer.Start();

                cmd.CommandText = "{call MAZP.QA_VALIDACIONES(?,?,?,?,?,?)}";
                cmd.CommandType = CommandType.StoredProcedure;

                cmd.Parameters.Add("@canal_cod", OdbcType.VarChar, 2).Value = canal;
                cmd.Parameters.Add("@terminal", OdbcType.VarChar, 4).Value = terminal;
                cmd.Parameters.Add("@Id_tx", OdbcType.VarChar, 4).Value = Id_TX;
                cmd.Parameters.Add("@DATOS", OdbcType.VarChar, 3000).Direction = ParameterDirection.Output;
                cmd.Parameters.Add("@MENSAJE", OdbcType.VarChar, 70).Direction = ParameterDirection.Output;
                cmd.Parameters.Add("@return", OdbcType.Int).Direction = ParameterDirection.Output;


                ///SE AGREGA ESTE PARAMETRO PARA IDENTIFICAR EL TIPO DE TX
                //cmd.Parameter.Add("@tipo_tx", OdbcType.Varchar).Direction = ParameterDirection.Output;

                cmd.Connection = conn;

                cmd.ExecuteNonQuery();

                this.sbResult.Append("Salida:");
                this.sbResult.AppendLine();
                strRsp = cmd.Parameters["@DATOS"].Value.ToString();

                ///SE AGREGA ESTA CONDICION PARA SABER EL TIPO DE TRANSACCION QUE ES
                //this.qaecca.TipoTX = Convert.ToChar(cmd.Parameters["@tipo_tx"].Value.ToString());

                sbResult.AppendFormat("Datos: [{0}] ", strRsp);
                sbResult.AppendLine();
                object mensaje = cmd.Parameters["@MENSAJE"].Value;
                sbResult.AppendFormat("Mensaje: [{0}] ", mensaje);
                sbResult.AppendLine();
                resp = (int)cmd.Parameters["@return"].Value;
                //Result.AppendFormat("Return Value: [{0}] ", cmd.Parameters["@return"].Value);
                sbResult.AppendFormat("Return Value: [{0}] ", resp);
                sbResult.AppendLine();

                if (strRsp.Length > 5)
                {
                    getData(strRsp);
                }
                else
                    qaeaxca.ServiceResponse = "2";

                if (resp == 1)
                {
                    if (mensaje.ToString().Contains("terminal"))
                        qaeaxca.CaaErrVaria1 = "TERMINAL";
                    if (mensaje.ToString().Contains("canal"))
                        qaeaxca.CaaErrVaria1 = "CANAL";
                    if (mensaje.ToString().Contains("transaccion"))
                        qaeaxca.CaaErrVaria1 = "TRANSACCION";
                }

                timer.Stop();
                long timeElapsed = timer.ElapsedMilliseconds;
                this.sbResult.AppendFormat("Termina SP QA_VALIDACIONES: [{0} ms.]  ", timeElapsed);
                this.sbResult.AppendLine();

            }
            catch (Exception e)
            {
                this.sbResult.AppendFormat("Tuve un error en el SP: {0}", e.ToString());
                this.sbResult.AppendLine();
                qaeaxca.ServiceResponse = "5";
                qaeaxca.ProcessControl = "A";
                intQaeVal = 99;
                getErrMsg("QA7CVAQ", "QA_VALIDACIONES", e.ToString());
            }
            return resp;
        }

        /// <summary>
        /// METODO QUE LLAMA EL METODO PRINCIPAL DE LA TRANSACCION (ExecuteTx)
        /// </summary>
        /// <param name="Dll_TX">NOMBRE DE LA TRANSACCION</param>
        /// <param name="conn">OBJETO QUE TIENE ASIGNADO LA CONEXION A LA BASE DE DATOS</param>
        void EjecutarTx(string Dll_TX, OdbcConnection conn)
        {
            StringBuilder dll = new StringBuilder();
            Stopwatch timer = new Stopwatch();
            timer.Start();
            this.sbResult.AppendLine();
            this.sbResult.Append("Inicia Llamado a Componente de transacción");
            this.sbResult.AppendLine();
            this.sbResult.AppendFormat("Componente: [{0}]", (Dll_TX + ".dll"));
            this.sbResult.AppendLine();
            this.sbResult.AppendFormat("Entrada: [{0}]", qaeaxca.MensajeEntrada);
            this.sbResult.AppendLine();
            //STRING BUILDER QUE DESCARTA LA C Y AGREGA UN AX

            dll = dll.Append(Dll_TX.Substring(0, 3)).Append("X").Append(Dll_TX.Substring(4, 4));

            string DLL0 = @"C:\AlnovaEXE\" + dll + ".dll";
            this.sbResult.AppendFormat("DLL0: [{0}]", DLL0, "\n");

            try
            {
                string rspTX = string.Empty;
                //Assembly assembly0 = Assembly.LoadFrom(DLL0);
                QAEXCA.QAEXCA result = new QAEXCA.QAEXCA();


                //Console.WriteLine("\n QACCA :{0} \n", qaexca.Result);

                //Console.WriteLine("Salida: agregada para ver lo que tiene la cadena: {0} \n", rspTX);

                Console.WriteLine("Se va a ejecutar el componente");
                result = _componente.ExecuteTx(conn, qaeaxca);
                //Console.WriteLine("Se debio ejecutar el componente");
                //Console.ReadLine();
                qaeaxca = result;
                rspTX = qaeaxca.Result;

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
                    //Console.WriteLine("LLamado al programa: {0}-{1}", Dll_TX, rspTX);
                    //Console.ReadKey();
                    qaeaxca.ServiceResponse = "5";
                    qaeaxca.ProcessControl = "A";
                    intQaeVal = 99;
                    Console.WriteLine(Dll_TX, Dll_TX, rspTX);
                    //Console.ReadKey();
                    //getErrMsg(Dll_TX, Dll_TX, rspTX);
                }
                else
                {
                    //Console.WriteLine("Salida: [{0}]", result.ToString());
                    //Console.ReadKey();
                }


                timer.Stop();
                long timeElapsed = timer.ElapsedMilliseconds;
                Console.WriteLine("\n Termina Llamado a Componente de transacción: [{0} ms.] \n", timeElapsed);
                /*
                string rspTX = string.Empty;
                //Assembly assembly0 = Assembly.LoadFrom(DLL0);
                QAEXCA.QAEXCA result = new QAEXCA.QAEXCA();
                foreach (Type type in assembly0.GetExportedTypes())
                {
                    object c = Activator.CreateInstance(type);
                    //result = type.InvokeMember("executetx", BindingFlags.InvokeMethod, null, c, new object[] { conn, "0127", qaecca.MensajeEntrada, qaecca.Canal });
                   // result = (QAEXCA.QAEXCA)type.InvokeMember("ExecuteTx", BindingFlags.InvokeMethod, null, c, new object[] { conn, qaeaxca });
                    result = _componente.ExecuteTx(conn, qaeaxca);
                    qaeaxca = result;
                    rspTX = qaeaxca.Result;

                    this.sbResult.AppendFormat("QACCA :{0}", qaeaxca.Result);
                    this.sbResult.AppendLine();
                    this.sbResult.AppendFormat("Salida: agregada para ver lo que tiene la cadena: {0}", rspTX);

                    //Ingresar valores a la Commarea
                    //qaecca.CaaSwCodWA1 = commarea[0];
                    //qaecca.CaaWarn1Varia1 = commarea[1];
                    //qaecca.CaaWarn1Varia2 = commarea[2];
                    //qaecca.CaaTbScrDocu1 = commarea[3];
                    //qaecca.CaaSwErrcod = commarea[4];
                    //qaecca.CaaErrVaria1 = commarea[5];
                    //qaecca.CaaErrVaria2 = commarea[6];
                    //qaecca.EattDtaBffDC = commarea[7];
                    //qaecca.CaaSwCodWA2 = commarea[8];
                    //qaecca.CaaWarn2Varia1 = commarea[9];
                    //qaecca.CaaWarn2Varia2 = commarea[10];
                    //rspTX = commarea[11];

                }
                if (rspTX.Contains("ERROR"))
                {
                    sbResult.AppendFormat("LLamado al programa: {0}-{1}", Dll_TX, rspTX);
                    qaeaxca.ServiceResponse = "5";
                    qaeaxca.ProcessControl = "A";
                    intQaeVal = 99;
                    getErrMsg(Dll_TX, Dll_TX, rspTX);
                }
                else
                {
                    sbResult.AppendFormat("Salida: [{0}]", result.ToString());
                }


                timer.Stop();
                long timeElapsed = timer.ElapsedMilliseconds;
                this.sbResult.AppendLine();
                this.sbResult.AppendFormat("Termina Llamado a Componente de transacción: [{0} ms.]", timeElapsed);
                this.sbResult.AppendLine();*/

            }
            catch (Exception e)
            {
                sbResult.AppendFormat("Resultado del LLamado al programa dummy: {0}-{1}", DLL0, e.ToString());
                qaeaxca.ServiceResponse = "5";
                qaeaxca.ProcessControl = "A";
                intQaeVal = 99;
                getErrMsg(Dll_TX, Dll_TX, e.ToString());
            }

        }

        /// <summary>
        /// LLAMA AL METODO ExecuteTx
        /// </summary>
        /// <param name="Dll_TX"></param>
        /// <param name="conn"></param>
        public void seralize(string Dll_TX, OdbcConnection conn)
        {
            try
            {
                /*string serialized;
                using (MemoryStream stream = new MemoryStream())
                {
                    BinaryFormatter formatter = new BinaryFormatter();
                    formatter.Serialize(stream, commarea);
                    serialized = Convert.ToBase64String(stream.GetBuffer());
                }*/

                //EjecutarTx(Dll_TX + "_NET", conn);
                EjecutarTx(Dll_TX , conn);


            }
            catch (Exception e)
            {
                sbResult.AppendFormat("Error al serializar: {0}", e.ToString());
                qaeaxca.ServiceResponse = "5";
                qaeaxca.ProcessControl = "A";
                intQaeVal = 99;
                getErrMsg(Dll_TX, Dll_TX, e.ToString());
            }


        }

        /// <summary>
        /// VALIDA QUE EL USUARIO TENGA PERMISOS PARA MANIPULAR LA TRANSACCION
        /// </summary>
        /// <param name="transactionName">NOMBRE DE LA TRANSACCION</param>
        /// <param name="userName">NOMBRE DEL USUARIO</param>
        void call_IsAllow(string transactionName, string userName)
        {

            try
            {
                this.sbResult.Append("Entrada:");
                this.sbResult.AppendLine();
                this.sbResult.AppendFormat("Usuario: [{0}]", userName);
                this.sbResult.AppendLine();
                this.sbResult.AppendFormat("Transaccion: [{0}]", transactionName);
                this.sbResult.AppendLine();
                string domainControllerName = "DC=produccion,DC=bancoazteca";
                string transactionOU = "OU=Resources";
                string userOU = "OU=UsuariosAlnova";
                transactionName = "CN=" + transactionName;
                userName = "CN=" + userName;
                Stopwatch timer = new Stopwatch();
                timer.Start();

                //int valueR = IsAllowToRes(domainControllerName, transactionOU, userOU, transactionName, userName);
                int valueR = 1;

                timer.Stop();
                long timeElapsed = timer.ElapsedMilliseconds;
                if (valueR == 1)
                {
                    this.sbResult.Append("Salida: [Usuario Autorizado]" + valueR);
                    this.sbResult.AppendLine();
                }
                else
                {
                    intQaeVal = 10;
                    this.sbResult.Append("Salida: [Usuario No Autorizado]" + valueR);
                    this.sbResult.AppendLine();
                }
                this.sbResult.AppendFormat("Termina IsAllowToRes: [{0} ms.]", timeElapsed);
                this.sbResult.AppendLine();
            }
            catch (Exception e)
            {
                sbResult.AppendFormat("Error al serializar: {0}", e.ToString());
                qaeaxca.ServiceResponse = "5";
                qaeaxca.ProcessControl = "A";
                intQaeVal = 99;
                getErrMsg("QA7CSEG", "IsAllowToRes", e.ToString());
            }

        }
        /// <summary>
        /// LLAMA AL SP DE TECLEOS
        /// </summary>
        /// <param name="conn">OBJETO DE CONEXION DE LA BASE DE DATOS</param>
        /// <param name="commarea">OBJETO DE LA CLASE QAEAXCA</param>
        public void call_spTtcl(OdbcConnection conn, QAEXCA.QAEXCA commarea)
        {
            OdbcCommand cmd = new OdbcCommand();
            try
            {
                Stopwatch timer = new Stopwatch();
                timer.Start();

                //odbc

                cmd.CommandText = "{call MAZP.QA_TECLEOS(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
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

                cmd.Connection = conn;
                cmd.ExecuteNonQuery();
                sbResult.AppendFormat("Salida: [{0}] ", cmd.Parameters["@ERRORES"].Value);
                sbResult.AppendLine();
                timer.Stop();
                long timeElapsed = timer.ElapsedMilliseconds;
                this.sbResult.AppendFormat("Termina Registro en Tecleos - SP QA_TECLEOS: [{0} ms.]  ", timeElapsed);
            }
            catch (Exception e)
            {
                this.sbResult.AppendFormat("Tuve un error en el SP: {0}", e.ToString());
                this.sbResult.AppendLine();
                qaeaxca.ServiceResponse = "5";
                qaeaxca.ProcessControl = "A";
                intQaeVal = 99;
                getErrMsg("QA7CITCL", "QA_TECLEOS", e.ToString());
            }


        }

        /// <summary>
        /// LLAMA AL SP QA_QADTLOG
        /// </summary>
        /// <param name="conn">OBJETO DE LA CONEXION BASE DE DATOS</param>
        /// <param name="commarea">OBJETO DE LA CLASE QAEAXCA</param>
        /// <param name="excMsg">STRING QUE TIENE ASIGNADO EL ERROR</param>
        public void call_spLog(OdbcConnection conn, QAEXCA.QAEXCA commarea, string excMsg)
        {
            OdbcCommand cmd = new OdbcCommand();
            try
            {

                Stopwatch timer = new Stopwatch();
                timer.Start();

                cmd.CommandText = "{call MAZP.QA_QADTLOG(?,?,?,?,?,?,?,?,?)}";
                cmd.CommandType = CommandType.StoredProcedure;

                cmd.Parameters.Add("@Sys_Id", OdbcType.VarChar, 4).Value = commarea.Cics;
                cmd.Parameters.Add("@Num_task", OdbcType.Numeric).Value = commarea.IdTask;
                cmd.Parameters.Add("@Cod_trns", OdbcType.VarChar, 4).Value = commarea.IdTransaccion;
                cmd.Parameters.Add("@Nom_TRM", OdbcType.VarChar, 8).Value = commarea.TerminalFisica;
                cmd.Parameters.Add("@User", OdbcType.VarChar, 8).Value = commarea.UsuarioDeTerminal;
                cmd.Parameters.Add("@Nom_PGM", OdbcType.VarChar, 8).Value = commarea.ProgramaTx;
                cmd.Parameters.Add("@Exec_MSG", OdbcType.VarChar, 150).Value = excMsg;
                cmd.Parameters.Add("@Mensaje_ret", OdbcType.VarChar, 50).Direction = ParameterDirection.Output;
                cmd.Parameters.Add("@return", OdbcType.Int, 3).Direction = ParameterDirection.Output;



                cmd.Connection = conn;
                cmd.ExecuteNonQuery();
                sbResult.AppendFormat("Salida: [{0}] ", cmd.Parameters["@Mensaje_ret"].Value);
                sbResult.AppendLine();
                timer.Stop();
                long timeElapsed = timer.ElapsedMilliseconds;
                this.sbResult.AppendFormat("Termina Registro en QADTLOG - SP Insert Tlog: [{0} ms.]  ", timeElapsed);
            }
            catch (Exception e)
            {
                this.sbResult.AppendFormat("Tuve un error en el SP: {0}", e.ToString());
                this.sbResult.AppendLine();
                qaeaxca.ServiceResponse = "5";
                qaeaxca.ProcessControl = "A";
                intQaeVal = 99;
                getErrMsg("QA1CENT", "QA_QADTLOG", e.ToString());
            }


        }
        /// <summary>
        /// OBTIENE UN MENSAJE DE ERROR
        /// </summary>
        /// <param name="PGM"></param>
        /// <param name="OBJ"></param>
        /// <param name="ERR"></param>
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

            strErrMsg = buildErrorLog.ToString();
        }
        /// <summary>
        /// OBTIENE UNA LISTA DE DATOS DEL SP DE VALIDACIONES
        /// </summary>
        /// <param name="s"></param>
        void getData(String s)
        {
            qaeaxca.ServiceResponse = "1";
            qaeaxca.ProcessControl = "000     ";

            char[] searchString = { ',', '|' };
            String[] listData = s.Split(searchString);
            qaeaxca.PanelFormat = listData[2];
            qaeaxca.TipoTX = listData[1];
            qaeaxca.ProgramaTx = listData[5];
            qaeaxca.Modulo = listData[6];
            qaeaxca.Centro = listData[8];
            qaeaxca.DesInpCpy = listData[10];
        }

        /// <summary>
        /// 
        /// </summary>
        void getHost()
        {
            try
            {
                // Get the local computer host name.
                qaeaxca.Cics = Dns.GetHostName();
                qaeaxca.Cics = qaeaxca.Cics.Substring(qaeaxca.Cics.Length - 4);
            }
            catch (Exception e)
            {

                this.sbResult.AppendFormat("Tuve un error en el GetHost: {0}", e.ToString());
                this.sbResult.AppendLine();
                qaeaxca.ServiceResponse = "5";
                qaeaxca.ProcessControl = "A";
                intQaeVal = 99;
                getErrMsg("QA1CENT", "GetHost", e.ToString());
            }

        }

        [DllImport("IsAllowToRes.dll",
            EntryPoint = "IsAllowToRes", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        public static extern int IsAllowToRes(string s1, string s2, string s3, string s4, string s5);
    }
}
