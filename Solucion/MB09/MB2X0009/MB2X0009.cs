﻿using QAEXCA;
using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Odbc;
using System.Text;
using System.Text.RegularExpressions;

namespace MB2X0009
{
    public class MB2X0009
    {

        #region Transaction LOG
        //  *----------------------------------------------------------------*
        //  *          L O G    D E    M O D I F I C A C I O N E S           *
        //  *----------------------------------------------------------------*
        //  *                                                                *
        //  * MARCA         AUTOR   FECHA    DESCRIPCION                     *
        //  * -----------  -------  -------  ------------------------------- *
        //  * @                                                              *
        //  *                                                                *
        //  *                                                                *
        //  *                                                                *
        //  *----------------------------------------------------------------*
        //  *----------------------------------------------------------------*
        //  * PE -                                                           *
        //  *                                                                *
        //  *                                                                *
        //  *                                                                *
        //  *----------------------------------------------------------------*
        #endregion

        #region Local Variables
        //String Builder general
        StringBuilder stringSbTodo = new StringBuilder();
        //String Builder salida
        StringBuilder stringSbSalida = new StringBuilder();
        //Objeto de entrada.
        static MBNE0009.MBNE0009 mbne0009 = new MBNE0009.MBNE0009();

        //!! SIN OBJETO DE SALIDA.
        //Objeto de la QAEXCA (Commarea)
        private QAEXCA.QAEXCA qaexca = new QAEXCA.QAEXCA();
        //Se le asigna el nombre del copy de salida que te arroja el cobol
        string outputCopyCode = "MPNC0251";
        string filler = "FILLERFILLER";
        #endregion

        public QAEXCA.QAEXCA ExecuteTx(OdbcConnection conn, QAEXCA.QAEXCA qaexca)
        {
            Console.WriteLine("Imprimiendo ****");
            Console.WriteLine("GetInputMessage antes de llamar metodo " + qaexca.getStringValues());

            Console.WriteLine("Inicia el MB2X009 ---------------------------");
            System.Diagnostics.Stopwatch timer = new System.Diagnostics.Stopwatch();
            timer.Start();

            GetInputMessage(qaexca);

            if (ValidateInputMessage()) ExecuteStoreProcedure(conn);
            //Se le asigna el mensaje de error, en caso de que tenga uno
            this.qaexca.Result = Convert.ToString(stringSbSalida);
            //Se retorna el objeto de la commarea

            timer.Stop();
            long timeElapsed = timer.ElapsedMilliseconds;
            Console.WriteLine("\n Termina proceso en mp2x025: [{0} ms.] ---------------------\n", timeElapsed);
            return this.qaexca;
        }


        public void GetInputMessage(QAEXCA.QAEXCA qaexca)
        {
            Console.WriteLine("GetInputMessage --> ");
            Console.WriteLine("GetInputMessage --> " + qaexca.getStringValues());

            this.qaexca = qaexca;
            Console.WriteLine(" <--- MENSAJE DE ENTRADA ANTES MATCH THIS --> " + this.qaexca.MensajeEntrada);

            String ME = qaexca.MensajeEntrada;
            Console.WriteLine(" <--- MENSAJE DE ENTRADA ANTES MATCH --> " + ME);

            string regexPattern = @"\<ME\>(?<FILLER>\w{17}\s{2})"
                                + @"A(?<BDMID>\w{0,40})\s{2}"
                                + @"A(?<NUMTARJ>\w{16})\s{2}"
                                + @"A(?<NUMCUEN>\w{14})\s{2}"
                                + @"A(?<FECHINI>\w{10})\s{2}"
                                + @"A(?<FECHFIN>\w{10})\s{2}"
                                + @"A(?<NUMECEL>\w{15})\s{2}"
                                + @"A(?<IDCELUL>\w{13})\s{2}"
                                + @"A(?<<LATITUD>\w{11})\s{2}"
                                + @"A(?<LONGITD>\w{11})\s{2}"
                                + @"A(?<CONTREG>\w{3})\s{2}"
                                + @"A(?<DATPAG>\w{22}))";

            Console.WriteLine("regexPattern --> " + regexPattern);
            var match = Regex.Match(ME, regexPattern);
            Console.WriteLine(" <--- MENSAJE DE ENTRADA DESPUES MATCH --> " + this.qaexca.MensajeEntrada);

            if (match.Success)
            {
                mbne0009.BDMID = match.Groups["BDMID"].Value.Trim();
                Console.WriteLine(" MATCH SUCCES " + qaexca.MensajeEntrada);
            }
            else
            {
                Console.WriteLine(" MATCH NO SUCCES " + qaexca.MensajeEntrada);
            }

            /*
            //Console.WriteLine(qaexca.getStringValues());
            try
            {

                //Se guarda el objeto de la commarea al objeto que instanciamos
                this.qaexca = qaexca;
                //Agregamos una expresión regular para validar despues la cadena

                string regexPattern = @"\<ME\>(?<FILLER>\w{17}\s{2})"
                                + @"A(?<BDMID>\w{40})\s{2}"
                                + @"A(?<NUMTARJ>\w{16})\s{2}"
                                + @"A(?<NUMCUEN>\w{14})\s{2}"
                                + @"A(?<FECHINI>\w{10})\s{2}"
                                + @"A(?<FECHFIN>\w{10})\s{2}"
                                + @"A(?<NUMECEL>\w{15})\s{2}"
                                + @"A(?<IDCELUL>\w{13})\s{2}"
                                + @"A(?<<LATITUD>\w{11})\s{2}"
                                + @"A(?<LONGITD>\w{11})\s{2}"
                                + @"A(?<CONTREG>\w{3})\s{2}"
                                + @"A(?<DATPAG>\w{22}))"; 


                var match = Regex.Match(this.qaexca.MensajeEntrada, regexPattern);
                Console.WriteLine(qaexca.MensajeEntrada);
                
                if (match.Success)
                {
                    mbne0009.BDMID = match.Groups["BDMID"].Value.Trim();
                    mbne0009.NUMTARJ = match.Groups["NUMTARJ"].Value.Trim();
                    mbne0009.NUMCUEN = match.Groups["NUMCUEN"].Value.Trim();
                    mbne0009.NUMECEL = match.Groups["NUMECEL"].Value.Trim();
                    mbne0009.BDMID = match.Groups["NUMECEL"].Value.Trim();
                    Console.WriteLine("MATCH CORRECTO");

                    if ((IsNullOrWhiteSpace(mbne0009.NUMTARJ) || !Regex.IsMatch(mbne0009.NUMTARJ, @"^[0-9]+$")) || ( IsNullOrWhiteSpace(mbne0009.NUMCUEN) || !Regex.IsMatch(mbne0009.NUMCUEN, @"^[0-9]+$") ))
                    {
                        Console.WriteLine("ERROR CADENA");
                    }
                    else
                    {
                        Console.WriteLine("CORRECTO CADENA");
                    }
                   
                }
                else
                {
                    Console.WriteLine("MATCH CORRECTO");
                }
            }
            catch (Exception e)
            {
                stringSbSalida.AppendFormat($"ERROR AL DESERIALIZAR MENSAJE DE ENTRADA: {0}", Convert.ToString(e.Message.ToString()));
                stringSbSalida.AppendLine();
                throw;
            }
            */


        }

        public bool ValidateInputMessage()
        {/*
            if (IsNullOrWhiteSpace(merm770.CRDTYP) || IsNullOrWhiteSpace(merm770.CRDBIN) || IsNullOrWhiteSpace(merm770.FFC))
            {
                return true;
            }
            try
            {
                //Si no existe ningun error, procede a ejecutar el procedimiento almacenado
                return qaexca.CaaSwErrcod == "" || qaexca.CaaSwErrcod == null || qaexca.CaaSwErrcod == string.Empty;
            }
            catch (Exception ex)
            {
                stringSbSalida.AppendFormat($"ERROR AL VALIDAR MENSAJE DE ENTRADA: {0}", ex.Message.ToString());
                stringSbSalida.AppendLine();
                throw;
            }*/
            return true;
        }

        public void ExecuteStoreProcedure(OdbcConnection conn)
        {
            string entity = qaexca.Ent == string.Empty || qaexca.Ent == null || IsNullOrWhiteSpace(qaexca.Ent) ? "0127" : qaexca.Ent;
            string lngKey = qaexca.Idioma == string.Empty || qaexca.Idioma == null || IsNullOrWhiteSpace(qaexca.Idioma) ? "E" : qaexca.Idioma;

            //string query = "MAZP.SP_M025_MP2X0025";
            try
            {
                /*
                OdbcCommand cmd = new OdbcCommand("{CALL MAZP.SP_ME77_ME2X7700 (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}" +
                    "", conn);
                //Inicializar el comando
                cmd.CommandType = CommandType.StoredProcedure;

                //Parametros de NECESARIOS
                cmd.Parameters.Add("@IP_OPTION", OdbcType.VarChar, 1).Value = 1;
                cmd.Parameters.Add("@IP_CODE_ENTITY", OdbcType.VarChar, 4).Value = entity;
             
                cmd.Parameters.Add("@IP_LNGKEY", OdbcType.VarChar, 1).Value = qaexca.Idioma;
                cmd.Parameters.Add("@IP_TERMINAL", OdbcType.VarChar, 4).Value = qaexca.TerminalFisica;
                cmd.Parameters.Add("@IP_BRANCH", OdbcType.VarChar, 4).Value = qaexca.Centro;
                cmd.Parameters.Add("@IP_DATE_OPE", OdbcType.VarChar, 8).Value = DateTime.Now.ToString("yyyy-MM-dd");
                //Parametros de entrada OPCIONALES
                //Dependen de la opcion
                if (true)
                {
                    cmd.Parameters.Add("@IP_ACU_FREE1", OdbcType.Decimal, 2).Value = null;
                    cmd.Parameters.Add("@IP_ACU_FREE2", OdbcType.Decimal, 2).Value = null;
                    cmd.Parameters.Add("@IP_ACU_FREE3", OdbcType.Decimal, 2).Value = null;
                    cmd.Parameters.Add("@IP_ACU_FREE4", OdbcType.Decimal, 2).Value = null;
                }

                //Parametros de salida
                cmd.Parameters.Add("@OP_NL_ERROR", OdbcType.VarChar, 3).Direction = ParameterDirection.Output;


                cmd.ExecuteNonQuery();
                /*
                DataTable datos = new DataTable();
                OdbcDataAdapter adapter = new OdbcDataAdapter(cmd);
                adapter.Fill(datos);
                */
                /*
                Console.WriteLine("Respuesta del servidor: {0}", cmd.Parameters["@OP_NL_ERROR"].Value);
                SetOutputVariables(cmd);
                
                */
            }
            catch (Exception e)
            {
                stringSbTodo.Append("Error al ejecutar SP: " + e.Message.ToString());
                throw;
            }


        }

        public void SetOutputVariables(OdbcCommand cmd)
        {

            try
            {
                int nlError = Convert.ToInt32(cmd.Parameters["@OP_NL_ERROR"].Value);

                switch (nlError)
                {
                    case 0:
                        break;
                    case 1:
                        break;
                    case 100:
                        //No encontro datos
                        qaexca.CaaSwErrcod = "MCE0012";
                        break;
                    default:
                        //Error al ejecutar el SP.
                        qaexca.CaaSwErrcod = "MCDT147";
                        break;
                }
            }
            catch (Exception e)
            {

            }

        }

        public void SetOutputMessage()
        {
            try
            {
                //<OH>2610        00469</OH><AV>MCA08050</AV><DE>1P 00      E</DE><OC>B1MPNC0310             4155710200675220 00002104741844 Hernandez           Cruz                Elsa                 6 TDDC3                                        0.00 2022-05-05  14.40  26/05               0.00               0.00               0.00               0.00                            S N VIGENTE              N                               S N</OC>
                //StringBuilder que concatenara la cadena de salida
                StringBuilder outputMessagePS9 = new StringBuilder();
                //Se concatena los valores para formar la cadena de salida
                outputMessagePS9.Append("B1")
                //Se agrega el nombre de nuestro copy de salida
                .Append(outputCopyCode)
                //.Append(new string(' ', 2))
                .Append(filler)
                .Append($"{new string(' ', 2)}A")
                ;
                //Console.WriteLine(Convert.ToString(outputMessagePS9));
                //Se guarda la cadena en la variable "EattDtaBffDC"
                qaexca.EattDtaBffDC = Convert.ToString(outputMessagePS9);
                /*
                    DESPROD
                    IDSUPRO
                    DESUPRO
                    SALDTOT
                    STAPLAN
                    PAGOPUN
                    PAGONOR
                    PAGOSUG
                    PAGOLIQ
                    PAGOREQ
                    PAGOMIN
                    PAGONGI
                    FECPROX
                    STAPRIO
                    NUMTARJ
                    SALDDIS
                    STATARJ
                    FECCORT
                 */
            }
            catch (Exception ex)
            {
                stringSbTodo.AppendFormat($"ERROR EN LA CREACION DE LA CADENA DE SALIDA (PS9): " + ex.Message.ToString());
                stringSbTodo.AppendLine();
                throw;
            }
        }

        public StringBuilder FillString(string value, int valueLength, int fillType)
        {
            //El caracter puede variar dependiendo del atributo a pasar
            char fillValue = ' ';
            //Es la cadena que se regresará
            StringBuilder newValue = new StringBuilder();
            //Se valida que atributo se esta consultando para despues saber con que se llenara
            switch (fillType)
            {
                case 1:
                    //Se llenará la cadena con ceros
                    fillValue = '0';
                    break;
            }
            //Si la cadena es nula
            if (value == null)
            {
                value = " ";
            }
            //Se valida si el numero de caracteres coincide con el numero que deberia de tener la cadena
            if (value.Length == valueLength)
            {
                newValue.Append(value);
            }
            else if (value.Length < valueLength)   //Si es menor, se llena de espacios
            {
                newValue.Append(value.PadRight(valueLength, fillValue));

            }   //Si es mayo el numero de caracteres se corta
            else if (value.Length > valueLength)
            {
                newValue.Append(value.Substring(0, valueLength));
            }
            //Retorna la cadena modificada
            return newValue;
        }

        public bool IsNullOrWhiteSpace(string value)
        {
            if (value != null)
            {
                for (int i = 0; i < value.Length; i++)
                {
                    if (!char.IsWhiteSpace(value[i]))
                    {
                        return false;
                    }
                }
            }
            return true;
        }
        public bool validateInt(string a)
        {
            int salida;
            if (int.TryParse(a, out salida))
            {
                return true;
            }
            return false;
        }
    }
}
