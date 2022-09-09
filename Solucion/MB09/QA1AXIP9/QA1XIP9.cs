using System;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using System.Diagnostics;

namespace QA1XIP9
{
    [Serializable]
    public class QA1XIP9
    {
        /// <summary>
        /// METODO QUE RECIBE EL OBETO DE LA QAEAXCA CON LA CADENA DE ENTRADA CON LA QUE SE REALIZA LA VALIDACION
        /// </summary>
        /// <param name="input">CADENA DE ENTRADA</param>
        /// <param name="qaecca">OBJETO DE LA QAEAXCA</param>
        /// <returns></returns>
        public int ConstruirCadenaEntrada(string strInput, QAEXCA.QAEXCA qaeaxca)
        {
            try
            {
                string strPattern = @"\<IH\>(?<IdProtocolo>\d{2})(?<TerminalLogica>\w{0,8}\s{0,8})(?<TerminalFisica>\w{0,8}\s{0,8})(?<UsuarioDeTerminal>\w{0,8}|\w{0,8}\s{0,8})(?<NumeroSecuenciaPs9>\w{0,8}\s{0,8})(?<IdTransaccion>\w{0,8}\s{0,8})(?<LlaveDeFuncion>\w{0,2}\s{0,2})(?<TamañoMensajeEntrada>\d{5})(?<IndicadorCommit>\d{1})(?<TipoCabecera>\d{1})(?<TipoProceso>\w{1})(?<Canal>\d{2})(?<PreformateoDatos>\w{1})(?<Idioma>\w{1})\</IH\>(?<MensajeEntrada>\<ME\>.+)";
                var match = Regex.Match(strInput, strPattern);

                if (match.Success)
                {
                    qaeaxca.IdProtocolo = match.Groups["IdProtocolo"].Value.Trim();
                    qaeaxca.TerminalLogica = match.Groups["TerminalLogica"].Value.Trim();
                    qaeaxca.TerminalFisica = match.Groups["TerminalFisica"].Value.Trim();
                    qaeaxca.UsuarioDeTerminal = match.Groups["UsuarioDeTerminal"].Value.Trim();
                    qaeaxca.NumeroSecuenciaPs9 = match.Groups["NumeroSecuenciaPs9"].Value.Trim();
                    qaeaxca.IdTransaccion = match.Groups["IdTransaccion"].Value.Trim();
                    qaeaxca.LlaveDeFuncion = match.Groups["LlaveDeFuncion"].Value.Trim();
                    qaeaxca.TamañoMensajeEntrada = match.Groups["TamañoMensajeEntrada"].Value.Trim();
                    qaeaxca.IndicadorCommit = match.Groups["IndicadorCommit"].Value.Trim();
                    qaeaxca.TipoCabecera = match.Groups["TipoCabecera"].Value.Trim();
                    qaeaxca.TipoProceso = match.Groups["TipoProceso"].Value.Trim();
                    qaeaxca.Canal = match.Groups["Canal"].Value.Trim();
                    qaeaxca.PreformateoDatos = match.Groups["PreformateoDatos"].Value.Trim();
                    qaeaxca.Idioma = match.Groups["Idioma"].Value.Trim();
                    qaeaxca.MensajeEntrada = match.Groups["MensajeEntrada"].Value;
                    return -1;
                }
                else
                {
                    return 99;

                }

            }
            catch (Exception ex)
            {
                EventLog.WriteEntry("COMMAREA", "\nNo se pudieron asignar campos de la cadena PS09, favor de revisar la cadena.\n\n" + ex.Message, EventLogEntryType.Error);
                return 99;
            }

        }
    }
}
