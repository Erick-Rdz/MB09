using System;
using System.Diagnostics;
using System.Text;

namespace QAEXCA
{
    [Serializable]
    public class QAEXCA
    {
        /// <summary>
        /// STRING BUILDER QUE CONSTRUYE EL INPUT HEADER DE SALIDA
        /// </summary>
        public StringBuilder sbSalida = new StringBuilder();
        /// <summary>
        /// STRING BUILDER QUE CONSTRUYE EL OUTPUT HEADER DE SALIDA
        /// </summary>
        public StringBuilder sbHeader = new StringBuilder();

        /// <summary>
        /// _PanelFormat
        /// </summary>
        string _PanelFormat;
        public string PanelFormat
        {
            get { return _PanelFormat; }
            set { _PanelFormat = value; }
        }

        /// <summary>
        /// _TipoTX
        /// </summary>
        string _TipoTX;
        public string TipoTX
        {
            get { return _TipoTX; }
            set { _TipoTX = value; }
        }
        /// <summary>
        /// _IdTask
        /// </summary>
        int _IdTask = 0;
        public int IdTask
        {
            get { return _IdTask; }
            set { _IdTask = value; }
        }

        /// <summary>
        /// _IdProtocolo
        /// </summary>
        string _IdProtocolo = string.Empty;
        public string IdProtocolo
        {
            get { return _IdProtocolo; }
            set { _IdProtocolo = value; }
        }

        /// <summary>
        /// _terminalLogica
        /// </summary>
        string _terminalLogica = string.Empty;
        public string TerminalLogica
        {

            get { return _terminalLogica; }
            set { _terminalLogica = value; }
        }

        /// <summary>
        /// _terminalFisica
        /// </summary>
        string _terminalFisica = string.Empty;
        public string TerminalFisica
        {
            get { return _terminalFisica; }
            set { _terminalFisica = value; }
        }

        /// <summary>
        /// _usuarioDeTerminal
        /// </summary>
        string _usuarioDeTerminal = string.Empty;
        public string UsuarioDeTerminal
        {
            get { return _usuarioDeTerminal; }
            set { _usuarioDeTerminal = value; }
        }

        /// <summary>
        /// _numeroSecuenciaPs9
        /// </summary>
        string _numeroSecuenciaPs9 = string.Empty;
        public string NumeroSecuenciaPs9
        {
            get { return _numeroSecuenciaPs9; }
            set { _numeroSecuenciaPs9 = value; }
        }

        /// <summary>
        /// _idTransaccion
        /// </summary>
        string _idTransaccion = string.Empty;
        public string IdTransaccion
        {
            get { return _idTransaccion; }
            set { _idTransaccion = value; }
        }

        /// <summary>
        /// _llaveDeFuncion
        /// </summary>
        string _llaveDeFuncion = string.Empty;
        public string LlaveDeFuncion
        {
            get { return _llaveDeFuncion; }
            set { _llaveDeFuncion = value; }
        }

        /// <summary>
        /// _tamañoMensajeEntrada
        /// </summary>
        string _tamañoMensajeEntrada = string.Empty;
        public string TamañoMensajeEntrada
        {
            get { return _tamañoMensajeEntrada; }
            set { _tamañoMensajeEntrada = value; }
        }

        /// <summary>
        /// _indicadorCommit
        /// </summary>
        string _indicadorCommit = string.Empty;
        public string IndicadorCommit
        {
            get { return _indicadorCommit; }
            set { _indicadorCommit = value; }
        }

        /// <summary>
        /// _tipoCabecera
        /// </summary>
        string _tipoCabecera = string.Empty;
        public string TipoCabecera
        {
            get { return _tipoCabecera; }
            set { _tipoCabecera = value; }
        }

        /// <summary>
        /// _tipoProceso
        /// </summary>
        string _tipoProceso = string.Empty;
        public string TipoProceso
        {
            get { return _tipoProceso; }
            set { _tipoProceso = value; }
        }

        /// <summary>
        /// _canal
        /// </summary>
        string _canal = string.Empty;
        public string Canal
        {
            get { return _canal; }
            set { _canal = value; }
        }

        /// <summary>
        /// _preformateoDatos
        /// </summary>
        string _preformateoDatos = string.Empty;
        public string PreformateoDatos
        {
            get { return _preformateoDatos; }
            set { _preformateoDatos = value; }
        }

        /// <summary>
        /// _idioma
        /// </summary>
        string _idioma = string.Empty;
        public string Idioma
        {
            get { return _idioma; }
            set { _idioma = value; }
        }

        /// <summary>
        /// _mensajeEntrada
        /// </summary>
        string _mensajeEntrada = string.Empty;
        public string MensajeEntrada
        {
            get { return _mensajeEntrada; }
            set { _mensajeEntrada = value; }
        }

        /// <summary>
        /// _programaTx
        /// </summary>
        string _programaTx = string.Empty;
        public string ProgramaTx
        {
            get { return _programaTx; }
            set { _programaTx = value; }
        }

        /// <summary>
        /// _desInpCpy
        /// </summary>
        string _desInpCpy = string.Empty;
        public string DesInpCpy
        {
            get { return _desInpCpy; }
            set { _desInpCpy = value; }
        }

        /// <summary>
        /// _caaSwErrcod
        /// </summary>
        string _caaSwErrcod = string.Empty;
        public string CaaSwErrcod
        {
            get { return _caaSwErrcod; }
            set { _caaSwErrcod = value; }
        }

        /// <summary>
        /// _serviceResponse
        /// </summary>
        string _serviceResponse = string.Empty;
        public string ServiceResponse
        {
            get { return _serviceResponse; }
            set { _serviceResponse = value; }
        }

        /// <summary>
        /// _processControl
        /// </summary>
        string _processControl = string.Empty;
        public string ProcessControl
        {
            get { return _processControl; }
            set { _processControl = value; }
        }

        /// <summary>
        /// _outputLength
        /// </summary>
        string _outputLength = string.Empty;
        public string OutputLength
        {
            get { return _outputLength; }
            set { _outputLength = value; }
        }

        /// <summary>
        /// _caaErrVaria1
        /// </summary>
        string _caaErrVaria1 = string.Empty;
        public string CaaErrVaria1
        {
            get { return _caaErrVaria1; }
            set { _caaErrVaria1 = value; }
        }

        /// <summary>
        /// _caaErrVaria2
        /// </summary>
        string _caaErrVaria2 = string.Empty;
        public string CaaErrVaria2
        {
            get { return _caaErrVaria2; }
            set { _caaErrVaria2 = value; }
        }

        /// <summary>
        /// _caaSwCodWA1
        /// </summary>
        string _caaSwCodWA1 = string.Empty;
        public string CaaSwCodWA1
        {
            get { return _caaSwCodWA1; }
            set { _caaSwCodWA1 = value; }
        }

        /// <summary>
        /// _caaSwCodWA2
        /// </summary>
        string _caaSwCodWA2 = string.Empty;
        public string CaaSwCodWA2
        {
            get { return _caaSwCodWA2; }
            set { _caaSwCodWA2 = value; }
        }

        /// <summary>
        /// _caaWarn1Varia1
        /// </summary>
        string _caaWarn1Varia1 = string.Empty;
        public string CaaWarn1Varia1
        {
            get { return _caaWarn1Varia1; }
            set { _caaWarn1Varia1 = value; }
        }

        /// <summary>
        /// _caaWarn1Varia2
        /// </summary>
        string _caaWarn1Varia2 = string.Empty;
        public string CaaWarn1Varia2
        {
            get { return _caaWarn1Varia2; }
            set { _caaWarn1Varia2 = value; }
        }

        /// <summary>
        /// _caaWarn2Varia1
        /// </summary>
        string _caaWarn2Varia1 = string.Empty;
        public string CaaWarn2Varia1
        {
            get { return _caaWarn2Varia1; }
            set { _caaWarn2Varia1 = value; }
        }

        /// <summary>
        /// _caaWarn2Varia2
        /// </summary>
        string _caaWarn2Varia2 = string.Empty;
        public string CaaWarn2Varia2
        {
            get { return _caaWarn2Varia2; }
            set { _caaWarn2Varia2 = value; }
        }

        /// <summary>
        /// _caaTbScrDocu1
        /// </summary>
        string _caaTbScrDocu1 = string.Empty;
        public string CaaTbScrDocu1
        {
            get { return _caaTbScrDocu1; }
            set { _caaTbScrDocu1 = value; }
        }

        /// <summary>
        /// _caaTbDocNum1
        /// </summary>
        string _caaTbDocNum1 = string.Empty;
        public string CaaTbDocNum1
        {
            get { return _caaTbDocNum1; }
            set { _caaTbDocNum1 = value; }
        }

        /// <summary>
        /// _caaTbFstLinDoc1
        /// </summary>
        string _caaTbFstLinDoc1 = string.Empty;
        public string CaaTbFstLinDoc1
        {
            get { return _caaTbFstLinDoc1; }
            set { _caaTbFstLinDoc1 = value; }
        }

        /// <summary>
        /// _caaTbForm1
        /// </summary>
        string _caaTbForm1 = string.Empty;
        public string CaaTbForm1
        {
            get { return _caaTbForm1; }
            set { _caaTbForm1 = value; }
        }

        /// <summary>
        /// _caaTbLng1
        /// </summary>
        string _caaTbLng1;
        public string CaaTbLng1
        {
            get { return _caaTbLng1; }
            set { _caaTbLng1 = value; }
        }

        /// <summary>
        /// _caaTbScrDocu2
        /// </summary>
        string _caaTbScrDocu2 = string.Empty;
        public string CaaTbScrDocu2
        {
            get { return _caaTbScrDocu2; }
            set { _caaTbScrDocu2 = value; }
        }

        /// <summary>
        /// _caaTbDocNum2
        /// </summary>
        string _caaTbDocNum2 = string.Empty;
        public string CaaTbDocNum2
        {
            get { return _caaTbDocNum2; }
            set { _caaTbDocNum2 = value; }
        }

        /// <summary>
        /// _caaTbFstLinDoc2
        /// </summary>
        string _caaTbFstLinDoc2 = string.Empty;
        public string CaaTbFstLinDoc2
        {
            get { return _caaTbFstLinDoc2; }
            set { _caaTbFstLinDoc2 = value; }
        }

        /// <summary>
        /// _caaTbForm2
        /// </summary>
        string _caaTbForm2 = string.Empty;
        public string CaaTbForm2
        {
            get { return _caaTbForm2; }
            set { _caaTbForm2 = value; }
        }

        /// <summary>
        /// _caaTbLng2
        /// </summary>
        string _caaTbLng2;
        public string CaaTbLng2
        {
            get { return _caaTbLng2; }
            set { _caaTbLng2 = value; }
        }

        /// <summary>
        /// _caaTbScrDocu3
        /// </summary>
        string _caaTbScrDocu3 = string.Empty;
        public string CaaTbScrDocu3
        {
            get { return _caaTbScrDocu3; }
            set { _caaTbScrDocu3 = value; }
        }

        /// <summary>
        /// _caaTbDocNum3
        /// </summary>
        string _caaTbDocNum3 = string.Empty;
        public string CaaTbDocNum3
        {
            get { return _caaTbDocNum3; }
            set { _caaTbDocNum3 = value; }
        }

        /// <summary>
        /// _caaTbFstLinDoc3
        /// </summary>
        string _caaTbFstLinDoc3 = string.Empty;
        public string CaaTbFstLinDoc3
        {
            get { return _caaTbFstLinDoc3; }
            set { _caaTbFstLinDoc3 = value; }
        }

        /// <summary>
        /// _caaTbForm3
        /// </summary>
        string _caaTbForm3 = string.Empty;
        public string CaaTbForm3
        {
            get { return _caaTbForm3; }
            set { _caaTbForm3 = value; }
        }

        /// <summary>
        /// _caaTbLng3
        /// </summary>
        string _caaTbLng3;
        public string CaaTbLng3
        {
            get { return _caaTbLng3; }
            set { _caaTbLng3 = value; }
        }

        /// <summary>
        /// _caaTbScrDocu4
        /// </summary>
        string _caaTbScrDocu4 = string.Empty;
        public string CaaTbScrDocu4
        {
            get { return _caaTbScrDocu4; }
            set { _caaTbScrDocu4 = value; }
        }

        /// <summary>
        /// _caaTbDocNum4
        /// </summary>
        string _caaTbDocNum4 = string.Empty;
        public string CaaTbDocNum4
        {
            get { return _caaTbDocNum4; }
            set { _caaTbDocNum4 = value; }
        }

        /// <summary>
        /// _caaTbFstLinDoc4
        /// </summary>
        string _caaTbFstLinDoc4 = string.Empty;
        public string CaaTbFstLinDoc4
        {
            get { return _caaTbFstLinDoc4; }
            set { _caaTbFstLinDoc4 = value; }
        }

        /// <summary>
        /// _caaTbForm4
        /// </summary>
        string _caaTbForm4 = string.Empty;
        public string CaaTbForm4
        {
            get { return _caaTbForm4; }
            set { _caaTbForm4 = value; }
        }

        /// <summary>
        /// _caaTbLng4
        /// </summary>
        string _caaTbLng4;
        public string CaaTbLng4
        {
            get { return _caaTbLng4; }
            set { _caaTbLng4 = value; }
        }

        /// <summary>
        /// _caaTbScrDocu5
        /// </summary>
        string _caaTbScrDocu5 = string.Empty;
        public string CaaTbScrDocu5
        {
            get { return _caaTbScrDocu5; }
            set { _caaTbScrDocu5 = value; }
        }

        /// <summary>
        /// _caaTbDocNum5
        /// </summary>
        string _caaTbDocNum5 = string.Empty;
        public string CaaTbDocNum5
        {
            get { return _caaTbDocNum5; }
            set { _caaTbDocNum5 = value; }
        }

        /// <summary>
        /// _caaTbFstLinDoc5
        /// </summary>
        string _caaTbFstLinDoc5 = string.Empty;
        public string CaaTbFstLinDoc5
        {
            get { return _caaTbFstLinDoc5; }
            set { _caaTbFstLinDoc5 = value; }
        }

        /// <summary>
        /// _caaTbForm5
        /// </summary>
        string _caaTbForm5 = string.Empty;
        public string CaaTbForm5
        {
            get { return _caaTbForm5; }
            set { _caaTbForm5 = value; }
        }

        /// <summary>
        /// _caaTbLng5
        /// </summary>
        string _caaTbLng5;
        public string CaaTbLng5
        {
            get { return _caaTbLng5; }
            set { _caaTbLng5 = value; }
        }

        /// <summary>
        /// _eattDtaBffDC
        /// </summary>
        string _eattDtaBffDC = string.Empty;
        public string EattDtaBffDC
        {
            get { return _eattDtaBffDC; }
            set { _eattDtaBffDC = value; }
        }

        /// <summary>
        /// _totalTime
        /// </summary>
        double _totalTime = 0;
        public double TotalTime
        {
            get { return _totalTime; }
            set { _totalTime = value; }
        }

        /// <summary>
        /// _modulo
        /// </summary>
        string _modulo = string.Empty;

        public string Modulo
        {

            get { return _modulo; }

            set { _modulo = value; }

        }

        /// <summary>
        /// _centro
        /// </summary>
        string _centro = string.Empty;

        public string Centro
        {

            get { return _centro; }

            set { _centro = value; }

        }

        /// <summary>
        /// _cics
        /// </summary>
        string _cics = string.Empty;

        public string Cics
        {

            get { return _cics; }

            set { _cics = value; }

        }

        /// <summary>
        /// _ent
        /// </summary>
        string _ent = string.Empty;

        public string Ent
        {

            get { return _ent; }

            set { _ent = value; }

        }

        /// <summary>
        /// _result
        /// </summary>
        string _result = string.Empty;
        public string Result
        {
            get { return _result; }
            set { _result = value; }
        }


        public String getStringValues()
        {
            StringBuilder sbValues = new StringBuilder();
            Stopwatch timer = new Stopwatch();
            timer.Start();
            sbValues.AppendFormat("Inicia Generación QAECCA[\n");
            sbValues.AppendFormat("IdProtocolo: [{0}]\n", IdProtocolo);
            sbValues.AppendFormat("TerminalLogica: [{0}]\n", TerminalLogica);
            sbValues.AppendFormat("TerminalFisica: [{0}]\n", TerminalFisica);
            sbValues.AppendFormat("UsuarioDeTerminal: [{0}]\n", UsuarioDeTerminal);
            sbValues.AppendFormat("NumeroSecuenciaPs9: [{0}]\n", NumeroSecuenciaPs9);
            sbValues.AppendFormat("IdTransaccion: [{0}]\n", IdTransaccion);
            sbValues.AppendFormat("LlaveDeFuncion: [{0}]\n", LlaveDeFuncion);
            sbValues.AppendFormat("TamañoMensajeEntrada: [{0}]\n", TamañoMensajeEntrada);
            sbValues.AppendFormat("IndicadorCommit: [{0}]\n", IndicadorCommit);
            sbValues.AppendFormat("TipoCabecera: [{0}]\n", TipoCabecera);
            sbValues.AppendFormat("TipoProceso: [{0}]\n", TipoProceso);
            sbValues.AppendFormat("Canal: [{0}]\n", Canal);
            sbValues.AppendFormat("PreformateoDatos: [{0}]\n", PreformateoDatos);
            sbValues.AppendFormat("Idioma: [{0}]\n", Idioma);
            sbValues.AppendFormat("MensajeEntrada: [{0}]\n]", MensajeEntrada);
            timer.Stop();
            long timeElapsed = timer.ElapsedMilliseconds;
            sbValues.AppendFormat("Termina Generació QAECCA[{0} ms.]\n", timeElapsed);
            return sbValues.ToString();
        }

    }
}
