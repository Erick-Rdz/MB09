using QAEXCA;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

namespace QA1XOP9
{
    [Serializable]
    public class QA1XOP9
    {
        //QAECCA_NET.QAECCA_NET qaecca = new QAECCA_NET.QAECCA_NET();
        public StringBuilder ConstruirSalida(QAEXCA.QAEXCA qaeaxca)
        {
            try
            {
                if (qaeaxca.CaaSwErrcod != string.Empty)
                {
                    //Error
                    //Tag
                    qaeaxca.sbSalida.Append("<ER>");
                    //error code
                    qaeaxca.sbSalida.Append(qaeaxca.CaaSwErrcod.PadRight(7, ' '));
                    //Number of Variables
                    int cont = 0;
                    if (qaeaxca.CaaErrVaria1 != string.Empty)
                        cont++;
                    if (qaeaxca.CaaErrVaria2 != string.Empty)
                        cont++;
                    qaeaxca.sbSalida.Append(cont);
                    if (cont == 1)
                    {
                        //Variable 1
                        qaeaxca.sbSalida.Append(qaeaxca.CaaErrVaria1.PadRight(20, ' '));
                    }
                    if (cont == 2)
                    {
                        qaeaxca.sbSalida.Append(qaeaxca.CaaErrVaria1.PadRight(20, ' '));
                        //Variable 2
                        qaeaxca.sbSalida.Append(qaeaxca.CaaErrVaria2.PadRight(20, ' '));
                    }
                    //Tag
                    qaeaxca.sbSalida.Append("</ER>");
                }

                //PERFORM 220000-WARNINGS
                //1
                if (qaeaxca.CaaSwCodWA1 != string.Empty)
                {
                    //Error
                    //Tag
                    qaeaxca.sbSalida.Append("<AV>");
                    //error code
                    qaeaxca.sbSalida.Append(qaeaxca.CaaSwCodWA1.PadRight(7, ' '));
                    //Number of Variables
                    int cont = 0;
                    if (qaeaxca.CaaWarn1Varia1 != string.Empty)
                        cont++;
                    if (qaeaxca.CaaWarn1Varia2 != string.Empty)
                        cont++;
                    qaeaxca.sbSalida.Append(cont);
                    if (cont == 1)
                    {
                        //Variable 1
                        qaeaxca.sbSalida.Append(qaeaxca.CaaWarn1Varia1.PadRight(20, ' '));
                    }
                    if (cont == 2)
                    {
                        qaeaxca.sbSalida.Append(qaeaxca.CaaWarn1Varia1.PadRight(20, ' '));
                        //Variable 2
                        qaeaxca.sbSalida.Append(qaeaxca.CaaWarn1Varia2.PadRight(20, ' '));
                    }
                    //Tag
                    qaeaxca.sbSalida.Append("</AV>");
                }
                //2

                if (qaeaxca.CaaSwCodWA2 != string.Empty)
                {
                    //Error
                    //Tag
                    qaeaxca.sbSalida.Append("<AV>");
                    //error code
                    qaeaxca.sbSalida.Append(qaeaxca.CaaSwCodWA2.PadRight(7, ' '));
                    //Number of Variables
                    int cont = 0;
                    if (qaeaxca.CaaWarn2Varia1 != string.Empty)
                        cont++;
                    if (qaeaxca.CaaWarn2Varia2 != string.Empty)
                        cont++;
                    qaeaxca.sbSalida.Append(cont);
                    if (cont == 1)
                    {
                        //Variable 1
                        qaeaxca.sbSalida.Append(qaeaxca.CaaWarn2Varia1.PadRight(20, ' '));
                    }
                    if (cont == 2)
                    {
                        qaeaxca.sbSalida.Append(qaeaxca.CaaWarn2Varia1.PadRight(20, ' '));
                        //Variable 2
                        qaeaxca.sbSalida.Append(qaeaxca.CaaWarn2Varia2.PadRight(20, ' '));
                    }
                    qaeaxca.sbSalida.Append("</AV>");
                   //Tag
                }
                //SE IDENTIFICA EL TIPO DE TRANSACCION (TRANSACCIONAL | CONVERSACIONAL)
                tipoTransaccion(qaeaxca);
                //SE ASIGNA EL HEADER DE SALIDA
                asignarHeader(qaeaxca);
                //INSERTAMOS EL HEADER A LA SALIDA
                qaeaxca.sbSalida = qaeaxca.sbSalida.Insert(0, qaeaxca.sbHeader);
            }
            catch (Exception ex)
            {
                EventLog.WriteEntry("COMMAREA", "\nError al generar la cadena PS9 de salida, validar los datos de entrada.\n\n" + ex.Message, EventLogEntryType.Error);
            }

            return qaeaxca.sbSalida;
        }


        public void tipoTransaccion(QAEXCA.QAEXCA qaeaxca)
        {
            if (qaeaxca.TipoTX != "C")
            {
                //PERFORM 240000-DESTINATION
                if (qaeaxca.CaaTbScrDocu1 != string.Empty)
                {
                    //Tag
                    qaeaxca.sbSalida.Append("<DE>");
                    //ID
                    qaeaxca.sbSalida.Append("1");
                    //Destination of the format
                    qaeaxca.sbSalida.Append(qaeaxca.CaaTbScrDocu1.PadLeft(1, '0'));
                    //Type of document 
                    qaeaxca.sbSalida.Append(qaeaxca.CaaTbDocNum1.PadLeft(1, ' '));
                    //Position of the first line 
                    qaeaxca.sbSalida.Append(qaeaxca.CaaTbFstLinDoc1.PadLeft(2, '0'));
                    //form 
                    qaeaxca.sbSalida.Append(qaeaxca.CaaTbForm1.PadLeft(6, ' '));
                    //form 
                    qaeaxca.sbSalida.Append(qaeaxca.Idioma.PadLeft(1, ' '));
                    //Tag
                    qaeaxca.sbSalida.Append("</DE>");
                }
                if (qaeaxca.CaaTbScrDocu2 != string.Empty)
                {
                    //Tag
                    qaeaxca.sbSalida.Append("<DE>");
                    //ID
                    qaeaxca.sbSalida.Append("2");
                    //Destination of the format
                    qaeaxca.sbSalida.Append(qaeaxca.CaaTbScrDocu2.PadLeft(1, '0'));
                    //Type of document 
                    qaeaxca.sbSalida.Append(qaeaxca.CaaTbDocNum2.PadLeft(1, ' '));
                    //Position of the first line 
                    qaeaxca.sbSalida.Append(qaeaxca.CaaTbFstLinDoc2.PadLeft(2, '0'));
                    //form 
                    qaeaxca.sbSalida.Append(qaeaxca.CaaTbForm2.PadLeft(6, ' '));
                    //form 
                    qaeaxca.sbSalida.Append(qaeaxca.Idioma.PadLeft(1, ' '));
                    //Tag
                    qaeaxca.sbSalida.Append("</DE>");
                }
                if (qaeaxca.CaaTbScrDocu3 != string.Empty)
                {
                    //Tag
                    qaeaxca.sbSalida.Append("<DE>");
                    //ID
                    qaeaxca.sbSalida.Append("3");
                    //Destination of the format
                    qaeaxca.sbSalida.Append(qaeaxca.CaaTbScrDocu3.PadLeft(1, '0'));
                    //Type of document 
                    qaeaxca.sbSalida.Append(qaeaxca.CaaTbDocNum3.PadLeft(1, ' '));
                    //Position of the first line 
                    qaeaxca.sbSalida.Append(qaeaxca.CaaTbFstLinDoc3.PadLeft(2, '0'));
                    //form 
                    qaeaxca.sbSalida.Append(qaeaxca.CaaTbForm3.PadLeft(6, ' '));
                    //form 
                    qaeaxca.sbSalida.Append(qaeaxca.Idioma.PadLeft(1, ' '));
                    //Tag
                    qaeaxca.sbSalida.Append("</DE>");
                }
                if (qaeaxca.CaaTbScrDocu4 != string.Empty)
                {
                    //Tag
                    qaeaxca.sbSalida.Append("<DE>");
                    //ID
                    qaeaxca.sbSalida.Append("4");
                    //Destination of the format
                    qaeaxca.sbSalida.Append(qaeaxca.CaaTbScrDocu4.PadLeft(1, '0'));
                    //Type of document 
                    qaeaxca.sbSalida.Append(qaeaxca.CaaTbDocNum4.PadLeft(1, ' '));
                    //Position of the first line 
                    qaeaxca.sbSalida.Append(qaeaxca.CaaTbFstLinDoc4.PadLeft(2, '0'));
                    //form 
                    qaeaxca.sbSalida.Append(qaeaxca.CaaTbForm4.PadLeft(6, ' '));
                    //form 
                    qaeaxca.sbSalida.Append(qaeaxca.Idioma.PadLeft(1, ' '));
                    //Tag
                    qaeaxca.sbSalida.Append("</DE>");
                }
                if (qaeaxca.CaaTbScrDocu5 != string.Empty)
                {
                    //Tag
                    qaeaxca.sbSalida.Append("<DE>");
                    //ID
                    qaeaxca.sbSalida.Append("5");
                    //Destination of the format
                    qaeaxca.sbSalida.Append(qaeaxca.CaaTbScrDocu5.PadLeft(1, '0'));
                    //Type of document 
                    qaeaxca.sbSalida.Append(qaeaxca.CaaTbDocNum5.PadLeft(1, ' '));
                    //Position of the first line 
                    qaeaxca.sbSalida.Append(qaeaxca.CaaTbFstLinDoc5.PadLeft(2, '0'));
                    //form 
                    qaeaxca.sbSalida.Append(qaeaxca.CaaTbForm5.PadLeft(6, ' '));
                    //form 
                    qaeaxca.sbSalida.Append(qaeaxca.Idioma.PadLeft(1, ' '));
                    //Tag
                    qaeaxca.sbSalida.Append("</DE>");
                }

                //PERFORM 250000-OC
                if (qaeaxca.EattDtaBffDC != string.Empty)
                {
                    //Tag
                    qaeaxca.sbSalida.Append("<OC>");
                    //Data
                    qaeaxca.sbSalida.Append(qaeaxca.EattDtaBffDC);
                    //Tag
                    qaeaxca.sbSalida.Append("</OC>");
                }
            }
            else
            {
                //Tag
                qaeaxca.sbSalida.Append("<SG>");
                //Data
                qaeaxca.sbSalida.Append(qaeaxca.EattDtaBffDC);
                //Tag
                qaeaxca.sbSalida.Append("</SG>");
            }

        }

        public void asignarHeader(QAEXCA.QAEXCA qaeaxca)
        {
            //calcular long e ingresarlo en la creacion del header.
            int length = qaeaxca.sbSalida.Length + 26;
            qaeaxca.OutputLength = length.ToString();

            //Output header
            //Tag
            qaeaxca.sbHeader.Append("<OH>");
            //Protocol Identification
            if (qaeaxca.IdProtocolo.Length > 2)
                qaeaxca.IdProtocolo = qaeaxca.IdProtocolo.Substring(0, 2);
            qaeaxca.sbHeader.Append(qaeaxca.IdProtocolo.PadLeft(2, '0'));
            //Service response
            if (qaeaxca.ServiceResponse.Length > 1)
                qaeaxca.ServiceResponse = qaeaxca.ServiceResponse.Substring(0, 1);
            qaeaxca.sbHeader.Append(qaeaxca.ServiceResponse.PadLeft(1, '0'));
            //Process Control
            if (qaeaxca.ProcessControl.Length > 1)
                qaeaxca.ProcessControl = qaeaxca.ProcessControl.Substring(0, 1);
            qaeaxca.sbHeader.Append(qaeaxca.ProcessControl.PadLeft(1, '0'));
            //Sequence number
            if (qaeaxca.NumeroSecuenciaPs9.Length > 8)
                qaeaxca.NumeroSecuenciaPs9 = qaeaxca.NumeroSecuenciaPs9.Substring(0, 8);
            qaeaxca.sbHeader.Append(qaeaxca.NumeroSecuenciaPs9.PadRight(8, '0'));
            //Output message length
            qaeaxca.sbHeader.Append(qaeaxca.OutputLength.PadLeft(5, '0'));
            qaeaxca.sbHeader.Append("</OH>"); //tag
        }
    }
}
