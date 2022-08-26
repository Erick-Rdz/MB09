      ******************************************************************
      *A.MF QAECREAM                  Nombre objeto                    *
      *A.MF MEMO-NM                   Nombre de la memoria             *
      *A.MF OPTION                    Opción                           *
      *A.MF NUM-RECORD                Número de registros              *
      *A.MF COD-RETURN                Código de respuesta              *
      *A.MF RECORD                    Registro                         *
      ******************************************************************
      ******************************************************************
      *B.MF QAECREAM                  Object name                      *
      *B.MF MEMO-NM                   Memory name                      *
      *B.MF OPTION                    Option                           *
      *B.MF NUM-RECORD                Number of records                *
      *B.MF COD-RETURN                Return code                      *
      *B.MF RECORD                    Record                           *
      ******************************************************************
      *OL.S
      ******************************************************************
      *                        QAECRMEM                                 
      *                                                                *
      ******************************************************************
      *OL.E
       02 QAECREAM.                                                     
          05 EREAM-MEMO-NM                 PIC X(8).                    
          05 EREAM-OPTION                  PIC X(1).                    
          05 EREAM-NUM-RECORD              PIC X(2).                    
          05 EREAM-COD-RETURN              PIC S9(5) COMP-3.            
          05 EREAM-RECORD                  PIC X(20000).                
                                                                        
      *OL.S
      ******************************************************************
      *I  COPY LENGTH         (IN BYTES):   20 BYTES                    
      *E  LONGITUD DE LA COPY (EN BYTES):   20 BYTES                    
      ***************************************************************** 
      *OL.E

