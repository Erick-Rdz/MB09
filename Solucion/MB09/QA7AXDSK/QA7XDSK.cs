using System;
using System.Collections.Generic;
using System.Text;
using System.Data.Odbc;
using System.Data;

namespace QA7XDSK
{ 
    public class QA7XDSK
    {
        public bool disconnect(OdbcConnection conn)
        {
            conn.Close();
            return true;
        }
    }
}
