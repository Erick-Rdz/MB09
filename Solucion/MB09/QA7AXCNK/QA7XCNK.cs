using System;
using System.Collections.Generic;
using System.Data.Odbc;
using System.Text;

namespace QA7XCNK
{
    public class QA7XCNK
    {
        public OdbcConnection connect(string ODBC)
        {
            OdbcConnection conn = new OdbcConnection(ODBC);
            conn.Open();
            return conn;
        }
    }
}
