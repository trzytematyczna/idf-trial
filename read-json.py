import json
import os
from pandas.io.json import json_normalize
import calendar
#exec(open("read-json.py").read())
#"/data/sample_guardian-article.json"
def parseJSONtoCSV(fromYear, toYear):
    for y in range(fromYear,toYear+1):
        for m in range(1,13):
            fromDate =  str(y)+'-'+str(format(m, '02d'))+'-'+'01'
            toDate = str(y)+'-'+str(format(m, '02d'))+'-'+str(calendar.monthrange(y,m)[1])
            with open(os.getcwd() + "/data/guardian-data/guardian_"+fromDate+"-"+toDate) as fh:
                data = json.loads(fh.read())
                fh.close()
            data_normalized = json_normalize(data)
            data_normalized.to_csv(os.getcwd() +"/data/guardian_csv/guardian_"+fromDate+"-"+toDate+'.csv', encoding="UTF-8")