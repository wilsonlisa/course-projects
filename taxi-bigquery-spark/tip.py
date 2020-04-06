import json
import pyspark
import pprint
# from numpy import mean
from operator import add
# from pyspark.sql.functions import avg
# from pyspark.sql.functions import stddev
# from numpy import stdev
import math

def StdDev( data ):
    suma = 0
    for x in data: suma += ((x - sum(data)/len(data))**2)  # every iteration subtracks the mean value of a list [sum(data)/len(data)]  from every [x] element of a list and then raises to the second power [**2]
    print((suma/(len(data)-1))**0.5) # then divide by n-1 [len(data)-1] and extract the root of 2 [**0.5]

sc = pyspark.SparkContext()

bucket = sc._jsc.hadoopConfiguration().get('fs.gs.system.bucket')
project = sc._jsc.hadoopConfiguration().get('fs.gs.project.id')
input_directory = 'gs://{}/hadoop/tmp/bigquery/pyspark_input'.format(bucket)
output_directory = 'gs://{}/pyspark_demo_output_f2'.format(bucket)

conf = {
    # Input Parameters.
    'mapred.bq.project.id': project,
    'mapred.bq.gcs.bucket': bucket,
    'mapred.bq.temp.gcs.path': input_directory,
    'mapred.bq.input.project.id': 'final-project-269604',
    'mapred.bq.input.dataset.id': 'taxi',
    'mapred.bq.input.table.id': 'fare2',
}

table_data = sc.newAPIHadoopRDD(
    'com.google.cloud.hadoop.io.bigquery.JsonTextBigQueryInputFormat',
    'org.apache.hadoop.io.LongWritable',
    'com.google.gson.JsonObject',
    conf=conf
)

vals = table_data.values()
vals = vals.map(lambda line: json.loads(line))
vals_crd = vals.filter(lambda x: str(x['payment_type'])=='CRD').map(lambda x: (str(x['month_pickup']), float(x['tip_amount'])))
sum_crd = vals_crd.reduceByKey(add)
count_crd = vals_crd.countByKey()
avg_crd = sum_crd.map(lambda x: (x[0], float(x[1]/count_crd[x[0]])))
max_crd = vals_crd.max()
min_crd = vals_crd.min()

vals_csh = vals.filter(lambda x: str(x['payment_type'])=='CSH').map(lambda x: (str(x['month_pickup']), float(x['tip_amount'])))
sum_csh = vals_csh.reduceByKey(add)
count_csh = vals_csh.countByKey()
avg_csh = sum_csh.map(lambda x: (x[0], float(x[1]/count_csh[x[0]])))
max_csh = vals_csh.max()
min_csh = vals_csh.min()

# hour_avg2 = vals_hour.groupByKey().mean()
# hour_std = vals_hour.reduceByKey(stdev)
# hour_std2 = vals_hour.groupByKey().map(lambda x: (x[0], stddev(x[1])))
# hour_std = vals_hour.groupByKey().stdev()

# standard deviation
# had trouble using reduceByKey with built-in standard deviation functions in various packages
# so used code from the following blog post
# https://blog.scottlogic.com/2016/12/19/spark-unaffordable-britain.html
# countByKey = vals_crd.map(lambda kvp: (kvp[0], 1)).reduceByKey(lambda a,b: a + b)
# totalByKey = vals_crd.reduceByKey(lambda a,b: a + b)
# sumSqByKey = vals_crd.map(lambda kvp: (kvp[0], kvp[1]**2)).reduceByKey(lambda a,b: a + b)
# mean = totalByKey.join(countByKey).map(lambda kvp: (kvp[0], kvp[1][0] / kvp[1][1]))
# avgSquare = sumSqByKey.join(countByKey).map(lambda kvp: (kvp[0], kvp[1][0] / kvp[1][1]))
# std_crd = avgSquare.join(mean).map(lambda kvp: (kvp[0], math.sqrt(kvp[1][0] - kvp[1][1]**2)))

# countByKey = vals_csh.map(lambda kvp: (kvp[0], 1)).reduceByKey(lambda a,b: a + b)
# totalByKey = vals_csh.reduceByKey(lambda a,b: a + b)
# sumSqByKey = vals_csh.map(lambda kvp: (kvp[0], kvp[1]**2)).reduceByKey(lambda a,b: a + b)
# mean = totalByKey.join(countByKey).map(lambda kvp: (kvp[0], kvp[1][0] / kvp[1][1]))
# avgSquare = sumSqByKey.join(countByKey).map(lambda kvp: (kvp[0], kvp[1][0] / kvp[1][1]))
# std_csh = avgSquare.join(mean).map(lambda kvp: (kvp[0], math.sqrt(kvp[1][0] - kvp[1][1]**2)))

# pprint.pprint(vals_crd.collect())
pprint.pprint(avg_crd.collect())
# pprint.pprint(std_crd.collect())

# pprint.pprint(vals_csh.collect())
pprint.pprint(avg_csh.collect())
# pprint.pprint(std_csh.collect())

pprint.pprint([max_crd, min_crd, max_csh, min_csh])

# crd_avgstd = avg_crd.union(std_crd)
# csh_avgstd = avg_csh.union(std_csh)
crdcsh = avg_crd.union(avg_csh)
# pprint.pprint(crd_avgstd.collect())
# pprint.pprint(csh_avgstd.collect())
# pprint.pprint(crdcsh.collect())

crdcsh.saveAsTextFile(output_directory)

input_path = sc._jvm.org.apache.hadoop.fs.Path(input_directory)
input_path.getFileSystem(sc._jsc.hadoopConfiguration()).delete(input_path, True)
